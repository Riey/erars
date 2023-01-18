use crate::{CompileError, CompileResult, Instruction};
use erars_ast::{
    BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable, Expr, FormExpr, FormText,
    Function, FunctionHeader, SelectCaseCond, Stmt, StmtWithPos, StrKey, Variable,
};
use hashbrown::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompiledFunction {
    pub header: FunctionHeader,
    pub goto_labels: HashMap<StrKey, u32>,
    pub body: Box<[Instruction]>,
}

pub struct Compiler {
    pub out: Vec<Instruction>,
    pub goto_labels: HashMap<StrKey, u32>,
    pub continue_marks: Vec<Vec<u32>>,
    pub break_marks: Vec<Vec<u32>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            out: Vec::new(),
            goto_labels: HashMap::new(),
            continue_marks: Vec::new(),
            break_marks: Vec::new(),
        }
    }

    fn push(&mut self, inst: Instruction) {
        self.out.push(inst);
    }

    fn mark(&mut self) -> u32 {
        let ret = self.current_no();
        self.push(Instruction::nop());
        ret
    }

    fn current_no(&self) -> u32 {
        self.out.len() as u32
    }

    fn insert(&mut self, mark: u32, inst: Instruction) {
        *self
            .out
            .get_mut(mark as usize)
            .unwrap_or_else(|| unreachable!("Invalid mark {}", mark)) = inst;
    }

    fn push_opt_list(
        &mut self,
        args: impl IntoIterator<IntoIter = impl ExactSizeIterator + IntoIterator<Item = Option<Expr>>>,
        empty_arg: impl Fn(usize, &mut Vec<Instruction>) -> CompileResult<()>,
    ) -> CompileResult<u32> {
        let args = args.into_iter();
        let len = args.len();
        for (idx, arg) in args.into_iter().enumerate() {
            match arg {
                Some(arg) => self.push_expr(arg)?,
                None => empty_arg(idx, &mut self.out)?,
            }
        }
        Ok(len as u32)
    }

    fn push_list(
        &mut self,
        args: impl IntoIterator<IntoIter = impl ExactSizeIterator + IntoIterator<Item = Expr>>,
    ) -> CompileResult<u32> {
        let args = args.into_iter();
        let len = args.len();
        for arg in args {
            self.push_expr(arg)?;
        }
        Ok(len as u32)
    }

    fn push_expr(&mut self, expr: Expr) -> CompileResult<()> {
        match expr {
            Expr::IncOpExpr {
                var,
                is_inc,
                is_pre,
            } => {
                let op = if is_inc {
                    BinaryOperator::Add
                } else {
                    BinaryOperator::Sub
                };

                if is_pre {
                    self.push_var(var.clone())?;
                    self.push(Instruction::load_int(1));
                    self.push(Instruction::binop(op));
                    self.push(Instruction::duplicate());
                    self.store_var(var)?;
                } else {
                    self.push_var(var.clone())?;
                    self.push(Instruction::read_var());
                    self.push(Instruction::duplicate());
                    self.push(Instruction::load_int(1));
                    self.push(Instruction::binop(op));
                    self.store_var(var)?;
                }
            }
            Expr::BuiltinVar(var, args) => {
                let c = self.push_list(args)?;
                self.push(Instruction::load_int(c as i32));
                self.push(Instruction::builtin_var(var));
            }
            Expr::BuiltinMethod(meth, args) => {
                let c = self.push_opt_list(args, |idx, out| default_arg_method(meth, idx, out))?;
                self.push(Instruction::load_int(c as i32));
                self.push(Instruction::builtin_method(meth));
            }
            Expr::String(s) => self.push(Instruction::load_str(s)),
            Expr::Int(i) => {
                if let Ok(i) = i.try_into() {
                    self.push(Instruction::load_int(i))
                } else {
                    #[cfg(target_engian = "big")]
                    compile_error!("Big endian not supported");
                    let (l, r): (i32, i32) = unsafe { std::mem::transmute(i) };
                    self.push(Instruction::load_int(l));
                    self.push(Instruction::load_int_suffix(r));
                }
            }
            Expr::BinopExpr(lhs, op, rhs) => {
                match op {
                    // short circuit
                    BinaryOperator::And => {
                        self.push_expr(*lhs)?;
                        let lhs_end = self.mark();
                        self.push_expr(*rhs)?;
                        let rhs_end = self.mark();
                        self.insert(lhs_end, Instruction::goto_if_not(self.current_no()));
                        self.push(Instruction::load_int(0));
                        self.insert(rhs_end, Instruction::goto(self.current_no()));
                    }
                    // short circuit
                    BinaryOperator::Or => {
                        self.push_expr(*lhs)?;
                        let lhs_end = self.mark();
                        self.push_expr(*rhs)?;
                        let rhs_end = self.mark();
                        self.insert(lhs_end, Instruction::goto_if(self.current_no()));
                        self.push(Instruction::load_int(1));
                        self.insert(rhs_end, Instruction::goto(self.current_no()));
                    }
                    _ => {
                        self.push_expr(*lhs)?;
                        self.push_expr(*rhs)?;
                        self.push(Instruction::binop(op));
                    }
                }
            }
            Expr::UnaryopExpr(expr, op) => {
                self.push_expr(*expr)?;
                self.push(Instruction::unaryop(op));
            }
            Expr::FormText(form) => {
                self.push_form(form)?;
            }
            Expr::CondExpr(cond, if_true, or_false) => {
                self.push_expr(*cond)?;
                let begin = self.mark();
                self.push_expr(*if_true)?;
                let true_end = self.mark();
                self.insert(begin, Instruction::goto_if_not(true_end + 1));
                self.push_expr(*or_false)?;
                self.insert(true_end, Instruction::goto(self.current_no()));
            }
            Expr::Method(name, args) => {
                let count = self.push_opt_list(args, |idx, out| {
                    log::error!("TODO: default arg for {name}:{idx}");
                    out.push(Instruction::load_int(0));
                    Ok(())
                })?;
                self.push(Instruction::load_str(name));
                self.push(Instruction::call(count));
            }
            Expr::Var(var) => {
                self.push_var(var)?;
            }
        }

        Ok(())
    }

    fn push_var_ref(&mut self, var: Variable) -> CompileResult<()> {
        let count = self.push_list(var.args)?;
        self.push(Instruction::load_str(var.var));
        match var.func_extern {
            Some(e) => {
                self.push(Instruction::load_str(e));
                self.push(Instruction::load_extern_varref(count));
            }
            None => {
                self.push(Instruction::load_var_ref(count));
            }
        }
        Ok(())
    }

    fn push_count(&mut self) {
        self.push(Instruction::load_count_var_ref());
    }

    fn store_count(&mut self) {
        self.push(Instruction::load_count_var_ref());
        self.push(Instruction::store_var());
    }

    fn store_var(&mut self, var: Variable) -> CompileResult<()> {
        self.push_var_ref(var)?;
        self.push(Instruction::store_var());

        Ok(())
    }

    fn push_var(&mut self, var: Variable) -> CompileResult<()> {
        self.push_var_ref(var)?;

        Ok(())
    }

    fn push_form(&mut self, form: FormText) -> CompileResult<()> {
        self.push(Instruction::load_str(form.first.into()));

        if !form.other.is_empty() {
            let count = 1 + form.other.len() as u32 * 2;
            for (
                FormExpr {
                    expr,
                    align,
                    padding,
                },
                text,
            ) in form.other
            {
                self.push_expr(expr)?;
                if let Some(padding) = padding {
                    self.push_expr(padding)?;
                    self.push(Instruction::pad_str(align.unwrap_or_default()));
                }
                self.push(Instruction::load_str(text));
            }
            self.push(Instruction::concat_string(count));
        }

        Ok(())
    }

    fn push_for(
        &mut self,
        var: Option<Variable>,
        init: Expr,
        end: Expr,
        step: Expr,
        body: Vec<StmtWithPos>,
    ) -> CompileResult<()> {
        // var = init;
        //
        // NOTE: These values are immutable
        //
        // let step = step;
        // let end = end;
        //
        // while var > end {
        //     body
        //     var += step;
        //     goto LOOP;
        // }

        macro_rules! push_count_var {
            () => {
                match var.clone() {
                    Some(var) => self.push_var(var)?,
                    None => self.push_count(),
                }
            };
        }

        macro_rules! store_count_var {
            () => {
                match var.clone() {
                    Some(var) => self.store_var(var)?,
                    None => self.store_count(),
                }
            };
        }

        self.push_expr(init)?;
        store_count_var!();

        self.push_expr(step)?;
        self.push(Instruction::read_var());
        self.push_expr(end)?;
        self.push(Instruction::read_var());

        // stack: [step, end]

        self.begin_loop_block();

        let start = self.current_no();

        // compare var with end

        // stack: [step, end, var]
        push_count_var!();
        // stack: [step, end, var, end]
        self.push(Instruction::duplicate_prev());
        // stack: [step, end, exit?]
        self.push(Instruction::binop(BinaryOperator::Less));

        // stack: [step, end]
        let exit = self.mark();

        for stmt in body {
            self.push_stmt_with_pos(stmt)?;
        }

        let continue_mark = self.current_no();

        // add step to var
        self.push(Instruction::duplicate_prev());
        push_count_var!();
        self.push(Instruction::binop(BinaryOperator::Add));
        store_count_var!();
        self.push(Instruction::goto(start));

        self.insert(exit, Instruction::goto_if_not(self.current_no()));

        self.end_loop_block(continue_mark);

        // remove pinned values
        self.push(Instruction::pop());
        self.push(Instruction::pop());

        Ok(())
    }

    #[inline]
    pub fn push_stmt_with_pos(&mut self, stmt: StmtWithPos) -> CompileResult<()> {
        self.push(Instruction::report_position(stmt.1));
        self.push_stmt(stmt.0)
    }

    pub fn push_stmt(&mut self, stmt: Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::CallEvent(ty) => {
                self.push(Instruction::call_event(ty));
            }
            Stmt::PrintButton { flags, text, value } => {
                self.push_expr(text)?;
                self.push_expr(value)?;
                self.push(Instruction::print_button(flags));
            }
            Stmt::Print(flags, text) => {
                self.push_expr(text)?;
                self.push(Instruction::print(flags));
            }
            Stmt::PrintFormS(flags, text) => {
                self.push_expr(text)?;
                self.push(Instruction::eval_form_string());
                self.push(Instruction::print(flags));
            }
            Stmt::PrintList(flags, args) => {
                let count = self.push_list(args)?;
                self.push(Instruction::concat_string(count));
                self.push(Instruction::print(flags));
            }
            Stmt::PrintData(flags, cond, list) => {
                let cond = cond.unwrap_or_else(|| {
                    Expr::BuiltinVar(BuiltinVariable::Rand, vec![Expr::Int(list.len() as _)])
                });
                self.push_expr(cond)?;

                let branch = self.current_no();

                for _ in 0..list.len() * 4 {
                    self.mark();
                }

                let mut line_positions = Vec::new();

                for (i, part) in list.into_iter().enumerate() {
                    let start = self.current_no();

                    self.insert(branch + i as u32 * 4, Instruction::duplicate());
                    self.insert(branch + i as u32 * 4 + 1, Instruction::load_int(i as _));
                    self.insert(
                        branch + i as u32 * 4 + 2,
                        Instruction::binop(BinaryOperator::NotEqual),
                    );
                    self.insert(branch + i as u32 * 4 + 3, Instruction::goto_if_not(start));

                    for line in part {
                        self.push_expr(line)?;
                    }

                    self.push(Instruction::print(flags));

                    let end = self.mark();
                    line_positions.push(end);
                }

                for end in line_positions {
                    self.insert(end, Instruction::goto(self.current_no()));
                }
            }
            Stmt::ReuseLastLine(text) => {
                self.push(Instruction::load_str(text));
                self.push(Instruction::reuse_lastline());
            }
            Stmt::Sif(cond, body) => {
                self.push_expr(cond)?;
                let mark = self.mark();
                self.push_stmt_with_pos(*body)?;
                self.insert(mark, Instruction::goto_if_not(self.current_no()));
            }
            Stmt::SelectCase(cond, cases, case_else) => {
                self.push_expr(cond)?;
                self.push(Instruction::read_var());

                let mut ends = Vec::new();
                let mut cond_ends = Vec::new();
                for (conds, body) in cases {
                    for cond in conds {
                        match cond {
                            SelectCaseCond::Single(e) => {
                                self.push(Instruction::duplicate());
                                self.push_expr(e)?;
                                self.push(Instruction::binop(BinaryOperator::Equal));
                            }
                            SelectCaseCond::Is(op, e) => {
                                self.push(Instruction::duplicate());
                                self.push_expr(e)?;
                                self.push(Instruction::binop(op));
                            }
                            SelectCaseCond::To(from, to) => {
                                self.push(Instruction::duplicate());
                                self.push_expr(from)?;
                                self.push(Instruction::binop(BinaryOperator::GreaterOrEqual));
                                self.push(Instruction::duplicate_prev());
                                self.push_expr(to)?;
                                self.push(Instruction::binop(BinaryOperator::LessOrEqual));
                                self.push(Instruction::binop(BinaryOperator::And));
                            }
                        }
                        // self.push(Instruction::debug(StrKey::new("cond_end")));
                        cond_ends.push(self.mark());
                    }
                    let cond_block_end = self.mark();

                    for cond_end in cond_ends.drain(..) {
                        self.insert(cond_end, Instruction::goto_if(self.current_no()));
                    }

                    // self.push(Instruction::debug(StrKey::new("cond_pop")));
                    self.push(Instruction::pop());
                    for stmt in body {
                        self.push_stmt_with_pos(stmt)?;
                    }

                    ends.push(self.mark());

                    self.insert(cond_block_end, Instruction::goto(self.current_no()));
                }

                // self.push(Instruction::debug(StrKey::new("cond_pop_end")));
                self.push(Instruction::pop());

                if let Some(case_else) = case_else {
                    for stmt in case_else {
                        self.push_stmt_with_pos(stmt)?;
                    }
                }

                for end in ends {
                    self.insert(end, Instruction::goto(self.current_no()));
                }
            }
            Stmt::Continue => {
                let mark = self.mark();
                self.continue_marks
                    .last_mut()
                    .ok_or(CompileError::ContinueNotLoop)?
                    .push(mark);
            }
            Stmt::Break => {
                let mark = self.mark();
                self.break_marks
                    .last_mut()
                    .ok_or(CompileError::BreakNotLoop)?
                    .push(mark);
            }
            Stmt::Repeat(end, body) => {
                self.push_for(None, Expr::int(0), end, Expr::int(1), body)?
            }
            Stmt::Do(cond, body) => {
                self.begin_loop_block();
                let start = self.current_no();
                for line in body {
                    self.push_stmt_with_pos(line)?;
                }
                self.push_expr(cond)?;
                let end = self.mark();
                self.push(Instruction::goto(start));
                self.insert(end, Instruction::goto_if_not(self.current_no()));
                self.end_loop_block(start);
            }
            Stmt::While(cond, body) => {
                self.begin_loop_block();
                let start = self.current_no();
                self.push_expr(cond)?;
                let end = self.mark();
                for line in body {
                    self.push_stmt_with_pos(line)?;
                }
                self.push(Instruction::goto(start));
                self.insert(end, Instruction::goto_if_not(self.current_no()));
                self.end_loop_block(start);
            }
            Stmt::For(var, args, body) => self.push_for(Some(var), args.0, args.1, args.2, body)?,
            Stmt::If(else_ifs, else_part) => {
                let mut end_stack = Vec::with_capacity(32);

                for (cond, body) in else_ifs {
                    self.push(Instruction::report_position(cond.1));
                    self.push_if(cond.0, body)?;
                    end_stack.push(self.mark());
                }

                for line in else_part {
                    self.push_stmt_with_pos(line)?;
                }

                for end in end_stack {
                    self.insert(end, Instruction::goto(self.current_no()));
                }
            }
            Stmt::Assign(var, add_op, rhs) => {
                if let Some(add_op) = add_op {
                    self.push_var(var.clone())?;
                    self.push_expr(rhs)?;
                    self.push(Instruction::binop(add_op));
                } else {
                    self.push_expr(rhs)?;
                }

                self.store_var(var)?;
            }
            Stmt::Call {
                name,
                args,
                try_body,
                catch_body: catch,
                is_jump,
                is_method,
            } => {
                let count = self.push_opt_list(args, |idx, out| {
                    log::error!("TODO: {name:?}({idx})");
                    out.push(Instruction::load_int(0));
                    Ok(())
                })?;
                self.push_expr(name)?;

                if let Some(catch) = catch {
                    if is_jump {
                        self.push(Instruction::try_jump(count));
                    } else {
                        self.push(Instruction::try_call(count));
                    }
                    let try_top = self.mark();
                    let try_top2 = self.mark();
                    for try_body in try_body {
                        self.push_stmt_with_pos(try_body)?;
                    }
                    let try_end = self.mark();
                    let catch_top = self.current_no();
                    for stmt in catch {
                        self.push_stmt_with_pos(stmt)?;
                    }
                    self.insert(try_top, Instruction::goto_if_not(catch_top));
                    if is_method {
                        self.insert(try_top2, Instruction::pop());
                    }
                    self.insert(try_end, Instruction::goto(self.current_no()));
                } else {
                    #[allow(clippy::collapsible_else_if)]
                    if is_jump {
                        self.push(Instruction::jump(count));
                    } else {
                        self.push(Instruction::call(count));
                        if is_method {
                            self.push(Instruction::pop());
                        }
                    }
                }
            }
            Stmt::Goto {
                label,
                catch_body: catch,
            } => {
                self.push_expr(label)?;

                if let Some(catch) = catch {
                    self.push(Instruction::try_goto_label());
                    let catch_top = self.mark();
                    for stmt in catch {
                        self.push_stmt_with_pos(stmt)?;
                    }
                    self.insert(catch_top, Instruction::goto_if_not(self.current_no()));
                } else {
                    self.push(Instruction::goto_label());
                }
            }
            Stmt::Label(label) => {
                if self.goto_labels.insert(label, self.current_no()).is_some() {
                    return Err(CompileError::DuplicatedGotoLabel);
                }
            }
            Stmt::Begin(ty) => {
                self.push(Instruction::begin(ty));
            }
            Stmt::Alignment(align) => {
                self.push(Instruction::set_aligment(align));
            }
            Stmt::Command(command, args) => {
                let count =
                    self.push_opt_list(args, |idx, out| default_arg_command(command, idx, out))?;
                self.push(Instruction::load_int(count as i32));
                self.push(Instruction::builtin_command(command));
            }
            Stmt::Method(meth, args) => {
                let count =
                    self.push_opt_list(args, |idx, out| default_arg_method(meth, idx, out))?;
                self.push(Instruction::load_int(count as i32));
                self.push(Instruction::builtin_method(meth));
                self.push(Instruction::store_result());
            }
            Stmt::Times(var, ratio) => {
                self.push_var(var.clone())?;
                self.push(Instruction::times(ratio));
                self.store_var(var)?;
            }
        }

        Ok(())
    }

    fn push_if(&mut self, cond: Expr, body: Vec<StmtWithPos>) -> CompileResult<()> {
        self.push_expr(cond)?;
        let begin = self.mark();

        for line in body {
            self.push_stmt_with_pos(line)?;
        }
        self.insert(begin, Instruction::goto_if_not(self.current_no() + 1));

        Ok(())
    }

    fn begin_loop_block(&mut self) {
        self.continue_marks.push(Vec::new());
        self.break_marks.push(Vec::new());
    }

    fn end_loop_block(&mut self, continue_no: u32) {
        for continue_mark in self.continue_marks.pop().unwrap() {
            self.insert(continue_mark, Instruction::goto(continue_no));
        }

        for break_mark in self.break_marks.pop().unwrap() {
            self.insert(break_mark, Instruction::goto(self.current_no()));
        }
    }
}

pub fn compile_stmt(stmt: Stmt) -> CompileResult<Box<[Instruction]>> {
    let mut compiler = Compiler::new();

    compiler.push_stmt(stmt)?;

    Ok(compiler.out.into_boxed_slice())
}

pub fn compile_expr(expr: Expr) -> CompileResult<Box<[Instruction]>> {
    let mut compiler = Compiler::new();

    compiler.push_expr(expr)?;

    Ok(compiler.out.into_boxed_slice())
}

pub fn compile(func: Function) -> CompileResult<CompiledFunction> {
    let mut compiler = Compiler::new();

    for stmt in func.body {
        compiler.push_stmt_with_pos(stmt)?;
    }

    Ok(CompiledFunction {
        header: func.header,
        goto_labels: compiler.goto_labels,
        body: compiler.out.into_boxed_slice(),
    })
}

fn default_arg_method(
    method: BuiltinMethod,
    idx: usize,
    out: &mut Vec<Instruction>,
) -> CompileResult<()> {
    match method {
        _ => Err(CompileError::NoArgumentForMethod(method, idx)),
    }
}

fn default_arg_command(
    command: BuiltinCommand,
    idx: usize,
    out: &mut Vec<Instruction>,
) -> CompileResult<()> {
    match command {
        _ => Err(CompileError::NoArgumentForCommand(command, idx)),
    }
}
