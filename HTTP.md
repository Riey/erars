# GET /

```rust

struct Ret {
    current_req: Option<InputRequest>,
    bg_color: Color,
    hl_color: Color,
    lines: [{parts: [ConsoleLinePart]}]
}

struct InputRequest {
    generation: u32,
    ty: InputRequestType,
    timeout: Option<Timeout>,
    is_one: bool,
}

enum InputRequestType {
    Anykey,
    EnterKey,
    Int,
    Str,
}

struct Timeout {
    /// Unix timestamp in nanos
    timeout: i128,
    show_timer: bool,
    timeout_msg: String,
}

enum ConsoleLinePart {
    Text(String, TextStyle),
    Line(String, TextStyle),
    Button([(String, TextStyle)], {Int or String}),
}

struct TextStyle {
    color: Color,
    font_family: String,
    font_style: FontStyle,
}

bitset FontStyle {
    NORMAL = 0x0;
    BOLD = 0x1;
    ITALIC = 0x2;
    STRIKELINE = 0x4;
    UNDERLINE = 0x8;
}

type Color = [u8; 3];

```

## Query

?from=`line_no` 를 붙이면 해당 라인 번호부터 시작하는 결과를 전송함 범위를 넘으면 빈 배열 반환.

# POST /input

유저입력을 body에 담아서 전송

InputRequest가 Anykey, EnterKey면 형식은 상관없음.

Int일시엔 꼭 숫자 문자열로 전송 Str이면 그냥 그대로 전송

# GET /listen

웹소켓 연결 가능

연결시 이밴트 발생시 메세지로 전송해줌

## 이밴트 목록

* REDRAW(1)

* TIMEOUT(2)
