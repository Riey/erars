# HANDOUT — eraTHYMKR 파싱·컴파일 최속·최정확 리서치 (2026-09-02 ~ 09-03)

**상태 (2026-09-03): 리서치 완료 + 구현 완료.** 파서·렉서·컴파일러·로더 권고가 실제 구현·측정됨 — 아래 6a(구현/미구현)·6b(구현 내역·측정) 참조.

다른 세션이 이어받기 위한 컨텍스트. 리서치 내역은 아래 1–5c, 구현·측정 내역은 6a/6b. (상태는 위 첫 문단의 한 줄 참조.)
갱신 시각: 2026-09-03.

**보고서 원본 (durable, 리포 안 미추적)**: `docs/research/2026-09-03-erathymkr-parse/` — `00-research-prompt.md`, `10-kimi-report.md`, `20-opus-report.md`(§7 사후 조정 포함), `30-kimi-review.md`, `expr-V0-to-V1.diff`(권고 #1의 실제 diff, 31줄), `phases-harness.rs`(단계별 타이밍 + 순서 결정성 검증 하네스; `crates/erars-loader/examples/phases.rs`로 놓고 `cargo run --release -p erars-loader --features multithread --example phases`), `order2.txt`. `/tmp` 스크래치는 재부팅 시 사라지므로 이쪽을 쓸 것.

## 1. 요청과 운영자 결정

- 요청: "opus·kimi·glm으로 `~/repos/eraTHYMKR`을 가장 빠르고 정확하게 파싱·컴파일하는 방법을 리서치".
- 운영자 결정 (09-02 밤): **glm 제외** (zai 플랜 한도 초과, zen glm-5.2도 쓰지 말 것). **리뷰 = kimi-k3, 작업 = opus.**
- 끝나면 이 파일에 컨텍스트를 정리해 둘 것 (운영자 지시).

## 2. 대상과 파이프라인

| 항목 | 값 |
|---|---|
| eraTHYMKR | Emuera 1.8.1.8 kr3 게임. ERB 873개(erars 글롭 `ERB/**/*.ERB`로는 857개 로드), 1.45M 줄, 59MB, UTF-8 BOM, ERH 7, CSV 251 |
| erars 브랜치 | `new-renderer` (a8190fd). 추적 파일은 리서치 중 수정 없음 |
| 파이프라인 | `crates/erars-loader/src/lib.rs` `run_script`: glob → CSV(par_bridge) → ERH 병합(직렬) → ERB `parse_and_compile`(rayon par_bridge, `lib.rs:333-356`) → `insert_compiled_func`(직렬) → lint. `save_script/load_script` = game.era (erars-bytecode + rmp-serde + memmap2) |
| 파서 | `crates/erars-compiler/src/parser.rs`(ParserContext, Preprocessor+PP_REGEX, nom 7), `parser/expr.rs`, `compiler.rs`; 렉서 `crates/erars-lexer`(logos 0.14) |
| CLI | `crates/erars-stdio` (`--save`/`--load`/`--measure-memory`/`--quite`/`--log-level`). **`--use-input /dev/null`은 패닉** (빈 RON). `< /dev/null`로 실행 |
| 빌드 | `cargo build --release -p erars-stdio` → `target/release/erars-stdio`. stdio는 loader의 `multithread` 기본 활성 |

## 3. 베이스라인 (32 스레드)

| 측정 | 리드(erars-a3 세션) | kimi (웜 캐시, hyperfine 10회) |
|---|---|---|
| `--save` 전체 | 522ms ±84 (3회) | **237ms ±6** (콜드 첫 실행 443ms) |
| Parse/Compile ERB 단계 | 383ms | 176–285ms (직렬 insert 포함) |
| `--load --measure-memory` | 167ms ±20 | 91ms ±9 |
| 직렬 parse+compile | — | 1.7s (렉스 128ms, 읽기 19ms, insert 22ms, write 37ms) |
| 병렬 스케일 | — | 4t 355 / 8t 225 / 16t 148 / 32t 126ms, 8t 이후 평탄 |
| 파일 하한 | — | EVENT_K42.erb 1.89MB, 106ms |
| 진단 | 파스 오류 0. 경고: 알 수 없는 CSV JUEL·NOWEX, `[SKIPEND]` TODO 21건 | 동일 (E1000/E2000/W1001 모두 0) |

## 4. kimi 보고서 핵심 (`/tmp/claude-1000/-home-riey-repos-erars/6648b565-fad0-4a94-b80a-ec88c824873c/scratchpad/kimi/report.md`)

정확성:
- **이벤트 함수 392개 등록 순서 비결정** — `par_bridge().collect()` 순서 불특정, Emuera는 파일명 정렬 로드. `lib.rs:333-356` → `crates/erars-vm/src/function.rs:238-262` (None 플래그 이벤트는 `insert(0)`로 역순).
- 중복 일반 함수 25개(`TROPHY_FEAT_GET_`×18 등) 승자 무작위 (`function.rs:234-235`).
- game.era 바이트 매 실행 상이 (hashbrown RandomState + ThreadedRodeo 번호). MD5 불일치 확인.
- `[IF 심볼]` 블록 무조건 스킵 (`erars-lexer/src/lib.rs:230-247`); 이 게임은 심볼이 주석 처리돼 우연히 일치.
- `[SKIPEND]` 21건 무해, JUEL/NOWEX는 별칭으로 흡수, 진단 개수 표시 버그(`lib.rs:387-401`).

속도: 직렬 시간의 ~92%가 nom 표현식 파서(`expr.rs`: form 문자열 String 할당, `ident_no_case` Cow 대문자화, Binop/Unary Box). 융합 `parse_and_compile`이 2단계보다 8% 빠름(유지).

권고 순위: ① 파일 목록 정렬 후 `par_iter` 인덱스 병렬로 순서 결정성 → ② 파일별 증분 캐시(content+header+compiler 버전 해시) → ③ nom → 손 재귀하강 파서(직렬 5–10× 기대) → ④ 저장 경로 BTreeMap 재구축 제거 → ⑤ 함수 단위 병렬 → ⑥ `[IF]` 의미 수정 → ⑦ 진단 개수 → ⑧ 인터너 번호 결정화.

## 5. opus 보고서 핵심 (`.../scratchpad/opus/report.md`, 254줄; 1차 실행은 사망, 2차 `opus-research-2`가 01:20 완료)

측정 조건 주의: 박스에 무관한 GPU/영상 작업이 돌아 load 14~33. 절대값은 섹션 간 비교 금지, 같은 hyperfine 호출 안의 상대 비교만 신뢰.

**헤드라인 (실측)**: 병목은 파서 구조·렉서·인터너·rayon이 아니라 **nom `VerboseError`** — 실패한 alt 분기마다 `vec![(input, kind)]` 힙 할당 + `context()`가 전파 중 push. `single_expr`(`expr.rs:458-490`)은 14분기 alt라 식별자마다 12분기 실패를 겪음. `nom::error::Error<&str>`로 교체(`expr.rs:15,23` 타입 별칭 + `:401`의 `err.map` 수정, ~5줄) 시:

| 변형 | 직렬 parse+compile (4라운드 중앙값) | 비고 |
|---|---:|---|
| V0 현행 | 1314ms | |
| V1 Error 교체 | **650ms (2.02×)** | 함수 16,859·인터너 334,738·game.era 크기 동일 |
| V2 alt 순서만 (`ident_or_method_expr` 3번째로) | 1108ms (1.19×) | |
| V3 둘 다 | 622ms (2.11×) | V1 위에선 +4%뿐 → 선택 사항 |

엔드투엔드(같은 hyperfine 호출): `--save` 32t 230→190ms(-17%), compile-only 32t 197→158ms(-20%), **4t 363→251ms(-31%)**, user CPU 1772→1186ms(-33%). 리스크: E2000 메시지의 `context()` 라벨 소실 → 권고 #2(실패 시에만 VerboseError로 재파싱).

**직렬 예산**: read 15 → lex 71 → parse+compile 1314 → insert 10 → write 22ms. 렉스는 5.4%.
**스케일링**: 1/2/4/8/16/32t = 1268/694/367/220/126/127ms → 16t까지 10.1×, 물리 코어 넘어 SMT에서 평탄(kimi의 "8t 이후 평탄" 반박). V1 이후 32t 63ms vs 최악 파일 47ms → 파일 단위 병렬 거의 소진.
**문제 아님(실측)**: 인터너 경합(웜 패스 동일 속도), 파일 읽기(15ms), 전처리 DFA, insert 직렬(10ms), 저장(22ms, 단 BTreeMap 재구축은 낭비).

**정확성 (kimi 검증 결과)**: 등록 순서 비결정 **확인·강화**(같은 프로세스에서 두 번 collect하면 16,859 위치 전부 다름; glob 자체는 정렬됨 → rayon이 깨뜨림). 중복 함수는 25→**15개**(각 2회, 대문자 기준). game.era MD5 3회 3종. `[IF]` 스킵 확인. 추가 발견: **`EventFlags::None`이 `insert(0)`로 역순 등록**(`function.rs:258`, Pre는 `insert(empty_count)`); BOM 없는 ERH 2개가 Shift-JIS 폴백(`erars-reader/src/lib.rs:15`, 지금은 ASCII라 무해); `from_utf8_unchecked` 비건전성(`:18-20`).

**권고 순위 (opus)**: #1 VerboseError→Error(실측 2×) → #2 실패 시에만 Verbose 재파싱으로 진단 품질 유지 → #3 정렬된 `Vec<PathBuf>` + 인덱스 `par_iter`(결정성; 하네스 실측 122–138ms vs par_bridge 152ms로 느려지지 않음) → #4 `insert(0)` 역순 판정(Emuera 의미 확인 필요) → #5 중복 정의 진단+승자 확정(Emuera는 첫 정의 유지+경고) → #6 `[IF]` 매크로 해석 → #7 큰 파일 먼저(LPT) 정렬 → #8 `write_to` BTreeMap→Vec → #9 재현 가능한 game.era(증분 캐시 전제) → #10 파일별 증분 캐시 → #11 mmap 차용+UTF-8 검증 → #12 BOM 없을 때 설정 인코딩(CP949) → #13 함수 단위 병렬 → #14 진단 개수 수정.
**손 재귀하강 파서(kimi #3)에 대한 의견**: #1 이후 32t에서 63ms vs 47ms 하한이라 다코어 벽시계 이득 작음; CPU·저코어용으론 여전히 유효하나 #1 먼저 하지 않으면 효과가 재작성에 잘못 귀속됨.

**열린 질문**: Emuera의 무플래그 이벤트 실행 순서(로드 순 vs 역순), 중복 함수 규칙(첫/마지막), V1 잔여 650ms 중 String/Cow vs Box 비중(다음 절제: Bump `Expr`), V1 E2000 메시지 저하 실측, `--load` 91~128ms 편차 원인, `benches/parse.rs`가 합성 입력이라 회귀 못 잡음.

산출물: `opus/expr.V{0,1,2,3b}.rs`, `opus/bin/phases_V*`·`erars-stdio-V1`, 스크립트 `ab.sh ablate.sh measure2.sh e2e.sh final.sh`, 원시 출력 `*.txt`, `hf_final.json hf_e2e.json`, `diag.log`(전체 진단 23줄).

## 5b. kimi 리뷰 결과 (`.../scratchpad/kimi/review.md`, 74줄; 잡 oc_23b112, $0.87+0.87)

- **헤드라인 검증됨**: opus 바이너리 `bin/phases_V0/V1` 재실행 4라운드 중앙값 **1411 → 721ms (1.96×)**; hyperfine 같은 호출에서 `--measure-memory` **279.6 → 165.8ms (-41% 벽시계), user 1764 → 1151ms (-35%)** — opus의 -20%보다 크게 나온 건 opus 측정 창의 부하 때문. game.era 크기 두 변형 동일(50,354,238B), 실행 간 MD5는 여전히 상이(V1이 비결정성에 영향 없음).
- **쟁점 판정**: 스케일링 평탄 지점 → opus 옳음(16→32t에서도 V0 162.6→125.3, V1 73.4→60.8ms로 아직 늘어남). 최악 파일 → opus 옳음(직렬 재측정 DAILY_LIFE_SCRIPT 69ms 1위, K42는 32t 경합으로 부풀려짐). 중복 함수 → opus 방향 옳음, 단 kimi가 대문자화해 다시 세면 **13개**(opus 15, 재조정 필요). 이벤트 392 vs 391 → 단독 이벤트(EVENTBUY/FIRST/LOAD) 포함 여부 차이, 양쪽 다 맞음. BOM 없는 ERH Shift-JIS 폴백·`from_utf8_unchecked` UB → kimi가 놓친 실제 발견으로 인정.
- **opus 지적 사항**: "16,859 위치 전부 다름"은 근거 미제시(과장 표현), 중복 15는 13일 가능성, alt 재정렬(V2)은 스스로 불필요하다면서 권고에 남긴 자기모순, 진단 품질 대안(#2)의 "feature-selected"와 이중 단형화 충돌 설명 부족.
- **놓친 리스크**: R1 `from_utf8_unchecked` 소유 UB(높음), R3 `insert(0)` 의미 판정이 결정성 수정(#2)을 막음(중간).
- **통합 권고 순위 (kimi 최종)**: ① nom Error 교체(실측) → ② 정렬 Vec + 인덱스 par_iter(결정성) → ③ `EventFlags::None` 의미 판정 → ④ 중복 정의 진단+승자 확정 → ⑤ 재현 가능 game.era → ⑥ `[IF]` 매크로 해석 → ⑦ 실패 시 Verbose 재파싱 → ⑧ LPT 정렬 → ⑨ write_to BTreeMap 제거 → ⑩ reader UTF-8 검증+BOM 없을 때 설정 인코딩 → ⑪ 증분 캐시 → ⑫ 함수 단위 병렬 → ⑬ 진단 개수 → ⑭ benches 실제 코퍼스. kimi 자신의 "손 파서 재작성"은 ⑪~⑬ 아래로 강등.
- 주의: 첫 리뷰 잡(oc_d1dfab)은 resume에 tier를 안 붙여 default(deepseek)로 잘못 라우팅돼 취소함(**resume 시에도 tier 명시**). 두 번째(oc_d65586)는 write=false → plan 에이전트라 계획만 내놓음 → `agent: build`로 재개해야 실행함.

## 5c. opus 사후 조정 결과 (`20-opus-report.md` §7)

- **중복 함수 = 15개 확정** (컴파일된 함수 목록 기준, 각 정확히 2회). 핵심 분할: **9개는 같은 파일 안 중복**(뒤 정의가 결정적으로 이김, Emuera도 동일 → 순수 게임 버그), **6개만 파일 간 중복**(`KOJO_243_TRAIN_MESSAGE_S_COM_243`, `KOJO_MESSAGE_MARKCNG_19`, `TRY_13`, `TRY_51`, `TRY_52`, `TRY_59`)이라 승자가 비결정적. 권고 "중복 진단"은 15개 경고 + 6개 정확성 리스크로 범위 조정.
- **"16,859 위치 전부 다름"**: 한 프로세스에서 파이프라인을 두 번 돌려 (file, name) 순서 비교 — 정렬 후 멀티셋 동일(하네스 버그 아님), 위치별 일치 0. 단일 시행이므로 "내가 돌린 시행에선 한 위치도 같지 않았다"로 표현 완화.
- **이벤트 391 vs 392**: opus는 컴파일 함수 이름이 EventType으로 파싱되는 것(EVENTTRAIN 191, EVENTEND 163, TURNEND 11, COM 10, COMEND 8, SHOP 5, BUY/FIRST/LOAD 각 1), kimi는 소스 수준 집계(EVENTEND 164). 결론 불변.
- **-20% vs -41%**: CPU 시간은 -33%/-35%로 일치(부하 무관 지표, 이걸 인용). 벽시계는 부하 창 차이. **인용 문구: CPU -33~-35%, 벽시계 -20~-41%(부하 의존), 4스레드에서 -31%.**
- **통합 순위 수용**, 단 두 가지 조정: kimi #7(실패 시 VerboseError 재파싱)을 #1 바로 옆으로(같이 배포하지 않으면 모든 E2000 메시지 저하; cargo feature가 아니라 같은 제네릭 파서의 두 번째 단형화로 구현), V2 alt 재정렬은 완전히 제외(#1 이후 4%뿐), kimi R1(`from_utf8_unchecked` 건전성)은 #10보다 위로.

## 6a. 최종 권고 순서 → 구현 상태

아래는 리서치 종료 시 두 모델이 합의한 권고 순서. **✓ = 구현됨, ✗ = 미구현(남은 작업).**

| # | 항목 | 상태 |
|---|---|---|
| 1 | `expr.rs` 오류 타입 `VerboseError` → 비할당 `ExprError` + `single_expr` 14분기 `alt` → 바이트 디스패치 + `binop` 손작성 + `bin_expr`/`calculate_binop_expr` 비할당화 | ✓ 구현 (6b) |
| 1′ | 실패 시에만 VerboseError로 재파싱해 E2000 메시지 유지 (`try_nom!`) | ✗ 불필요해짐 — `ExprError`가 위치 + 최내부 context 라벨을 그대로 들고 있어(`or`/`deeper`가 최심 실패 유지, `add_context`가 최내부 라벨 유지) 재파싱 없이 메시지 유지. `try_nom!`은 그대로 |
| 2 | ERB 경로 정렬 `Vec<PathBuf>` + 인덱스 `par_iter().flat_map()`(결정성), 큰 파일 먼저(LPT) | ✓ 구현 |
| 3 | Emuera 의미 판정(무플래그 `#EVENT` 순서, 중복 정의 승자) → 판정 후 진단 | ✗ 미판정 (남은 작업) |
| 4 | `read_file` `from_utf8_unchecked` 검증, BOM 없을 때 설정 인코딩(CP949) | ✗ 미구현 (남은 작업) |
| 5 | `[IF 심볼]`을 `#DEFINE`에 대해 실제 평가 | ✗ 미구현 (남은 작업) |
| 6 | 재현 가능한 game.era(`write_to` BTreeMap 재구축 제거+고정 순서), 증분 캐시 | ✗ 미구현 (남은 작업) |
| 7 | 함수 단위 병렬, 진단 개수 수정, `benches/parse.rs` 실제 코퍼스, 손 재귀하강 파서 | ✗ 미구현(단, CPU·저코어 이득은 #1 이후 감소) |

권고 밖에서 함께 구현된 항목(6b): `parse_form_normal_str`/`parse_str_inner` `Cow` 반환 + memchr 2단계 스캔, `upper_no_case` ASCII 패스트패스, 파일 단위 식별자 인터너 캐시(`intern_ident`), 렉서 `InstMemo`(64슬롯 직접 매핑), 식별자 프레디킷 ASCII 비트 마스크 + 전 코드포인트 동치 테스트, `cut_ident`/`cut_comment` 바이트 스캔, 컴파일러 `with_capacity` → `Vec::new()`.

**#2 변이**: 권고는 인덱스 `par_iter().flat_map()`이었으나, 실제로는 `par_iter`가 연속 범위로 쪼개 largest-first 밸런싱을 무너뜨려(실측 같은 벽시계, 피크 RSS +8~18MB) **largest-first 큐 위 `par_bridge`**로 구현함.

## 6b. 구현 내역 (2026-09-03)

### 변경 요약 (crates/erars-compiler, crates/erars-lexer, crates/erars-loader)
- **expr.rs (라운드 1)**: nom `VerboseError` → **비할당 `ExprError`**(위치 + 최내부 context 라벨만, 힙 없음; `or`/`deeper`가 최심 실패 유지, `add_context`가 최내부 라벨 유지 → **권고 1′의 "실패 시 VerboseError 재파싱"은 불필요해짐**, 재파싱 없음). `single_expr`의 14분기 `alt` → **첫 바이트 디스패치 `single_expr_atom`**. `binop` 손작성(21분기 `alt` 제거). `bin_expr`/`calculate_binop_expr`는 공통 경로에서 할당 없음.
- **expr.rs (라운드 2)**: `parse_form_normal_str`/`parse_str_inner`가 `Cow` 반환 — **실제 이스케이프에서만 할당**, memchr 기반 2단계 스캔. `upper_no_case` ASCII 패스트패스가 `cow_to_uppercase` 대체. 파일 단위 식별자 인터너 캐시(`ParserContext::intern_ident`) — **리터럴 텍스트는 의도적으로 캐시 안 함**(측정 4.5% 느려짐).
- **erars-lexer**: `InstructionCode::from_str`용 **64슬롯 직접 매핑 `InstMemo`**(패스당 라인별 1회 룩업, 890,801회 중 52% 미스, 미스도 메모). 식별자 프레디킷을 **ASCII 비트 마스크**로 + 전 코드포인트 동치 테스트. `cut_ident`/`cut_comment` **바이트 스캔** (unsafe는 +0.4%라 제거).
- **erars-compiler**: `parse_and_compile`의 `Vec::with_capacity(1024)` 및 IF 블록당 `with_capacity(128)` → **`Vec::new()`**(코퍼스 패스당 ~1.8GiB의 미사용 예약 용량 제거).
- **erars-loader**: ERB 경로를 **정렬된 `Vec<PathBuf>`**로 수집(결정적 등록 순서; 다중 실행·`multithread` off에서도 동일함을 검증), **largest-first 큐 위 `par_bridge` 디스패치**(인덱스 `par_iter().flat_map()`은 연속 범위로 쪼개 밸런싱을 무너뜨려 배제; 실측 같은 벽시계, +8~18MB 피크 RSS), `Vec<(usize, CompiledFunction)>` 플랫 수집 + 안정 정렬. **이전 중첩 `Vec<(usize, Vec<..>)>` 형태는 파일별 vec이 112KiB 용량을 유지해 +102MB 피크 RSS의 원인이었음.**

### 결과 표 (본 세션 실측, interleaved 6회씩 중앙값; now = 현재 워킹트리, baseline = pristine HEAD)

`erars-stdio --save --quite </dev/null`:

| tc | baseline real | baseline user | baseline RSS MB | now real | now user | now RSS MB | real speedup | user speedup |
|---|---|---|---|---|---|---|---|---|
| 1 | 1.194s | 1.106s | 238.6 | 0.428s | 0.374s | 235.7 | 2.79× | 2.95× |
| 2 | 0.655s | 1.154s | 247.4 | 0.288s | 0.420s | 243.0 | 2.27× | 2.74× |
| 4 | 0.396s | 1.195s | 265.5 | 0.192s | 0.426s | 252.2 | 2.06× | 2.80× |
| 8 | 0.294s | 1.377s | 296.7 | 0.177s | 0.577s | 282.1 | 1.66× | 2.39× |

`phases` 하네스(`RAYON_NUM_THREADS=4`, 3회, CPU min):

| 단계 | baseline cpu min | now cpu min |
|---|---|---|
| preprocess+lex serial | 71.9ms | 59.4ms |
| parse+compile serial | 993.6ms | 286.8ms |

함수 수 16,859, 인터너 334,738은 두 빌드 동일. (이전에 로더가 중첩 Vec 형태일 땐 now가 baseline보다 피크 RSS가 +100MB였으나, 로더를 플랫 `Vec` 형태로 바꾸며 now가 오히려 baseline보다 낮아짐.)

### 남은 작업
- game.era 바이트 비결정성(인터너 번호)
- 증분 캐시(파일 내용 + HeaderInfo + 컴파일러 버전 해시)
- `[IF 심볼]` 매크로 해석
- `EventFlags::None`의 `insert(0)` 의미 판정(Emuera 소스 확인 필요)
- 중복 정의 진단(파일 간 6개 비결정 승자)
- reader의 `from_utf8_unchecked` 검증 / BOM 없는 ERH 설정 인코딩(CP949)
- 진단 개수 표시 버그(`take(40)`/`checked_sub(20)`)

### 벤치 하네스
`crates/erars-loader/examples/phases.rs`는 이제 **상시 타이밍·프로파일링 하네스**로 유지됨.
- 실행: `cargo run --release -p erars-loader --features multithread --example phases -- <게임Dir> [rounds]` (최소 벽시계/중앙값 + CPU 시간을 단계별로 출력; `RAYON_NUM_THREADS=<n>`.)
- `PHASES_SERIAL_ONLY=1`: 직렬 단계만 실행(병렬 파트 생략).
- `PHASES_PROFILE=1`: in-process SIGPROF 샘플링으로 직렬 parse+compile self-time 히스토그램 출력.
- dev-dependencies `libc`/`mimalloc`/`pprof`를 `crates/erars-loader/Cargo.toml`에 추가함(하네스가 의존).

## 6. 인프라 메모 (09-02)

- opencode 위임: **zen(`opencode/...`) 프로바이더만 동작.** zai-coding-plan / opencode-go / anthropic은 인증 복구 후에도 접수 0초에 `Model not found: <provider>/<model>. Did you mean: <model>?` (잡 oc_5e5e64, oc_c30196, oc_e0897d, oc_c0a601, oc_6bd289). 서버 `/provider`는 해당 모델을 나열함 → 플러그인의 모델 참조 형식 vs opencode 1.18.25 불일치 의심. opencode-cc-rs 세션 없어 서버는 건드리지 않음.
- `oc_list`는 옛 세션 툴 목록에 없음(새 세션에서 확인).
- rieypc(ssh 프록시 경로) 09-02 밤 connection refused.
- kimi 잡 oc_39a785: 26분, $4.43, 정상 완료.

## 7. 정리 상태 (01:50 완료)

- 추적 변경: erars — `Cargo.lock`, `crates/erars-compiler/src/parser.rs`, `parser/expr.rs`, `crates/erars-lexer/src/lib.rs`, `utils.rs`, `crates/erars-loader/Cargo.toml`, `src/lib.rs`가 `M`. eraTHYMKR — 추적 변경 없음.
- 새(non-tracked) 파일: erars — `crates/erars-loader/examples/phases.rs`(상시 하네스, dev-deps `libc`/`mimalloc`/`pprof` 추가, 실행법은 6b), `crates/erars-lexer/src/inst_memo.rs`(새 렉서 모듈, 현재 미추적). `HANDOUT.md`, `docs/research/…`, 세션 전 파일 `2026-09-02-120202-ultracode-rust-gpu.txt`는 미추적 잔여.
- **`~/repos/eraTHYMKR/game.era` 삭제함** — 이번 벤치 실행이 만든 `--save` 산출물(untracked, `check-ignore` 없음). `last_log.log` 심링크도 삭제, eraTHYMKR `git status --short`는 비움. `logs/`는 gitignore 대상이라 둠.
- `/tmp` 스크래치(`.../scratchpad/opus/`에 V0~V3 변형·바이너리·스크립트·원시 출력, `kimi/`에 hyperfine JSON)는 재부팅 전까지만 존재. 필요한 것은 위 docs/research로 복사해 둠.
- 비용: kimi 리서치 $4.43 + 리뷰 $1.73, opus는 Claude 구독. glm zen 잡 $0.18 낭비(중단).

## 8. Emuera 명령·함수 패리티 작업 (2026-09-03, 6·7절 이후)

§6b의 성능 작업과 **별개 작업**이다. 대상은 Emuera 명령/함수 커버리지이며, 성능 관련 코드는 건드리지 않았다. 유일한 권위는 WebEmuera 소스(`/tmp/webemuera/WebEmuera-master/WebEmuera/Emuera/`, 재다운로드 명령은 §8 끝).

### 구현 범위

| 열거형 | pristine `a8190fd` | 현재 | 실행기 arm 보유 |
|---|---:|---:|---:|
| `BuiltinCommand` | 81 | **113** | 113 / 113 |
| `BuiltinMethod` | 96 | **124** | 124 / 124 |
| `InstructionCode` | 294 | **295** | 파서 arm 295 / 295 |

`BuiltinCommand`/`BuiltinMethod`의 **기존 discriminant는 하나도 재배치되지 않았다**(검증: 두 열거형의 base↔now 매핑 비교에서 renumbered 0, 재사용 슬롯 0, 중복 0). 유일한 이동은 `SpriteCreate`로, Emuera에선 명령이 아니라 **메서드**다(`SpriteCreateMethod`, `Creator.cs:194`) — `BuiltinCommand` 301에서 `BuiltinMethod` 201로 옮겼고 **301은 비워 뒀다**(`ArrayMSort = 302`부터 시작). 디코딩이 `unsafe transmute`(`crates/erars-compiler/src/instruction.rs:57-65`)이고 `game.era`에 **버전 게이트가 없으므로**(`crates/erars-loader/src/lib.rs:62-82`) 슬롯 재사용은 낡은 `game.era`를 다른 명령으로 조용히 디코딩시킨다.

가족별로: 그래픽·스프라이트 23개(`crates/erars-vm/src/graphics.rs` 신설), 세이브/글로벌/텍스트 12개, `PRINT_*` 계열 15개, 라인헤드 메서드 형태 26개, TOOLTIP 3개, 리스트/트레인 4개, 캐릭터 추가·삭제·검색 6개, `HTML_PRINT`(`crates/erars-vm/src/html.rs` 신설, 선형 스캐너), `FORCEKANA`(`crates/erars-ui/src/kana.rs` 신설), `SORTCHARA`, `STRDATA`, `AWAIT`, `OUTPUTLOG`, `ARRAYMSORT`, 기본 `@BEGIN TITLE` 화면.

### `InstructionCode` 폴스루 0 — 컴파일러가 강제함

`parser.rs`의 포괄 arm(`inst => { log::warn!("...not yet implemented"); Stmt::Command(Throw, ...) }`)을 **삭제**했다. `match inst`(`crates/erars-compiler/src/parser.rs:1506`)는 이제 `InstructionCode` 전체에 대해 **exhaustive**이므로, arm 하나를 빼면 빌드가 깨진다 — 문서 주장이 아니라 타입 검사로 보장된다.

마지막까지 포괄 arm에 떨어졌던 18개의 처리:
- **실제 누락 1개 — `STRDATA`**: `PRINTDATA` 하위 블록이 아니라 **독립 문**이다(`FunctionIdentifier.cs:302`, `funcMatch[STRDATA]="ENDDATA"`는 `:460`). 자기 `DATA`/`DATAFORM`/`DATALIST` 블록을 열고 한 항목을 무작위로 뽑아 파트를 `"\n"`으로 이어 붙여 변수(기본 `RESULTS:0`)에 대입한다(`ArgumentBuilder.cs:1640-1648`, `Process.ScriptProc.cs:750-774`). 빈 블록은 **아무것도 대입하지 않는다**(`:752-757`). `Stmt::StrData`(`erars-ast/src/ast.rs:36-41`), 파서 `parser.rs:2119-2151`, 공용 `read_data_block`(`parser.rs:1324-1409`), 컴파일러 `compiler.rs:462-480`.
- **블록 구분자 17개**(`CASE CASEELSE CATCH DATA DATAFORM DATALIST ELSE ELSEIF ENDCATCH ENDDATA ENDIF ENDLIST ENDSELECT LOOP NEXT REND WEND`): 정상 경로에선 각자의 블록 파서가 소비한다. 최상위 `match inst`에 도달했다는 건 **미아 구분자**라는 뜻이고 Emuera는 이를 치명적 로드 에러로 보고한다(`ParserMediator.Warn(…, 2, true, false)`, `ErbLoader.cs:1063-1420`). 이제 Emuera 원문 메시지를 그대로 낸다(`parser.rs:2153-2200`). 중첩 거부 3종(`NestedStrdata`/`StrdataInsidePrintdata`/`PrintdataInsideStrdata`, `Lang.cs:822-824`)도 재현.
- **날조된 별칭 1개 제거**: `FINDCHARADATA`(언더바 없음)는 Emuera 소스·코퍼스 모두 0회이고 실제 이름은 `FIND_CHARADATA`(`Creator.cs:63`). Emuera가 거부하는 표기를 받아주는 건 조용한 divergence이므로 `InstructionCode` variant와 파서 arm을 **삭제**했다(문서화가 아니라 제거).

실행기 스텁도 0이다. `crates/erars-vm/src/terminal_vm/executor.rs`에 `TODO|todo!|not implemented|unimplemented` 스윕 결과가 없다. 마지막 스텁은 `BeginType::Title`의 `todo!("Default TITLE")`였고, `run_default_title`(`executor.rs:612-691`)로 구현했다 — `beginTitle`의 내장 타이틀 화면과 `endOpenning`의 `[0]`/`[1]` 분기(`Process.SystemProc.cs:193-252`). 남은 `bail!` 둘은 스텁이 아니다: `executor.rs:438-442`는 손상된/미래 버전 `game.era`를 막는 가드(소스 텍스트로는 도달 불가), `BuiltinCommand::Throw`는 존재하지만 파서가 더는 발생시키지 않는다.

### 도중에 발견한 기존 버그 (패리티 작업과 별개의 실제 결함)

- **`PRINTDATA`의 인수는 선택자가 아니라 출력 변수다.** Emuera는 항상 `GetNextRand(count)`를 뽑고 뽑힌 인덱스를 인수 변수에 **쓴다**(`PrintDataArgument`, `ArgumentBuilder.cs:1619-1630`, `Instraction.Child.cs:213-217`). erars는 이걸 선택자로 읽었으므로 `PRINTDATA LOCAL`(초기값 0)은 **항상 0번 항목만 출력하고 `LOCAL`에 쓰지 않았다**. **코퍼스 73개 사이트**가 조용히 0번에 고정돼 있었다 — 미관 문제가 아니라 게임 플레이 버그.
- **`DATALIST` 항목은 파트마다 콘솔 줄이 하나다.** Emuera는 파트 사이에 `Console.NewLine()`을 넣고 명령 자신의 개행/대기는 마지막에 한 번만 적용한다(`Instraction.Child.cs:222-238`). erars는 파트를 한 문자열로 이어 붙였다.
- **`PRINTDATA`가 스택에 셀렉터를 흘렸다.** 분기 테이블은 각 분기가 자기 duplicate만 소비하므로 선택자가 스택에 남는다 — 선택된 분기 이후 한 번 `pop`이 필요하다(`compiler.rs:355-399`, 주석 포함).
- **`cut_comment`가 문자열 리터럴을 무시했다.** `memchr(b';')`로 무조건 잘랐기 때문에 `HTML_PRINT "&lt;"`가 `HTML_PRINT "&lt`로 절단됐다. Emuera의 `;`는 **토큰 위치** 주석이라 `SkipWhiteSpace`가 토큰 사이에서만 처리하고(`LexicalAnalyzer.cs:753-765`), `"…"` 안의 `;`는 `ReadString`이 먹어 도달하지 않는다. 리터럴 인식 스캔으로 교체(`crates/erars-lexer/src/utils.rs:16-64`), 핫패스는 memchr 2회 유지(이 함수는 코퍼스 890,801줄에 걸리므로 예산이 있다). 테스트 `cut_comment_skips_string_literals`(`utils.rs:226-250`).
- **`Sign`이 `-v`를 반환했다** (부호가 아니라 부정). `MoneyStr`은 **포맷 문자열 인수를 버렸다** — 지금은 커스텀 패턴 구현(의도적 미구현 범위는 `executor.rs:4029-4039`에 명시).
- **`単位の位置`(`unit_forward`) 기본값이 틀렸다.** Emuera 기본은 `前`=true(`ConfigData.cs:187`).
- **시스템 메뉴 문자열에 접두어가 박혀 있었다.** Emuera는 `システムメニュー0/1`을 **맨 문자열로** 저장하고 출력 시 `"[0] "`/`"[1] "`를 붙인다(`ConfigData.cs:193-194` vs `Process.SystemProc.cs:208-209`). `_Replace.CSV`로 덮어쓰는 게임이 접두어를 잃었을 것. 기본값을 맨 문자열로 고침(`parser.rs:135-140,161-162`).
- **`OUTPUTLOG` 확인 메시지**가 영문 날조 문구 + **원본 인수**였다(기본 인수일 때 파일명이 빈칸). Emuera 원문 `※※※ログファイルを{0}に出力しました※※※`(`Lang.cs:1284`)에 **해결된 경로 − 작업 디렉터리**를 넣는다(`EmueraConsole.Print.cs:704`).
- **CSV 정수 파싱을 strict로**: 비수치 값은 경고+`0`이 아니라 로드 에러(`csv_parse_int!`, `parser.rs:54-61`). 코퍼스 근거로 판단, 픽스처는 `csv_int_strict.{erb,out}`(구 `ignore_nonnumber`).

### 의도적 divergence 색인

**`docs/research/2026-09-03-emuera-command-gap.md` §5가 유일한 통합 목록이다.** 원인별로 5.1 픽셀 서피스 없음 / 5.2 호스트 로케일 없음 / 5.3 Emuera가 미지정으로 둔 순서를 erars가 좁힘 / 5.4 erars 전용 확장(`ARRAYMOVE`) / 5.5 의도적으로 바꾼 진단 / 5.6 의도적으로 구현하지 않은 동작. 각 항목은 양쪽 `file:line`을 달고 있고 소스에도 `DELIBERATE …` 주석으로 표시돼 있다(`grep -rn DELIBERATE crates/`). 같은 문서의 §1~§3은 이번 작업 종료 시점으로 재작성됐다(이전 판의 `ARRAYMOVE`/`ARRAYMSORT` 서술은 틀렸었다). README에 `## Emuera parity` 절로 링크.

`SORTCHARA`는 5.3에 **없다**: Emuera 비교자는 양방향 모두 정렬 전 인덱스로 tie를 깬다(`CharacterData.cs:683-696`, 내림차순에서도 tiebreak를 부정하지 않음). 즉 이미 완전히 규정된 동작이고 erars가 그대로 재현한다(`executor.rs:1008-1032`).

### 픽스처

`tests/run_tests/` 총 **99쌍**(`.erb`/`.out`), 이번 작업에서 21쌍 추가. 신설 디렉터리 `control_flow/`(5), `savevar/`(4), `spchara/`(1, 자체 `emuera.config`), `basic/`에 9쌍, `instruction/`에 2쌍.

규칙: **`.out` 기대값은 첫 실행 전에 C# 소스에서 손으로 유도한다** — erars 출력을 캡처해 `.out`으로 만들면 안 된다. 하네스 주의점: `tests/run_tests.rs`는 단일 `run_test`라 **첫 실패에서 전체가 중단**되므로 green 이전의 `[o]` 개수는 부분 집계다. `.out` 누락은 `unwrap()` 패닉. 비교는 `k9::assert_equal!(ret, expected_ret)`로 **왼쪽이 실제, 오른쪽이 `.out`**. VM 에러는 픽스처 전체를 중단시키므로 `.erb` 하나에 에러 케이스는 하나만. **파스 에러는 픽스처로 못 만든다**(하네스가 파스를 `unwrap`) — `/tmp` 게임 + `erars-stdio`로 확인해야 한다.

난수 픽스처는 시드가 아니라 **구성으로** 결정적으로 만든다(VM RNG는 `ChaCha20Rng::from_entropy()`, `crates/erars-vm/src/variable.rs:77`): 1항목 블록으로 `RAND(1)==0`을 고정하거나, 동일한 항목 여러 개로 텍스트를 고정한 채 범위만 단언한다.

### 검증 (작업 종료 시점)

- `cargo build --all` → 에러 0. `cargo build --all --all-targets` → 에러 0.
- `cargo test --all` → **30 스위트 / 264 passed / 0 failed**.
- `cargo test --test run_tests` → **99 `[o]` / 0 `[x]`**.
- 코퍼스 회귀: `cargo build --release -p erars-stdio && ./target/release/erars-stdio --save --quite ../eraTHYMKR </dev/null` → exit 0, **진단 0줄**, pristine `HEAD` 대비 출력이 **단계 타이밍만 다름**. 비교용 베이스라인은 `git worktree add /tmp/erars_base a8190fd`로 만들었고 작업 후 제거함(그 워크트리는 상대 경로를 자기 cwd 기준으로 풀므로 게임 경로를 **절대 경로로** 넘겨야 한다).
- 함수 **16,859** / 인터너 **334,738** — §6b와 동일(불변). 측정: `cargo run --release -p erars-loader --features multithread --example phases -- ../eraTHYMKR`. `phases`는 `rayon`을 직접 쓰므로 `multithread` 없이는 빌드되지 않는다 → `Cargo.toml`에 `[[example]] required-features` 선언을 추가했다(워크스페이스 전체 빌드는 프런트엔드 크레이트에서 피처가 unify돼 그냥 된다).
- `game.era` **바이트 크기는 회귀 신호로 쓸 수 없다** — 같은 바이너리로도 실행마다 다르다(50,293,391 / 50,292,261 / 50,281,464). §5b·§5c의 rayon 등록 순서 비결정성과 동일 원인. 개수와 진단만 비교할 것.
- 런타임 스모크(`/tmp`에 남겨둠, 재부팅까지): `g6_smoke`(PRINTDATA 인덱스 기록·DATALIST 개행·STRDATA 3케이스·음수 ARRAYSHIFT·OUTPUTLOG UTF-16LE+BOM+CRLF·FIND_CHARADATA·HTML_PRINT), `g7_delim`(미아 구분자·중첩 거부 20종 전부 원문 일치), `g8_title`(기본 타이틀 화면, 버전 표기 1234→1.234 / 1230→1.23 / 1200→1.20 / 1000→1.00 / 0→생략 / 12345→12.345, `[0]`·`[1]`·무효값·`@TITLE_LOADGAME` 분기).
- 실게임 `SORTCHARA`: `./target/release/erars-stdio ../eraTHYMKR --quite --use-input /tmp/sortdrive2.ron`으로 `@CHARA_SORT`(메인 메뉴 `[666]`)를 구동. 오름/내림 + `ABL`/`EXP` 키를 실행했고 **명부 순서가 실제로 뒤집혔다**([레이무,대요정] → 내림 [대요정,레이무] → 오름 [레이무,대요정]). 미실행: `MARK`/`JUEL`/`CFLAG`/`BASE` 키(같은 경로, 키 `VariableRef`만 다름), `CHARANUM<=1`, 문자열 키(CSTR) 분기.

### Emuera 소스 탐색 메모

재다운로드: `curl -sL https://codeload.github.com/wozpren/WebEmuera/zip/refs/heads/master -o /tmp/we.zip && cd /tmp && mkdir -p webemuera && unzip -q -o we.zip -d webemuera`.
- `GameProc/Function/FunctionIdentifier.cs`의 `addFunction(FunctionCode.X, argb[FunctionArgType.Y], flags)`가 인수 형태를 준다. `:428-440`은 `Creator.cs`의 모든 메서드를 문장 딕셔너리에 접어 넣으므로 **메서드 전용 명령에 라인헤드 `normal_method!` arm이 있는 게 맞다.** `:437-460`은 `funcMatch`(opener→closer 이름), `:465-468`은 `funcParent`(closer→opener 코드).
- 형태별 빌더는 `GameProc/Function/ArgumentBuilder.cs`, 본체는 `GameProc/Process.ScriptProc.cs` 또는 `GameProc/Function/Instraction.Child.cs`, 식 메서드는 `GameData/Function/Creator.Method.cs`, 에러 문자열은 `_Library/EvilMask/Lang.cs`.
- `GameView/HtmlManager.cs`, `Forms/MainWindow.cs`, `Sub/LexicalAnalyzer.cs`, `Sub/EraStreamReader.cs`는 `GameData/` **아래가 아니다**.
- 로케일 대조용 Wine 참조: `/tmp/wine_locale.c`.
- **스카우트 주장은 항상 재검증할 것.** 이번에 인계 문서 자신의 "`PRINTDATA <n>`은 n번 항목을 선택한다"는 주장이 틀렸고 이미 픽스처에 박혀 있었다.

### 규약 (다음 세션도 유지)

- `BuiltinCommand`/`BuiltinMethod` 기존 discriminant는 **절대 재배치하지 않는다**(바이트코드 호환). 새 variant는 꼬리에 추가. `InstructionCode`는 재배치 가능.
- 구현했다고 보고하는 것에 스텁·no-op·`todo!()`·가짜 상수를 두지 않는다. 정말 불가능한 동작은 Emuera 소스 줄과 이유를 적은 `DELIBERATE …` 주석 블록을 달고 §5 목록에 올린다.
- VM/내부 에러 메시지는 한국어, 게임에 보이는 텍스트는 Emuera 원문 일본어.
- 클레임마다 erars와 Emuera C# **양쪽** `file:line`을 단다.
