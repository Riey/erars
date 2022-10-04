# GET /

```

Ret {
    current_req: Option\<InputRequest>,
    bg_color: Color,
    hl_color: Color,
    lines: [{parts: [ConsoleLinePart]}]
}

InputRequest {
    Anykey,
    EnterKey,
    Int,
    Str,
}

ConsoleLinePart {
    Text: (String, TextStyle),
    Line: (String, TextStyle),
    Button: ([(String, TextStyle)], {Int or String}),
}

TextStyle {
    color: Color,
    font_family: String,
    font_style: FontStyle,
}

FontStyle {
    NORMAL = 0x0;
    BOLD = 0x1;
    ITALIC = 0x2;
    STRIKELINE = 0x4;
    UNDERLINE = 0x8;
}

Color = [u8; 3]

```

## Query

?from=`line_no` 를 붙이면 해당 라인 번호부터 시작하는 결과를 전송함 범위를 넘으면 빈 배열 반환.

# POST /input

유저입력을 body에 담아서 전송

InputRequest가 Anykey, EnterKey면 형식은 상관없음.

Int일시엔 꼭 숫자 문자열로 전송 Str이면 그냥 그대로 전송

# GET /listen

웹소켓 연결 가능

연결시 UI가 업데이트 될때마다 아무 메세지를 전송함
