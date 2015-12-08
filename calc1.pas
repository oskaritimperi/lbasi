program calc1;

{$H+}

uses
    typinfo, sysutils, character;

type
    TokenType = (TT_Integer, TT_Plus, TT_Minus, TT_Eof);

    Token = class
        TokenType: TokenType;
        constructor Create(Type_: TokenType);
        function ToStr: String; virtual;
    end;

    TokenInteger = class(Token)
        Val: Integer;
        constructor Create(Value_: Integer);
        function ToStr: String; override;
    end;

    TokenPlus = class(Token)
        constructor Create;
    end;

    TokenMinus = class(Token)
        constructor Create;
    end;

    TokenEof = class(Token)
        constructor Create;
    end;

    Interpreter = class
        Text: AnsiString;
        CurPos: Integer;
        Current: Token;

        constructor Create(Text_: AnsiString);

        procedure Error;
        procedure SkipWhitespace;
        function AtEnd: Boolean;
        function CurChar: Char;
        function GetInteger: Integer;
        function GetNextToken: Token;
        procedure Eat(T: TokenType);
        function Expr: Integer;
    end;

constructor Token.Create(Type_: TokenType);
begin
    inherited Create;
    TokenType := Type_;
end;

function Token.ToStr: String;
begin
    Result := GetEnumName(TypeInfo(TokenType), Ord(Self.TokenType));
end;

constructor TokenInteger.Create(Value_: Integer);
begin
    inherited Create(TT_Integer);
    Val := Value_;
end;

function TokenInteger.ToStr: String;
begin
    Result := inherited;
    Result := Result + '(' + IntToStr(Val) + ')';
end;

constructor TokenPlus.Create;
begin
    inherited Create(TT_Plus);
end;

constructor TokenMinus.Create;
begin
    inherited Create(TT_Minus);
end;


constructor TokenEof.Create;
begin
    inherited Create(TT_Eof);
end;

constructor Interpreter.Create(Text_: String);
begin
    inherited Create;
    Text := Text_;
    CurPos := 1;
    Current := Default(Token);
end;

procedure Interpreter.Error;
begin
    Raise Exception.Create('error!');
end;

procedure Interpreter.SkipWhitespace;
begin
    while (not AtEnd) and IsWhiteSpace(CurChar) do
        Inc(CurPos);
end;

function Interpreter.AtEnd: Boolean;
begin
    Result := CurPos > Length(Text);
end;

function Interpreter.CurChar: Char;
begin
    Result := Text[CurPos];
end;

function Interpreter.GetInteger: Integer;
var
    Start: Integer;
begin
    Start := CurPos;

    while (not AtEnd) and IsDigit(CurChar) do
        Inc(CurPos);

    Result := StrToInt(Copy(Text, Start, CurPos - Start));
end;

function Interpreter.GetNextToken: Token;
begin
    SkipWhitespace;

    if AtEnd then
    begin
        Result := TokenEof.Create;
        Exit;
    end;

    if IsDigit(CurChar) then
    begin
        Result := TokenInteger.Create(GetInteger);
        Exit;
    end
    else if CurChar = '+' then
    begin
        Result := TokenPlus.Create;
        Inc(CurPos);
        Exit;
    end
    else if CurChar = '-' then
    begin
        Result := TokenMinus.Create;
        Inc(CurPos);
        Exit;
    end;

    Error;
end;

procedure Interpreter.Eat(T: TokenType);
begin
    if Current.TokenType = T then
    begin
        Current := GetNextToken;
    end
    else
        Error;
end;

function Interpreter.Expr: Integer;
var
    Left, Op, Right: Token;
begin
    Current := GetNextToken;

    Left := Current;
    Eat(TT_Integer);

    Op := Current;

    if Op.TokenType = TT_Plus then
        Eat(TT_Plus)
    else
        Eat(TT_Minus);

    Right := Current;
    Eat(TT_Integer);

    if Op.TokenType = TT_Plus then
        Result := TokenInteger(Left).Val + TokenInteger(Right).Val
    else
        Result := TokenInteger(Left).Val - TokenInteger(Right).Val;
end;

var
    Interp: Interpreter;
    Line: String;
    I: Integer;

procedure InterpString(S: String);
begin
    Interp := Interpreter.Create(S);
    WriteLn(Interp.Expr);
    FreeAndNil(Interp);
end;

procedure InputLoop;
begin
    while True do
    begin
        Write('calc> ');
        if Eof(Input) then break;
        ReadLn(Line);
        if Length(Line) = 0 then continue;
        InterpString(Line);
    end;
end;

begin
    if ParamCount > 0 then
    begin
        Line := '';
        for I := 1 to ParamCount do
        begin
            Line := Line + ParamStr(I);
        end;
        InterpString(Line);
    end
    else
        InputLoop;
end.
