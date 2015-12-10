program calc1;

{$H+}

uses
    typinfo, sysutils, character;

type
    TokenType = (TT_Integer, TT_Plus, TT_Minus, TT_Asterisk, TT_Slash, TT_Eof);

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

    TLexer = class
        Text: AnsiString;
        CurPos: Integer;

        constructor Create(Text_: AnsiString);

        procedure Error;
        procedure SkipWhitespace;
        function AtEnd: Boolean;
        function CurChar: Char;
        function GetInteger: Integer;
        function GetNextToken: Token;
    end;

    TInterpreter = class
        Lexer: TLexer;
        CurrentToken: Token;

        constructor Create(Lexer_: TLexer);

        procedure Error;
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

constructor TLexer.Create(Text_: String);
begin
    inherited Create;
    Text := Text_;
    CurPos := 1;
end;

procedure TLexer.Error;
begin
    Raise Exception.Create('invalid input');
end;

procedure TLexer.SkipWhitespace;
begin
    while (not AtEnd) and IsWhiteSpace(CurChar) do
        Inc(CurPos);
end;

function TLexer.AtEnd: Boolean;
begin
    Result := CurPos > Length(Text);
end;

function TLexer.CurChar: Char;
begin
    Result := Text[CurPos];
end;

function TLexer.GetInteger: Integer;
var
    Start: Integer;
begin
    Start := CurPos;

    while (not AtEnd) and IsDigit(CurChar) do
        Inc(CurPos);

    Result := StrToInt(Copy(Text, Start, CurPos - Start));
end;

function TLexer.GetNextToken: Token;
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
    end
    else if CurChar = '+' then
    begin
        Result := TokenPlus.Create;
        Inc(CurPos);
    end
    else if CurChar = '-' then
    begin
        Result := TokenMinus.Create;
        Inc(CurPos);
    end
    else if CurChar = '*' then
    begin
        Result := Token.Create(TT_Asterisk);
        Inc(CurPos);
    end
    else if CurChar = '/' then
    begin
        Result := Token.Create(TT_Slash);
        Inc(CurPos);
    end
    else
        Error;
end;

constructor TInterpreter.Create(Lexer_: TLexer);
begin
    Lexer := Lexer_;
    CurrentToken := Lexer.GetNextToken;
end;

procedure TInterpreter.Error;
begin
    Raise Exception.Create('syntax error');
end;

procedure TInterpreter.Eat(T: TokenType);
begin
    if CurrentToken.TokenType = T then
    begin
        CurrentToken := Lexer.GetNextToken;
    end
    else
        Error;
end;

function TInterpreter.Expr: Integer;
var
    Left, Op, Right: Token;
begin
    CurrentToken := Lexer.GetNextToken;

    Left := CurrentToken;
    Eat(TT_Integer);

    Op := CurrentToken;

    while (Op.TokenType = TT_Plus) or (Op.TokenType = TT_Minus) do
    begin
        Eat(Op.TokenType);

        Right := CurrentToken;
        Eat(TT_Integer);

        if Op.TokenType = TT_Plus then
            Result := TokenInteger(Left).Val + TokenInteger(Right).Val
        else
            Result := TokenInteger(Left).Val - TokenInteger(Right).Val;

        TokenInteger(Left).Val := Result;

        Op := CurrentToken;

        if Op.TokenType = TT_Eof then
            Exit;
    end;

    if Op.TokenType = TT_Asterisk then
        Eat(TT_Asterisk)
    else
        Eat(TT_Slash);

    Right := CurrentToken;
    Eat(TT_Integer);

    if Op.TokenType = TT_Plus then
        Result := TokenInteger(Left).Val + TokenInteger(Right).Val
    else if Op.TokenType = TT_Minus then
        Result := TokenInteger(Left).Val - TokenInteger(Right).Val
    else if Op.TokenType = TT_Asterisk then
        Result := TokenInteger(Left).Val * TokenInteger(Right).Val
    else
        Result := Round(TokenInteger(Left).Val / TokenInteger(Right).Val);
end;

var
    Lexer: TLexer;
    Interp: TInterpreter;
    Line: String;
    I: Integer;

procedure InterpString(S: String);
begin
    Lexer := TLexer.Create(S);
    Interp := TInterpreter.Create(Lexer);
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
