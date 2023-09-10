import py_interpreter.monkey_token as t
from py_interpreter.monkey_lexer import Lexer


def test_next_token():
    source = r"""let five = 5;
let ten = 10;

let add = fn (x, y) {
    x + y;
};

let result = add (five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
macro(x, y) { x + y; };
"""

    tests = [
        (t.LET, "let"),
        (t.IDENT, "five"),
        (t.ASSIGN, "="),
        (t.INT, "5"),
        (t.SEMICOLON, ";"),
        (t.LET, "let"),
        (t.IDENT, "ten"),
        (t.ASSIGN, "="),
        (t.INT, "10"),
        (t.SEMICOLON, ";"),
        (t.LET, "let"),
        (t.IDENT, "add"),
        (t.ASSIGN, "="),
        (t.FUNCTION, "fn"),
        (t.LPAREN, "("),
        (t.IDENT, "x"),
        (t.COMMA, ","),
        (t.IDENT, "y"),
        (t.RPAREN, ")"),
        (t.LBRACE, "{"),
        (t.IDENT, "x"),
        (t.PLUS, "+"),
        (t.IDENT, "y"),
        (t.SEMICOLON, ";"),
        (t.RBRACE, "}"),
        (t.SEMICOLON, ";"),
        (t.LET, "let"),
        (t.IDENT, "result"),
        (t.ASSIGN, "="),
        (t.IDENT, "add"),
        (t.LPAREN, "("),
        (t.IDENT, "five"),
        (t.COMMA, ","),
        (t.IDENT, "ten"),
        (t.RPAREN, ")"),
        (t.SEMICOLON, ";"),
        (t.BANG, "!"),
        (t.MINUS, "-"),
        (t.SLASH, "/"),
        (t.ASTERISK, "*"),
        (t.INT, "5"),
        (t.SEMICOLON, ";"),
        (t.INT, "5"),
        (t.LT, "<"),
        (t.INT, "10"),
        (t.GT, ">"),
        (t.INT, "5"),
        (t.SEMICOLON, ";"),
        (t.IF, "if"),
        (t.LPAREN, "("),
        (t.INT, "5"),
        (t.LT, "<"),
        (t.INT, "10"),
        (t.RPAREN, ")"),
        (t.LBRACE, "{"),
        (t.RETURN, "return"),
        (t.TRUE, "true"),
        (t.SEMICOLON, ";"),
        (t.RBRACE, "}"),
        (t.ELSE, "else"),
        (t.LBRACE, "{"),
        (t.RETURN, "return"),
        (t.FALSE, "false"),
        (t.SEMICOLON, ";"),
        (t.RBRACE, "}"),
        (t.INT, "10"),
        (t.EQUALS, "=="),
        (t.INT, "10"),
        (t.SEMICOLON, ";"),
        (t.INT, "10"),
        (t.NOT_EQUALS, "!="),
        (t.INT, "9"),
        (t.SEMICOLON, ";"),
        (t.STRING, "foobar"),
        (t.STRING, "foo bar"),
        (t.LBRACKET, "["),
        (t.INT, "1"),
        (t.COMMA, ","),
        (t.INT, "2"),
        (t.RBRACKET, "]"),
        (t.SEMICOLON, ";"),
        (t.LBRACE, "{"),
        (t.STRING, "foo"),
        (t.COLON, ":"),
        (t.STRING, "bar"),
        (t.RBRACE, "}"),
        (t.MACRO, "macro"),
        (t.LPAREN, "("),
        (t.IDENT, "x"),
        (t.COMMA, ","),
        (t.IDENT, "y"),
        (t.RPAREN, ")"),
        (t.LBRACE, "{"),
        (t.IDENT, "x"),
        (t.PLUS, "+"),
        (t.IDENT, "y"),
        (t.SEMICOLON, ";"),
        (t.RBRACE, "}"),
        (t.SEMICOLON, ";"),
        (t.EOF, ""),
    ]

    lexer = Lexer(source)

    for expected_kind, expected_literal in tests:
        token = lexer.next_token()

        assert token.kind == expected_kind
        assert token.literal == expected_literal
