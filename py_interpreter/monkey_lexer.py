from dataclasses import dataclass

import py_interpreter.monkey_token as t
from py_interpreter.monkey_token import Token


def _is_letter(char: str) -> bool:
    return (str.isalpha(char) and str.isascii(char)) or char == "_"


@dataclass
class Lexer:
    input_: str
    position: int = 0
    read_position: int = 0
    char: str = ""

    def __init__(self, input_: str):
        self.input_ = input_
        self.read_char()

    def read_char(self):
        if self.read_position >= len(self.input_):
            self.char = ""
        else:
            self.char = self.input_[self.read_position]

        self.position = self.read_position
        self.read_position += 1

    def read_identifier(self) -> str:
        position = self.position

        while _is_letter(self.char):
            self.read_char()

        return self.input_[position : self.position]  # noqa: E203

    def read_number(self) -> str:
        position = self.position

        while str.isdigit(self.char):
            self.read_char()

        return self.input_[position : self.position]  # noqa: E203

    def skip_whitespace(self):
        while str.isspace(self.char):
            self.read_char()

    def peek_char(self) -> str:
        if self.read_position >= len(self.input_):
            return ""
        else:
            return self.input_[self.read_position]

    def read_string(self) -> str:
        position = self.position + 1

        self.read_char()
        while self.char != '"' and self.char != 0:
            self.read_char()

        return self.input_[position : self.position]

    def next_token(self) -> Token:
        self.skip_whitespace()

        match self.char:
            case "=":
                if self.peek_char() == "=":
                    token = Token(t.EQUALS, "==")
                    self.read_char()
                else:
                    token = Token(t.ASSIGN, self.char)
            case "!":
                if self.peek_char() == "=":
                    token = Token(t.NOT_EQUALS, "!=")
                    self.read_char()
                else:
                    token = Token(t.BANG, self.char)
            case "-":
                token = Token(t.MINUS, self.char)
            case "*":
                token = Token(t.ASTERISK, self.char)
            case "/":
                token = Token(t.SLASH, self.char)
            case "<":
                token = Token(t.LT, self.char)
            case ">":
                token = Token(t.GT, self.char)
            case ";":
                token = Token(t.SEMICOLON, self.char)
            case ",":
                token = Token(t.COMMA, self.char)
            case "(":
                token = Token(t.LPAREN, self.char)
            case ")":
                token = Token(t.RPAREN, self.char)
            case "+":
                token = Token(t.PLUS, self.char)
            case "{":
                token = Token(t.LBRACE, self.char)
            case "}":
                token = Token(t.RBRACE, self.char)
            case "":
                token = Token(t.EOF, "")
            case '"':
                token = Token(t.STRING, self.read_string())
            case "[":
                token = Token(t.LBRACKET, self.char)
            case "]":
                token = Token(t.RBRACKET, self.char)
            case ":":
                token = Token(t.COLON, self.char)
            case _:
                if _is_letter(self.char):
                    identifier = self.read_identifier()
                    token = Token(t.lookup_ident(identifier), identifier)

                    # Return earlier to avoid read_char()
                    return token

                elif str.isdigit(self.char):
                    token = Token(t.INT, self.read_number())

                    # Return earlier to avoid read_char()
                    return token

                else:
                    token = Token(t.ILLEGAL, self.char)

        self.read_char()

        return token
