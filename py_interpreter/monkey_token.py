from dataclasses import dataclass

TokenType = str


@dataclass
class Token:
    kind: TokenType = ""
    literal: str = ""


ILLEGAL = "ILLEGAL"
EOF = "EOF"

# Identifiers + literals
IDENT = "IDENT"
INT = "INT"
STRING = "STRING"

# Operators
ASSIGN = "="
PLUS = "+"
MINUS = "-"
BANG = "!"
ASTERISK = "*"
SLASH = "/"
LT = "<"
GT = ">"
EQUALS = "=="
NOT_EQUALS = "!="

# Delimiters
COMMA = ","
SEMICOLON = ";"
COLON = ":"

LPAREN = "("
RPAREN = ")"
LBRACE = "{"
RBRACE = "}"
LBRACKET = "["
RBRACKET = "]"

MACRO = "MACRO"

# Keywords
FUNCTION = "FUNCTION"
LET = "LET"
TRUE = "TRUE"
FALSE = "FALSE"
IF = "IF"
ELSE = "ELSE"
RETURN = "RETURN"

KEYWORDS = {
    "fn": FUNCTION,
    "let": LET,
    "true": TRUE,
    "false": FALSE,
    "if": IF,
    "else": ELSE,
    "return": RETURN,
    "macro": MACRO,
}


def lookup_ident(ident: str) -> TokenType:
    return KEYWORDS.get(ident, IDENT)
