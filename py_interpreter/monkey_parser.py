from dataclasses import dataclass
from enum import IntEnum, auto
from typing import Callable

import py_interpreter.monkey_token as t
from py_interpreter.monkey_ast import (
    ArrayLiteral,
    BlockStatement,
    BooleanExpression,
    CallExpression,
    Expression,
    ExpressionStatement,
    FunctionLiteral,
    HashLiteral,
    Identifier,
    IfExpression,
    IndexExpression,
    InfixExpression,
    IntegerLiteral,
    LetStatement,
    MacroLiteral,
    PrefixExpression,
    Program,
    ReturnStatement,
    Statement,
    StringLiteral,
)
from py_interpreter.monkey_lexer import Lexer
from py_interpreter.monkey_token import Token, TokenType

PrefixParseFunction = Callable[[], Expression | None]
InfixParseFunction = Callable[[Expression | None], Expression | None]


class OpPrecedence(IntEnum):
    LOWEST = auto()
    EQUALS = auto()
    LESSGREATER = auto()
    SUM = auto()
    PRODUCT = auto()
    PREFIX = auto()
    CALL = auto()
    INDEX = auto()


PRECEDENCES = {
    t.EQUALS: OpPrecedence.EQUALS,
    t.NOT_EQUALS: OpPrecedence.EQUALS,
    t.LT: OpPrecedence.LESSGREATER,
    t.GT: OpPrecedence.LESSGREATER,
    t.PLUS: OpPrecedence.SUM,
    t.MINUS: OpPrecedence.SUM,
    t.SLASH: OpPrecedence.PRODUCT,
    t.ASTERISK: OpPrecedence.PRODUCT,
    t.LPAREN: OpPrecedence.CALL,
    t.LBRACKET: OpPrecedence.INDEX,
}


@dataclass
class Parser:
    lexer: Lexer
    current_token: Token
    peek_token: Token
    errors: list[str]
    prefix_parse_functions: dict[TokenType, PrefixParseFunction]
    infix_parse_functions: dict[TokenType, InfixParseFunction]

    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.current_token = Token("", "")
        self.peek_token = Token("", "")
        self.errors = []

        self.prefix_parse_functions = {}
        self.register_prefix(t.IDENT, self.parse_identifier)
        self.register_prefix(t.INT, self.parse_integer_literal)
        self.register_prefix(t.BANG, self.parse_prefix_expression)
        self.register_prefix(t.MINUS, self.parse_prefix_expression)
        self.register_prefix(t.TRUE, self.parse_boolean)
        self.register_prefix(t.FALSE, self.parse_boolean)
        self.register_prefix(t.LPAREN, self.parse_grouped_expression)
        self.register_prefix(t.RPAREN, self.parse_grouped_expression)
        self.register_prefix(t.IF, self.parse_if_expression)
        self.register_prefix(t.FUNCTION, self.parse_function_literal)
        self.register_prefix(t.STRING, self.parse_string_literal)
        self.register_prefix(t.LBRACKET, self.parse_array_literal)
        self.register_prefix(t.LBRACE, self.parse_hash_literal)
        self.register_prefix(t.MACRO, self.parse_macro_literal)

        self.infix_parse_functions = {}
        self.register_infix(t.PLUS, self.parse_infix_expression)
        self.register_infix(t.MINUS, self.parse_infix_expression)
        self.register_infix(t.SLASH, self.parse_infix_expression)
        self.register_infix(t.ASTERISK, self.parse_infix_expression)
        self.register_infix(t.EQUALS, self.parse_infix_expression)
        self.register_infix(t.NOT_EQUALS, self.parse_infix_expression)
        self.register_infix(t.LT, self.parse_infix_expression)
        self.register_infix(t.GT, self.parse_infix_expression)
        self.register_infix(t.LPAREN, self.parse_call_expression)
        self.register_infix(t.LBRACKET, self.parse_index_expression)

        self.next_token()
        self.next_token()

    def register_prefix(self, kind: TokenType, function: PrefixParseFunction):
        self.prefix_parse_functions[kind] = function

    def register_infix(self, kind: TokenType, function: InfixParseFunction):
        self.infix_parse_functions[kind] = function

    def next_token(self):
        self.current_token = self.peek_token
        self.peek_token = self.lexer.next_token()

    def parse_statement(self) -> Statement | None:
        match self.current_token.kind:
            case t.LET:
                return self.parse_let_statement()
            case t.RETURN:
                return self.parse_return_statement()
            case _:
                return self.parse_expression_statement()

    def parse_let_statement(self) -> LetStatement | None:
        token = self.current_token

        if not self.expect_peek(t.IDENT):
            return None

        name = Identifier(token=self.current_token, value=self.current_token.literal)

        if not self.expect_peek(t.ASSIGN):
            return None

        self.next_token()

        value = self.parse_expression(OpPrecedence.LOWEST)

        if self.peek_token_is(t.SEMICOLON):
            self.next_token()

        return LetStatement(token=token, name=name, value=value)

    def parse_return_statement(self) -> ReturnStatement | None:
        token = self.current_token

        self.next_token()

        return_value = self.parse_expression(OpPrecedence.LOWEST)

        if self.peek_token_is(t.SEMICOLON):
            self.next_token()

        return ReturnStatement(token=token, return_value=return_value)

    def parse_expression_statement(self) -> ExpressionStatement:
        statement = ExpressionStatement(token=self.current_token, expression=self.parse_expression(OpPrecedence.LOWEST))

        if self.peek_token_is(t.SEMICOLON):
            self.next_token()

        return statement

    def parse_expression_list(self, end: TokenType) -> list[Expression | None] | None:
        list_: list[Expression | None] = []

        if self.peek_token_is(end):
            self.next_token()
            return list_

        self.next_token()
        list_.append(self.parse_expression(OpPrecedence.LOWEST))

        while self.peek_token_is(t.COMMA):
            self.next_token()
            self.next_token()
            list_.append(self.parse_expression(OpPrecedence.LOWEST))

        if not self.expect_peek(end):
            return None

        return list_

    def parse_expression(self, precedence: int) -> Expression | None:
        prefix_parse_function = self.prefix_parse_functions.get(self.current_token.kind, None)

        if prefix_parse_function is None:
            self.no_prefix_parse_function_error(self.current_token.kind)
            return None

        left_expression = prefix_parse_function()

        while not self.peek_token_is(t.SEMICOLON) and precedence < self.peek_precedence():
            infix_parse_function = self.infix_parse_functions.get(self.peek_token.kind, None)

            if infix_parse_function is None:
                return left_expression

            self.next_token()

            left_expression = infix_parse_function(left_expression)

        return left_expression

    def parse_identifier(self) -> Expression:
        return Identifier(token=self.current_token, value=self.current_token.literal)

    def parse_boolean(self) -> Expression | None:
        return BooleanExpression(token=self.current_token, value=self.current_token_is(t.TRUE))

    def parse_string_literal(self) -> Expression | None:
        return StringLiteral(token=self.current_token, value=self.current_token.literal)

    def parse_array_literal(self) -> Expression | None:
        return ArrayLiteral(token=self.current_token, elements=self.parse_expression_list(t.RBRACKET))

    def parse_hash_literal(self) -> Expression | None:
        token = self.current_token

        pairs = {}

        while not self.peek_token_is(t.RBRACE):
            self.next_token()
            key = self.parse_expression(OpPrecedence.LOWEST)

            if not self.expect_peek(t.COLON):
                return None

            self.next_token()

            value = self.parse_expression(OpPrecedence.LOWEST)

            pairs[key] = value

            if not self.peek_token_is(t.RBRACE) and not self.expect_peek(t.COMMA):
                return None

        if not self.expect_peek(t.RBRACE):
            return None

        return HashLiteral(token=token, pairs=pairs)

    def parse_integer_literal(self) -> Expression | None:
        token = self.current_token

        try:
            value = int(token.literal)
        except Exception:
            self.errors.append(f"could not parse {token.literal} as integer")

            return None

        return IntegerLiteral(token=token, value=value)

    def parse_prefix_expression(self) -> Expression:
        token = self.current_token
        operator = self.current_token.literal

        self.next_token()

        expression = self.parse_expression(OpPrecedence.PREFIX)

        return PrefixExpression(token=token, operator=operator, right=expression)

    def parse_infix_expression(self, left: Expression | None) -> Expression | None:
        token = self.current_token
        operator = self.current_token.literal
        left = left

        precedence = self.current_precedence()

        self.next_token()

        right = self.parse_expression(precedence)

        return InfixExpression(token=token, operator=operator, left=left, right=right)

    def parse_grouped_expression(self) -> Expression | None:
        self.next_token()

        expression = self.parse_expression(OpPrecedence.LOWEST)

        if not self.expect_peek(t.RPAREN):
            return None

        return expression

    def parse_block_statement(self) -> BlockStatement:
        token = self.current_token

        statements = []

        self.next_token()

        while not self.current_token_is(t.RBRACE) and not self.current_token_is(t.EOF):
            statement = self.parse_statement()
            if statement:
                statements.append(statement)

            self.next_token()

        return BlockStatement(token=token, statements=statements)

    def parse_if_expression(self) -> Expression | None:
        token = self.current_token

        if not self.expect_peek(t.LPAREN):
            return None

        self.next_token()

        condition = self.parse_expression(OpPrecedence.LOWEST)

        if not self.expect_peek(t.RPAREN):
            return None

        if not self.expect_peek(t.LBRACE):
            return None

        consequence = self.parse_block_statement()

        alternative = None
        if self.peek_token_is(t.ELSE):
            self.next_token()

            if not self.expect_peek(t.LBRACE):
                return None

            alternative = self.parse_block_statement()

        return IfExpression(token=token, condition=condition, consequence=consequence, alternative=alternative)

    def parse_function_parameters(self) -> list[Identifier] | None:
        identifiers: list[Identifier] = []

        if self.peek_token_is(t.RPAREN):
            self.next_token()
            return identifiers

        self.next_token()

        identifier = Identifier(token=self.current_token, value=self.current_token.literal)

        identifiers.append(identifier)

        while self.peek_token_is(t.COMMA):
            self.next_token()
            self.next_token()

            identifier = Identifier(token=self.current_token, value=self.current_token.literal)
            identifiers.append(identifier)

        if not self.expect_peek(t.RPAREN):
            return None

        return identifiers

    def parse_function_literal(self) -> Expression | None:
        token = self.current_token

        if not self.expect_peek(t.LPAREN):
            return None

        parameters = self.parse_function_parameters()

        if not self.expect_peek(t.LBRACE):
            return None

        body = self.parse_block_statement()

        return FunctionLiteral(token=token, parameters=parameters, body=body)

    def parse_macro_literal(self) -> Expression | None:
        token = self.current_token

        if not self.expect_peek(t.LPAREN):
            return None

        parameters = self.parse_function_parameters()

        if not self.expect_peek(t.LBRACE):
            return None

        body = self.parse_block_statement()

        return MacroLiteral(token=token, parameters=parameters, body=body)

    def parse_call_expression(self, function: Expression | None) -> Expression | None:
        token = self.current_token

        arguments = self.parse_expression_list(t.RPAREN)

        return CallExpression(token=token, function=function, arguments=arguments)

    def parse_index_expression(self, left: Expression) -> Expression | None:
        token = self.current_token

        self.next_token()
        index = self.parse_expression(OpPrecedence.LOWEST)

        if not self.expect_peek(t.RBRACKET):
            return None

        return IndexExpression(token=token, left=left, index=index)

    def current_token_is(self, kind: TokenType) -> bool:
        return self.current_token.kind == kind

    def peek_token_is(self, kind: TokenType) -> bool:
        return self.peek_token.kind == kind

    def expect_peek(self, kind: TokenType) -> bool:
        if self.peek_token_is(kind):
            self.next_token()
            return True
        else:
            self.peek_error(kind)
            return False

    def peek_error(self, kind: TokenType):
        message = f"expected next token to be '{kind}', got '{self.peek_token.kind}' instead."

        self.errors.append(message)

    def peek_precedence(self) -> int:
        return PRECEDENCES.get(self.peek_token.kind, OpPrecedence.LOWEST)

    def current_precedence(self) -> int:
        return PRECEDENCES.get(self.current_token.kind, OpPrecedence.LOWEST)

    def no_prefix_parse_function_error(self, kind: TokenType):
        self.errors.append(f"no prefix parse function for '{kind}' found")

    def parse_program(self) -> Program | None:
        program = Program([])

        while not self.current_token_is(t.EOF):
            if statement := self.parse_statement():
                program.statements.append(statement)

            self.next_token()

        return program
