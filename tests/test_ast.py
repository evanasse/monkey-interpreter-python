import py_interpreter.monkey_token as t
from py_interpreter.monkey_ast import (
    ArrayLiteral,
    BlockStatement,
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
    Node,
    PrefixExpression,
    Program,
    ReturnStatement,
    modify,
)
from py_interpreter.monkey_token import Token


def test_string():
    program = Program(
        statements=[
            LetStatement(
                token=Token(kind=t.LET, literal="let"),
                name=Identifier(token=Token(kind=t.IDENT, literal="myVar"), value="myVar"),
                value=Identifier(token=Token(kind=t.IDENT, literal="anotherVar"), value="anotherVar"),
            ),
        ]
    )

    assert str(program) == "let myVar = anotherVar;"


def test_modify():
    def one() -> Expression:
        return IntegerLiteral(value=1)

    def two() -> Expression:
        return IntegerLiteral(value=2)

    def turn_one_into_two(node: Node) -> Node:
        if not isinstance(node, IntegerLiteral):
            return node

        if node.value != 1:
            return node

        return IntegerLiteral(value=2)

    tests = [
        {"source": one(), "expected": two()},
        {
            "source": Program(statements=[ExpressionStatement(expression=one())]),
            "expected": Program(statements=[ExpressionStatement(expression=two())]),
        },
        {
            "source": InfixExpression(left=one(), operator="+", right=two()),
            "expected": InfixExpression(left=two(), operator="+", right=two()),
        },
        {
            "source": InfixExpression(left=two(), operator="+", right=one()),
            "expected": InfixExpression(left=two(), operator="+", right=two()),
        },
        {
            "source": PrefixExpression(operator="-", right=one()),
            "expected": PrefixExpression(operator="-", right=two()),
        },
        {
            "source": InfixExpression(left=two(), operator="+", right=one()),
            "expected": InfixExpression(left=two(), operator="+", right=two()),
        },
        {
            "source": IndexExpression(left=one(), index=one()),
            "expected": IndexExpression(left=two(), index=two()),
        },
        {
            "source": IfExpression(
                condition=one(),
                consequence=BlockStatement(statements=[ExpressionStatement(expression=one())]),
                alternative=BlockStatement(statements=[ExpressionStatement(expression=one())]),
            ),
            "expected": IfExpression(
                condition=two(),
                consequence=BlockStatement(statements=[ExpressionStatement(expression=two())]),
                alternative=BlockStatement(statements=[ExpressionStatement(expression=two())]),
            ),
        },
        {
            "source": ReturnStatement(return_value=one()),
            "expected": ReturnStatement(return_value=two()),
        },
        {
            "source": LetStatement(value=one()),
            "expected": LetStatement(value=two()),
        },
        {
            "source": FunctionLiteral(
                parameters=[], body=BlockStatement(statements=[ExpressionStatement(expression=one())])
            ),
            "expected": FunctionLiteral(
                parameters=[], body=BlockStatement(statements=[ExpressionStatement(expression=two())])
            ),
        },
        {
            "source": ArrayLiteral(elements=[one(), one()]),
            "expected": ArrayLiteral(elements=[two(), two()]),
        },
    ]

    for test in tests:
        modified = modify(test["source"], turn_one_into_two)

        assert modified == test["expected"]

    hash_literal = HashLiteral(pairs={one(): one()})

    modified = modify(hash_literal, turn_one_into_two)

    for key, value in modified.pairs.items():
        assert key.value == 2
        assert value.value == 2
