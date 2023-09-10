from typing import Any

from py_interpreter.monkey_ast import (
    ArrayLiteral,
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
    ReturnStatement,
    StringLiteral,
)
from py_interpreter.monkey_lexer import Lexer
from py_interpreter.monkey_parser import Parser


def _test_let_statement(statement: LetStatement, expected_identifier: str) -> bool:
    if statement.token_literal() != "let":
        raise Exception(f"statement.token_literal() is not 'let'. Got: {statement.token_literal()}")

    if not isinstance(statement, LetStatement):
        raise Exception(f"statement is not a 'LetStatement'. Got: {type(statement)}")

    if statement.name.value != expected_identifier:
        raise Exception(f"statement.name.value is not '{expected_identifier}'. Got: {statement.name.value}")

    if statement.name.token_literal() != expected_identifier:
        raise Exception(
            f"statement.name.token_literal() is not '{expected_identifier}. Got: {statement.name.token_literal()}"
        )

    return True


def _test_integer_literal(expression: Expression | None, expected_value: int) -> bool:
    if not isinstance(expression, IntegerLiteral):
        raise Exception(f"expression is not an 'IntegerLiteral'. Got: '{type(expression)}'")

    if expression.value != expected_value:
        raise Exception(f"expression.value not '{expected_value}'. Got: '{expression.value}'")

    if expression.token_literal() != str(expected_value):
        raise Exception(f"expression.token_literal() is not '{expected_value}'. Got: '{expression.token_literal()}")

    return True


def _test_identifier(expression: Expression | None, expected_value: str) -> bool:
    if not isinstance(expression, Identifier):
        raise Exception(f"expression is not an 'Identifier'. Got: '{type(expression)}'")

    if expression.value != expected_value:
        raise Exception(f"expression.value is not '{expected_value}'. Got: '{expression.value}'")

    if expression.token_literal() != expected_value:
        raise Exception(f"expression.token_literal() is not '{expected_value}'. Got: '{expression.token_literal()}'")

    return True


def _test_boolean_literal(expression: Expression | None, expected_value: bool) -> bool:
    if not isinstance(expression, BooleanExpression):
        raise Exception(f"expression is not an 'Boolean'. Got: '{type(expression)}'")

    if expression.value != expected_value:
        raise Exception(f"expression.value is not '{expected_value}'. Got: '{expression.value}'")

    if expression.token_literal() != str(expected_value).lower():
        raise Exception(
            f"expression.token_literal() is not '{str(expected_value).lower()}'. Got: '{expression.token_literal()}'"
        )

    return True


def _test_literal_expression(expression: Expression | None, expected: Any) -> bool:
    if isinstance(expected, bool):
        return _test_boolean_literal(expression, expected)
    elif isinstance(expected, int):
        return _test_integer_literal(expression, int(expected))
    elif isinstance(expected, str):
        return _test_identifier(expression, expected)

    raise Exception(f"type expression not handled. Got: '{type(expected)}'")


def _test_infix_expression(
    expression: Expression, expected_left: Any, expected_operator: str, expected_right: Any
) -> bool:
    if not isinstance(expression, InfixExpression):
        raise Exception(f"expression is not 'InfixExpression'. Got: '{type(expression)}'")

    if not _test_literal_expression(expression.left, expected_left):
        print(expected_left)
        return False

    if expression.operator != expected_operator:
        raise Exception(f"expression.operator is not '{expected_operator}'. Got: '{expression.operator}'")

    if not _test_literal_expression(expression.right, expected_right):
        return False

    return True


def _check_parser_errors(parser: Parser):
    errors = parser.errors

    if len(errors) == 0:
        return

    pretty_errors = "\nparser error: " + "\nparser error: ".join(errors)

    raise Exception(f"parser has {len(errors)} error{'s' if len(errors) > 1 else ''}: {pretty_errors}")


def test_let_statements():
    tests = [
        {"source": "let x = 5;", "expected_identifier": "x", "expected_value": 5},
        {"source": "let y = true;", "expected_identifier": "y", "expected_value": True},
        {"source": "let foobar = y;", "expected_identifier": "foobar", "expected_value": "y"},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        assert len(program.statements) == 1

        statement = program.statements[0]
        assert _test_let_statement(statement, test["expected_identifier"])

        value = statement.value
        assert _test_literal_expression(value, test["expected_value"])


def test_return_statements():
    tests = [
        {"source": "return 5;", "expected_value": 5},
        {"source": "return 10;", "expected_value": 10},
        {"source": "return 993322;", "expected_value": 993322},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        assert len(program.statements) == 1

        statement = program.statements[0]
        assert isinstance(statement, ReturnStatement)
        assert statement.token_literal() == "return"
        assert _test_literal_expression(statement.return_value, test["expected_value"])


def test_identifier_expression():
    source = "foobar;"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression_statement = program.statements[0]
    assert isinstance(expression_statement, ExpressionStatement)

    assert _test_identifier(expression_statement.expression, "foobar")


def test_integer_literal_expression():
    source = "5;"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression_statement = program.statements[0]
    assert isinstance(expression_statement, ExpressionStatement)

    assert _test_integer_literal(expression_statement.expression, 5)


def test_boolean_expression():
    source = "true;"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression_statement = program.statements[0]
    assert isinstance(expression_statement, ExpressionStatement)

    assert _test_boolean_literal(expression_statement.expression, True)


def test_parsing_prefix_expressions():
    tests = [
        {"source": "!5;", "operator": "!", "value": 5},
        {"source": "-15;", "operator": "-", "value": 15},
        {"source": "!true", "operator": "!", "value": True},
        {"source": "!false", "operator": "!", "value": False},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        assert len(program.statements) == 1

        expression_statement = program.statements[0]
        assert isinstance(expression_statement, ExpressionStatement)

        prefix_expression = expression_statement.expression
        assert isinstance(prefix_expression, PrefixExpression)

        assert prefix_expression.operator == test["operator"]

        assert _test_literal_expression(expression=prefix_expression.right, expected=test["value"])


def test_parsing_infix_expressions():
    tests = [
        {"source": "5 + 5;", "left_value": 5, "operator": "+", "right_value": 5},
        {"source": "5 - 5;", "left_value": 5, "operator": "-", "right_value": 5},
        {"source": "5 * 5;", "left_value": 5, "operator": "*", "right_value": 5},
        {"source": "5 / 5;", "left_value": 5, "operator": "/", "right_value": 5},
        {"source": "5 < 5;", "left_value": 5, "operator": "<", "right_value": 5},
        {"source": "5 > 5;", "left_value": 5, "operator": ">", "right_value": 5},
        {"source": "5 == 5;", "left_value": 5, "operator": "==", "right_value": 5},
        {"source": "5 != 5;", "left_value": 5, "operator": "!=", "right_value": 5},
        {"source": "true == true", "left_value": True, "operator": "==", "right_value": True},
        # {"source": "true != false", "left_value": True, "operator": "!=", "right_value": False},
        # {"source": "false == false", "left_value": False, "operator": "==", "right_value": False},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        assert len(program.statements) == 1

        expression_statement = program.statements[0]
        assert isinstance(expression_statement, ExpressionStatement)

        assert _test_infix_expression(
            expression=expression_statement.expression,
            expected_left=test["left_value"],
            expected_operator=test["operator"],
            expected_right=test["right_value"],
        )


def test_operator_precedence_parsing():
    tests = [
        {"source": "-a * b", "expected": "((-a) * b)"},
        {"source": "!-a", "expected": "(!(-a))"},
        {"source": "a + b + c", "expected": "((a + b) + c)"},
        {"source": "a + b - c", "expected": "((a + b) - c)"},
        {"source": "a * b * c", "expected": "((a * b) * c)"},
        {"source": "a * b / c", "expected": "((a * b) / c)"},
        {"source": "a + b / c", "expected": "(a + (b / c))"},
        {"source": "a + b * c + d / e - f", "expected": "(((a + (b * c)) + (d / e)) - f)"},
        {"source": "3 + 4; -5 * 5", "expected": "(3 + 4)((-5) * 5)"},
        {"source": "5 > 4 == 3 < 4", "expected": "((5 > 4) == (3 < 4))"},
        {"source": "5 < 4 != 3 > 4", "expected": "((5 < 4) != (3 > 4))"},
        {"source": "3 + 4 * 5 == 3 * 1 + 4 * 5", "expected": "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
        {"source": "true", "expected": "true"},
        {"source": "false", "expected": "false"},
        {"source": "3 > 5 == false", "expected": "((3 > 5) == false)"},
        {"source": "3 < 5 == true", "expected": "((3 < 5) == true)"},
        {"source": "1 + ( 2 + 3 ) + 4", "expected": "((1 + (2 + 3)) + 4)"},
        {"source": "(5 + 5) * 2", "expected": "((5 + 5) * 2)"},
        {"source": "2 / (5 + 5)", "expected": "(2 / (5 + 5))"},
        {"source": "-(5 + 5)", "expected": "(-(5 + 5))"},
        {"source": "!(true == true)", "expected": "(!(true == true))"},
        {"source": "a + add(b * c) + d", "expected": "((a + add((b * c))) + d)"},
        {
            "source": "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "expected": "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        {"source": "add(a + b + c * d / f + g)", "expected": "add((((a + b) + ((c * d) / f)) + g))"},
        {"source": "a * [1, 2, 3, 4][b * c] * d", "expected": "((a * ([1, 2, 3, 4][(b * c)])) * d)"},
        {"source": "add(a * b[2], b[1], 2 * [1, 2][1])", "expected": "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        assert str(program) == test["expected"]


def test_if_expression():
    source = "if (x < y) { x }"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression = program.statements[0].expression
    assert isinstance(expression, IfExpression)

    assert _test_infix_expression(
        expression=expression.condition, expected_left="x", expected_operator="<", expected_right="y"
    )

    assert len(expression.consequence.statements) == 1

    consequence = expression.consequence.statements[0]
    assert isinstance(consequence, ExpressionStatement)

    assert _test_identifier(consequence.expression, "x")

    assert expression.alternative is None


def test_if_else_expression():
    source = "if (x < y) { x } else { y }"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression = program.statements[0].expression
    assert isinstance(expression, IfExpression)

    assert _test_infix_expression(
        expression=expression.condition, expected_left="x", expected_operator="<", expected_right="y"
    )

    assert len(expression.consequence.statements) == 1

    consequence = expression.consequence.statements[0]
    assert isinstance(consequence, ExpressionStatement)

    assert _test_identifier(consequence.expression, "x")

    alternative = expression.alternative.statements[0]
    assert isinstance(alternative, ExpressionStatement)

    assert _test_identifier(alternative.expression, "y")


def test_function_literal_parsing():
    source = "fn(x, y) { x + y; }"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression_statement = program.statements[0]
    assert isinstance(expression_statement, ExpressionStatement)

    function_literal = expression_statement.expression
    assert isinstance(function_literal, FunctionLiteral)

    assert len(function_literal.parameters) == 2

    assert _test_literal_expression(function_literal.parameters[0], "x")
    assert _test_literal_expression(function_literal.parameters[1], "y")

    assert len(function_literal.body.statements) == 1

    body_statement = function_literal.body.statements[0]
    assert isinstance(body_statement, ExpressionStatement)

    assert _test_infix_expression(body_statement.expression, "x", "+", "y")


def test_function_parameters_parsing():
    tests = [
        {"source": "fn() {};", "expected_params": []},
        {"source": "fn(x) {};", "expected_params": ["x"]},
        {"source": "fn(x, y, z) {};", "expected_params": ["x", "y", "z"]},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        statement = program.statements[0]
        function = statement.expression

        assert len(function.parameters) == len(test["expected_params"])

        for i, identifier in enumerate(test["expected_params"]):
            assert _test_literal_expression(function.parameters[i], identifier)


def test_call_expression_parsing():
    source = "add(1, 2 * 3, 4 + 5)"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    expression_statement = program.statements[0]
    assert isinstance(expression_statement, ExpressionStatement)

    expression = expression_statement.expression
    assert isinstance(expression, CallExpression)

    assert _test_identifier(expression.function, "add")

    assert len(expression.arguments) == 3

    assert _test_literal_expression(expression.arguments[0], 1)
    assert _test_infix_expression(expression.arguments[1], 2, "*", 3)
    assert _test_infix_expression(expression.arguments[2], 4, "+", 5)


def test_call_expression_arguments_parsing():
    tests = [
        {"source": "add()", "expected_params": []},
        {"source": "add(x)", "expected_params": ["x"]},
        {"source": "add(x, y, z)", "expected_params": ["x", "y", "z"]},
    ]

    for test in tests:
        lexer = Lexer(test["source"])
        parser = Parser(lexer)

        program = parser.parse_program()
        _check_parser_errors(parser)

        statement = program.statements[0]
        call_expression = statement.expression

        assert len(call_expression.arguments) == len(test["expected_params"])

        for i, identifier in enumerate(test["expected_params"]):
            assert _test_literal_expression(call_expression.arguments[i], identifier)


def test_string_literal():
    source = '"hello world";'

    lexer = Lexer(source)
    parser = Parser(lexer)
    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    literal = statement.expression

    assert isinstance(literal, StringLiteral)

    assert literal.value == "hello world"


def test_parsing_array_literals():
    source = "[1, 2 * 2, 3 + 3]"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    array = statement.expression

    assert isinstance(array, ArrayLiteral)

    assert len(array.elements) == 3

    assert _test_integer_literal(array.elements[0], 1)
    assert _test_infix_expression(array.elements[1], 2, "*", 2)
    assert _test_infix_expression(array.elements[2], 3, "+", 3)


def test_parsing_empty_array_literal():
    source = "[]"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    array = statement.expression

    assert isinstance(array, ArrayLiteral)

    assert len(array.elements) == 0


def test_parsing_index_expressions():
    source = "my_array[1 + 1]"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    index_expression = statement.expression

    assert isinstance(index_expression, IndexExpression)

    assert _test_identifier(index_expression.left, "my_array")
    assert _test_infix_expression(index_expression.index, 1, "+", 1)


def test_parsing_hash_literals_string_keys():
    source = '{"one": 1, "two": 2, "three": 3}'

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    hash = statement.expression

    assert isinstance(hash, HashLiteral)

    assert len(hash.pairs) == 3

    expected = {"one": 1, "two": 2, "three": 3}

    for key, value in hash.pairs.items():
        assert isinstance(key, StringLiteral)

        expected_value = expected[key.value]

        assert _test_integer_literal(value, expected_value)


def test_parsing_hash_literals_boolean_keys():
    source = "{true: 1, false: 0}"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    hash = statement.expression

    assert isinstance(hash, HashLiteral)

    assert len(hash.pairs) == 2

    expected = {True: 1, False: 0}

    for key, value in hash.pairs.items():
        assert isinstance(key, BooleanExpression)

        expected_value = expected[key.value]

        assert _test_integer_literal(value, expected_value)


def test_parsing_hash_literals_integer_keys():
    source = "{1: 1, 2: 2, 3: 3}"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    hash = statement.expression

    assert isinstance(hash, HashLiteral)

    assert len(hash.pairs) == 3

    expected = {1: 1, 2: 2, 3: 3}

    for key, value in hash.pairs.items():
        assert isinstance(key, IntegerLiteral)

        expected_value = expected[key.value]

        assert _test_integer_literal(value, expected_value)


def test_parsing_empty_hash_literal():
    source = "{}"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    hash = statement.expression

    assert isinstance(hash, HashLiteral)

    assert len(hash.pairs) == 0


def test_parsing_hash_literals_with_expressions():
    source = '{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}'

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    statement = program.statements[0]
    hash = statement.expression

    assert isinstance(hash, HashLiteral)

    assert len(hash.pairs) == 3

    tests = {
        "one": lambda e: _test_infix_expression(e, 0, "+", 1),
        "two": lambda e: _test_infix_expression(e, 10, "-", 8),
        "three": lambda e: _test_infix_expression(e, 15, "/", 5),
    }

    for key, value in hash.pairs.items():
        assert isinstance(key, StringLiteral)

        tests[key.value](value)


def test_macro_literal_parsing():
    source = "macro(x, y) { x + y; }"

    lexer = Lexer(source)
    parser = Parser(lexer)

    program = parser.parse_program()
    _check_parser_errors(parser)

    assert len(program.statements) == 1

    statement = program.statements[0]
    assert isinstance(statement, ExpressionStatement)

    macro = statement.expression

    assert isinstance(macro, MacroLiteral)

    assert len(macro.parameters) == 2

    assert _test_literal_expression(macro.parameters[0], "x")
    assert _test_literal_expression(macro.parameters[1], "y")

    assert len(macro.body.statements) == 1

    body_statement = macro.body.statements[0]
    assert isinstance(body_statement, ExpressionStatement)

    assert _test_infix_expression(body_statement.expression, "x", "+", "y")
