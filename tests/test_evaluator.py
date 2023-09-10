from py_interpreter.monkey_ast import Program
from py_interpreter.monkey_evaluator import FALSE, NULL, TRUE, define_macros, evaluate, expand_macros
from py_interpreter.monkey_lexer import Lexer
from py_interpreter.monkey_parser import Parser
from py_interpreter.monkey_thing import (
    Array,
    Boolean,
    Error,
    Function,
    Hash,
    Integer,
    Macro,
    Null,
    Quote,
    String,
    Thing,
    new_environment,
)


def _test_eval(source: str) -> Thing | None:
    lexer = Lexer(source)
    parser = Parser(lexer)
    program = parser.parse_program()
    env = new_environment()

    return evaluate(program, env)


def _test_integer_thing(thing: Thing, expected: int) -> bool:
    assert isinstance(thing, Integer)
    assert thing.value == expected

    return True


def _test_boolean_thing(thing: Thing, expected: bool) -> bool:
    assert isinstance(thing, Boolean)
    assert thing.value == expected

    return True


def _test_null_thing(thing: Thing) -> bool:
    assert thing == NULL

    return True


def _test_array_thing(thing: Thing, expected: list) -> bool:
    assert isinstance(thing, Array)
    assert [e.value for e in thing.elements] == expected

    return True


def _test_parse_program(source: str) -> Program:
    lexer = Lexer(source)
    parser = Parser(lexer)

    return parser.parse_program()


def test_eval_integer_expression():
    tests = [
        {"source": "5", "expected": 5},
        {"source": "10", "expected": 10},
        {"source": "-5", "expected": -5},
        {"source": "-10", "expected": -10},
        {"source": "5 + 5 + 5 + 5 - 10", "expected": 10},
        {"source": "2 * 2 * 2 * 2 * 2", "expected": 32},
        {"source": "-50 + 100 + -50", "expected": 0},
        {"source": "5 * 2 + 10", "expected": 20},
        {"source": "5 + 2 * 10", "expected": 25},
        {"source": "20 + 2 * -10", "expected": 0},
        {"source": "50 / 2 * 2 + 10", "expected": 60},
        {"source": "2 * (5 + 10)", "expected": 30},
        {"source": "3 * 3 * 3 + 10", "expected": 37},
        {"source": "3 * (3 * 3) + 10", "expected": 37},
        {"source": "(5 + 10 * 2 + 15 / 3) * 2 + -10", "expected": 50},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])
        assert _test_integer_thing(evaluated, test["expected"])


def test_eval_boolean_expression():
    tests = [
        {"source": "true", "expected": True},
        {"source": "false", "expected": False},
        {"source": "1 < 2", "expected": True},
        {"source": "1 > 2", "expected": False},
        {"source": "1 < 1", "expected": False},
        {"source": "1 > 1", "expected": False},
        {"source": "1 == 1", "expected": True},
        {"source": "1 != 1", "expected": False},
        {"source": "1 == 2", "expected": False},
        {"source": "1 != 2", "expected": True},
        {"source": "true == true", "expected": True},
        {"source": "false == false", "expected": True},
        {"source": "true == false", "expected": False},
        {"source": "true != false", "expected": True},
        {"source": "false != true", "expected": True},
        {"source": "(1 < 2) == true", "expected": True},
        {"source": "(1 < 2) == false", "expected": False},
        {"source": "(1 > 2) == true", "expected": False},
        {"source": "(1 > 2) == false", "expected": True},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])
        assert _test_boolean_thing(evaluated, test["expected"])


def test_bang_operator():
    tests = [
        {"source": "!true", "expected": False},
        {"source": "!false", "expected": True},
        {"source": "!5", "expected": False},
        {"source": "!!true", "expected": True},
        {"source": "!!false", "expected": False},
        {"source": "!!5", "expected": True},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])
        assert _test_boolean_thing(evaluated, test["expected"])


def test_if_else_expression():
    tests = [
        {"source": "if (true) { 10 }", "expected": 10},
        {"source": "if (false) { 10 }", "expected": None},
        {"source": "if (1) { 10 }", "expected": 10},
        {"source": "if (1 < 2) { 10 }", "expected": 10},
        {"source": "if (1 > 2) { 10 }", "expected": None},
        {"source": "if (1 > 2) { 10 } else { 20 }", "expected": 20},
        {"source": "if (1 < 2) { 10 } else { 20 }", "expected": 10},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        if isinstance(test["expected"], int):
            _test_integer_thing(evaluated, test["expected"])
        else:
            _test_null_thing(evaluated)


def test_return_statements():
    test = [
        {"source": "return 10;", "expected": 10},
        {"source": "return 10; 9;", "expected": 10},
        {"source": "return 2 * 5; 9;", "expected": 10},
        {"source": "9; return 2 * 5; 9;", "expected": 10},
        {
            "source": """
         if (10 > 1) {
             if (10 > 1) {
                 return 10;
             }
             return 1;
         }
         """,
            "expected": 10,
        },
    ]

    for test in test:
        evaluated = _test_eval(test["source"])
        _test_integer_thing(evaluated, test["expected"])


def test_error_handling():
    tests = [
        {"source": "5 + true;", "expected": "type mismatch: INTEGER + BOOLEAN"},
        {"source": "5 + true; 5;", "expected": "type mismatch: INTEGER + BOOLEAN"},
        {"source": "-true;", "expected": "unknown operator: -BOOLEAN"},
        {"source": "true + false;", "expected": "unknown operator: BOOLEAN + BOOLEAN"},
        {"source": "5; true + false; 5;", "expected": "unknown operator: BOOLEAN + BOOLEAN"},
        {"source": "if (10 > 1) { true + false; }", "expected": "unknown operator: BOOLEAN + BOOLEAN"},
        {
            "source": """
         if (10 > 1) {
             if (10 > 1) {
                 return true + false;
             }

             return 1;
         }
         """,
            "expected": "unknown operator: BOOLEAN + BOOLEAN",
        },
        {"source": '"Hello" - "World"', "expected": "unknown operator: STRING - STRING"},
        {"source": '{"name": "Monkey"}[fn(x) { x }];', "expected": "unusable as hash key: FUNCTION"},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        assert isinstance(evaluated, Error)

        assert evaluated.message == test["expected"]


def test_let_statements():
    tests = [
        {"source": "let a = 5; a;", "expected": 5},
        {"source": "let a = 5 * 5; a;", "expected": 25},
        {"source": "let a = 5; let b = a; b;", "expected": 5},
        {"source": "let a = 5; let b = a; let c = a + b + 5; c;", "expected": 15},
    ]

    for test in tests:
        _test_integer_thing(_test_eval(test["source"]), test["expected"])


def test_function_thing():
    source = "fn(x) { x + 2; }"

    evaluated = _test_eval(source)

    assert isinstance(evaluated, Function)

    assert len(evaluated.parameters) == 1

    assert str(evaluated.parameters[0]) == "x"

    expected_body = "(x + 2)"

    assert str(evaluated.body) == expected_body


def test_function_application():
    tests = [
        {"source": "let identity = fn(x) { x; }; identity(5);", "expected": 5},
        {"source": "let identity = fn(x) { return x; }; identity(5);", "expected": 5},
        {"source": "let double = fn(x) { x * 2; }; double(5);", "expected": 10},
        {"source": "let add = fn(x, y) { x + y; }; add(5, 5);", "expected": 10},
        {"source": "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", "expected": 20},
        {"source": "fn(x) { x; }(5)", "expected": 5},
    ]

    for test in tests:
        _test_integer_thing(_test_eval(test["source"]), test["expected"])


def test_closures():
    source = """
    let newAdder = fn(x) {
        fn(y) { x + y };
    };

    let addTwo = newAdder(2);
    addTwo(2);
    """

    _test_integer_thing(_test_eval(source), 4)


def test_string_literal():
    source = '"Hello World!"'

    evaluated = _test_eval(source)

    assert isinstance(evaluated, String)

    assert evaluated.value == "Hello World!"


def test_string_concatenation():
    source = '"Hello" + " " + "World!"'

    evaluated = _test_eval(source)

    assert isinstance(evaluated, String)

    assert evaluated.value == "Hello World!"


def test_builtin_functions():
    tests = [
        {"source": 'len("")', "expected": 0},
        {"source": 'len("four")', "expected": 4},
        {"source": 'len("hello world")', "expected": 11},
        {"source": "len(1)", "expected": "argument to 'len' not supported, got INTEGER"},
        {"source": 'len("one", "two")', "expected": "wrong number of arguments. got=2, want=1"},
        {"source": "first([1])", "expected": 1},
        {"source": "first([1,2,3])", "expected": 1},
        {"source": "first([])", "expected": None},
        {"source": "first(1)", "expected": "argument to 'first' must be ARRAY, got INTEGER"},
        {"source": "first([1,2], [3,4])", "expected": "wrong number of arguments. got=2, want=1"},
        {"source": "last([1])", "expected": 1},
        {"source": "last([1,2,3])", "expected": 3},
        {"source": "last([])", "expected": None},
        {"source": "last(1)", "expected": "argument to 'last' must be ARRAY, got INTEGER"},
        {"source": "last([1,2], [3,4])", "expected": "wrong number of arguments. got=2, want=1"},
        {"source": "rest([1])", "expected": []},
        {"source": "rest([1,2,3])", "expected": [2, 3]},
        {"source": "rest([])", "expected": None},
        {"source": "rest(1)", "expected": "argument to 'rest' must be ARRAY, got INTEGER"},
        {"source": "rest([1,2], [3,4])", "expected": "wrong number of arguments. got=2, want=1"},
        {"source": "push([1,2], 3)", "expected": [1, 2, 3]},
        {"source": "push([], 3)", "expected": [3]},
        {"source": "push(3, 3)", "expected": "first argument to 'push' must be ARRAY, got INTEGER"},
        {"source": "push(3)", "expected": "wrong number of arguments. got=1, want=2"},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        if isinstance(test["expected"], int):
            _test_integer_thing(evaluated, test["expected"])
        elif isinstance(test["expected"], str):
            assert isinstance(evaluated, Error)
            assert evaluated.message == test["expected"]
        elif test["expected"] is None:
            assert isinstance(evaluated, Null)
        elif isinstance(test["expected"], list):
            _test_array_thing(evaluated, test["expected"])


def test_array_literals():
    source = "[1, 2 * 2, 3 + 3]"

    evaluated = _test_eval(source)
    assert isinstance(evaluated, Array)

    assert len(evaluated.elements) == 3

    _test_integer_thing(evaluated.elements[0], 1)
    _test_integer_thing(evaluated.elements[1], 4)
    _test_integer_thing(evaluated.elements[2], 6)


def test_array_index_expressions():
    tests = [
        {"source": "[1, 2, 3][0]", "expected": 1},
        {"source": "[1, 2, 3][1]", "expected": 2},
        {"source": "[1, 2, 3][2]", "expected": 3},
        {"source": "let i = 0; [1][i];", "expected": 1},
        {"source": "[1, 2, 3][1 + 1];", "expected": 3},
        {"source": "let my_array = [1, 2, 3]; my_array[2];", "expected": 3},
        {"source": "let my_array = [1, 2, 3]; my_array[0] + my_array[1] + my_array[2];", "expected": 6},
        {"source": "let my_array = [1, 2, 3]; let i = my_array[0]; my_array[i]", "expected": 2},
        {"source": "[1, 2, 3][3]", "expected": None},
        {"source": "[1, 2, 3][-1]", "expected": None},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])
        if isinstance(test["expected"], int):
            _test_integer_thing(evaluated, test["expected"])
        else:
            _test_null_thing(evaluated)


def test_hash_literals():
    source = """
    let two = "two";
    {
        "one": 10 - 9,
        two: 1 + 1,
        "thr" + "ee": 6 / 2,
        4: 4,
        true: 5,
        false: 6
    }
    """

    evaluated = _test_eval(source)

    assert isinstance(evaluated, Hash)

    expected = {
        String(value="one").hash_key(): 1,
        String(value="two").hash_key(): 2,
        String(value="three").hash_key(): 3,
        Integer(value=4).hash_key(): 4,
        TRUE.hash_key(): 5,
        FALSE.hash_key(): 6,
    }

    assert len(evaluated.pairs) == len(expected)

    for expected_key, expected_value in expected.items():
        pair = evaluated.pairs[expected_key]

        _test_integer_thing(pair.value, expected_value)


def test_hash_index_expressions():
    tests = [
        {"source": '{"foo": 5}["foo"]', "expected": 5},
        {"source": '{"foo": 5}["bar"]', "expected": None},
        {"source": 'let key = "foo"; {"foo": 5}[key]', "expected": 5},
        {"source": '{}["foo"]', "expected": None},
        {"source": "{5: 5}[5]", "expected": 5},
        {"source": "{true: 5}[true]", "expected": 5},
        {"source": "{false: 5}[false]", "expected": 5},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        if isinstance(test["expected"], int):
            _test_integer_thing(evaluated, test["expected"])
        else:
            _test_null_thing(evaluated)


def test_quote():
    tests = [
        {"source": "quote(5)", "expected": "5"},
        {"source": "quote(5 + 8)", "expected": "(5 + 8)"},
        {"source": "quote(foobar)", "expected": "foobar"},
        {"source": "quote(foobar + barfoo)", "expected": "(foobar + barfoo)"},
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        assert isinstance(evaluated, Quote)
        assert evaluated is not None

        assert str(evaluated.node) == test["expected"]


def test_quote_unquote():
    tests = [
        {"source": "quote(unquote(4))", "expected": "4"},
        {"source": "quote(unquote(4 + 4))", "expected": "8"},
        {"source": "quote(8 + unquote(4 + 4))", "expected": "(8 + 8)"},
        {"source": "quote(unquote(4 + 4) + 8)", "expected": "(8 + 8)"},
        {"source": "let foobar = 8; quote(foobar)", "expected": "foobar"},
        {"source": "let foobar = 8; quote(unquote(foobar))", "expected": "8"},
        {"source": "quote(unquote(true))", "expected": "true"},
        {"source": "quote(unquote(true == false))", "expected": "false"},
        {"source": "quote(unquote(quote(4 + 4)))", "expected": "(4 + 4)"},
        {
            "source": """
                let quoted_infix_expression = quote(4 + 4);
                quote(unquote(4 + 4) + unquote(quoted_infix_expression))
            """,
            "expected": "(8 + (4 + 4))",
        },
    ]

    for test in tests:
        evaluated = _test_eval(test["source"])

        assert isinstance(evaluated, Quote)
        assert evaluated is not None

        assert str(evaluated.node) == test["expected"]


def test_define_macros():
    source = """
    let number = 1;
    let function = fn(x, y) { x + y };
    let mymacro = macro(x, y) { x + y };
    """

    env = new_environment()
    program = _test_parse_program(source)

    define_macros(program, env)

    assert len(program.statements) == 2

    assert env.get("number") is None
    assert env.get("function") is None

    macro = env.get("mymacro")
    assert macro is not None
    assert isinstance(macro, Macro)

    assert len(macro.parameters) == 2

    assert str(macro.parameters[0]) == "x"
    assert str(macro.parameters[1]) == "y"

    expected_body = "(x + y)"
    assert str(macro.body) == expected_body


def test_expand_macros():
    tests = [
        {
            "source": """
                let infixExpression = macro() { quote(1 + 2); };
                infixExpression();
             """,
            "expected": "(1 + 2)",
        },
        {
            "source": """
                let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
                reverse(2 + 2, 10 - 5)
            """,
            "expected": "(10 - 5) - (2 + 2)",
        },
        {
            "source": """
                let unless = macro(condition, consequence, alternative) {
                    quote(if(!(unquote(condition))) {
                        unquote(consequence);
                    } else {
                        unquote(alternative);
                    });
                };
                unless(10 > 5, puts("not greater"), puts("greater"));
            """,
            "expected": 'if(!(10 > 5)){ puts("not greater") } else { puts("greater") }',
        },
    ]

    for test in tests:
        expected = _test_parse_program(test["expected"])
        program = _test_parse_program(test["source"])

        env = new_environment()
        define_macros(program, env)
        expanded = expand_macros(program, env)

        assert str(expanded) == str(expected)
