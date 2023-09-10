from typing import Tuple

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
    Node,
    PrefixExpression,
    Program,
    ReturnStatement,
    Statement,
    StringLiteral,
    modify,
)
from py_interpreter.monkey_thing import (
    ARRAY_THING,
    ERROR_THING,
    HASH_THING,
    INTEGER_THING,
    RETURN_VALUE_THING,
    Array,
    Boolean,
    Builtin,
    Environment,
    Error,
    Function,
    Hash,
    HashPair,
    Integer,
    Macro,
    Null,
    Quote,
    ReturnValue,
    String,
    Thing,
    new_enclosed_environment,
)
from py_interpreter.monkey_token import Token

NULL = Null()
TRUE = Boolean(value=True)
FALSE = Boolean(value=False)


def _evaluate_program(program: Program, env: Environment) -> Thing:
    result: Thing = NULL

    for statement in program.statements:
        result = evaluate(statement, env)

        match result:
            case ReturnValue():
                return result.value
            case Error():
                return result

    return result


def _evaluate_block_statement(block: BlockStatement, env: Environment) -> Thing:
    result: Thing = NULL

    for statement in block.statements:
        result = evaluate(statement, env)

        if result is not None and (result.kind() == RETURN_VALUE_THING or result.kind() == ERROR_THING):
            return result

    return result


def _evaluate_bang_operator_expression(right: Thing) -> Thing:
    thing = FALSE
    if isinstance(right, Boolean) and right.value is True:
        thing = FALSE
    elif isinstance(right, Boolean) and right.value is False:
        thing = TRUE
    elif isinstance(right, Null):
        thing = TRUE

    return thing


def _evaluate_minus_prefix_operator_expression(right: Thing):
    if not isinstance(right, Integer):
        return _new_error(f"unknown operator: -{right.kind()}")

    return Integer(value=-right.value)


def _evaluate_prefix_expression(operator: str, right: Thing) -> Thing:
    match operator:
        case "!":
            return _evaluate_bang_operator_expression(right)
        case "-":
            return _evaluate_minus_prefix_operator_expression(right)
        case _:
            return _new_error(f"unknown operator: {operator}{right}")


def _evaluate_integer_infix_expression(operator: str, left: Integer, right: Integer) -> Thing:
    match operator:
        case "+":
            return Integer(value=left.value + right.value)
        case "-":
            return Integer(value=left.value - right.value)
        case "*":
            return Integer(value=left.value * right.value)
        case "/":
            return Integer(value=left.value // right.value)
        case "<":
            return _native_bool_to_boolean_thing(left.value < right.value)
        case ">":
            return _native_bool_to_boolean_thing(left.value > right.value)
        case "==":
            return _native_bool_to_boolean_thing(left.value == right.value)
        case "!=":
            return _native_bool_to_boolean_thing(left.value != right.value)
        case _:
            return _new_error(f"unknown operator: {left.kind()} {operator} {right.kind()}")


def _evaluate_string_infix_expression(operator, left, right) -> Thing:
    match operator:
        case "+":
            return String(f"{left.value}{right.value}")
        case _:
            return _new_error(f"unknown operator: {left.kind()} {operator} {right.kind()}")


def _evaluate_infix_expression(operator: str, left: Thing, right: Thing) -> Thing:
    if isinstance(left, Integer) and isinstance(right, Integer):
        thing = _evaluate_integer_infix_expression(operator, left, right)
    elif operator == "==":
        thing = _native_bool_to_boolean_thing(left == right)
    elif operator == "!=":
        thing = _native_bool_to_boolean_thing(left != right)
    elif left.kind() != right.kind():
        thing = _new_error(f"type mismatch: {left.kind()} {operator} {right.kind()}")
    elif isinstance(left, String) and isinstance(right, String):
        thing = _evaluate_string_infix_expression(operator, left, right)
    else:
        thing = _new_error(f"unknown operator: {left.kind()} {operator} {right.kind()}")

    return thing


def _is_truthy(thing: Thing) -> bool:
    if thing == NULL:
        return False
    elif thing == TRUE:
        return True
    elif thing == FALSE:
        return False
    else:
        return True


def _evaluate_if_expression(if_expression: IfExpression, env: Environment) -> Thing:
    condition = evaluate(if_expression.condition, env)

    if _is_truthy(condition):
        return evaluate(if_expression.consequence, env)
    elif if_expression.alternative is not None:
        return evaluate(if_expression.alternative, env)
    else:
        return NULL


def _evaluate_identifier(node: Identifier, env: Environment) -> Thing:
    value = env.get(node.value)
    if value is not None:
        return value

    builtin = builtins.get(node.value, None)
    if builtin is not None:
        return builtin

    return _new_error(f"identifier not found: {node.value}")


def _evaluate_expressions(expressions: list[Expression], env: Environment) -> list[Thing]:
    result = []
    for e in expressions:
        evaluated = evaluate(e, env)
        if isinstance(evaluated, Error):
            return [evaluated]
        result.append(evaluated)

    return result


def _evaluate_array_index_expression(array: Thing, index: Thing) -> Thing:
    idx = index.value

    max = len(array.elements) - 1

    if idx < 0 or idx > max:
        return NULL

    return array.elements[idx]


def _evaluate_hash_index_expression(hash: Thing, index: Thing) -> Thing:
    if not hasattr(index, "hash_key"):
        return _new_error(f"unusable as hash key: {index.kind()}")

    pair = hash.pairs.get(index.hash_key(), None)

    if pair is None:
        return NULL

    return pair.value


def _evaluate_index_expression(left: Thing, index: Thing) -> Thing:
    if left.kind() == ARRAY_THING and index.kind() == INTEGER_THING:
        return _evaluate_array_index_expression(left, index)
    elif left.kind() == HASH_THING:
        return _evaluate_hash_index_expression(left, index)

    return _new_error(f"index operator not supported: {left.kind()}")


def _evaluate_hash_literal(node: HashLiteral, env: Environment) -> Thing:
    pairs = {}

    for key_node, value_node in node.pairs.items():
        key = evaluate(key_node, env)
        if isinstance(key, Error):
            return key

        if not hasattr(key, "hash_key"):
            return _new_error(f"unusable as hash key: {key.kind()}")

        value = evaluate(value_node, env)
        if isinstance(value, Error):
            return value

        hashed = key.hash_key()
        pairs[hashed] = HashPair(key=key, value=value)

    return Hash(pairs=pairs)


def _native_bool_to_boolean_thing(source: bool) -> Boolean:
    return TRUE if source else FALSE


def _new_error(message: str) -> Error:
    return Error(message=message)


def _extend_function_env(fn: Function, args: list[Thing]) -> Environment:
    env = new_enclosed_environment(fn.env)

    for i, param in enumerate(fn.parameters):
        env.set(param.value, args[i])

    return env


def _unwrap_return_value(thing: Thing) -> Thing:
    if isinstance(thing, ReturnValue):
        return thing.value

    return thing


def _apply_function(fn: Thing, args: list[Thing]) -> Thing:
    if isinstance(fn, Function):
        extended_env = _extend_function_env(fn, args)
        evaluated = evaluate(fn.body, extended_env)
        return _unwrap_return_value(evaluated)
    elif isinstance(fn, Builtin):
        return fn.function(*args)

    return _new_error(f"not a function: {fn.kind()}")


def _convert_thing_to_ast_node(thing: Thing) -> Node:
    match thing:
        case Integer():
            token = Token(kind=t.INT, literal=str(thing.value))
            return IntegerLiteral(token=token, value=thing.value)

        case Boolean():
            if thing.value:
                token = Token(kind=t.TRUE, literal="true")
            else:
                token = Token(kind=t.FALSE, literal="false")

            return BooleanExpression(token=token, value=thing.value)

        case Quote():
            return thing.node

        case _:
            return None


def _is_unquote_call(node: Node) -> bool:
    if not isinstance(node, CallExpression):
        return False

    return node.function.token_literal() == "unquote"


def _evaluate_unquote_calls(quoted: Node, env: Environment) -> Node:
    def modifier(node: Node) -> Node:
        if not _is_unquote_call(node):
            return node

        if not isinstance(node, CallExpression):
            return node

        if len(node.arguments) != 1:
            return node

        unquoted = evaluate(node.arguments[0], env)
        return _convert_thing_to_ast_node(unquoted)

    return modify(quoted, modifier)


def _quote(node: Node, env: Environment) -> Thing:
    node = _evaluate_unquote_calls(node, env)
    return Quote(node)


def evaluate(node: Node, env: Environment) -> Thing:
    match node:
        # Statements
        case Program():
            return _evaluate_program(node, env)
        case ExpressionStatement():
            return evaluate(node.expression, env)
        case BlockStatement():
            return _evaluate_block_statement(node, env)
        case ReturnStatement():
            value = evaluate(node.return_value, env)
            return ReturnValue(value=value)
        case LetStatement():
            value = evaluate(node.value, env)
            if isinstance(value, Error):
                return value

            env.set(node.name.value, value)
        case Identifier():
            return _evaluate_identifier(node, env)

        # Expressions
        case IntegerLiteral():
            return Integer(value=node.value)
        case StringLiteral():
            return String(value=node.value)
        case BooleanExpression():
            return _native_bool_to_boolean_thing(source=node.value)
        case PrefixExpression():
            right = evaluate(node.right, env)
            return _evaluate_prefix_expression(node.operator, right)
        case InfixExpression():
            left = evaluate(node.left, env)
            right = evaluate(node.right, env)
            return _evaluate_infix_expression(node.operator, left, right)
        case IfExpression():
            return _evaluate_if_expression(node, env)
        case FunctionLiteral():
            return Function(parameters=node.parameters, body=node.body, env=env)
        case CallExpression():
            if node.function.token_literal() == "quote":
                return _quote(node.arguments[0], env)

            function = evaluate(node.function, env)
            if isinstance(function, Error):
                return function

            args = _evaluate_expressions(node.arguments, env)
            if len(args) == 1 and isinstance(args[0], Error):
                return args[0]

            return _apply_function(function, args)
        case ArrayLiteral():
            elements = _evaluate_expressions(node.elements, env)
            if len(elements) == 1 and isinstance(elements[0], Error):
                return elements[0]

            return Array(elements=elements)
        case IndexExpression():
            left = evaluate(node.left, env)
            if isinstance(left, Error):
                return left

            index = evaluate(node.index, env)
            if isinstance(index, Error):
                return index

            return _evaluate_index_expression(left, index)
        case HashLiteral():
            return _evaluate_hash_literal(node, env)

    return None


def _is_macro_definition(node: Statement) -> bool:
    return isinstance(node, LetStatement) and isinstance(node.value, MacroLiteral)


def _add_macro(statement: Statement, env: Environment):
    macro_literal = statement.value

    macro = Macro(parameters=macro_literal.parameters, env=env, body=macro_literal.body)

    env.set(statement.name.value, macro)


def define_macros(program: Program, env: Environment):
    definitions = []

    for i, statement in enumerate(program.statements):
        if _is_macro_definition(statement):
            _add_macro(statement, env)
            definitions.append(i)

    program.statements = [s for i, s in enumerate(program.statements) if i not in definitions]
    # for i in definitions:
    #     program.statements.pop(i)


def _is_macro_call(expression: CallExpression, env: Environment) -> Tuple[Macro | None, bool]:
    identifier = expression.function
    if not isinstance(identifier, Identifier):
        return None, False

    thing = env.get(identifier.value)
    if thing is None:
        return None, False

    if not isinstance(thing, Macro):
        return None, False

    return thing, True


def _quote_args(expression: CallExpression) -> list[Quote]:
    return [Quote(node=arg) for arg in expression.arguments]


def _extend_macro_env(macro: Macro, args: list[Quote]) -> Environment:
    extended = new_enclosed_environment(macro.env)

    for i, parameter in enumerate(macro.parameters):
        extended.set(parameter.value, args[i])

    return extended


def expand_macros(program: Node, env: Environment) -> Node:
    def modifier(node: Node) -> Node:
        if not isinstance(node, CallExpression):
            return node

        macro, is_macro_call = _is_macro_call(node, env)
        if not is_macro_call:
            return node

        args = _quote_args(node)
        eval_env = _extend_macro_env(macro, args)

        evaluated = evaluate(macro.body, eval_env)

        if not isinstance(evaluated, Quote):
            raise RuntimeError("we only support returnin AST-nodes from macros")

        return evaluated.node

    return modify(program, modifier)


### BUILTINS


def _len(*args) -> Thing:
    if len(args) != 1:
        return _new_error(f"wrong number of arguments. got={len(args)}, want=1")

    if isinstance(args[0], String):
        return Integer(value=len(args[0].value))
    elif isinstance(args[0], Array):
        return Integer(value=len(args[0].elements))

    return _new_error(f"argument to 'len' not supported, got {args[0].kind()}")


def _first(*args) -> Thing:
    if len(args) != 1:
        return _new_error(f"wrong number of arguments. got={len(args)}, want=1")

    array = args[0]

    if not isinstance(array, Array):
        return _new_error(f"argument to 'first' must be ARRAY, got {args[0].kind()}")

    if len(array.elements) > 0:
        return array.elements[0]

    return NULL


def _last(*args) -> Thing:
    if len(args) != 1:
        return _new_error(f"wrong number of arguments. got={len(args)}, want=1")

    array = args[0]

    if not isinstance(array, Array):
        return _new_error(f"argument to 'last' must be ARRAY, got {args[0].kind()}")

    if len(array.elements) > 0:
        return array.elements[-1]

    return NULL


def _rest(*args) -> Thing:
    if len(args) != 1:
        return _new_error(f"wrong number of arguments. got={len(args)}, want=1")

    if not isinstance(args[0], Array):
        return _new_error(f"argument to 'rest' must be ARRAY, got {args[0].kind()}")

    array = args[0]

    length = len(array.elements)
    if length > 0:
        return Array(elements=[e for e in array.elements[1:]])

    return NULL


def _push(*args) -> Thing:
    if len(args) != 2:
        return _new_error(f"wrong number of arguments. got={len(args)}, want=2")

    if not isinstance(args[0], Array):
        return _new_error(f"first argument to 'push' must be ARRAY, got {args[0].kind()}")

    array = args[0]
    new_array = [e for e in array.elements]
    new_array.append(args[1])

    return Array(elements=new_array)


def _puts(*args) -> Thing:
    for arg in args:
        print(arg.inspect())

    return NULL


builtins = {
    "len": Builtin(_len),
    "first": Builtin(_first),
    "last": Builtin(_last),
    "rest": Builtin(_rest),
    "push": Builtin(_push),
    "puts": Builtin(_puts),
}
