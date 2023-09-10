import sys

from py_interpreter.monkey_evaluator import define_macros, evaluate, expand_macros
from py_interpreter.monkey_lexer import Lexer
from py_interpreter.monkey_parser import Parser
from py_interpreter.monkey_thing import new_environment

PROMPT = ">>"


def print_parse_errors(errors: list[str]):
    print(f"/!\\ Parser error{'s' if len(errors) > 1 else ''} /!\\")
    for e in errors:
        print(e)


def repl():
    print("Hello! This is the Monkey programming language!")
    print("Feel free to type in commands:")

    env = new_environment()
    macro_env = new_environment()

    while line := input(f"{PROMPT} "):
        lexer = Lexer(line)
        parser = Parser(lexer)

        program = parser.parse_program()

        if len(parser.errors) != 0:
            print_parse_errors(parser.errors)
            continue

        define_macros(program, macro_env)
        expanded = expand_macros(program, macro_env)

        evaluated = evaluate(expanded, env)
        if evaluated is not None:
            print(f"{evaluated.inspect()}")


def execute(filepath: str):
    with open(filepath, "r") as f:
        source_code = f.read()

        lexer = Lexer(source_code)
        parser = Parser(lexer)

        program = parser.parse_program()
        if len(parser.errors) != 0:
            print_parse_errors(parser.errors)

        evaluate(program, new_environment())


if __name__ == "__main__":
    if len(sys.argv) == 1:
        repl()
    elif len(sys.argv) == 2:
        execute(sys.argv[1])
    else:
        print("Too many arguments. Expecting a filename to execute or no argument to start the REPL.")
