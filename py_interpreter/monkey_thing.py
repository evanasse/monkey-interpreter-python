from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Callable, Protocol

from py_interpreter.monkey_ast import BlockStatement, Identifier, Node

ThingType = str

INTEGER_THING = "INTEGER"
BOOLEAN_THING = "BOOLEAN"
NULL_THING = "NULL"
RETURN_VALUE_THING = "RETURN_VALUE"
ERROR_THING = "ERROR"
FUNCTION_THING = "FUNCTION"
STRING_THING = "STRING"
BUILTIN_THING = "BUILTIN"
ARRAY_THING = "ARRAY"
HASH_THING = "HASH"
QUOTE_THING = "QUOTE"
MACRO_THING = "MACRO"


class Thing(Protocol):
    def kind(self):
        ...

    def inspect(self):
        ...


BuiltinFunction = Callable[[...], Thing]


@dataclass(frozen=True)
class HashKey:
    kind: ThingType
    value: int


class GenericEnvironment(ABC):
    @abstractmethod
    def get(self, name: str) -> Thing | None:
        pass

    @abstractmethod
    def set(self, name: str, value: Thing) -> Thing:
        pass


@dataclass
class Environment(GenericEnvironment):
    store: dict[str, Thing]
    outer: GenericEnvironment | None

    def get(self, name: str) -> Thing | None:
        try:
            thing = self.store[name]
        except KeyError:
            thing = None

        if thing is None and self.outer is not None:
            thing = self.outer.get(name)

        return thing

    def set(self, name: str, value: Thing) -> Thing:
        self.store[name] = value

        return value


def new_environment() -> Environment:
    return Environment(store=dict(), outer=None)


def new_enclosed_environment(outer: Environment) -> Environment:
    env = new_environment()
    env.outer = outer

    return env


@dataclass
class Integer(Thing):
    value: int

    def kind(self) -> ThingType:
        return INTEGER_THING

    def inspect(self) -> str:
        return f"{self.value}"

    def hash_key(self) -> HashKey:
        return HashKey(self.kind(), self.value)


@dataclass
class String(Thing):
    value: str

    def kind(self) -> ThingType:
        return STRING_THING

    def inspect(self) -> str:
        return f"{self.value}"

    def hash_key(self) -> HashKey:
        return HashKey(self.kind(), self.value.__hash__())


@dataclass
class Boolean(Thing):
    value: bool

    def kind(self) -> ThingType:
        return BOOLEAN_THING

    def inspect(self) -> str:
        return f"{self.value}"

    def hash_key(self) -> HashKey:
        if self.value:
            value = 1
        else:
            value = 0

        return HashKey(self.kind(), value)


@dataclass
class Null(Thing):
    def kind(self) -> ThingType:
        return NULL_THING

    def inspect(self) -> str:
        return "null"


@dataclass
class ReturnValue(Thing):
    value: Thing

    def kind(self) -> ThingType:
        return RETURN_VALUE_THING

    def inspect(self) -> str:
        return f"{self.value.inspect() if self.value else None}"


@dataclass
class Error(Thing):
    message: str

    def kind(self) -> ThingType:
        return ERROR_THING

    def inspect(self) -> str:
        return f"ERROR: {self.message}"


@dataclass
class Function(Thing):
    parameters: list[Identifier]
    body: BlockStatement
    env: Environment

    def kind(self) -> ThingType:
        return FUNCTION_THING

    def inspect(self) -> str:
        params = ", ".join([str(p) for p in self.parameters])

        return f"fn({params})" + "{\n" + f"{self.body}" + "\n}"


@dataclass
class Macro(Thing):
    parameters: list[Identifier]
    body: BlockStatement
    env: Environment

    def kind(self) -> ThingType:
        return MACRO_THING

    def inspect(self) -> str:
        params = ", ".join([str(p) for p in self.parameters])

        return f"macro({params})" + "{\n" + f"{self.body}" + "\n}"


@dataclass
class Builtin(Thing):
    function: BuiltinFunction

    def kind(self) -> ThingType:
        return BUILTIN_THING

    def inspect(self) -> str:
        return "builtin function"


@dataclass
class Array(Thing):
    elements: list[Thing]

    def kind(self) -> ThingType:
        return ARRAY_THING

    def inspect(self) -> str:
        elems = ", ".join([str(e.value) for e in self.elements])

        return f"[{elems}]"


@dataclass
class HashPair:
    key: Thing
    value: Thing


@dataclass
class Hash(Thing):
    pairs: dict[HashKey, HashPair]

    def kind(self) -> ThingType:
        return HASH_THING

    def inspect(self) -> str:
        pairs = ", ".join([f"{pair.key.inspect()}:{pair.value.inspect()}" for pair in self.pairs.values()])

        return f"{{{pairs}}}"


@dataclass
class Quote(Thing):
    node: Node

    def kind(self) -> ThingType:
        return QUOTE_THING

    def inspect(self) -> str:
        return f"QUOTE({self.node})"
