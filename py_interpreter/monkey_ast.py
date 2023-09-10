from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Callable, Protocol

from py_interpreter.monkey_token import Token


class Node(Protocol):
    def token_literal(self) -> str:
        ...

    def __str__(self) -> str:
        ...


@dataclass
class Statement(ABC):
    @abstractmethod
    def token_literal(self) -> str:
        ...


@dataclass
class Expression(ABC):
    @abstractmethod
    def token_literal(self) -> str:
        ...

    def __eq__(self, other) -> bool:
        return hasattr(other, "__dict__") and self.__dict__ == other.__dict__


@dataclass
class Identifier(Expression):
    token: Token = field(default_factory=Token)
    value: str = ""

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return self.value

    def __hash__(self) -> int:
        return self.value.__hash__()


@dataclass
class PrefixExpression(Expression):
    right: Expression
    token: Token = field(default_factory=Token)
    operator: str = ""

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"({self.operator}{self.right})"

    def __hash__(self) -> int:
        return self.operator.__hash__() + self.right.__hash__()


@dataclass
class InfixExpression(Expression):
    left: Expression
    right: Expression
    token: Token = field(default_factory=Token)
    operator: str = ""

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"

    def __hash__(self) -> int:
        return self.left.__hash__() + self.operator.__hash__() + self.right.__hash__()


@dataclass
class IntegerLiteral(Expression):
    token: Token = field(default_factory=Token)
    value: int = 0

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return self.token.literal

    def __hash__(self) -> int:
        return self.value.__hash__()


@dataclass
class StringLiteral(Expression):
    token: Token = field(default_factory=Token)
    value: str = ""

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return self.token.literal

    def __hash__(self) -> int:
        return self.value.__hash__()


@dataclass
class LetStatement(Statement):
    value: Expression
    name: Identifier = field(default_factory=Identifier)
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"{self.token_literal()} {self.name} = {self.value if self.value else ''};"


@dataclass
class ReturnStatement(Statement):
    return_value: Expression
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"{self.token_literal()} {self.return_value if self.return_value else ''};"


@dataclass
class ExpressionStatement(Statement):
    expression: Expression
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"{self.expression if self.expression else ''}"


@dataclass
class BlockStatement(Statement):
    statements: list[Statement]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        stmts = "\n".join([str(s) for s in self.statements])
        return f"{stmts}"


@dataclass
class BooleanExpression(Expression):
    value: bool
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return str(self.value).lower()

    def __str__(self) -> str:
        return str(self.value).lower()

    def __hash__(self) -> int:
        return self.value.__hash__()


@dataclass
class IfExpression(Expression):
    condition: Expression
    consequence: BlockStatement
    alternative: BlockStatement
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"if{self.condition} {self.consequence} {'else ' + str(self.alternative) if self.alternative else ''}"


@dataclass
class FunctionLiteral(Expression):
    body: BlockStatement
    parameters: list[Identifier]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        if self.parameters:
            params = ", ".join([str(p) for p in self.parameters])
        else:
            params = ""
        return f"{self.token_literal()}({params}){self.body}"


@dataclass
class MacroLiteral(Expression):
    body: BlockStatement
    parameters: list[Identifier]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        if self.parameters:
            params = ", ".join([str(p) for p in self.parameters])
        else:
            params = ""
        return f"{self.token_literal()}({params}){self.body}"


@dataclass
class CallExpression(Expression):
    function: Expression
    arguments: list[Expression]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        if self.arguments:
            args = ", ".join([str(a) for a in self.arguments])
        else:
            args = ""
        return f"{self.function}({args})"


@dataclass
class ArrayLiteral(Expression):
    elements: list[Expression]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        if self.elements:
            elems = ", ".join([str(e) for e in self.elements])
        else:
            elems = ""
        return f"[{elems}]"


@dataclass
class IndexExpression(Expression):
    left: Expression
    index: Expression
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        return f"({self.left}[{self.index}])"


@dataclass
class HashLiteral(Expression):
    pairs: dict[Expression, Expression]
    token: Token = field(default_factory=Token)

    def token_literal(self) -> str:
        return self.token.literal

    def __str__(self) -> str:
        pairs = ", ".join([f"{k}:{v}" for k, v in self.pairs.items()])
        return f"{{{pairs}}}"


@dataclass
class Program(Node):
    statements: list[Statement]

    def token_literal(self) -> str:
        if len(self.statements) > 0:
            return self.statements[0].token_literal()
        else:
            return ""

    def __str__(self) -> str:
        string = ""
        for s in self.statements:
            string += str(s)

        return string


ModifierFunc = Callable[[Node], Node]


def modify(node: Node, modifier: ModifierFunc) -> Node:
    if isinstance(node, Program):
        for i, statement in enumerate(node.statements):
            node.statements[i] = modify(statement, modifier)

    elif isinstance(node, ExpressionStatement):
        expression_statement = node
        node.expression = modify(expression_statement.expression, modifier)

    elif isinstance(node, InfixExpression):
        node.left = modify(node.left, modifier)
        node.right = modify(node.right, modifier)

    elif isinstance(node, PrefixExpression):
        node.right = modify(node.right, modifier)

    elif isinstance(node, IndexExpression):
        node.left = modify(node.left, modifier)
        node.index = modify(node.index, modifier)

    elif isinstance(node, IfExpression):
        node.condition = modify(node.condition, modifier)
        node.consequence = modify(node.consequence, modifier)

        if node.alternative is not None:
            node.alternative = modify(node.alternative, modifier)

    elif isinstance(node, BlockStatement):
        for i in range(len(node.statements)):
            node.statements[i] = modify(node.statements[i], modifier)

    elif isinstance(node, ReturnStatement):
        node.return_value = modify(node.return_value, modifier)

    elif isinstance(node, LetStatement):
        node.value = modify(node.value, modifier)

    elif isinstance(node, FunctionLiteral):
        for i in range(len(node.parameters)):
            node.parameters[i] = modify(node.parameters[i], modifier)

        node.body = modify(node.body, modifier)

    elif isinstance(node, ArrayLiteral):
        for i in range(len(node.elements)):
            node.elements[i] = modify(node.elements[i], modifier)

    elif isinstance(node, HashLiteral):
        new_pairs = {}
        for key, value in node.pairs.items():
            new_key = modify(key, modifier)
            new_value = modify(value, modifier)
            new_pairs[new_key] = new_value

        node.pairs = new_pairs

    return modifier(node)
