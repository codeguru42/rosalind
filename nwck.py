from abc import ABC, abstractmethod
from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum
from typing import Optional

import pytest
import typer


class TokenType(Enum):
    SEMICOLON = ";"
    LEFT_PAREN = "("
    RIGHT_PAREN = ")"
    COMMA = ","
    COLON = ":"
    NAME = "NAME"
    NUMBER = "NUMBER"
    EOF = "EOF"


class Tokenizer:
    def __init__(self, s: str):
        self.s = s
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.i >= len(self.s):
            raise StopIteration
        return self.next_token()

    def next_token(self) -> tuple[TokenType, str]:
        if self.i >= len(self.s):
            return TokenType.EOF, ""
        match self.s[self.i]:
            case ";":
                self.i += 1
                return TokenType.SEMICOLON, ";"
            case "(":
                self.i += 1
                return TokenType.LEFT_PAREN, "("
            case ")":
                self.i += 1
                return TokenType.RIGHT_PAREN, ")"
            case ",":
                self.i += 1
                return TokenType.COMMA, ","
            case ":":
                self.i += 1
                return TokenType.COLON, ":"
            case c:
                if c.isalpha():
                    start = self.i
                    while self.i < len(self.s) and (
                        self.s[self.i].isalpha() or self.s[self.i] == "_"
                    ):
                        self.i += 1
                    return TokenType.NAME, self.s[start : self.i]
                elif c.isdigit():
                    start = self.i
                    while self.i < len(self.s) and self.s[self.i].isdigit():
                        self.i += 1
                    return TokenType.NUMBER, self.s[start : self.i]
                else:
                    raise ValueError(f"Unexpected character: {c}")


@dataclass
class Leaf:
    name: str

    def __str__(self):
        return self.name

    def accept(self, visitor: "Visitor"):
        return visitor.visit_leaf(self)


@dataclass
class Internal:
    branch_set: "BranchSet"
    name: Optional[str]
    
    def __init__(self, branch_set: "BranchSet", name: Optional[str] = None):
        self.branch_set = branch_set
        if name:
            self.name = name
        else:
            self.name = str(self.branch_set)

    def __str__(self):
        return f"({self.branch_set!s}){self.name or ''}"

    def accept(self, visitor: "Visitor"):
        return visitor.visit_internal(self)


@dataclass
class Branch:
    subtree: Internal | Leaf
    length: int = 1

    def __str__(self):
        return f"{self.subtree!s}:{self.length}"

    def accept(self, visitor: "Visitor"):
        return visitor.visit_branch(self)


@dataclass
class BranchSet:
    branches: list[Branch]

    def __str__(self):
        return ",".join(map(str, self.branches))

    def accept(self, visitor: "Visitor"):
        return visitor.visit_branch_set(self)


@dataclass
class TreeAST:
    subtree: Internal | Leaf

    def __str__(self):
        return f"{self.subtree!s};"

    def accept(self, visitor: "Visitor"):
        return visitor.visit_tree(self)


class Parser:
    def __init__(self, s: str):
        self.tokens = Tokenizer(s)
        self.nextToken = next(self.tokens)
        self.at_eof = False

    def match(self, token_type: TokenType) -> bool:
        if self.check(token_type):
            self.advance()
            return True
        return False

    def check(self, token_type: TokenType) -> bool:
        return not self.at_eof and token_type == self.nextToken[0]

    def advance(self):
        try:
            self.nextToken = next(self.tokens)
        except StopIteration:
            self.at_eof = True

    def parse_tree(self) -> TreeAST:
        subtree = self.parse_subtree()
        if not self.match(TokenType.SEMICOLON):
            raise ValueError("Expected semicolon")
        if not self.at_eof:
            raise ValueError(f"Unexpected characters at end of input: {self.nextToken}")
        return TreeAST(subtree)

    def parse_subtree(self) -> Internal | Leaf:
        if self.match(TokenType.LEFT_PAREN):
            return self.parse_internal()
        name_token = self.nextToken
        if self.match(TokenType.NAME):
            return Leaf(name_token[1])
        raise ValueError(f"Unexpected token: {self.nextToken}")

    def parse_internal(self) -> Internal:
        branch_set = self.parse_branch_set()
        if not self.match(TokenType.RIGHT_PAREN):
            raise ValueError(f"Expected right parenthesis. Got {self.nextToken}")
        name = self.nextToken
        if self.match(TokenType.NAME):
            return Internal(branch_set, name[1])
        return Internal(branch_set)

    def parse_branch_set(self) -> BranchSet:
        branches = list(self.parse_branches())
        return BranchSet(branches)

    def parse_branches(self) -> Iterator[Branch]:
        while not self.check(TokenType.RIGHT_PAREN):
            yield self.parse_branch()
            if not self.match(TokenType.COMMA):
                break

    def parse_branch(self) -> Branch:
        subtree = self.parse_subtree()
        if self.match(TokenType.COLON):
            length = self.parse_length()
            return Branch(subtree, length)
        return Branch(subtree)

    def parse_length(self) -> int:
        length_token = self.nextToken
        if not self.match(TokenType.NUMBER):
            raise ValueError("Expected number")
        return int(length_token[1])


def test_tokenizer_eof():
    s = ""
    tokenizer = Tokenizer(s)
    assert tokenizer.next_token() == (TokenType.EOF, "")


@pytest.mark.parametrize(
    "s, expected",
    [
        (";", (TokenType.SEMICOLON, ";")),
        ("(", (TokenType.LEFT_PAREN, "(")),
        (")", (TokenType.RIGHT_PAREN, ")")),
        (",", (TokenType.COMMA, ",")),
        (":", (TokenType.COLON, ":")),
    ],
)
def test_tokenizer_single_character(s, expected):
    tokenizer = Tokenizer(s)
    assert tokenizer.next_token() == expected


def test_tokenizer_name():
    s = "foo"
    tokenizer = Tokenizer(s)
    assert tokenizer.next_token() == (TokenType.NAME, "foo")


def test_tokenizer_number():
    s = "123"
    tokenizer = Tokenizer(s)
    assert tokenizer.next_token() == (TokenType.NUMBER, "123")


def test_tokenizer_iter():
    s = "(a,b)c;"
    expected = [
        (TokenType.LEFT_PAREN, "("),
        (TokenType.NAME, "a"),
        (TokenType.COMMA, ","),
        (TokenType.NAME, "b"),
        (TokenType.RIGHT_PAREN, ")"),
        (TokenType.NAME, "c"),
        (TokenType.SEMICOLON, ";"),
    ]
    tokenizer = Tokenizer(s)
    assert list(tokenizer) == expected


@pytest.mark.parametrize(
    "s,expected",
    (
        (
            "(a:1,b:2)c;",
            TreeAST(
                Internal(
                    BranchSet(
                        [
                            Branch(Leaf("a"), 1),
                            Branch(Leaf("b"), 2),
                        ]
                    ),
                    "c",
                )
            ),
        ),
        (
            "(a,b);",
            TreeAST(
                Internal(
                    BranchSet(
                        [
                            Branch(Leaf("a"), 1),
                            Branch(Leaf("b"), 1),
                        ]
                    )
                ),
            ),
        ),
    ),
)
def test_parser(s, expected):
    parser = Parser(s)
    actual = parser.parse_tree()
    assert expected == actual


def test_distance_visitor1():
    tree = Parser("(a,b)c;").parse_tree()
    actual = DepthVisitor().get_depth(tree, "a")
    expected = 1
    assert expected == actual


def test_distance_visitor2():
    tree = Parser("(a,(b,c))d;").parse_tree()
    actual = DepthVisitor().get_depth(tree, "b")
    expected = 2
    assert expected == actual


def test_build_tree():
    s = "(a:1,b:2)c;"
    tree = Parser(s).parse_tree()
    actual = TreeVisitor().visit_tree(tree)
    expected = Tree(
        root=Tree.Node(
            name="c",
            children=[
                Tree.Node(name="a", children=[]),
                Tree.Node(name="b", children=[]),
            ],
        )
    )
    assert expected == actual


def test_get_path():
    s = "(a:1,b:2)c;"
    tree_ast = Parser(s).parse_tree()
    tree = TreeVisitor().visit_tree(tree_ast)
    actual = tree.get_path("a")
    expected = [
        Tree.Node(
            name="c",
            children=[
                Tree.Node(name="a", children=[]),
                Tree.Node(name="b", children=[]),
            ],
        ),
        Tree.Node(name="a", children=[]),
    ]
    assert actual == expected


def test_dist1():
    tree_ast = Parser("(cat)dog;").parse_tree()
    tree = TreeVisitor().visit_tree(tree_ast)
    actual = dist(tree, "dog", "cat")
    expected = 1
    assert actual == expected


def test_dist2():
    tree_ast = Parser("(dog,cat);").parse_tree()
    tree = TreeVisitor().visit_tree(tree_ast)
    actual = dist(tree, "dog", "cat")
    expected = 2
    assert actual == expected


def test_str():
    s = "(a:1,b:2)c;"
    parser = Parser(s)
    actual = str(parser.parse_tree())
    expected = s
    assert expected == actual


class Visitor(ABC):
    @abstractmethod
    def visit_tree(self, tree: TreeAST):
        pass

    @abstractmethod
    def visit_internal(self, internal: Internal):
        pass

    @abstractmethod
    def visit_leaf(self, leaf: Leaf):
        pass

    @abstractmethod
    def visit_branch_set(self, branch_set: BranchSet):
        pass

    @abstractmethod
    def visit_branch(self, branch: Branch):
        pass


class DepthVisitor(Visitor):
    def get_depth(self, tree: TreeAST, node: str) -> int:
        self.depth = 0
        self.node = node
        tree.accept(self)
        return self.depth

    def visit_tree(self, tree: TreeAST):
        tree.subtree.accept(self)

    def visit_internal(self, internal: Internal):
        self.depth += 1
        internal.branch_set.accept(self)

    def visit_leaf(self, leaf: Leaf):
        if leaf.name == self.node:
            return self.depth
        return 0

    def visit_branch_set(self, branch_set: BranchSet):
        for branch in branch_set.branches:
            branch.accept(self)

    def visit_branch(self, branch: Branch):
        return branch.subtree.accept(self)


@dataclass
class Tree:
    @dataclass
    class Node:
        name: str
        children: list["Tree.Node"]

        def get_path(self, node_name: str) -> tuple[bool, list["Tree.Node"]]:
            if self.name == node_name:
                return True, [self]
            for child in self.children:
                found, path = child.get_path(node_name)
                if found:
                    return True, [self] + path
            return False, []

    root: Node

    def get_path(self, node_name: str) -> list[Node]:
        _, path = self.root.get_path(node_name)
        return path


class TreeVisitor(Visitor):
    def visit_tree(self, tree: TreeAST) -> Tree:
        children = tree.subtree.accept(self)
        return Tree(root=Tree.Node(tree.subtree.name, children))

    def visit_internal(self, internal: Internal) -> Tree.Node:
        return Tree.Node(name=internal.name, children=internal.branch_set.accept(self))

    def visit_leaf(self, leaf: Leaf) -> Tree.Node:
        return Tree.Node(leaf.name, [])

    def visit_branch_set(self, branch_set: BranchSet) -> list[Tree.Node]:
        return [branch.accept(self) for branch in branch_set.branches]

    def visit_branch(self, branch: Branch) -> list[Tree.Node] | Tree.Node:
        return branch.subtree.accept(self)


def get_common_ancestor(path_a, path_b):
    for i, p in enumerate(zip(path_a, path_b)):
        a, b = p
        if a != b:
            return path_a[i - 1]
    return path_a[-1]


def dist(tree: Tree, a: str, b: str) -> int:
    path_a = tree.get_path(a)
    path_b = tree.get_path(b)
    ca = get_common_ancestor(path_a, path_b)
    dist_ca = path_a.index(ca) + 1
    return len(path_a) + len(path_b) - 2 * dist_ca


def main(filename: str):
    with open(filename) as file:
        try:
            while True:
                tree_ast = Parser(file.readline().strip()).parse_tree()
                nodes = file.readline().strip().split()
                file.readline()
                tree = TreeVisitor().visit_tree(tree_ast)
                print(dist(tree, *nodes), end=" ")
        except StopIteration:
            pass


if __name__ == "__main__":
    typer.run(main)
