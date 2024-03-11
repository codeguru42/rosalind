from collections.abc import Iterator
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


def test_parser1():
    s = "(a:1,b:2)c;"
    parser = Parser(s)
    actual = parser.parse_tree()
    expected = Tree(
        Internal(
            BranchSet(
                [
                    Branch(Leaf("a"), 1),
                    Branch(Leaf("b"), 2),
                ]
            ),
            "c",
        )
    )
    assert expected == actual


def test_parser2():
    s = "(a,b);"
    parser = Parser(s)
    actual = parser.parse_tree()
    expected = Tree(
        Internal(
            BranchSet(
                [
                    Branch(Leaf("a"), 1),
                    Branch(Leaf("b"), 1),
                ]
            )
        ),
    )
    assert expected == actual


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
                    while self.i < len(self.s) and self.s[self.i].isalpha():
                        self.i += 1
                    return TokenType.NAME, self.s[start : self.i]
                elif c.isdigit():
                    start = self.i
                    while self.i < len(self.s) and self.s[self.i].isdigit():
                        self.i += 1
                    return TokenType.NUMBER, self.s[start : self.i]
                else:
                    raise ValueError(f"Unexpected character: {c}")


class Leaf:
    def __init__(self, name: str):
        self.name = name

    def __eq__(self, other):
        return other and self.name == other.name

    def __repr__(self):
        return f"Leaf({self.name=!r})"


class Internal:
    def __init__(self, branch_set: "BranchSet", name: Optional[str] = None):
        self.branch_set = branch_set
        self.name = name

    def __eq__(self, other):
        return other and self.branch_set == other.branch_set and self.name == other.name

    def __repr__(self):
        return f"Internal({self.branch_set=!r}, {self.name=!r})"


class Branch:
    def __init__(self, subtree: Internal | Leaf, length: int = 1):
        self.subtree = subtree
        self.length = length

    def __eq__(self, other):
        return other and self.subtree == other.subtree and self.length == other.length

    def __repr__(self):
        return f"Branch({self.subtree=!r}, {self.length=!r})"


class BranchSet:
    def __init__(self, branches: list[Branch]):
        self.branches = branches

    def __eq__(self, other):
        return other and self.branches == other.branches

    def __repr__(self):
        return f"BranchSet({self.branches=!r})"


class Tree:
    def __init__(self, subtree: Internal | Leaf):
        self.subtree = subtree

    def __eq__(self, other):
        return other and self.subtree == other.subtree

    def __repr__(self):
        return f"Tree({self.subtree=!r})"


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

    def parse_tree(self) -> Tree:
        subtree = self.parse_subtree()
        if not self.match(TokenType.SEMICOLON):
            raise ValueError("Expected semicolon")
        if not self.at_eof:
            raise ValueError(f"Unexpected characters at end of input: {self.nextToken}")
        return Tree(subtree)

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
            raise ValueError("Expected right parenthesis")
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


def main(filename: str):
    with open(filename) as file:
        try:
            while True:
                tree = Parser(file.readline().strip()).parse_tree()
                nodes = file.readline().strip().split()
                file.readline()
                print(tree)
                print(nodes)
        except StopIteration:
            pass


if __name__ == "__main__":
    typer.run(main)
