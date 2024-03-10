from enum import Enum

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


class Tokenizer:
    def __init__(self, s: str):
        self.s = s
        self.i = 0

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


def main(filename: str):
    with open(filename) as file:
        print(file.readlines())


if __name__ == "__main__":
    typer.run(main)
