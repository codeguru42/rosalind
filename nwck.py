from enum import Enum

import typer


class TokenType(Enum):
    SEMICOLON = ";"
    LEFT_PAREN = "("
    RIGHT_PAREN = ")"
    COMMA = ","
    NAME = "NAME"
    NUMBER = "NUMBER"


def main(filename: str):
    with open(filename) as file:
        print(file.readlines())


if __name__ == "__main__":
    typer.run(main)
