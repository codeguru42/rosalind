from typing import Iterable

import typer

from rosalind import parse_fasta


def suffixes(s: str) -> Iterable[str]:
    for i in range(len(s)):
        yield s[i:]


def is_prefix(s: str, t: str) -> bool:
    return s == t[: len(s)]


def failure(s):
    for k in range(1, len(s) + 1):
        yield max((k - j for j in range(2, k) if is_prefix(s[j: k], s)), default=0)


def main(filename: str):
    with open(filename) as f:
        fasta = parse_fasta(f)
        for dna in fasta:
            print(list(failure(dna)))

if __name__ == "__main__":
    typer.run(main)
