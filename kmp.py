from typing import Iterable

import typer

from rosalind import parse_fasta


def suffixes(s: str) -> Iterable[str]:
    for i in range(len(s)):
        yield s[i:]


def main(filename: str):
    with open(filename) as f:
        fasta = parse_fasta(f)
        for rna in fasta:
            print(list(suffixes(rna)))


if __name__ == "__main__":
    typer.run(main)
