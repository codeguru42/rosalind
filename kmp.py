from typing import Iterable

import pytest
import typer

from rosalind import parse_fasta


@pytest.mark.parametrize(
    "w,expected",
    (
        ("CAGCATGGTATCACAGCAGAG", [0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 3, 4, 5, 3, 0, 0]),
    ),
)
def test_failure(w, expected):
    assert failure(w) == expected


def failure(pat: str) -> Iterable[int]:
    lps = [0] * len(pat)
    n = 0
    i = 1
    while i < len(pat):
        if pat[i] == pat[n]:
            n += 1
            lps[i] = n
            i += 1
        elif pat[i] != pat[n] and n == 0: 
            lps[i] = 0
            i += 1
        elif pat[i] != pat[n] and n > 0:
            n = lps[n-1]
    return lps


def main(filename: str):
    with open(filename) as f:
        fasta = parse_fasta(f)
        for dna in fasta:
            print(" ".join(str(i) for i in failure(dna)))


if __name__ == "__main__":
    typer.run(main)
