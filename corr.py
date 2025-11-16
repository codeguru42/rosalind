from collections import defaultdict
from typing import Iterable

import typer

from rosalind import parse_fasta, reverse_complement


def hamming(s1: str, s2: str) -> int:
    d = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            d += 1
    return d


def build_distance_mapping(rna1: str, rnas: Iterable[str]) -> dict[int, list[str]]:
    result = defaultdict(list)
    for rna2 in rnas:
        d = hamming(rna1, rna2)
        result[d].append(rna2)
    return result


def build_errors(rnas: list[str]) -> Iterable[str]:
    for rna in rnas:
        dm = build_distance_mapping(rna, rnas)
        rev_comps = [reverse_complement(rna2) for rna2 in rnas]
        dmrc = build_distance_mapping(rna, rev_comps)
        if len(dm[0]) + len(dmrc[0]) < 2:
            yield rna


def main(filename: str):
    with open(filename) as f:
        rnas = list(parse_fasta(f))
        errors = list(build_errors(rnas))
        correct = set(rnas) - set(errors)
        for error in errors:
            dm = build_distance_mapping(error, correct)
            rev_comps = [reverse_complement(rna2) for rna2 in correct]
            dmrc = build_distance_mapping(error, rev_comps)
            # Found a match - correctly sequenced
            if len(dm[1]) >= 1:
                print(f"{error}->{dm[1][0]}")
                continue
            if len(dmrc[1]) >= 1:
                print(f"{error}->{dmrc[1][0]}")


if __name__ == "__main__":
    typer.run(main)
