from collections import Counter, defaultdict

import typer

from rosalind import parse_fasta


def complement(rna: str) -> str:
    def f():
        for c in rna:
            match c:
                case "A":
                    yield "T"
                case "C":
                    yield "G"
                case "G":
                    yield "C"
                case "T":
                    yield "A"
                case _:
                    raise f"Invalid nucleotide: {c}"

    return "".join(f())


def reverse_complement(rna: str) -> str:
    return complement(rna[::-1])


def hamming(s1: str, s2: str) -> int:
    d = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            d += 1
    return d


def build_distance_mapping(rna1: str, rnas: list[str]) -> dict[int, list[str]]:
    result = defaultdict(list)
    for rna2 in rnas:
        d = hamming(rna1, rna2)
        result[d].append(rna2)
    return result


def main(filename: str):
    with open(filename) as f:
        rnas = list(parse_fasta(f))
        for rna1 in rnas:
            dm = build_distance_mapping(rna1, rnas)
            rev_comps = [reverse_complement(rna2) for rna2 in rnas]
            dmrc = build_distance_mapping(rna1, rev_comps)
            # Found a match - correctly sequenced
            if len(dm[0]) + len(dmrc[0]) >= 2:
                continue
            if len(dm[1]) == 1:
                print(f"{rna1}->{dm[1][0]}")
                continue
            if len(dmrc[1]) >= 1:
                print(f"{rna1}->{dmrc[1][0]}")


if __name__ == "__main__":
    typer.run(main)
