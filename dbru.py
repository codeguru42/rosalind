from pathlib import Path

import typer

from rosalind import reverse_complement


def main(filename: Path):
    with open(filename) as f:
        dnas = [line.strip() for line in f.readlines()]
        rcs = [reverse_complement(dna) for dna in dnas]
        s = set(dnas) | set(rcs)
        for kmer in sorted(s):
            print(f"({kmer[:-1]}, {kmer[1:]})")


if __name__ == "__main__":
    typer.run(main)
