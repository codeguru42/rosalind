import typer

from rosalind import parse_fasta


def hamming(s1, s2):
    d = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            d += 1
    return d


def main(filename: str):
    with open(filename) as f:
        rnas = parse_fasta(f)
        print(list(rnas))


if __name__ == '__main__':
    typer.run(main)