import typer

from rosalind import parse_fasta


def complement(rna: str) -> str:
    def f():
        for c in rna:
            match c:
                case 'A':
                    yield 'T'
                case 'C':
                    yield 'G'
                case 'G':
                    yield 'C'
                case 'T':
                    yield 'A'
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


def main(filename: str):
    with open(filename) as f:
        rnas = list(parse_fasta(f))
        for rna1 in rnas:
            for rna2 in rnas:
                d = hamming(rna1, rna2)
                if d == 0:
                    continue
                if d == 1:
                    print(f'{rna1}->{rna2}')
                    continue
                rc = reverse_complement(rna2)
                d_comp = hamming(rna1, rc)
                if d_comp == 0:
                    continue
                if d_comp == 1:
                    print(f'{rna1}->{rc}')


if __name__ == '__main__':
    typer.run(main)