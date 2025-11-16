import collections
import io
import itertools


def parse_fasta(fasta_file):
    nucleotide = ""
    for line in fasta_file:
        if line[0] == ">":
            if nucleotide != "":
                yield nucleotide
                nucleotide = ""
        else:
            nucleotide += line.strip()
    yield nucleotide


fasta = """>Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA
"""
fasta2 = """>Rosalind_1
GAT
TACA
>Rosalind_2
TAG
ACCA
>Rosalind_3
ATA
CA
"""


def test_parse_fasta():
    expected = [
        "GATTACA",
        "TAGACCA",
        "ATACA",
    ]
    dna = list(parse_fasta(io.StringIO(fasta)))
    assert dna == expected


def test_parse_fasta2():
    expected = [
        "GATTACA",
        "TAGACCA",
        "ATACA",
    ]
    dna = list(parse_fasta(io.StringIO(fasta2)))
    assert dna == expected


def sliding_window(iterable, n):
    """Collect data into overlapping fixed-length chunks or blocks."""
    """sliding_window('ABCDEFG', 4) --> ABCD BCDE CDEF DEFG"""
    it = iter(iterable)
    window = collections.deque(itertools.islice(it, n - 1), maxlen=n)
    for x in it:
        window.append(x)
        yield tuple(window)


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
                    raise Exception(f"Invalid nucleotide: {c}")

    return "".join(f())


def reverse_complement(rna: str) -> str:
    return complement(rna[::-1])
