import io


def parse_fasta(fasta_file):
    nucleotide = ''
    for line in fasta_file:
        if line[0] == '>':
            if nucleotide != '':
                yield nucleotide
                nucleotide = ''
        else:
            nucleotide += line.strip()
    yield nucleotide


fasta = '''>Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA
'''

fasta2 = '''>Rosalind_1
GAT
TACA
>Rosalind_2
TAG
ACCA
>Rosalind_3
ATA
CA
'''


def test_parse_fasta():
    expected = [
        'GATTACA',
        'TAGACCA',
        'ATACA',
    ]
    dna = list(parse_fasta(io.StringIO(fasta)))
    assert dna == expected


def test_parse_fasta2():
    expected = [
        'GATTACA',
        'TAGACCA',
        'ATACA',
    ]
    dna = list(parse_fasta(io.StringIO(fasta2)))
    assert dna == expected


def main():
    with open("long.txt") as file:
        fasta = parse_fasta(file)
        for rna in fasta:
            print(rna)


if __name__ == '__main__':
    main()
