import io


def parse_fasta(fasta_file):
    for line in fasta_file:
        if line[0] != '>':
            yield line.strip()


fasta = '''>Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA
'''


def test_parse_fasta():
    expected = [
        'GATTACA',
        'TAGACCA',
        'ATACA',
    ]
    dna = list(parse_fasta(io.StringIO(fasta)))
    assert dna == expected


def main():
    pass


if __name__ == '__main__':
    main()
