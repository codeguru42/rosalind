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


class Ukkonen:
    def __init__(self, string):
        self.string = string
        self.position = 0
        self.tree = {}

    def step(self):
        self.tree[(self.position, '#')] = {}
        self.position += 1
        return self.tree


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


def test_ukkonen_step1():
    ukkonen = Ukkonen('abc')
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
    }
    assert expected == result


def test_ukkonen_step2():
    ukkonen = Ukkonen('abc')
    ukkonen.step()
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
        (1, '#'): {},
    }
    assert expected == result


def test_ukkonen_step3():
    ukkonen = Ukkonen('abc')
    ukkonen.step()
    ukkonen.step()
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
        (1, '#'): {},
        (2, '#'): {},
    }
    assert expected == result


def main():
    with open('lcsm.txt') as file:
        print(list(parse_fasta(file)))


if __name__ == '__main__':
    main()
