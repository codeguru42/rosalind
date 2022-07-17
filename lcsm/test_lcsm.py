import io

from lcsm import lcsm

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
    dna = list(lcsm.parse_fasta(io.StringIO(fasta)))
    assert dna == expected


def test_parse_fasta2():
    expected = [
        'GATTACA',
        'TAGACCA',
        'ATACA',
    ]
    dna = list(lcsm.parse_fasta(io.StringIO(fasta2)))
    assert dna == expected


def test_ukkonen_step1():
    ukkonen = lcsm.Ukkonen('abc')
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
    }
    assert expected == result


def test_ukkonen_step2():
    ukkonen = lcsm.Ukkonen('abc')
    ukkonen.step()
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
        (1, '#'): {},
    }
    assert expected == result


def test_ukkonen_step3():
    ukkonen = lcsm.Ukkonen('abc')
    ukkonen.step()
    ukkonen.step()
    result = ukkonen.step()
    expected = {
        (0, '#'): {},
        (1, '#'): {},
        (2, '#'): {},
    }
    assert expected == result
