import pytest

catalan_numbers = [
    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900,
    2674440, 9694845, 35357670, 129644790, 477638700, 1767263190, 6564120420,
    24466267020, 91482563640, 343059613650, 1289904147324, 4861946401452,
    18367353072152, 69533550916004, 263747951750360, 1002242216651368,
    3814986502092304
]


@pytest.mark.parametrize("n,cat", enumerate(catalan_numbers))
def test_catalan(n, cat):
    assert catalan(n) == cat


@pytest.mark.parametrize("rna,cat", [('AUAU', 2), ('UAGCGUGAUCAC', 2)])
def test_catalan_rna(rna, cat):
    assert catalan_rna(rna) == cat


def complement(base):
    if base == 'A':
        return 'U'
    if base == 'C':
        return 'G'
    if base == 'G':
        return 'C'
    if base == 'U':
        return 'A'
    raise "Invalid base"


def catalan_rna(rna):
    n = len(rna)
    cat = [0] * (n//2+2)  # so I don't have to deal with IndexError when n == 0

    cat[0] = 1
    cat[1] = 1
    for i in range(2, n//2+2):
        for k in range(1, i+1):
            if rna[k-1] == complement(rna[i-k]):
                cat[i] += cat[k-1] * cat[i-k]

    return cat[-1]


def catalan(n):
    cat = [0] * (n+2)  # so I don't have to deal with IndexError when n == 0

    cat[0] = 1
    cat[1] = 1
    for i in range(2, n+1):
        for k in range(1, i+1):
            cat[i] += cat[k-1] * cat[i-k]

    return cat[-2]


def main():
    pass


if __name__ == '__main__':
    main()
