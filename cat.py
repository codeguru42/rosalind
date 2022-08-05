def test_catalan1():
    assert catalan('AUAU') == 2


def test_catalan2():
    assert catalan('UAGCGUGAUCAC') == 2


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


def catalan(rna):
    n = len(rna)
    # cat[i][j] is the number of non-crossing matchings with an edge from rna[i] to rna[j]
    cat = [[0] * n for _ in range(n)]

    cat[0][0] = 1
    for i in range(1, n):
        for j in range(1, n):
            cat[i] = sum(cat[k-1] * cat[n-k] if rna[i] == complement(rna[k]) else 0 for k in range(1, n))

    return cat[-1]


def main():
    pass


if __name__ == '__main__':
    main()
