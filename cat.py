import pytest


@pytest.mark.parametrize("n,cat", [(0, 1), (2, 2), (4, 5), (6, 14)])
def test_catalan(n, cat):
    assert catalan(n) == cat


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


def catalan(n):
    cat = [0] * (n//2+2)  # so I don't have to deal with IndexError when n == 0

    cat[0] = 1
    cat[1] = 1
    for i in range(2, n//2+2):
        for k in range(1, i+1):
            cat[i] += cat[k-1] * cat[i-k]

    return cat[-1]


def main():
    pass


if __name__ == '__main__':
    main()
