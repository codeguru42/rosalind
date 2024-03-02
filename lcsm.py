from collections.abc import Generator

from rosalind import parse_fasta, sliding_window


def test_longest_common_substring1():
    a = "GATTACA"
    b = "TAGACCA"
    result = longest_common_substring(a, b)
    expected = {"TA", "GA", "CA", "AC"}
    assert result == expected


def test_longest_common_substring2():
    a = "GATTACA"
    b = "ATACA"
    result = longest_common_substring(a, b)
    expected = {"TACA"}
    assert result == expected


def test_suffix_array():
    s = "ababbab"
    expected = [5, 0, 2, 6, 4, 1, 3]
    actual = list(suffix_array(s))
    assert expected == actual


def test_longest_common_prefix_array():
    s = "ababbab"
    expected = [0, 2, 2, 0, 1, 3, 1]
    actual = list(longest_common_prefix_array(s))
    assert expected == actual


def longest_common_substring(s: str, t: str) -> set[str]:
    r = len(s)
    n = len(t)
    L = [[0] * n for _ in range(r)]
    z = 0
    ret = set()

    for i in range(r):
        for j in range(n):
            if s[i] == t[j]:
                if i == 0 or j == 0:
                    L[i][j] = 1
                else:
                    L[i][j] = L[i - 1][j - 1] + 1
                if L[i][j] > z:
                    z = L[i][j]
                    ret = {s[i - z + 1 : i + 1]}
                elif L[i][j] == z:
                    ret.add(s[i - z + 1 : i + 1])
            else:
                L[i][j] = 0
    return ret


def suffix_array(s: str) -> Generator[int]:
    for i, _ in sorted(suffixes(s), key=lambda x: x[1]):
        yield i


def suffixes(s: str) -> Generator[tuple[int, str]]:
    for i in range(len(s)):
        yield i, s[i:]


def longest_common_prefix_array(s: str) -> Generator[int]:
    yield 0
    for i, j in sliding_window(list(suffix_array(s)), 2):
        suff1 = s[i:]
        suff2 = s[j:]
        prefix = common_prefix(suff1, suff2)
        yield len(prefix)


def common_prefix(s1: str, s2: str) -> str:
    def prefix_generator(s1: str, s2: str) -> Generator[str]:
        for c1, c2 in zip(s1, s2):
            if c1 == c2:
                yield c1
            else:
                break

    return "".join(prefix_generator(s1, s2))


def main():
    with open("lcsm.txt") as file:
        print(list(parse_fasta(file)))


if __name__ == "__main__":
    main()
