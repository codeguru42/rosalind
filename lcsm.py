from rosalind import parse_fasta


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


def main():
    with open("lcsm.txt") as file:
        print(list(parse_fasta(file)))


if __name__ == "__main__":
    main()
