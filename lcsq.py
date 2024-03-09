import typer

from rosalind import parse_fasta


def test_longest_common_subsequence1():
    a = "AACCTTGG"
    b = "ACACTGTGA"
    expected = "AACTTG"
    result = longest_common_subsequence(a, b)
    assert result == expected


def test_longest_common_subsequence2():
    a = "AGCAT"
    b = "GAC"
    expected = "AC"
    result = longest_common_subsequence(a, b)
    assert result == expected


def longest_common_subsequence(str1: str, str2: str) -> str:
    m = len(str1)
    n = len(str2)
    subsequences = [["" for _ in range(n + 1)] for _ in range(m + 1)]
    for i in range(m):
        for j in range(n):
            if str1[i] == str2[j]:
                subsequences[i + 1][j + 1] = subsequences[i][j] + str1[i]
            else:
                sub1 = subsequences[i][j + 1]
                sub2 = subsequences[i + 1][j]
                if len(sub1) >= len(sub2):
                    subsequences[i + 1][j + 1] = sub1
                else:
                    subsequences[i + 1][j + 1] = sub2
    return subsequences[-1][-1]  # maybe?


def main(filename: str):
    with open(filename, "r") as file:
        dnas = parse_fasta(file)
        print(longest_common_subsequence(*dnas))


if __name__ == "__main__":
    typer.run(main)
