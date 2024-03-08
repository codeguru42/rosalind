def test_longest_increasing_subsequence1():
    sequence = [5, 1, 4, 2, 3]
    expected = [1, 2, 3]
    actual = longest_increasing_subsequence(sequence)
    assert actual == expected


def test_longest_increasing_subsequence2():
    sequence = [8, 2, 1, 6, 5, 7, 4, 3, 9]
    expected = [2, 6, 7, 9]
    actual = longest_increasing_subsequence(sequence)
    assert actual == expected


def test_longest_decreasing_subsequence1():
    sequence = [5, 1, 4, 2, 3]
    expected = [5, 4, 3]
    actual = longest_decreasing_subsequence(sequence)
    assert actual == expected


def test_longest_decreasing_subsequence2():
    sequence = [8, 2, 1, 6, 5, 7, 4, 3, 9]
    expected = [8, 6, 5, 4, 3]
    actual = longest_decreasing_subsequence(sequence)
    assert actual == expected


def parse(file):
    n = int(file.readline().strip())
    return [int(n) for n in file.readline().strip().split()]


def longest_increasing_subsequence(sequence: list[int]) -> list[int]:
    subsequences = [[] for _ in range(len(sequence))]
    for i in range(len(sequence)):
        subs = [
            sub for j, sub in enumerate(subsequences[:i]) if sequence[j] < sequence[i]
        ]
        subsequences[i] = max(subs, key=len, default=[]) + [sequence[i]]
    return subsequences[-1]


def longest_decreasing_subsequence(sequence):
    return longest_increasing_subsequence(sequence[::-1])[::-1]


def main():
    with open("rosalind_lgis.txt") as file:
        sequence = parse(file)
        increasing = longest_increasing_subsequence(sequence)
        print(" ".join(str(n) for n in increasing))
        decreasing = longest_decreasing_subsequence(sequence)
        print(" ".join(str(n) for n in decreasing))


if __name__ == "__main__":
    main()
