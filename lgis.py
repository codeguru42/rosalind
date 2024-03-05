def test_longest_increasing_subsequence():
    sequence = [5, 1, 4, 2, 3]
    expected = [1, 2, 3]
    actual = longest_increasing_subsequence(sequence)
    assert expected == actual


def parse(file):
    n = int(file.readline().strip())
    return [int(n) for n in file.readline().strip().split()]


def longest_increasing_subsequence(sequence: list[int]) -> list[int]:
    subsequences = [[] for _ in range(len(sequence))]
    for i in range(len(sequence)):
        found = False
        for j in range(i):
            if sequence[j] < sequence[i]:
                subsequences[i] = [*subsequences[j], sequence[i]]
                found = True
        if not found:
            subsequences[i] = [sequence[i]]
    return subsequences[-1]


def main():
    with open("rosalind_lgis.txt") as file:
        print(parse(file))


if __name__ == "__main__":
    main()
