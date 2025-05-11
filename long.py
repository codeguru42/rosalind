from rosalind import parse_fasta


def prefixes(s):
    n = len(s)
    for i in range(n + 1):
        yield s[: n - i]


def test_prefixes():
    result = list(prefixes("ATTAGACCTG"))
    expected = [
        "ATTAGACCTG",
        "ATTAGACCT",
        "ATTAGACC",
        "ATTAGAC",
        "ATTAGA",
        "ATTAG",
        "ATTA",
        "ATT",
        "AT",
        "A",
        "",
    ]
    assert result == expected


def suffixes(s):
    n = len(s)
    for i in range(n + 1):
        yield s[i:]


def test_suffixes():
    result = list(suffixes("ATTAGACCTG"))
    expected = [
        "ATTAGACCTG",
        "TTAGACCTG",
        "TAGACCTG",
        "AGACCTG",
        "GACCTG",
        "ACCTG",
        "CCTG",
        "CTG",
        "TG",
        "G",
        "",
    ]
    assert result == expected


def most_overlapping_pair(dnas):
    max_a = ""
    max_b = ""
    max_overlap = ""
    for a in dnas:
        for b in dnas:
            if a != b:
                for suff in suffixes(a):
                    pre = b[: len(suff)]
                    if suff == pre and len(pre) > len(max_overlap):
                        max_a = a
                        max_b = b
                        max_overlap = pre
                        break
    return max_a, max_b, max_overlap


def test_most_overlapping_pair():
    dnas = [
        "ATTAGACCTG",
        "CCTGCCGGAA",
        "AGACCTGCCG",
        "GCCGGAATAC",
    ]
    result = most_overlapping_pair(dnas)
    expected = (
        "ATTAGACCTG",
        "AGACCTGCCG",
        "AGACCTG",
    )
    assert result == expected


def long(dnas):
    temp = dnas.copy()
    while len(temp) > 1:
        a, b, overlap = most_overlapping_pair(temp)
        temp.remove(a)
        temp.remove(b)
        temp.append(a + b[len(overlap) :])
    return temp[0]


def test_long():
    dnas = [
        "ATTAGACCTG",
        "CCTGCCGGAA",
        "AGACCTGCCG",
        "GCCGGAATAC",
    ]
    result = long(dnas)
    expected = "ATTAGACCTGCCGGAATAC"
    assert result == expected


def main():
    with open("rosalind_long.txt") as file:
        fasta = parse_fasta(file)
        print(long(list(fasta)))


if __name__ == "__main__":
    main()
