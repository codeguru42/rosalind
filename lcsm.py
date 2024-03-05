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


def longest_common_substring(*strings: str) -> set[str]:
    lcs = set()
    n = len(strings)
    all_strings = "".join(s + chr(i) for i, s in enumerate(strings))
    sentinels = list(sentinel_indexes(*strings))
    suff_idxs = list(suffix_array(all_strings))
    lcps = longest_common_prefix_array(all_strings)
    colors = [get_color(i, sentinels) for i in suff_idxs]
    suffs = [all_strings[i:] for i in suff_idxs]
    search_list = list(
        zip(
            lcps,
            suff_idxs,
            colors,
            suffs,
        )
    )
    top = 0
    bottom = 1
    lcs_length = 0
    while top < len(search_list) and bottom < len(search_list):
        window = search_list[top:bottom]
        color_count = len(set(color for _, _, color, _ in window))
        if color_count < n:
            bottom += 1
            continue
        if color_count == n:
            lcp = min(lcp for lcp, _, _, _ in window[1:])
            if lcp > lcs_length:
                lcs = set()
                lcs_length = lcp
            if lcp == lcs_length:
                window_suff_idxs = [suff_idx for _, suff_idx, _, _ in window]
                lcs.add(all_strings[window_suff_idxs[0] : window_suff_idxs[0] + lcp])
            top += 1
    return lcs


def sentinel_indexes(s1, *strings: str) -> Generator[int]:
    i = len(s1)
    yield i
    for s in strings:
        i += len(s) + 1
        yield i


def get_color(idx, sentinels):
    for i, s_idx in enumerate(sentinels):
        if idx <= s_idx:
            return i


def suffix_array(s: str) -> Generator[int]:
    yield from sorted(range(len(s)), key=lambda x: s[x:])


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
