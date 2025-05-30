import typer

from rosalind import sliding_window

monoisotopic_mass_table = {
    "A": 71.03711,
    "C": 103.00919,
    "D": 115.02694,
    "E": 129.04259,
    "F": 147.06841,
    "G": 57.02146,
    "H": 137.05891,
    "I": 113.08406,
    "K": 128.09496,
    "L": 113.08406,
    "M": 131.04049,
    "N": 114.04293,
    "P": 97.05276,
    "Q": 128.05858,
    "R": 156.10111,
    "S": 87.03203,
    "T": 101.04768,
    "V": 99.06841,
    "W": 186.07931,
    "Y": 163.06333,
}


def parse(f):
    for line in f:
        yield float(line)


def diffs(l):
    for x, y in sliding_window(l, 2):
        yield y - x


def calc_isotope(x):
    return min(monoisotopic_mass_table.items(), key=lambda i: abs(i[1] - x))[0]


def main(filename: str):
    with open(filename, "r") as f:
        l = parse(f)
        d = diffs(l)
        p = "".join(calc_isotope(x) for x in d)
        print(p)


if __name__ == "__main__":
    typer.run(main)
