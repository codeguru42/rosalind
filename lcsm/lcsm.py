def parse_fasta(fasta_file):
    nucleotide = ''
    for line in fasta_file:
        if line[0] == '>':
            if nucleotide != '':
                yield nucleotide
                nucleotide = ''
        else:
            nucleotide += line.strip()
    yield nucleotide


class Ukkonen:
    def __init__(self, string):
        self.string = string
        self.position = 0
        self.tree = {}

    def step(self):
        self.tree[(self.position, '#')] = {}
        self.position += 1
        return self.tree


def main():
    with open('lcsm.txt') as file:
        print(list(parse_fasta(file)))


if __name__ == '__main__':
    main()
