from rosalind import parse_fasta


def main():
    with open("lcsm.txt") as file:
        print(list(parse_fasta(file)))


if __name__ == "__main__":
    main()
