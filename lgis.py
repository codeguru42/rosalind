def parse(file):
    n = int(file.readline().strip())
    return [int(n) for n in file.readline().strip().split()]


def main():
    with open("rosalind_lgis.txt") as file:
        print(parse(file))


if __name__ == "__main__":
    main()
