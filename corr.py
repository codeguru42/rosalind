import typer

from rosalind import parse_fasta


def main(filename: str):
    with open(filename) as f:
        rnas = parse_fasta(f)
        print(list(rnas))


if __name__ == '__main__':
    typer.run(main)