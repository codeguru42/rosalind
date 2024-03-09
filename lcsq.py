import typer

from rosalind import parse_fasta


def main(filename: str):
    with open(filename, "r") as file:
        dnas = parse_fasta(file)
        print(list(dnas))


if __name__ == "__main__":
    typer.run(main)
