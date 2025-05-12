import typer

from rosalind import parse_fasta


def main(filename: str):
    with open(filename) as f:
        fasta = parse_fasta(f)
        print(fasta)

if __name__ == '__main__':
    typer.run(main)