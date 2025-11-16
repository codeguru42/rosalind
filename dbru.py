from pathlib import Path

import typer



def main(filename: Path):
    with open(filename) as f:
        dnas = [line.strip() for line in f.readlines()]
        print(dnas)


if __name__ == "__main__":
    typer.run(main)
