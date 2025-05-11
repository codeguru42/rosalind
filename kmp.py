import typer


def main(filename: str):
    with open(filename) as f:
        print(f.read())


if __name__ == '__main__':
    typer.run(main)