import typer


def main(filename: str):
    with open(filename) as file:
        print(file.readlines())


if __name__ == "__main__":
    typer.run(main)
