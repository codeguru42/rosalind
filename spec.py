import typer

def main(filename: str):
    with open(filename, "r") as f:
        for line in f:
            print(line.strip())

if __name__ == '__main__':
    typer.run(main)