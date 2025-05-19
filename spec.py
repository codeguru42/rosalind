import typer


def parse(f):
    for line in f:
        yield float(line)

def main(filename: str):
    with open(filename, "r") as f:
        l = list(parse(f))
        print(l)

if __name__ == '__main__':
    typer.run(main)