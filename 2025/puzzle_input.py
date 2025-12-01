import sys


def puzzle_input() -> str:
    t = ""
    try:
        t = input("> " if sys.stdin.isatty() else "")
        while True:
            t += "\n" + input()
    except KeyboardInterrupt:
        return t
    except EOFError:
        return t
