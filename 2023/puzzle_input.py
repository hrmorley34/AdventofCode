def puzzle_input() -> str:
    t = ""
    try:
        t = input("> ")
        while True:
            t += "\n" + input()
    except KeyboardInterrupt:
        return t
    except EOFError:
        return t
