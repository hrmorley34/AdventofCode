def puzzle_input():
    t = ""
    try:
        t = input("> ")
        while True:
            t += "\n"+input()
    except KeyboardInterrupt:
        return t
    except EOFError:
        return t
