from puzzle_input import puzzle_input


class Console:
    script: list
    acc: int

    def __init__(self, script: list, pc: int = 0, acc: int = 0):
        self.script = list(script)
        self.pc = pc
        self.acc = acc

    def run(self, cmd: str):
        args = cmd.lower().split()
        if args[0] == "acc":
            self.acc += int(args[1])
        elif args[0] == "jmp":
            self.pc += int(args[1])
            return
        elif args[0] == "nop":
            pass
        else:
            raise Exception
        self.pc += 1

    def run_iter(self):
        while True:
            self.run(self.script[self.pc])
            yield self
            if self.pc >= len(self.script):
                return

    @property
    def finished(self):
        return self.pc >= len(self.script)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    c = Console(PUZZLE_INPUT)

    pcs = set()
    prevacc = None
    for c in c.run_iter():
        if c.pc in pcs:
            break
        pcs.add(c.pc)
        prevacc = c.acc

    print(prevacc)

    for i in range(len(PUZZLE_INPUT)):
        c = Console(PUZZLE_INPUT)
        if c.script[i].lower().startswith("jmp"):
            c.script[i] = "nop" + c.script[i][3:]
        else:
            continue
        pcs = set()
        for c in c.run_iter():
            if c.pc in pcs:
                break
            pcs.add(c.pc)
        if c.pc >= len(c.script):
            print(c.acc)
