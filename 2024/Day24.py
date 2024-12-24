from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass(frozen=True)
class Operation:
    left: str
    op: str
    right: str
    dest: str

    def ready(self, ns: dict[str, bool]) -> bool:
        return self.left in ns and self.right in ns

    def calculate(self, ns: dict[str, bool]) -> None:
        left = ns[self.left]
        right = ns[self.right]
        if self.op == "AND":
            value = left and right
        elif self.op == "OR":
            value = left or right
        elif self.op == "XOR":
            value = left ^ right
        else:
            raise ValueError
        ns[self.dest] = value

    @classmethod
    def from_line(cls, line: str):
        lhs, dest = line.split(" -> ")
        left, op, right = lhs.split(" ")
        return cls(left, op, right, dest)


def bin2dec(name: str, ns: dict[str, bool]) -> int:
    return sum(v << int(s[1:]) for s, v in ns.items() if s[0] == name)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    data, ops = PUZZLE_INPUT.split("\n\n")
    VARS = {v: b == "1" for v, b in (line.split(": ") for line in data.splitlines())}
    OPS = set(map(Operation.from_line, ops.splitlines()))

    operations = OPS.copy()
    ns: dict[str, bool] = VARS.copy()
    while operations:
        assert any(op.ready(ns) for op in operations)
        for op in operations.copy():
            if op.ready(ns):
                op.calculate(ns)
                operations.remove(op)
    print("Part 1:", bin2dec("z", ns))
