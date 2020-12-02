import re
from puzzle_input import puzzle_input


CMDRE = re.compile(
    r"^((cut (-?\d+))|(deal with increment (\d+))|(deal into new stack))$"
)


class DeckTracker:
    cards: int
    initial: int
    track: int

    def __init__(self, cards: int, track: int):
        self.track = self.initial = int(track)
        self.cards = int(cards)

    def deal_into_new_stack(self, reverse: bool = False):
        " (Reverse has no effect) "
        self.track = (-self.track - 1) % self.cards

    def Rdeal_into_new_stack(self):
        return self.deal_into_new_stack(reverse=True)

    def cut(self, N: int, reverse: bool = False):
        # modulo fixes negatives
        N = int(N) % self.cards
        if reverse:
            N = -N
        self.track -= N
        self.track %= self.cards

    def Rcut(self, N: int):
        return self.cut(N, reverse=True)

    def deal_with_increment(self, N: int, reverse: bool = False):
        if reverse:
            # Thanks Mark Dikinson
            # https://stackoverflow.com/questions/4798654/modular-multiplicative-inverse-function-in-python
            N = int(pow(N, -1, self.cards))
        self.track = (self.track * N) % self.cards

    def Rdeal_with_increment(self, N: int):
        return self.deal_with_increment(N, reverse=True)

    def _parse(self, cmd: str, reverse: bool = False):
        if isinstance(cmd, re.Match):
            m = cmd
        else:
            m = CMDRE.match(cmd)
        if not m:
            print(cmd)
            raise Exception(r"Invalid command {cmd!r}")
        if m.group(2):
            return lambda: self.cut(int(m.group(3)), reverse=reverse)
        elif m.group(4):
            return lambda: self.deal_with_increment(int(m.group(5)), reverse=reverse)
        elif m.group(6):
            return lambda: self.deal_into_new_stack(reverse=reverse)
        raise Exception(r"Invalid command {cmd!r}")

    def parse(self, cmd: str, reverse: bool = False):
        self._parse(cmd=cmd, reverse=reverse)()

    def parse_many(self, lines, reverse: bool = False):
        if reverse:
            lines = reversed(lines)
        for line in iter(lines):
            self.parse(line, reverse=reverse)

    def parse_many_dry(self, lines, reverse: bool = False):
        if reverse:
            lines = reversed(lines)
        return tuple(self._parse(line, reverse=reverse) for line in iter(lines))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    # Part 1
    t1 = DeckTracker(10007, 2019)
    t1.parse_many(PUZZLE_INPUT)
    print(t1.track)

    # Part 2 - still rather slow
    t2 = DeckTracker(119315717514047, 2020)
    funcs = t2.parse_many_dry(PUZZLE_INPUT, True)
    seen = (2020,)
    x = 1
    total = 101741582076661
    while x < total:
        if x % 10 ** 4 == 0:
            print(f"{x}".rjust(15), end="\r")
        for f in funcs:
            f()
        seen += (t2.track,)
        if t2.track == 2020:
            print(f"{x}".rjust(15))
            print(seen[total % x])
            break
        x += 1
    else:
        print(f"{x}".rjust(15))
        print(t2.track)
