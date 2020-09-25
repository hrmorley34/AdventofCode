import re
from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().strip().splitlines(False)

### PART 1
##class Deck(list):
##    def __init__(self, len=10, order=None):
##        if order is None:
##            order = range(0, len)
##        try:
##            list.__init__(self, order)
##        except OverflowError:
##            list.__init__(self, map(float, order))
##    def __repr__(self):
##        return "Deck("+(", ".join(map(repr, self)))+")"
##    
##    def parse_command(self, cmd):
##        if re.match(r"^deal into new stack$", cmd.strip(), re.I):
##            self.deal_into_new()
##            return
##        r = re.match(r"^cut (-?\d+)$", cmd.strip(), re.I)
##        if r:
##            self.cut(int(r.group(1)))
##            return
##        r = re.match(r"^deal with increment (\d+)$", cmd.strip(), re.I)
##        if r:
##            self.deal_with_increment(int(r.group(1)))
##            return
##        raise ValueError("Unknown command: {0}".format(cmd.strip()))
##
##    def deal_into_new(self):
##        self.reverse()
##    def cut(self, n):
##        if n > 0:
##            top = [self.pop(0) for i in range(0, n)]
##            self += top
##        elif n < 0:
##            self.cut(len(self) + n) # move back to front is same as front to back
##    def deal_with_increment(self, n):
##        new = [None] * len(self)
##        for count in range(0, len(self)*n, n):
##            new[count % len(self)] = self[count//n]
##        Deck.__init__(self, order=new)
##
##deck = Deck(10007)
##for cmd in PUZZLE_INPUT:
##    deck.parse_command(cmd)
##print(deck.index(2019))

# PART 2 - too inefficient
class DeckTracker:
    def __init__(self, len, *pos):
        self.len = len
        self.pos = list(pos)
    def __repr__(self):
        return "DeckTracker(...)"
    
    def parse_command(self, cmd):
        if self._re_deal_into_new.match(cmd.strip()):
            self.deal_into_new()
            return
        r = self._re_cut.match(cmd.strip())
        if r:
            self.cut(int(r.group(1)))
            return
        r = self._re_deal_with_increment.match(cmd.strip())
        if r:
            self.deal_with_increment(int(r.group(1)))
            return
        raise ValueError("Unknown command: {0}".format(cmd.strip()))
    def reverse_parse_command(self, cmd):
        if self._re_deal_into_new.match(cmd.strip()):
            self.reverse_deal_into_new()
            return
        r = self._re_cut.match(cmd.strip())
        if r:
            self.reverse_cut(int(r.group(1)))
            return
        r = self._re_deal_with_increment.match(cmd.strip())
        if r:
            self.reverse_deal_with_increment(int(r.group(1)))
            return
        raise ValueError("Unknown command: {0}".format(cmd.strip()))

    _re_deal_into_new = re.compile(r"^deal into new stack$", re.I)
    def deal_into_new(self):
        self.pos = [self.len-p-1 for p in self.pos]
    reverse_deal_into_new = deal_into_new
    _re_cut = re.compile(r"^cut (-?\d+)$", re.I)
    def cut(self, n):
        self.pos = [(p-n) % self.len for p in self.pos]
    def reverse_cut(self, n):
        self.cut(-n)
    _re_deal_with_increment = re.compile(r"^deal with increment (\d+)$", re.I)
    def deal_with_increment(self, n):
        npos = []
        for p in self.pos:
            npos.append((p*n) % self.len)
        self.pos = npos
    def reverse_deal_with_increment(self, n):
        npos = []
        for p in self.pos:
            count = p
            while count/n != count//n: count += self.len
            npos.append(count // n)
        self.pos = npos

# P1 but much more efficient
deck = DeckTracker(10007, 2019)
for cmd in PUZZLE_INPUT:
    deck.parse_command(cmd)
print(deck.pos[0])
# --------

deck = DeckTracker(119315717514047, 2020)
hstart = deck.pos[0]
i = 0
rep = None
max_ = 101741582076661
while i <= max_:
    i += 1
    if i%(10**4) == 0: print("-", i)
    for cmd in reversed(PUZZLE_INPUT):
        deck.reverse_parse_command(cmd)
    if deck.pos[0] == hstart:
        print(">>>>>>>>>>>")
        rep = i
        max_ %= rep

print(deck.pos[0])
