regex = "^ENWWW(NEEE|SSE(EE|N))$"
regex = regex[1:-1]

class RegAll:
    def __init__(self, text=None):
        self._order = []
        self._depth = 0
        if text is not None:
            self.parse(text)
    @property
    def _point(self):
        ci = self._order
        for i in range(0, int(self._depth*2)):
            ci = ci[-1]
        return(ci)
    def _parse_char(self, char):
        if char == "(":
            self._point.append([[]])
            self._depth += 1
        elif char == ")":
            self._depth -= 1
        elif char == "|":
            self._depth -= 0.5
            self._point.append([])
            self._depth += 0.5
        else:
            if len(self._point) and isinstance(self._point[-1], str):
                self._point[-1] += char
            else:
                self._point.append(char)
    def parse(self, text):
        for c in text:
            self._parse_char(c)
    def all_possibles(self):
        return(self._all_p(self._order))
    def _all_p(self, lst):
        combos = [""]
        for c in lst:
            new_combos = []
            if isinstance(c, str):
                for combo in combos:
                    new_combos.append(combo + c)
            else:
                ilist = list(map(self._all_p, c))
                for i in ilist:
                    for icombo in i:
                        for combo in combos:
                            new_combos.append(combo+icombo)
            combos = new_combos
        return(combos)

all_reg = RegAll(regex)
all_orders = all_reg.all_possibles()
