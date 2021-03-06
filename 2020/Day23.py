from typing import Union, Any


class CircularList:
    def __init__(self, values: list):
        self._values = list(values)
        self.current = 0

    def __repr__(self):
        t = type(self).__name__ + "(["
        for idx, item in enumerate(self._values):
            if idx:
                t += ", "
            if idx == self.current:
                t += "(" + repr(item) + ")"
            else:
                t += repr(item)
        t += ")]"
        return t

    def _repr_pretty_(self, p, cycle):
        " iPython pretty print "
        typename = type(self).__name__
        if cycle:
            p.text(typename + "([...])")
        else:
            with p.group(len(typename) + 2, typename + "([", "])"):
                for idx, item in enumerate(self._values):
                    if idx:
                        p.text(",")
                        p.breakable()

                    if idx == self.current:
                        with p.group(1, "(", ")"):
                            p.pretty(item)
                    else:
                        p.pretty(item)

    def __getitem__(self, i: Union[int, slice]):
        if isinstance(i, slice):
            return [
                self._values[x % len(self._values)]
                for x in range(i.start, i.stop, i.step or 1)
            ]
        elif isinstance(i, int):
            return self._values[i % len(self._values)]
        else:
            raise ValueError

    def __setitem__(self, i: int, v: Any):
        if isinstance(i, int):
            self._values[i % len(self._values)] = v
        else:
            raise ValueError

    def __contains__(self, v: Any):
        return v in self._values

    def __len__(self):
        return len(self._values)

    def pop(self, i: int):
        index = i % len(self._values)
        if index <= self.current:
            self.current -= 1  # calc mod later
        # print(".pop({}) [{}]".format(i, self.current))
        return self._values.pop(index)

    def insert(self, i: int, v: Any):
        index = i % len(self._values)
        if index <= self.current:
            self.current += 1  # calc mod later
        # print(".insert({}, {}) [{}]".format(i, v, self.current))
        return self._values.insert(index, v)

    def remove(self, v: Any):
        return self.pop(self.index(v))  # correctly modifies self.current

    def index(self, v: Any):
        return self._values.index(v)

    def perform_move(self, length: int = 3):
        " Day23 function "
        maximum = max(self._values)
        movenums = [self[self.current + x + 1] for x in range(length)]
        for n in movenums:
            self.remove(n)
        # print(movenums, self._values, self.current)

        dest = (self[self.current] - 2) % maximum + 1
        while dest not in self:
            dest = (dest - 2) % maximum + 1
        destindex = (self.index(dest) + 1) % len(self._values)
        # print(dest, destindex, self._values, self.current)
        for n in reversed(movenums):
            self.insert(destindex, n)

        self.current = (self.current + 1) % len(self)

    def get_after(self, id: int = 1, limit: int = None):
        if limit is None:
            limit = len(self) - 1
        index1 = self.index(id)
        return self[index1 + 1 : index1 + limit + 1]

    def get_label(self):
        return "".join(map(str, self.get_after(1, limit=None)))


if __name__ == "__main__":
    ilist = list(map(int, input("> ")))

    c = CircularList(ilist)
    for x in range(100):
        c.perform_move()

    print(c.get_label())

