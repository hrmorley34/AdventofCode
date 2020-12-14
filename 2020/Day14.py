import re
from puzzle_input import puzzle_input


class Mask:
    value: str

    def __init__(self, value: str):
        self.value = value

    def __xor__(self, o: int):
        for d, c in enumerate(reversed(self.value)):
            if c == "0":
                o &= 2 ** 36 - 2 ** d - 1
            elif c == "1":
                o |= 2 ** d
        return o

    def floating_masks(self, o: int):
        possible = [o]
        for d, c in enumerate(reversed(self.value)):
            if c == "1":
                possible = [p | (2 ** d) for p in possible]
            elif c == "X":
                it = list(possible)
                possible.clear()
                for p in it:
                    possible.append(p & (2 ** 36 - 2 ** d - 1))
                    possible.append(p | (2 ** d))
        return possible


maskline = re.compile(r"mask = ([X01]{36})")
memline = re.compile(r"mem\[(\d+)\] = (\d+)")


PUZZLE_INPUT = puzzle_input().splitlines()


mask = Mask("")
memory_1 = {}
memory_2 = {}
for line in PUZZLE_INPUT:
    m = maskline.match(line)
    if m:
        mask = Mask(m.group(1))
        continue
    m = memline.match(line)
    if m:
        memory_1[int(m[1])] = mask ^ int(m[2])
        for i in mask.floating_masks(int(m[1])):
            memory_2[i] = int(m[2])
        continue
    print(line)

print(sum(memory_1.values()))
print(sum(memory_2.values()))
