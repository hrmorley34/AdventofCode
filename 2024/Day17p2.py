from Day17 import Computer
from puzzle_input import puzzle_input

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    octd = []
    COMPUTER = Computer.from_string(PUZZLE_INPUT)
    while len(octd) < len(COMPUTER.script):
        for d in range(8):
            COMPUTER = Computer.from_string(PUZZLE_INPUT)
            COMPUTER.a = int("".join(map(str, octd + [d])), 8)
            out = COMPUTER.run()
            if out == COMPUTER.script[-len(octd) - 1 :]:
                octd.append(d)
                break
        else:
            octd[-1] += 1
            if octd[-1] >= 8:
                octd[-2] += 1
                octd[-1] = 0
            if octd[-2] >= 8:
                octd[-3] += 1
                octd[-2] = 0
            if octd[-3] >= 8:
                raise ValueError()
    print("Part 2:", int("".join(map(str, octd)), 8))
