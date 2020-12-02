from Day09 import IntcodeComputer, Memory


# PART 1
class NullComputer:
    def __init__(self):
        self.memory = Memory([])
        self.queue = ([], [])


class Network:
    def __init__(self, code, count, hardware=IntcodeComputer):
        self.computers = {i: hardware(code) for i in range(count)}
        for i, computer in self.computers.items():
            computer.queue[0].append(i)
            computer.run_until_input()

    def run_once(self):
        for computer in self.computers.values():
            if isinstance(computer, NullComputer):
                continue
            if len(computer.queue[0]) <= 0:
                computer.queue[0].append(-1)
            computer.run_until_input()
            while len(computer.queue[1]):
                c = computer.queue[1].pop(0)
                x = computer.queue[1].pop(0)
                y = computer.queue[1].pop(0)
                self.computers[c].queue[0].extend([x, y])

    def run_until_id(self, id=255):
        if id not in self.computers:
            self.computers[id] = NullComputer()
        iterations = 0
        while len(self.computers[id].queue[0]) <= 0:
            self.run_once()
            iterations += 1
            if iterations % 20000 == 0:
                print(iterations)
        return self.computers[id].queue[0]


PUZZLE_INPUT = [int(i) for i in input("> ").split(",")]

n = Network(PUZZLE_INPUT, 50)
v = n.run_until_id()[1]
