from Day09 import IntcodeComputer, Memory

# PART 1
class NullComputer:
    def __init__(self):
        self.memory = Memory([])
        self.queue = ([], [])
class Network:
    def __init__(self, code, count, hardware=IntcodeComputer):
        self.computers = {i: hardware(code) for i in range(0, count)}

    def run_once(self):
        for c in self.computers.values():
            if len(c.queue[0]) <= 0:
                c.queue[0].append(-1)
        for computer in self.computers.values():
            computer.run_until_input()
            outputs = [[computer.queue[1].pop(0) for x in range(0, 3)] \
                       for x in range(0, len(computer.queue[1])//3)]
            for c, x, y in outputs:
                self.computers[c].queue[0] += [x, y]

    def run_until_id(self, id=255):
        self.computers[id] = NullComputer()
        while len(self.computers[id].queue[0]) <= 0:
            self.run_once()
        return self.computers[id].queue[0]

PUZZLE_INPUT = [int(i) for i in input("> ").split(",")]

n = Network(PUZZLE_INPUT, 50)
v = n.run_until_id()[1]
