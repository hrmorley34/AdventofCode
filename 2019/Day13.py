from Day09 import Memory, IntcodeComputer

PUZZLE_INPUT = input("> ")
PUZZLE_INPUT = [int(x) for x in PUZZLE_INPUT.split(",")]

# PART 1
i = IntcodeComputer(PUZZLE_INPUT)

tile_grid = {}
i.run_until_input()
while len(i.queue[1]):
    for n in range(3):
        op = i.queue[1].pop(0)
        if n == 0:
            x = op
        elif n == 1:
            y = op
        else:
            tile_grid[(x, y)] = op

print(list(tile_grid.values()).count(2)) # 2 = block tile

# PART 2
i = IntcodeComputer(PUZZLE_INPUT)
i.memory[0] = 2

CHARS = {0: " ", 1: "#", 2: "O", 3: "-", 4: "*"}

score = 0
tile_grid = {}
i.run_until_input()
iteration = 0
while True:
    while len(i.queue[1]):
        for n in range(3):
            op = i.queue[1].pop(0)
            if n == 0:
                x = op
            elif n == 1:
                y = op
            elif x == -1 and y == 0:
                score = op
            else:
                tile_grid[(x, y)] = op
    if i.run_until_input():
        break
    else:
        if iteration % 10 == 0: # every tenth frame only
            s = str(score).rjust(42, " ") + "\n"
            for y in range(0, 25):
                for x in range(0, 42):
                    s += CHARS[tile_grid.get((x, y), 0)]
                s += "\n"
            s += "\n"
            print(s, end="")
        #inp = input("> ").lower()
        #if inp in ["a", "l", "-1"]:
        #    i.queue[0].append(-1)
        #elif inp in ["d", "r", "1", "+1"]:
        #    i.queue[0].append(+1)
        #else: # default to neutral - " ", "s", "w", "n", "0"
        #    i.queue[0].append(0)
        # -- USE MEMORY HACKS --
        i.queue[0].append(0)
        rap = list(i.memory)[639+42*23:639+42*24] # row above paddle
        if 4 in rap:
            rwp = list(i.memory)[639+42*24:639+42*25] # row with paddle
            i.memory[639+42*24+rwp.index(3)] = 0 # delete old paddle
            i.memory[639+42*24+rap.index(4)] = 3 # move paddle under ball
        i.run_until_input()
    iteration += 1

s = str(score).rjust(42, " ") + "\n"
for y in range(0, 25):
    for x in range(0, 42):
        s += CHARS[tile_grid.get((x, y), 0)]
    s += "\n"
s += "\n"
print(s, end="")
