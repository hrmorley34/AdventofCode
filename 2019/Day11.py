import collections
from Day09 import Memory, IntcodeComputer

PUZZLE_INPUT = input("> ")
PUZZLE_INPUT = [int(x) for x in PUZZLE_INPUT.split(",")]

# PART 1
i = IntcodeComputer(PUZZLE_INPUT)

paint_area = {}
pos = (0, 0)
rot = 0 # 0-up, 1-right, 2-down, 3-left
while True:
    i.queue[0].append(paint_area.get(pos, 0))
    if i.run_until_input(): break
    for x in range(2):
        op = i.queue[1].pop(0)
        if x == 0:
            paint_area[pos] = op
        else:
            rot += 1 if op else -1
            rot %= 4
            if rot == 0: pos = (pos[0], pos[1]+1)
            elif rot == 1: pos = (pos[0]+1, pos[1])
            elif rot == 2: pos = (pos[0], pos[1]-1)
            elif rot == 3: pos = (pos[0]-1, pos[1])

print(len(paint_area))

# PART 2
i = IntcodeComputer(PUZZLE_INPUT)

pos = (0, 0)
paint_area = {pos: 1}
rot = 0 # 0-up, 1-right, 2-down, 3-left
while True:
    i.queue[0].append(paint_area.get(pos, 0))
    if i.run_until_input(): break
    for x in range(2):
        op = i.queue[1].pop(0)
        if x == 0:
            paint_area[pos] = op
        else:
            rot += 1 if op else -1
            rot %= 4
            if rot == 0: pos = (pos[0], pos[1]+1)
            elif rot == 1: pos = (pos[0]+1, pos[1])
            elif rot == 2: pos = (pos[0], pos[1]-1)
            elif rot == 3: pos = (pos[0]-1, pos[1])

for y in range(10, -50, -1):
    s = ""
    for x in range(-30, 30):
        s += "O" if paint_area.get((x,y),0) else " "
    print(s)
