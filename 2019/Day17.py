from Day09 import IntcodeComputer

PUZZLE_INPUT = [int(i) for i in input("> ").split(",")]
# PART 1
i = IntcodeComputer(list(PUZZLE_INPUT))

s = ""
i.run_until_input()
while len(i.queue[1]):
    n = i.queue[1].pop(0)
    char = chr(n)
    s += char

print(s)

sl = s.strip().splitlines(False)
intersections = set()
for y in range(0, len(sl)):
    for x in range(0, len(sl[y])):
        if sl[y][x] == "#":
            count = 0
            if y-1 >= 0 and sl[y-1][x] == "#": count += 1
            if x-1 >= 0 and sl[y][x-1] == "#": count += 1
            if y+1 < len(sl) and sl[y+1][x] == "#": count += 1
            if x+1 < len(sl[y]) and sl[y][x+1] == "#": count += 1
            if count > 2: # 2 is one pathway
                intersections.add((x, y))

print(sum([x*y for (x,y) in intersections]))

# PART 2
def get_nextpos(pos, direction):
    if direction == 0:
        return (pos[0], pos[1]-1)
    if direction == 1:
        return (pos[0]+1, pos[1])
    if direction == 2:
        return (pos[0], pos[1]+1)
    if direction == 3:
        return (pos[0]-1, pos[1])
def get_pos(sl, pos, d=None):
    x, y = pos
    if 0 > y or y >= len(sl) or 0 > x or x >= len(sl[y]):
        return d
    else:
        return sl[y][x]

path = []
pos = [(s.index("^"),i) for (i,s) in enumerate(sl) if s.find("^") != -1][0]
direction = 0 # 0-N, 1-E, 2-S, 3-W
while True:
    nextpos = get_nextpos(pos, direction)
    count = 0
    while get_pos(sl, nextpos) == "#":
        count += 1
        pos = nextpos
        nextpos = get_nextpos(pos, direction)
    if count > 0:
        path.append(count)
    r_nextpos = get_nextpos(pos, (direction+1)%4)
    l_nextpos = get_nextpos(pos, (direction-1)%4)
    if get_pos(sl, r_nextpos) == "#":
        path.append("R")
        direction += 1
        direction %= 4
    elif get_pos(sl, l_nextpos) == "#":
        path.append("L")
        direction -= 1
        direction %= 4
    else:
        break

pathstr = ",".join(map(str, path))
print(pathstr)
A = "R,4,L,10,L,10"
B = "L,8,R,12,R,10,R,4"
C = "L,8,L,8,R,10,R,4"
MAIN = "A,B,A,B,A,C,B,C,A,C"
print(MAIN)
print("A:", A)
print("B:", B)
print("C:", C)
MAINREPL = MAIN.replace("A",A).replace("B",B).replace("C",C)
print("->", MAINREPL)
assert MAINREPL == pathstr

i = IntcodeComputer(list(PUZZLE_INPUT))
i.memory[0] = 2
l = i.queue[0]
l += [ord(c) for c in MAIN+"\n"]
l += [ord(c) for c in A+"\n"]
l += [ord(c) for c in B+"\n"]
l += [ord(c) for c in C+"\n"]
l += [ord("n"), ord("\n")]

i.run_until_input()

dust = i.queue[1].pop(-1)
print(dust)
