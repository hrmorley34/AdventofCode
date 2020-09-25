import time
from Day09 import IntcodeComputer

PUZZLE_INPUT = input("> ")
INTCODE = [int(x) for x in PUZZLE_INPUT.split(",")]

### PART 1 attempt 1 - non-functional
##def print_maze(maze, dpos=None, path=[]):
##    xrange = {pos[0] for pos in maze.keys()}
##    yrange = {pos[1] for pos in maze.keys()}
##    xmin, xmax = min(xrange), max(xrange)
##    ymin, ymax = min(yrange), max(yrange)
##    s = "="*(xmax-xmin+1)+"\n"
##    for y in range(ymax, ymin-1, -1):
##        for x in range(xmin, xmax+1, 1):
##            if (x, y) == dpos: s += "D"; continue
##            if (x, y) in path: s += "@"; continue
##            m = maze.get((x, y), -1)
##            if m == -1: s += " "
##            elif m == 0: s += "#"
##            elif m == 1: s += "."
##            elif m == 2: s += "*"
##            elif m == 3: s += "0"
##            else: s += "?"
##        s += "\n"
##    s += "="*(xmax-xmin+1)
##    print(s+"\n", end="")
##def convdir(x):
##    if x%4 == 1: return 1 # N
##    elif x%4 == 2: return 4 # E
##    elif x%4 == 3: return 2 # S
##    elif x%4 == 0: return 3 # W
##
##pos = (0, 0)
##maze = {pos: 3}
##i = IntcodeComputer(INTCODE)
##OXLOC = None
##nextdir = 1
##iteration = 0
##while True:
##    if iteration%100 == 0: print_maze(maze, pos)
##    iteration += 1
##
##    nextdir %= 4
##    if nextdir % 4 == 1: # N
##        newpos = (pos[0], pos[1] + 1)
##    elif nextdir % 4 == 2: # E
##        newpos = (pos[0] + 1, pos[1])
##    elif nextdir % 4 == 3: # S
##        newpos = (pos[0], pos[1] - 1)
##    elif nextdir % 4 == 0: # W
##        newpos = (pos[0] - 1, pos[1])
##
##    i.queue[0].append(convdir(nextdir))
##    i.run_until_input()
##    code = i.queue[1].pop(0)
##
##    #print(pos, DIRNAMES[nextdir%4], code, newpos)
##
##    if code == 0:
##        if newpos not in maze.keys():
##            maze[newpos] = 0
##        nextdir += 1
##    elif code == 1:
##        pos = newpos
##        if pos == (0, 0) and OXLOC is not None:
##            break
##        if pos not in maze.keys():
##            maze[pos] = 1
##        nextdir -= 1
##    elif code == 2:
##        maze[newpos] = 2
##        OXLOC = newpos
##
##print_maze(maze)
##
##def dist_to_go(pos): return abs(pos[0]-OXLOC[0])+abs(pos[1]-OXLOC[1])
##def neighbours(pos): return [p for p in [(pos[0], pos[1]+1),
##                                         (pos[0]+1, pos[1]),
##                                         (pos[0], pos[1]-1),
##                                         (pos[0]-1, pos[1])]\
##                             if maze.get(p, 0) != 0]
##
### A*-ish algorithm
##pos = 0
##checked = {}
##to_check = [((0, 0), 0, None)]
##iteration = 0
##while True:
##    iteration += 1
##    to_check.sort(key=lambda x: dist_to_go(x[0]))
##
##    pos, d, from_ = to_check.pop(0)
##    if pos == OXLOC:
##        checked[pos] = (from_, d)
##        break
##    nbrs = neighbours(pos)
##    for n in nbrs:
##        if n not in checked.keys():
##            to_check.append((n, d+1, pos))
##    checked[pos] = (from_, d)
##    
##    if iteration%2 == 0:
##        chain = []
##        chainfrom = from_
##        while chainfrom not in [None, (0, 0)]:
##            chain.append(chainfrom)
##            chainfrom, _ = checked[chainfrom]
##        print_maze(maze, path=chain)
##### Brute-force algorithm
####pos = 0
####checked = {}
####to_check = [((0, 0), 0, None)]
####iteration = 0
####while True:
####    iteration += 1
####    to_check.sort(key=lambda x: x[1])
####
####    pos, d, from_ = to_check.pop(0)
####    if pos == OXLOC:
####        checked[pos] = (from_, d)
####        break
####    nbrs = neighbours(pos)
####    for n in nbrs:
####        if n not in checked.keys():
####            to_check.append((n, d+1, pos))
####    checked[pos] = (from_, d)
####    
####    if iteration%5 == 0:
####        chain = []
####        chainfrom = from_
####        while chainfrom not in [None, (0, 0)]:
####            chain.append(chainfrom)
####            chainfrom, _ = checked[chainfrom]
####        print_maze(maze, path=chain)
##
##chain = []
##chainfrom, n = checked[OXLOC]
##print(n)
##while chainfrom != (0, 0):
##    chain.append(chainfrom)
##    chainfrom, n = checked[chainfrom]
##    print(n)
##print_maze(maze, path=chain)
##print(checked[OXLOC][1])

# PART 1 attempt 2 - functional
def print_maze(maze, dpos=None, path=[]):
    xrange = {pos[0] for pos in maze.keys()}
    yrange = {pos[1] for pos in maze.keys()}
    xmin, xmax = min(xrange), max(xrange)
    ymin, ymax = min(yrange), max(yrange)
    s = "="*(xmax-xmin+1)+"\n"
    for y in range(ymax, ymin-1, -1):
        for x in range(xmin, xmax+1, 1):
            if (x, y) == dpos: s += "D"; continue
            if (x, y) == (0,0): s += "0"; continue
            if (x, y) == OXLOC: s += "*"; continue
            if (x, y) in path: s += "@"; continue
            m = maze.get((x, y), -1)
            if m == -1: s += " "
            elif m == None: s += "#"
            else: s += "."
        s += "\n"
    s += "="*(xmax-xmin+1)
    print(s+"\n", end="")
def convdir(x):
    if x%4 == 1: return 1 # N
    elif x%4 == 2: return 4 # E
    elif x%4 == 3: return 2 # S
    elif x%4 == 0: return 3 # W

pos = (0, 0)
pscore = 0
maze = {pos: pscore}
i = IntcodeComputer(INTCODE)
OXLOC = None
nextdir = 1
iteration = 0
while True:
    if iteration%100 == 0: print_maze(maze, pos)
    iteration += 1

    nextdir %= 4
    if nextdir % 4 == 1: # N
        newpos = (pos[0], pos[1] + 1)
    elif nextdir % 4 == 2: # E
        newpos = (pos[0] + 1, pos[1])
    elif nextdir % 4 == 3: # S
        newpos = (pos[0], pos[1] - 1)
    elif nextdir % 4 == 0: # W
        newpos = (pos[0] - 1, pos[1])

    i.queue[0].append(convdir(nextdir))
    i.run_until_input()
    code = i.queue[1].pop(0)

    #print(pos, DIRNAMES[nextdir%4], code, newpos)

    if code == 0:
        if newpos not in maze.keys():
            maze[newpos] = None
        nextdir += 1
    elif code == 1:
        pos = newpos
        if pos not in maze.keys():
            pscore += 1
            maze[pos] = pscore
        else:
            pscore = maze[pos]
        nextdir -= 1
    elif code == 2:
        pscore += 1
        maze[newpos] = pscore
        pos = OXLOC = newpos
        break

ANSWER_P1 = pscore

# PART 2
def print_maze_p2(maze):
    xrange = {pos[0] for pos in maze.keys()}
    yrange = {pos[1] for pos in maze.keys()}
    xmin, xmax = min(xrange), max(xrange)
    ymin, ymax = min(yrange), max(yrange)
    s = "="*(xmax-xmin+1)+"\n"
    for y in range(ymax, ymin-1, -1):
        for x in range(xmin, xmax+1, 1):
            if (x, y) == (0,0): s += "0"; continue
            if (x, y) == OXLOC: s += "*"; continue
            m = maze.get((x, y), -1)
            if m == -1: s += " "
            elif m == None: s += "#"
            elif m == False: s += "."
            else: s += "@"
        s += "\n"
    s += "="*(xmax-xmin+1)
    print(s+"\n", end="")

pscore = 0
maze = {p: (False if isinstance(maze[p], int) else maze[p]) for p in maze.keys()}
maze[pos] = 0
nextdir = 1
iteration = 0
while False in maze.values():
    if iteration%500 == 0: print_maze_p2(maze)
    iteration += 1

    nextdir %= 4
    if nextdir % 4 == 1: # N
        newpos = (pos[0], pos[1] + 1)
    elif nextdir % 4 == 2: # E
        newpos = (pos[0] + 1, pos[1])
    elif nextdir % 4 == 3: # S
        newpos = (pos[0], pos[1] - 1)
    elif nextdir % 4 == 0: # W
        newpos = (pos[0] - 1, pos[1])

    i.queue[0].append(convdir(nextdir))
    i.run_until_input()
    code = i.queue[1].pop(0)

    #print(pos, DIRNAMES[nextdir%4], code, newpos)

    if code == 0:
        if newpos not in maze.keys():
            maze[newpos] = None
        nextdir += 1
    elif code in (1, 2):
        pos = newpos
        if maze.get(pos, False) == False:
            pscore += 1
            maze[pos] = pscore
        else:
            pscore = maze[pos]
        nextdir -= 1

ANSWER_P2 = max([m for m in maze.values() if isinstance(m, int)])

print(ANSWER_P1)
print(ANSWER_P2)
