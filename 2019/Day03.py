WIRE_A = input("> ").split(",")
WIRE_B = input("> ").split(",")

# PART 1
grid = {0: {0: 0}}

crossings = []

def grid_or(x, y, n):
    grid[x] = grid.get(x, {})
    if grid[x].get(y, 0):
        crossings.append((x, y)) # global
    grid[x][y] = grid[x].get(y, 0) | n

for i, wire in enumerate((WIRE_A, WIRE_B)):
    pos = [0, 0]
    grid_or(*pos, 2**i)
    for direction, *digits in wire:
        number = int("".join(digits))
        if direction == "U":
            for y in range(pos[1]+1, pos[1]+number+1):
                grid_or(pos[0], y, 2**i)
            pos[1] = y
        if direction == "D":
            for y in range(pos[1]-1, pos[1]-number-1, -1):
                grid_or(pos[0], y, 2**i)
            pos[1] = y
        if direction == "L":
            for x in range(pos[0]-1, pos[0]-number-1, -1):
                grid_or(x, pos[1], 2**i)
            pos[0] = x
        if direction == "R":
            for x in range(pos[0]+1, pos[0]+number+1):
                grid_or(x, pos[1], 2**i)
            pos[0] = x

crossings.sort(key=lambda pos:sum([abs(c) for c in pos]))

print(sum(crossings[1])) # first crossing, non-origin

def print_grid(step=1):
    for y in range(60*step, -20*step, -step):
        s = ""
        for x in range(-20*step, 60*step, step):
            v = 0
            for dx in range(0, step):
                for dy in range(0, step):
                    v |= grid.get(x+dx,{}).get(y+dy,0)
            if v == 0:s+=" "
            elif v==1:s+="a"
            elif v==2:s+="b"
            elif v==3:s+="X"
        print(s)

### PART 2 attempt 1 - NONFUNCTIONAL
##grid2 = {}
##
##crossings2 = []
##
##def grid_add(x, y, i, count):
##    grid2[x] = grid2.get(x, {})
##    if grid2[x].get(y, None) is not None:
##        crossings2.append((x, y)) # global
##    grid2[x][y] = grid2[x].get(y, {})
##    if grid2[x][y].get(i, None) is None:
##        grid2[x][y][i] = count
##        return count
##    else:
##        return grid2[x][y][i]
##
##for i, wire in enumerate((WIRE_A, WIRE_B)):
##    pos = [0, 0]
##    count = grid_add(*pos, i, 0)
##    for direction, *digits in wire:
##        number = int("".join(digits))
##        if direction == "U":
##            for y in range(pos[1]+1, pos[1]+number+1):
##                count += 1
##                count = grid_add(pos[0], y, i, count)
##            pos[1] = y
##        if direction == "D":
##            for y in range(pos[1]-1, pos[1]-number-1, -1):
##                count += 1
##                count = grid_add(pos[0], y, i, count)
##            pos[1] = y
##        if direction == "L":
##            for x in range(pos[0]-1, pos[0]-number-1, -1):
##                count += 1
##                count = grid_add(x, pos[1], i, count)
##            pos[0] = x
##        if direction == "R":
##            for x in range(pos[0]+1, pos[0]+number+1):
##                count += 1
##                count = grid_add(x, pos[1], i, count)
##            pos[0] = x
##
##crossings2.sort(key=lambda pos:sum(grid2[pos[0]][pos[1]].values()))
##
##print(sum(grid2[crossings2[1][0]][crossings2[1][1]].values())) # first crossing, non-origin
##
##def print_grid2(step=1, offset=(0, 0)):
##    for y in range(20*step+offset[1], -5*step+offset[1], -step):
##        s = ""
##        for x in range(-5*step+offset[0], 20*step+offset[0], step):
##            v = {}
##            for dx in range(0, step):
##                for dy in range(0, step):
##                    d = grid2.get(x+dx,{}).get(y+dy,{})
##                    for k in d.keys():
##                        if d[k]<v.get(k,float("inf")):v[k]=d[k]
##            s += str(sum(v.values())).rjust(3, " ")
##        print(s)
##
### PART 2 attempt 2 - NONFUNCTIONAL
##grid2a = {}
##crossings2a = []
##
##def grid_adda(x, y, i, count):
##    grid2a[x] = grid2a.get(x, {})
##    grid2a[x][y] = grid2a[x].get(y, {})
##    grid2a[x][y][i] = grid2a[x][y].get(i, count)
##    #return grid2a[x][y][i]
##    return count
##
##for i, wire in enumerate((WIRE_A, WIRE_B)):
##    pos = [0, 0]
##    count = grid_adda(*pos, i, 0)
##    for direction, *digits in wire:
##        number = int("".join(digits))
##        if direction == "U":
##            for y in range(pos[1]+1, pos[1]+number+1):
##                count += 1
##                count = grid_adda(pos[0], y, i, count)
##            pos[1] = y
##        if direction == "D":
##            for y in range(pos[1]-1, pos[1]-number-1, -1):
##                count += 1
##                count = grid_adda(pos[0], y, i, count)
##            pos[1] = y
##        if direction == "L":
##            for x in range(pos[0]-1, pos[0]-number-1, -1):
##                count += 1
##                count = grid_adda(x, pos[1], i, count)
##            pos[0] = x
##        if direction == "R":
##            for x in range(pos[0]+1, pos[0]+number+1):
##                count += 1
##                count = grid_adda(x, pos[1], i, count)
##            pos[0] = x
##
##for x in grid2a.keys():
##    for y in grid2a[x].keys():
##        if len(grid2a[x][y]) >= 2:
##            crossings2a.append((x,y))
##
##crossings2a.sort(key=lambda pos:sum(grid2[pos[0]][pos[1]].values()))
##
##print(sum(grid2[crossings2a[1][0]][crossings2a[1][1]].values())) # first crossing, non-origin

# PART 2 attempt 3 - FUNCTIONAL!!!!
grid2b_a = {}
grid2b_b = {}
crossings2b = []

for grid2b, wire in zip((grid2b_a, grid2b_b), (WIRE_A, WIRE_B)):
    pos = [0, 0]
    count = 0
    grid2b[tuple(pos)] = grid2b.get(tuple(pos), count)
    for direction, *digits in wire:
        number = int("".join(digits))
        if direction == "U":
            for y in range(pos[1]+1, pos[1]+number+1):
                count += 1
                grid2b[(pos[0], y)] = grid2b.get((pos[0], y), count)
            pos[1] = y
        if direction == "D":
            for y in range(pos[1]-1, pos[1]-number-1, -1):
                count += 1
                grid2b[(pos[0], y)] = grid2b.get((pos[0], y), count)
            pos[1] = y
        if direction == "L":
            for x in range(pos[0]-1, pos[0]-number-1, -1):
                count += 1
                grid2b[(x, pos[1])] = grid2b.get((x, pos[1]), count)
            pos[0] = x
        if direction == "R":
            for x in range(pos[0]+1, pos[0]+number+1):
                count += 1
                grid2b[(x, pos[1])] = grid2b.get((x, pos[1]), count)
            pos[0] = x

crossings2b = list(set(grid2b_a.keys()) & set(grid2b_b.keys()))
crossings2b.sort(key=lambda pos:grid2b_a[pos]+grid2b_b[pos])

pos = crossings2b[1]
print(grid2b_a[pos]+grid2b_b[pos]) # first crossing, non-origin
