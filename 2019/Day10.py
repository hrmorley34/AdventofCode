import collections, math

from puzzle_input import puzzle_input
PUZZLE_INPUT = puzzle_input().splitlines(False)

# PART 1
#GRID = []
positions = []
for y, line in enumerate(PUZZLE_INPUT):
    #GRID.append([])
    for x, value in enumerate(line):
        #GRID[y].append(value == "#")
        if value == "#": positions.append((x, y))

def match_lineofsight(x,y, px,py, tx,ty):
    pdx, pdy = px-x, py-y
    tdx, tdy = tx-x, ty-y
    if pdx == 0 and tdx == 0 and (pdy>0) == (tdy>0): return True
    if pdy == 0 and tdy == 0 and (pdx>0) == (tdx>0): return True
    if pdx == 0 or tdx == 0: return False
    if pdy == 0 or tdy == 0: return False

    if (pdx>0) != (tdx>0) or (pdy>0) != (tdy>0): return False

    if (pdx/tdx == pdy/tdy):
        return True
    return False

top = (None, 0)
counts = collections.Counter()
for x, y in positions:
    nonblockpositions = []
    for px, py in sorted(positions, key=lambda pos: ((pos[0]-x)**2 + (pos[1]-y)**2)**0.5)[1:]: #exclude self
        c = True
        pdx, pdy = px-x, py-y
        for tx, ty in nonblockpositions:
            if match_lineofsight(x,y, px,py, tx,ty):
                c = False
                break
        if c:
            nonblockpositions.append((px, py))
            counts[(x, y)] += 1
    if len(nonblockpositions) > top[1]:
        top = ((x, y), len(nonblockpositions))

print(top[1])

# PART 2
def atan2_nowrap(x, y):
    a = math.atan2(x, -y)
    if a < 0: a += 2*math.pi
    return a
def match_lineofsight2(x,y, px,py, tx,ty):
    pdx, pdy = x-px, y-py
    tdx, tdy = x-tx, y-ty
    return math.atan2(pdx, pdy) == math.atan2(tdx, tdy)

X, Y = top[0]
pos_set = set(positions)
pos_set.remove((X, Y))
ordered_destroy = []
print(">", len(ordered_destroy), "<-", len(pos_set))
while len(pos_set):
    nonblockrelpositions = []
    for px, py in sorted(list(pos_set), key=lambda pos: ((pos[0]-X)**2 + (pos[1]-Y)**2)**0.5):
        c = True
        pdx, pdy = px-X, py-Y
        for tdx, tdy in nonblockrelpositions:
            tx, ty = X+tdx, Y+tdy
            if match_lineofsight2(X,Y, px,py, tx,ty):
                c = False
                by = tx,ty
                break
        if c:
            pos_set.remove((px, py))
            nonblockrelpositions.append((pdx, pdy))
    ordered_destroy += [(X+dx, Y+dy) for (dx,dy) in sorted(nonblockrelpositions, key=lambda dpos: atan2_nowrap(*dpos))]
    print(">", len(ordered_destroy), "<-", len(pos_set))
    if len(ordered_destroy) > 200: break

sx, sy = ordered_destroy[199] # 200th starting at 1
print(sx*100 + sy)
