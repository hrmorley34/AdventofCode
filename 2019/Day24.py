from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().strip().splitlines()

GRID = {x + y*1j: PUZZLE_INPUT[y][x] for y in range(0, 5) for x in range(0, 5)}

# PART 1
def pass_minute(grid):
    newgrid = {}
    for x in range(0, 5):
        for y in range(0, 5):
            c = grid[x + y*1j]
            sur = [grid.get(x + y*1j + adj, ".") for adj in (1, 1j, -1, -1j)]
            sc = sur.count("#")
            if c == "#" and sc == 1:
                newgrid[x + y*1j] = "#"
            elif c == "." and sc in (1, 2):
                newgrid[x + y*1j] = "#"
            else:
                newgrid[x + y*1j] = "."
    return newgrid

g = GRID
ghash = "\n".join(["".join([g[x+y*1j] for x in range(0,5)]) for y in range(0,5)])
seen = set()
while ghash not in seen:
    seen.add(ghash)
    g = pass_minute(g)
    ghash = "\n".join(["".join([g[x+y*1j] for x in range(0,5)]) for y in range(0,5)])

print(ghash)

p = 0
bdr = 0
for y in range(0, 5):
    for x in range(0, 5):
        if g[x + y*1j] == "#":
            bdr += 2**p
        p += 1

print(bdr)

# PART 2
def get_adj(grids, lvl, cell):
    survals = []
    adjs = [cell+adj for adj in (1,1j,-1,-1j)]
    for c in adjs:
        if c.real in range(0,5) and c.imag in range(0,5) and c != 2+2j:
            survals.append(grids.get(lvl, {}).get(c, "."))
        elif c == 2+2j:
            if c-cell == 1+0j: line = [y*1j for y in range(0, 5)]
            elif c-cell == 1j: line = [x+0j for x in range(0, 5)]
            elif c-cell == -1+0j: line = [4+y*1j for y in range(0, 5)]
            elif c-cell == -1j: line = [x+4j for x in range(0, 5)]
            survals += [grids.get(lvl-1, {}).get(v, ".") for v in line]
        else:
            survals.append(grids.get(lvl+1, {}).get(2+2j + (c-cell), "."))
    return survals
def pass_minute_recursive(grids):
    newgrids = {}
    grids[max(grids.keys())+1] = {}
    grids[min(grids.keys())-1] = {}
    for d, grid in grids.items():
        newgrid = {}
        for x in range(0, 5):
            for y in range(0, 5):
                c = grid.get(x + y*1j, ".")
                sc = get_adj(grids, d, x + y*1j).count("#")
                if c == "#" and sc == 1:
                    newgrid[x + y*1j] = "#"
                elif c == "." and sc in (1, 2):
                    newgrid[x + y*1j] = "#"
                else:
                    newgrid[x + y*1j] = "."
        newgrid[2+2j] = "?"
        newgrids[d] = newgrid
    if "#" not in set(newgrids[max(newgrids.keys())].values()):
        del newgrids[max(newgrids.keys())]
    if "#" not in set(newgrids[min(newgrids.keys())].values()):
        del newgrids[min(newgrids.keys())]
    return newgrids

gs = {0: GRID}
for x in range(0, 200):
    gs = pass_minute_recursive(gs)

count = sum([int(v[x+y*1j]=="#") for v in gs.values() \
             for x in range(0, 5) for y in range(0, 5)])
print(count)
