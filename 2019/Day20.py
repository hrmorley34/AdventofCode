import string
from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().splitlines(False)

# PART 1
def find_hole(maze):
    if isinstance(maze, str):
        maze = maze.splitlines(False)
    P = None
    for yp, r in enumerate(maze[3:-3], 3): # ignore door space and first/last donut row/col
        xp = r.find(" ", 3, len(r)-3)
        if xp > 0:
            P = xp + yp*1j
            break
    if P is None:
        raise ValueError("No hole")
    R = (len(maze[2]) - P.real - 1) + (len(maze) - P.imag - 1)*1j
    return (P, R)

def in_complex_range(p1, test, p2):
    return (p1.real <= test.real < p2.real) and (p1.imag <= test.imag < p2.imag)
def in_maze_range(test):
    return in_complex_range(0j, test, len(PUZZLE_INPUT)*1j + len(PUZZLE_INPUT[2]))

def get_portals(portals={}):
    # OUTER PORTALS
    z = list(zip(PUZZLE_INPUT[0], PUZZLE_INPUT[1])) # first two rows
    for i, c in enumerate(z[2:-2], 2): # ignore first/last two cols
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i + 1j)
        portals["".join(c)] = l

    z = list(zip(map(lambda x:x[0], PUZZLE_INPUT), # first two cols
                 map(lambda x:x[1], PUZZLE_INPUT)))
    for i, c in enumerate(z[2:-2], 2): # ignore first/last two rows
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(1 + i*1j)
        portals["".join(c)] = l

    z = list(zip(PUZZLE_INPUT[-2], PUZZLE_INPUT[-1])) # last two rows
    for i, c in enumerate(z[2:-2], 2): # ignore first/last two cols
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i + (len(PUZZLE_INPUT)-2)*1j)
        portals["".join(c)] = l

    z = list(zip(map(lambda x:x[-2], PUZZLE_INPUT), # last two cols
                 map(lambda x:x[-1], PUZZLE_INPUT)))
    for i, c in enumerate(z[2:-2], 2): # ignore first/last two rows
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(len(PUZZLE_INPUT[2])-2 + i*1j)
        portals["".join(c)] = l

    # INNER PORTALS
    P, R = find_hole(PUZZLE_INPUT)
    Pr, Pi, Rr, Ri = int(P.real), int(P.imag), int(R.real), int(R.imag)
    PxR, RxP = Pr + Ri*1j, Rr + Pi*1j
    
    z = list(zip(PUZZLE_INPUT[Pi], PUZZLE_INPUT[Pi+1])) # first two rows
    for i, c in enumerate(z[Pr:Rr+1], 0):
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i + P)
        portals["".join(c)] = l

    z = list(zip(map(lambda x:x[Pr], PUZZLE_INPUT), # first two cols
                 map(lambda x:x[Pr+1], PUZZLE_INPUT)))
    for i, c in enumerate(z[Pi:Ri+1], 0):
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i*1j + P)
        portals["".join(c)] = l

    z = list(zip(PUZZLE_INPUT[Ri-1], PUZZLE_INPUT[Ri])) # last two rows
    for i, c in enumerate(z[Pr:Rr+1], 0):
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i + PxR)
        portals["".join(c)] = l

    z = list(zip(map(lambda x:x[Rr-1], PUZZLE_INPUT), # last two cols
                 map(lambda x:x[Rr], PUZZLE_INPUT)))
    for i, c in enumerate(z[Pi:Ri+1], 0):
        if c[0] not in string.ascii_uppercase: continue
        if c[1] not in string.ascii_uppercase: continue
        l = portals.get("".join(c), set())
        l.add(i*1j + RxP)
        portals["".join(c)] = l

    return portals

def neighbours(p):
    nlocs = [p + adj for adj in (1+0j,1j,-1+0j,-1j) \
             if in_maze_range(p+adj)]
    return nlocs
def neighbourvalues(p):
    return [PUZZLE_INPUT[int(p.imag)][int(p.real)] for p in neighbours(p)]

def get_nodes(include_corners=True):
    nodes = set()
    for y in range(2, len(PUZZLE_INPUT)-2):
        for x in range(2, len(PUZZLE_INPUT[y])-2):
            p = x + y*1j
            if PUZZLE_INPUT[int(p.imag)][int(p.real)] != ".":
                continue
            nv = neighbourvalues(p)
            if nv.count(".") > 2:
                nodes.add(p)
            elif nv.count(".") == 2 and include_corners:
                for d in (1+0j,1j,-1+0j,-1j):
                    a, ra = p+d, p+(d*1j)
                    if in_maze_range(a) and in_maze_range(ra) \
                       and PUZZLE_INPUT[int(a.imag)][int(a.real)] == "." \
                       and PUZZLE_INPUT[int(ra.imag)][int(ra.real)] == ".":
                        nodes.add(p)
                        break
            elif nv.count(".") == 1:
                nodes.add(p) # dead-end or by door
    return nodes

def get_node_map():
    nodes:set = get_nodes(include_corners=True)
    portals:dict = get_portals()

    nodemap = {n: (None, dict()) for n in nodes}
    portalpos = set()
    for portal, pos in portals.items():
        # linked dicts (the same object)
        pt = (portal, {p:0 for p in pos})
        for p in pos:
            nodemap[p] = pt
            portalpos.add(p)

    for node, (ln,con) in nodemap.items():
        for nei in neighbours(node):
            if PUZZLE_INPUT[int(nei.imag)][int(nei.real)] == "." or nei in portalpos:
                d = node-nei
                while nei not in nodemap.keys():
                    nei -= d
                    if not in_maze_range(nei):
                        print(node, d, nodemap.keys())
                        raise ValueError
                con[nei] = abs(node-nei)
                if ln is not None or nodemap[nei][0] is not None:
                    con[nei] -= 0.5

    return nodemap

def floodfill_map(start="AA", end="ZZ"):
    if end is None: end = "ZZ"
    steps = 0
    if isinstance(end, str):
        end = list(get_portals()[end])[0]
        steps -= 0.5
    done = _floodfill_map(start=start, steps=steps)
    return done[end]
def _floodfill_map(start="AA", steps=0, done=dict(), nodemap=None):
    if start is None: start = "AA"
    if isinstance(start, str):
        start = list(get_portals()[start])[0]
        steps -= 0.5
    if nodemap is None: nodemap = get_node_map()

    if start in done.keys() and done[start] <= steps:
        return done
    done[start] = steps
    name, anodes = nodemap[start]
    for node, dist in anodes.items():
        done = _floodfill_map(start=node, steps=steps+dist, done=done, nodemap=nodemap)
    return done

print("...")
dist = floodfill_map()
print(dist)

# PART 2 - non-functional
def get_node_map_p2(depth=0):
    nodes:set = get_nodes(include_corners=True)
    portals:dict = get_portals()

    nodemap = {n: (None, dict()) for n in nodes}
    portalpos = set()
    outerportalpos = set()
    innerportalpos = set()
    aazzportalpos = set()
    for portal, pos in portals.items():
        # linked dicts (the same object)
        pt = (portal, {p:0 for p in pos})
        for p in pos:
            nodemap[p] = pt
            portalpos.add(p)
            if portal in ("AA", "ZZ"):
                aazzportalpos.add(p)
            elif in_complex_range(3+3j, p, len(PUZZLE_INPUT[2])-3 + (len(PUZZLE_INPUT)-3)*1j):
                innerportalpos.add(p)
            else:
                outerportalpos.add(p)

    for node, (ln,con) in nodemap.items():
        if (depth == 0 and ln in outerportalpos) or \
           (depth > 0 and ln in aazzportalpos): continue
        for nei in neighbours(node):
            if PUZZLE_INPUT[int(nei.imag)][int(nei.real)] == "." or nei in portalpos:
                d = node-nei
                while nei not in nodemap.keys():
                    nei -= d
                    if not in_maze_range(nei):
                        print(node, d, nodemap.keys())
                        raise ValueError
                if depth == 0 and (nodemap[nei][0] in outerportalpos):
                    continue
                if depth > 0 and (nodemap[nei][0] in aazzportalpos):
                    continue
                con[nei] = abs(node-nei)
                if ln is not None or nodemap[nei][0] is not None:
                    con[nei] -= 0.5

    return nodemap

def floodfill_map_p2(start="AA", end="ZZ", MAXDEPTH=5):
    if start is None: start = "AA"
    if end is None: end = "ZZ"
    stepsadj = 0
    if isinstance(start, str):
        start = list(get_portals()[start])[0]
        stepsadj -= 0.5
    if isinstance(end, str):
        end = list(get_portals()[end])[0]
        stepsadj -= 0.5

    nodemaps = {n: get_node_map_p2(depth=n) for n in range(0, MAXDEPTH+1)}

    done = {}
    to_do = {(start, 0): (0, None)}

    while len(to_do):
        pos, lvl = list(sorted(to_do.keys(), key=lambda x: to_do[x][0]))[0] # by shortest distance
        dist, from_ = to_do.pop((pos, lvl))
        if pos == end and lvl == 0:
            chain = []
            while from_ is not None:
                chain.append(from_)
                from_ = done[from_][1]
            return dist + stepsadj, chain
        done[(pos, lvl)] = (dist, from_)

        name, cons = nodemaps[lvl][pos+0j]
        for c, d in cons.items():
            if name is None or d != 0:
                l = lvl
            elif in_complex_range(3+3j, pos, len(PUZZLE_INPUT[2])-3 + (len(PUZZLE_INPUT)-3)*1j):
                l = lvl + 1
                if l > MAXDEPTH:
                    continue
            else:
                l = lvl - 1
                if l < 0 and c == end:
                    chain = [(pos, lvl)]
                    while from_ is not None:
                        chain.append(from_)
                        from_ = done[from_][1]
                    return dist + d + stepsadj, chain
                elif l < 0 and c == start:
                    continue
                elif l < 0:
                    l = 0
            if (c, l) in done.keys():
                continue
            to_do[(c, l)] = (dist + d, (pos, lvl))

##def _floodfill_map_p2(start="AA", steps=0, done=dict(), nodemaps=None,
##                      depth=0, MAXDEPTH=5):
##    if depth > MAXDEPTH: return done
##    if start is None: start = "AA"
##    if isinstance(start, str):
##        start = list(get_portals()[start])[0]
##        steps -= 0.5
##    if nodemaps is None:
##        nodemaps = {depth: get_node_map_p2(depth=depth)}
##    if nodemaps.get(depth) is None:
##        nodemaps[depth] = get_node_map_p2(depth=depth)
##
##    if start in done.keys() and done[start] <= steps:
##        return done
##    done[(start, depth)] = steps
##    name, anodes = nodemaps[depth][start]
##    for node, dist in anodes.items():
##        if name is None:
##            ch = 0
##        elif in_complex_range(3+3j, start, len(PUZZLE_INPUT[2])-3 + (len(PUZZLE_INPUT)-3)*1j):
##            ch = 1
##        else:
##            ch = -1
##        done = _floodfill_map_p2(start=node, steps=steps+dist, done=done,
##                                 nodemaps=nodemaps, depth=depth+ch,
##                                 MAXDEPTH=MAXDEPTH)
##    return done

print("...")
dist, chain = floodfill_map_p2()
print(dist)
print(chain)
