import string
from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().strip().splitlines(False)
MAZE = {x+y*1j: c for (x,line) in enumerate(PUZZLE_INPUT) \
                  for (y,c) in enumerate(line)}

def locate(o):
    return [i + 1j*r.index(o) for (i,r) in enumerate(PUZZLE_INPUT) if r.find(o) != -1][0]
ENTRANCE = locate("@")
##col_keys = set()
##acc_keys = set()
##acc_doors = set()
##
def neighbours(pos):
    return [pos+p for p in (1,1j,-1,-1j) if MAZE.get(pos+p,"#") != "#"]
##def find_acc(pos, done=set()):
##    if pos in done: return set(), set(), done
##    done.add(pos)
##    if MAZE[pos] == "#":
##        return set(), set(), done
##    elif MAZE[pos] in string.ascii_uppercase:
##        return set(), {MAZE[pos]}, done
##    elif MAZE[pos] in string.ascii_lowercase:
##        ak, ad = {MAZE[pos]}, set()
##    else:
##        ak, ad = set(), set()
##    for s in neighbours(pos):
##        nak, nad, done = find_acc(s, done)
##        ak |= nak; ad |= nad
##    return ak, ad, done
def dist_to_go(pos, to):
    return abs(pos.real-to.real) + abs(pos.imag-to.imag)
##def astar(pos, to):
##    checked = {}
##    to_check = [(pos, 0, None)]
##    while True:
##        to_check.sort(key=lambda x: dist_to_go(x[0], to))
##
##        pos, d, from_ = to_check.pop(0)
##        if pos == to:
##            checked[pos] = (from_, d)
##            break
##        nbrs = neighbours(pos)
##        for n in nbrs:
##            if n not in checked.keys():
##                to_check.append((n, d+1, pos))
##        checked[pos] = (from_, d)
##    return d
##def arrangements(l, prefix=[], ends=[]):
##    if len(l) == 1 and len(ends) > 0:
##        r = []
##        for e in ends:
##            r.append(list(prefix)+list(l)+[e])
##        return r
##    elif len(l) == 1:
##        return [list(prefix)+list(l)]
##    ol = []
##    for i in range(0, len(l)):
##        ol += arrangements(l[:i]+l[i+1:], list(prefix)+[l[i]], ends)
##    return ol
##def min_all_astars(pos, tos, ends=[]):
##    orders = {}
##    minimum = ((), float("inf"))
##    for arrangement in arrangements(tos, ends=ends):
##        total = 0
##        farr = (pos, *arrangement)
##        for i in range(0, len(farr)-1):
##            total += astar(farr[i], farr[i+1])
##        orders[tuple(arrangement)] = total
##        if total < minimum[1]:
##            minimum = (arrangement, total)
##    return minimum
##
##path_length = 0
##pos = ENTRANCE
##full_arr = [pos]
##acc_keys, acc_doors, _ = find_acc(pos)
##while acc_keys:
##    openable = {c.upper() for c in acc_keys} & acc_doors
##
##    print("MA*", pos, [locate(c) for c in acc_keys], [locate(c) for c in openable])
##    arr, tot = min_all_astars(pos, [locate(c) for c in acc_keys],
##                              ends=[locate(c) for c in openable])
##    path_length += tot
##    full_arr += list(arr)
##    pos = arr[-1]
##
##    for p in [locate(c) for c in acc_keys]:
##        MAZE[p] = "."
##
##    acc_keys, acc_doors, _ = find_acc(pos)
##
##print(path_length)


def astar(pos, to, col_keys=set()):
    checked = {}
    to_check = [(pos, 0, None)]
    while True:
        to_check.sort(key=lambda x: dist_to_go(x[0], to))
        if len(to_check) <= 0:
            return None
        pos, d, from_ = to_check.pop(0)
        if pos == to:
            checked[pos] = (from_, d)
            break
        nbrs = neighbours(pos)
        for n in nbrs:
            if MAZE[n] in col_keys:
                continue
            if n not in checked.keys():
                to_check.append((n, d+1, pos))
        checked[pos] = (from_, d)
    return d
def arrangements(l, prefix=[], ends=[]):
    if len(l) == 1 and MAZE[l[0]] == MAZE[l[0]].upper() and MAZE[l[0]].lower() not in map(MAZE.__getitem__, prefix):
        raise StopIteration
    elif len(l) == 1 and len(ends) > 0:
        for e in ends:
            yield list(prefix)+list(l)+[e]
    elif len(l) == 1:
        yield list(prefix)+list(l)
    for i in range(0, len(l)):
        if MAZE[l[i]] == MAZE[l[i]].upper() and MAZE[l[i]].lower() not in map(MAZE.__getitem__, prefix):
            continue
        for arr in arrangements(l[:i]+l[i+1:], list(prefix)+[l[i]], ends):
            yield arr
def min_all_astars(pos, tos, ends=[]):
    c = 0
    minimum = ((), float("inf"))
    stoparr = None
    for arrangement in arrangements(tos, ends=ends):
        if stoparr and stoparr == arrangement[:len(stoparr)]:
            continue
        elif stoparr:
            stoparr = None
        total = 0
        farr = (pos, *arrangement)
        try:
            for i in range(0, len(farr)-1):
                total += astar(farr[i], farr[i+1])
                if total > minimum[1]:
                    stoparr = farr[:i+2]
        except TypeError:
            continue
        if total < minimum[1]:
            minimum = (arrangement, total)

        c += 1
        if c%1000 == 0: print(c)
    return minimum

l = [c for c in string.ascii_letters if c in "\n".join(PUZZLE_INPUT)]
l = [locate(c) for c in l]
arr, path_length = min_all_astars(ENTRANCE, l)
print(path_length)
