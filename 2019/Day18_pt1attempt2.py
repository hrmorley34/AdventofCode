import string
from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().strip()
MAZE = {x+y*1j: c for (x,line) in enumerate(PUZZLE_INPUT.splitlines(False)) \
                  for (y,c) in enumerate(line)}

def locate(o):
    if isinstance(o, complex): return o
    return [i + 1j*r.index(o) for (i,r) in enumerate(PUZZLE_INPUT.splitlines(False)) if r.find(o) != -1][0]

ENTRANCE = locate("@")

all_keys = {c for c in PUZZLE_INPUT if c in string.ascii_lowercase}
all_doors = {c for c in PUZZLE_INPUT if c in string.ascii_uppercase}

def neighbours(pos):
    return [pos+p for p in (1,1j,-1,-1j) if MAZE.get(pos+p,"#") != "#"]

def find_acc(pos, col_keys=set(), done=set()):
    if pos in done or pos not in MAZE.keys():
        return set(), set(), done
    done.add(pos)
    if MAZE[pos] == "#":
        return set(), set(), done
    elif MAZE[pos] in string.ascii_uppercase:
        ak, ad = set(), {MAZE[pos]}
        if MAZE[pos].lower() not in col_keys:
            return ak, ad, done
    elif MAZE[pos] in string.ascii_lowercase:
        ak, ad = {MAZE[pos]}, set()
    else:
        ak, ad = set(), set()
    for s in neighbours(pos):
        if MAZE[s] in string.ascii_uppercase and MAZE[s].lower() not in col_keys:
            continue
        nak, nad, done = find_acc(s, col_keys=col_keys, done=done)
        ak |= nak; ad |= nad
    return ak, ad, done

def dist_to_go(pos, to):
    return abs(pos.real-to.real) + abs(pos.imag-to.imag)

def astar(pos, to, col_keys=set()):
    checked = {}
    to_check = [(pos, 0, None)]
    while True:
        to_check.sort(key=lambda x: dist_to_go(locate(x[0]), locate(to)))
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

def arrangements(pos, tos=None, col_keys=set()):
    if tos is None:
        tos, _, _ = find_acc(pos, col_keys=col_keys)
    return _arrangements(list(tos))
def _arrangements(l, prefix=[], ends=[], col_keys=set()):
    if len(l) == 1 and l[0] == l[0].upper() \
       and l[0].lower() not in prefix:
        raise StopIteration
    elif len(l) == 1 and len(ends) > 0:
        for e in ends:
            yield list(prefix)+list(l)+[e]
    elif len(l) == 1:
        yield list(prefix)+list(l)
    for i in range(0, len(l)):
        if l[i] == l[i].upper() \
           and l[i].lower() not in prefix:
            continue
        for arr in _arrangements(l[:i]+l[i+1:], prefix=list(prefix)+[l[i]],
                                 ends=ends, col_keys=col_keys):
            yield arr

def min_all_astars(pos, col_keys=set(), i=0):
    col_keys = set(col_keys)
    if all_keys - col_keys == set():
        return ((), 0)
    ak, ad, _ = find_acc(pos, col_keys=col_keys)
    min_ = ((), float("inf"))
    tos = ak
    stoparr = None
    print(i*2*" "+str(tos), list(arrangements(pos, tos)), all_keys, col_keys, ak, ad)
    for arrangement in arrangements(pos, tos):
        if stoparr and stoparr == arrangement[:len(stoparr)]:
            continue
        elif stoparr:
            stoparr = None
        total = 0
        farr = (pos, *map(locate, arrangement))
        print(i*2*" "+str(farr), total)
        try:
            for i in range(0, len(farr)-1):
                total += astar(farr[i], farr[i+1])
                if total > min_[1]:
                    stoparr = farr[:i+2]
                    break
        except TypeError as e:
            raise
            continue
        print(i*2*" "+str(arrangement), total)
        m = min_all_astars(farr[-1], col_keys=col_keys \
                                     | set(map(MAZE.__getitem__, farr[1:])),i=i+1)
        total += m[1]
        print(i*2*" "+str(arrangement), total)
        if total < min_[1]:
            min_ = (list(arrangement)+list(m[0]), total)

    return min_

arr, tot = min_all_astars(ENTRANCE)
print(tot)
