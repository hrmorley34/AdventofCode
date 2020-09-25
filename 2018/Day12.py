initial = ".##.##...#.###..#.#..##..###..##...####.#...#.##....##.#.#...#...###.........##...###.....##.##.##"
conversions = """##... => .
#...# => .
.###. => #
.##.# => #
#.... => .
..##. => #
##..# => #
.#... => #
.#.## => #
#.### => #
.#..# => .
##.#. => #
..#.. => .
.##.. => #
###.# => .
.#### => .
##### => .
#.#.. => #
...## => #
...#. => .
###.. => .
..... => .
#.#.# => .
##.## => #
#.##. => #
####. => #
#..#. => #
.#.#. => .
#..## => #
....# => .
..#.# => #
..### => ."""

def strtodict(s, start=0):
    odict = {}
    for x in range(0, len(s)):
        odict[x+start] = (s[x] == "#")
    return(odict)
def dicttostr(d, start=0, end=None):
    if start is None:
        start = min(d.keys())
    if end is None:
        end = max(d.keys())
    ostr = ""
    for x in range(start, end+1):
        if d.get(x, False):
            ostr += "#"
        else:
            ostr += "."
    return(ostr)

def update_position(d, pos):
    old_set = dicttostr(d, pos-2, pos+2)
    if conversions.get(old_set, ".") == "#":
        return(True)
    else:
        return(False)

def calculate_sum(d):
    total = 0
    for k, v in d.items():
        total += k * int(v)
    return(total)

conversions = conversions.split("\n")
conversions = list(map(lambda c: c.split(" => "), conversions))
conversions = dict(conversions)

plants = strtodict(initial)

for generation in range(0, 50000000000):
    if not (generation+1)%1000000000: print(generation+1)
    new_plants = {}
    for pos in range(min(plants.keys())-2, max(plants.keys())+3):
        n_plant = update_position(plants, pos)
        if n_plant:
            new_plants[pos] = n_plant
    plants = new_plants
print(calculate_sum(plants))
