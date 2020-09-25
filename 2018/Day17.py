coordstr = """\
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"""

spring = [500, 0]
coordstr = coordstr.splitlines()
def split_coords(line):
    line = line.split(", ")
    line.sort()
    line = list(map(lambda c: c.split("="), line))
    coords = []
    for name, value in line:
        if ".." in value:
            value = list(map(int, value.split("..")))
            value[1] += 1
            value = range(*value)
        else:
            value = [int(value)]
        coords.append(value)
    return(coords)
coordbanks = list(map(split_coords, coordstr))
dims = [range(min(list(map(lambda x: min(x[0]),coordbanks))+[spring[0]+1])-1,
              max(list(map(lambda x: max(x[0]),coordbanks))+[spring[0]-1])+2),
        range(min(list(map(lambda x: min(x[1]),coordbanks))+[spring[1]+1])-1,
              max(list(map(lambda x: max(x[1]),coordbanks))+[spring[1]-1])+2)]
coordmap = {}
for y in dims[1]:
    coordmap[y] = {}
    for x in dims[0]:
        coordmap[y][x] = "."
coordmap[spring[1]][spring[0]] = "+"
for xs, ys in coordbanks:
    for x in xs:
        for y in ys:
            coordmap[y][x] = "#"

for y in dims[1]:
    for x in dims[0]:
        print(coordmap[y][x], end="")
    print()
