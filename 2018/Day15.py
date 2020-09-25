gridstr = """\
#######
#.G.E.#
#E.G.E#
#.G.E.#
#######"""
gridstr = gridstr.splitlines()

class Goblin:
    def __init__(self, pos, hp=200):
        self.hp = hp
        self.pos = pos
    def __repr__(self):
        return("G({})".format(self.hp))
    def __str__(self):
        return("G")
class Elf:
    def __init__(self, pos, hp=200):
        self.hp = hp
        self.pos = pos
    def __repr__(self):
        return("E({})".format(self.hp))
    def __str__(self):
        return("E")
class Wall:
    def __init__(self, pos):
        self.pos = pos
    def __repr__(self):
        return("#")
    def __str__(self):
        return("#")
class Space:
    def __init__(self, pos, goblins={}, elves={}):
        self.distances = [goblins, elves]
        self.pos = pos
    def __repr__(self):
        return(str(self.distances))
    def __str__(self):
        if self.pos in map(lambda p: goblins[p].pos, goblins.keys()):
            return("G")
        elif self.pos in map(lambda p: elves[p].pos, elves.keys()):
            return("E")
        return(".")

grid = []
goblins = {}
elves = {}
for row in range(0, len(gridstr)):
    grid.append([])
    for col in range(0, len(gridstr[row])):
        c_squ = gridstr[row][col]
        c_pos = (row, col)
        if c_squ == "#":
            grid[row].append(Wall(c_pos))
        elif c_squ == "G":
            goblins[max(list(goblins.keys())+[-1])+1] = Goblin(c_pos)
            grid[row].append(Space(c_pos, goblins={max(goblins.keys()):0}))
        elif c_squ == "E":
            elves[max(list(elves.keys())+[-1])+1] = Elf(c_pos)
            grid[row].append(Space(c_pos, elves={max(elves.keys()):0}))
        else:
            grid[row].append(Space(c_pos))
