from numpy import array

class HexPoint:
    def __init__(self, x=0, y=0):
        self.pos = array((x, y))
    def __repr__(self):
        return(", ".join(map(str, self.pos)))
    def move(self, d):
        if d == "n":
            self.pos += array((0, +2))
        elif d == "ne":
            self.pos += array((+1, +1))
        elif d == "se":
            self.pos += array((+1, -1))
        elif d == "s":
            self.pos += array((0, -2))
        elif d == "sw":
            self.pos += array((-1, -1))
        elif d == "nw":
            self.pos += array((-1, +1))
        else:
            print("Invalid move {d}".format(d=d))
    @property
    def distance(self):
        pos = abs(self.pos) # take absolute as it doesn't matter which way you go (up/down and left/right)
        moves = 0

        moves += min(pos)
        pos -= min(pos)
        x, y = pos
        
        moves += x
        x -= x
        moves += y/2
        y -= y/2

        return(moves)
