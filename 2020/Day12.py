from puzzle_input import puzzle_input


DIRECTIONS = {"N": 1j, "E": 0j + 1, "S": -1j, "W": 0j - 1}


class Ship:
    direction: complex = DIRECTIONS["E"]
    position: complex = 0j

    def __init__(self):
        pass

    def move(self, command):
        cmd, num = command[0].upper(), int(command[1:])
        if cmd in DIRECTIONS:
            self.position += DIRECTIONS[cmd] * num
        elif cmd in "RL":
            rotations = num / 90
            if cmd == "R":
                rotations = -rotations
            self.direction *= 1j ** rotations
        elif cmd == "F":
            self.position += num * self.direction
        else:
            print(f"Unrecognised command: {command}")

    @property
    def manhattan_distance(self):
        return abs(self.position.real) + abs(self.position.imag)


class RelativeWaypointShip(Ship):
    rwaypointpos: complex = 10 + 1j

    def move(self, command):
        cmd, num = command[0].upper(), int(command[1:])
        if cmd in DIRECTIONS:
            self.rwaypointpos += DIRECTIONS[cmd] * num
        elif cmd in "RL":
            rotations = num / 90
            if cmd == "R":
                rotations = -rotations
            self.rwaypointpos *= 1j ** rotations
        elif cmd == "F":
            self.position += num * self.rwaypointpos
        else:
            print(f"Unrecognised command: {command}")


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    s1 = Ship()
    s2 = RelativeWaypointShip()
    for cmd in PUZZLE_INPUT:
        s1.move(cmd)
        s2.move(cmd)
    print(int(s1.manhattan_distance))
    print(int(s2.manhattan_distance))
