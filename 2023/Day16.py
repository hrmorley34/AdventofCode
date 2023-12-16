from dataclasses import dataclass
from typing import NamedTuple

from puzzle_input import puzzle_input


class LightBeam(NamedTuple):
    pos: tuple[int, int]
    direction: tuple[int, int]


@dataclass
class MirrorMap:
    mirrors: list[str]

    def in_range(self, pos: tuple[int, int]) -> bool:
        return 0 <= pos[1] < len(self.mirrors) and 0 <= pos[0] < len(
            self.mirrors[pos[1]]
        )

    def get_beam_dir(
        self, pos: tuple[int, int], old_dir: tuple[int, int]
    ) -> set[tuple[int, int]]:
        (x, y), (dx, dy) = pos, old_dir
        if self.mirrors[y][x] == "\\":
            return {(+dy, +dx)}
        elif self.mirrors[y][x] == "/":
            return {(-dy, -dx)}
        elif self.mirrors[y][x] == "|":
            return {(dy, +dx), (dy, -dx)} if dx else {old_dir}
        elif self.mirrors[y][x] == "-":
            return {(+dy, dx), (-dy, dx)} if dy else {old_dir}
        else:
            assert self.mirrors[y][x] == "."
            return {old_dir}

    def energise(self, start: LightBeam) -> tuple[set[tuple[int, int]], set[LightBeam]]:
        energised: set[tuple[int, int]] = set()
        seen: set[LightBeam] = set()

        beams = [start]
        new_beams: list[LightBeam] = []

        while beams:
            for beam in beams:
                if beam in seen:
                    continue
                seen.add(beam)

                if self.in_range(beam.pos):
                    energised.add(beam.pos)
                new_pos = (
                    beam.pos[0] + beam.direction[0],
                    beam.pos[1] + beam.direction[1],
                )
                if not self.in_range(new_pos):
                    continue
                new_dirs = self.get_beam_dir(new_pos, beam.direction)
                new_beams.extend(LightBeam(new_pos, new_dir) for new_dir in new_dirs)

            beams, new_beams = new_beams, []

        return energised, seen

    def energised_print(
        self, energised: set[tuple[int, int]], seen: set[LightBeam]
    ) -> None:
        gridb = list(map(list, self.mirrors))
        gride = list(map(list, self.mirrors))
        for b in seen:
            if gridb[b.pos[1]][b.pos[0]] in "\\|/-":
                gride[b.pos[1]][b.pos[0]] = "#"
            elif gridb[b.pos[1]][b.pos[0]] in ">v<^":
                gridb[b.pos[1]][b.pos[0]] = "2"
            elif gridb[b.pos[1]][b.pos[0]] in "23456789":
                i = int(gridb[b.pos[1]][b.pos[0]]) + 1
                assert i < 10
                gridb[b.pos[1]][b.pos[0]] = str(i)
            else:
                assert gridb[b.pos[1]][b.pos[0]] == "."
                match b.direction:
                    case (1, 0):
                        gridb[b.pos[1]][b.pos[0]] = ">"
                    case (0, 1):
                        gridb[b.pos[1]][b.pos[0]] = "v"
                    case (-1, 0):
                        gridb[b.pos[1]][b.pos[0]] = "<"
                    case (0, -1):
                        gridb[b.pos[1]][b.pos[0]] = "^"
                    case t:
                        assert False, t
                gride[b.pos[1]][b.pos[0]] = "#"

        # for x, y in energised:
        #     assert gride[y][x] == "#", (x,y)

        for line in gridb:
            print("".join(line))
        print()
        for line in gride:
            print("".join(line))
        print()

    def exhaustive_energise(self) -> int:
        # This could probably be smarter.
        energ_xp = (
            len(mm.energise(LightBeam((-1, y), (1, 0)))[0])
            for y in range(0, len(self.mirrors))
        )
        energ_xn = (
            len(mm.energise(LightBeam((len(self.mirrors[0]), y), (-1, 0)))[0])
            for y in range(0, len(self.mirrors))
        )
        energ_yp = (
            len(mm.energise(LightBeam((x, -1), (0, 1)))[0])
            for x in range(0, len(self.mirrors))
        )
        energ_yn = (
            len(mm.energise(LightBeam((x, len(self.mirrors)), (0, -1)))[0])
            for x in range(0, len(self.mirrors[0]))
        )
        return max(*energ_xp, *energ_xn, *energ_yp, *energ_yn)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    mm = MirrorMap(PUZZLE_INPUT)

    e, s = mm.energise(LightBeam((-1, 0), (1, 0)))
    mm.energised_print(e, s)
    print("Part 1:", len(e))

    print("Part 2:", mm.exhaustive_energise())
