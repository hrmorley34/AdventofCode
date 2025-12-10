import re
from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class Machine:
    indicators: list[bool]
    buttons: list[set[int]]
    joltage: list[int]

    @classmethod
    def from_string(cls, line: str):
        m = re.match(r"^\[([\.#]+)\] ((?:\(\d+(?:,\d+)*\) )+)\{(\d+(?:,\d+)+)\}$", line)
        assert m, line
        indic, butt, jolt = m[1], m[2], m[3]
        return cls(
            [i == "#" for i in indic],
            [set(map(int, part[1:-1].split(","))) for part in butt.split()],
            list(map(int, jolt.split(","))),
        )

    def configure_lights(self) -> int:
        light_state = [False] * len(self.indicators)
        steps_to_state = {tuple(light_state): 0}
        new_states = {tuple(light_state)}
        while tuple(self.indicators) not in steps_to_state:
            it_states = new_states.copy()
            new_states.clear()
            for state in it_states:
                steps = steps_to_state[state]
                for button in self.buttons:
                    statel = list(state)
                    for pos in button:
                        statel[pos] = not statel[pos]
                    nstate = tuple(statel)
                    if nstate in steps_to_state:
                        continue
                    steps_to_state[nstate] = steps + 1
                    new_states.add(nstate)
        return steps_to_state[tuple(self.indicators)]


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    machines = list(map(Machine.from_string, PUZZLE_INPUT.splitlines()))
    print("Part 1:", sum(m.configure_lights() for m in machines))
