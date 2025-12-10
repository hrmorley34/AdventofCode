import re
from dataclasses import dataclass

import numpy as np
from puzzle_input import puzzle_input
from scipy.optimize import linprog


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
        light_state = tuple([False] * len(self.indicators))
        target_state = tuple(self.indicators)
        steps_to_state = {light_state: 0}
        new_states = {light_state}
        while target_state not in steps_to_state:
            it_states = new_states.copy()
            new_states.clear()
            if not it_states:
                break
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
        return steps_to_state[target_state]

    def configure_joltage(self) -> int:
        restrictions: list[tuple[set[int], int]] = []
        for indicator, count in enumerate(self.joltage):
            buttons = [i for i, b in enumerate(self.buttons) if indicator in b]
            restrictions.append((set(buttons), count))
        A_eq = np.array(
            [[int(i in b) for b in self.buttons] for i in range(len(self.joltage))]
        )
        b_eq = np.array(self.joltage)
        soln = linprog(
            np.array([1] * len(self.buttons)), None, None, A_eq, b_eq, integrality=1
        )
        return sum(soln.x)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    machines = list(map(Machine.from_string, PUZZLE_INPUT.splitlines()))
    print("Part 1:", sum(m.configure_lights() for m in machines))
    s = sum(m.configure_joltage() for m in machines)
    p, q = s.as_integer_ratio()  # convert out of np.float64
    assert q == 1
    print("Part 2:", p)
