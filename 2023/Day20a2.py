from __future__ import annotations

from abc import ABC, abstractmethod
from collections import Counter
from math import lcm

from puzzle_input import puzzle_input


class Module(ABC):
    name: str
    dest: list[str]

    def __init__(self, name: str, dests: list[str]) -> None:
        self.name = name
        self.dest = dests

    @abstractmethod
    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        raise NotImplementedError

    def add_src(self, src: str) -> None:
        pass

    def get_state(self) -> object:
        return None

    @classmethod
    def from_line(cls, line: str):
        name, dests = line.split(" -> ")
        destl = dests.split(", ")
        if name[0] == "%":
            return FlipFlop(name[1:], destl)
        elif name[0] == "&":
            return Conjunction(name[1:], destl)
        elif name == "broadcaster":
            return Broadcaster(destl)
        else:
            assert False, name


class FlipFlop(Module):
    state: bool

    def __init__(self, name: str, dests: list[str]) -> None:
        super().__init__(name, dests)
        self.state = False

    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        if not pulse:
            self.state = not self.state
            return [(d, self.state) for d in self.dest]
        return []

    def get_state(self) -> object:
        return self.state


class Conjunction(Module):
    current_state: dict[str, bool]

    def __init__(self, name: str, dests: list[str]) -> None:
        super().__init__(name, dests)
        self.current_state = {}  # needs filling

    def add_src(self, src: str) -> None:
        self.current_state[src] = False
        return super().add_src(src)

    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        assert self.current_state
        self.current_state[src] = pulse
        send_state = not all(self.current_state.values())
        return [(d, send_state) for d in self.dest]

    def get_state(self) -> object:
        return [m for m, b in self.current_state.items() if b]


class Broadcaster(Module):
    def __init__(self, dests: list[str]) -> None:
        super().__init__("broadcaster", dests)

    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        return [(d, pulse) for d in self.dest]


class Output(Module):
    def __init__(self) -> None:
        super().__init__("output", [])

    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        return []


class Rx(Module):
    edge: bool
    delivered: bool
    srces: list[str]

    def __init__(self, edge: bool = False) -> None:
        super().__init__("rx", [])
        self.edge = edge
        self.delivered = False
        self.srces = []

    def send(self, src: str, pulse: bool) -> list[tuple[str, bool]]:
        if self.edge == pulse:
            self.delivered = True
        # else:
        #     print("Anti-rx!")
        return []

    def add_src(self, src: str) -> None:
        self.srces.append(src)


def make_modules(lines: list[str]) -> dict[str, Module]:
    modules: dict[str, Module] = {m.name: m for m in map(Module.from_line, lines)}
    for m in modules.copy().values():
        for d in m.dest:
            if d == "output" and "output" not in modules:
                modules["output"] = Output()
            if d == "rx" and "rx" not in modules:
                modules["rx"] = Rx()
            modules[d].add_src(m.name)
    return modules


MSTATE = dict[str, object]
STATEL = list[tuple[int, MSTATE, int, int]]


def get_state(modules: dict[str, Module]) -> MSTATE:
    return {n: m.get_state() for n, m in modules.items()}


def check_state(st: MSTATE, states: STATEL) -> tuple[int | None, int, int]:
    for i, s, hi, lo in states:
        if st == s:
            return i, hi, lo
    return None, 0, 0


def press_button(modules: dict[str, Module]) -> tuple[int, int]:
    queue: list[tuple[str, str, bool]] = [("button", "broadcaster", False)]
    pulses: Counter[bool] = Counter()
    while queue:
        src, dest, pulse = queue.pop(0)
        new = modules[dest].send(src, pulse)
        pulses[pulse] += 1
        # print(src, pulse, "->", dest, new)
        queue.extend((dest, d, p) for d, p in new)
    return pulses[True], pulses[False]


def get_rx_delivered(modules: dict[str, Module]) -> bool:
    rx = modules.get("rx")
    if rx is None:
        return True  # part 1 case
    assert isinstance(rx, Rx)
    return rx.delivered


def get_submodule(
    modules: dict[str, Module], start: str, end: str
) -> dict[str, Module]:
    submodules: dict[str, Module] = {}
    m_checks: list[str] = [start]
    while m_checks:
        mn = m_checks.pop(0)
        if mn == end or mn in submodules:
            continue
        submodules[mn] = m = modules[mn]
        m_checks.extend(m.dest)
    return submodules


def do_by_submodules(modules: dict[str, Module]) -> int:
    BC = modules["broadcaster"]
    assert isinstance(BC, Broadcaster)
    RX = modules["rx"]
    assert isinstance(RX, Rx)
    assert len(RX.srces) == 1
    END = RX.srces[0]
    ENDM = modules[END]
    assert isinstance(ENDM, Conjunction)

    sms: dict[str, dict[str, Module]] = {}
    for d in BC.dest:
        sms[d] = sm = get_submodule(modules, d, END)
        sm["broadcaster"] = Broadcaster([d])
        sm[END] = cj = Conjunction(END, ["rx"])
        sm["rx"] = Rx()

        for m in sm.values():
            if END in m.dest:
                cj.add_src(m.name)

    time = 1
    for sm in sms.values():
        seen_states: STATEL = [(0, get_state(sm), 0, 0)]

        i = 0
        rxt = -1
        while True:
            i += 1
            press_button(sm)

            if rxt < 0 and get_rx_delivered(sm):
                rxt = i
                break

            pi, _, _ = check_state(get_state(sm), seen_states)
            if pi is not None:
                break

        time = lcm(time, i)
    return time


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    modules = make_modules(PUZZLE_INPUT)

    seen_states: STATEL = [(0, get_state(modules), 0, 0)]
    hi = lo = 0
    i = 0
    rxd = False
    while i < 1000:
        i += 1
        dhi, dlo = press_button(modules)
        hi += dhi
        lo += dlo

        pi, phi, plo = check_state(get_state(modules), seen_states)
        if pi is not None:
            di = i - pi
            dhi = hi - phi
            dlo = lo - plo
            while i < 1000 - di + 1:
                i += di
                hi += dhi
                lo += dlo

    print("Part 1:", hi * lo)

    modules = make_modules(PUZZLE_INPUT)
    print("Part 2:", do_by_submodules(modules))
