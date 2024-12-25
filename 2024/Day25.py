from dataclasses import dataclass

from puzzle_input import puzzle_input

PIN_MAX = 5


@dataclass(frozen=True)
class Lock:
    pins: tuple[int, ...]

    @classmethod
    def from_schematic(cls, schem: str):
        return cls(tuple(z.count("#") - 1 for z in zip(*schem.splitlines())))


@dataclass(frozen=True)
class Key:
    pins: tuple[int, ...]

    @classmethod
    def from_schematic(cls, schem: str):
        return cls(tuple(z.count("#") - 1 for z in zip(*schem.splitlines())))


def from_schematic(schem: str) -> Lock | Key:
    if schem[0] == "#":
        return Lock.from_schematic(schem)
    else:
        return Key.from_schematic(schem)


def test_lock(lock: Lock, key: Key) -> bool:
    return all(a + b <= PIN_MAX for a, b in zip(lock.pins, key.pins))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    DEVS = list(map(from_schematic, PUZZLE_INPUT))
    LOCKS: list[Lock] = []
    KEYS: list[Key] = []
    for d in DEVS:
        if isinstance(d, Lock):
            LOCKS.append(d)
        else:
            KEYS.append(d)

    print("Part 1:", sum(test_lock(lock, key) for lock in LOCKS for key in KEYS))
