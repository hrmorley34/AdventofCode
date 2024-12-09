from collections.abc import Iterator
from dataclasses import dataclass
from itertools import chain, count


@dataclass
class Region:
    size: int


@dataclass
class File(Region):
    id: int


def get_regions(s: str) -> Iterator[Region]:
    is_file = True
    ids = count(0)
    for c in s:
        size = int(c)
        if size:
            if is_file:
                yield File(size=size, id=next(ids))
            else:
                yield Region(size=size)
        is_file = not is_file


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    REGIONS = get_regions(PUZZLE_INPUT)
    BLOCKS = list(
        chain.from_iterable(
            ([r.id if isinstance(r, File) else None] * r.size) for r in REGIONS
        )
    )
    for i in range(len(BLOCKS) - 1, -1, -1):
        if BLOCKS[i] is not None:
            swap_index = BLOCKS.index(None)  # find first empty space
            if swap_index >= i:
                break
            BLOCKS[i], BLOCKS[swap_index] = BLOCKS[swap_index], BLOCKS[i]
    print("Part 1:", sum(i * f for i, f in enumerate(BLOCKS) if f is not None))
