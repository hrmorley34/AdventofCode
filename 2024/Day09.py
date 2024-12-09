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
    REGIONS = list(get_regions(PUZZLE_INPUT))

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

    i = len(REGIONS)
    while i > 0:
        i -= 1
        if isinstance(REGIONS[i], File):
            for swap_index in range(i):
                if REGIONS[swap_index].size >= REGIONS[i].size and not isinstance(
                    REGIONS[swap_index], File
                ):
                    break  # find first empty space
            else:
                continue  # nowhere to swap into
            if swap_index >= i:
                continue
            extra_space = REGIONS[swap_index].size - REGIONS[i].size
            REGIONS[i], REGIONS[swap_index] = REGIONS[swap_index], REGIONS[i]
            if extra_space:
                REGIONS[i].size -= extra_space  # the now-empty region
                REGIONS.insert(swap_index + 1, Region(extra_space))
                i += 1  # handle the existance of an extra region below
    print(
        "Part 2:",
        sum(
            i * f
            for i, f in enumerate(
                chain.from_iterable(
                    ([r.id if isinstance(r, File) else None] * r.size) for r in REGIONS
                )
            )
            if f is not None
        ),
    )
