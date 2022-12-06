def find_distinct(stream: str, count: int) -> int | None:
    for i in range(count, len(stream)):
        if len(set(stream[i - count : i])) == count:
            return i
    return None


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")

    sop = find_distinct(PUZZLE_INPUT, 4)
    assert sop is not None
    print(f"Part 1: {sop}")

    som = find_distinct(PUZZLE_INPUT, 14)
    assert som is not None
    print(f"Part 2: {som}")
