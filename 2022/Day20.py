from puzzle_input import puzzle_input


def mix(data_in: list[int]) -> list[int]:
    data = list(enumerate(data_in))
    for index, item in data.copy():
        current_index = data.index((index, item))
        new_index = current_index + item
        new_index %= len(data) - 1
        data.insert(new_index, data.pop(current_index))
    return [item for _, item in data]


if __name__ == "__main__":
    PUZZLE_INPUT = [int(i) for i in puzzle_input().splitlines()]

    mixed = mix(PUZZLE_INPUT)
    zeroi = mixed.index(0)
    groovesum = sum(mixed[(zeroi + i) % len(mixed)] for i in [1000, 2000, 3000])
    print(f"Part 1: {groovesum}")
