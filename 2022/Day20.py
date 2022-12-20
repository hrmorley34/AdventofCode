from puzzle_input import puzzle_input


def mix(data_in: list[int], reps: int = 1) -> list[int]:
    data = list(enumerate(data_in))
    iter_data = data.copy()
    for _ in range(reps):
        for index, item in iter_data:
            current_index = data.index((index, item))
            new_index = current_index + item
            new_index %= len(data) - 1
            data.insert(new_index, data.pop(current_index))
    return [item for _, item in data]


if __name__ == "__main__":
    PUZZLE_INPUT = [int(i) for i in puzzle_input().splitlines()]
    KEY = 811589153
    DECRYPTED = [KEY * i for i in PUZZLE_INPUT]

    mixed = mix(PUZZLE_INPUT)
    zeroi = mixed.index(0)
    groovesum = sum(mixed[(zeroi + i) % len(mixed)] for i in [1000, 2000, 3000])
    print(f"Part 1: {groovesum}")

    mixed = mix(DECRYPTED, 10)
    zeroi = mixed.index(0)
    groovesum = sum(mixed[(zeroi + i) % len(mixed)] for i in [1000, 2000, 3000])
    print(f"Part 2: {groovesum}")
