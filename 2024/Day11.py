def blink(stones: list[int]) -> list[int]:
    result: list[int] = []
    for stone in stones:
        if stone == 0:
            result.append(1)
        elif len(str(stone)) % 2 == 0:
            left = int(str(stone)[: len(str(stone)) // 2])
            right = int(str(stone)[len(str(stone)) // 2 :])
            result.extend((left, right))
        else:
            result.append(stone * 2024)
    return result


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    stones = list(map(int, PUZZLE_INPUT.split()))

    for _ in range(25):
        stones = blink(stones)
    print("Part 1:", len(stones))
