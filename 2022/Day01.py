from puzzle_input import puzzle_input


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    food: list[int] = [0]
    for el in PUZZLE_INPUT:
        if el == "":
            food.append(0)
        else:
            food[-1] += int(el)

    # print(f"Part 1: {max(food)}")

    food.sort()

    print(f"Part 1: {food[-1]}")
    print(f"Part 2: {sum(food[-3:])}")
