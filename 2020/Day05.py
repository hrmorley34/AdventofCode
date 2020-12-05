from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().splitlines()


def coords(bsp: str):
    return (
        int(bsp[:7].replace("F", "0").replace("B", "1"), 2),
        int(bsp[7:].replace("L", "0").replace("R", "1"), 2),
    )


def seat_id(bsp: str):
    r, c = coords(bsp)
    return r * 8 + c


existingi = set(map(seat_id, PUZZLE_INPUT))

print(max(existingi))

for i in range(2 ** 10):
    if i - 1 in existingi and i not in existingi and i + 1 in existingi:
        print(i)
