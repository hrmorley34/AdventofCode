from puzzle_input import puzzle_input

COORD = tuple[int, int, int]
DIRECTIONS = ((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))


def get_surface(cubes: set[COORD], cube: COORD) -> int:
    return sum(
        (cube[0] + dx, cube[1] + dy, cube[2] + dz) not in cubes
        for dx, dy, dz in DIRECTIONS
    )


def recurse_hidden_area(
    cubes: set[COORD],
    cube: COORD,
    bounds: tuple[COORD, COORD],
    open_areas: set[COORD],
) -> tuple[bool, set[COORD]]:
    found: set[COORD] = {cube}
    queue: list[COORD] = [cube]
    while queue:
        item = queue.pop(0)
        for dx, dy, dz in DIRECTIONS:
            c = (item[0] + dx, item[1] + dy, item[2] + dz)
            if c in found or c in cubes:
                continue
            if (
                not all(bounds[0][i] <= c[i] <= bounds[1][i] for i in range(3))
                or c in open_areas
            ):
                return False, found
            else:
                queue.append(c)
                found.add(c)
    return True, found


def get_hidden_areas(cubes: set[COORD]) -> set[COORD]:
    BOUNDS = (
        min((c[0] for c in cubes)),
        min((c[1] for c in cubes)),
        min((c[2] for c in cubes)),
    ), (
        max((c[0] for c in cubes)),
        max((c[1] for c in cubes)),
        max((c[2] for c in cubes)),
    )
    closed_areas: set[COORD] = set()
    open_areas: set[COORD] = set()
    for cube in cubes:
        for dx, dy, dz in DIRECTIONS:
            c = (cube[0] + dx, cube[1] + dy, cube[2] + dz)
            if c in cubes or c in closed_areas or c in open_areas:
                continue
            b, s = recurse_hidden_area(cubes, c, BOUNDS, open_areas)
            if b:
                closed_areas.update(s)
            else:
                open_areas.update(s)
    return closed_areas


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    CUBES = {
        (int(x), int(y), int(z)) for x, y, z in (s.split(",") for s in PUZZLE_INPUT)
    }

    surface = sum(get_surface(CUBES, c) for c in CUBES)
    print(f"Part 1: {surface}")

    HIDDEN_CUBES = get_hidden_areas(CUBES)
    hidden_surface = sum(get_surface(HIDDEN_CUBES, c) for c in HIDDEN_CUBES)
    print(f"Part 2: {surface - hidden_surface}")
