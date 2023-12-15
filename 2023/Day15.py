from collections import defaultdict


def hash_algo(s: str) -> int:
    cv = 0
    for ci in map(ord, s):
        cv = ((cv + ci) * 17) % 256
    return cv


def do_cmd(s: str, d: dict[int, list[tuple[str, int]]]) -> None:
    if s[-1] == "-":
        lens = s[:-1]
        box = d[hash_algo(lens)]

        for index, (label, _) in reversed(list(enumerate(box))):
            if label == lens:
                box.pop(index)
    else:
        lens, focal_s = s.split("=")
        box = d[hash_algo(lens)]
        focal = int(focal_s)

        for index, (label, _) in enumerate(box):
            if label == lens:
                box[index] = (lens, focal)
                break
        else:
            box.append((lens, focal))


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ").split(",")

    print("Part 1:", sum(map(hash_algo, PUZZLE_INPUT)))

    lensemap: dict[int, list[tuple[str, int]]] = defaultdict(list)
    for s in PUZZLE_INPUT:
        do_cmd(s, lensemap)

    print(
        "Part 2:",
        sum(
            (box + 1) * (i + 1) * focal
            for box, lenses in lensemap.items()
            for i, (_, focal) in enumerate(lenses)
        ),
    )
