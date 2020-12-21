from puzzle_input import puzzle_input
from collections import defaultdict, Counter
import re


class NullAdd:
    def __init__(self):
        pass

    def __and__(self, o):
        return o


RE_LINE = re.compile(r"^(\w+(?: \w+)*) \(contains (\w+(?:, \w+)*)\)$")


def match_lines(text: str):
    allergens = defaultdict(NullAdd)
    ingredoccur = Counter()
    for line in text.splitlines():
        m = RE_LINE.match(line)
        ings, alls = m[1].split(" "), m[2].split(", ")
        for i in ings:
            ingredoccur[i] += 1
        for i in alls:
            allergens[i] &= set(ings)

    matched = match_allergens(allergens)
    nonallergen = set(ingredoccur.keys()) - set(matched.values())

    return sum(ingredoccur[a] for a in nonallergen), (allergens, matched, nonallergen)


def match_allergens(ingred: dict):
    ingred = dict(ingred)
    matched = dict()
    while len(ingred):
        for ing in [k for k, v in ingred.items() if len(v) == 1]:
            s = ingred.pop(ing)
            allergen = list(s)[0]
            matched[ing] = allergen
        for ingset in ingred.values():
            ingset -= set(matched.values())
    return matched


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    # PUZZLE_INPUT = "\n".join(input().splitlines())  # for hydrogen

    safecount, (_, matched, _) = match_lines(PUZZLE_INPUT)
    print(safecount)
    print(",".join([matched[k] for k in sorted(matched.keys())]))
