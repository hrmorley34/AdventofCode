from puzzle_input import puzzle_input
import collections
import re

PUZZLE_INPUT = puzzle_input().splitlines()

ROW_RE = re.compile(
    r"(\w+ \w+) bags contain ((\d+ \w+ \w+ bags?, )*\d+ \w+ \w+ bags?|(no other bags))."
)
ROWPART_RE = re.compile(r"(\d+) (\w+ \w+) bags?")

forward_map_c = collections.defaultdict(list)
reverse_map = collections.defaultdict(list)

for row in PUZZLE_INPUT:
    m = ROW_RE.match(row)
    if not m.group(4):
        for s in m.group(2).split(", "):
            m2 = ROWPART_RE.match(s)
            reverse_map[m2.group(2)].append(m.group(1))
            forward_map_c[m.group(1)].append((m2.group(2), int(m2.group(1))))


def fill_answer_set(mapping: dict, parent: str, answer_set: set = None):
    if answer_set is None:
        answer_set = set()
    answer_set.add(parent)
    for child in mapping[parent]:
        if child in answer_set:
            continue
        fill_answer_set(mapping, child, answer_set)
    return answer_set


def count_children(mapping: dict, parent: str):
    i = 1  # parent
    for child, count in mapping[parent]:
        i += (
            count_children(
                mapping,
                child,
            )
            * count
        )
    return i


s = fill_answer_set(reverse_map, "shiny gold")
print(len(s) - 1)  # exclude self

i = count_children(forward_map_c, "shiny gold")
print(i - 1)
