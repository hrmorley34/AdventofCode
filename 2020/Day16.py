import re
from puzzle_input import puzzle_input


class MatchRange:
    def __init__(self, start: int, end: int):
        self.startends = [(int(start), int(end))]

    def __repr__(self):
        return "MatchRange({})".format(
            ", ".join("{}-{}".format(a, b) for a, b in self.startends)
        )

    def append(self, start: int, end: int):
        self.startends.append((int(start), int(end)))

    def extend(self, r: "MatchRange"):
        self.startends.extend(r.startends)

    def match(self, number: int):
        n = int(number)
        for s, e in self.startends:
            if int(s) <= n <= int(e):
                return True
        return False


PUZZLE_INPUT = puzzle_input()
FIELDS, MY, NEARBY = PUZZLE_INPUT.split("\n\n")
RE_FIELD = re.compile(r"([a-z ]+): ((\d+-\d+)( or \d+-\d+)*)")
my_ticket = tuple(map(int, MY.splitlines()[1].split(",")))


classes = {}
for c in FIELDS.splitlines():
    m = RE_FIELD.match(c)
    classes[m[1]] = MatchRange(*m[3].split("-"))
    for m2 in m[2].split(" or ")[1:]:
        classes[m[1]].append(*m2.split("-"))


total = 0
filtered_tickets = []
for ticket in NEARBY.splitlines()[1:]:
    invalid = False
    for i in ticket.split(","):
        if any(mr.match(int(i)) for mr in classes.values()):
            continue
        else:
            total += int(i)
            invalid = True
    if not invalid:
        filtered_tickets.append(tuple(map(int, ticket.split(","))))

print(total)


VALID_FIELDS = [set(classes.keys()) for x in range(len(my_ticket))]
for ticket in filtered_tickets:
    for tf, vf in zip(ticket, VALID_FIELDS):
        for v in set(vf):
            if not classes[v].match(tf):
                vf.remove(v)

for x in range(5):  # just n case
    for f in VALID_FIELDS:
        if len(f) == 1:
            for s in VALID_FIELDS:
                if s is not f:
                    s -= f

VALID_FIELDS = tuple(
    field.pop() if len(field) == 1 else field for field in VALID_FIELDS
)
# print(VALID_FIELDS)

DF = {}
mult = 1
for i, v in enumerate(VALID_FIELDS):
    if isinstance(v, str) and v.startswith("departure"):
        DF[v] = my_ticket[i]
        mult *= my_ticket[i]

# print(DF)
print(mult)
