import collections
import math
import re
from puzzle_input import puzzle_input

PUZZLE_INPUT = puzzle_input().splitlines(False)

# PART 1
ARG_MATCH = re.compile("^(\d+) ([A-Z]+)$")
rev_conversions = {}
for line in PUZZLE_INPUT:
    match = re.match(r"^(\d+ [A-Z]+(, \d+ [A-Z]+)*) => (\d+ [A-Z]+)$", line)
    ins, out = match.group(1).split(", "), match.group(3)
    outm = ARG_MATCH.match(out)
    ins = {m.group(2): int(m.group(1)) for m in map(ARG_MATCH.match, ins)}
    outmat, outtot = outm.group(2), int(outm.group(1))
    rev_conversions[outmat] = (outtot, ins)

def treedepth(material):
    m = 0
    for k in rev_conversions.get(material, ("",{}))[1].keys():
        if k == "ORE": continue
        x = treedepth(k) + 1
        if x > m: m = x
    return m

def get_total_ore(material, count=1):
    requirements = collections.Counter({"FUEL": count})
    done = {"ORE"}
    while len(set(requirements.keys())-done):
        mat = sorted(set(requirements.keys())-done, key=treedepth, reverse=True)[0]
        vol = requirements[mat]
        done.add(mat)
        
        prod, needs = rev_conversions[mat]
        upfactor = math.ceil(vol / prod)
        for m, c in needs.items():
            requirements[m] += c * upfactor
            #print("{0:5} += {1:2} * {2:2} (= {3:3} -> {4:5})".format(
            #    m, c, upfactor, c * upfactor, requirements[m]))
    return requirements

requirements = get_total_ore("FUEL")
print(requirements["ORE"])

# PART 2
MAX_ORE = 10**12

x = 10**1
while get_total_ore("FUEL", x)["ORE"] < MAX_ORE:
    x *= 10
while get_total_ore("FUEL", x)["ORE"] > MAX_ORE:
    x -= 10**8
while get_total_ore("FUEL", x)["ORE"] < MAX_ORE:
    x += 10**7
while get_total_ore("FUEL", x)["ORE"] > MAX_ORE:
    x -= 10**6
while get_total_ore("FUEL", x)["ORE"] < MAX_ORE:
    x += 10**5
while get_total_ore("FUEL", x)["ORE"] > MAX_ORE:
    x -= 10**4
while get_total_ore("FUEL", x)["ORE"] < MAX_ORE:
    x += 10**3
while get_total_ore("FUEL", x)["ORE"] > MAX_ORE:
    x -= 10**2
while get_total_ore("FUEL", x)["ORE"] < MAX_ORE:
    x += 10**1
while get_total_ore("FUEL", x)["ORE"] > MAX_ORE:
    x -= 10**0

print(x)
