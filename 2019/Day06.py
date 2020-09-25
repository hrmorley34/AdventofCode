from puzzle_input import puzzle_input


orbits = puzzle_input().splitlines(False)
# PART 1
orbitchain = {}
for orbit in orbits:
    aaa, bbb = orbit.split(")")
    orbitchain[bbb] = aaa

total = 0
for orbitor in orbitchain.keys():
    while orbitor != "COM":
        total += 1
        orbitor = orbitchain[orbitor]

print(total)

# PART 2
you_chain = []
orbitor = "YOU"
while orbitor != "COM":
    orbitor = orbitchain[orbitor]
    you_chain.append(orbitor)

orbitor = "SAN"
total = 0
while orbitor != "COM":
    orbitor = orbitchain[orbitor]
    if orbitor in you_chain:
        total += you_chain.index(orbitor)
        break
    else:
        total += 1

print(total)
