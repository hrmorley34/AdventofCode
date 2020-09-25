from Day09 import IntcodeComputer

PUZZLE_INPUT = [int(n) for n in input("> ").split(",")]

# PART 1
def test_pos(x, y):
    i = IntcodeComputer(PUZZLE_INPUT)
    i.queue[0].append(x)
    i.queue[0].append(y)
    i.run_until_input()
    rv = i.queue[1].pop(0)
    del i
    return rv

s = []
count = 0
for x in range(0, 50):
    s.append("")
    for y in range(0, 50):
        rv = test_pos(x, y)
        s[x] += "#" if rv else "."
        count += int(bool(rv))

print("\n".join(s))
print(count)

# PART 2
x, y = 100, 100
while True:
    y += 40
    rv = test_pos(x, y)
    while not rv:
        x += 5
        rv = test_pos(x, y)

    if test_pos(x, y-50) and test_pos(x+50, y-50):
        break
print(x,y)
while True:
    y += 1
    rv = test_pos(x, y)
    while not rv:
        x += 1
        rv = test_pos(x, y)

    if test_pos(x, y-99) and test_pos(x+99, y-99):
        break
X, Y = x, y-99
print(x*10000 + Y)
