N = 110201
recipes = [3, 7]

part1 = None
part2 = None

e1, e2 = 0, 1
time = 0
while part1 is None or part2 is None:
    if not time % 1000000000:
        print(time)
    recipes += list(map(int, list(str(recipes[e1]+recipes[e2]))))
    e1 += recipes[e1] + 1
    e2 += recipes[e2] + 1
    e1 %= len(recipes)
    e2 %= len(recipes)
    if not time % 1000000:
        if part1 is None and len(recipes) - 12 > int(N):
            part1 = "".join(map(str, recipes[int(N):int(N)+10]))
        if part2 is None and str(N) in "".join(map(str, recipes)):
            part2 = "".join(map(str, recipes)).find(str(N))
    time += 1

print(part1)
print(part2)
