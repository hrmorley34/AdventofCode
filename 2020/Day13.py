TIME = int(input("> "))
BUSES = input("> ").split(",")


diffs = {}
top = (None, float("inf"))
for b in BUSES:
    try:
        i = int(b)
    except ValueError:
        continue
    else:
        diffs[i] = i - (TIME % i)
        if diffs[i] < top[1]:
            top = (i, diffs[i])

print(top[0], top[1], "->", top[0] * top[1])
