import numpy


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


lcm = numpy.int64(1)
count = numpy.int64(0)
for i, v in enumerate(BUSES):
    try:
        vi = int(v)
    except ValueError:
        continue
    else:
        while count % vi != (-i) % vi:
            count += lcm
        lcm = numpy.lcm(lcm, vi)

print(count)
