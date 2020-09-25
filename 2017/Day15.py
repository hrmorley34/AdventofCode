def gen(start, factor, modreq=1):
    v = start
    while True:
        v *= factor
        v %= 2147483647
        if v % modreq == 0:
            yield v

start = [722, 354]
factor = [16807, 48271]
modreq = [4, 8]

genA, genB = gen(start[0], factor[0], modreq[0]), gen(start[1], factor[1], modreq[1])

score = 0
for x in range(0, 5000000):
    vA, vB = next(genA), next(genB)
    if vA % (2**16) == vB % (2**16):
        score += 1
print(score)
