# Part 1
def sharebank(bank):
    b = bank[:]
    m = max(b)
    i = b.index(m)
    b[i] = 0
    for x in range(1, m+1):
        b[(i+x) % len(b)] += 1
    return(b)

def countbank(bank):
    b = bank.split("\t")
    for x in range(0, len(b)):
        b[x] = int(b[x])
    done = [b]
    c = b
    while True:
        c = sharebank(c)
        if c in done:
            break
        else:
            done += [c]
    return(len(done))

# Part 2
def countbanktorepeat(bank):
    b = bank.split("\t")
    for x in range(0, len(b)):
        b[x] = int(b[x])
    done = [b]
    c = b
    while True:
        c = sharebank(c)
        if c in done:
            break
        else:
            done += [c]
    a = done.index(c)
    b = len(done)
    return(b - a)
