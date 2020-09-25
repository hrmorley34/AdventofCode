# Part 1
def jump1(jumps):
    j = jumps.split("\n")
    l = []
    for item in j:
        l += [int(item)]
    length = len(l)
    loc = 0
    counter = 0
    while 0 <= loc and loc < length:
        counter += 1
        jump = l[loc]
        l[loc] += 1
        loc += jump
    return(counter)

# Part 2
def jump2(jumps):
    j = jumps.split("\n")
    l = []
    for item in j:
        l += [int(item)]
    length = len(l)
    loc = 0
    counter = 0
    while 0 <= loc and loc < length:
        counter += 1
        jump = l[loc]
        if jump >= 3:
            l[loc] -= 1
        else:
            l[loc] += 1
        loc += jump
    return(counter)
