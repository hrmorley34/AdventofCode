puzzle_input = input("> ")
MIN, MAX = (int(x) for x in puzzle_input.split("-"))

# PART 1
def is_ordered(n):
    return list(str(c)) == sorted(str(c))
def has_double(n):
    return len(set(str(n))) < len(str(n))

c = MIN
matching = 0
while c <= MAX:
    if is_ordered(c):
        if has_double(c):
            matching += 1
        c += 1
    else:
        # Find too low digit, bring up
        # eg. 123172
        #  -> 123333
        pd = 0
        for i, digit in enumerate(str(c)):
            if int(digit) < pd: break
            else: pd = int(digit)
        cl = list(str(c))
        for x in range(i, len(cl)):
            cl[x] = str(pd)
        c = int("".join(cl))

print(matching)

# PART 2
def has_single_double(n): # must have at least 1 double that isn't part of a triple etc.
    for d in set(str(n)):
        if str(n).count(d) == 2: return True
    return False

c = MIN
matching = 0
while c <= MAX:
    if is_ordered(c):
        if has_single_double(c):
            matching += 1
        c += 1
    else:
        # Find too low digit, bring up
        # eg. 123172
        #  -> 123333
        pd = 0
        for i, digit in enumerate(str(c)):
            if int(digit) < pd: break
            else: pd = int(digit)
        cl = list(str(c))
        for x in range(i, len(cl)):
            cl[x] = str(pd)
        c = int("".join(cl))

print(matching)
