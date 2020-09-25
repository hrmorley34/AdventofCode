# Part 1
def parsephrase(phrase):
    sp = phrase.lower().split()
    checked = []
    for p in sp:
        if p in checked:
            return(False)
        else:
            checked += [p]
    return(True)

def phrases(lines):
    x = 0
    for l in lines.split("\n"):
        pp = parsephrase(l.lower())
        if pp:
            x += 1
    return(x)

# Part 2
def parse2(phrase):
    sp = phrase.lower().split()
    checked = []
    for p in sp:
        sortp = list(p)
        sortp.sort()
        sortp = "".join(sortp)
        if sortp in checked:
            return(False)
        else:
            checked += [sortp]
    return(True)

def phrases2(lines):
    x = 0
    for l in lines.split("\n"):
        pp = parse2(l.lower())
        if pp:
            x += 1
    return(x)
