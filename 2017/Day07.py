# Part 1

# TOP (weight)
# STACK (weight) -> THE, ONES, ON, TOP

def splitline(line):
    ia = line.index(" ")
    a = line[:ia]
    ib = line.index(")", ia+1)
    b = line[ia+2:ib]
    afterb = bool(line[ib+1:])
    if afterb:
        ontop = line[line.index("->")+3:].split(", ")
    else:
        ontop = None
    return(a, b, ontop)

def getall(lines):
    unlayered = {}
    layers = {}
    clayer = 0
    print(clayer, len(unlayered))
    layers[clayer] = {}
    for x in lines.split("\n"):
        l = splitline(x)
        if l[2]:
            unlayered[l[0]] = l[1:]
        else:
            layers[clayer][l[0]] = l[1:]
    while len(unlayered) > 1:
        clayer += 1
        print(clayer, len(unlayered))
        layers[clayer] = {}
        unlayered2 = {}
        for l in unlayered:
            output = True
            for x in unlayered[l][1]:
                moutput = False
                for layer in range(0,clayer):
                    if x in layers[layer]:
                        moutput = True
                if not moutput:
                    output = False
                    break
            if output:
                layers[clayer][l] = unlayered[l]
            else:
                unlayered2[l] = unlayered[l]
        unlayered = dict(unlayered2)
    return(unlayered, layers)

# Part 2
def getchildfromname(child, layers):
    for l in layers:
        if child in layers[l].keys():
            return(layers[l][child], l)
    return(None)

def odd_one_out(weights, children):
    c_w = list(zip(children, weights))
    counts = {}
    for n in range(0,len(weights)):
        w = weights[n]
        try:
            counts[w] += 1
        except KeyError:
            counts[w] = 1
    for n in counts:
        if counts[n] == 1:
            return(c_w.pop(weights.index(n)), c_w)
    return(None)

def checktotalweights(lines):
    unlayered, layers = getall(lines)
    layers[len(layers)] = unlayered
    for clayer in range(1, len(layers)): # Miss out layer 0 - no children
        print(clayer)
        oldlayer = layers[clayer]
        newlayer = {}
        for name in oldlayer:
            weight, children = oldlayer[name]
            try:
                weight = [int(weight)]
            except TypeError:
                weight = [sum(weight)]
            for child in children:
                try:
                    weight += [int(getchildfromname(child, layers)[0][0])]
                except TypeError:
                    weight += [sum(getchildfromname(child, layers)[0][0])]
            newlayer[name] = weight, children
            if odd_one_out(weight[1:], children) is not None:
                return(odd_one_out(weight[1:], children)+ (layers,))
        layers[clayer] = newlayer
    return(None)

def fix_weight(bad, goods, layers):
    bad_c = getchildfromname(bad[0], layers)
    print(bad, goods, bad_c)
    return(goods[0][1] - sum(bad_c[0][0][1:]))
