def parseline(line):
    print(line)
    l, ifs = line.split(" if ")
    l = l.split(" ")
    add = int(l[2])
    if l[1] == "dec":
        add *= -1
    return(l[0], add, ifs)

def parseif(ifs, var):
    ifsp = ifs.split(" ")
    iffs = "var['"+ifsp[0]+"'] " + (" ".join(ifsp[1:]))
    try:
        ifsout = eval(iffs)
    except KeyError:
        ifsout = eval("0 " + (" ".join(ifsp[1:])))
    return(ifsout)

def parseall(lines):
    var = {}
    highestvalue = 0
    for l in lines.split("\n"):
        n, add, ifs = parseline(l)
        ifsout = parseif(ifs, var)
        if not ifsout:
            add = 0
        try:
            var[n] += add
        except KeyError:
            var[n] = add
        if var[n] > highestvalue:
            highestvalue = var[n]
    return(var, highestvalue)
