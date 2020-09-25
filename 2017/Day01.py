def captcha_add1(t):
    l = len(t)
    out = 0

    for x in range(0,l):
        d0 = t[(x)%l]
        d1 = t[(x+1)%l]
        if d0 == d1:
            out += int(d0)
    return(out)

def captcha_addhalf(t):
    l = len(t)
    out = 0

    for x in range(0,l):
        d0 = t[(x)%l]
        d1 = t[(x+int(l/2))%l]
        if d0 == d1:
            out += int(d0)
    return(out)
