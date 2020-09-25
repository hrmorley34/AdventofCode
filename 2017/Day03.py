import math

## Part 1
def taketo1from(n):
    x, y = getlocinspiral(n)
    total = abs(x)+abs(y)
    return(total)

def halfwayinbetween(u, d):
    m = min(u, d)
    x = (max(u, d) - m) / 2
    return(x+m)

def getlocinspiral(n):
    sqrt = math.sqrt(n)
    if sqrt == int(sqrt): # sqrt is whole
        if sqrt % 2: # sqrt is odd
            x = (sqrt - 1) / 2
            return(x, -x)
        else: # sqrt is even
            x = sqrt / 2 -1
            return(-x, x+1)
    else:
        us, ds = int(sqrt) + 1, int(sqrt)
        u, d = us**2, ds**2
        ud, dd = abs(n - u), abs(n - d)
        ul, dl = getlocinspiral(u), getlocinspiral(d)
        hwib = halfwayinbetween(u, d)
        
        if ds % 2: # ds is odd, us is even
            tm = +1
        else: # ds is even, us is odd
            tm = -1
        if n < hwib:
            x, y = dl
            x += tm # * 1
            y += (dd-1) * tm
        else:
            x, y = ul
            x += ud * tm # y stays the same
        return(x, y)

## Part 2
def adjsqus(x,y):
    for xc in range(-1, 2):
        for yc in range(-1, 2):
            yield(xc+x, yc+y)

def adjnmorethan(n):
    store = {(0, 0):1}
    s = 0
    count = 1 # miss out 1: already in dict
    while s <= n:
        count += 1
        s = 0
        cx, cy = getlocinspiral(count)
        for squ in adjsqus(cx, cy):
            try:
                s += store[squ]
            except KeyError:
                s += 0
        store[(cx, cy)] = s
    return(s, store)
