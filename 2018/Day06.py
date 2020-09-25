coords = """118, 274
102, 101
216, 203
208, 251
309, 68
330, 93
91, 179
298, 278
201, 99
280, 272
141, 312
324, 290
41, 65
305, 311
198, 68
231, 237
164, 224
103, 189
216, 207
164, 290
151, 91
166, 250
129, 149
47, 231
249, 100
262, 175
299, 237
62, 288
228, 219
224, 76
310, 173
80, 46
312, 65
183, 158
272, 249
57, 141
331, 191
163, 359
271, 210
142, 137
349, 123
55, 268
160, 82
180, 70
231, 243
133, 353
246, 315
164, 206
229, 97
268, 94"""

coords = list(map(lambda x: list(map(int, x.split(","))), coords.split("\n")))
coordsd = {}
for c in coords:
    coordsd[max(list(coordsd.keys())+[0])+1] = c

def nearest_loc(pos):
    x, y = pos
    mhds = {}
    min_d = [[0], float("inf")]
    for c in coordsd:
        mhds[c] = abs(coordsd[c][0]-x) + abs(coordsd[c][1]-y)
        if min_d[1] > mhds[c]:
            min_d = [[c], mhds[c]]
        elif min_d[1] == mhds[c]:
            min_d[0].append(c)
    return(*min_d, mhds)

coordmap = {}
totals_map = {}
buf = 10
lt10k = []
for x in range(min(map(lambda l: l[0], coords))-buf,max(map(lambda l: l[0], coords))+buf):
    for y in range(min(map(lambda l: l[1], coords))-buf,max(map(lambda l: l[1], coords))+buf):
        nl = nearest_loc((x,y))
        if sum(nl[2].values()) < 10000:
            lt10k.append(((x,y), sum(nl[2].values())))
        coordmap[x] = coordmap.get(x, {})
        if len(nl[0]) == 1:
            if nl[1] == 0:
                coordmap[x][y] = nl[0][0]
            else:
                coordmap[x][y] = nl[0][0]
        else:
            coordmap[x][y] = 0
        if x in (min(map(lambda l: l[0], coords))-buf, max(map(lambda l: l[0], coords))+buf-1) or y in (min(map(lambda l: l[1], coords))-buf, max(map(lambda l: l[1], coords))+buf-1):
            # if at a border of finite grid, assume infinite
            totals_map[coordmap[x][y]] = float("inf")
        else:
            totals_map[coordmap[x][y]] = 1 + totals_map.get(coordmap[x][y], 0)

top_v = 0
for k in totals_map:
    if totals_map[k] != float("inf") and totals_map[k] > top_v:
        top_v = totals_map[k]

print(top_v)
print(len(lt10k))
