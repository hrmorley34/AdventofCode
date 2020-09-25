from Day10 import get_hash

key = "ljoxqyyw"

def get_used_squares(key):
    out = ""
    for x in range(0, 128):
        c_key = "{key}-{x}".format(key=key, x=x)
        c_hash = get_hash(c_key)
        c_hash_bin = "{:0128b}".format(int(c_hash, 16))
        out += c_hash_bin.replace("0",".").replace("1","#") + "\n"
    return(out.strip(), out.count("#"))

def make_zones(mp):
    mp_list = list(map(list, mp.split("\n")))
    regions = {0:[]}
    for x in range(0, 128):
        for y in range(0, 128):
            if mp_list[x][y] != ".":
                added = False
                for k in regions:
                    if x>0 and mp_list[x-1][y] != "." and (x-1, y) in regions[k]:
                        regions[k].append((x, y))
                        added = True
                        if y>0 and mp_list[x][y-1] != ".":
                            for k2 in regions:
                                if k2 != k and (x, y-1) in regions[k2]:
                                    regions[k] += regions.pop(k2)
                                    break
                        break
                    elif y>0 and mp_list[x][y-1] != "." and (x, y-1) in regions[k]:
                        regions[k].append((x, y))
                        added = True
                if not added:
                    for n in range(1, max(regions.keys())+2):
                        if not n in regions.keys():
                            regions[n] = [(x,y)]
                            break
    return(regions)

if __name__ == "__main__":
    mp, used = get_used_squares(key)
    zones = make_zones(mp)
