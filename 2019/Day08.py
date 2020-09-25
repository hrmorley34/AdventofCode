puzzle_input = [int(x) for x in input("> ")]
width, height = int(input("w> ")), int(input("h> "))

# PART 1
layers = []
layerng = []
for x in range(0, len(puzzle_input), width*height):
    rows = []
    for y in range(0, width*height, width):
        rows.append(puzzle_input[x+y:x+y+width])
    layers.append(rows)
    layerng.append(puzzle_input[x:x+width*height])

least_0_layerng = sorted(layerng, key=lambda l: l.count(0))[0]
print(least_0_layerng.count(1) * least_0_layerng.count(2))

# PART 2
merge_layer = []
for y in range(0, height):
    merge_layer.append([])
    for x in range(0, width):
        for l in layers:
            if l[y][x] != 2:
                merge_layer[y].append(l[y][x])
                break

as_image = "\n".join(["".join([("O" if i==1 else " ") for i in r]) for r in merge_layer])
print(as_image)
