serial = 9221

def get_power(position):
    rackID = position[0] + 10
    power = rackID * position[1]
    power += serial
    power *= rackID
    power = (abs(power)%1000)//100
    power -= 5
    return(power)

def get_square(tlpos, size=3):
    total = 0
    for x in range(tlpos[0], tlpos[0]+size):
        for y in range(tlpos[1], tlpos[1]+size):
            total += grid[x][y]
    return(total)

grid = {}

for row in range(1, 301):
    grid[row] = {}
    for col in range(1, 301):
        grid[row][col] = get_power((row, col))

top_sqr = (None, float("-inf"))

for row in range(1, 299):
    print("Row {r}".format(r=row))
    for col in range(1, 299):
        for size in range(1, 301-max((row, col))):
            c_squ = get_square((row, col), size)
            if top_sqr[1] < c_squ:
                top_sqr = ((row, col, size), c_squ)

print(",".join(map(str, top_sqr[0])))
