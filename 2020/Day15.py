def instances(array: list[int], obj: int) -> list[int]:
    out: list[int] = []
    try:
        index = array.index(obj)
        while index >= 0:
            out.append(index)
            index = array.index(obj, index + 1)
    except ValueError:
        pass
    return out


PUZZLE_INPUT = [int(x) for x in input("> ").split(",")]

lastindexes = {i: instances(PUZZLE_INPUT, i)[-1] + 1 for i in set(PUZZLE_INPUT[:-1])}
index = len(PUZZLE_INPUT)
num = PUZZLE_INPUT[-1]
while index <= 30000000:
    if num not in lastindexes:
        new_num = 0
    else:
        new_num = index - lastindexes[num]
    lastindexes[num] = index
    if index in (2020, 30000000):
        print(index, num)
    index += 1
    num = new_num
