def instances(array: list, obj: object) -> list[int]:
    out = []
    try:
        index = array.index(obj)
        while index >= 0:
            out.append(index)
            index = array.index(obj, index + 1)
    except ValueError:
        pass
    return out


PUZZLE_INPUT = [int(x) for x in input("> ").split(",")]

arr = list(PUZZLE_INPUT)
while len(arr) < 2020:
    i = arr[-1]
    if i in arr[:-1]:
        *_, index2, index1 = instances(arr, i)
        arr.append(index1 - index2)
    else:
        arr.append(0)

print(arr[2019])
