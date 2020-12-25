from collections import defaultdict


MAX_NUM = 20201227


def transform_iter(subject_number: int, loop_size: int, value: int = 1) -> int:
    for x in range(loop_size):
        value *= subject_number
        value %= MAX_NUM
    return value


def transform_short(subject_number: int, loop_size: int, value: int = 1) -> int:
    return value * pow(subject_number, loop_size, MAX_NUM) % MAX_NUM


transform = transform_short


REVERSE_TRANSFORM_CALCED = defaultdict(dict)


def reverse_transform_bruteforce(public_key: int, subject_number: int = 7) -> int:
    if public_key in REVERSE_TRANSFORM_CALCED[subject_number]:
        return REVERSE_TRANSFORM_CALCED[subject_number][public_key]
    else:
        x = max(REVERSE_TRANSFORM_CALCED[subject_number], default=0)
        while True:
            x += 1
            t = transform(subject_number, x)
            REVERSE_TRANSFORM_CALCED[t][subject_number] = x
            if t == public_key:
                return x


reverse_transform = reverse_transform_bruteforce


def calculate_encryption_key(loop_a: int, loop_b: int, subject_number: int = 7):
    return transform(subject_number, loop_a * loop_b)


if __name__ == "__main__":
    PUBA = int(input("> "))
    PUBB = int(input("> "))

    loopa = reverse_transform(PUBA)
    loopb = reverse_transform(PUBB)

    key = calculate_encryption_key(loopa, loopb)
    print(key)
