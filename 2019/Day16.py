PUZZLE_INPUT = input("> ")

# PART 1
def phase(input_signal, base_pat=(0,1,0,-1)):
    if isinstance(input_signal, int): input_signal = str(input_signal)
    numbers = [int(x) for x in input_signal] # 12345 -> 1, 2, 3, 4, 5

    new_numbers = []
    for n in range(1, len(input_signal)+1):
        number_total = 0
        for i in range(0, len(input_signal)):
            if n == 0: bpi = (i+1) % len(base_pat) # avoid ZeroDivisionError
            else: bpi = ((i+1) // n) % len(base_pat)
            number_total += numbers[i] * base_pat[bpi]
        new_numbers.append(int(str(number_total)[-1]))
    return "".join(map(str, new_numbers))

numbers = PUZZLE_INPUT
for x in range(0, 100):
    numbers = phase(numbers)

print(str(numbers)[:8]) # first 8 digits

# PART 2
OFFSET = int(PUZZLE_INPUT[:7])
numbers = str(PUZZLE_INPUT) * 10000

shortened = numbers[OFFSET:]
def shortphase(input_signal):
    if isinstance(input_signal, int): input_signal = str(input_signal)
    s = sum(map(int, input_signal))
    ol = ""
    for i in range(0, len(input_signal)):
        ol += str(s)[-1]
        s -= int(input_signal[i])
    return ol

for x in range(0, 100):
    shortened = shortphase(shortened)

print(str(shortened)[:8])
