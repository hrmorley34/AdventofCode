# PART 1
PUZZLE_INPUT = input("> ")
memory = [int(x) for x in PUZZLE_INPUT.split(",")]


memory[1] = 12
memory[2] = 2

pointer = 0
while True:
    opcode = memory[pointer]
    if opcode == 1:
        memory[memory[pointer+3]] = memory[memory[pointer+1]] + memory[memory[pointer+2]]
        # memory at location specified in memory after opcode
        pointer += 4
    elif opcode == 2:
        memory[memory[pointer+3]] = memory[memory[pointer+1]] * memory[memory[pointer+2]]
        pointer += 4
    elif opcode == 99:
        print("Finished")
        break
    else:
        raise Exception(f"Unknown opcode at {pointer}: {opcode}")
print(memory[0])

# PART 2
i, j = 0, 0
trylimit = 10
ptrylimit = 0
while memory[0] != 19690720:
    memory = [int(x) for x in PUZZLE_INPUT.split(",")]

    memory[1] = noun = i
    memory[2] = verb = j

    pointer = 0
    while True:
        opcode = memory[pointer]
        if opcode == 1:
            memory[memory[pointer+3]] = memory[memory[pointer+1]] + memory[memory[pointer+2]]
            # memory at location specified in memory after opcode
            pointer += 4
        elif opcode == 2:
            memory[memory[pointer+3]] = memory[memory[pointer+1]] * memory[memory[pointer+2]]
            pointer += 4
        elif opcode == 99:
            break
        else:
            raise Exception(f"Unknown opcode at {pointer}: {opcode}")
    i += 1
    if i >= trylimit:
        j += 1
        i = ptrylimit if j < ptrylimit else 0
    if j >= trylimit:
        j = 0
        i = ptrylimit = trylimit
        trylimit += 10
    
print(100*noun + verb)
