# PART 2
import asyncio

PUZZLE_INPUT = input("> ")

async def async_run_thruster(arglist, output=[]):
    memory = [int(x) for x in PUZZLE_INPUT.split(",")]
    
    INPUT_QUEUE = arglist
    OUTPUT_QUEUE = output

    def mode_mem(pointer, mode):
        if mode == 0:  return memory[int(pointer)] # location specified
        else: return int(pointer) # direct number

    pointer = 0
    while True:
        operation = memory[pointer]
        modes, opcode = [int(x) for x in str(operation)[-3::-1]], int(str(operation)[-2:])
        while len(modes) < 3: modes.append(0)
        if opcode == 1: # +
            memory[mode_mem(pointer+3, 0)] = \
                                      memory[mode_mem(pointer+1, modes[0])] \
                                    + memory[mode_mem(pointer+2, modes[1])]
            pointer += 4
        elif opcode == 2: # *
            memory[mode_mem(pointer+3, 0)] = \
                                      memory[mode_mem(pointer+1, modes[0])] \
                                    * memory[mode_mem(pointer+2, modes[1])]
            pointer += 4
        elif opcode == 3: # IN
            while len(INPUT_QUEUE) == 0:
                await asyncio.sleep(0.1) # wait until an input is given
            memory[mode_mem(pointer+1, 0)] = int(INPUT_QUEUE.pop(0))
            #memory[mode_mem(pointer+1, 0)] = int(input("- "))
            pointer += 2
        elif opcode == 4: # OUT
            #print(memory[mode_mem(pointer+1, modes[0])])
            OUTPUT_QUEUE.append(memory[mode_mem(pointer+1, modes[0])])
            pointer += 2
        elif opcode == 5: # jump if non-zero
            if memory[mode_mem(pointer+1, modes[0])] != 0:
                pointer = memory[mode_mem(pointer+2, modes[1])]
            else:
                pointer += 3
        elif opcode == 6: # jump if zero
            if memory[mode_mem(pointer+1, modes[0])] == 0:
                pointer = memory[mode_mem(pointer+2, modes[1])]
            else:
                pointer += 3
        elif opcode == 7: # <
            if memory[mode_mem(pointer+1, modes[0])] \
               < memory[mode_mem(pointer+2, modes[1])]:
                memory[mode_mem(pointer+3, 0)] = 1
            else:
                memory[mode_mem(pointer+3, 0)] = 0
            pointer += 4
        elif opcode == 8: # ==
            if memory[mode_mem(pointer+1, modes[0])] \
               == memory[mode_mem(pointer+2, modes[1])]:
                memory[mode_mem(pointer+3, 0)] = 1
            else:
                memory[mode_mem(pointer+3, 0)] = 0
            pointer += 4
        elif opcode == 99:
            #print("Finished")
            break
        else:
            raise Exception(f"Unknown opcode at {pointer}: {opcode}")
    return OUTPUT_QUEUE

async def run_feedback(t1, t2, t3, t4, t5):
    l51, l12, l23, l34, l45 = [t1, 0], [t2], [t3], [t4], [t5]
    task1 = async_run_thruster(l51, l12)
    task2 = async_run_thruster(l12, l23)
    task3 = async_run_thruster(l23, l34)
    task4 = async_run_thruster(l34, l45)
    task5 = async_run_thruster(l45, l51)
    await asyncio.gather(task1, task2, task3, task4, task5)
    return l51

top = (0, None)
for t1 in range(5, 10): # 5-9
    for t2 in range(5, 10):
        if t2 == t1: continue # only used once each
        for t3 in range(5, 10):
            if t3 in (t1, t2): continue
            for t4 in range(5, 10):
                if t4 in (t1, t2, t3): continue
                for t5 in range(5, 10):
                    if t5 in (t1, t2, t3, t4): continue
                    output = asyncio.run(run_feedback(t1, t2, t3, t4, t5))
                    if output[0] > top[0]:
                        top = (output[0], (t1,t2,t3,t4,t5))
print(top[0])
