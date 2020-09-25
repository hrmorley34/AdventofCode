n_players = 455
max_points = 71223

max_points *= 100 # part 2

c_list = [0]
c_pos = 0
c_elf = 1
marbles = range(1, max_points+1)
scores = {}

for m in marbles:
    if m % 23 == 0:
        scores[c_elf] = scores.get(c_elf, 0) + m
        c_pos = (c_pos-7) % len(c_list)
        scores[c_elf] += c_list.pop(c_pos)
    else:
        c_pos = (c_pos+1) % len(c_list) + 1
        c_list.insert(c_pos, m)
    c_elf = (c_elf % n_players) + 1 # 1..n not 0..n-1
    if not len(c_list)%100000:
        print(str(len(c_list)//1000).rjust(5," ") + "k items / ~7000k")

print(max(scores.values()))
