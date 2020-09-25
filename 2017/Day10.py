def circle_slice(lst, min_, max_, step=1):
    ol = []
    for x in range(min_, max_, step):
        ol.append(lst[x % len(lst)])
    return(ol)
def circle_replace(lst, lst2, pos):
    for x in range(0, len(lst2)):
        lst[(pos+x)%len(lst)] = lst2[x]
    #return(lst) # input list is changed anyway

def knot(lst, ln, pos):
    " Put a knot in lst by reversing ln elements starting at pos "
    slice_ = circle_slice(lst, pos, pos+ln)
    slice_.reverse()
    circle_replace(lst, slice_, pos)
    #return(lst) # input list is changed anyway

def xor(*numbers):
    out = 0
    for n in numbers:
        out ^= n
    return(out)
def xor_reduce(sparse):
    dense = []
    for x in range(0, 256, 16):
        dense.append(xor(*sparse[x:x+16]))
    return(dense)
def dense_to_hex(dense):
    return(("{:02x}"*16).format(*dense))

lens_str = "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100"

def get_hash(lens_str):
    nrounds = 64

    lst = list(range(0,256))
    lengths = [ord(char) for char in lens_str] + [17, 31, 73, 47, 23]
    skip_size = 0
    cur_pos = 0

    for round_number in range(0, nrounds):
        for ln in lengths:
            knot(lst, ln, cur_pos)
            cur_pos += ln + skip_size
            skip_size += 1

    dense = xor_reduce(lst)
    hx = dense_to_hex(dense)
    return(hx)

if __name__ == "__main__":
    print(get_hash(lens_str))
