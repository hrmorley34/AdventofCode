def parse(text):
    count = 0
    level = 1
    prev = False
    gar = False
    garc = 0
    for c in text:
        if prev:
            prev = False
            continue
        else:
            if c == "!":
                prev = True
            else:
                if gar:
                    if c == ">":
                        gar = False
                    else:
                        garc += 1
                else:
                    if c == "{":
                        count += level
                        level += 1
                    elif c == "}":
                        level -= 1
                    elif c == "<":
                        gar = True
    return(count, garc)
