def checksum1(spreadsheet):
    ss = []
    checksum = 0
    
    rows = spreadsheet.split("\n")
    for row in rows:
        crow = []
        for x in row.split():
            crow += [int(x)]
        ss += [crow]

    for row in ss:
        checksum += max(row) - min(row)

    return(checksum)

def checksum2(spreadsheet):
    ss = []
    checksum = 0
    
    rows = spreadsheet.split("\n")
    for row in rows:
        crow = []
        for x in row.split():
            crow += [int(x)]
        ss += [crow]

    for row in ss:
        print(row)
        rowd = False
        for xi in range(0, len(row)-1):
            x = row[xi]
            for yi in range(xi+1, len(row)):
                y = row[yi]
                print("I1:",xi,"\tI2:",yi,"\tX =",x,"\tY =",y)
                if x/y == x//y:
                    print(x,"/",y,"=",x/y)
                    checksum += x//y
                    rowd = True
                elif y/x == y//x:
                    print(y,"/",x,"=",y/x)
                    checksum += y//x
                    rowd = True
                if rowd:
                    break
            if rowd:
                break

    return(checksum)
