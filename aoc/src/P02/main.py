with open('./data', 'r') as file:
    rows = file.readlines()

rows = [[int(x) for x in r.strip().split( )] for r in rows]

def is_safe(row: list[int]) -> bool:

    diff = [x1-x2 for x1,x2 in zip(row[1:], row[:-1])]
    return all([1<=x<=3 for x in diff]) or all([-3<=x<=-1 for x in diff])

cnt = 0
for n, row in enumerate(rows,1):
    if is_safe(row):
        print(n, True)
        cnt += 1
    else:
        r = False
        for i in range(len(row)):
            if is_safe(row[:i]+row[i+1:]):
                #print(n, row + ['-', i, '-'] + row[:i]+row[i+1:])
                r=True    
                break
        if r:
                cnt += 1
                print(n, True)
        else:                  
            print(n, False)

        

print("\nTotal nb of safes =", cnt)