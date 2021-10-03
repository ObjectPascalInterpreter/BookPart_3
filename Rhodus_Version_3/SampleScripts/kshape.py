rows = int(input("Enter K Shape Alphabets Pattern Rows = "))

print("====K Shape Alphabets Pattern====")

for i in lists.range(rows, 0, -1):
    alphabet = 65
    for j in lists.range(0, i):
        print('%c' %(alphabet + j), end = ' ')
    print()

for i in lists.range(1, rows):
    alphabet = 65
    for j in lists.range(0, i + 1):
        print('%c' %(alphabet + j), end = ' ')
    print()