# Timing test
# Repeat/until loop
# i = 1
# Repeat
#   print (i)
#   i = i + 1
# unti i > 1000
#
        pushi 1
        store 0
l1:     pushi 1
        load 0
        add
        store 0
        pushi 1
        pop
        pushi 1
        pop
        pushi 1
        pop
        pushi 1
        pop
        pushi 1
        pop
        pushi 1
        pop
        load 0
        pushi 500000
        isGt
        jmpIfFalse l1
l2:     halt
