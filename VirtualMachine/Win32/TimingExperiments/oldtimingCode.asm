# Timing test
        pushi 1
        store 0
L1:     pushi 1
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
L2:     nop