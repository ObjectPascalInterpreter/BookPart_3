# Evaluate an expression  (2+7)*4/2
#
        pushi 2
        jmp l2
l1:     pushi 2
        pushi 7
        add
        pushi 4
        mult
        pushi 2
        divide
        jmp l3
l2:     jmp l1
l3:     halt
