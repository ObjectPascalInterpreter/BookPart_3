# while loop
# i = 5
# while i > 0 do
#    print "In loop"
#    i = i - 1
# end
# print "Done"
#
        pushi 5
        store 0
l2:     load 0
        pushi 0
        isGt
        jmpIfFalse l1
        pushs "In loop"
        pushi 1
        println 
        load 0
        pushi 1
        sub
        store 0
        jmp l2
l1:     pushs "Done"
        pushi 1
        print
        halt