# while loop
# i = 5
# while i > 0 do
#    print "In loop"
#    i = i - 1
# end
# print "Done"
#
        pushi 5000000
        store 0
L2:     load 0
        pushi 0
        isGt
        jmpIfFalse L1
        load 0
        pushi 1
        sub
        store 0
        jmp L2
L1:     halt