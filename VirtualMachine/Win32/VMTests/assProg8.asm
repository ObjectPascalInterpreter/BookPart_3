# if/then/else
# i = 2
# if i == 1 then
#    print "Yes"
# else
#    print "No"
# print "Done"
#
        pushi 2
        store 0
        load 0
        pushi 1
        isEq
        jmpIfFalse l1
        pushs "Yes"
        jmp l2
l1:     pushs "No"
        pushi 1
        println
        pushs "Done"
        pushi 1
l2:     print
        halt