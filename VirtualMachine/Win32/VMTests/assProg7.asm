# Repeat/until loop
# i = 1
# Repeat
#   print i
#   i = i + 1
# unti i > 5
# print "Done"
#
        pushi 1
        store 0
l1:     load 0
        pushi 1
        println
        pushi 1
        load 0
        add
        store 0
        load 0
        pushi 5
        isGt
        jmpIfFalse l1
        pushs "Done"
        pushi 1
l2:     print
        halt