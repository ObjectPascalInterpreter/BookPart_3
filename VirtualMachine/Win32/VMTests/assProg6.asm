# Test store, load and add one
#
        pushs "Test store, load and add 1"
        pushi 1
        println
        pushi 1
        store 0
        load 0
        pushi 1
        add
        store 0
        load 0
        halt
