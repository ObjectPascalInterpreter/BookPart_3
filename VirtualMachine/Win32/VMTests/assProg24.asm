# List test, create nested list {{6,7},4,9}
#
     pushi 1
     pushi 4
     pushi 9
     createList 3
     store 0
     pushi 6
     pushi 7
     createList 2
     load 0
     pushi 0
     svecIdx
     load 0
     halt