# List test, create list and change one of the elements and print
     pushi 1
     pushi 4
     pushi 9
     createList 3
     store 0
 # load to new value to assign
     pushi 12
 # load the list object to assign to
     load 0
 # load the index to the list
     pushi 0
     svecIdx
     load 0
     pushi 1
     print
     halt