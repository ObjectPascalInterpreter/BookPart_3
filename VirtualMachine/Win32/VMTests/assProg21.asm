# List test, create list and access element and print it
#
     pushi 1
     pushi 4
     pushi 9
     createList 3
     store 0
# load the list variable
     load 0
# load the index to the list
     pushi 2
# retrieve the element at index for the variable
     lvecIdx
     pushi 1
     print
     halt
