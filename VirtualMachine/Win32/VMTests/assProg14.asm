# Test simple builtin call, 3 is 
# the sqrt function, repeat 10 times
#
      pushd 10.0
      store 0
 L1:  load 0 
      pushs "Number = "
      pushi 1
      print
      load 0
      pushi 1
      println
      pushs "Square root = "
      pushi 1
      print
      load 0
      pushi 3
      builtin
      pushi 1
      println
      load 0
      pushd 1.0
      sub
      store 0
      load 0
      pushd 0.0
      isGt
      jmpIfTrue L1
      halt
