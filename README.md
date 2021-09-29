# BookPart 3

This repo holds source code for part III of the book series on building an interpreter using Object Pascal (https://www.objectpascalinterpreter.com). the code is now fairly stable but I will continue to tidy up the code and possibly add one or two additional features. The major outward change in this version includes library support, and a range of small builtin libraries such as math (these will be expanded). The biggest internal change is to separate code generation from syntax analysis in the form of an abstract syntax tree. User functions are also now first-class entities which can be passed around like any other variable. A lot of work has been done on ensure that garbage collection doesn't leak memory. 

<img src="/Images/demo1.gif" width="609" height="345"/>

Example code:

    
    // Quick sort algorithm, depends on recursion
    function QSort (numbers, left, right)
      l_ptr = left
      r_ptr = right
      pivot = numbers[left]

      while (left < right) do

            while ((numbers[right] >= pivot) AND (left < right)) do
              right = right - 1
            end

            if (left != right) then
               numbers[left] = numbers[right]
               left = left + 1
            end

            while ((numbers[left] <= pivot) AND (left < right)) do
               left = left + 1
            end

            if (left != right) then
               numbers[right] = numbers[left]
               right = right - 1
            end
      end

      numbers[left] = pivot
      pivot = left
      left = l_ptr
      right = r_ptr

      if (left < pivot) Then
          QSort(numbers, left, pivot-1)
      end

      if (right > pivot) Then
         QSort(numbers, pivot+1, right)
      end 
    end

    // Driver code to test above   
    numbers = {12, 7, 13, 5, 6}
    QSort(numbers, 0, lists.len (numbers) - 1) 
    res = assertTrueEx (numbers == {5, 6, 7, 12, 13}
    if res == "." then
       println("----PASS----")
    else
       println("----FAIL----")
    end

    // Bigger example
    numbers = random.randlist (100, 100)
    size = lists.len (numbers)
    QSort(numbers, 0, size-1)
    println ("Larger example: ", numbers)


Passing functions as arguments:

    // Add some user defined funtions
    function sqr (x) return x*x; end
    function cube (x) return x*x*x; end

    function runtest (fcn, x)
       return fcn (x)
    end

    s = runtest (sqr, 5)
    println (s)

    s = runtest (cube, 5)
    println (s)

Use of modules:

First define a new module called lib.rh:

    a1 = 1.234
    b1 = 5.678

    function sqrt (x)
       return x^0.5
    end;

    function input (prompt)
       print (prompt + " ");
       return readString()
    end
    
Importing and using the module:

    import lib

    println (lib.sqrt (25))
    println (lib.a1)
    
Printing out all the colors:

        colors = {
          "Snow",           "FloralWhite",     "LavenderBlush", "OldLace",         "Ivory",                "CornSilk",       "Beige",          "AntiqueWhite",
          "Wheat",          "AliceBlue",       "GhostWhite",    "Lavender",        "Seashell",             "LightYellow",    "PapayaWhip",     "NavajoWhite",
          "Moccasin",       "Burlywood",       "Azure",         "Mintcream",       "Honeydew",             "Linen",          "LemonChiffon",   "BlanchedAlmond",
          "Bisque",         "PeachPuff",       "Tan",           "Yellow",          "DarkOrange",           "Red",            "DarkRed",        "Maroon",
          "IndianRed",      "Salmon",          "Coral",         "Gold",            "Tomato",               "Crimson",        "Brown",          "Chocolate",
          "SandyBrown",     "LightSalmon",     "LightCoral",    "Orange",          "OrangeRed",            "Firebrick",      "SaddleBrown",    "Sienna",
          "Peru",           "DarkSalmon",      "RosyBrown",     "PaleGoldenrod",   "LightGoldenrodYellow", "Olive",          "ForestGreen",    "GreenYellow",
          "Chartreuse",     "LightGreen",      "Aquamarine",    "SeaGreen",        "GoldenRod",            "Khaki",          "OliveDrab",      "Green",
          "YellowGreen",    "LawnGreen",       "PaleGreen",     "MediumAquamarine","MediumSeaGreen",       "DarkGoldenRod",  "DarkKhaki",      "DarkOliveGreen",
          "Darkgreen",      "LimeGreen",       "Lime",          "SpringGreen",     "MediumSpringGreen",    "DarkSeaGreen",   "LightSeaGreen",  "PaleTurquoise",
          "LightCyan",      "LightBlue",       "LightSkyBlue",  "CornFlowerBlue",  "DarkBlue",             "Indigo",         "MediumTurquoise","Turquoise",
          "Cyan",           "PowderBlue",      "SkyBlue",       "RoyalBlue",       "MediumBlue",           "MidnightBlue",   "DarkTurquoise",  "CadetBlue",
          "DarkCyan",       "Teal",            "DeepSkyBlue",   "DodgerBlue",      "Blue",                 "Navy",           "DarkViolet",     "DarkOrchid",
          "Magenta",        "DarkMagenta",     "MediumVioletRed", "PaleVioletRed", "BlueViolet",           "MediumOrchid",   "MediumPurple",   "Purple",
          "DeepPink",       "LightPink",       "Violet",        "Orchid",          "Plum",                 "Thistle",         "HotPink",       "Pink",
          "LightSteelBlue", "MediumSlateBlue", "LightSlateGray","White",           "Lightgrey",            "Gray",            "SteelBlue",     "SlateBlue",
          "SlateGray",      "WhiteSmoke",      "Silver",        "DimGray",         "MistyRose",            "DarkSlateBlue",   "DarkSlategray", "Gainsboro",
          "DarkGray",       "Black"}   

        l = lists.len (colors)
        for i = 1 to l do
            setColor (colors[i-1])
            print ("Color Test  ");
            if i mod 4 == 0 then
               println() 
            end
        end
