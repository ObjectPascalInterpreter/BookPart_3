# BookPart 3

Update (11/6/2021): Added new binary release (3.0.2.0). Includes console, dll library and GUI app in binary form. 

Update (11/6/2021): More updates to the book. Added array type, plus a lot of minor changes and bug fixes elsewhere. First version of the embeddable version of the interpreter in folder librhodus. This generates a dll which 
can be imported into another application. At the moment only four methods are exported from the dll, rhodus_initialise, rhodus_run, rhodus_terminate, and rhodus_getlastError. Example usage in RhodusIDE. The 
rhodus group project file will load all subprojects. 

Update (10/14/2021): I uploaded the beginning of the book that will accompany the project. You'll find the text in the folder Book. I've not decided how to receive corrections etc, but we could use pull requests. At the moment I've just included the first draft of Chapter 1 and a corresponding pdf

Update (10/13/2021): Updated to 3.0.1.2. This includes the beginnings of array support. Also the repl was updated with new commands such as showtree and showcode. showtree lets you view the generated AST and showcode the byte code. The accompanying textbook has been started and the first chapter will be released sometime in the next few weeks. One big change is that after much thought and a number of attempts at designing array support I decided to switch from curley brackets to square brackets for lists. This matches python. Curely brackets will likely be reserved for maps/dictionaries. The same code below has been updated to reflect this change. Note that gif video
still shows curely brackets. This will be updated shortly. There is a new binary release to accompany this version.

Update (10/2/2021): The 3.0.1.0 update represents a major rewrite of the AST/compiler and parts of the parser. It now supports a simple object model on strings and lists as well as revising the grammar to take care of the new module syntax. I will also split the parsing into two parts to make it easer to handle memory managements during compile errors. All samples and tests have been updated to the new object syntax.  

This repo holds source code for part III of the book series on building an interpreter using Object Pascal (https://www.objectpascalinterpreter.com). the code is now fairly stable but I will continue to tidy up the code and possibly add one or two additional features.

The major outward change in this version includes library support, and a range of small builtin libraries such as math (these will be expanded). The biggest internal change is to separate code generation from syntax analysis in the form of an abstract syntax tree. User functions are also now first-class entities which can be passed around like any other variable. A lot of work has been done on ensure that garbage collection doesn't leak memory. 

<img src="/Images/demo1.gif" width="609" height="345"/>

## Build

To build the code, load RhodusVersionThreeGroup.groupproj into your Delphi IDE and run. 

The only dependency is FastMM4 for memory leak debugging but it can be left out by commenting out the line 35 in RhodusVersionThreeProject.dpr. 

## Examples

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
    numbers = [12, 7, 13, 5, 6]
    QSort(numbers, 0, numbers.len () - 1) 
    res = assertTrueEx (numbers == [5, 6, 7, 12, 13])
    if res == "." then
       println("----PASS----")
    else
       println("----FAIL----")
    end

    // Bigger example
    numbers = random.randlist (100, 100)
    size = numbers.len ()
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

        colors = [
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
          "DarkGray",       "Black"]   

        l = colors.len ()
        for i = 1 to l do
            setColor (colors[i-1])
            print ("Color Test  ");
            if i mod 6 == 0 then
               println() 
            end
        end

<img src="/Images/allColors.png" width="250"/>
