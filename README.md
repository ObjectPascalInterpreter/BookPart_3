# BookPart_3

This is source code for part III of the book series on building an interpreter using Object Pascal. The major outward change in this version include library support, a range of (currently) small builtin libraries such as math. The biggest internal change is to separate code generation from syntax analysis in the for of an abstract syntax tree. User functions are also now first-class entities which can be passed around like any other variable. Example code:

