unit uRhodusTypes;


// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


interface

type
   // symNonExistant: Requests for symbols that aren't even in the symbol table
   TSymbolElementType = (symNonExistant = -1, symUndefined, symInteger, symDouble, symBoolean,
         symString, symList, symArray, symUserFunc, symModule);


implementation

end.
