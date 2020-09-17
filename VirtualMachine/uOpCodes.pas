unit uOpCodes;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses System.SysUtils;

const
   oNop         = byte (0);
   oHalt        = byte (1);

   // Arithmetic
   oAdd         = byte (2);   // Pop two values off stack; add them and push reult on to stack
   oSub         = byte (3);
   oMult        = byte (4);
   oDivi        = byte (5);   // Integer division
   oMod         = byte (6);   // Modulus of two integer values
   oDivide      = byte (7);
   oUmi         = byte (8);
   oPower       = byte (9);
   oInc         = byte (10);  // Increment stack entry by arg
   oLocalInc    = byte (11);
   oDec         = byte (12);  // Decrement stack entry by arg
   oLocalDec    = byte (13);

   // Load and store values to symbol table
   oLoad        = byte (15);  // Load symbol argument onto stack, operand contains index to the symbol table
   oStore       = byte (16);  // Save item on stack to symbol table, operand contains index to the symbol table
   oLoadLocal   = byte (17);  // Load local symbol argument onto stack, operand contains index to the local symbol table
   oStoreLocal  = byte (18);  // Save item on stack to local symbol table, operand contains index to the symbol table

   // Logical
   oAnd         = byte (20);
   oOr          = byte (22);
   oNot         = byte (23);
   oXor         = byte (24);

   // Push and pop to stac
   oPushi       = byte (30);   // Push integer onto stack, operand contains the integer
   oPushd       = byte (31);   // Push double onto stack, operand contains index to constant table
   oPushb       = byte (32);   // Push boolean value onto stack, operand contains index to constant table
   oPushs       = byte (33);   // Push string value onto stack, operand contains index to constant table
   oPushNone    = byte (34);   // Push noneStackType onto the stack, used by retVal for returning nothing
   oPushModule  = byte (35);   // Push a module ptr onto the stack
   oPushFunction= byte (36);   // Push the function ptr onto the stack
   oPop         = byte (37);   // Only pop() stack

   // Boolean Tests
   oIsEq        = byte (40);   // Push True if TOS1 = TOS
   oIsGt        = byte (41);   // Push True if TOS1 > TOS
   oIsGte       = byte (42);   // Push True if TOS1 >= TOS
   oIsLt        = byte (43);   // Push True if TOS1 < TOS
   oIsLte       = byte (44);   // Push True if TOS1 <= TOS
   oIsNotEq     = byte (45);   // Push True if TOS1 <> TOS

   // Jump Instructions
   oJmp         = byte (50);  // Relative jump to new pc location
   oJmpIfTrue   = byte (51);  // Relative jump if stack entry is True
   oJmpIfFalse  = byte (52);  // Relative jump if stack entry is False

   oCreateList  = byte (60);  //
   oLvecIdx     = byte (61);  // Load element from vector
   oSvecIdx     = byte (62);  // Save element to vector
   oLocalLvecIdx= byte (63);  // Load element from local vector
   oLocalSvecIdx= byte (64);  // Save element to local vector

   // Calling routines
   oCall        = byte (80);  // Call a user defined function
   oBuiltin     = byte (81);  // Call a builin function
   oRet         = byte (83);  // Return from a function

   oPrint       = byte (90);  // Pop the stack and write the item to stdout
   oPrintln     = byte (91);  // Pop the stack and write a newline stdout
   oAssertTrue  = byte (92);  // Pop the stack and check that the operand is a boolean True
   oAssertFalse = byte (93);  // Pop the stack and check that the operand is a boolean False

   oAppendList  = byte (101);

   oPopAndSend = byte (200); // Debug opcode

   type
    TOpCode = byte;

   var
      opCodeNames : array[0..255] of string;

implementation


initialization
  opCodeNames[oNop]          := 'nop';
  opCodeNames[oHalt]         := 'halt';
  opCodeNames[oAdd]          := 'add';
  opCodeNames[oSub]          := 'sub';
  opCodeNames[oMult]         := 'mult';
  opCodeNames[oDivi]         := 'divi';
  opCodeNames[oDivide]       := 'divide';
  opCodeNames[oMod]          := 'mod';
  opCodeNames[oUmi]          := 'umi';
  opCodeNames[oPower]        := 'power';
  opCodeNames[oInc]          := 'inc';
  opCodeNames[oLocalInc]     := 'localInc';
  opCodeNames[oDec]          := 'dec';
  opCodeNames[oLocalDec]     := 'localDec';

  opCodeNames[oLoad]         := 'load';
  opCodeNames[oStore]        := 'store';
  opCodeNames[oLoadLocal]    := 'loadLocal';
  opCodeNames[oStoreLocal]   := 'storeLocal';

  opCodeNames[oAnd]          := 'and';
  opCodeNames[oOr]           := 'or';
  opCodeNames[oNot]          := 'not';
  opCodeNames[oXor]          := 'xor';

  opCodeNames[oPushi]        := 'pushi';
  opCodeNames[oPushb]        := 'pushb';
  opCodeNames[oPushd]        := 'pushd';
  opCodeNames[oPushs]        := 'pushs';
  opCodeNames[oPushNone]     := 'pushNone';
  opCodeNames[oPushModule]   := 'pushModule';
  opCodeNames[oPushFunction] := 'pushFunction';
  opCodeNames[oPop]          := 'pop';

  opCodeNames[oIsEq]         := 'isEq';
  opCodeNames[oIsGt]         := 'isGt';
  opCodeNames[oIsGte]        := 'isGte';
  opCodeNames[oIsLt]         := 'isLt';
  opCodeNames[oIsLte]        := 'isLte';
  opCodeNames[oIsNotEq]      := 'isNotEq';

  opCodeNames[oJmp]          := 'jmp';
  opCodeNames[oJmpIfTrue]    := 'jmpIfTrue';
  opCodeNames[oJmpIfFalse]   := 'jmpIfFalse';

  opCodeNames[oLvecIdx]      := 'lvecIdx';
  opCodeNames[oSvecIdx]      := 'svecIdx';
  opCodeNames[oLocalLvecIdx] := 'locallvecIdx';
  opCodeNames[oLocalSvecIdx] := 'localsvecIdx';

  opCodeNames[oCall]         := 'call';
  opCodeNames[oBuiltIn]      := 'builtin';
  opCodeNames[oRet]          := 'ret';

  opCodeNames[oPrint]        := 'print';
  opCodeNames[oPrintln]      := 'println';
  opCodeNames[oAssertTrue]   := 'assertTrue';
  opCodeNames[oAssertFalse]  := 'assertFalse';

  opCodeNames[oCreateList]   := 'createList';

end.




