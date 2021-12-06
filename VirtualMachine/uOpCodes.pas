unit uOpCodes;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses System.SysUtils;

const
   oNop            = byte (0);
   oHalt           = byte (1);

   // Arithmetic
   oAdd            = byte (2);   // Pop two values off stack; add them and push reult on to stack
   oSub            = byte (3);
   oMult           = byte (4);
   oDivi           = byte (5);   // Integer division
   oMod            = byte (6);   // Modulus of two integer values
   oDivide         = byte (7);
   oDotProduct     = byte (8);
   oUmi            = byte (9);
   oPower          = byte (10);
   oInc            = byte (11);  // Increment stack entry by arg
   oLocalInc       = byte (12);
   oDec            = byte (13);  // Decrement stack entry by arg
   oLocalDec       = byte (14);

   // Load and store values to symbol table
   oLoadLocal      = byte (20);  // Load local symbol argument onto stack, operand contains index to the local symbol table
   oLoadSymbol     = byte (21);
   oLoadAttr       = byte (22);
   oStoreSymbol    = byte (23);
   oStoreAttr      = byte (24);
   oStoreLocal     = byte (25);
   //oLoadSlice      = byte (26);

   // Logical
   oAnd            = byte (30);
   oOr             = byte (32);
   oNot            = byte (33);
   oXor            = byte (34);

   // Push and pop to stac
   oPushi         = byte (50);   // Push integer onto stack, operand contains the integer
   oPushd         = byte (51);   // Push double onto stack, operand contains index to constant table
   oPushb         = byte (52);   // Push boolean value onto stack, operand contains index to constant table
   oPushs         = byte (53);   // Push string value onto stack, operand contains index to constant table
   oPushNone      = byte (54);   // Push noneStackType onto the stack, used by retVal for returning nothing
   oPop           = byte (55);   // pop() stack is next isntruction is not a halt
   oDup           = byte (56);   // Duplicate the stack entry
   oPopDup        = byte (57);   // Pop the stack
   oToDbl         = byte (58);   // Convert the stack entry to a double value (used in the for loop)

   // Boolean Tests
   oIsEq          = byte (70);   // Push True if TOS1 == TOS
   oIsGt          = byte (71);   // Push True if TOS1 > TOS
   oIsGte         = byte (72);   // Push True if TOS1 >= TOS
   oIsLt          = byte (73);   // Push True if TOS1 < TOS
   oIsLte         = byte (74);   // Push True if TOS1 <= TOS
   oIsNotEq       = byte (75);   // Push True if TOS1 != TOS

   // Jump Instructions
   oJmp          = byte (80);  // Relative jump to new pc location
   oJmpIfTrue    = byte (81);  // Relative jump if stack entry is True
   oJmpIfFalse   = byte (82);  // Relative jump if stack entry is False

   oCreateList   = byte (90);  //
   oLvecIdx      = byte (91);  // Load element from vector
   oSvecIdx      = byte (92);  // Save element to vector
   oLocalLvecIdx = byte (93);  // Load element from local vector
   oLocalSvecIdx = byte (94);  // Save element to local vector
   oBuildSlice   = byte (95);  // Build a slice object then push it onto stack
   oSliceAll     = byte (96);
   oSliceObj     = byte (97);

   oCreateArray  = byte (100);

   // Calling routines
   oCall         = byte (110);  // Call a user defined function
   oBuiltin      = byte (111);  // Call a builin function
   oRet          = byte (112);  // Return from a function

   oPrint        = byte (120);  // Pop the stack and write the item to stdout
   oPrintln      = byte (121);  // Pop the stack and write a newline stdout
   oSetColor     = byte (122);  // Pop the stack and set the color
   oAssertTrue   = byte (123);  // Pop the stack and check that the operand is a boolean True, then print
   oAssertTrueEx = byte (124);  // Pop the stack and check that the operand is a boolean True
   oAssertFalse  = byte (125);  // Pop the stack and check that the operand is a boolean False
   oHelp         = byte (126);

   oImportModule = byte (130);  //


   oPopAndSend  = byte (300); // Debug opcode, not currently used

   type
    TOpCode = byte;

   var
      opCodeNames : array[0..255] of string;

implementation


initialization
  opCodeNames[oNop]            := 'nop';
  opCodeNames[oHalt]           := 'halt';
  opCodeNames[oAdd]            := 'add';
  opCodeNames[oSub]            := 'sub';
  opCodeNames[oMult]           := 'mult';
  opCodeNames[oDivi]           := 'divi';
  opCodeNames[oDivide]         := 'divide';
  opCodeNames[oMod]            := 'mod';
  opCodeNames[oUmi]            := 'umi';
  opCodeNames[oPower]          := 'power';
  opCodeNames[oInc]            := 'inc';
  opCodeNames[oLocalInc]       := 'localInc';
  opCodeNames[oDec]            := 'dec';
  opCodeNames[oLocalDec]       := 'localDec';

  opCodeNames[oLoadLocal]      := 'loadLocal';
  opCodeNames[oStoreLocal]     := 'storeLocal';
  opCodeNames[oLoadSymbol]     := 'load';
  opCodeNames[oLoadAttr]       := 'loadAttr';

  opCodeNames[oStoreSymbol ]   := 'store';
  opCodeNames[oStoreAttr]      := 'storeAttr';

  opCodeNames[oBuildSlice]     := 'buildSlice';
  opCodeNames[oSliceAll]       := 'sliceAll';
  opCodeNames[oSliceObj]       := 'sliceObj';

  opCodeNames[oAnd]            := 'and';
  opCodeNames[oOr]             := 'or';
  opCodeNames[oNot]            := 'not';
  opCodeNames[oXor]            := 'xor';

  opCodeNames[oPushi]          := 'pushi';
  opCodeNames[oPushb]          := 'pushb';
  opCodeNames[oPushd]          := 'pushd';
  opCodeNames[oPushs]          := 'pushs';
  opCodeNames[oPushNone]       := 'pushNone';
  opCodeNames[oPop]            := 'pop';
  opCodeNames[oDup]            := 'dup';
  opCodeNames[oPopDup]         := 'popDup';
  opCodeNames[oToDbl]          := 'toDbl';

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

  opCodeNames[oCreateArray]  := 'createArray';

  opCodeNames[oCall]         := 'call';
  opCodeNames[oBuiltIn]      := 'builtin';
  opCodeNames[oRet]          := 'ret';

  opCodeNames[oPrint]        := 'print';
  opCodeNames[oPrintln]      := 'println';
  opCodeNames[oSetColor]     := 'setColor';
  opCodeNames[oAssertTrue]   := 'assertTrue';
  opCodeNames[oAssertFalse]  := 'assertFalse';

  opCodeNames[oCreateList]   := 'createList';
  opCodeNames[oImportModule] := 'importModule';

end.




