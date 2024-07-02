unit uJumpTables;

interface

Uses Classes, SysUtils, uMachineStack;

type
  TBinOp = procedure (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
  TJumpTable = array[TStackType,TStackType] of TBinOp;

var
  addJumpTable : TJumpTable;

implementation

Uses uStringObject,
     uListObject,
     uArrayObject,
     uVectorObject,
     uMatrixObject,
     uValueObject,
     uSymbolTable,
     uRhodusTypes,
     uVMExceptions;


procedure errorFunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  raise ERuntimeException.Create (stToStr(d1.stackType) + ' and ' +
    stToStr(d2.stackType) + ' cannot be used with the add operation');
end;

procedure addIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.iValue := d1.iValue + d2.iValue;
  result.stackType := stInteger;
end;

procedure addIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue + d2.iValue;
  result.stackType := stDouble;
end;

procedure addDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue + d2.dValue;
  result.stackType := stdouble;
end;

procedure addDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue + d2.dValue;
  result.stackType := stdouble;
end;

procedure addIntegerValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.voValue) + d2.iValue;
  result.stackType := stdouble;
end;

procedure addValueObjectIntegerfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d2.voValue) + d1.iValue;
  result.stackType := stdouble;
end;

procedure addDoubleValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.voValue) + d2.dValue;
  result.stackType := stdouble;
end;

procedure addValueObjectDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d2.voValue) + d1.dValue;
  result.stackType := stdouble;
end;

procedure addStringStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.sValue := TStringObject.add(d2.sValue, d1.sValue);
  result.stackType := stString;
end;

procedure addListIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.lValue := d2.lValue.clone;
   result.lValue.append(d1.iValue);
   result.stackType := stList;
end;

procedure addIntListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.lValue := d1.lValue.clone;
   result.lValue.insert(0, d2.iValue);
   result.stackType := stList;
end;

procedure addBoolListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d1.lValue.clone;
  result.lValue.insert (0, d2.bValue);
  result.stackType := stList;
end;

procedure addListBoolfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d2.lValue.clone;
  result.lValue.append(d1.bValue);
  result.stackType := stList;
end;

procedure addDoubleListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d1.lValue.clone;
  result.lValue.insert (0, d2.dValue);
  result.stackType := stList;
end;

procedure addListDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d2.lValue.clone;
  result.lValue.append(d1.dValue);
  result.stackType := stList;
end;

procedure addStringListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TStringObject;
begin
  result.lValue := d1.lValue.clone;
  tmp   := d2.sValue.clone;
  tmp.blockType := btOwned;
  result.lValue.insert(0, tmp);
  result.stackType := stList;
end;

procedure addListStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d2.lValue.clone;
  result.lValue.append(d1.sValue.clone);
  result.stackType := stList;
end;

procedure addListListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.lValue := TListObject.addLists(d2.lValue, d1.lValue);
   result.stackType := stList
end;

procedure addFuncListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TUserFunction;
begin
  result.lValue := d1.lValue.clone;
  tmp := d2.fValue.clone;
  tmp.blockType := btOwned;
  result.lValue.insertUserFunction (0, tmp);
  result.stackType := stList;
end;

procedure addListFuncfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TUserFunction;
begin
  result.lValue := d2.lValue.clone;
  tmp := d1.fValue.clone;
  tmp.blockType := btOwned;
  result.lValue.appendUserFunction(tmp);
  result.stackType := stList;
end;

procedure addListModulefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d2.lValue.clone;
  result.lValue.appendModule (d1.module);
  result.stackType := stList;
end;

procedure addModuleListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d1.lValue.clone;
  result.lValue.appendModule (d2.module);
  result.stackType := stList;
end;

procedure addListArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d2.lValue.clone;
  result.lValue.append(d1.aValue.clone);
  result.stackType := stList;
end;

procedure addArrayListfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := d1.lValue.clone;
  result.lValue.append(d2.aValue.clone);
  result.stackType := stList;
end;

procedure addArrayArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d2.aValue, d1.aValue);
  result.stackType := stArray;
end;

procedure addVectorVectorfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.vValue := TVectorObject.add(d2.vValue, d1.vValue);
  result.stackType := stVector;
end;


procedure addMatrixMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.add(d2.mValue, d1.mValue);
  result.stackType := stMatrix;
end;


procedure addIntegerMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin

end;


procedure addMatrixDoublefunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin

end;


procedure addDoubleMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin

end;

procedure create_addJumpTable;
var s1, s2 : TStackType;
begin
  for s1 := stNone to stObject do
      for s2 := stNone to stObject  do
          addJumpTable[s1, s2] := errorFunc;

  addJumpTable[stInteger, stInteger] := addIntIntfunc;
  addJumpTable[stInteger, stDouble]  := addIntDoublefunc;
  addJumpTable[stDouble,  stInteger] := addDoubleIntfunc;
  addJumpTable[stDouble,  stDouble]  := addDoubleDoublefunc;
  addJumpTable[stInteger,  stValueObject] := addIntegerValueObjectfunc;
  addJumpTable[stValueObject, stInteger] := addValueObjectIntegerfunc;
  addJumpTable[stDouble,  stValueObject] := addDoubleValueObjectfunc;
  addJumpTable[stValueObject, stDouble] := addValueObjectDoublefunc;

  addJumpTable[stString,  stString]  := addStringStringfunc;
  addJumpTable[stList,    stInteger] := addListIntfunc;
  addJumpTable[stInteger, stList]    := addIntListfunc;
  addJumpTable[stBoolean, stList]    := addBoolListfunc;
  addJumpTable[stList,    stBoolean] := addListBoolfunc;
  addJumpTable[stDouble,  stList]    := addDoubleListfunc;
  addJumpTable[stList,    stDouble]  := addListDoublefunc;
  addJumpTable[stString,  stList]    := addStringListfunc;
  addJumpTable[stList,    stString]  := addListStringfunc;
  addJumpTable[stList,    stList]    := addListListfunc;
  addJumpTable[stFunction, stList]   := addFuncListfunc;
  addJumpTable[stList, stFunction]   := addListFuncfunc;
  addJumpTable[stlist,    stModule]  := addListModulefunc;
  addJumpTable[stModule,  stList]    := addModuleListfunc;
  addJumpTable[stList,    stArray]   := addListArrayfunc;
  addJumpTable[stArray,   stList]    := addArrayListfunc;
  addJumpTable[stArray,   stArray]   := addArrayArrayfunc;
  addJumpTable[stVector,  stVector]  := addVectorVectorfunc;

  addJumpTable[stMatrix,  stMatrix]  := addMatrixMatrixfunc;
  //addJumpTable[stMatrix,  stVector] := addMatrixIntegerfunc;
  //addJumpTable[stInteger, stMatrix]  := addIntegerMatrixfunc;
  //addJumpTable[stDouble, stMatrix]   := addMatrixDoublefunc;
  //addJumpTable[stMatrix, stInteger]  := addDoubleMatrixfunc;
end;


initialization
  create_addJumpTable;
end.
