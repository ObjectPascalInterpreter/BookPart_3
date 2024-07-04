unit uJumpTables;

interface

Uses Classes, SysUtils, uMachineStack;

type
  TBinOp = procedure (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
  TJumpTable = array[TStackType,TStackType] of TBinOp;

var
  addJumpTable : TJumpTable;
  subJumpTable : TJumpTable;
  multJumpTable : TJumpTable;
  divJumpTable : TJumpTable;

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


procedure addErrorFunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  raise ERuntimeException.Create (stToStr(d1.stackType) + ' and ' +
    stToStr(d2.stackType) + ' cannot be used with the add operation');
end;

procedure subErrorFunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  raise ERuntimeException.Create (stToStr(d1.stackType) + ' and ' +
    stToStr(d2.stackType) + ' cannot be used with the subtract operation');
end;

procedure multErrorFunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  raise ERuntimeException.Create (stToStr(d1.stackType) + ' and ' +
    stToStr(d2.stackType) + ' cannot be used with the multiply operation');
end;

procedure divErrorFunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  raise ERuntimeException.Create (stToStr(d1.stackType) + ' and ' +
    stToStr(d2.stackType) + ' cannot be used with the divide operation');
end;


// ----------------------------------------------------------------------------------
// Addition operations

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

procedure addIntValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.voValue) + d2.iValue;
  result.stackType := stdouble;
end;

procedure addValueObjectIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
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

procedure addIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d1.aValue, d2.iValue);
  result.stackType := stArray;
end;

procedure addArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d2.aValue, d1.iValue);
  result.stackType := stArray;
end;

procedure addDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d1.aValue, d2.dValue);
  result.stackType := stArray;
end;

procedure addArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d2.aValue, d1.dValue);
  result.stackType := stArray;
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
  result.mValue := TMatrixObject.add (d1.mValue, double (d2.iValue));
  result.stackType := stMatrix;
end;

procedure addMatrixIntegerfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.add (d2.mValue, double (d1.iValue));
  result.stackType := stMatrix;
end;


procedure addMatrixDoublefunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.add (d2.mValue,  d1.dValue);
  result.stackType := stMatrix;
end;


procedure addDoubleMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.add (d1.mValue, d2.dValue);
  result.stackType := stMatrix;
end;


procedure addValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d2.aValue, d1.voValue.getScalar());
  result.stackType := stArray;
end;

procedure addArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.add(d1.aValue, d2.voValue.getScalar());
  result.stackType := stArray;
end;


// ----------------------------------------------------------------------------------
// Subtraction operations

procedure subIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.iValue := d1.iValue - d2.iValue;
  result.stackType := stInteger;
end;

procedure subIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - d2.iValue;
  result.stackType := stDouble;
end;

procedure subDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue - d2.dValue;
  result.stackType := stdouble;
end;

procedure subDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - d2.dValue;
  result.stackType := stdouble;
end;

procedure subIntegerValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.voValue) - d2.iValue;
  result.stackType := stdouble;
end;

procedure subValueObjectIntegerfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue - TValueObject.getValue(d2.voValue);
  result.stackType := stdouble;
end;

procedure subDoubleValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.voValue) - d2.dValue;
  result.stackType := stdouble;
end;

procedure subValueObjectDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - TValueObject.getValue(d2.voValue);
  result.stackType := stdouble;
end;


procedure subIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subleft(d1.aValue, d2.iValue);
  result.stackType := stArray;
end;

procedure subArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subRight(d2.aValue, d1.iValue);
  result.stackType := stArray;
end;

procedure subDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subLeft(d1.aValue, d2.dValue);
  result.stackType := stArray;
end;

procedure subArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subRight(d2.aValue, d1.dValue);
  result.stackType := stArray;
end;

procedure subArrayArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.sub(d1.aValue, d2.aValue);
  result.stackType := stArray;
end;

//procedure addVectorVectorfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
//begin
//  result.vValue := TVectorObject.add(d2.vValue, d1.vValue);
//  result.stackType := stVector;
//end;


procedure subMatrixMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.sub(d1.mValue, d2.mValue);
  result.stackType := stMatrix;
end;


procedure subIntMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.subLeft (d1.mValue, double (d2.iValue));
  result.stackType := stMatrix;
end;

procedure subMatrixIntfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.subRight (d2.mValue, double (d1.iValue));
  result.stackType := stMatrix;
end;


procedure subMatrixDoublefunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.subRight (d2.mValue, d1.dValue);
  result.stackType := stMatrix;
end;


procedure subDoubleMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.subLeft (d1.mValue, d2.dValue);
  result.stackType := stMatrix;
end;


procedure subValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subLeft(d1.aValue, d2.voValue.getScalar());
  result.stackType := stArray;
end;

procedure subArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.subRight(d2.aValue, d1.voValue.getScalar());
  result.stackType := stArray;
end;


// ----------------------------------------------------------------------------------
// Muliplication Operations

procedure multIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.iValue := d1.iValue * d2.iValue;
  result.stackType := stInteger;
end;

procedure multDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue * d2.dValue;
  result.stackType := stDouble;
end;

procedure multIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * d2.iValue;
  result.stackType := stDouble;
end;

procedure multDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * d2.dValue;
  result.stackType := stDouble;
end;


procedure multIntValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.voValue.getScalar() * d2.iValue;
  result.stackType := stDouble;
end;


procedure multValueIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue * d2.voValue.getScalar();
  result.stackType := stDouble;
end;


procedure multDoubleValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.voValue.getScalar() * d2.dValue;
  result.stackType := stDouble;
end;


procedure multValueDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * d2.voValue.getScalar();
  result.stackType := stDouble;
end;

//###
procedure multValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult(d1.aValue, d2.voValue.getScalar());
  result.stackType := stArray;
end;

procedure multArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult(d2.aValue, d1.voValue.getScalar());
  result.stackType := stArray;
end;

procedure multValueMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d1.mValue, d2.voValue.getScalar());
  result.stackType := stMatrix;
end;

procedure multIntegerArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult (d1.aValue, d2.iValue);
  result.stackType := stArray;
end;

procedure multArrayIntegerfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult (d2.aValue, d1.iValue);
  result.stackType := stArray;
end;

procedure multDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult (d1.aValue, d2.dValue);
  result.stackType := stArray;
end;

procedure multArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.arrayScalarDoubleMult (d2.aValue, d1.dValue);
  result.stackType := stArray;
end;

procedure multArrayArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.mult (d2.aValue, d1.aValue);
  result.stackType := stArray;
end;

// Strings

procedure multIntStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var i : integer;
    value : string;
begin
   value := d1.sValue.value;
   for i := 2 to d2.iValue do
       value := value + d1.sValue.value;

  result.sValue := TStringObject.Create(value);
  result.stackType := stString;
end;


procedure multStringIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var i : integer;
    value : string;
begin
   value := d2.sValue.value;
   for i := 2 to d1.iValue do
       value := value + d2.sValue.value;

  result.sValue := TStringObject.Create(value);
  result.stackType := stString;
end;


// Lists

procedure multIntListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := TListObject.multiply(d2.iValue, d1.lValue);
  result.stackType := stList;
end;


procedure multListIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.lValue := TListObject.multiply(d1.iValue, d2.lValue);
  result.stackType := stList;
end;

// -----No more list mults

procedure multIntMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d1.mValue, d2.iValue);
  result.stackType := stMatrix;
end;


procedure multMatrixIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d2.mValue, d1.iValue);
  result.stackType := stMatrix;
end;


procedure multMatrixDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d2.mValue, d1.dValue);
  result.stackType := stMatrix;
end;

procedure multMatrixValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d2.mValue, d1.voValue.getScalar());
  result.stackType := stMatrix;
end;


procedure multDoubleMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.scalarMult (d1.mValue, d2.dValue);
  result.stackType := stMatrix;
end;


procedure multMatrixMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.mValue := TMatrixObject.mult (d2.mValue, d1.mValue);
  result.stackType := stMatrix;
end;



// ----------------------------------------------------------------------------------
// Division operations

procedure divIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / d2.iValue;
  result.stackType := stDouble;
end;

procedure divDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / d2.dValue;
  result.stackType := stDouble;
end;


procedure divDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / d2.dValue;
  result.stackType := stDouble;
end;


procedure divIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / d2.iValue;
  result.stackType := stDouble;
end;

procedure divIntValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.voValue.getScalar() / d2.iValue;
  result.stackType := stDouble;
end;


procedure divValueIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / d2.voValue.getScalar();
  result.stackType := stDouble;
end;

procedure divDoubleValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.voValue.getScalar() / d2.dValue;
  result.stackType := stDouble;
end;


procedure divValueDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / d2.voValue.getScalar();
  result.stackType := stDouble;
end;

procedure divIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d1.aValue, d2.iValue, False);
  result.stackType := stArray;
end;

procedure divArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d2.aValue, d1.iValue, True); // True = compute reciprical
  result.stackType := stArray;
end;

procedure divDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d1.aValue, d2.dValue, False);
  result.stackType := stArray;
end;

procedure divArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d2.aValue, d1.dValue, True);
  result.stackType := stArray;
end;

procedure divValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d1.aValue, d2.voValue.getScalar(), False);
  result.stackType := stArray;
end;

procedure divArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.aValue := TArrayObject.divide (d2.aValue, d1.voValue.getScalar(), True);
  result.stackType := stArray;
end;

// ----------------------------------------------------------------------------------

procedure create_addJumpTable;
var s1, s2 : TStackType;
begin
  // This loop is used to mark every conbination of types with an error
  // function. We then assign actual functions for the ones we have.
  // This makes it easy to trap illegal combinations.
  for s1 := stNone to stObject do
      for s2 := stNone to stObject  do
          addJumpTable[s1, s2] :=addErrorFunc;

  addJumpTable[stInteger, stInteger] := addIntIntfunc;
  addJumpTable[stInteger, stDouble]  := addIntDoublefunc;
  addJumpTable[stDouble,  stInteger] := addDoubleIntfunc;
  addJumpTable[stDouble,  stDouble]  := addDoubleDoublefunc;
  addJumpTable[stInteger,  stValueObject] := addIntValueObjectfunc;
  addJumpTable[stValueObject, stInteger] := addValueObjectIntfunc;
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
  addJumpTable[stList,   stFunction] := addListFuncfunc;
  addJumpTable[stlist,    stModule]  := addListModulefunc;
  addJumpTable[stModule,  stList]    := addModuleListfunc;
  addJumpTable[stList,    stArray]   := addListArrayfunc;
  addJumpTable[stArray,   stList]    := addArrayListfunc;
  addJumpTable[stArray,   stArray]   := addArrayArrayfunc;
  addJumpTable[stVector,  stVector]  := addVectorVectorfunc;

  addJumpTable[stMatrix,  stMatrix]  := addMatrixMatrixfunc;
  addJumpTable[stInteger, stMatrix]  := addIntegerMatrixfunc;
  addJumpTable[stMatrix,  stInteger] := addMatrixIntegerfunc;
  addJumpTable[stMatrix,  stDouble]  := addMatrixDoublefunc;
  addJumpTable[stDouble,  stMatrix]  := addDoubleMatrixfunc;

  addJumpTable[stInteger,  stArray]  := addIntArrayfunc;
  addJumpTable[stArray,  stInteger]  := addArrayIntfunc;
  addJumpTable[stDouble,   stArray]  := addDoubleArrayfunc;
  addJumpTable[stArray,   stDouble]  := addArrayDoublefunc;

  addJumpTable[stArray,  stValueObject]  := addArrayValuefunc;
  addJumpTable[stValueObject,  stArray]  := addValueArrayfunc;
  addJumpTable[stArray,   stValueObject]  := addValueArrayfunc;
  addJumpTable[stValueObject,   stArray]  := addArrayValuefunc;

  // -----------------------------------------------------------------------
  // Subraction
  for s1 := stNone to stObject do
      for s2 := stNone to stObject  do
          subJumpTable[s1, s2] := subErrorFunc;

  subJumpTable[stInteger, stInteger] := subIntIntfunc;
  subJumpTable[stInteger, stDouble]  := subIntDoublefunc;
  subJumpTable[stDouble,  stInteger] := subDoubleIntfunc;
  subJumpTable[stDouble,  stDouble]  := subDoubleDoublefunc;
  subJumpTable[stInteger,  stValueObject] := subIntegerValueObjectfunc;
  subJumpTable[stValueObject, stInteger] := subValueObjectIntegerfunc;
  subJumpTable[stDouble,  stValueObject] := subDoubleValueObjectfunc;
  subJumpTable[stValueObject, stDouble] := subValueObjectDoublefunc;

  subJumpTable[stInteger,stArray]    := subIntArrayfunc;
  subJumpTable[stArray,   stInteger] := subArrayIntfunc;
  subJumpTable[stDouble,   stArray]  := subDoubleArrayfunc;
  subJumpTable[stArray, stDouble]    := subArrayDoublefunc;
  subJumpTable[stArray, stArray]    := subArrayArrayfunc;
  subJumpTable[stArray, stValueObject] := subArrayValuefunc;
  subJumpTable[stValueObject, stArray]  := subValueArrayfunc;


  subJumpTable[stMatrix,  stMatrix]  := subMatrixMatrixfunc;
  subJumpTable[stInteger, stMatrix]  := subIntMatrixfunc;
  subJumpTable[stMatrix,  stInteger] := subMatrixIntfunc;
  subJumpTable[stMatrix,  stDouble]  := subMatrixDoublefunc;
  subJumpTable[stDouble,  stMatrix]  := subDoubleMatrixfunc;

  // -----------------------------------------------------------------------
  // Muliplication
  for s1 := stNone to stObject do
      for s2 := stNone to stObject  do
          multJumpTable[s1, s2] := multErrorFunc;

  multJumpTable[stInteger,  stInteger]  := multIntIntfunc;
  multJumpTable[stInteger,  stDouble]   := multIntDoublefunc;
  multJumpTable[stInteger,  stValueObject] := multIntValuefunc;
  multJumpTable[stInteger,  stArray]     := multIntegerArrayfunc;
  multJumpTable[stInteger,  stString]    := multIntStringfunc;
  multJumpTable[stInteger,  stList]      := multIntListfunc;
  multJumpTable[stInteger,  stMatrix]    := multIntMatrixfunc;

  multJumpTable[stDouble,   stInteger]    := multDoubleIntfunc;
  multJumpTable[stDouble,   stDouble]     := multDoubleDoublefunc;
  multJumpTable[stDouble,   stValueObject] := multDoubleValuefunc;
  multJumpTable[stDouble,   stArray]      := multDoubleArrayfunc;
  multJumpTable[stDouble,   stMatrix]     := multDoubleMatrixfunc;

  multJumpTable[stValueObject, stDouble]  := multValueDoublefunc;
  multJumpTable[stValueObject, stInteger] := multValueIntfunc;
  multJumpTable[stValueObject, stArray]   := multValueArrayfunc;
  multJumpTable[stValueObject, stMatrix]  := multValueMatrixfunc;

  multJumpTable[stArray,     stInteger]  := multArrayIntegerfunc;
  multJumpTable[stArray,     stDouble]   := multArrayDoublefunc;
  multJumpTable[stArray,     stArray]    := multArrayArrayfunc;
  multJumpTable[stArray,  stValueObject] := multArrayValuefunc;

  multJumpTable[stMatrix,     stInteger] := multMatrixIntfunc;
  multJumpTable[stMatrix,     stDouble]  := multMatrixDoublefunc;
  multJumpTable[stMatrix,     stMatrix]  := multMatrixMatrixfunc;
  multJumpTable[stMatrix,     stValueObject] := multMatrixValuefunc;

  multJumpTable[stList,      stInteger]  := multListIntfunc;

  multJumpTable[stString,    stInteger]  := multStringIntfunc;

  // -----------------------------------------------------------------------
  // Division
  for s1 := stNone to stObject do
      for s2 := stNone to stObject  do
          divJumpTable[s1, s2] := divErrorFunc;

  divJumpTable[stInteger,  stInteger] := divIntIntfunc;
  divJumpTable[stDouble,   stDouble]  := divDoubleDoublefunc;
  divJumpTable[stDouble,   stInteger] := divDoubleIntfunc;
  divJumpTable[stInteger,  stDouble]  := divIntDoublefunc;

  divJumpTable[stInteger,     stValueObject] := divIntValuefunc;
  divJumpTable[stValueObject, stDouble]      := divValueDoublefunc;
  divJumpTable[stValueObject, stInteger]     := divValueIntfunc;
  divJumpTable[stDouble,     stValueObject]  := divDoubleValuefunc;

  divJumpTable[stInteger,   stArray]    := divIntArrayfunc;
  divJumpTable[stArray,     stInteger]  := divArrayIntfunc;
  divJumpTable[stDouble,    stArray]    := divDoubleArrayfunc;
  divJumpTable[stArray,    stDouble]    := divArrayDoublefunc;

  divJumpTable[stValueObject,  stArray]    := divValueArrayfunc;
  divJumpTable[stArray,    stValueObject]  := divArrayValuefunc;
end;


initialization
  create_addJumpTable;
end.

