unit uJumpTables;

interface

Uses Classes, SysUtils, uMachineStack, uRhodusTypes;

type
  TBinOp = procedure (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
  TJumpTable = array[TElementType,TElementType] of TBinOp;

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
     uDataObject,
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
  result.stackType := symInteger;
end;

procedure addIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue + d2.iValue;
  result.stackType := symDouble;
end;

procedure addDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue + d2.dValue;
  result.stackType := symDouble;
end;

procedure addDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue + d2.dValue;
  result.stackType := symDouble;
end;

procedure addIntValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.dataObject as TValueObject) + d2.iValue;
  result.stackType := symDouble;
end;

procedure addValueObjectIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d2.dataObject as TValueObject) + d1.iValue;
  result.stackType := symDouble;
end;

procedure addDoubleValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.dataObject as TValueObject) + d2.dValue;
  result.stackType := symDouble;
end;

procedure addValueObjectDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d2.dataObject as TValueObject) + d1.dValue;
  result.stackType := symDouble;
end;

procedure addStringStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TStringObject.add(TStringObject(d2.dataObject), TStringObject(d1.dataObject));
  result.stackType := symString;
end;

procedure addListIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.dataObject := d2.dataObject.clone;
   TListObject (result.dataObject).append(d1.iValue);
   result.stackType := symList;
end;

procedure addIntListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.dataObject := d1.dataObject.clone;
   TListObject (result.dataObject).insert(0, d2.iValue);
   result.stackType := symList;
end;

procedure addBoolListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d1.dataObject.clone;
  TListObject (result.dataObject).insert (0, d2.bValue);
  result.stackType := symList;
end;

procedure addListBoolfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d2.dataObject.clone;
  TListObject (result.dataObject).append(d1.bValue);
  result.stackType := symList;
end;

procedure addDoubleListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d1.dataObject.clone;
  TListObject (result.dataObject).insert (0, d2.dValue);
  result.stackType := symList;
end;

procedure addLisymDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d2.dataObject.clone;
  TListObject (result.dataObject).append(d1.dValue);
  result.stackType := symList;
end;

procedure addStringListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TDataObject;
begin
  result.dataObject := d1.dataObject.clone;
  tmp  := d2.dataObject.clone;
  tmp.blockType := btOwned;
  TListObject (result.dataObject).insert(0, tmp);
  result.stackType := symList;
end;

procedure addLisymStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d2.dataObject.clone;
  TListObject (result.dataObject).append(d1.dataObject.clone);
  result.stackType := symList;
end;


procedure addLisymListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
   result.dataObject := TListObject.addLists(TListObject (d2.dataObject), TListObject (d1.dataObject));
   result.stackType := symList
end;


procedure addFuncListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TDataObject;
begin
  result.dataObject := d1.dataObject.clone ;
  tmp := TUserFunction (d2.dataObject).clone;
  tmp.blockType := btOwned;
  TListObject (result.dataObject).insert (0, tmp);
  result.stackType := symList;
end;


procedure addListFuncfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var tmp : TDataObject;
begin
  result.dataObject := d2.dataObject.clone;
  tmp := TUserFunction (d1.dataObject).clone;
  tmp.blockType := btOwned;
  TListObject (result.dataObject).append(tmp);
  result.stackType := symList;
end;

procedure addLisymModulefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d2.dataObject.clone;
  TListObject (result.dataObject).appendModule (d1.module);
  result.stackType := symList;
end;

procedure addModuleListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d1.dataObject.clone;
  TListObject (result.dataObject).appendModule (d2.module);
  result.stackType := symList;
end;

procedure addLisymArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d2.dataObject.clone;
  TListObject (result.dataObject).append(d1.dataObject.clone);
  result.stackType := symList;
end;

procedure addArrayListfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := d1.dataObject.clone;
  TListObject (result.dataObject).append(d2.dataObject.clone);
  result.stackType := symList;
end;

procedure addIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d1.dataObject), d2.iValue);
  result.stackType := symArray;
end;

procedure addArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d2.dataObject), d1.iValue);
  result.stackType := symArray;
end;

procedure addDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d1.dataObject), d2.dValue);
  result.stackType := symArray;
end;

procedure addArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d2.dataObject), d1.dValue);
  result.stackType := symArray;
end;


procedure addArrayArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d2.dataObject), TArrayObject (d1.dataObject));
  result.stackType := symArray;
end;

procedure addVectorVectorfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TVectorObject.add(TVectorObject (d2.dataObject), TVectorObject (d1.dataObject));
  result.stackType := symVector;
end;


procedure addMatrixMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.add(TMatrixObject (d2.dataObject), TMatrixObject (d1.dataObject));
  result.stackType := symMatrix;
end;


procedure addIntegerMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.add (TMatrixObject (d1.dataObject), double (d2.iValue));
  result.stackType := symMatrix;
end;

procedure addMatrixIntegerfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.add (TMatrixObject (d2.dataObject), double (d1.iValue));
  result.stackType := symMatrix;
end;


procedure addMatrixDoublefunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.add (TMatrixObject (d2.dataObject),  d1.dValue);
  result.stackType := symMatrix;
end;


procedure addDoubleMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.add (TMatrixObject (d1.dataObject), d2.dValue);
  result.stackType := symMatrix;
end;


procedure addValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d2.dataObject), (d1.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;

procedure addArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.add(TArrayObject (d1.dataObject), (d2.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;


// ----------------------------------------------------------------------------------
// Subtraction operations

procedure subIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.iValue := d1.iValue - d2.iValue;
  result.stackType := symInteger;
end;

procedure subIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - d2.iValue;
  result.stackType := symDouble;
end;

procedure subDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue - d2.dValue;
  result.stackType := symDouble;
end;

procedure subDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - d2.dValue;
  result.stackType := symDouble;
end;

procedure subIntegerValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.dataObject as TValueObject) - d2.iValue;
  result.stackType := symDouble;
end;

procedure subValueObjectIntegerfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue - TValueObject.getValue(d2.dataObject as TValueObject);
  result.stackType := symDouble;
end;

procedure subDoubleValueObjectfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := TValueObject.getValue(d1.dataObject as TValueObject) - d2.dValue;
  result.stackType := symDouble;
end;

procedure subValueObjectDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue - TValueObject.getValue(d2.dataObject as TValueObject);
  result.stackType := symDouble;
end;


procedure subIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subleft(TArrayObject (d1.dataObject), d2.iValue);
  result.stackType := symArray;
end;

procedure subArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subRight(TArrayObject (d2.dataObject), d1.iValue);
  result.stackType := symArray;
end;

procedure subDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subLeft(TArrayObject (d1.dataObject), d2.dValue);
  result.stackType := symArray;
end;

procedure subArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subRight(TArrayObject (d2.dataObject), d1.dValue);
  result.stackType := symArray;
end;

procedure subArrayArrayfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.sub(TArrayObject (d1.dataObject), TArrayObject (d2.dataObject));
  result.stackType := symArray;
end;

//procedure addVectorVectorfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
//begin
//  result.vValue := TVectorObject.add(d2.vValue, d1.vValue);
//  result.stackType := symVector;
//end;


procedure subMatrixMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.sub(TMatrixObject (d1.dataObject), TMatrixObject (d2.dataObject));
  result.stackType := symMatrix;
end;


procedure subIntMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.subLeft (TMatrixObject (d1.dataObject), double (d2.iValue));
  result.stackType := symMatrix;
end;

procedure subMatrixIntfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.subRight (TMatrixObject (d2.dataObject), double (d1.iValue));
  result.stackType := symMatrix;
end;


procedure subMatrixDoublefunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.subRight (TMatrixObject (d2.dataObject), d1.dValue);
  result.stackType := symMatrix;
end;


procedure subDoubleMatrixfunc  (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.subLeft (TMatrixObject (d1.dataObject), d2.dValue);
  result.stackType := symMatrix;
end;


procedure subValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subLeft(TArrayObject (d1.dataObject), (d2.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;

procedure subArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.subRight(TArrayObject (d2.dataObject), (d1.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;


// ----------------------------------------------------------------------------------
// Muliplication Operations

procedure multIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.iValue := d1.iValue * d2.iValue;
  result.stackType := symInteger;
end;

procedure multDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue * d2.dValue;
  result.stackType := symDouble;
end;

procedure multIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * d2.iValue;
  result.stackType := symDouble;
end;

procedure multDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * d2.dValue;
  result.stackType := symDouble;
end;


procedure multIntValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := (d1.dataObject as TValueObject).getScalar() * d2.iValue;
  result.stackType := symDouble;
end;


procedure multValueIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue * (d2.dataObject as TValueObject).getScalar();
  result.stackType := symDouble;
end;


procedure multDoubleValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := (d1.dataObject as TValueObject).getScalar() * d2.dValue;
  result.stackType := symDouble;
end;


procedure multValueDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue * (d2.dataObject as TValueObject).getScalar();
  result.stackType := symDouble;
end;

//###
procedure multValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult(TArrayObject (d1.dataObject), (d2.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;

procedure multArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult (TArrayObject (d2.dataObject), (d1.dataObject as TValueObject).getScalar());
  result.stackType := symArray;
end;

procedure multValueMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d1.dataObject), (d2.dataObject as TValueObject).getScalar());
  result.stackType := symMatrix;
end;

procedure multIntegerArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult (TArrayObject (d1.dataObject), d2.iValue);
  result.stackType := symArray;
end;

procedure multArrayIntegerfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult (TArrayObject (d2.dataObject), d1.iValue);
  result.stackType := symArray;
end;

procedure multDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult (TArrayObject (d1.dataObject), d2.dValue);
  result.stackType := symArray;
end;

procedure multArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.arrayScalarDoubleMult (TArrayObject (d2.dataObject), d1.dValue);
  result.stackType := symArray;
end;

procedure multArrayArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.mult (TArrayObject (d2.dataObject), TArrayObject (d1.dataObject));
  result.stackType := symArray;
end;

// Strings

procedure multIntStringfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var i : integer;
    value : string;
begin
   value := TStringObject(d1.dataObject).value;
   for i := 2 to d2.iValue do
       value := value + TStringObject(d1.dataObject).value;

  result.dataObject := TStringObject.Create(value);
  result.stackType := symString;
end;


procedure multStringIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
var i : integer;
    value : string;
begin
   value := TStringObject(d2.dataObject).value;
   for i := 2 to d1.iValue do
       value := value + TStringObject(d2.dataObject).value;

  result.dataObject := TStringObject.Create(value);
  result.stackType := symString;
end;


// Lists

procedure multIntListfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TListObject.multiply(d2.iValue, TListObject (d1.dataObject));
  result.stackType := symList;
end;


procedure multListIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TListObject.multiply(d1.iValue, TListObject (d2.dataObject));
  result.stackType := symList;
end;

// -----No more list mults

procedure multIntMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d1.dataObject), d2.iValue);
  result.stackType := symMatrix;
end;


procedure multMatrixIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d2.dataObject), d1.iValue);
  result.stackType := symMatrix;
end;


procedure multMatrixDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d2.dataObject), d1.dValue);
  result.stackType := symMatrix;
end;

procedure multMatrixValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d2.dataObject), (d1.dataObject as TValueObject).getScalar());
  result.stackType := symMatrix;
end;


procedure multDoubleMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.scalarMult (TMatrixObject (d1.dataObject), d2.dValue);
  result.stackType := symMatrix;
end;


procedure multMatrixMatrixfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TMatrixObject.mult (TMatrixObject (d2.dataObject), TMatrixObject (d1.dataObject));
  result.stackType := symMatrix;
end;



// ----------------------------------------------------------------------------------
// Division operations

procedure divIntIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / d2.iValue;
  result.stackType := symDouble;
end;

procedure divDoubleDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / d2.dValue;
  result.stackType := symDouble;
end;


procedure divDoubleIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / d2.dValue;
  result.stackType := symDouble;
end;


procedure divIntDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / d2.iValue;
  result.stackType := symDouble;
end;

procedure divIntValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := (d1.dataObject as TValueObject).getScalar() / d2.iValue;
  result.stackType := symDouble;
end;


procedure divValueIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.iValue / (d2.dataObject as TValueObject).getScalar();
  result.stackType := symDouble;
end;

procedure divDoubleValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := (d1.dataObject as TValueObject).getScalar() / d2.dValue;
  result.stackType := symDouble;
end;


procedure divValueDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dValue := d1.dValue / (d2.dataObject as TValueObject).getScalar();
  result.stackType := symDouble;
end;

procedure divIntArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d1.dataObject), d2.iValue, False);
  result.stackType := symArray;
end;

procedure divArrayIntfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d2.dataObject), d1.iValue, True); // True = compute reciprical
  result.stackType := symArray;
end;

procedure divDoubleArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d1.dataObject), d2.dValue, False);
  result.stackType := symArray;
end;

procedure divArrayDoublefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d2.dataObject), d1.dValue, True);
  result.stackType := symArray;
end;

procedure divValueArrayfunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d1.dataObject), (d2.dataObject as TValueObject).getScalar(), False);
  result.stackType := symArray;
end;

procedure divArrayValuefunc (d1, d2 : PMachineStackRecord; var result : TMachineStackRecord);
begin
  result.dataObject := TArrayObject.divide (TArrayObject (d2.dataObject), (d1.dataObject as TValueObject).getScalar(), True);
  result.stackType := symArray;
end;

// ----------------------------------------------------------------------------------

procedure create_addJumpTable;
var s1, s2 : TElementType;
begin
  // This loop is used to mark every conbination of types with an error
  // function. We then assign actual functions for the ones we have.
  // This makes it easy to trap illegal combinations.
  for s1 := symNonExistant to symEndObject do
      for s2 := symNonExistant to symEndObject  do
          addJumpTable[s1, s2] := addErrorFunc;

  addJumpTable[symInteger, symInteger] := addIntIntfunc;
  addJumpTable[symInteger, symDouble]  := addIntDoublefunc;
  addJumpTable[symDouble,  symInteger] := addDoubleIntfunc;
  addJumpTable[symDouble,  symDouble]  := addDoubleDoublefunc;
  addJumpTable[symInteger,  symValueObject] := addIntValueObjectfunc;
  addJumpTable[symValueObject, symInteger] := addValueObjectIntfunc;
  addJumpTable[symDouble,  symValueObject] := addDoubleValueObjectfunc;
  addJumpTable[symValueObject, symDouble] := addValueObjectDoublefunc;

  addJumpTable[symString,  symString]  := addStringStringfunc;
  addJumpTable[symList,    symInteger] := addListIntfunc;
  addJumpTable[symInteger, symList]    := addIntListfunc;
  addJumpTable[symBoolean, symList]    := addBoolListfunc;
  addJumpTable[symList,    symBoolean] := addListBoolfunc;
  addJumpTable[symDouble,  symList]    := addDoubleListfunc;
  addJumpTable[symList,    symDouble]  := addLisymDoublefunc;
  addJumpTable[symString,  symList]    := addStringListfunc;
  addJumpTable[symList,    symString]  := addLisymStringfunc;
  addJumpTable[symList,    symList]    := addLisymListfunc;
  addJumpTable[symUserFunc, symList]   := addFuncListfunc;
  addJumpTable[symList,   symUserFunc] := addListFuncfunc;
  addJumpTable[symList,    symModule]  := addLisymModulefunc;
  addJumpTable[symModule,  symList]    := addModuleListfunc;
  addJumpTable[symList,    symArray]   := addLisymArrayfunc;
  addJumpTable[symArray,   symList]    := addArrayListfunc;
  addJumpTable[symArray,   symArray]   := addArrayArrayfunc;
  addJumpTable[symVector,  symVector]  := addVectorVectorfunc;

  addJumpTable[symMatrix,  symMatrix]  := addMatrixMatrixfunc;
  addJumpTable[symInteger, symMatrix]  := addIntegerMatrixfunc;
  addJumpTable[symMatrix,  symInteger] := addMatrixIntegerfunc;
  addJumpTable[symMatrix,  symDouble]  := addMatrixDoublefunc;
  addJumpTable[symDouble,  symMatrix]  := addDoubleMatrixfunc;

  addJumpTable[symInteger,  symArray]  := addIntArrayfunc;
  addJumpTable[symArray,  symInteger]  := addArrayIntfunc;
  addJumpTable[symDouble,   symArray]  := addDoubleArrayfunc;
  addJumpTable[symArray,   symDouble]  := addArrayDoublefunc;

  addJumpTable[symArray,  symValueObject]  := addArrayValuefunc;
  addJumpTable[symValueObject,  symArray]  := addValueArrayfunc;
  addJumpTable[symArray,   symValueObject]  := addValueArrayfunc;
  addJumpTable[symValueObject,   symArray]  := addArrayValuefunc;

  // -----------------------------------------------------------------------
  // Subraction
  for s1 := symNonExistant to symEndObject do
      for s2 := symNonExistant to symEndObject  do

          subJumpTable[s1, s2] := subErrorFunc;

  subJumpTable[symInteger, symInteger] := subIntIntfunc;
  subJumpTable[symInteger, symDouble]  := subIntDoublefunc;
  subJumpTable[symDouble,  symInteger] := subDoubleIntfunc;
  subJumpTable[symDouble,  symDouble]  := subDoubleDoublefunc;
  subJumpTable[symInteger,  symValueObject] := subIntegerValueObjectfunc;
  subJumpTable[symValueObject, symInteger] := subValueObjectIntegerfunc;
  subJumpTable[symDouble,  symValueObject] := subDoubleValueObjectfunc;
  subJumpTable[symValueObject, symDouble] := subValueObjectDoublefunc;

  subJumpTable[symInteger,symArray]    := subIntArrayfunc;
  subJumpTable[symArray,   symInteger] := subArrayIntfunc;
  subJumpTable[symDouble,   symArray]  := subDoubleArrayfunc;
  subJumpTable[symArray, symDouble]    := subArrayDoublefunc;
  subJumpTable[symArray, symArray]    := subArrayArrayfunc;
  subJumpTable[symArray, symValueObject] := subArrayValuefunc;
  subJumpTable[symValueObject, symArray]  := subValueArrayfunc;


  subJumpTable[symMatrix,  symMatrix]  := subMatrixMatrixfunc;
  subJumpTable[symInteger, symMatrix]  := subIntMatrixfunc;
  subJumpTable[symMatrix,  symInteger] := subMatrixIntfunc;
  subJumpTable[symMatrix,  symDouble]  := subMatrixDoublefunc;
  subJumpTable[symDouble,  symMatrix]  := subDoubleMatrixfunc;

  // -----------------------------------------------------------------------
  // Muliplication
  for s1 := symNonExistant to symEndObject do
      for s2 := symNonExistant to symEndObject  do
          multJumpTable[s1, s2] := multErrorFunc;

  multJumpTable[symInteger,  symInteger]  := multIntIntfunc;
  multJumpTable[symInteger,  symDouble]   := multIntDoublefunc;
  multJumpTable[symInteger,  symValueObject] := multIntValuefunc;
  multJumpTable[symInteger,  symArray]     := multIntegerArrayfunc;
  multJumpTable[symInteger,  symString]    := multIntStringfunc;
  multJumpTable[symInteger,  symList]      := multIntListfunc;
  multJumpTable[symInteger,  symMatrix]    := multIntMatrixfunc;

  multJumpTable[symDouble,   symInteger]    := multDoubleIntfunc;
  multJumpTable[symDouble,   symDouble]     := multDoubleDoublefunc;
  multJumpTable[symDouble,   symValueObject] := multDoubleValuefunc;
  multJumpTable[symDouble,   symArray]      := multDoubleArrayfunc;
  multJumpTable[symDouble,   symMatrix]     := multDoubleMatrixfunc;

  multJumpTable[symValueObject, symDouble]  := multValueDoublefunc;
  multJumpTable[symValueObject, symInteger] := multValueIntfunc;
  multJumpTable[symValueObject, symArray]   := multValueArrayfunc;
  multJumpTable[symValueObject, symMatrix]  := multValueMatrixfunc;

  multJumpTable[symArray,     symInteger]  := multArrayIntegerfunc;
  multJumpTable[symArray,     symDouble]   := multArrayDoublefunc;
  multJumpTable[symArray,     symArray]    := multArrayArrayfunc;
  multJumpTable[symArray,  symValueObject] := multArrayValuefunc;

  multJumpTable[symMatrix,     symInteger] := multMatrixIntfunc;
  multJumpTable[symMatrix,     symDouble]  := multMatrixDoublefunc;
  multJumpTable[symMatrix,     symMatrix]  := multMatrixMatrixfunc;
  multJumpTable[symMatrix,     symValueObject] := multMatrixValuefunc;

  multJumpTable[symList,      symInteger]  := multListIntfunc;

  multJumpTable[symString,    symInteger]  := multStringIntfunc;

  // -----------------------------------------------------------------------
  // Division
  for s1 := symNonExistant to symEndObject do
      for s2 := symNonExistant to symEndObject  do
          divJumpTable[s1, s2] := divErrorFunc;

  divJumpTable[symInteger,  symInteger] := divIntIntfunc;
  divJumpTable[symDouble,   symDouble]  := divDoubleDoublefunc;
  divJumpTable[symDouble,   symInteger] := divDoubleIntfunc;
  divJumpTable[symInteger,  symDouble]  := divIntDoublefunc;

  divJumpTable[symInteger,    symValueObject] := divIntValuefunc;
  divJumpTable[symValueObject, symDouble]      := divValueDoublefunc;
  divJumpTable[symValueObject, symInteger]     := divValueIntfunc;
  divJumpTable[symDouble,     symValueObject]  := divDoubleValuefunc;

  divJumpTable[symInteger,   symArray]    := divIntArrayfunc;
  divJumpTable[symArray,     symInteger]  := divArrayIntfunc;
  divJumpTable[symDouble,    symArray]    := divDoubleArrayfunc;
  divJumpTable[symArray,    symDouble]    := divArrayDoublefunc;

  divJumpTable[symValueObject,  symArray]    := divValueArrayfunc;
  divJumpTable[symArray,    symValueObject]  := divArrayValuefunc;
end;


initialization
  create_addJumpTable;
end.

