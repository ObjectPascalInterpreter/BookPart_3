unit uMatrixObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2024 Herbert M Sauro

// Author Contact Informion:
// email: hsauro@gmail.com

interface

uses SysUtils, Classes,
     uMemoryManager,
     uDataObjectMethods,
     uDataObject,
     uArrayObject,
     uVectorObject,
     Generics.Collections,
     uVMExceptions,
     uHelpUnit,
     uRhodusTypes;

type
  TMatrixMethods = class (TMethodsBase)
     procedure   getNumRows (vm: TObject);
     procedure   getNumCols (vm: TObject);
     procedure   getShape (vm : TObject);
     procedure   getColumn (vm : TObject);
     procedure   getRow (vm : TObject);

     procedure   getToArray (vm : TObject);
     procedure   getToList (vm : TObject);

     constructor Create;
     destructor  Destroy; override;
  end;


  TRow = TArray<Extended>;
  TMatrixObject = class (TDataObject)
    private
       data : TArray<TRow>;
    public

    class var matrixMethods : TMatrixMethods;

    function  numRows : integer;
    function  numCols : integer;
    procedure setval (ri, ci : integer; value : Extended);
    function  getval (ri, ci : integer) : Extended;
    function  getRow (index : integer) : TRow;

    class function  add (m : TMatrixObject; value : double) : TMatrixObject; overload;
    class function  add (m1, m2 : TMatrixObject) : TMatrixObject; overload;

    class function  subLeft (m : TMatrixObject; value : double) : TMatrixObject; overload;
    class function  subRight (m : TMatrixObject; value : double) : TMatrixObject; overload;
    class function  sub (m1, m2 : TMatrixObject) : TMatrixObject; overload;
    class function  minus (m : TMatrixObject) : TMatrixObject;
    class function  scalarMult (m1 : TMatrixObject; alpha : double) : TMatrixObject;
    class function  dotmult (m1, m2 : TMatrixObject) : TMatrixObject;
    class function  mult (m1, m2 : TMatrixObject) : TMatrixObject;
    class function  isEqualTo (m1, m2 : TMatrixObject) : boolean;
    class function  transpose (m : TMatrixObject) : TMatrixObject;
    class function  applyUniFunction (obj : TDataObject; func : TUniFunction) : TDataObject;

    class function  toList (m : TMatrixObject) : TDataObject;  // Due to circular reference issue
    class function  toArray (m : TMatrixObject) : TArrayObject;

    procedure setNumRows (n : integer);
    procedure swapRows (i, j : integer);
    procedure addRow (index : integer; vec : TVectorObject);
    function  clone : TDataObject; override;
    function  ToString: string; override;
    function  getSize : integer; override;

    function  slice (var slices : TSliceObjectList) : TMatrixObject;

    constructor     Create (numrows, numcols : integer); overload;
    constructor     CreateIdent (n : integer);
    constructor     Create; overload;
    destructor      Destroy; override;

    property Item[index1, index2: Integer]: Extended read getval write setval; default;
    property row[index : integer] : TRow read getRow;
  end;


  procedure createAndAttachMethods;


implementation

Uses Math, uVM,
     uUtils,
     uListObject,
     uBuiltInMath,
     uBuiltInGlobal,
     uMachineStack,
     uValueObject,
     uStringObject,
     uSymbolTable;



procedure createAndAttachMethods;
begin
  TMatrixObject.matrixMethods := TMatrixMethods.Create;
end;


constructor TMatrixMethods.Create;
begin
  methodList := TMethodList.Create (self);

  self.helpStr := 'Matrix Type';

  // -1 means variable arguments, use the constant VARIABLE_ARGS for this

  methodList.Add(TMethodDetails.Create ('rows', 'MatrixObject', 0, getNumRows));
  methodList.Add(TMethodDetails.Create ('cols', 'MatrixObject', 0, getNumCols));
  methodList.Add(TMethodDetails.Create ('shape',   0, 'get the shape of the matrix var.shape()', getShape));
  methodList.Add(TMethodDetails.Create ('row',     1, 'extract a column: var.ec(n)', getRow));
  methodList.Add(TMethodDetails.Create ('col',     1, 'extract a row: var.er(n)', getColumn));

  methodList.Add(TMethodDetails.Create ('toArray', 0, 'Convert a matrix to an array: a = m.toArray()', getToArray));
  methodList.Add(TMethodDetails.Create ('toList',  0, 'Convert a matrix to a list: l = m.toList()', getToList));
end;

destructor TMatrixMethods.Destroy;
begin
  inherited;
end;


procedure TMatrixMethods.getNumRows (vm : TObject);
var m : TMatrixObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   m := TMatrixObject (md.self);

  TVM (vm).push(m.numRows);
end;


procedure TMatrixMethods.getNumCols (vm : TObject);
var m : TMatrixObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   m := TMatrixObject (md.self);

  TVM (vm).push(m.numCols);
end;


procedure TMatrixMethods.getShape (vm : TObject);
var s : TMatrixObject;
    r : TListObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TMatrixObject (md.self);

  r := TListObject.Create(2);
  r.list[0].itemType := symInteger;
  r.list[0].iValue := s.numRows;
  r.list[1].itemType := symInteger;
  r.list[1].iValue := s.numCols;
  TVM (vm).push(r);
end;


procedure TMatrixMethods.getColumn (vm : TObject);
var s : TMatrixObject;
    m : TMatrixObject;
    i : integer;
    n : integer;
    md : TMethodDetails;
begin
  n := TVM (vm).popInteger;

  md := TVM (vm).popMethodDetails;
  s := TMatrixObject (md.self);

  if (n < 0) or (n > s.numCols) then
     raise ERuntimeException.Create('Column index out of range');

  m := TMatrixObject.Create (s.numRows, 1);
  for i := 0 to s.numRows - 1 do
      m[i, 0] := s[i, n];

  TVM (vm).push(m);
end;


procedure TMatrixMethods.getRow (vm : TObject);
var s : TMatrixObject;
    m : TMatrixObject;
    i : integer;
    n : integer;
    md : TMethodDetails;
begin
  n := TVM (vm).popInteger;

  md := TVM (vm).popMethodDetails;
  s := TMatrixObject (md.self);

  if (n < 0) or (n > s.numRows) then
     raise ERuntimeException.Create('Row index out of range');

  m := TMatrixObject.Create (1, s.numCols);
  for i := 0 to s.numRows - 1 do
      m[0, i] := s[n, i];

  TVM (vm).push(m);
end;


procedure TMatrixMethods.getToArray (vm : TObject);
var m : TMatrixObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   m := TMatrixObject (md.self);

  TVM (vm).push(TMatrixObject.toArray(m));
end;


procedure TMatrixMethods.getToList (vm : TObject);
var m : TMatrixObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   m := TMatrixObject (md.self);

  TVM (vm).push(TMatrixObject.toList(m) as TListObject);
end;

// -----------------------------------------------------------------------------------

constructor TMatrixObject.Create;
begin
  inherited Create; // Adds object to the memory pool
  blockType := btGarbage;
  objectType := symMatrix;
  self.methods := TMatrixObject.matrixMethods;
end;


constructor TMatrixObject.Create (numrows, numcols : integer);
var i : integer;
begin
  Create;
  setLength (self.data, numrows);
  for i := 0 to numrows - 1 do
      begin
      setlength (self.data[i], numcols);
      end;
end;


constructor  TMatrixObject.CreateIdent (n : integer);
var i : integer;
begin
  Create (n, n);
  for i := 0 to n - 1 do
      self[i,i] := 1;
end;


destructor TMatrixObject.Destroy;
begin
  inherited;
end;


function TMatrixObject.numRows : integer;
begin
  result := length (data);
end;


function TMatrixObject.numCols : integer;
begin
 result := length (data[0]);
end

;
procedure TMatrixObject.setval (ri, ci : integer; value : Extended);
begin
  data[ri][ci] := value;
end;


function TMatrixObject.getval (ri, ci : integer) : Extended;
begin
  result := data[ri,ci];
end;


function TMatrixObject.getRow (index : integer) : TRow;
begin
  result := data[index];
end;


class function TMatrixObject.add (m : TMatrixObject; value : double) : TMatrixObject;
var i, j : integer;
begin
  result := TMatrixObject.Create (m.numRows, m.numCols);
  for i := 0 to m.numrows - 1 do
      for j := 0 to m.numcols - 1 do
          result.setval(i, j, m.getval(i, j) + value);
end;


class function TMatrixObject.add (m1, m2 : TMatrixObject) : TMatrixObject;
var i, j : integer;
begin
  if (m1.numRows = m2.numRows) and (m1.numcols = m2.numCols) then
     begin
     result := TMatrixObject.Create (m1.numRows, m2.numCols);
    for i := 0 to m1.numrows - 1 do
       for j := 0 to m1.numcols - 1 do
          result.setval(i, j, m1.getval(i, j) + m2.getval(i, j));
    end
 else
   raise ERuntimeException.Create ('Matrices must have the same dimensions in add');
end;


class function TMatrixObject.subLeft (m : TMatrixObject; value : double) : TMatrixObject;
var i, j : integer;
begin
  result := m.clone as TMatrixObject;
  for i := 0 to m.numrows - 1 do
      for j := 0 to m.numcols - 1 do
          result.setval(i, j, m.getval(i, j) - value);
end;


class function TMatrixObject.subRight (m : TMatrixObject; value : double) : TMatrixObject;
var i, j : integer;
begin
  result := m.clone as TMatrixObject;
  for i := 0 to m.numrows - 1 do
      for j := 0 to m.numcols - 1 do
          result.setval(i, j, value - m.getval(i, j));
end;


class function TMatrixObject.sub (m1, m2 : TMatrixObject) : TMatrixObject;
var i, j : integer;
begin
  if (m1.numRows = m2.numRows) and (m1.numcols = m2.numCols) then
     begin
     result := TMatrixObject.Create (m1.numRows, m2.numCols);
     for i := 0 to m1.numrows - 1 do
       for j := 0 to m1.numcols - 1 do
          result.setval(i, j, m1.getval(i, j) - m2.getval(i, j));
     end
 else
   raise ERuntimeException.Create ('Matrices must have the same dimensions in sub');
end;


class function TMatrixObject.minus (m : TMatrixObject) : TMatrixObject;
var i, j : integer;
begin
   result := TMatrixObject.Create(m.numRows, m.numCols);
   for i := 0 to m.numrows - 1 do
       for j := 0 to m.numcols - 1 do
          result.setval(i, j, -m.getval(i, j));
end;


class function TMatrixObject.scalarMult (m1 : TMatrixObject; alpha : double) : TMatrixObject;
var i, j : integer;
begin
  result := TMatrixObject.Create (m1.numRows, m1.numCols);
  for i := 0 to m1.numrows - 1 do
     for j := 0 to m1.numcols - 1 do
         result.data[i,j] := alpha*m1.data[i,j];
end;


// This does a dot product
class function TMatrixObject.dotmult (m1, m2 : TMatrixObject) : TMatrixObject;
var i, j, k : integer;
    sum : double;
begin
  result := TMatrixObject.Create (m1.numRows, m2.numCols);
  if (m1.numCols = m2.numRows) then  // if cols = row?
     begin
	   for i := 0 to m1.numRows - 1 do
		     for j := 0 to m2.numCols - 1 do
           begin
           sum := 0;
           for k := 0 to m1.numCols - 1 do
					      sum := sum + m1.getval(i,k) * m2.getval(k,j);
           result.setval(i,j, sum);
					end;
		end
  else
     raise ERuntimeException.Create ('Incompatible matrix operands to multiply');
end;


// This does a term by term multiplication
class function TMatrixObject.mult (m1, m2 : TMatrixObject) : TMatrixObject;
var i, j : integer;
begin
  if (m1.numRows = m2.numRows) and (m1.numCols = m2.numCols) then  // if cols = row?
     begin
     result := TMatrixObject.Create (m1.numRows, m1.numCols);
	   for i := 0 to m1.numRows - 1 do
		     for j := 0 to m1.numCols - 1 do
					   result.setval(i,j, m1.getval(i,j) * m2.getval(i,j));
		end
  else
     raise ERuntimeException.Create ('Incompatible matrix operands to multiply term by term');
end;


class function TMatrixObject.isEqualTo (m1, m2 : TMatrixObject) : boolean;
var i, j : integer;
    epsSymbol : TSymbol;
begin
  epsSymbol := mainModule.find('math', 'eps');

  if (m1.numRows = m1.numRows) and (m1.numCols = m2.numCols) then
     begin
     for i := 0 to m1.numrows - 1 do
       for j := 0 to m1.numcols - 1 do
           begin
           if not sameValue (m1.data[i,j], m2.data[i,j], (epsSymbol.dataObject as TValueObject).dValue) then
              exit (False);
           end;

     exit (True);
     end
  else
     exit (False);
end;


class function TMatrixObject.toList (m : TMatrixObject) : TDataObject;
var i, j : integer;
    l : TListObject;
    row : TListObject;
begin
  l := TListObject.Create;
  // Create the rows, each row is numCols wide
  for i := 0 to m.numRows - 1 do
      l.append (TListObject.Create(m.numCols));
  // Set each items that ow exists in each row to a value
  for i := 0 to m.numRows - 1 do
      begin
      row := TListObject (l[i].dataObject);
      for j := 0 to m.numCols - 1 do
          row.setItemToDouble(j, m[i,j]);
      end;

  result := l;
end;


class function TMatrixObject.toArray (m : TMatrixObject) : TArrayObject;
var i, j : integer;
begin
  result := TArrayObject.Create([m.numRows, m.numCols]);
  for i := 0 to m.numRows - 1 do
      for j := 0 to m.numCols - 1 do
          result.setValue2D(i, j, m[i,j]);
end;


// -----------------------------------------------------------------------------------------------
procedure TMatrixObject.setNumRows (n : integer);
begin
  setlength (data, n);
end;


procedure TMatrixObject.swapRows (i, j : integer);
var r : TRow;
begin
  if i = j then
     exit;

  r := self.data[i];
  self.data[i] := self.data[j];
  self.data[j] := r;
end;

// We must call setNumRows first
// Copy the numbers in vec to row index
procedure TMatrixObject.addRow (index : integer; vec : TVectorObject);
begin
  setlength (data[index], vec.size);
  self.data[index] := Copy (vec.data, 0);
end;


function TMatrixObject.clone : TDataObject;
var i : integer;
begin
   result := TMatrixObject.Create (self.numRows, self.numCols);
   for i := 0 to length (self.data) - 1 do
       begin
       (result as TMatrixObject).data[i] := Copy (self.data[i], 0);
       //len := length (self.data[i]);
       //TArray.Copy(self.data[i].data, result.data[i].data, 0, 0,  len);
       end;
end;


// If we've got this far it means there is at least one slice
// in the index. it is possible that the nunber of slices is
// less then the number of dimensions of the array, this means
// we must fill out the remining slices as :
// eg 3D array where the user has provided the following slice
// [1:3,4:10] must be expanded to [1:3,4:10,:]
//
// Slices are encoded  with SLICE_ALL to represent : or SLICE_EQUAL
// for something line [3:3]
// For a mix slices with slices and indexes, the index must be turned into
// a SLICE_ALL
// eg [:,0] is converted to [:,0:0]
//
// Must be var because the length of slices can change and the caller
// needs to be able free any new slice objects. Nonvar arrays are copied
// when a call is made.
function TMatrixObject.slice (var slices : TSliceObjectList) : TMatrixObject;
var i, j : integer;
    count : integer;
    inputLists : TIntLists;
    alist : TIntList;
    cp : TCartesianProduct;
    dim : array of integer;
    coord : TArray<integer>;
    nr, nc : integer;
begin
  nr := self.numRows;
  nc := self.numCols;

  // Check for missing slices
  if length (slices) < 2 then
     begin
     // fill out the missing slices which should all be ':' (SLICE_ALL)
     for i := length (slices) to 2 - 1 do
         begin
         setLength (slices, length (slices)+1);
         slices[length (slices)-1] := TSliceObject.Create(0, SLICE_ALL);
         end;
     end;

  setlength (dim, 2);
  dim[0] := nr;
  dim[1] := nc;

  // Create space for the number of dimensions of the slice.
  // setlength (result.dim, length (slices));
  // Compute the number of elements that will be in the slice
  // eg [1:2,3:5] will have 6 elements
  // At this point we also convert the SLICE_ALL and SLICE_EQUAL to coordinates
  //slicesize := 1;
  for i := 0 to length (slices) - 1 do
      begin
      if slices[i].lower = SLICE_ALL then
         slices[i].lower := 0;
      if slices[i].upper = SLICE_ALL then
         slices[i].upper := dim[i] - 1;
      if slices[i].upper = SLICE_EQUAL then  // eg m[:,4] where the 4 means slice 4 to 4
         slices[i].upper := slices[i].lower;

      if slices[i].lower < 0 then
         slices[i].lower := 0;
      if slices[i].upper > self.numRows - 1 then
         slices[i].upper := dim[i] - 1;

      // Store the size of the dimension in dim[i]
      dim[i] := slices[i].upper - slices[i].lower+1;
      if dim[i] <= 0  then
         raise ERuntimeException.Create('Slice dimensions exceeding size of matrix');
      //slicesize := slicesize * (slices[i].upper - slices[i].lower+1);
      end;

  // Allocate space for the slice
  result := TMatrixObject.Create (dim[0], dim[1]);

  // Generate the coordinates for each item that we need to copy from the source array
  setlength (inputLists, length (slices));
  for i := 0 to length (slices) - 1 do
      begin
      setLength (alist, dim[i]);
      for j := slices[i].lower to slices[i].upper do
          alist[j-slices[i].lower] := j;
      inputLists[i] := alist;
      end;
  cp := TCartesianProduct.Create (inputlists);
  try
    // Copy all data over to target
    count := 0;
    for i := 0 to dim[0] - 1 do
        for j := 0 to dim[1] - 1 do
            begin
            coord := cp.getIthCartesianProduct(count);
            result.data[i, j] := self.getVal(coord[0], coord[1]);
            inc (count);
            end;
  finally
    cp.free;
  end;
end;


class function TMatrixObject.applyUniFunction (obj : TDataObject; func : TUniFunction) : TDataObject;
var i, j : integer;
    m : TMatrixObject;
begin
  m := TMatrixObject (obj);
  result := TMatrixObject.Create (m.numCols, m.numRows);
  for i := 0 to m.numRows - 1 do
      for j := 0 to m.numCols - 1 do
          (result as TMatrixObject).setval(i, j, func (m[i,j]));
end;



class function TMatrixObject.transpose (m : TMatrixObject) : TMatrixObject;
var i, j : integer;
begin
  result := TMatrixObject.Create (m.numCols, m.numRows);
  for i := 0 to m.numRows - 1 do
      for j := 0 to m.numCols - 1 do
          result.setval(j,i, m.getval(i,j));
end;



function  TMatrixObject.toString: string;
var i, j : integer;
    formatStr : string;
begin
  formatStr := (SysLibraryRef.find ('doubleFormat').dataObject as TStringObject).value;
  result := '{';
  for i := 0 to length (self.data) - 1 do
      begin
      result := result + '{';
         for j := 0 to length (self.data[i]) - 1 do
             begin
             result := result + Format(formatStr, [self.data[i,j]]);
             if j < length (self.data[i]) - 1 then
                result := result + ', ';
            end;
        if i < length (self.data) - 1 then
           result := result + '}, ' + sLineBreak;
     end;
  result := result + '}}';
end;


function TMatrixObject.getSize : integer;
begin
  result := numRows*numCols;
end;


initialization
   // Initialize the class varialbe tha tpoints to the methods list
   //TMatrixObject.matrixMethods := TMatrixMethods.Create;
finalization
   TMatrixObject.matrixMethods.Free;
end.

