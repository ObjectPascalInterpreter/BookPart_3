unit uArrayObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes,
     uMemoryManager,
     uDataObjectMethods,
     Generics.Collections,
     uDataObject,
     uRhodusTypes;

type
  TArrayMethods = class;

  TIntArray = array of double;
  TFloatArray = array of double;

  TUniFunction = function (const value : extended) : extended;

  TArrayDataType = (adtInteger, adtDouble);

  TArrayObject = class (TDataObject)
    private
    // dataType : TArrayDataType;
    function  arrayRecursiveToString (var Indices : TArray<integer>; depth : integer) : string;
    function  NdimensionalArrayToString : string;
    public
     datai : TIntArray;
     dataf : TFloatArray;
     dim  : TIndexArray;

     class var arrayMethods : TArrayMethods;

     function        getNumDimensions : integer;
     function        getIndex (const dim, idx : array of integer) : integer;

     function        getValue (idx : array of integer) : double;
     function        getValue1D (i : integer) : double;
     function        getValue2D (i, j : integer) : double;

     procedure       setValue (idx : array of integer; value : double);
     procedure       setValue1D (i : integer; value : double);
     procedure       setValue2D (i, j : integer; value : double);

     function        getRow (index : integer) : TArrayObject;
     function        getDim : TIndexArray;
     function        getNumberOfElements : integer;
     function        getNthDimension (i : integer) : integer;
     function        getSize : integer; override;

     function        ToString () : string; override;

     function        clone : TDataObject; override;
     procedure       resize2D (n, m : integer);

     property        ndims : integer read getNumDimensions;
     property        item[index1, index2: Integer]: double read getValue2D write setValue2D; default;

     function        slice (var slices : TSliceObjectList) : TArrayObject;

     procedure       append (arrayArg : TArrayObject);

    class function   toList (a : TArrayObject) : TDataObject;  // Due to circular reference issue
    class function   toMatrix (a : TArrayObject) : TDataObject;

     class function  isEqualTo (a1, a2 : TArrayObject) : boolean;
     function        applyUniFunction (func : TUniFunction) : TArrayObject;

     class function  minus (a : TArrayObject) : TArrayObject;
     class function  add (a1, a2 : TArrayObject) : TArrayObject; overload;
     class function  add (a : TArrayObject; value : double) : TArrayObject; overload;
     class function  sub (a1, a2 : TArrayObject) : TArrayObject; overload;
     class function  subLeft (a : TArrayObject; value : double) : TArrayObject; overload;
     class function  subRight (a : TArrayObject; value : double) : TArrayObject; overload;
     class function  arrayScalarIntMult (m1 : TArrayObject; alpha : integer) : TArrayObject;
     class function  arrayScalarDoubleMult (m : TArrayObject; alpha : double) : TArrayObject;

     class function  mult (m1, m2 : TArrayObject) : TArrayObject;

     class function  divide (a : TArrayObject; value : double; reciprocal : boolean) : TArrayObject;
     class function  getMax (a1 : TArrayObject) : double;
     class function  getMin (a1 : TArrayObject) : double;
     constructor     Create; overload;
     constructor     Create (dim : TIndexArray); overload;
     constructor     Create (n : integer); overload;
     constructor     CreateDim;
     destructor      Destroy; override;
  end;

  TArrayMethods = class (TMethodsBase)
     procedure   getLength (vm : TObject);
     procedure   getShape (vm : TObject);
     procedure   getNumRows (vm: TObject);
     procedure   getNumCols (vm: TObject);
     procedure   getNumDim (vm : TObject);
     procedure   appendRow (vm : TObject);
     procedure   appendCol (vm : TObject);
     procedure   getTranspose (vm : TObject);
     procedure   getSqr (vm : TObject);
     procedure   toMatrix (vm : TObject);
     procedure   add (vm : TObject);
     procedure   sub (vm : TObject);
     procedure   getMax (vm : TObject);
     procedure   getMin (vm : TObject);
     procedure   getTrunc (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses SysUtils,
     StrUtils,
     Math,
     System.Character,
     uUtils,
     uMachineStack,
     uVM,
     uSymbolTable,
     uListObject,
     uValueObject,
     uVMExceptions,
     uMatrixObject,
     uStringObject,
     uBuiltInGlobal;

const outOfRangeMsg = 'Index out of range while accessing array element';
      sameDimensionsMsg = 'Arrays must have the same dimensions';

// var arrayMethods : TArrayMethods;

constructor TArrayMethods.Create;
begin
  methodList := TMethodList.Create (self);

  // -1 means variable arguments
  methodList.Add(TMethodDetails.Create ('len',   VARIABLE_ARGS, 'get the length of an array: var.len ()', getLength));
  methodList.Add(TMethodDetails.Create ('shape',  0, 'get the dimensions of the array: var.shape ()', getShape));
  methodList.Add(TMethodDetails.Create ('ndim',   0, 'get the number of dimensions of the array: var.ndim ()', getNumDim));
  methodList.Add(TMethodDetails.Create ('rows',   0, 'get the number of rows of a matrix: var.rows()', getNumRows));
  methodList.Add(TMethodDetails.Create ('cols',   0, 'get the number of columns of a matrix: var.cols()', getNumCols));

  methodList.Add(TMethodDetails.Create ('toMatrix', 0, 'Convert a 2D array into a matrix: m = var.tomatrix()', toMatrix));


  methodList.Add(TMethodDetails.Create('appendrow',  1, 'append rows', appendRow));
  methodList.Add(TMethodDetails.Create('appendcol',  1, 'append columns', appendCol));
  //methodList.Add(TMethodDetails.Create('append',  1, 'append columns', append));

  methodList.Add(TMethodDetails.Create ('tr',     0, 'Transpose the matrix: var.tr ()', getTranspose));
  methodList.Add(TMethodDetails.Create ('sqr',    0, 'square each element in the array: var.sqr ()', getSqr));
  methodList.Add(TMethodDetails.Create ('add',    1, 'add an array argument to the array: c = a.add (b)', add));
  methodList.Add(TMethodDetails.Create ('sub',    1, 'subtract an array argument from the array: c = a.sub (b)', sub));
  methodList.Add(TMethodDetails.Create ('trunc',  0, 'Truncate all entries to whole numbers: c = a.trunc ()', getTrunc));
  methodList.Add(TMethodDetails.Create ('max',    0, 'Find the maximum value in an array: c = a.max ()', getMax));
  methodList.Add(TMethodDetails.Create ('min',    0, 'Find the minimum value in an array: c = a.min ()', getMin));
end;


destructor TArrayMethods.Destroy;
begin
  inherited;
end;

// ---------------------------------------------------------------------------------------

function sameDimensions (m1, m2 : TArrayObject) : boolean;
var i : integer;
    n: integer;
begin
  result := True;
  if m1.getNumDimensions() <> m2.getNumDimensions() then
     exit (False);

  n := m1.getNumDimensions() - 1;
  for i := 0 to n do
      if m1.dim[i] <> m2.dim[i] then
         exit (False);
end;


procedure TArrayMethods.getLength (vm : TObject);
var s : TArrayObject;
    nArgs, d : integer;
    md : TMethodDetails;
begin
   d := 0;
   nArgs := TVM (vm).popInteger;
   if nArgs = 1 then
      d := TVM (vm).popInteger
   else
   if nArgs > 1 then
      raise ERuntimeException.Create('Too many arguments passed to len()');

   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

   if nArgs = 0 then
      TVM (vm).push(s.getNumberOfElements())
   else
      begin
      if nArgs = 1 then
         begin
         if (d <= s.ndims - 1) and (d > -1) then
            TVM (vm).push(s.dim[d])
         else
            raise ERuntimeException.Create('Array only has: ' + inttostr (s.ndims) + ' dimensions');
         end;
      end;
end;


procedure TArrayMethods.getShape (vm : TObject);
var s : TArrayObject;
    r : TListObject;
    i : integer;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  r := TListObject.Create(length (s.dim));
  for i := 0 to length (s.dim) - 1 do
      begin
      r.list[i].itemType := symInteger;
      r.list[i].iValue := s.dim[i];
      end;

  TVM (vm).push(r);
end;


procedure TArrayMethods.getNumDim (vm: TObject);
var s :TArrayObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  TVM (vm).push (length (s.dim));
end;


procedure TArrayMethods.getNumRows (vm: TObject);
var s :TArrayObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  if s.getNumDimensions() <= 2 then
     TVM (vm).push (s.dim[0])
  else
     raise ERuntimeException.Create('Method <rows> only applies to 2D arrays (matrices)');
end;


procedure TArrayMethods.getNumCols (vm: TObject);
var s :TArrayObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  if s.getNumDimensions() = 2 then
     TVM (vm).push (s.dim[1])
  else
     raise ERuntimeException.Create('Method <cols> only applies to 2D arrays (matrices)');
end;


//procedure TArrayObject.append (mat : TArrayObject);
//var i : integer;
//begin
//  if length (self.dim) <> 2 then
//     raise ERuntimeException.Create('Only 2D arrays are currently supported in append');
//
//  // Check that the number of columns is compatible
//  if self.dim[1] = mat.dim[1] then
//     begin
//     inc (self.dim[0]);
//     setLength (dataf, self.dim[0] * self.dim[1]);
//
//     for i := 0 to mat.dim[1] - 1 do
//         self.setValue2D (self.dim[0]-1, i, mat.getValue2D (0, i));
//
//     setLength (mat.dataf, 0);
//     end
//  else
//    raise ERuntimeException.Create(' column dimensions must match for each row of the matrix');
//end;

// m1.append (appendee)
procedure TArrayMethods.appendRow (vm : TObject);
var s, appendee : TArrayObject;
    target : TArrayObject;
    i, j : integer;
    sRows: integer;
    md : TMethodDetails;
begin
  appendee := TVM (vm).popArray;

   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  if (s.getNumDimensions() = 2) and (appendee.getNumDimensions() = 2) then
     begin
     if s.dim[1] = appendee.dim[1] then
        begin
        target := s.clone as TArrayObject;
        sRows := s.dim[0];
        target.resize2D (sRows + appendee.dim[0], s.dim[1]);

        for i := 0 to appendee.dim[0] - 1 do
            for j := 0 to appendee.dim[1] - 1 do
                target.setValue2D (sRows + i, j, appendee.getValue2D (i, j));
        s.Free;
        s := target;
        end
     else
        raise ERuntimeException.Create('method <appendrow> column sizes don''t match');
     end
  else
     raise ERuntimeException.Create('The method <appendrow> only applies to 2D arrays');
  //TVM (vm).push (target);
  TVM (vm).push (@noneStackType);
end;


procedure TArrayMethods.appendCol (vm : TObject);
var s,appendee :TArrayObject;
    md : TMethodDetails;
    target : TArrayObject;
begin
  appendee := TVM (vm).popArray;

   md := TVM (vm).popMethodDetails;
   s := TArrayObject (md.self);

  if (s.getNumDimensions() = 2) and (appendee.getNumDimensions() = 2) then
     begin
    if s.dim[0] = appendee.dim[0] then
        begin


        end
     else
        raise ERuntimeException.Create('method <appendcol> rows sizes don''t match');
     end
  else
     raise ERuntimeException.Create('The method <appendcol> only applies to 2D matrices');
  TVM (vm).push (target);
end;


procedure TArrayMethods.getTranspose (vm : TObject);
var s, tmp :TArrayObject;
    i, j : integer;
    r, c : integer;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TArrayObject (md.self);
  if s.ndims < 2 then
     raise ERuntimeException.Create('The array must be 2-dimensional to evaluate the transpose');

  r := s.dim[0];
  c := s.dim[1];

  tmp := TArrayObject.Create ([c, r]);
  tmp.blockType := btTemporary;  // protect from garbage collector
  for i := 0 to r - 1 do
      for j := 0 to c - 1 do
          tmp[j,i] := s[i,j];
  tmp.blockType := btGarbage;
  TVM (vm).push (tmp);
end;


procedure TArrayMethods.getSqr (vm : TObject);
var i : integer;
    s1, s2 : TArrayObject;
    len : integer;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s1 := TArrayObject (md.self);

   s2 := s1.clone as TArrayObject;

   len := s1.getNumberOfElements - 1;
   for i := 0 to len do
       s2.dataf[i] := s1.dataf[i]*s1.dataf[i];
    TVM (vm).push (s2);
end;


procedure TArrayMethods.toMatrix (vm : TObject);
var md : TMethodDetails;
    s : TArrayObject;
begin
  md := TVM (vm).popMethodDetails;
  s := TArrayObject (md.self);

  TVM (vm).push(TArrayObject.toMatrix(s));
end;


// eg c = a.add (b)
procedure TArrayMethods.add (vm : TObject);
var s1, s2 : TArrayObject;
    argument : TArrayObject;
    md : TMethodDetails;
begin
  argument := TVM (vm).popArray;

  md := TVM (vm).popMethodDetails;
  s1 := TArrayObject (md.self);

  s2 := TArrayObject.add (s1, argument);
  TVM (vm).push (s2);
end;


// c = a.sub (b)
procedure TArrayMethods.sub (vm : TObject);
var s1, s2 : TArrayObject;
    argument : TArrayObject;
    md : TMethodDetails;
begin
  argument := TVM (vm).popArray;

  md := TVM (vm).popMethodDetails;
  s1 := TArrayObject (md.self);

  s2 := TArrayObject.sub (s1, argument);
  TVM (vm).push (s2);
end;


procedure TArrayMethods.getMax (vm : TObject);
var s1 : TArrayObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s1 := TArrayObject (md.self);

   TVM (vm).push (TArrayObject.getMax(s1));
end;


procedure TArrayMethods.getMin (vm : TObject);
var s1 : TArrayObject;
    md : TMethodDetails;
begin
   md := TVM (vm).popMethodDetails;
   s1 := TArrayObject (md.self);

   TVM (vm).push (TArrayObject.getMin(s1)); 
end;


procedure TArrayMethods.getTrunc (vm : TObject);
var s, target : TArrayObject;
    i : integer;
    md : TMethodDetails;
begin
  md := TVM (vm).popMethodDetails;
  s := TArrayObject (md.self);

  target := s.clone as TArrayObject;

  for i := 0 to s.getNumberOfElements() - 1 do
      target.dataf[i] := trunc (s.dataf[i]);

  TVM (vm).push(target);
end;


// --------------------------------------------------------------------------------

constructor TArrayObject.Create;
begin
  inherited Create;

  objectType := symArray;
  self.methods := TArrayObject.arrayMethods;
end;


constructor TArrayObject.CreateDim;
begin
  Create;
end;


constructor TArrayObject.Create (n : integer);
begin
  Create;
  setlength (self.dim, 1);
  self.dim[0] := n;
  setlength (dataf, getNumberOfElements());
end;


constructor TArrayObject.Create (dim : TIndexArray);
begin
  Create;

  self.dim := copy (dim);
  setlength (dataf, getNumberOfElements());
end;


destructor TArrayObject.Destroy;
begin
  setLength (datai, 0);
  setLength (dataf, 0);
  setLength (dim, 0);
  inherited;
end;


function TArrayObject.getNumDimensions : integer;
begin
  result := length (dim);
end;


function TArrayObject.getNthDimension (i : integer) : integer;
begin
  result := dim[i];
end;


// See
//  https://stackoverflow.com/questions/33537595/how-to-calculate-the-element-addresses-of-an-n-dimensional-array
function TArrayObject.getIndex (const dim, idx : array of integer) : integer;
begin
  result := idx[0];
  for var i := 1 to high (dim) do
      result := result*dim[i] + idx[i];
end;


function TArrayObject.getValue (idx : array of integer) : double;
var index : integer;
begin
  for var i := 0 to length (dim) - 1 do
      if idx[i] >= dim[i] then
         raise ERuntimeException.Create(outOfRangeMsg + ': ' + inttostr (idx[i]));

  index := getIndex (dim, idx);
  result := dataf[index];
end;


function TArrayObject.getValue1D (i : integer) : double;
begin
  if i >= dim[0] then
     raise ERuntimeException.Create(outOfRangeMsg);

  result := dataf[i];
end;


function TArrayObject.getValue2D (i, j : integer) : double;
var x : integer;
begin
  if (i >= dim[0]) or (j >= dim[1]) then
     raise ERuntimeException.Create(outOfRangeMsg);

  x := getIndex ([dim[0],dim[1]],[i,j]);
  //x := j + dim[1]*i;
  result := dataf[x];
end;


function TArrayObject.getRow (index : integer) : TArrayObject;
var i : integer;
begin
  if (index < 0) or (index >= self.dim[1]) then
     raise ERuntimeException.Create(outOfRangeMsg);

  result := TArrayObject.Create([1, self.dim[1]]);
  for i := 0 to self.dim[1] - 1 do
      result.setValue2D(0, i, self.getValue2D(index, i));
end;


function TArrayObject.getDim : TIndexArray;
begin
  result := dim;
end;


procedure TArrayObject.setValue1D (i : integer; value : double);
begin
  if i >= dim[0] then
     raise ERuntimeException.Create(outOfRangeMsg);

  dataf[getIndex (dim, [i])] := value;
end;


procedure TArrayObject.setValue2D (i, j : integer; value : double);
begin
  if (i >= dim[0]) or (j >= dim[1]) then
     raise ERuntimeException.Create(outOfRangeMsg);

  dataf[getIndex (dim, [i, j])] := value;
end;



procedure TArrayObject.setValue (idx : array of integer; value : double);
var index : integer;
    i : integer;
begin
  for i := 0 to length (dim) - 1 do
      if idx[i] >= dim[i] then
         raise ERuntimeException.Create(outOfRangeMsg);

  index := getIndex (dim, idx);
  dataf[index] := value;
end;


function TArrayObject.clone : TDataObject;
begin
  result := TArrayObject.Create (dim);
  (result as TArrayObject).dataf := copy (self.dataf);
end;


function TArrayObject.getSize() : integer;
begin
  result := self.InstanceSize;
  result := result + getNumDimensions()*SizeOf(double);
end;


procedure TArrayObject.resize2D (n, m : integer);
begin
  setlength (dataf, n*m);
  dim[0] := n;
  dim[1] := m;
end;


function TArrayObject.getNumberOfElements : integer;
var i : integer;
begin
  if length(dim) = 0 then
     result := 0
  else
     begin
     result := 1;
     for i := 0 to length (dim) - 1 do
         result := result * dim[i];
     end;
end;


function TArrayObject.arrayRecursiveToString (var Indices : TArray<integer>; depth : integer) : string;
var
  i, Offset: Integer;
  fmt : string;
begin
  fmt := (SysLibraryRef.find ('doubleFormat').dataObject as TStringObject).value;
  if depth = High(dim) then
  begin
    result := result + '(';
    for i := 0 to dim[depth] - 1 do
    begin
      if i > 0 then
        result := result + ', ';
      Offset := 0;
      for var j := 0 to High(Indices) do
        Offset := Offset * dim[j] + Indices[j];
      Offset := Offset * dim[depth] + i;
      result := result + (Format (fmt, [dataf[Offset]]));
    end;
    result := result + ')';
  end
  else
  begin
    result := result + '(';
    for i := 0 to dim[Depth] - 1 do
    begin
      if i > 0 then
        result := result + ', ';
      SetLength(Indices, depth + 1);
      Indices[depth] := i;
      result := arrayRecursiveToString(Indices, depth + 1);
    end;
    result := result + ')';
  end;
end;


function TArrayObject.NdimensionalArrayToString : string;
var Indices: TArray<integer>;
    depth : integer;
begin
  depth := 0;
  SetLength(Indices, 0);
  result := arrayRecursiveToString (Indices, depth);
end;


// This needs to be redone at some point so that n-dimensional
// arrays are convert to string format correctly.
function TArrayObject.toString: string;
var i, j : integer;
    fmt : string;
    sym : TSymbol;
begin
    sym := SysLibraryRef.find ('doubleFormat');
    fmt := (sym.dataObject as TStringObject).value;

  if length (dim) = 1 then
     begin
     result := '(';
     for i := 0 to self.getNthDimension(0) - 1 do
         begin
         result := result + Format(fmt, [self.getValue1D(i)]);
         if i < self.getNthDimension(0) - 1 then
             result := result + ', ';
         end;
     result := result + ')';
     exit;
     end;

  if length (dim) = 2 then
     begin
     result := '(';
     for i := 0 to self.getNthDimension(0) - 1 do
         begin
         result := result + '(';
         for j := 0 to self.getNthDimension(1) - 1 do
             begin
             result := result + Format(fmt, [self.getValue2D(i, j)]);
             if j < self.getNthDimension(1) - 1 then
                result := result + ', ';
            end;
        if i < self.getNthDimension(0) - 1 then
           result := result + '), ' + sLineBreak;
        end;
     result := result + '))';
     exit;
     end;

  // Deal with the general case of an n-dim arrays
  result := NdimensionalArrayToString;
end;



procedure TArrayObject.append (arrayArg : TArrayObject);
var i : integer;
begin
  if length (self.dim) <> 2 then
     raise ERuntimeException.Create('Only 2D arrays are currently supported in append');

  // Check that the number of columns is compatible
  if self.dim[1] = arrayArg.dim[1] then
     begin
     inc (self.dim[0]);
     setLength (dataf, self.dim[0] * self.dim[1]);

     for i := 0 to arrayArg.dim[1] - 1 do
         self.setValue2D (self.dim[0]-1, i, arrayArg.getValue2D (0, i));

     setLength (arrayArg.dataf, 0);
     end
  else
    raise ERuntimeException.Create(' column dimensions must match for each row of the matrix');
end;


procedure coordCallback (coord : TIntList);
begin

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
function TArrayObject.slice (var slices : TSliceObjectList) : TArrayObject;
var i, j : integer;
    slicesize : integer;
    inputLists : TIntLists;
    alist : TIntList;
    cp : TCartesianProduct;
begin
  result := TArrayObject.Create;
  // Check for missing slices
  if length (dim)  > length (slices) then
     begin
     // fill out the missing slices which should all be ':' (SLICE_ALL)
     for i := length (slices) to length (dim) - 1do
         begin
         setLength (slices, length (slices)+1);
         slices[length (slices)-1] := TSliceObject.Create(0, SLICE_ALL);
         end;
     end;

  // Create space for the number of dimensions of the slice.
  setlength (result.dim, length (slices));
  // Compute the number of elements that will be in the slice
  // eg [1:2,3:5] will have 6 elements
  // At this point we also convert the SLICE_ALL and SLICE_EQUAL to coordinates
  slicesize := 1;
  for i := 0 to length (slices) - 1 do
      begin
      if slices[i].lower = SLICE_ALL then
         slices[i].lower := 0;
      if slices[i].upper = SLICE_ALL then
         slices[i].upper := Self.dim[i] - 1;
      if slices[i].upper = SLICE_EQUAL then
         slices[i].upper := slices[i].lower;

      // Sotre the size of the dimension in dim[i]
      result.dim[i] := slices[i].upper - slices[i].lower+1;
      slicesize := slicesize * (slices[i].upper - slices[i].lower+1);
      end;
  // Allocate space for the slice
  setlength (result.dataf, slicesize);
  // Generate the coordinates for each item that we need to copy from the source array
  setlength (inputLists, length (slices));
  for i := 0 to length (slices) - 1 do
      begin
      setLength (alist, result.dim[i]);
      for j := slices[i].lower to slices[i].upper do
          alist[j-slices[i].lower] := j;
      inputLists[i] := alist;
      end;
  cp := TCartesianProduct.Create (inputlists);
  try
    // Copy all data over to target
    for i := 0 to cp.numberofProducts - 1 do
        result.dataf[i] := self.getValue(cp.getIthCartesianProduct(i));
  finally
    cp.free;
  end;
end;


function TArrayObject.applyUniFunction (func : TUniFunction) : TArrayObject;
var i, n : integer;
begin
  n := self.getNumberOfElements();
  result := self.clone() as TArrayObject;
  for i := 0 to n - 1 do
      result.dataf[i] := func (self.dataf[i]);
end;


class function TArrayObject.minus (a : TArrayObject) : TArrayObject;
var i, n : integer;
begin
  n := a.getNumberOfElements();
  result := a.clone() as TArrayObject;
  for i := 0 to n - 1 do
      result.dataf[i] := -a.dataf[i];
end;

class function TArrayObject.add (a1, a2 : TArrayObject) : TArrayObject;
var i, j : integer;
begin
  if length (a1.dim) = length (a2.dim) then
     begin
     if length (a1.dim) = 1 then
        begin
        if a1.dim[0] = a2.dim[0] then
           begin
           result := TArrayObject.Create (a1.dim);
           for i := 0 to a1.dim[0] - 1 do
               result.setValue1D(i, a1.getValue1D(i) + a2.getValue1D(i));
           end
        else
           raise ERuntimeException.Create(sameDimensionsMsg);
        exit;
        end;

     if (a1.dim[0] = a2.dim[0]) and (a1.dim[1] = a2.dim[1]) then
        begin
        result := TArrayObject.Create (a1.dim);
        for i := 0 to a1.dim[0] - 1 do
            for j := 0 to a1.dim[1] - 1 do
                result.setValue2D(i, j, a1.getValue2D(i, j) + a2.getValue2D(i, j));
        end
     else
        raise ERuntimeException.Create(sameDimensionsMsg);
     end
  else
     raise ERuntimeException.Create(sameDimensionsMsg);
end;


class function TArrayObject.add (a : TArrayObject; value : double) : TArrayObject;
var i, n : integer;
begin
  n := a.getNumberOfElements;
  result := a.clone as TArrayObject;
  for i := 0 to n - 1 do
      result.dataf[i] := a.dataf[i] + value;
end;


class function TArrayObject.sub (a1, a2 : TArrayObject) : TArrayObject;
var i, j : integer;
begin
  if length (a1.dim) = length (a2.dim) then
     begin
     if length (a1.dim) = 1 then
        begin
        if a1.dim[0] = a2.dim[0] then
           begin
           result := TArrayObject.Create (a1.dim);
           for i := 0 to a1.dim[0] - 1 do
               result.setValue1D(i, a1.getValue1D(i) - a2.getValue1D(i));
           end
        else
           raise ERuntimeException.Create(sameDimensionsMsg);
        exit;
        end;
     end;

  if length (a1.dim) = length (a2.dim) then
     begin
     if (a1.dim[0] = a2.dim[0]) and (a1.dim[1] = a2.dim[1]) then
        begin
        result := TArrayObject.Create (a1.dim);
        for i := 0 to a1.dim[0] - 1 do
            for j := 0 to a1.dim[1] - 1 do
                result.setValue2D(i, j, a1.getValue2D(i, j) - a2.getValue2D(i, j));
        end
     else
        raise ERuntimeException.Create(sameDimensionsMsg);
     end
  else
     raise ERuntimeException.Create(sameDimensionsMsg);
end;


class function TArrayObject.subLeft (a : TArrayObject; value : double) : TArrayObject;
var i, n : integer;
begin
  n := a.getNumberOfElements;
  result := a.clone as TArrayObject;
  for i := 0 to n - 1 do
      result.dataf[i] := a.dataf[i] - value;
end;


class function TArrayObject.subRight (a : TArrayObject; value : double) : TArrayObject;
var i, n : integer;
begin
  n := a.getNumberOfElements;
  result := a.clone as TArrayObject;
  for i := 0 to n - 1 do
      result.dataf[i] := value - a.dataf[i];
end;


class function TArrayObject.arrayScalarIntMult (m1 : TArrayObject; alpha : integer) : TArrayObject;
var n : integer;
begin
  result := TArrayObject.Create ([m1.dim[0], m1.dim[1]]);
  n := m1.getNumberOfElements;
  for var i := 0 to n - 1 do
      result.dataf[i] := alpha*m1.dataf[i];
end;


class function TArrayObject.arrayScalarDoubleMult (m : TArrayObject; alpha : double) : TArrayObject;
var n : integer;
begin
  result := m.clone as TArrayObject;
  n := m.getNumberOfElements;
  for var i := 0 to n - 1 do
      result.dataf[i] := alpha*m.dataf[i];
end;


// element-wise multiplication
class function TArrayObject.mult (m1, m2 : TArrayObject) : TArrayObject;
var n : integer;
begin
  if (m1.getNumDimensions() = 1) and (m2.getNumDimensions() = 1) then
     begin
     result := TArrayObject.Create ([m1.dim[0]]);
     for var i := 0 to m1.dim[0] - 1 do
        result.setValue([i], m1.getValue([i]) * m2.getValue ([i]));
     exit;
     end;

  if not sameDimensions(m1, m2) then
     raise ERuntimeException.Create('Array multiplication requires the same dimensions for each array');

  result := TArrayObject.Create ([m1.dim[0], m2.dim[1]]);
  n := m1.dim[0] * m1.dim[1];
  for var i := 0 to n - 1 do
      result.dataf[i] :=  m1.dataf[i] * m2.dataf[i];
end;


class function TArrayObject.divide (a : TArrayObject; value : double; reciprocal : boolean) : TArrayObject;
var i : integer;
begin
  result := a.clone as TArrayObject;
  for i := 0 to a.getNumberOfElements() - 1 do
      if reciprocal then
         result.dataf[i] := value/a.dataf[i]
      else
         result.dataf[i] := a.dataf[i]/value;
end;


class function TArrayObject.getMax (a1 : TArrayObject) : double;
var i : integer;
    maxValue : double; 
begin
  if length (a1.dataf) = 0 then
     result := 0
  else
      begin
      maxValue := a1.dataf[0];
      for i := 1 to length (a1.dataf) - 1 do
          if a1.dataf[i] > maxValue then
             maxValue := a1.dataf[i];
      result := maxValue
      end;  
end;


class function TArrayObject.getMin (a1 : TArrayObject) : double;
var i : integer;
    minValue : double; 
begin
  if length (a1.dataf) = 0 then
     result := 0
  else
      begin
      minValue := a1.dataf[0];
      for i := 1 to length (a1.dataf) - 1 do
          if a1.dataf[i] < minValue then
             minValue := a1.dataf[i];
      result := minValue
      end;  
end;


// This is a tough one.
class function TArrayObject.toList (a : TArrayObject) : TDataObject;
//var l : TListObject;
//    i, j : integer;
//    item : TListItem;
//    row : TListObject;
begin
  result := TListObject.Create;

  // Create the rows, each row is numCols wide
//  for i := 0 to m.numRows - 1 do
//      l.append (TListObject.Create(m.numCols));
//  // Set each items that ow exists in each row to a value
//  for i := 0 to m.numRows - 1 do
//      begin
//      row := a[i].lValue;
//      for j := 0 to m.numCols - 1 do
//          row.setItemToDouble(j, m[i,j]);
//      end;
//
//  result := l;
end;


class function TArrayObject.toMatrix (a : TArrayObject) : TDataObject;
var i, j : integer;
    m : TMatrixObject;
begin
  if a.getNumDimensions = 2 then
     begin
     m := TMatrixObject.Create (a.dim[0], a.dim[1]);
     for i := 0 to a.dim[0] - 1 do
         for j := 0 to a.dim[1] - 1 do
             m[i,j] := a.getValue2D(i, j);
     end
  else
    raise ERuntimeException.Create('The array must be two dimensional');
  result := m;
end;


class function TArrayObject.isEqualTo (a1, a2 : TArrayObject) : boolean;
var n: integer;
    epsSymbol : TSymbol;
begin
  //epsSymbol :=  mainModule.find('math' 'eps');
   // eps symbol now stored in main since math may not be imported yet
  mainModule.symbolTable.find('eps', epsSymbol);

  result := True;
  n := a1.getNumDimensions() - 1;
  if not sameDimensions(a1, a2) then
     exit (False);

  for var i := 0 to n do
      begin
      if not sameValue (a1.dataf[i], a2.dataf[i], (epsSymbol.dataObject as TValueObject).dValue) then
         exit (False);
      end;
end;


// -----------------------------------------------------------------------

initialization
   TArrayObject.arrayMethods := TArrayMethods.Create;
   //arrayMethods := TArrayMethods.Create;
finalization
   TArrayObject.arrayMethods.free;
   //arrayMethods.Free;
end.


