unit uArrayObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, uMemoryManager, uObjectSupport, Generics.Collections;

type
  TIndexArray = array of integer;

  TArrayMethods = class (TMethodsBase)
     procedure   getLength (vm : TObject);
     procedure   getShape (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
  end;

  T1DArray = array of double;
  T2DArray = array of T1DArray;

  TArrayObject = class (TRhodusObject)
    private

     size : integer;
     function getNumDimensions : integer;

    public
     data : T1DArray;
     dim : TIndexArray;
     arrayMethods : TArrayMethods;
     function        getIndex (const dim, idx : array of integer) : integer;
     function        getValue1D (i : integer) : double;
     function        getValue2D (i, j : integer) : double;
     procedure       setValue1D (i : integer; value : double);
     procedure       setValue2D (i, j : integer; value : double);
     function        getRow (index : integer) : TArrayObject;
     function        getDim : TIndexArray;
     function        clone : TArrayObject;
     function        clonex : TArrayObject;
     function        arrayToString () : string;
     function        getSize() : integer;
     function        getNthDimension (i : integer) : integer;
     property        numDimensions : integer read getNumDimensions;

     //procedure       append (mat : TArrayObject);
     procedure       appendx (mat : TArrayObject);
     class function  add (a1, a2 : TArrayObject) : TArrayObject;
     class function  sub (a1, a2 : TArrayObject) : TArrayObject;
     constructor     Create; overload;
     constructor     Create (dim : TIndexArray); overload;
     destructor      Destroy; override;
  end;

  //function convertToArray (ar : TIArray) : TArrayObject;

implementation

Uses SysUtils,
     StrUtils,
     System.Character,
     uRhodusTypes,
     uVM,
     uMachineStack,
     uListObject,
     uVMExceptions;

var globalArrayMethods : TArrayMethods;


constructor TArrayMethods.Create;
begin
  methodList := TMethodList.Create;

  methodList.Add(TMethodDetails.Create ('len',   'get the length of an array: var.len ()', -1, getLength));
  methodList.Add(TMethodDetails.Create ('shape', 'get the dimensions of the array var.shape ()', 0, getShape));

  methodList.Add(TMethodDetails.Create ('dir',     'dir of array object methods', 0, dir));
end;


destructor TArrayMethods.Destroy;
begin
  for var i := 0 to methodList.Count - 1 do
      methodList[i].Free;
  methodlist.Free;
  inherited;
end;


procedure TArrayMethods.getLength (vm : TObject);
var s : TArrayObject;
    nArgs, d : integer;
begin
   nArgs := TVM (vm).popInteger;
   if nArgs = 1 then
      d := TVM (vm).popInteger
   else
   if nArgs > 1 then
      raise ERuntimeException.Create('Too many arguments passed to len()');

   TVM (vm).decStackTop; // Dump the object method
   s := TVM (vm).popArray;

   if nArgs = 0 then
      TVM (vm).push(s.size)
   else
      begin
      if nArgs = 1 then
         begin
         if (d <= s.numDimensions - 1) and (d > -1) then
            TVM (vm).push(s.dim[d])
         else
            raise ERuntimeException.Create('Array only has: ' + inttostr (s.numDimensions) + ' dimensions');
         end;
      end;
end;


procedure TArrayMethods.getShape (vm : TObject);
var s : TArrayObject;
    r : TListObject;
begin
   TVM (vm).decStackTop; // Dump the object method
   s := TVM (vm).popArray;
   r := TListObject.Create(2);
   r.list[0].itemType := liInteger;
   r.list[0].iValue := s.dim[0];
   r.list[1].itemType := liInteger;
   r.list[1].iValue := s.dim[1];

   TVM (vm).push(r);
end;

// ---------------------------------------------------------------------
function createArrayObject (const dim : TIndexArray) : TArrayObject;
begin
  result := TArrayObject.Create (dim);
end;

// ----------------------------------------------------------------------
constructor TArrayObject.Create ();
begin
  blockType := btGarbage;
  objectType := symArray;
  arrayMethods := globalArrayMethods;
  memoryList.addNode (self);
  end;

constructor TArrayObject.Create (dim : TIndexArray);
var i : integer;
begin
  Create;

  if length (dim) > 2 then
     raise ERuntimeException.Create('Only 1D and 2D arrays are currently supported');

  self.dim := copy (dim);
  self.size := dim[0];
  for i := 1 to High (dim) do
      self.size := self.size * dim[1];
  setlength (data, self.size);
end;


destructor TArrayObject.Destroy;
begin
  setLength (data, 0);
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


function TArrayObject.getIndex (const dim, idx : array of integer) : integer;
begin
  result := idx[0];
  for var i := 1 to high (dim) do
      //result := result*dim[i-1] + idx[i];
      result := result*dim[i] + idx[i];
end;


function TArrayObject.getValue1D (i : integer) : double;
begin
  result := data[i];
end;


function TArrayObject.getValue2D (i, j : integer) : double;
var x : integer;
begin
  x := j + dim[1]*i;
  result := data[x];
end;

function TArrayObject.getRow (index : integer) : TArrayObject;
var i : integer;
begin
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
  data[getIndex (dim, [i])] := value;
end;


procedure TArrayObject.setValue2D (i, j : integer; value : double);
begin
  data[getIndex (dim, [i, j])] := value;
end;


function TArrayObject.clone : TArrayObject;
begin
  result := TArrayObject.Create (dim);
  result.size := self.size;
  result.data := copy (self.data);
end;


function TArrayObject.clonex : TArrayObject;
begin
  result := TArrayObject.Create (dim);
  result.size := self.size;
  result.data := copy (self.data);
end;


function TArrayObject.getSize() : integer;
begin
  result := self.InstanceSize;
  result := result + size;
end;


function TArrayObject.arrayToString: string;
var i, j : integer;
    formatStr : string;
begin
  if length (dim) = 1 then
     begin
     result := '[';
     for i := 0 to self.getNthDimension(0) - 1 do
         begin
         if i = 0 then
            formatStr := '%9.4f'
         else
            formatStr := '%10.4f';
         result := result + Format(formatStr, [self.getValue1D(i)]);
         if i < self.getNthDimension(0) - 1 then
             result := result + ', ';
         end;
     end;

  if length (dim) = 2 then
     begin
     result := '[';
     for i := 0 to self.getNthDimension(0) - 1 do
         begin
         for j := 0 to self.getNthDimension(1) - 1 do
             begin
             if (i = 0) and (j=0) then formatStr := '%9.4f'
             else
                formatStr := '%10.4f';
              result := result + Format(formatStr, [self.getValue2D(i, j)]);
             if j < self.getNthDimension(1) - 1 then
                result := result + ', ';
            end;
        if i < self.getNthDimension(0) - 1 then
           result := result + '; ' + sLineBreak;
        end;
     end;

  result := result + ']';
end;


//procedure TArrayObject.append (mat : TArrayObject);
//var i : integer;
//begin
//  // Check that the number of columns is compatible
//  if self.dim[1] = mat.dim[1] then
//     begin
//     inc (self.dim[0]);
//     setLength (data, self.dim[0], self.dim[1]);
//
//     for i := 0 to mat.dim[1] - 1 do
//         self.setValue2D(self.dim[0]-1, i, mat.getValue2D (0, i));
//
//     setLength (mat.data, 0);
//     end
//  else
//    raise ERuntimeException.Create(' column dimensions must match for each row of the matrix');
//end;


procedure TArrayObject.appendx (mat : TArrayObject);
var i : integer;
begin
  // Check that the number of columns is compatible
  if self.dim[1] = mat.dim[1] then
     begin
     inc (self.dim[0]);
     setLength (data, self.dim[0] * self.dim[1]);

     for i := 0 to mat.dim[1] - 1 do
         self.setValue2D (self.dim[0]-1, i, mat.getValue2D (0, i));

     setLength (mat.data, 0);
     end
  else
    raise ERuntimeException.Create(' column dimensions must match for each row of the matrix');
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
           raise ERuntimeException.Create('Arrays must have the same dimensions.');
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
        raise ERuntimeException.Create('Arrays must have the same dimensions.');
     end
  else
     raise ERuntimeException.Create('Arrays must have the same dimensions.');
end;



class function TArrayObject.sub (a1, a2 : TArrayObject) : TArrayObject;
var i, j : integer;
begin
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
        raise ERuntimeException.Create('Arrays must the same dimensions.');
     end
  else
     raise ERuntimeException.Create('Arrays must the same dimensions.');
end;


//function TArrayObject.isEqualTo (str1 : TStringObject) : boolean;
//begin
//  result := self.value = str1.value;
//end;
//
//
//class function TArrayObject.add (str1, str2 : TStringObject) : TArrayObject;
//begin
//  result := TArrayObject.Create (str1.value + str2.value);
//end;

// -----------------------------------------------------------------------

initialization
   globalArrayMethods := TArrayMethods.Create;
finalization
   globalArrayMethods.Free;
end.

