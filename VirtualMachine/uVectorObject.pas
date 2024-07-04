unit uVectorObject;

// This source is distributed under Apache 2.0

// Copyright (C)  2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses SysUtils, Classes, uMemoryManager, uObjectSupport, uRhodusObject, Generics.Collections, uRhodusTypes;

type
  TVectorObject = class (TRhodusObject)
     private
       function  getValue (index : Integer) : double;
       procedure setValue (index : integer; value : double);
       function  getSize : integer;
       procedure setSize (value : integer);
     public
       data : TArray<double>;

       constructor Create; overload;
       constructor Create(Size: Integer); overload;
       destructor  Destroy; override;

       class function  add (v1, v2 : TVectorObject) : TVectorObject;
       class function  sub (v1, v2 : TVectorObject) : TVectorObject;
       class function  minus (v : TVectorObject) : TVectorObject;
       function   clone : TVectorObject;
       function   vectorToString: string;

       property size : Integer read getSize write setSize;
       property value[index : Integer] : double read getValue write setValue; default;
  end;


implementation

Uses uVMExceptions;

const outOfRangeMsg = 'Index out of range while accessing vector element';
      sameDimensionsMsg = 'Vectors must have the same dimensions';

// ----------------------------------------------------------------------
constructor TVectorObject.Create;
begin
  inherited Create;

  objectType := symVector;
  //self.methods := arrayMethods;
end;


constructor TVectorObject.Create(Size: Integer);
begin
  Create;
  System.SetLength(data, Size)
end;


destructor TVectorObject.Destroy;
begin
  data := nil;
  inherited;
end;


function TVectorObject.clone : TVectorObject;
begin
  result := TVectorObject.Create;
  result.data := self.data;
end;

function TVectorObject.getValue (index : Integer) : double;
begin
  result := data[index] // 0-based array
end;


procedure TVectorObject.setValue (index : integer; value : double);
begin
  data[index] := Value // 0-based array
end;


function  TVectorObject.getSize : integer;
begin
  result := length (data);
end;


procedure TVectorObject.setSize (value : integer);
begin
  SetLength (data, value);
end;


cLass function TVectorObject.add (v1, v2 : TVectorObject) : TVectorObject;
var i : integer;
begin
  if v1.size = v2.size then
     begin
     result := TVectorObject.Create(v1.size);
     for i := 0 to v1.size - 1 do
         result.data[i] := v1.data[i] + v2.data[i];
     end
  else
   raise ERuntimeException.Create(sameDimensionsMsg);
end;


cLass function TVectorObject.sub (v1, v2 : TVectorObject) : TVectorObject;
var i : integer;
begin
  if v1.size = v2.size then
     begin
     result := TVectorObject.Create(v1.size);
     for i := 0 to v1.size - 1 do
         result.data[i] := v1.data[i] - v2.data[i];
     end
  else
   raise ERuntimeException.Create(sameDimensionsMsg);
end;


class function TVectorObject.minus (v : TVectorObject) : TVectorObject;
var i : integer;
begin
   result := TVectorObject.Create(v.size);
   for i := 0 to v.size - 1 do
       result.data[i] := -v.data[i];
end;


function TVectorObject.vectorToString: string;
var i, j, n : integer;
    formatStr : string;
begin
  result := '{';
  for i := 0 to self.size - 1 do
      begin
      if i = 0 then
         formatStr := '%9.4f'
      else
         formatStr := '%10.4f';
      result := result + Format(formatStr, [self.getValue(i)]);
      if i < self.size - 1 then
          result := result + ', ';
      end;
  result := result + '}';
end;

end.
