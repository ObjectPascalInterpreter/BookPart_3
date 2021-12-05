unit uUtils;

// Developed using Delphi for Windows and Mac platforms.

// *** This source is distributed under Apache 2.0 ***

// Copyright (C) 2018 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


{$WARN SYMBOL_PLATFORM OFF}  // Stops memory API usage warnings


interface

Uses Windows, System.SysUtils, uRhodusTypes;

type
  TCartesianProduct = class (TObject)
       b, n: TIntList;
       lists : TIntLists;
       numberofProducts : integer;
       argc : integer;
       function getIthCartesianProduct (index : integer): TIntList;
       constructor Create (arg : TIntLists);

  end;


function getProcessTime : int64;
function getMemoryAllocated : UInt64;
function cartesianProduct (arg : TIntLists): TIntLists;

implementation


function getProcessTime : int64;
var lpCreationTime, lpExitTime, lpKernelTime, lpUserTime: TFileTime;
    TotalTime : integer;
      hProc, b: Cardinal;
begin
  result := 0;
  b := GetCurrentProcess;
  hProc := OpenProcess( PROCESS_QUERY_INFORMATION, False, b );
  if hProc = 0 then
     exit;

  GetProcessTimes(hProc,lpCreationTime, lpExitTime, lpKernelTime, lpUserTime);
  TotalTime := int64(lpKernelTime.dwLowDateTime or (lpKernelTime.dwHighDateTime shr 32));
  result := TotalTime;
end;


function getMemoryAllocated : UInt64;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
begin
  getMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
      result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;


// ------------------------------------------------------------------------

constructor TCartesianProduct.Create (arg : TIntLists);
begin
  self.lists := arg;
  argc := length(arg);
  numberofProducts := 1;
  for var a in arg do
      numberofProducts := numberofProducts * length(a);

  SetLength(b, numberofProducts * argc);
  SetLength(n, argc);
end;


// Use by the slicing function to generate the coordiantes
// that should be copied over from the source array.
// Algorithm taken and modified from rosettacode.org
// https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Delphi
function TCartesianProduct.getIthCartesianProduct (index : integer): TIntList;
var s, e : integer;
begin
  s := 0;
  e := s + argc;
  var Resi := copy(b, s, e - s);
  Result := Resi;

  for var j := 0 to high(n) do
  begin
    var nj := n[j];
    Resi[j] := lists[j, nj];
  end;

  for var j := high(n) downto 0 do
  begin
    inc(n[j]);
    if n[j] < Length(lists[j]) then
      Break;
    n[j] := 0;
  end;
end;


// OLD Code

// Use by the slicing function to generate the coordiantes
// that should be copied over from teh source array.
// Algorithm taken andmodified from rosettacode.org
// https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Delphi
function cartesianProduct (arg: TIntLists): TIntLists;
var
  b, n: TIntList;
  argc: Integer;
begin
  argc := length(arg);

  var c := 1;
  for var a in arg do
    c := c * length(a);

  if c = 0 then
    exit;

  SetLength(result, c);
  SetLength(b, c * argc);
  SetLength(n, argc);

  var s := 0;
  for var i := 0 to c - 1 do
  begin
    var e := s + argc;
    var Resi := copy(b, s, e - s);
    Result[i] := Resi;

    s := e;
    for var j := 0 to high(n) do
    begin
      var nj := n[j];
      Resi[j] := arg[j, nj];
    end;

    for var j := high(n) downto 0 do
    begin
      inc(n[j]);
      if n[j] < Length(arg[j]) then
        Break;
      n[j] := 0;
    end;
  end;
end;


// Old Code
function cartesianProductSingle (arg: TIntList): TIntLists;
var alen : integer; i : integer;
    alists : TIntLists;
begin
  alen := length (arg);
  setlength (alists, alen);
  for i := 0 to alen - 1 do
      alists[i] := arg;
  result := cartesianProduct(alists);
end;



end.
