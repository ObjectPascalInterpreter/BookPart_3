unit uIntStack;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// This is a lighweight integer stack. Implemented to ensure that
// the code can be used with Delphi versions that dodn't have
// generics

interface

Uses Classes, SysUtils;

const
   MAX_ENTRIES = 100;

type
  TStack = record
      data : array of integer;
      stackPtr : integer;
      maxSize : integer;
  end;

procedure create (var stack : TStack; n : integer);
procedure push (var stack : TStack; value : integer);
function  pop (var stack : TStack) : integer;
function  peek (var stack : TStack) : integer;

implementation

procedure create (var stack : TStack; n : integer);
begin
  stack.maxSize := n;
  setLength (stack.data, n);
end;

procedure push (var stack : TStack; value : integer);
begin
  if stack.stackPtr = stack.maxSize then
     raise Exception.Create('Integer stack overflow error.');
  inc (stack.stackPtr);
  stack.data[stack.stackPtr] := value;
end;

function pop (var stack : TStack) : integer;
begin
  if stack.stackPtr = -1 then
     raise Exception.Create('Integer stack underflow error.');
  result := stack.data[stack.stackPtr];
  dec (stack.stackPtr);
end;

function peek (var stack : TStack) : integer;
begin
  result := stack.data[stack.stackPtr];
end;


end.
