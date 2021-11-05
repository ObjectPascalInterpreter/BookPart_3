unit uMemoryManager;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2021 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses Classes, SysUtils, uRhodusTypes, uObjectSupport, uRhodusObject;

type
   // btTemporary is only used if a function is called
   // with a literal list, eg callme ({1,2,3})

//   TRhodusObject = class (TObject)
//      blockType : TBlockType;
//      objectType : TSymbolElementType;
//      methods : TMethodsBase;
//
//      function isConstant : boolean;
//      function isBound : boolean;
//      function isOwned : boolean;
//      function isGarbage : boolean;
//
//      function    getRhodusObjectSize : integer;
//      constructor Create;
//      destructor  Destroy; override;
//   end;

   // Allocated nodes are stored using a linked list
   PMemoryNodePtr = ^TMemoryNode;
   TMemoryNode = record
      robj : TRhodusObject;
      next : PMemoryNodePtr;
   end;

   TMemorySummary = record
      numGarbage, numConstant, numOwned, numBound : integer;
      totalAllocated : integer;
   end;


   TMemoryList = class (TObject)
       private
          sizeOfList : integer;
          head, tail : PMemoryNodePtr;
          procedure freeFirstNode (var cursor : PMemoryNodePtr);
          procedure freeLastNode  (var cursor, previous : PMemoryNodePtr);
          procedure freeMiddleNode (var cursor, previous : PMemoryNodePtr);
       public
          function  newNode (obj : TRhodusObject) : PMemoryNodePtr;
          function  addNode (robj : TRhodusObject) : PMemoryNodePtr;
          function  mapMemory : string;
          function  mapMemoryDetailed : string;
          function  getMemoryListSize : integer;
          function  getNumberAllocatedNodes : TMemorySummary;
          procedure freeGarbage;
          procedure freeList;

          constructor Create;
          destructor  Destroy; override;

   end;


var memoryList : TMemorylist;

implementation

Uses uStringObject, uListObject, uSymbolTable;


// -------------------------------------------------------------------------------

//constructor TRhodusObject.Create;
//begin
//  inherited;
//  blockType := btGarbage;
//end;
//
//destructor TRhodusObject.destroy;
//begin
//  inherited
//end;
//
//function TRhodusObject.getRhodusObjectSize : integer;
//begin
//  result := sizeof (blockType);
//end;
//
//
//function TRhodusObject.isConstant : boolean;
//begin
//  result := (blockType = btConstant);
//end;
//
//
//function TRhodusObject.isBound : boolean;
//begin
//  result := (blockType = btBound);
//end;
//
//
//function TRhodusObject.isOwned : boolean;
//begin
//  result := (blockType = btOwned);
//end;
//
//
//function TRhodusObject.isGarbage : boolean;
//begin
//  result := (blockType = btGarbage);
//end;

// --------------------------------------------------------------------------------------------

constructor TMemoryList.Create;
begin
   inherited;
   head := nil;
   tail := nil;
   sizeOfList := 0;
end;


destructor TMemoryList.Destroy;
begin
   freeList;
   inherited;
end;


function TMemoryList.getMemoryListSize : integer;
begin
  result := sizeOfList;
end;


function TMemoryList.newNode (obj : TRhodusObject) : PMemoryNodePtr;
begin
  new (result);
  result^.robj := obj;
  result^.next := nil;
end;


function TMemoryList.addNode (robj : TRhodusObject) : PMemoryNodePtr;
var cursor : PMemoryNodePtr;
begin
   inc (sizeOfList);
   cursor := newNode(robj);
   if head = nil then
       begin
       head := cursor;   // when linked list is empty
       tail := cursor;
       end
   else
       begin
       tail^.next := cursor;
       tail := cursor;
       end;
   result := tail;
end;


function TMemoryList.mapMemory : string;
var cursor : PMemoryNodePtr;
    count : integer;
    blockStr : string;
begin
  cursor := head; count := 1; result := '';
  while cursor <> nil do
        begin
        if cursor^.robj.isBound   then blockStr := 'B';
        if cursor^.robj.isOwned   then blockStr := 'O';
        if cursor^.robj.isGarbage then blockStr := 'G';
        if cursor^.robj.objectType = symUserFunc then
           if (cursor^.robj as TUserFunction).isbuiltInFunction then
              begin
              blockStr := '';
              dec (count); // don't count these
              end;
        result := result + blockStr;
        cursor := cursor^.next;
        inc (count);
        if (count mod 70) = 0 then
           result := result + sLineBreak;
        end;
end;


function TMemoryList.mapMemoryDetailed : string;
var cursor : PMemoryNodePtr;
    count : integer;
    blockStr : string;
begin
  cursor := head; count := 1; result := 'Note: builtin functions are not listed here' + sLineBreak;
  while cursor <> nil do
        begin
        if cursor^.robj.isBound   then blockStr := 'B';
        if cursor^.robj.isOwned   then blockStr := 'O';
        if cursor^.robj.isGarbage then blockStr := 'G';
        case cursor^.robj.objectType of
            symList : begin
                      result := result + blockStr + ' : list' + sLineBreak;
                      end;
            symString :
                      begin
                      result := result + blockStr + ' : string' + sLineBreak;
                      end;
            symUserFunc : begin
                          if not (cursor^.robj as TUserFunction).isbuiltInFunction then
                             begin
                             result := result + blockStr + ' : userFunction <';
                             result := result + (cursor^.robj as TUserFunction).name + '>';
                             result := result + sLineBreak;
                             end
                          else
                             dec (count);
                          end
        else
           result := result + ' : unknown';
        end;
        cursor := cursor^.next;
        inc (count);
        if (count mod 70) = 0 then
           result := result + sLineBreak;
        end;
end;

// Find out the number of allocated nodes.
function TMemoryList.getNumberAllocatedNodes : TMemorySummary;
var temp : PMemoryNodePtr;
begin
  result.totalAllocated := 0; result.numGarbage := 0;
  result.numConstant := 0;    result.numBound := 0;
  result.numOwned := 0;
  temp := head;
  while temp <> nil do
        begin
        if temp^.robj.objectType = symUserFunc then
           begin
           if (temp^.robj as TUserFunction).isbuiltInFunction then
              begin
              // ignore builtins
              temp := temp^.next;
              end
           else
              // Applies to user functions that are not builtins
              begin
              inc (result.totalAllocated );
              if temp^.robj.isConstant then
                 inc (result.numConstant);
              if temp^.robj.isGarbage  then
                 inc (result.numGarbage);
              if temp^.robj.isBound    then
                 inc (result.numBound);
              if temp^.robj.isOwned    then
                 inc (result.numOwned);
              temp := temp^.next;
              end;
           end
        else
           begin
           // Everything else
           inc (result.totalAllocated );
           if temp^.robj.isConstant then
              inc (result.numConstant);
           if temp^.robj.isGarbage  then
              inc (result.numGarbage);
           if temp^.robj.isBound    then
              inc (result.numBound);
           if temp^.robj.isOwned    then
              inc (result.numOwned);
           temp := temp^.next;
           end;
        end;
end;


procedure TMemoryList.freeFirstNode (var cursor : PMemoryNodePtr);
begin
  cursor := cursor^.next;
  head^.robj.Free;
  dispose (head);
  head := cursor;
end;


procedure TMemoryList.freeLastNode  (var cursor, previous : PMemoryNodePtr);
begin
   tail := previous;
   previous^.next := nil; // Make sure previous one now points to nil.
   cursor^.robj.Free;
   dispose (cursor);
   cursor := nil;
end;


procedure TMemoryList.freeMiddleNode (var cursor, previous : PMemoryNodePtr);
begin
   previous^.next := cursor^.next;
   cursor^.robj.Free;
   dispose (cursor);
   cursor := previous^.next;
end;


procedure TMemoryList.freeGarbage;
var cursor, previous : PMemoryNodePtr;
begin
  if head = nil then
      exit;

  cursor := head;
  while cursor <> nil do
        begin
        if cursor^.robj.isGarbage then
           begin
           dec (sizeOfList);
           if head = cursor then  // We're at the first node
              freeFirstNode (cursor)
           else
              begin  // Then we're in the middle or at the end
              // If we're at the end...
              if cursor^.next = nil then
                 freeLastNode (cursor, previous)
              else
                 freeMiddleNode (cursor, previous);
              end;
           end
        else
           begin
           previous := cursor;
           cursor := cursor^.next;
           end;
        end;
end;


// Free the entire list, called when application exits.
procedure TMemoryList.freeList;
var cursor : PMemoryNodePtr;
begin
  while head <> nil do
        begin
        cursor := head;
        head := head^.next;
        cursor^.robj.Free;
        cursor^.robj := nil;
        Dispose (cursor);
        end;
  head := nil;
  tail := nil;
  sizeOfList := 0;
end;


initialization
  memoryList := TMemorylist.Create;
finalization
  memoryList.Free;
end.
