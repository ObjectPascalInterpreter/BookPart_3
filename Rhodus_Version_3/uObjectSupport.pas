unit uObjectSupport;

// This source is distributed under Apache 2.0

// Copyright (C) 2019-2024 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

// Base class for all mathods and constants attached to objects, eg math.sin (3) or math.pi
// Methods automatically get dir and help support



interface

Uses Generics.Collections, uHelpUnit;

type
   TObjectMethod = procedure (vm : TObject) of object;

   TMethodDetails = class (TObject)
        name : string;
        helpStr : string;
        nArgs : integer;
        method : TObjectMethod;
        self : pointer;  // Only used during execution but during execution to the 'self' associated with this method
        constructor Create (const name : string; nArgs: integer; const helpStr : string; helpObj : THelp; fcn : TObjectMethod); overload;
        constructor Create (const name : string; nArgs: integer; const helpStr : string; fcn : TObjectMethod); overload;
   end;

   TMethodsBase = class;
   TMethodList = class (TList<TMethodDetails>)
        function find (const name : string) : TMethodDetails;
        constructor Create (methodBase : TMethodsBase);
   end;

   // TMethodBase is the parent of the object that will hold the
   // methods that are applicable to the object its attached to.
   // methodlist is a reference to the list of methods.
   // Note, don't free methodlist as TMethodBase doesn't own it.
   // MethodsBase automatically provides dir() and help() methods
   TMethodsBase = class (TObject)
     methodList : TMethodList;
     procedure   dir (vm : TObject);
     procedure   getHelp (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
   end;

implementation

Uses uListObject,
     uStringObject,
     uRhodusObject,
     uMachineStack,
     uVM;

constructor TMethodsBase.Create;
begin
  inherited Create;
end;


destructor TMethodsBase.Destroy;
begin
  for var i := 0 to methodList.Count - 1 do
      methodList[i].Free;
  methodlist.Free;
  inherited;
end;


procedure TMethodsBase.dir(vm: TObject);
var ls : TListObject;
begin
  TVM (vm).popMethodDetails;
  ls := TListObject.create (0);
  for var i := 0 to methodList.Count - 1 do
      begin
      ls.append (TStringObject.Create (methodList[i].name));
      end;

  TVM (vm).push (ls);
end;


procedure TMethodsBase.getHelp(vm: TObject);
var m : TMethodDetails;
    obj : TRhodusObject;
begin
  m := TVM (vm).popMethodDetails();
  obj := TRhodusObject (m.self);

  if obj.help <> nil then
     begin
      TVM (vm).push(TStringObject.Create(obj.help.getHelp()));
     end
  else
     TVM (vm).push (TStringObject.Create('No help'));
end;


constructor TMethodList.Create (methodBase : TMethodsBase);
begin
  inherited Create;

  self.Add(TMethodDetails.Create ('help',   0, 'Returns the help associated with this symbol', methodBase.getHelp));
  self.Add(TMethodDetails.Create ('dir',    0, 'dir of string object methods', methodBase.dir));
end;

// This could be replaced at some point with a dictionary
function TMethodList.find (const name : string) : TMethodDetails;
begin
  for var i := 0 to Count - 1 do
      if self[i].name = name then
         begin
         result := self[i];
         exit;
         end;
  result := nil;
end;



// -------------------------------------------------------------------------------

constructor TMethodDetails.Create (const name : string; nArgs : integer; const helpStr : string;  helpObj : THelp; fcn : TObjectMethod);
begin
  inherited Create;
  self.name := name;
  self.helpStr := helpStr;
  self.nArgs := nArgs;
  self.method := fcn;
end;

constructor TMethodDetails.Create (const name : string; nArgs : integer; const helpStr : string; fcn : TObjectMethod);
begin
  inherited Create;
  self.name := name;
  self.helpStr := helpStr;
  self.nArgs := nArgs;
  self.method := fcn;
end;

end.
