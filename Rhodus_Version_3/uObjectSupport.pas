unit uObjectSupport;

interface

Uses Generics.Collections;

type
   TObjectMethod = procedure (vm : TObject) of object;

   TMethodDetails = class (TObject)
        name : string;
        helpStr : string;
        nArgs : integer;
        method : TObjectMethod;
        constructor Create (const name : string; nArgs: integer; const helpStr : string; fcn : TObjectMethod);
   end;

   TMethodList = class (TList<TMethodDetails>)
        function find (const name : string) : TMethodDetails;
   end;

   // TMethodBase is the parent of the object that will hold the
   // methods that are applicable to the object its attached to.
   // methodlist is a reference to the list of methods so that
   // we can implement dir(). Note, don't free this reference here
   // as TMethodBase doesn't own it.
   TMethodsBase = class (TObject)
     methodList : TMethodList;
     procedure   dir (vm : TObject);
     constructor Create;
     destructor  Destroy; override;
   end;

implementation

Uses uListObject,
     uStringObject,
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
  TVM (vm).decStackTop;
  ls := TListObject.create (0);
  for var i := 0 to methodList.Count - 1 do
      begin
      ls.append (TStringObject.Create (methodList[i].name));
      end;

  TVM (vm).pop(); // Dump the object
  TVM (vm).push (ls);
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

constructor TMethodDetails.Create (const name : string; nArgs : integer; const helpStr : string;  fcn : TObjectMethod);
begin
  inherited Create;
  self.name := name;
  self.helpStr := helpStr;
  self.nArgs := nArgs;
  self.method := fcn;
end;


end.
