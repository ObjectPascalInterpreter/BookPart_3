unit uListOfBuiltIns;

interface

Uses Classes, Generics.Collections, uLibModule;

type
  TBuiltIn = class (TObject)
     name : string;
     className : string;
     constructor Create (name, classname : string);
     destructor  Destroy; override;
  end;

  // Use TObjectList so that it frees all objects in the list when it is freed.
  TListOfBuiltIns = class (TObjectList<TBuiltIn>)
       function find (name : string; var index : integer) : boolean;
  end;

var listofBuiltIns : TListOfBuiltIns;

implementation

Uses uBuiltInMath,
     uBuiltInSys,
     uBuiltInConfig,
     uBuiltInArray,
     uBuiltInMatrix,
     uBuiltInRandom,
     uBuiltInFile,
     uBuiltInOS,
     uBuiltInStr;


constructor TBuiltIn.Create (name, className : string);
begin
  inherited Create;
  self.name := name;
  self.className := className;
end;

destructor TBuiltIn.Destroy;
begin
  inherited;
end;


function TListOfBuiltIns.find (name : string; var index : integer) : boolean;
begin
  result := false;
  for var i := 0 to Count - 1 do
      if self[i].name = name then
         begin
         index := i;
         exit (True);
         end;
end;


initialization
  listOfBuiltIns := TListOfBuiltIns.Create;

  listOfBuiltIns.add (TBuiltIn.Create ('math',   'uBuiltInMath.'   + TBuiltInMath.ClassName));
  listOfBuiltIns.add (TBuiltIn.Create ('os',     'uBuiltInOS.'     + TBuiltInOS.ClassName));
  listOfBuiltIns.Add (TBuiltIn.Create ('sys',    'uBuiltInSys.'    + TBuiltInSys.ClassName));
  listOfBuiltIns.Add (TBuiltIn.Create ('config', 'uBuiltInConfig.' + TBuiltInConfig.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('random', 'uBuiltInRandom.' + TBuiltInRandom.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('arrays', 'uBuiltInArray.'  + TBuiltInArray.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('mat',    'uBuiltInMatrix.' + TBuiltInMatrix.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('file',   'uBuiltInFile.'   + TBuiltInFile.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('time',   'uBuiltInOS.'     + TBuiltInTime.ClassName));
  listofBuiltIns.Add (TBuiltIn.Create ('strings','uBuiltInStrings.' + TBuiltInStr.ClassName));
finalization
  listOfBuiltIns.Free;
end.

