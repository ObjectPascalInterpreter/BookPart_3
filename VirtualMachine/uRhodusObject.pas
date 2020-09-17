unit uRhodusObject;

interface

Uses Classes, SysUtils;

type
  TObjectType = (otList);

  TRhodusObject = class (TObject)
      objectType : TObjectType;
      value : TObject;

      function isList : boolean;
      function getString : string;

      constructor Create;
  end;

implementation

Uses uListObject;

constructor TRhodusObject.Create;
begin
  inherited Create;
end;

function TRhodusObject.isList : boolean;
begin
  if objectType = otList then
     result := True
  else
     result := False;
end;


function TRhodusObject.getString : string;
begin
  case objectType of
    otList: result := TListObject (value).listToString;
  else
     raise Exception.Create('Unknown object type in rhodusobject toString');

  end;
end;

end.
