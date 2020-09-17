unit uStringObject;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

uses Classes, uMemoryManager;

type
  TStringObject = class (TRhodusObject)

     value : string;

     function        isEqualTo (str1 : TStringObject) : boolean;
     class function  add (str1, str2 : TStringObject) : TStringObject;
     function        clone : TStringObject;
     constructor     createConstantObj (value : string);
     function        getSize() : integer;
     constructor     create (value : string);
     destructor      Destroy; override;
  end;

implementation

function createStringObject (value : string) : TStringObject;
begin
  result := TStringObject.Create (value);
end;


constructor TStringObject.createConstantObj (value : string);
begin
  blockType := btConstant;
  self.value := value;
end;


constructor TStringObject.Create (value : string);
begin
  blockType := btGarbage;
  self.value := value;
  memoryList.addNode (self);
end;


destructor TStringObject.Destroy;
begin
  value := '';
  inherited;
end;


function TStringObject.clone : TStringObject;
begin
  result := TStringObject.Create (value);
end;


function TStringObject.getSize() : integer;
begin
  result := self.InstanceSize;
  result := result + Length (value);
end;


function TStringObject.isEqualTo (str1 : TStringObject) : boolean;
begin
  result := self.value = str1.value;
end;


class function TStringObject.add (str1, str2 : TStringObject) : TStringObject;
begin
  result := TStringObject.Create (str1.value + str2.value);
end;


end.
