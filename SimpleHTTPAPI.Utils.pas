unit SimpleHTTPAPI.Utils;

interface

uses
  System.SysUtils, System.Classes, JsonDataObjects;

type
  TCharSet = set of Char;

procedure _(const Subprogram: TProc); inline;

function CheckString(s: TArray<string>; LengthRange: TArray<integer>; const alphabet: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '_']): boolean;
function CheckJson(const JsonStr: string): boolean;

function LoadStringFromFile(const Filename: string): string;

implementation

function LoadStringFromFile(const Filename: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    result := sl.Text;
  except
    result := '';
  end;
  sl.DisposeOf;
end;

procedure _(const Subprogram: TProc); inline;
begin
  Subprogram();
end;

function CheckString(s: TArray<string>; LengthRange: TArray<integer>; const alphabet: TCharSet): boolean;
var
  i, j, k: integer;
begin
  result := true;
  for j := 0 to Length(s) - 1 do
  begin
    if (Length(s[j]) < LengthRange[0]) or (Length(s[j]) > LengthRange[1]) then
      exit(false);
    for i := 1 to Length(s[j]) do
      if not (s[j][i] in alphabet) then
        exit(false);
  end;
end;

function CheckJson(const JsonStr: string): boolean;
var
  Json: TJsonBaseObject;
begin
  result := false;
  try
    Json := TJsonBaseObject.Parse(JsonStr);
  finally
    result := Json <> nil;
    if Json <> nil then
      Json.DisposeOf;
  end;
end;

end.