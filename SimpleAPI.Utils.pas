unit SimpleAPI.Utils;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, JsonDataObjects;

type

  TCharSet = set of Char;

  TCustomAttributeClass = class of TCustomAttribute;

procedure _(const Subprogram: TProc); inline;

function CheckString(s: TArray<string>; LengthRange: TArray<integer>; const alphabet: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '_']): boolean;
function CheckJson(const JsonStr: string): boolean;

function LoadStringFromFile(const Filename: string): string;

function FindAttribute(Attrs: TArray<TCustomAttribute>; AttrClass: TCustomAttributeClass): TCustomAttribute;

function GetValueFromString(const s: string; TargetType: TRttiType; out InvalidParamValue: boolean): TValue;

function GetParamValue(sl: TStrings; const Name: string; out Exists: boolean): string;

implementation

function GetValueFromString(const s: string; TargetType: TRttiType; out InvalidParamValue: boolean): TValue;
var
  o: TObject;
begin
  InvalidParamValue := false;

  if (TargetType.QualifiedName = 'JsonDataObjects.TJsonObject') or
    (TargetType.QualifiedName = 'JsonDataObjects.TJsonArray') or
    (TargetType.QualifiedName = 'JsonDataObjects.TJsonBaseObject') then
  begin
    try
      o := TJsonBaseObject.Parse(s);
      result := TValue.From<TObject>(o);
    except
      InvalidParamValue := true;
    end;
    exit;
  end;

  try
    case TargetType.TypeKind of
      tkInteger, tkEnumeration, tkSet:              result := TValue.From<Integer>(s.ToInteger);
      tkInt64:                                      result := TValue.From<Int64>(s.ToInt64);
      tkChar, tkWideChar:                           result := TValue.From<Char>(s[1]);
      tkFloat:                                      result := TValue.From<Double>(s.ToDouble);
      tkString, tkLString, tkWString, tkUString:    result := TValue.From<string>(s);
      tkVariant:                                    result := s;
      else
        InvalidParamValue := true;
    end;
  except
    InvalidParamValue := true;
  end;
end;

function GetParamValue(sl: TStrings; const Name: string; out Exists: boolean): string;
var
  i: integer;
  s: string;
  Splitted: TArray<string>;
  LowerCasedName: string;
begin
  result := '';
  Exists := false;
  LowerCasedName := AnsiLowerCase(Name);
  for i := 0 to sl.Count - 1 do
  begin
    s := sl[i];
    Splitted := s.Split(['=']);
    if Length(Splitted) < 2 then
      continue;
    if AnsiLowerCase(Splitted[0]) = LowerCasedName then
    begin
      Exists := true;
      exit(Splitted[1]);
    end;
  end;
end;

function FindAttribute(Attrs: TArray<TCustomAttribute>; AttrClass: TCustomAttributeClass): TCustomAttribute;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Length(Attrs) - 1 do
    if Attrs[i] is AttrClass then
      exit(Attrs[i]);
end;

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