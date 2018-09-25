unit SimpleAPI.Utils;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults, System.Rtti, JsonDataObjects,
  FireDAC.Comp.Client;

type

  TDBConnectionManager = class
  private
    class var FConnectionsByThreadIds: TDictionary<cardinal, TFDConnection>;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure Use(c: TFDConnection);
    class procedure Unuse;
    class function Get: TFDConnection;
    class function CreateQuery: TFDQuery;
  end;

  TCharSet = set of Char;

  TCustomAttributeClass = class of TCustomAttribute;

  TArrayHelper = class abstract
  private
    class procedure QuickSort<T>(var Arr: TArray<T>; Comparer: IComparer<T>; const l, r: integer);
  public
    class function Contains<T>(const Arr: TArray<T>; const Element: T): boolean; inline;
    class function Find<T>(const Arr: TArray<T>; const Element: T): integer; overload; inline;
    class function Find<T>(const Arr: TArray<T>; const Predicat: TFunc<T,boolean>): integer; overload; inline;
    class function FindOfClass<T, T1: class>(const Arr: TArray<T>): integer; inline;
    class function ContainsOfClass<T, T1: class>(const Arr: TArray<T>): boolean; inline;
    class procedure Add<T>(var Arr: TArray<T>; Element: T); inline;
    class procedure Remove<T>(var Arr: TArray<T>; Element: T); overload; inline;
    class procedure Remove<T>(var Arr: TArray<T>; const Predicat: TFunc<T,boolean>); overload; inline;
    class procedure Delete<T>(var Arr: TArray<T>; Index: integer); inline;
    class procedure Swap<T>(var Arr: TArray<T>; const Index1, Index2: integer); inline;
    class procedure Sort<T>(var Arr: TArray<T>; Comparer: IComparer<T>); inline;
    class procedure Reverse<T>(var Arr: TArray<T>); inline;
    class procedure Insert<T>(var Arr: TArray<T>; Index: integer; Element: T); inline;
    class function GetRange<T>(var Arr: TArray<T>; Index, Count: integer): TArray<T>; inline;
    class procedure ForEach<T>(const Arr: TArray<T>; Callback: TProc<T>); inline;
    class function FilterByClass<T1:class; T2:class>(const Arr: TArray<T1>): TArray<T2>; inline;
  end;

  TArrayWithCounter = class abstract
    class procedure Add<T>(var Arr: TArray<T>; Element: T; var Counter: integer); inline;
    class procedure AddRange<T>(var Arr: TArray<T>; Elements: TArray<T>; var Counter: integer); overload; inline;
    class procedure AddRange<T>(var Arr: TArray<T>; Elements: TArray<T>; var Counter: integer; const ElementsCounter: integer); overload; inline;
    class procedure Remove<T>(var Arr: TArray<T>; Element: T; var Counter: integer); inline;
    class procedure Delete<T>(var Arr: TArray<T>; Index: integer; var Counter: integer); inline;
    class function Find<T>(const Arr: TArray<T>; Element: T; var Counter: integer): integer; inline;
    class procedure Trim<T>(var Arr: TArray<T>; var Counter: integer); inline;
    class function Contains<T>(const Arr: TArray<T>; Element: T; var Counter: integer): boolean; inline;
  end;

procedure _(const Subprogram: TProc); inline;

function CheckString(s: TArray<string>; LengthRange: TArray<integer>; const alphabet: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '_']): boolean;
function CheckJson(const JsonStr: string): boolean;

function LoadStringFromFile(const Filename: string): string;

function FindAttribute(Attrs: TArray<TCustomAttribute>; AttrClass: TCustomAttributeClass): TCustomAttribute;

function GetValueFromString(const s: string; TargetType: TRttiType; out InvalidParamValue: boolean): TValue;

function StringParamsToDictionary(s: TStrings): TDictionary<string, string>;

function GenerateUniqueId: string;

function CheckJsonObject(const JsonStr: string): TJsonObject;

implementation

function GenerateUniqueId: string;
var
  g: TGUID;
begin
  CreateGUID(g);
  SetLength(result, 32);
  StrLFmt(PChar(result), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [g.D1, g.D2, g.D3, g.D4[0], g.D4[1], g.D4[2], g.D4[3], g.D4[4], g.D4[5], g.D4[6], g.D4[7]]);
end;

function CheckJsonObject(const JsonStr: string): TJsonObject;
var
  Json: TJsonBaseObject;
begin
  result := nil;
  try
    Json := TJsonBaseObject.Parse(JsonStr);
  finally
    if (Json <> nil) and (Json is TJsonObject) then
      result := TJsonObject(Json)
    else if Json <> nil then
      Json.DisposeOf;
  end;
end;

function StringParamsToDictionary(s: TStrings): TDictionary<string, string>;
var
  i: integer;
begin
  result := TDictionary<string, string>.Create;
  for i := 0 to s.Count - 1 do
    result.AddOrSetValue(AnsiLowerCase(s.Names[i]), s.ValueFromIndex[i]);
end;

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
      tkInteger, tkEnumeration, tkSet:
        begin
          if TargetType.QualifiedName = 'System.Boolean' then
            result := TValue.From<Boolean>((LowerCase(s) = 'true') or (s = '1'))
          else
            result := TValue.From<Integer>(s.ToInteger);
        end;
      tkInt64:
        result := TValue.From<Int64>(s.ToInt64);
      tkChar, tkWideChar:
        result := TValue.From<Char>(s[1]);
      tkFloat:
        result := TValue.From<Double>(s.ToDouble);
      tkString, tkLString, tkWString, tkUString:
        result := TValue.From<string>(s);
      tkVariant:
        result := s;
      else
        InvalidParamValue := true;
    end;
  except
    InvalidParamValue := true;
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
    sl.LoadFromFile(Filename, TEncoding.UTF8);
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

{ TDBConnectionManager }

class constructor TDBConnectionManager.Create;
begin
  FConnectionsByThreadIds := TDictionary<cardinal, TFDConnection>.Create;
end;

class destructor TDBConnectionManager.Destroy;
begin
  FConnectionsByThreadIds.DisposeOf;
end;

class function TDBConnectionManager.Get: TFDConnection;
var
  ThreadId: Cardinal;
begin
  ThreadId := TThread.CurrentThread.ThreadID;
  result := nil;
  if FConnectionsByThreadIds.ContainsKey(ThreadId) then
    result := FConnectionsByThreadIds[ThreadId];
end;

class function TDBConnectionManager.CreateQuery: TFDQuery;
var
  c: TFDConnection;
begin
  result := nil;
  c := Get;
  if c = nil then
    exit;
  result := TFDQuery.Create(nil);
  result.Connection := c;
end;

class procedure TDBConnectionManager.Use(c: TFDConnection);
begin
  FConnectionsByThreadIds.AddOrSetValue(TThread.CurrentThread.ThreadID, c);
end;

class procedure TDBConnectionManager.Unuse;
var
  ThreadId: Cardinal;
begin
  ThreadId := TThread.CurrentThread.ThreadID;
  if FConnectionsByThreadIds.ContainsKey(ThreadId) then
    FConnectionsByThreadIds.Remove(ThreadId);
end;

{ TArrayHelper }

class function TArrayHelper.Contains<T>(const Arr: TArray<T>; const Element: T): boolean;
begin
  result := Find<T>(Arr, Element) <> -1;
end;

class function TArrayHelper.ContainsOfClass<T, T1>(const Arr: TArray<T>): boolean;
begin
  result := FindOfClass<T, T1>(Arr) <> -1;
end;

class function TArrayHelper.FilterByClass<T1, T2>(const Arr: TArray<T1>): TArray<T2>;
var
  i, c: integer;
begin
  SetLength(result, 100);
  c := 0;
  for i := 0 to Length(Arr) - 1 do
    if Arr[i] is T2 then
      TArrayWithCounter.Add<T2>(result, Arr[i] as T2, c);
  TArrayWithCounter.Trim<T2>(result, c);
end;

class function TArrayHelper.Find<T>(const Arr: TArray<T>; const Element: T): integer;
var
  Comparer: IComparer<T>;
  i: integer;
begin
  result := -1;
  Comparer := TComparer<T>.Default;
  for i := 0 to Length(Arr) - 1 do
    if Comparer.Compare(Arr[i], Element) = 0 then
    begin
      result := i;
      break;
    end;
end;

class function TArrayHelper.Find<T>(const Arr: TArray<T>; const Predicat: TFunc<T, boolean>): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Length(Arr) - 1 do
    if Predicat(Arr[i]) then
    begin
      result := i;
      break;
    end;
end;

class function TArrayHelper.FindOfClass<T, T1>(const Arr: TArray<T>): integer;
var
  Comparer: IComparer<T>;
  i: integer;
begin
  result := -1;
  Comparer := TComparer<T>.Default;
  for i := 0 to Length(Arr) - 1 do
    if TObject(Arr[i]) is T1 then
    begin
      result := i;
      break;
    end;
end;

class procedure TArrayHelper.ForEach<T>(const Arr: TArray<T>; Callback: TProc<T>);
var
  o: T;
begin
  for o in Arr do
    Callback(o);
end;

class function TArrayHelper.GetRange<T>(var Arr: TArray<T>; Index, Count: integer): TArray<T>;
begin
  result := Copy(Arr, Index, Count);
end;

class procedure TArrayHelper.Insert<T>(var Arr: TArray<T>; Index: integer; Element: T);
var
  l, i: integer;
begin
  l := Length(Arr) + 1;
  SetLength(Arr, l);

  for i := l - 1 downto Index + 1 do
    Arr[i] := Arr[i - 1];

  Arr[i] := Element;
end;

class procedure TArrayHelper.Add<T>(var Arr: TArray<T>; Element: T);
var
  l: integer;
begin
  l := Length(Arr) + 1;
  SetLength(Arr, l);
  Arr[l - 1] := Element;
end;

class procedure TArrayHelper.Remove<T>(var Arr: TArray<T>; Element: T);
var
  i: integer;
begin
  i := Find<T>(Arr, Element);
  if i > -1 then
    Delete<T>(Arr, i);
end;

class procedure TArrayHelper.Delete<T>(var Arr: TArray<T>; Index: integer);
var
  i: integer;
begin
  if Index = -1 then
    exit;
  for i := Index to Length(Arr) - 2 do
    Arr[i] := Arr[i+1];
  SetLength(Arr, Length(Arr) - 1);
end;

class procedure TArrayHelper.Remove<T>(var Arr: TArray<T>; const Predicat: TFunc<T, boolean>);
var
  i: integer;
begin
  for i := Length(Arr) - 1 downto 0 do
    if Predicat(Arr[i]) then
      Delete<T>(Arr, i);
end;

class procedure TArrayHelper.Reverse<T>(var Arr: TArray<T>);
var
  i, len: integer;
begin
  len := Length(Arr);
  if len < 2 then
    exit;

  for i := 0 to len div 2 do
    Swap<T>(Arr, i, len - 1 - i);
end;

class procedure TArrayHelper.Swap<T>(var Arr: TArray<T>; const Index1, Index2: integer);
var
  a: T;
begin
  a := Arr[Index1];
  Arr[Index1] := Arr[Index2];
  Arr[Index2] := a;
end;

class procedure TArrayHelper.QuickSort<T>(var Arr: TArray<T>; Comparer: IComparer<T>; const l, r: integer);
var
  m, i, j: integer;
  x: T;
begin
  if r = l then
    exit;
  if r - l = 1 then
    if Comparer.Compare(Arr[l], Arr[r]) = 1 then
      TArrayHelper.Swap<T>(Arr, l, r);

  m := l + (r - l) div 2;

  if ((Comparer.Compare(Arr[l], Arr[r]) = 1) and (Comparer.Compare(Arr[m], Arr[l]) = 1)) or
    ((Comparer.Compare(Arr[l], Arr[m]) = 1) and (Comparer.Compare(Arr[r], Arr[l]) = 1)) then
    x := Arr[l]
  else if ((Comparer.Compare(Arr[r], Arr[l]) = 1) and (Comparer.Compare(Arr[m], Arr[r]) = 1)) or
    ((Comparer.Compare(Arr[r], Arr[m]) = 1) and (Comparer.Compare(Arr[l], Arr[r]) = 1)) then
    x := Arr[r]
  else
    x := Arr[m];

  i := l;
  j := r;
  while i <= j do
  begin
    while Comparer.Compare(Arr[i], x) < 0 do
      inc(i);
    while Comparer.Compare(Arr[j], x) > 0 do
      dec(j);
    if i <= j then
    begin
      TArrayHelper.Swap<T>(Arr, i, j);
      inc(i);
      dec(j);
    end;
  end;

  if j > l then
    QuickSort(Arr, Comparer, l, j);
  if i < r then
    QuickSort(Arr, Comparer, i, r);
end;

class procedure TArrayHelper.Sort<T>(var Arr: TArray<T>; Comparer: IComparer<T>);
begin
  if Length(Arr) < 2 then
    exit;

  QuickSort<T>(Arr, Comparer, 0, Length(Arr) - 1);
end;

{ TArrayWithCounter }

class procedure TArrayWithCounter.Add<T>(var Arr: TArray<T>; Element: T; var Counter: integer);
begin
  if Length(Arr) = Counter then
    SetLength(Arr, Counter * 2 + 1);

  Arr[Counter] := Element;
  Inc(Counter);
end;

class procedure TArrayWithCounter.AddRange<T>(var Arr: TArray<T>; Elements: TArray<T>; var Counter: integer);
var
  i, NewSize: integer;
begin
  NewSize := Counter;

  while Length(Arr) <= Counter + Length(Elements) do
  begin
    NewSize := NewSize * 2;
    SetLength(Arr, NewSize);
  end;

  for i := 0 to Length(Elements) - 1 do
  begin
    Arr[Counter] := Elements[i];
    inc(Counter);
  end;
end;

class procedure TArrayWithCounter.AddRange<T>(var Arr: TArray<T>; Elements: TArray<T>; var Counter: integer;
  const ElementsCounter: integer);
var
  i, NewSize: integer;
begin
  NewSize := Counter;

  while Length(Arr) <= Counter + ElementsCounter do
  begin
    NewSize := NewSize * 2;
    SetLength(Arr, NewSize);
  end;

  for i := 0 to ElementsCounter - 1 do
  begin
    Arr[Counter] := Elements[i];
    inc(Counter);
  end;
end;

class function TArrayWithCounter.Contains<T>(const Arr: TArray<T>; Element: T; var Counter: integer): boolean;
begin
  result := Find<T>(Arr, Element, Counter) <> -1;
end;

class procedure TArrayWithCounter.Delete<T>(var Arr: TArray<T>; Index: integer; var Counter: integer);
var
  i: integer;
begin
  if Index = -1 then
    exit;
  for i := Index to Counter - 2 do
    Arr[i] := Arr[i+1];
  Dec(Counter);
end;

class function TArrayWithCounter.Find<T>(const Arr: TArray<T>; Element: T; var Counter: integer): integer;
var
  Comparer: IComparer<T>;
  i: integer;
begin
  result := -1;
  Comparer := TComparer<T>.Default;
  for i := 0 to Counter - 1 do
    if Comparer.Compare(Arr[i], Element) = 0 then
    begin
      result := i;
      break;
    end;
end;

class procedure TArrayWithCounter.Remove<T>(var Arr: TArray<T>; Element: T; var Counter: integer);
begin
  Delete<T>(Arr, Find<T>(Arr, Element, Counter), Counter);
end;

class procedure TArrayWithCounter.Trim<T>(var Arr: TArray<T>; var Counter: integer);
begin
  SetLength(Arr, Counter);
end;

end.