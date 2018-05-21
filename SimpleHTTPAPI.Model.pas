unit SimpleHTTPAPI.Model;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Rtti,

  FireDAC.Comp.Client,

  IdCustomHTTPServer,

  SimpleHTTPAPI;

type

  // Attributes used for model descriptions

  TCustomAttributeClass = class of TCustomAttribute;

  ModelAttribute = class(TCustomAttribute)
  public
    FName: string;
    constructor Create(const AName: string);
  end;

  ActionAttribute = class(TCustomAttribute)
  public
    FName: string;
    FMethod: string;
    constructor Create(const AName: string; const AMethod: string = 'GET');
  end;

  PublicAccessAttribute = class(TCustomAttribute);

  // Base TModel class

  TMethodData = record
    RttiMethod: TRttiMethod;
    PublicAccess: boolean;
  end;

  TModelClass = class of TModel;

  TModel = class abstract
  private class var
    FRTTIContext: TRttiContext;
    FModels: TDictionary<string, TModel>;

  private
    FActions: TDictionary<string, TMethodData>;

    FFDConnection: TFDConnection;
    FFDQuery: TFDQuery;
    FInput: TIdHTTPRequestInfo;
    FOutput: TIdHTTPResponseInfo;
    FURI: TArray<string>;
    FUserObject: TUserObject;

  public
    class procedure Execute(Input: TIdHTTPRequestInfo; Output: TIdHTTPResponseInfo;
      FDConnection: TFDConnection; FDQuery: TFDQuery; UserObjectClass: TUserObjectClass);

    property Input: TIdHTTPRequestInfo read FInput;
    property Output: TIdHTTPResponseInfo read FOutput;
    property FDConnection: TFDConnection read FFDConnection;
    property FDQuery: TFDQuery read FFDQuery;
    property URI: TArray<string> read FURI;
    property UserObject: TUserObject read FUserObject;

    constructor Create;
    procedure DisposeOf; reintroduce; virtual;

    class procedure Init;
    class destructor Destroy;
    class procedure Register;
  end;

implementation

uses
  SimpleHTTPAPI.Utils, SimpleHTTPAPI.Model.Accounts;

{ ModelAttribute }

constructor ModelAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ActionAttribute }

constructor ActionAttribute.Create(const AName: string; const AMethod: string = 'GET');
begin
  FName := AName;
  FMethod := AMethod;
end;

{ TModel }

class procedure TModel.Init;
begin
  if FModels <> nil then
    exit;

  FRTTIContext := TRttiContext.Create;
  FModels := TDictionary<string, TModel>.Create;
end;

class destructor TModel.Destroy;
var
  m: TModel;
begin
  for m in FModels.Values.ToArray do
    m.DisposeOf;
  FModels.DisposeOf;

  FRTTIContext.Free;
end;

class procedure TModel.Register;
var
  Attrs: TArray<TCustomAttribute>;
begin
  TModel.Init;

  Attrs := FRTTIContext.GetType(Self).GetAttributes;
  if (Length(Attrs) > 0) and (Attrs[0] is ModelAttribute) then
    FModels.Add(LowerCase(ModelAttribute(Attrs[0]).FName), Self.Create);
end;

class procedure TModel.Execute(Input: TIdHTTPRequestInfo; Output: TIdHTTPResponseInfo;
  FDConnection: TFDConnection; FDQuery: TFDQuery; UserObjectClass: TUserObjectClass);
var
  Uri: string;
  SplittedUri: TArray<string>;
  Method, Model, Action: string;
  m, m1: TModel;
  ActionId: string;
  md: TMethodData;
begin
  Uri := Input.URI;
  if Uri.EndsWith('/') then
    Uri := Copy(Uri, 1, Length(Uri) - 1);
  SplittedUri := Uri.Split(['/']);
  if Length(SplittedUri) < 3 then
    exit;

  Method := UpperCase(Input.Command);
  Model := LowerCase(SplittedUri[1]);
  Action := LowerCase(SplittedUri[2]);

  if FModels.ContainsKey(LowerCase(Model)) then
  begin
    m := FModels[Model];

    ActionId := Method + '_' + Action;
    if m.FActions.ContainsKey(ActionId) then
    begin
      md := m.FActions[ActionId];

      m1 := m.NewInstance as TModel;

      m1.FFDConnection := FDConnection;
      m1.FFDQuery := FDQuery;
      m1.FInput := Input;
      m1.FOutput := Output;
      m1.FURI := SplittedUri;
      m1.FUserObject := nil;

      if (not md.PublicAccess) and (Length(SplittedUri) > 3) and CheckString([SplittedUri[3]], [64, 64]) then
        m1.FUserObject := TAccountsModel.GetUserObjectByToken(SplittedUri[3], FDConnection, UserObjectClass);

      if md.PublicAccess or (m1.FUserObject <> nil) then
        md.RttiMethod.Invoke(m1, [])
      else
        Output.ContentText := '{"error":"auth_failed"}';

      m1.FUserObject.DisposeOf;

      m1.DisposeOf;
    end;
  end;
end;

constructor TModel.Create;

  function FindAttribute(Attrs: TArray<TCustomAttribute>; AttrClass: TCustomAttributeClass): TCustomAttribute;
  var
    i: integer;
  begin
    result := nil;
    for i := 0 to Length(Attrs) - 1 do
      if Attrs[i] is AttrClass then
        exit(Attrs[i]);
  end;

var
  Methods: TArray<TRttiMethod>;
  Attrs: TArray<TCustomAttribute>;
  i: integer;
  ActionAttr: ActionAttribute;
  PublicAccessAttr: PublicAccessAttribute;
  md: TMethodData;

begin
  FActions := TDictionary<string, TMethodData>.Create;

  Methods := FRTTIContext.GetType(ClassType).GetMethods;
  for i := 0 to Length(Methods) - 1 do
  begin
    Attrs := Methods[i].GetAttributes;

    ActionAttr := ActionAttribute(FindAttribute(Attrs, ActionAttribute));
    PublicAccessAttr := PublicAccessAttribute(FindAttribute(Attrs, PublicAccessAttribute));

    if ActionAttr = nil then
      continue;

    md.RttiMethod := Methods[i];
    md.PublicAccess := PublicAccessAttr <> nil;

    FActions.Add(UpperCase(ActionAttribute(Attrs[0]).FMethod) + '_' +
      LowerCase(ActionAttribute(Attrs[0]).FName), md);
  end;
end;

procedure TModel.DisposeOf;
begin
  FActions.DisposeOf;

  inherited;
end;

initialization

TModel.Register;

end.