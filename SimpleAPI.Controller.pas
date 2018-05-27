unit SimpleAPI.Controller;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Rtti,

  FireDAC.Comp.Client,

  IdCustomHTTPServer,

  SimpleAPI;

type

  // Attributes used for controller description

  TCustomAttributeClass = class of TCustomAttribute;

  ControllerAttribute = class(TCustomAttribute)
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

  // Base TController class

  TMethodData = record
    RttiMethod: TRttiMethod;
    PublicAccess: boolean;
  end;

  TControllerClass = class of TController;

  TController = class abstract
  private class var
    FRTTIContext: TRttiContext;
    FControllers: TDictionary<string, TController>;

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
  SimpleAPI.Utils, SimpleAPI.Controller.Accounts;

{ ControllerAttribute }

constructor ControllerAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ActionAttribute }

constructor ActionAttribute.Create(const AName: string; const AMethod: string = 'GET');
begin
  FName := AName;
  FMethod := AMethod;
end;

{ TController }

class procedure TController.Init;
begin
  if FControllers <> nil then
    exit;

  FRTTIContext := TRttiContext.Create;
  FControllers := TDictionary<string, TController>.Create;
end;

class destructor TController.Destroy;
var
  m: TController;
begin
  for m in FControllers.Values.ToArray do
    m.DisposeOf;
  FControllers.DisposeOf;

  FRTTIContext.Free;
end;

class procedure TController.Register;
var
  Attrs: TArray<TCustomAttribute>;
begin
  TController.Init;

  Attrs := FRTTIContext.GetType(Self).GetAttributes;
  if (Length(Attrs) > 0) and (Attrs[0] is ControllerAttribute) then
    FControllers.Add(LowerCase(ControllerAttribute(Attrs[0]).FName), Self.Create);
end;

class procedure TController.Execute(Input: TIdHTTPRequestInfo; Output: TIdHTTPResponseInfo;
  FDConnection: TFDConnection; FDQuery: TFDQuery; UserObjectClass: TUserObjectClass);
var
  Uri: string;
  SplittedUri: TArray<string>;
  Method, Controller, Action: string;
  m, m1: TController;
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
  Controller := LowerCase(SplittedUri[1]);
  Action := LowerCase(SplittedUri[2]);

  if FControllers.ContainsKey(LowerCase(Controller)) then
  begin
    m := FControllers[Controller];

    ActionId := Method + '_' + Action;
    if m.FActions.ContainsKey(ActionId) then
    begin
      md := m.FActions[ActionId];

      m1 := m.NewInstance as TController;

      m1.FFDConnection := FDConnection;
      m1.FFDQuery := FDQuery;
      m1.FInput := Input;
      m1.FOutput := Output;
      m1.FURI := SplittedUri;
      m1.FUserObject := nil;

      if (not md.PublicAccess) and (Length(SplittedUri) > 3) and CheckString([SplittedUri[3]], [64, 64]) then
        m1.FUserObject := TAccountsController.GetUserObjectByToken(LowerCase(SplittedUri[3]), FDConnection, UserObjectClass);

      if md.PublicAccess or (m1.FUserObject <> nil) then
        md.RttiMethod.Invoke(m1, [])
      else
        Output.ContentText := '{"error":"auth_failed"}';

      m1.FUserObject.DisposeOf;

      m1.DisposeOf;
    end;
  end;
end;

constructor TController.Create;

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

procedure TController.DisposeOf;
begin
  FActions.DisposeOf;

  inherited;
end;

initialization

TController.Register;

end.