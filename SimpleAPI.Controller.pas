unit SimpleAPI.Controller;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Classes, System.Rtti, JsonDataObjects,

  FireDAC.Comp.Client,

  IdCustomHTTPServer,

  SimpleAPI;

type

  // Attributes used for controller description

  ControllerAttribute = class(TCustomAttribute)
  public
    FName: string;
    constructor Create(const AName: string);
  end;

  ActionAttribute = class(TCustomAttribute)
  public
    FName: string;
    FMethod: string;
    constructor Create(const AName: string = ''; const AMethod: string = 'GET');
  end;

  DefaultAttribute = class(TCustomAttribute)
  public
    FValue: string;
    constructor Create(AValue: string);
  end;

  PublicAccessAttribute = class(TCustomAttribute);

  // Base TController class

  TActionData = record
    RttiMethod: TRttiMethod;
    PublicAccess: boolean;
  end;

  TControllerClass = class of TController;

  TController = class abstract
  private class var
    FRTTIContext: TRttiContext;
    FControllers: TDictionary<string, TController>;
    FActions: TDictionary<string, TActionData>;

  private
    FParams: TDictionary<string, string>;
    FSessionObject: TSessionObject;
    FAPI: TSimpleAPI;

  public
    class function Execute(AAPI: TSimpleAPI; const AControllerName, AMethodName, AActionName: string; AParams: TDictionary<string, string>;
      ASessionObject: TSessionObject): string;

    property SessionObject: TSessionObject read FSessionObject;
    property Params: TDictionary<string, string> read FParams;
    property API: TSimpleAPI read FAPI;

    constructor Create; virtual;
    procedure DisposeOf; reintroduce; virtual;

    class procedure Init;
    class destructor Destroy;

    class function Register: string;
  end;

implementation

uses
  SimpleAPI.Utils, SimpleAPI.Controller.Users;

{ ControllerAttribute }

constructor ControllerAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ActionAttribute }

constructor ActionAttribute.Create(const AName: string = ''; const AMethod: string = 'GET');
begin
  FName := AName;
  FMethod := AMethod;
end;

{ DefaultAttribute }

constructor DefaultAttribute.Create(AValue: string);
begin
  FValue := AValue;
end;

{ TController }

class procedure TController.Init;
begin
  if FControllers <> nil then
    exit;

  FRTTIContext := TRttiContext.Create;

  FControllers := TDictionary<string, TController>.Create;
  FActions := TDictionary<string, TActionData>.Create;
end;

class destructor TController.Destroy;
var
  m: TController;
begin
  for m in FControllers.Values.ToArray do
    m.DisposeOf;
  FControllers.DisposeOf;

  FActions.DisposeOf;

  FRTTIContext.Free;
end;

class function TController.Register: string;
var
  Attrs: TArray<TCustomAttribute>;
  ControllerName, ActionName: string;
  Methods: TArray<TRttiMethod>;
  i: integer;
  ActionAttr: ActionAttribute;
  PublicAccessAttr: PublicAccessAttribute;
  md: TActionData;
begin
  result := '';

  TController.Init;

  // Add controller

  Attrs := FRTTIContext.GetType(Self).GetAttributes;
  ControllerName := '';
  if not((Length(Attrs) > 0) and (Attrs[0] is ControllerAttribute)) then
    exit;

  ControllerName := LowerCase(ControllerAttribute(Attrs[0]).FName);
  result := ControllerName;

  if FControllers.ContainsKey(ControllerName) then
    exit;

  FControllers.Add(ControllerName, Self.Create);

  // Add controller actions

  Methods := FRTTIContext.GetType(Self).GetMethods;
  for i := 0 to Length(Methods) - 1 do
  begin
    Attrs := Methods[i].GetAttributes;

    ActionAttr := ActionAttribute(FindAttribute(Attrs, ActionAttribute));
    PublicAccessAttr := PublicAccessAttribute(FindAttribute(Attrs, PublicAccessAttribute));

    if ActionAttr = nil then
      continue;

    md.RttiMethod := Methods[i];
    md.PublicAccess := PublicAccessAttr <> nil;

    ActionName := LowerCase(ActionAttribute(Attrs[0]).FName);
    if ActionName = '' then
      ActionName := LowerCase(md.RttiMethod.Name);

    FActions.Add(ControllerName + '_' + UpperCase(ActionAttribute(Attrs[0]).FMethod) + '_' + ActionName, md);
  end;
end;

class function TController.Execute(AAPI: TSimpleAPI; const AControllerName, AMethodName, AActionName: string;
  AParams: TDictionary<string, string>; ASessionObject: TSessionObject): string;
var
  ActionId: string;
  c, c1: TController;
  ad: TActionData;

  Arguments: TArray<TValue>;
  ArgumentValue: TValue;
  p: TRttiParameter;
  ParamValue: string;
  ParamValueExists, ParamNotFoundError: boolean;
  ParamsNotFound: TArray<string>;
  DefaultValueAttr: TCustomAttribute;
  i: integer;
  InvalidParamValue: boolean;
begin
  ActionId := LowerCase(AControllerName) + '_' + UpperCase(AMethodName) + '_' + LowerCase(AActionName);

  if not FControllers.ContainsKey(LowerCase(AControllerName)) then
    exit(Format('{"error":"controller_not_found","controller":"%s"}', [LowerCase(AControllerName)]));

  if not FActions.ContainsKey(ActionId) then
    exit(Format('{"error":"action_not_found","action":"%s"}', [ActionId]));

  c := FControllers[LowerCase(AControllerName)];
  ad := FActions[ActionId];

  c1 := c.NewInstance as TController;
  c1.FParams := AParams;
  c1.FSessionObject := ASessionObject;
  c1.FAPI := AAPI;

  // Collect arguments values

  Arguments := [];
  ParamsNotFound := [];
  ParamNotFoundError := false;
  for p in ad.RttiMethod.GetParameters do
  begin
    ParamValueExists := AParams.ContainsKey(AnsiLowerCase(p.Name));
    if ParamValueExists then
      ParamValue := AParams[AnsiLowerCase(p.Name)];

    DefaultValueAttr := FindAttribute(p.GetAttributes, DefaultAttribute);
    if not ParamValueExists then
    begin
      if DefaultValueAttr <> nil then
      begin
        ArgumentValue := GetValueFromString(DefaultAttribute(DefaultValueAttr).FValue, p.ParamType, InvalidParamValue);
        if not InvalidParamValue then
          Arguments := Arguments + [ArgumentValue]
        else
        begin
          ParamsNotFound := ParamsNotFound + [p.Name];
          ParamNotFoundError := true;
        end;
      end
      else
      begin
        ParamsNotFound := ParamsNotFound + [p.Name];
        ParamNotFoundError := true;
      end;
    end
    else
    begin
      ArgumentValue := GetValueFromString(ParamValue, p.ParamType, InvalidParamValue);
      if not InvalidParamValue then
        Arguments := Arguments + [ArgumentValue]
      else
      begin
        ParamsNotFound := ParamsNotFound + [p.Name];
        ParamNotFoundError := true;
      end;
    end;
  end;

  // Call action method or error output

  if ParamNotFoundError then
    result := Format('{"error":"params_expected","params":"%s"}', [string.Join(',', ParamsNotFound)])

  else if ad.PublicAccess or (c1.FSessionObject.UserId <> '') or (not AAPI.InitializedUsers) then
  begin
    TDBConnectionManager.Use(c1.FSessionObject.FDConnection);

    result := ad.RttiMethod.Invoke(c1, Arguments).AsString;

    TDBConnectionManager.Unuse;
  end

  else
    result := '{"error":"auth_required"}';

  // DisposeOf all arguments-objects

  for ArgumentValue in Arguments do
    if ArgumentValue.Kind = tkClass then
      ArgumentValue.AsObject.DisposeOf;

  c1.DisposeOf;
end;

constructor TController.Create;
begin
end;

procedure TController.DisposeOf;
begin
  inherited;
end;

end.