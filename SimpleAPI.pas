unit SimpleAPI;

interface

uses

  System.Classes, System.SysUtils, System.Generics.Collections, JsonDataObjects,

  // FireDAC

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.DApt, FireDAC.VCLUI.Wait,

  // Indy

  IdHTTPServer, IdContext, IdCustomHTTPServer, IdSSL, IdSSLOpenSSL,

  SimpleAPI.WebSocketServer;

type

  TSimpleAPI = class;

  TSessionObjectClass = class of TSessionObject;

  TSessionObject = class abstract
  private
    FAPI: TSimpleAPI;
    FFDConnection: TFDConnection;
    FFDQuery: TFDQuery;
    FUserId: string;
  public
    property UserId: string read FUserId;
    property FDConnection: TFDConnection read FFDConnection;
    property FDQuery: TFDQuery read FFDQuery;

    procedure Auth(const Token: string); virtual;

    constructor Create(AAPI: TSimpleAPI); virtual;
    procedure DisposeOf; reintroduce; virtual;
  end;

  TServerMode = (HTTP, WebSocket);

  TSimpleAPI = class
  private class var
    FDBManager: TFDManager;
  
  private
    FServerMode: TServerMode;
    FConnectionUniqueDef: string;

    FIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    FHTTPServer: TIdHTTPServer;
    FWSServer: TWebSocketServer;
    FSSLOnGetPassword: TFunc<string>;

    FSessionObjectClass: TSessionObjectClass;

    FRegisteredControllers: TList<string>;

    FUsersInitialized, FDBInitialized: boolean;

    procedure SSLOnGetPassword(var Password: String);

    procedure HTTPConnect(AContext: TIdContext);
    procedure HTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure WSConnect(AContext: TIdContext);
    procedure WSExecute(AContext: TIdContext);
    procedure WSDisconnect(AContext: TIdContext);

    function GetPort: Word;
    procedure SetPort(const Value: Word);
  public
    property SessionObjectClass: TSessionObjectClass read FSessionObjectClass write FSessionObjectClass;

    property ServerMode: TServerMode read FServerMode;
    property Port: Word read GetPort write SetPort;

    function InitializedDB: boolean;
    function InitializedUsers: boolean;
    function InitializedSSL: boolean;

    procedure InitDB(DBConnectionStrings: TStringList); overload;
    procedure InitDB(DBConnectionStrings: TArray<string>); overload;
    procedure InitUsers;
    procedure InitSSL(CertFile, RootCertFile, KeyFile, DHParamsFile: string;
      Version: TIdSSLVersion = TIdSSLVersion.sslvTLSv1_2; OnGetPassword: TFunc<string> = nil);

    procedure RegisterControllers(ControllersArr: TArray<TClass>);

    property ConnectionDef: string read FConnectionUniqueDef;

    constructor Create(AMode: TServerMode; APort: Word); overload;
    procedure DisposeOf; reintroduce; virtual;

    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  SimpleAPI.Utils, SimpleAPI.Controller, SimpleAPI.Controller.Users;

type

  TIdHTTPRequestInfo2 = class(TIdHTTPRequestInfo)
  public
    procedure DecodeAndSetParams(const AValue: string); override;
  end;

{ TSimpleAPI }

class constructor TSimpleAPI.Create;
begin
  FDBManager := TFDManager.Create(nil);
end;

class destructor TSimpleAPI.Destroy;
begin
  FDBManager.Close;
  FDBManager.DisposeOf;
end;

constructor TSimpleAPI.Create(AMode: TServerMode; APort: Word);
begin
  FServerMode := AMode;
  FSessionObjectClass := TSessionObject;
  FRegisteredControllers := TList<string>.Create;
  FUsersInitialized := false;
  FDBInitialized := false;
  FConnectionUniqueDef := GenerateUniqueId;

  // Init server

  FHTTPServer := nil;
  FWSServer := nil;
  FIdServerIOHandlerSSLOpenSSL := nil;

  case FServerMode of
    HTTP:
      begin
        FHTTPServer := TIdHTTPServer.Create;
        FHTTPServer.DefaultPort := APort;
        FHTTPServer.OnCommandGet := HTTPCommandGet;
        FHTTPServer.OnConnect := HTTPConnect;
        FHTTPServer.Active := true;
      end;

    WebSocket:
      begin
        FWSServer := TWebSocketServer.Create;
        FWSServer.Bindings.DefaultPort := APort;
        FWSServer.OnExecute := WSExecute;
        FWSServer.OnConnect := WSConnect;
        FWSServer.OnDisconnect := WSDisconnect;
        FWSServer.Active := true;
      end;
  end;
end;

procedure TSimpleAPI.DisposeOf;
begin
  case FServerMode of
    HTTP:
      begin
        FHTTPServer.Active := false;
        FHTTPServer.DisposeOf;
      end;
    WebSocket:
      begin
        FWSServer.Active := false;
        FWSServer.DisposeOf;
      end;
  end;

  if FIdServerIOHandlerSSLOpenSSL <> nil then
    FIdServerIOHandlerSSLOpenSSL.DisposeOf;

  FRegisteredControllers.DisposeOf;

  inherited;
end;

procedure TSimpleAPI.InitDB(DBConnectionStrings: TStringList);
var
  c: TFDConnection;
begin
  try
    // Force 'Pooled=true' string

    DBConnectionStrings.Add('Pooled=true');
  
    // Init DB manager
    
    FDBManager.AddConnectionDef(FConnectionUniqueDef, DBConnectionStrings.Values['DriverId'], DBConnectionStrings);

    // Check DB connection

    c := TFDConnection.Create(nil);
    c.ConnectionDefName := FConnectionUniqueDef;
    c.Connected := true;

    if not c.Connected then
      raise Exception.Create('DB connection failed');

    c.DisposeOf;

    FDBInitialized := true;
  except
    on e: Exception do
    begin
      if c <> nil then
        c.DisposeOf;

      raise Exception.Create(e.Message + #13#10 + 'API works in NoDB mode');
    end;
  end;
end;

procedure TSimpleAPI.InitDB(DBConnectionStrings: TArray<string>);
var
  s: TStringList;
begin
  try
    s := TStringList.Create;
    s.AddStrings(DBConnectionStrings);
    InitDB(s);
    s.DisposeOf;
  except
    on e: Exception do
    begin
      s.DisposeOf;

      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TSimpleAPI.InitUsers;
var
  c: TFDConnection;
begin
  if not InitializedDB then
    raise Exception.Create('DB must be initialized first');

  // Init base structure for API (tokens, users)

  try
    c := TFDConnection.Create(nil);
    c.ConnectionDefName := ConnectionDef;
    c.Connected := true;

    if not c.Connected then
      raise Exception.Create('DB connection failed');

    c.ExecSQL(
      'CREATE TABLE IF NOT EXISTS tokens ('+
        'token varchar(64) PRIMARY KEY,'+
        'name varchar(50),'+
        'userId varchar(50),'+
        'CONSTRAINT unique_userId_name UNIQUE (userId, name)'+
      ');');

    c.ExecSQL(
      'CREATE TABLE IF NOT EXISTS users ('+
        'userid varchar(50) PRIMARY KEY,'+
        'passhash varchar(64)'+
      ');');

    c.DisposeOf;
  except
    on e: Exception do
    begin
      if c <> nil then
        c.DisposeOf;

      raise Exception.Create(e.Message + #13#10 + 'API works in NoDB mode');
    end;
  end;

  // Register users controller

  RegisterControllers([TUsersController]);

  FUsersInitialized := true;
end;

procedure TSimpleAPI.InitSSL(CertFile, RootCertFile, KeyFile, DHParamsFile: string;
  Version: TIdSSLVersion; OnGetPassword: TFunc<string>);
begin
  if FIdServerIOHandlerSSLOpenSSL <> nil then
    raise Exception.Create('SSL was already initialized');

  FIdServerIOHandlerSSLOpenSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.Mode := TIdSSLMode.sslmServer;
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.KeyFile := KeyFile;
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.CertFile := CertFile;
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.DHParamsFile := DHParamsFile;
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.Method := Version;
  FIdServerIOHandlerSSLOpenSSL.SSLOptions.RootCertFile := RootCertFile;
  FIdServerIOHandlerSSLOpenSSL.OnGetPassword := SSLOnGetPassword;

  case FServerMode of
    HTTP:
      begin
        FHTTPServer.Active := false;
        FHTTPServer.IOHandler := FIdServerIOHandlerSSLOpenSSL;
        FHTTPServer.Active := true;
      end;
    WebSocket:
      begin
        FWSServer.InitSSL(FIdServerIOHandlerSSLOpenSSL);
      end;
  end;
end;

procedure TSimpleAPI.SSLOnGetPassword(var Password: String);
begin
  Password := '';
  if Assigned(FSSLOnGetPassword) then
    Password := FSSLOnGetPassword;
end;

function TSimpleAPI.InitializedDB: boolean;
begin
  result := FDBInitialized;
end;

function TSimpleAPI.InitializedSSL: boolean;
begin
  result := FIdServerIOHandlerSSLOpenSSL <> nil;
end;

function TSimpleAPI.InitializedUsers: boolean;
begin
  result := FUsersInitialized;
end;

function TSimpleAPI.GetPort: Word;
begin
  case FServerMode of
    HTTP: Result := FHTTPServer.Bindings.DefaultPort;
    WebSocket: Result := FWSServer.Bindings.DefaultPort;
  end;
end;

procedure TSimpleAPI.SetPort(const Value: Word);
begin
  case FServerMode of
    HTTP: FHTTPServer.Bindings.DefaultPort := Value;
    WebSocket: FWSServer.Bindings.DefaultPort := Value;
  end;
end;

procedure TSimpleAPI.RegisterControllers(ControllersArr: TArray<TClass>);
var
  c: TClass;
  ControllerName: string;
begin
  for c in ControllersArr do
    if c.InheritsFrom(TController) then
    begin
      ControllerName := TControllerClass(c).Register;

      if ControllerName = '' then
        Continue;

      if not FRegisteredControllers.Contains(ControllerName) then
        FRegisteredControllers.Add(ControllerName);
    end
    else
      raise Exception.Create(Format('"%" is not a TController class', [c.ClassName]));
end;

procedure TSimpleAPI.HTTPConnect(AContext: TIdContext);
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;
end;

procedure TSimpleAPI.HTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  SessionObject: TSessionObject;
  json: TJsonObject;
  Uri, Method, Controller, Action, Token: string;
  SplittedUri: TArray<string>;
  AuthFailed: boolean;
  ParamsDict: TDictionary<string,string>;
begin
  // UTF-8 UrlEncoded parameters fix

  ARequestInfo.Charset := 'utf-8';
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');
  TIdHTTPRequestInfo2(ARequestInfo).DecodeAndSetParams(ARequestInfo.UnparsedParams);

  // Parse URI

  Uri := ARequestInfo.URI;
  if Uri.EndsWith('/') then
    Uri := Copy(Uri, 1, Length(Uri) - 1);
  SplittedUri := Uri.Split(['/']);
  if Length(SplittedUri) < 3 then
  begin
    AResponseInfo.ContentText := '{"error":"invalid_uri"}';
    exit;
  end;
  Method := UpperCase(ARequestInfo.Command);
  Controller := LowerCase(SplittedUri[1]);
  Action := LowerCase(SplittedUri[2]);
  Token := '';
  if Length(SplittedUri) >= 4 then
  begin
    Token := SplittedUri[3];
    if not CheckString([Token], [64, 64]) then
    begin
      AResponseInfo.ContentText := '{"error":"invalid_token"}';
      exit;
    end;
  end;

  // Handle command

  try
    // Create session object with DB connection
    SessionObject := FSessionObjectClass.Create(Self);

    // Start DB transaction
    if SessionObject.FDConnection <> nil then
      SessionObject.FDConnection.StartTransaction;

    // Try to authorize session using token
    AuthFailed := false;
    if (Token <> '') and InitializedUsers then
    begin
      SessionObject.Auth(Token);
      if SessionObject.UserId = '' then
      begin
        AResponseInfo.ContentText := '{"error":"auth_failed"}';
        AuthFailed := true;
      end;
    end;

    // Execute command
    if not AuthFailed then
      if FRegisteredControllers.Contains(Controller) then
      begin
        ParamsDict := StringParamsToDictionary(ARequestInfo.Params);
        AResponseInfo.ContentText := TController.Execute(Self, Controller, Method, Action, ParamsDict, SessionObject);
        ParamsDict.DisposeOf;
      end
      else
        AResponseInfo.ContentText := Format('{"error":"controller_not_found","controller":"%s"}', [Controller]);

    // Commit DB transaction
    if SessionObject.FDConnection <> nil then
      SessionObject.FDConnection.Commit;

  except
    on e: Exception do
    begin
      // Rollback DB transaction
      if SessionObject.FDConnection <> nil then
        SessionObject.FDConnection.Rollback;

      json := TJsonObject.Create;
      json.S['error'] := 'server_error';
      json.S['message'] := e.Message;
      AResponseInfo.ContentText := json.ToJSON;
      json.DisposeOf;
    end;
  end;

  try
    // Destroy session object
    SessionObject.DisposeOf;
  except
    on e: Exception do
  end;
end;

procedure TSimpleAPI.WSConnect(AContext: TIdContext);
var
  SessionObject: TSessionObject;
begin
  SessionObject := FSessionObjectClass.Create(Self);

  AContext.Data := SessionObject;
end;

procedure TSimpleAPI.WSExecute(AContext: TIdContext);
var
  Msg, Response, Controller, Action, Token, Method: string;
  Params: TDictionary<string, string>;
  c: TWebSocketIOHandlerHelper;
  SessionObject: TSessionObject;
  Request, json: TJsonObject;
  JsonParamValue: TJsonDataValueHelper;
  JsonParamValueStr: string;
  i: integer;
  rid: string;
begin
  c := TWebSocketIOHandlerHelper(AContext.Connection.IOHandler);
  c.CheckForDataOnSource(10);

  SessionObject := TSessionObject(AContext.Data);

  Msg := c.ReadString;
  if Msg = '' then
    exit;
  Request := nil;
  Params := nil;

  try
    Request := TJsonObject(TJsonObject.Parse(Msg));

    // Parse controller, action, rid, request params

    Token := Request.S['_token'];
    if Token = '' then
    begin
      Controller := LowerCase(Request.S['_act'].Split(['/'])[0]);
      Action := LowerCase(Request.S['_act'].Split(['/'])[1]);
    end;
    Method := 'GET';
    if Request.Contains('_method') then
      Method := Request['_method'];
    rid := '';
    if Request.Contains('_rid') then
      rid := Request['_rid'];

    Params := TDictionary<string, string>.Create;
    for i := 0 to Request.Count - 1 do
      if (Request.Names[i] <> '_act') and (Request.Names[i] <> '_method') and (Request.Names[i] <> '_token') and
        (Request.Names[i] <> '_rid') then
        begin
          JsonParamValue := Request.Values[Request.Names[i]];
          case JsonParamValue.Typ of
            jdtArray: JsonParamValueStr := JsonParamValue.ArrayValue.ToJSON;
            jdtObject: JsonParamValueStr := JsonParamValue.ObjectValue.ToJSON;
            else
              JsonParamValueStr := JsonParamValue.Value;
          end;
          Params.AddOrSetValue(AnsiLowerCase(Request.Names[i]), JsonParamValueStr);
        end;

    // Start DB transaction
    if SessionObject.FDConnection <> nil then
      SessionObject.FDConnection.StartTransaction;

    // Try to authorize session using special action with "token" field
    if (Token <> '') and InitializedUsers then
    begin
      SessionObject.Auth(Token);
      if SessionObject.UserId = '' then
        Response := '{"error":"auth_failed"}'
      else
        Response := Format('{"error":"auth_ok","userId":"%s"}', [SessionObject.UserId]);
    end

    // Execute command
    else if FRegisteredControllers.Contains(Controller) then
      Response := TController.Execute(Self, Controller, Method, Action, Params, SessionObject)

    // Controller not found error
    else
      Response := Format('{"error":"controller_not_found","controller":"%s"}', [Controller]);

    // Commit DB transaction
    if SessionObject.FDConnection <> nil then
      SessionObject.FDConnection.Commit;

  except
    on e: Exception do
    begin
      // Rollback DB transaction
      if SessionObject.FDConnection <> nil then
        SessionObject.FDConnection.Rollback;

      json := TJsonObject.Create;
      json.S['error'] := 'server_error';
      json.S['message'] := e.Message;
      Response := json.ToJSON;
      json.DisposeOf;
    end;
  end;

  if Request <> nil then
    Request.DisposeOf;

  if Params <> nil then
    Params.DisposeOf;

  // Try to add "rid" field to JsonObject response
  if rid <> '' then
  begin
    json := CheckJsonObject(Response);
    if json <> nil then
    begin
      json.S['_rid'] := rid;
      Response := json.ToJSON;
      json.DisposeOf;
    end;
  end;

  c.WriteString(Response);
end;

procedure TSimpleAPI.WSDisconnect(AContext: TIdContext);
begin
  TSessionObject(AContext.Data).DisposeOf;
  AContext.Data := nil;
end;

{ TSessionObject }

constructor TSessionObject.Create(AAPI: TSimpleAPI);
begin
  FAPI := AAPI;
  FUserId := '';

  if not FAPI.InitializedDB then
  begin
    // No DB mode

    FFDConnection := nil;
    FFDQuery := nil;
  end
  else
  begin
    // Create FDConnection and FDQuery for request

    FFDConnection := TFDConnection.Create(nil);
    FFDConnection.ConnectionDefName := FAPI.ConnectionDef;
    FFDConnection.Connected := true;

    FFDQuery := TFDQuery.Create(nil);
    FFDQuery.Connection := FFDConnection;
  end;
end;

procedure TSessionObject.DisposeOf;
begin
  // Free FDConnection and FDQuery

  if FAPI.InitializedDB then
  begin
    FFDQuery.DisposeOf;
    FFDConnection.Connected := false;
    FFDConnection.DisposeOf;
  end;

  inherited;
end;

procedure TSessionObject.Auth(const Token: string);
var
  q: TFDQuery;
begin
  q := TFDQuery.Create(nil);
  q.Connection := FDConnection;

  q.Open(Format('SELECT * FROM tokens WHERE token = ''%s''', [Token]));

  if q.RecordCount > 0 then
  begin
    q.First;
    FUserId := q.FieldValues['userid'];
  end
  else
    FUserId := '';

  q.DisposeOf;
end;

{ TIdHTTPRequestInfo2 }

procedure TIdHTTPRequestInfo2.DecodeAndSetParams(const AValue: string);
begin
  inherited;
end;

end.