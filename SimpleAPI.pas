unit SimpleAPI;

interface

uses

  System.Classes, System.SysUtils, JsonDataObjects,

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
  private
    FServerMode: TServerMode;

    FIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    FHTTPServer: TIdHTTPServer;
    FWSServer: TWebSocketServer;

    FFDManager: TFDManager;
    FSessionObjectClass: TSessionObjectClass;

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
    function InitializedSSL: boolean;

    procedure InitDB(DBConnectionStrings: TStringList); overload;
    procedure InitDB(DBConnectionStrings: TArray<string>); overload;
    procedure InitSSL(CertFile, RootCertFile, KeyFile, DHParamsFile: string; Version: TIdSSLVersion = TIdSSLVersion.sslvTLSv1_2);

    constructor Create(AMode: TServerMode; APort: Word); overload;
    procedure DisposeOf; reintroduce; virtual;
  end;

implementation

uses
  SimpleAPI.Utils, SimpleAPI.Controller;

type

  TIdHTTPRequestInfo2 = class(TIdHTTPRequestInfo)
  public
    procedure DecodeAndSetParams(const AValue: string); override;
  end;

{ TSimpleAPI }

constructor TSimpleAPI.Create(AMode: TServerMode; APort: Word);
begin
  FServerMode := AMode;
  FSessionObjectClass := TSessionObject;
  FFDManager := nil;

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
  if FIdServerIOHandlerSSLOpenSSL <> nil then
    FIdServerIOHandlerSSLOpenSSL.DisposeOf;

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

  if FFDManager <> nil then
  begin
    FFDManager.Close;
    FFDManager.DisposeOf;
  end;

  inherited;
end;

procedure TSimpleAPI.InitDB(DBConnectionStrings: TStringList);
var
  c: TFDConnection;
begin
  try
    // Init base

    FFDManager := TFDManager.Create(nil);
    FFDManager.AddConnectionDef('default', DBConnectionStrings.Values['DriverId'], DBConnectionStrings);

    // Init base structure for API (tokens, users)

    c := TFDConnection.Create(nil);
    c.ConnectionDefName := 'default';
    c.Connected := true;

    if not c.Connected then
      raise Exception.Create('Database connection failed');

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
      if FFDManager <> nil then
        FFDManager.DisposeOf;
      FFDManager := nil;

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

procedure TSimpleAPI.InitSSL(CertFile, RootCertFile, KeyFile, DHParamsFile: string;
  Version: TIdSSLVersion);
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

function TSimpleAPI.InitializedDB: boolean;
begin
  result := FFDManager <> nil;
end;

function TSimpleAPI.InitializedSSL: boolean;
begin
  result := FIdServerIOHandlerSSLOpenSSL <> nil;
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
    if FFDManager <> nil then
      SessionObject.FDConnection.StartTransaction;

    // Try to authorize session using token
    AuthFailed := false;
    if Token <> '' then
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
      AResponseInfo.ContentText := TController.ExecuteNew(Controller, Method, Action, ARequestInfo.Params,
        SessionObject);

    // Commit DB transaction
    if FFDManager <> nil then
      SessionObject.FDConnection.Commit;

  except
    on e: Exception do
    begin
      // Rollback DB transaction
      if FDManager <> nil then
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
  Params: TStringList;
  c: TWebSocketIOHandlerHelper;
  SessionObject: TSessionObject;
  Request, json: TJsonObject;
  i: integer;
begin
  c := TWebSocketIOHandlerHelper(AContext.Connection.IOHandler);
  c.CheckForDataOnSource(10);

  SessionObject := TSessionObject(AContext.Data);

  Msg := c.ReadString;
  if Msg = '' then
    exit;
  Request := nil;

  try
    Request := TJsonObject(TJsonObject.Parse(Msg));

    // Parse controller, action, request params
    Token := Request.S['token'];
    if Token <> '' then
    begin
      Controller := Request.S['act'].Split(['/'])[0];
      Action := Request.S['act'].Split(['/'])[1];
    end;
    Method := 'GET';
    if Request.Contains('method') then
      Method := Request['method'];
    Params := TStringList.Create;
    for i := 0 to Request.Count - 1 do
      if (Request.Names[i] <> 'act') and (Request.Names[i] <> 'method') and (Request.Names[i] <> 'token') then
        Params.Add(Request.Names[i] + '=' + Request.Values[Request.Names[i]].Value);

    // Start DB transaction
    if FFDManager <> nil then
      SessionObject.FDConnection.StartTransaction;

    if Token <> '' then
    begin
      // Try to authorize session using special action with token
      SessionObject.Auth(Token);
      if SessionObject.UserId = '' then
        Response := '{"error":"auth_failed"}';
    end
    else
      // Execute command
      Response := TController.ExecuteNew(Controller, Method, Action, Params, SessionObject);

    // Commit DB transaction
    if FFDManager <> nil then
      SessionObject.FDConnection.Commit;

  except
    on e: Exception do
    begin
      // Rollback DB transaction
      if FDManager <> nil then
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

  c.WriteString(Response);
end;

procedure TSimpleAPI.WSDisconnect(AContext: TIdContext);
begin
  TSessionObject(AContext.Data).DisposeOf;
end;

{ TSessionObject }

constructor TSessionObject.Create(AAPI: TSimpleAPI);
begin
  FAPI := AAPI;
  FUserId := '';

  if FAPI.FFDManager = nil then
  begin
    // No DB mode

    FFDConnection := nil;
    FFDQuery := nil;
  end
  else
  begin
    // Create FDConnection and FDQuery for request

    FFDConnection := TFDConnection.Create(nil);
    FFDConnection.ConnectionDefName := 'default';
    FFDConnection.Connected := true;

    FFDQuery := TFDQuery.Create(nil);
    FFDQuery.Connection := FFDConnection;
  end;
end;

procedure TSessionObject.DisposeOf;
begin
  // Free FDConnection and FDQuery

  if FAPI.FFDManager <> nil then
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