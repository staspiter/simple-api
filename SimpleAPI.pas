unit SimpleAPI;

interface

uses

  System.Classes, System.SysUtils, JsonDataObjects,

  // FireDAC

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.DApt, FireDAC.VCLUI.Wait,

  // Indy

  IdHTTPServer, IdContext, IdCustomHTTPServer;

type

  TUserObjectClass = class of TUserObject;

  TUserObject = class abstract
  private
    FFDConnection: TFDConnection;
    FUserId: string;
  public
    property UserId: string read FUserId;
    property FDConnection: TFDConnection read FFDConnection;

    constructor Create(const AUserId: string; AFDConnection: TFDConnection); virtual;
  end;

  TSimpleAPI = class
  private
    FHTTPServer: TIdHTTPServer;
    FFDManager: TFDManager;
    FUserObjectClass: TUserObjectClass;

    procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    function GetPort: Word;
    procedure SetPort(const Value: Word);
  public
    property UserObjectClass: TUserObjectClass read FUserObjectClass write FUserObjectClass;
    property Port: Word read GetPort write SetPort;

    constructor Create(APort: Word; DBConnectionStrings: TStringList);
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

constructor TSimpleAPI.Create(APort: Word; DBConnectionStrings: TStringList);
var
  c: TFDConnection;
begin
  FUserObjectClass := TUserObject;

  // Init base

  FFDManager := TFDManager.Create(nil);
  FFDManager.AddConnectionDef('default', DBConnectionStrings.Values['DriverId'],
    DBConnectionStrings);

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

  // Init HTTP server

  FHTTPServer := TIdHTTPServer.Create;
  FHTTPServer.DefaultPort := APort;
  FHTTPServer.OnCommandGet := CommandGet;
  FHTTPServer.Active := true;
end;

procedure TSimpleAPI.DisposeOf;
begin
  FHTTPServer.Active := false;
  FHTTPServer.DisposeOf;

  FFDManager.Close;
  FFDManager.DisposeOf;

  inherited;
end;

function TSimpleAPI.GetPort: Word;
begin
  Result := FHTTPServer.DefaultPort;
end;

procedure TSimpleAPI.SetPort(const Value: Word);
begin
  FHTTPServer.DefaultPort := Value;
end;

procedure TSimpleAPI.CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  json: TJsonObject;
begin
  // UTF-8 UrlEncoded parameters fix

  ARequestInfo.Charset := 'utf-8';
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  TIdHTTPRequestInfo2(ARequestInfo).DecodeAndSetParams(ARequestInfo.UnparsedParams);

  // Create FDConnection and FDQuery for request

  FDConnection := TFDConnection.Create(nil);
  FDConnection.ConnectionDefName := 'default';
  FDConnection.Connected := true;

  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FDConnection;

  // Execute request

  FDConnection.StartTransaction;

  try
    TController.Execute(ARequestInfo, AResponseInfo, FDConnection, FDQuery, FUserObjectClass);

    FDConnection.Commit;
  except
    on e: Exception do
    begin
      FDConnection.Rollback;

      json := TJsonObject.Create;
      json.S['error'] := 'server_error';
      json.S['message'] := e.Message;
      AResponseInfo.ContentText := json.ToJSON;
      json.DisposeOf;
    end;
  end;

  // Free FDConnection and FDQuery

  FDQuery.DisposeOf;
  FDConnection.Connected := false;
  FDConnection.DisposeOf;
end;

{ TIdHTTPRequestInfo2 }

procedure TIdHTTPRequestInfo2.DecodeAndSetParams(const AValue: string);
begin
  inherited;
end;

{ TUserObject }

constructor TUserObject.Create(const AUserId: string; AFDConnection: TFDConnection);
begin
  FUserId := AUserId;
  FFDConnection := AFDConnection;
end;

end.