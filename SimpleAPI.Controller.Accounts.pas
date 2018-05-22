unit SimpleAPI.Controller.Accounts;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Hash, System.Variants,

  FireDAC.Comp.Client,

  IdCustomHTTPServer,

  SimpleAPI, SimpleAPI.Controller;

type

  [Controller('accounts')]
  TAccountsController = class(TController)
  private
    class function HashPassword(const UserId, PassHashRequest: string): string;

    class function GetUserObjectByUserId(const UserId: string; FDConnection: TFDConnection;
      UserObjectClass: TUserObjectClass): TUserObject;

  public
    [Action('getToken'), PublicAccess]
    procedure GetToken;

    [Action('create'), PublicAccess]
    procedure Create;

    class function GetUserObjectByToken(const Token: string; FDConnection: TFDConnection;
      UserObjectClass: TUserObjectClass): TUserObject;
  end;

implementation

uses
  SimpleAPI.Utils;

{ TAccountsController }

procedure TAccountsController.GetToken;
var
  UserId, Name, PassHashRequest, PassHash, NewToken: string;
begin
  UserId := Input.Params.Values['userId'];
  Name := Input.Params.Values['name'];
  PassHashRequest := Input.Params.Values['passHash'];

  // Validate

  if (not CheckString([UserId, Name], [2, 50])) or (not CheckString([PassHashRequest], [64, 64])) then
  begin
    Output.ContentText := '{"error":"illegal_params"}';
    exit;
  end;

  // Check userId and password

  PassHash := HashPassword(UserId, PassHashRequest);

  FDQuery.Open(Format('SELECT userId FROM users WHERE userId = ''%s'' AND passHash = ''%s''',
    [UserId, PassHash]));
  if FDQuery.RecordCount = 0 then
  begin
    Output.ContentText := '{"error":"auth_failed"}';
    exit;
  end;

  // Generate new token with these userId and name

  NewToken := THashSHA2.GetHashString(Random(10000000).ToString + THashSHA2.GetHashString(Name) +
    THashSHA2.GetHashString(UserId) + THashSHA2.GetHashString(PassHashRequest));

  FDConnection.ExecSQL(Format(
    'INSERT INTO tokens (token, name, userId)'+
    'VALUES (''%s'', ''%s'', ''%s'')'+
    'ON CONFLICT ON CONSTRAINT unique_userId_name DO '+
    'UPDATE SET token = ''%s''',
    [NewToken, Name, UserId, NewToken]));

  // Answer

  Output.ContentText := Format('{"token":"%s","userId":"%s","name":"%s"}', [NewToken, UserId, Name]);
end;

procedure TAccountsController.Create;
var
  UserId, PassHashRequest, PassHash: string;
begin
  UserId := Input.Params.Values['userId'];
  PassHashRequest := Input.Params.Values['passHash'];

  // Validate

  if (not CheckString([UserId], [2, 50])) or (not CheckString([PassHashRequest], [64, 64])) then
  begin
    Output.ContentText := '{"error":"illegal_params"}';
    exit;
  end;

  // Check userId already exists

  FDQuery.Open(Format('SELECT userId FROM users WHERE userId = ''%s''', [UserId]));
  if FDQuery.RecordCount > 0 then
  begin
    Output.ContentText := '{"error":"userId_exists"}';
    exit;
  end;

  // Create new user record

  PassHash := HashPassword(UserId, PassHashRequest);

  FDConnection.ExecSQL(
    Format('INSERT INTO users (userId, passhash) VALUES (''%s'', ''%s'');', [UserId, PassHash]));

  Output.ContentText := '{"answer":"user_added"}';
end;

class function TAccountsController.HashPassword(const UserId, PassHashRequest: string): string;
begin
  result := THashSHA2.GetHashString(UserId + PassHashRequest {SHA256 of real password} + 'bf3jH39k');
end;

class function TAccountsController.GetUserObjectByUserId(const UserId: string; FDConnection: TFDConnection;
  UserObjectClass: TUserObjectClass): TUserObject;
begin
  Result := UserObjectClass.Create(UserId, FDConnection);
end;

class function TAccountsController.GetUserObjectByToken(const Token: string; FDConnection: TFDConnection;
  UserObjectClass: TUserObjectClass): TUserObject;
var
  q: TFDQuery;
begin
  result := nil;

  q := TFDQuery.Create(nil);
  q.Connection := FDConnection;

  q.Open(Format('SELECT * FROM tokens WHERE token = ''%s''', [Token]));

  if q.RecordCount > 0 then
  begin
    q.First;
    result := GetUserObjectByUserId(q.FieldValues['userid'], FDConnection, UserObjectClass);
  end;

  q.DisposeOf;
end;

initialization

TAccountsController.Register;

end.