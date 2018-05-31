unit SimpleAPI.Controller.Users;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Hash, System.Variants,

  FireDAC.Comp.Client,

  IdCustomHTTPServer,

  SimpleAPI, SimpleAPI.Controller;

type

  [Controller('users')]
  TUsersController = class(TController)
  private
    class function HashPassword(const UserId, PassHashRequest: string): string;
  public
    [Action('getToken'), PublicAccess]
    function GetToken(UserId, Name, PassHashRequest: string): string;

    [Action('create'), PublicAccess]
    function Create(UserId, PassHashRequest: string): string;
  end;

implementation

uses
  SimpleAPI.Utils;

{ TUsersController }

function TUsersController.GetToken(UserId, Name, PassHashRequest: string): string;
var
  PassHash, NewToken: string;
begin
  if SessionObject.FDConnection = nil then
    exit('{"error":"DB_not_connected"}');

  PassHashRequest := LowerCase(PassHashRequest);

  // Validate

  if (not CheckString([UserId, Name], [2, 50])) or (not CheckString([PassHashRequest], [64, 64])) then
    exit('{"error":"illegal_params"}');

  // Check userId and password

  PassHash := HashPassword(UserId, PassHashRequest);

  SessionObject.FDQuery.Open(Format('SELECT userId FROM users WHERE userId = ''%s'' AND passHash = ''%s''',
    [UserId, PassHash]));
  if SessionObject.FDQuery.RecordCount = 0 then
    exit('{"error":"auth_failed"}');

  // Generate new token with these userId and name

  NewToken := THashSHA2.GetHashString(Random(10000000).ToString + THashSHA2.GetHashString(Name) +
    THashSHA2.GetHashString(UserId) + THashSHA2.GetHashString(PassHashRequest));

  SessionObject.FDConnection.ExecSQL(Format(
    'INSERT INTO tokens (token, name, userId)'+
    'VALUES (''%s'', ''%s'', ''%s'')'+
    'ON CONFLICT ON CONSTRAINT unique_userId_name DO '+
    'UPDATE SET token = ''%s''',
    [NewToken, Name, UserId, NewToken]));

  // Answer

  result := Format('{"token":"%s","userId":"%s","name":"%s"}', [NewToken, UserId, Name]);
end;

function TUsersController.Create(UserId, PassHashRequest: string): string;
var
  PassHash: string;
begin
  if SessionObject.FDConnection = nil then
    exit('{"error":"DB_not_connected"}');

  PassHashRequest := LowerCase(PassHashRequest);

  // Validate

  if (not CheckString([UserId], [2, 50])) or (not CheckString([PassHashRequest], [64, 64])) then
    exit('{"error":"illegal_params"}');

  // Check userId already exists

  SessionObject.FDQuery.Open(Format('SELECT userId FROM users WHERE userId = ''%s''', [UserId]));
  if SessionObject.FDQuery.RecordCount > 0 then
    exit('{"error":"userId_exists"}');

  // Create new user record

  PassHash := HashPassword(UserId, PassHashRequest);

  SessionObject.FDConnection.ExecSQL(
    Format('INSERT INTO users (userId, passhash) VALUES (''%s'', ''%s'');', [UserId, PassHash]));

  result := '{"answer":"user_added"}';
end;

class function TUsersController.HashPassword(const UserId, PassHashRequest: string): string;
begin
  result := THashSHA2.GetHashString(UserId + PassHashRequest {SHA256 of real password} + 'bf3jH39k');
end;

initialization

TUsersController.Register;

end.