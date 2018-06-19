unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JsonDataObjects, System.StrUtils,

  FireDAC.Comp.Client,

  SimpleAPI, SimpleAPI.Controller, SimpleAPI.Utils;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    API: TSimpleAPI;
    WebSocketAPI: TSimpleAPI;
  end;

  [Controller('First')]
  TFirstController = class(TController)
  public

    // *** Public available request ***
    // http://localhost:8082/first/helloworld
    [Action, PublicAccess]
    function HelloWorld: string;

    // *** Reverse string from param. Method is public available ***
    // *** Used string and boolean parameters described as method arguments (with default decorator example) ***
    // http://localhost:8082/first/reverse?s=123456
    [Action, PublicAccess]
    function Reverse(s: string; [Default('false')] mirror: boolean): string;

    // *** User available request ***
    // http://localhost:8082/first/greetings/<Token>
    // Where Token you can get from
    // http://localhost:8082/users/getToken?userId=<RegisteredUserId>&passHashRequest=<SHA256 of password>&name=<Token name, i.e. "test">
    // Where RegisteredUserId can be registered at
    // http://localhost:8082/users/create?userId=<UserId>&passHash=<SHA256 of password>
    [Action]
    function Greetings: string;

    // *** Phone number "setter" for current user ***
    // http://localhost:8082/first/SetPhoneNumber/<Token>?phone=123456
    [Action]
    function SetPhoneNumber: string;

    // *** Phone number "getter" for current user ***
    // http://localhost:8082/first/GetPhoneNumber/<Token>?phone=123456
    [Action]
    function GetPhoneNumber: string;

    // *** List phone numbers of all users (available for authorized users only) ***
    // Used custom action name "ListPhoneNumbers" instead default method name
    // http://localhost:8082/first/GetPhoneNumber/<Token>?phone=123456
    [Action('ListPhoneNumbers')]
    function ListPhoneNumbersAction: string;
  end;

  TMySessionObject = class(TSessionObject)
  private
    function GetPhoneNumber: string;
    procedure SetPhoneNumber(const phone: string);
  public
    property PhoneNumber: string read GetPhoneNumber write SetPhoneNumber;
  end;

  [Controller('WSTest')]
  TWSTestController = class(TController)
  public
    // *** Get the time via WebSocket ***
    // This API will not use "users" feature, so all methods will be PublicAccess by default
    // Use "WebSocketTest.html" to test
    [Action]
    function GetTimeAndRandomNumber: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  c: TFDConnection;
begin
  TFirstController.Register;

  // Create HTTP API
  API := TSimpleAPI.Create(TServerMode.HTTP, 8082);

  // Init DB connection
  try
    API.InitDB([
      'Server=127.0.0.1',
      'Database=test',
      'User_Name=postgres',
      'Password=123456',
      'DriverId=PG'
    ]);
  except
    on e: Exception do
      ShowMessage('DB init failed with message: ' + e.Message);
  end;

  // Init default users mechanics with two DB tables (users, tokens) and "users" controller
  API.InitUsers;

  // Register "first" controller
  API.RegisterControllers([TFirstController]);

  // Uncomment to use SSL certificate for your server
  //API.InitSSL('cert\certificate.crt', '', 'cert\private.key', 'cert\dhparam.pem');

  // Set session object class
  API.SessionObjectClass := TMySessionObject;

  // Init additional table to store phone numbers for every user
  if API.InitializedDB then
  begin
    c := TFDConnection.Create(nil);
    c.ConnectionDefName := API.ConnectionDef;
    c.Connected := true;
    c.ExecSQL(
      'CREATE TABLE IF NOT EXISTS phone_numbers ('+
        'userid varchar(50) PRIMARY KEY,'+
        'phone varchar(64)'+
      ');');
    c.DisposeOf;
  end;

  // One more API through WebSocket server
  // It can also use the same controllers, DB, users, tokens and SSL (WSS)
  // But these features were demonstrated with HTTP server
  WebSocketAPI := TSimpleAPI.Create(TServerMode.WebSocket, 8083);
  WebSocketAPI.RegisterControllers([TWSTestController]);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  API.DisposeOf;

  WebSocketAPI.DisposeOf;
end;

{ TFirstController }

function TFirstController.HelloWorld: string;
begin
  result := 'Hello world!';
end;

function TFirstController.Greetings: string;
var
  Answer: TJsonObject;
begin
  Answer := TJsonObject.Create;
  Answer.S['userId'] := SessionObject.UserId;
  Answer.S['message'] := Format('Hello, %s!', [SessionObject.UserId]);

  result := Answer.ToJSON;

  Answer.DisposeOf;
end;

function TFirstController.Reverse(s: string; mirror: boolean): string;
begin
  if mirror then
    result := s + ReverseString(s)
  else
    result := ReverseString(s);
end;

function TFirstController.SetPhoneNumber: string;
begin
  // Validate phone number param
  // You can also get any parameters from "Params" field
  if not CheckString([Params.Values['phone']], [3, 12], ['0'..'9', '+', '-', ' '])  then
    exit('Phone number is incorrect');

  TMySessionObject(SessionObject).PhoneNumber := Params.Values['phone'];

  result := 'Phone number was changed';
end;

function TFirstController.GetPhoneNumber: string;
begin
  result := TMySessionObject(SessionObject).PhoneNumber;
end;

function TFirstController.ListPhoneNumbersAction: string;
var
  q: TFDQuery;
  arr: TJsonArray;
begin
  q := SessionObject.FDQuery;

  arr := TJsonArray.Create;

  q.Open('SELECT * FROM phone_numbers');

  if q.RecordCount > 0 then
  begin
    q.First;
    while not q.Eof do
    begin
      with arr.AddObject do
      begin
        S['userId'] := q.FieldValues['userid'];
        S['phoneNumber'] := q.FieldValues['phone'];
      end;
      q.Next;
    end;
  end;

  result := arr.ToJSON;

  arr.DisposeOf;
end;

{ TMySessionObject }

function TMySessionObject.GetPhoneNumber: string;
var
  q: TFDQuery;
begin
  result := '';

  q := TFDQuery.Create(nil);
  q.Connection := FDConnection;
  q.Open(Format('SELECT phone FROM phone_numbers WHERE userid = ''%s''', [UserId]));
  if q.RecordCount > 0 then
  begin
    q.First;
    result := q.FieldValues['phone'];
  end;
  q.DisposeOf;
end;

procedure TMySessionObject.SetPhoneNumber(const phone: string);
begin
  FDConnection.ExecSQL(Format(
    'INSERT INTO phone_numbers (userid, phone) VALUES (''%s'', ''%s'') '+
    'ON CONFLICT (userid) DO UPDATE SET phone = ''%s''',
    [UserId, phone, phone]));
end;

{ TWSTestController }

function TWSTestController.GetTimeAndRandomNumber: string;
begin
  result := DateTimeToStr(Now) + ' ' + (random(8999) + 1000).ToString;
end;

end.
