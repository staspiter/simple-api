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
  end;

  [Controller('FirstController')]
  TFirstController = class(TController)
  public

    // *** Public available request ***
    // http://localhost:8080/firstcontroller/helloworld
    [Action('HelloWorld'), PublicAccess]
    procedure HelloWorld;

    // *** User available request ***
    // http://localhost:8080/firstcontroller/greetings/<Token>
    // Where Token you can get from
    // http://localhost:8080/accounts/getToken?userId=<RegisteredUserId>&passHash=<SHA256 of password>&name=<Token name, i.e. "test">
    // Where RegisteredUserId can be registered at
    // http://localhost:8080/accounts/create?userId=<UserId>&passHash=<SHA256 of password>
    [Action('Greetings')]
    procedure Greetings;

    // *** Reverse string from param. Method is available for authorized user ***
    // http://localhost:8080/firstcontroller/reverse/<Token>?s=123456
    [Action('Reverse')]
    procedure Reverse;

    // *** Phone number "setter" for current user ***
    // http://localhost:8080/firstcontroller/SetPhoneNumber/<Token>?phone=123456
    [Action('SetPhoneNumber')]
    procedure SetPhoneNumber;

    // *** Phone number "getter" for current user ***
    // http://localhost:8080/firstcontroller/GetPhoneNumber/<Token>?phone=123456
    [Action('GetPhoneNumber')]
    procedure GetPhoneNumber;

    // *** List phone numbers of all users (available for authorized users only) ***
    // http://localhost:8080/firstcontroller/GetPhoneNumber/<Token>?phone=123456
    [Action('ListPhoneNumbers')]
    procedure ListPhoneNumbers;
  end;

  TMyUserObject = class(TUserObject)
  private
    FPhoneNumber: string;

    procedure SetPhoneNumber(const phone: string);
  public
    property PhoneNumber: string read FPhoneNumber write SetPhoneNumber;

    constructor Create(const AUserId: string; AFDConnection: TFDConnection); override;
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

  API := TSimpleAPI.Create(8080, [
      'Server=127.0.0.1',
      'Database=test',
      'User_Name=postgres',
      'Password=123456',
      'DriverId=PG',
      'Pooled=true'
    ]);

  API.UserObjectClass := TMyUserObject; // Set user object class

  // Init additional table

  c := TFDConnection.Create(nil);
  c.ConnectionDefName := 'default';
  c.Connected := true;
  c.ExecSQL(
    'CREATE TABLE IF NOT EXISTS phone_numbers ('+
      'userid varchar(50) PRIMARY KEY,'+
      'phone varchar(64)'+
    ');');
  c.DisposeOf;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  API.DisposeOf;
end;

{ TFirstController }

procedure TFirstController.HelloWorld;
begin
  Output.ContentText := 'Hello world!';
end;

procedure TFirstController.Greetings;
var
  Answer: TJsonObject;
begin
  Answer := TJsonObject.Create;
  Answer.S['userId'] := UserObject.UserId;
  Answer.S['message'] := Format('Hello, %s!', [UserObject.UserId]);

  Output.ContentText := Answer.ToJSON;

  Answer.DisposeOf;
end;

procedure TFirstController.Reverse;
begin
  if Input.Params.Values['s'] = '' then
  begin
    Output.ContentText := 'Param "s" is empty';
    exit;
  end;

  Output.ContentText := ReverseString(Input.Params.Values['s']);
end;

procedure TFirstController.SetPhoneNumber;
begin
  // Validate phone number param
  if not CheckString([Input.Params.Values['phone']], [3, 12], ['0'..'9', '+', '-', ' '])  then
  begin
    Output.ContentText := 'Phone number is incorrect';
    exit;
  end;

  TMyUserObject(UserObject).PhoneNumber := Input.Params.Values['phone'];

  Output.ContentText := 'Phone number was changed';
end;

procedure TFirstController.GetPhoneNumber;
begin
  Output.ContentText := TMyUserObject(UserObject).PhoneNumber;
end;

procedure TFirstController.ListPhoneNumbers;
var
  arr: TJsonArray;
begin
  arr := TJsonArray.Create;

  FDQuery.Open('SELECT * FROM phone_numbers');

  if FDQuery.RecordCount > 0 then
  begin
    FDQuery.First;
    while not FDQuery.Eof do
    begin
      with arr.AddObject do
      begin
        S['userId'] := FDQuery.FieldValues['userid'];
        S['phoneNumber'] := FDQuery.FieldValues['phone'];
      end;
      FDQuery.Next;
    end;
  end;

  Output.ContentText := arr.ToJSON;

  arr.DisposeOf;
end;

{ TMyUserObject }

constructor TMyUserObject.Create(const AUserId: string; AFDConnection: TFDConnection);
var
  q: TFDQuery;
begin
  inherited;

  FPhoneNumber := '';

  q := TFDQuery.Create(nil);
  q.Connection := FDConnection;
  q.Open(Format('SELECT phone FROM phone_numbers WHERE userid = ''%s''', [UserId]));
  if q.RecordCount > 0 then
  begin
    q.First;
    FPhoneNumber := q.FieldValues['phone'];
  end;
  q.DisposeOf;
end;

procedure TMyUserObject.SetPhoneNumber(const phone: string);
begin
  if FPhoneNumber = phone then
    exit;

  FPhoneNumber := phone;

  FDConnection.ExecSQL(Format(
    'INSERT INTO phone_numbers (userid, phone) VALUES (''%s'', ''%s'') '+
    'ON CONFLICT (userid) DO UPDATE SET phone = ''%s''',
    [UserId, phone, phone]));
end;

end.
