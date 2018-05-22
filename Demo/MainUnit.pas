// TODO: Custom user object
// TODO: DB access

unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JsonDataObjects, System.StrUtils,

  SimpleHTTPAPI, SimpleHTTPAPI.Model;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    API: TSimpleHTTPAPI;
  end;

  [Model('FirstModel')]
  TFirstModel = class(TModel)
  public

    // *** Public available request ***
    // http://localhost:8080/firstmodel/helloworld
    [Action('HelloWorld'), PublicAccess]
    procedure HelloWorld;

    // *** User available request ***
    // http://localhost:8080/firstmodel/greetings/<Token>
    // Where Token you can get from
    // http://localhost:8080/accounts/getToken?userId=<RegisteredUserId>&passHash=<SHA256 of password>&name=<Token name, i.e. "test">
    // Where RegisteredUserId can be registered at
    // http://localhost:8080/accounts/create?userId=<UserId>&passHash=<SHA256 of password>
    [Action('Greetings')]
    procedure Greetings;

    // *** Reverse string from param. Method is available for authorized user ***
    // http://localhost:8080/firstmodel/reverse/<Token>?s=123456
    [Action('Reverse')]
    procedure Reverse;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  ConnectionStrings: TStringList;
begin
  TFirstModel.Register;

  ConnectionStrings := TStringList.Create;
  ConnectionStrings.Add('Server=127.0.0.1');
  ConnectionStrings.Add('Database=test');
  ConnectionStrings.Add('User_Name=postgres');
  ConnectionStrings.Add('Password=!2#stas$5^');
  ConnectionStrings.Add('DriverId=PG');
  ConnectionStrings.Add('Pooled=true');

  API := TSimpleHTTPAPI.Create(8080, ConnectionStrings);

  ConnectionStrings.DisposeOf;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  API.DisposeOf;
end;

{ TFirstModel }

procedure TFirstModel.HelloWorld;
begin
  Output.ContentText := 'Hello world!';
end;

procedure TFirstModel.Greetings;
var
  Answer: TJsonObject;
begin
  Answer := TJsonObject.Create;
  Answer.S['userId'] := UserObject.UserId;
  Answer.S['message'] := Format('Hello, %s!', [UserObject.UserId]);

  Output.ContentText := Answer.ToJSON;

  Answer.DisposeOf;
end;

procedure TFirstModel.Reverse;
begin
  if Input.Params.Values['s'] = '' then
  begin
    Output.ContentText := 'Param "s" is empty';
    exit;
  end;

  Output.ContentText := ReverseString(Input.Params.Values['s']);
end;

end.
