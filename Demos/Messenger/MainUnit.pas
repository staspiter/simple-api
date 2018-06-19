unit MainUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  // Common FireDAC classes for working with DB directly
  FireDAC.Comp.Client,

  SimpleAPI,

  MessagesController;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  Form1: TForm1;
  API: TSimpleAPI;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  c: TFDConnection;
begin
  // Create main API object
  // Every object of TSimpleAPI class is works like a parallel API with some specific port and type
  // There are two types of API: HTTP and WebSocket
  // For now we'll use HTTP
  API := TSimpleAPI.Create(TServerMode.HTTP, 8080);

  // Initialize connection with PostgreSQL (it's just common FireDAC connection strings)
  API.InitDB([
    'Server=127.0.0.1',
    'Database=messenger',
    'User_Name=postgres',
    'Password=123456',
    'DriverId=PG'
  ]);

  // Initialize "users accounts features"
  // These features allows to make user-authorized actions
  // User can be registered, then you can sign any action by user's token
  // which can be also requested
  API.InitUsers;

  // Register some controllers with actions to let our API know about them
  API.RegisterControllers([TMessagesController]);

  // Create temporary and parallel connection to the DB in order to init some tables
  c := TFDConnection.Create(nil);
  c.ConnectionDefName := API.ConnectionDef;
  c.Connected := true;

  // Check if the connection was failed
  if not c.Connected then
    raise Exception.Create('DB connection failed');

  // Create messages table in the DB
  // This table will be used by TMessagesController during actions handling
  c.ExecSQL(
    'CREATE TABLE IF NOT EXISTS messages ('+
      'id bigserial PRIMARY KEY,'+
      'time bigint,'+
      'roomId varchar(50),'+
      'userId varchar(50),'+
      'message text'+
    ');');

  // Free the connection
  c.DisposeOf;

  // *** ADDITIONAL (BUT QUITE IMPORTANT) NOTES ***

  // To register new user, use "users/create" action
  // http://localhost:8080/users/create?userId=<USERID>&passHashRequest=<SHA256_OF_PASSWORD>
  // e.g. http://localhost:8080/users/create?userId=stas&passHashRequest=8D969EEF6ECAD3C29A3A629280E686CF0C3F5D5A86AFF3CA12020C923ADC6C92
  // for userId = "stas" and password "123456"
  // The answer will be
  // {"answer":"user_added"}

  // To execute an authorized action, you need to get TOKEN
  // TOKEN for any registered user can be obtained by "users/gettoken" action
  // http://localhost:8080/users/gettoken?userId=<USERID>&name=<TOKEN_NAME>&passHashRequest=<SHA256_OF_PASSWORD>
  // e.g. http://localhost:8080/users/gettoken?userId=stas&name=test&passHashRequest=8D969EEF6ECAD3C29A3A629280E686CF0C3F5D5A86AFF3CA12020C923ADC6C92
  // The answer will be like
  // {"token":"ee231498d6ef59c232906c103a34282c01672478d6cdb7a4efba8975db1af4e5","userId":"stas","name":"test"}
  // Where we can find TOKEN

  // Using TOKEN we can access to authorized actions
  // http://localhost:8080/messages/send/<TOKEN>?roomId=room&text=HelloWorld!
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free API object
  API.DisposeOf;
end;

end.
