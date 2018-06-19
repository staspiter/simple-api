program MessengerDemo;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  MessagesController in 'MessagesController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
