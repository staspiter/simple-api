program SimpleHTTPAPIDemo;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
