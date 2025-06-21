program ProjectDemo;

uses
  Vcl.Forms,
  demo in 'demo.pas' {frmDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
