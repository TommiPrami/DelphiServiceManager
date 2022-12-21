program FirbirdService;

uses
  Vcl.Forms,
  FirbirdService.MainForm in 'FirbirdService.MainForm.pas' {FormFirebierdServiceMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFirebierdServiceMain, FormFirebierdServiceMain);
  Application.Run;
end.
