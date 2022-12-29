program FirbirdService;

uses
  Vcl.Forms,
  FirbirdService.MainForm in 'FirbirdService.MainForm.pas' {FormFirebierdServiceMain},
  Windows.ServiceManager.Types in '..\..\Windows.ServiceManager.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFirebierdServiceMain, FormFirebierdServiceMain);
  Application.Run;
end.
