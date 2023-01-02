program FirbirdService;

uses
  Vcl.Forms,
  FirbirdService.MainForm in 'FirbirdService.MainForm.pas' {FormFirebierdServiceMain},
  Windows.ServiceManager in '..\..\Windows.ServiceManager.pas',
  Windows.ServiceManager.Types in '..\..\Windows.ServiceManager.Types.pas',
  Windows.ServiceManager.Consts in '..\..\Windows.ServiceManager.Consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFirebierdServiceMain, FormFirebierdServiceMain);
  Application.Run;
end.
