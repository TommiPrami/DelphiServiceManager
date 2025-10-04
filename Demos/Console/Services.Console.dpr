program Services.Console;

uses
  Vcl.Forms,
  SCForm.Console in 'SCForm.Console.pas' {SCConsoleForm},
  Windows.ServiceManager in '..\..\Windows.ServiceManager.pas',
  Windows.ServiceManager.Types in '..\..\Windows.ServiceManager.Types.pas',
  Windows.ServiceManager.Consts in '..\..\Windows.ServiceManager.Consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSCConsoleForm, SCConsoleForm);
  Application.Run;
end.
