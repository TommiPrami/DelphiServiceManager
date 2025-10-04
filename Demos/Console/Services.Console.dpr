program Services.Console;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  SCForm.Console in 'SCForm.Console.pas' {SCConsoleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSCConsoleForm, SCConsoleForm);
  Application.Run;
end.
