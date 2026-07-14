program DSMTestPausableService;

uses
  Vcl.SvcMgr,
  DSMTestPausableService.Main in 'DSMTestPausableService.Main.pas' {DSMTestPausable: TService};

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TDSMTestPausable, DSMTestPausable);
  Application.Run;
end.
