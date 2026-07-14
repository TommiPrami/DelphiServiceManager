program DSMTestSlowService;

uses
  Vcl.SvcMgr,
  DSMTestSlowService.Main in 'DSMTestSlowService.Main.pas' {DSMTestSlow: TService};

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TDSMTestSlow, DSMTestSlow);
  Application.Run;
end.
