program DSMTestFastService;

uses
  Vcl.SvcMgr,
  DSMTestFastService.Main in 'DSMTestFastService.Main.pas' {DSMTestFast: TService};

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;

  Application.CreateForm(TDSMTestFast, DSMTestFast);
  Application.Run;
end.
