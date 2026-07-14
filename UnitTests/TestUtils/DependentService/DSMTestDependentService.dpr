program DSMTestDependentService;

uses
  Vcl.SvcMgr,
  DSMTestDependentService.Main in 'DSMTestDependentService.Main.pas' {DSMTestDependent: TService};

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;

  Application.CreateForm(TDSMTestDependent, DSMTestDependent);
  Application.Run;
end.
