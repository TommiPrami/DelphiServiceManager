unit DSMTestFastService.Main;

{ Test service for the DelphiServiceManager unit tests.

  Starts and stops immediately, does not accept pause/continue. Used by tests that need a plain,
  well-behaving service. }

interface

uses
  Winapi.Windows, System.Classes, Vcl.SvcMgr;

type
  TDSMTestFast = class(TService)
  public
    function GetServiceController: TServiceController; override;
  end;

var
  DSMTestFast: TDSMTestFast;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DSMTestFast.Controller(CtrlCode);
end;

function TDSMTestFast.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

end.
