unit DSMTestPausableService.Main;

{ Test service for the DelphiServiceManager unit tests.

  Starts and stops immediately and accepts pause/continue. Used by the tests that verify the
  pause/continue handling. }

interface

uses
  Winapi.Windows, System.Classes, Vcl.SvcMgr;

type
  TDSMTestPausable = class(TService)
  public
    function GetServiceController: TServiceController; override;
  end;

var
  DSMTestPausable: TDSMTestPausable;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DSMTestPausable.Controller(CtrlCode);
end;

function TDSMTestPausable.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

end.
