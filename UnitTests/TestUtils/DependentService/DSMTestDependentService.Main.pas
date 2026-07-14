unit DSMTestDependentService.Main;

{ Test service for the DelphiServiceManager unit tests.

  Starts and stops immediately, does not accept pause/continue, and depends on the DSMTestFast
  service (see Dependencies in the dfm). Used by the tests that verify dependency handling:
  starting this service makes the SCM start DSMTestFast too, and DSMTestFast cannot be stopped
  while this service is running. }

interface

uses
  Winapi.Windows, System.Classes, Vcl.SvcMgr;

type
  TDSMTestDependent = class(TService)
  public
    function GetServiceController: TServiceController; override;
  end;

var
  DSMTestDependent: TDSMTestDependent;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DSMTestDependent.Controller(CtrlCode);
end;

function TDSMTestDependent.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

end.
