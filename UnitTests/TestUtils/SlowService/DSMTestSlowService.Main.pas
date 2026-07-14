unit DSMTestSlowService.Main;

{ Test service for the DelphiServiceManager unit tests.

  Deliberately slow to start and stop: both take SLOW_OPERATION_STEP_COUNT * SLOW_OPERATION_STEP_DELAY_MS
  milliseconds, reporting progress (incrementing checkpoints) to the SCM while at it, like a
  well-behaving service should. Used by the tests that verify the WaitFor logic.

  NOTE: If you change the timing constants, keep SLOW_TEST_SERVICE_OPERATION_MS in
        DWSMUnit.TestServiceUtils.pas in sync. }

interface

uses
  Winapi.Windows, System.Classes, Vcl.SvcMgr;

type
  TDSMTestSlow = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  strict private
    procedure SimulateSlowOperation;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  DSMTestSlow: TDSMTestSlow;

implementation

{$R *.dfm}

const
  SLOW_OPERATION_STEP_COUNT = 4;
  SLOW_OPERATION_STEP_DELAY_MS = 750;

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  DSMTestSlow.Controller(CtrlCode);
end;

function TDSMTestSlow.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TDSMTestSlow.SimulateSlowOperation;
var
  LStep: Integer;
begin
  for LStep := 1 to SLOW_OPERATION_STEP_COUNT do
  begin
    Sleep(SLOW_OPERATION_STEP_DELAY_MS);

    // Tell the SCM we are still alive and making progress (increments the checkpoint)
    ReportStatus;
  end;
end;

procedure TDSMTestSlow.ServiceStart(Sender: TService; var Started: Boolean);
begin
  SimulateSlowOperation;

  Started := True;
end;

procedure TDSMTestSlow.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  SimulateSlowOperation;

  Stopped := True;
end;

end.
