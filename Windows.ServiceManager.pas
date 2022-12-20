unit Windows.ServiceManager;

{ --------------------------------------------------------------------------- }
{                                                                             }
{ Written with                                                                }
{   - Delphi XE3 Pro                                                          }
{   - Refactored with 11.2                                                    }
{                                                                             }
{ Created Nov 24, 2012 by Darian Miller                                       }
{   - Some refactoring etc by Tommi Prami                                     }
{                                                                             }
{ Based on answer by Ritsaert Hornstra on May 6, 2011 to question:            }
{   - http://stackoverflow.com/questions/5913279/detect-windows-service-state }
{   - https://stackoverflow.com/users/246383/ritsaert-hornstra (Thanks man)   }
{                                                                             }
{ --------------------------------------------------------------------------- }

interface

uses
  System.SysUtils, Winapi.Windows, Winapi.Winsvc;

type
  ECustomServiceManagerException = class(Exception);
  EIndexOutOfBounds = class(ECustomServiceManagerException);
  EServiceNotFound = class(ECustomServiceManagerException);
  EOSNotSupoerted = class(ECustomServiceManagerException);
  EOperationNotAllowedWhileActive = class(ECustomServiceManagerException);
  ELockingNotAllowed = class(ECustomServiceManagerException);
  EServiceStateUnkown = class(ECustomServiceManagerException);
  EServiceCannotBeContinued = class(ECustomServiceManagerException);
  EServiceCannotBePaused = class(ECustomServiceManagerException);
  EServiceCannotBeStopped = class(ECustomServiceManagerException);
  EServiceDidNotRespond = class(ECustomServiceManagerException);
  EServiceServiceStartTypeUnknown = class(ECustomServiceManagerException);

  // Forward declaration of Service manager class
  TServiceManager = class;

  { The states a service can be in. }
  TServiceState = (ssStopped,
                   ssStartPending,
                   ssStopPending,
                   ssRunning,
                   ssContinuePending,
                   ssPausePending,
                   ssPaused);

  { Enumeration of the standard "controls" a service can accept. The shutdown control, if not
    accepted is ignored. The shutdown control can only be sent when a shutdown occurs. }
  TServiceAccept = (saStop,
                    saPauseContinue,
                    saShutdown);

  { The set of "controls" a service can accept. }
  TServiceAccepts = set of TServiceAccept;

  { The service startup enumeration determines how a service is started. ssAutomatic will start the
    service automatically at startup. ssManual will allow applications and other services to start
    this service manually and ssDisabled will disallow the service to be started altogether (but it
    will be kept in the service database). }
  TServiceStartup = (ssAutomatic,
                     ssManual,
                     ssDisabled);

  { Gives information of and controls a single Service. Can be accessed via @link(TServiceManager). }
  TServiceInfo = class(TObject)
  private
    { Placeholder of the Index property.  Assigned by the ServiceManager that created this instance. }
    FIndex: Integer;
    { Link the the creating service manager. }
    FServiceManager: TServiceManager;
    { Handle of the service during several member calls. }
    FServiceHandle
    : SC_HANDLE;
    { Status of this service. This contains several fields for several properties. }
    FServiceStatus: TServiceStatus;
    { Key name of this service. }
    FServiceName: string;
    { Display name of this service. }
    FDisplayName: string;
    { Are the depenedents searched. If so the @link(FDependents) array is filled with those. }
    FDependentsSearched: Boolean;
    { Array of @link(TServiceInfo) instances that depent on this service. Only filled when
      @link(FDependentsSearched) is True. }
    FDependents: array of TServiceInfo;
    { Placeholder for the live}
    FLive: Boolean;
    // Query Config
    FConfigQueried: Boolean;
    FOwnProcess: Boolean;
    FInteractive: Boolean;
    FStartType: TServiceStartup;
    FBinaryPathName: string;
    FUserName: string;
    function GetDependent(const AIndex: Integer): TServiceInfo;
    function GetDependentCount: Integer;
    function GetState: TServiceState;
    function GetOwnProcess: Boolean;
    function GetInteractive: Boolean;
    function GetStartType: TServiceStartup;
    function GetBinaryPathname: string;
    procedure WaitForPendingServiceState(const AServiceState: TServiceState);
    procedure SetState(const AServiceState: TServiceState);
    function GetServiceAccepts: TServiceAccepts;
    procedure SetStartType(const AValue: TServiceStartup);
  protected
    { Cleanup the handle created with @link(GetHandle). }
    procedure CleanupHandle;
    { Open a handle to the service with the given access rights.
      This handle can be deleted via @link(CleanupHandle). }
    procedure GetHandle(const AAccess: DWORD);
    { Query all dependent services (list them via the @link(TServiceManager). }
    procedure SearchDependents;
    { Query the current status of this service }
    procedure Query;
    { Wait for a given status of this service... }
    procedure WaitFor(const AState: DWORD);
    { Fetch the configuration information }
    procedure QueryConfig;
  public
    constructor Create;
    destructor Destroy; override;

    { Action: Pause a running service. }
    procedure ServicePause(const AWait: Boolean);
    { Action: Continue a paused service. }
    procedure ServiceContinue(const AWait: Boolean);
    { Action: Stop a running service. }
    procedure ServiceStop(const AWait: Boolean);
    { Action: Start a not running service.
      You can use the @link(State) property to change the state from ssStopped to ssRunning }
    procedure ServiceStart(const AWait: Boolean);
    { Name of this service. }
    property ServiceName: string read FServiceName;
    { Display name of this service }
    property DisplayName: string read FDisplayName;
    { Number of dependant services of this service }
    property DependentCount: Integer read GetDependentCount;
    { Access to serviced that depent on this service }
    property Dependents[const AIndex: Integer]: TServiceInfo read GetDependent;
    { The current state of the service. You can set the service only to the non-transitional states.
      You can restart the service by first setting the State to first ssStopped and second ssRunning. }
    property State: TServiceState read GetState write SetState;
    { Are various properties using live information or historic information. }
    property Live: Boolean read FLive write FLive;
    { When service is running, does it run as a separate process (own process) or combined with
      other services under svchost. }
    property OwnProcess: Boolean read GetOwnProcess;
    { Is the service capable of interacting with the desktop.
      Possible: The logon must the Local System Account. }
    property Interactive: Boolean read GetInteractive;
    { How is this service started. See @link(TServiceStartup) for a description of startup types.
      If you want to set this property, the manager must be activeted with AllowLocking set to True. }
    property StartType: TServiceStartup read GetStartType write SetStartType;
    { Path to the binary that implements the service. }
    property BinaryPathName: string read GetBinaryPathName;
    { See what controls the service accepts. }
    property ServiceAccepts: TServiceAccepts read GetServiceAccepts;
    { Index in ServiceManagers list }
    property Index: Integer read FIndex write FIndex;
    property UserName: string read FUserName;
  end;

  { A service manager allows the services of a particular machine to be explored and modified. }
  TServiceManager = class(TObject)
  strict private
    FManagerHandle: SC_HANDLE;
    FLockHandle: SC_LOCK;
    FMachineName: string;
    FServices: TArray<TServiceInfo>;
    FAllowLocking: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const ASetToActive: Boolean);
    procedure SetMachineName(const AMachineName: string);
    function GetServiceCount: Integer;
    function GetService(const AIndex: Integer): TServiceInfo;
    function GetServiceByName(const AName: string): TServiceInfo;
    procedure CheckArrayBounds(const AIndex: Integer);
    procedure SetAllowLocking(const AValue: Boolean);
  protected
    function GetManagerHandle: SC_HANDLE;
    { Internal function that frees up all the @link(TServiceInfo) classes. }
    procedure CleanupServices;
    { Internal function for locking the manager }
    procedure Lock;
    { Internal function for unlocking the manager }
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;

    { Requeries the states, names etc of all services on the given @link(MachineName).
      Works only while active. }
    procedure RebuildServicesList;
    { Delete a service... }
    // procedure DeleteService(Index: Integer);
    { Get the number of services. This number is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. }
    property ServiceCount: Integer read GetServiceCount;
    { Find a servce by index in the services list. This list is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. Valid Index
      values are 0..@link(ServiceCount) - 1. }
    property Services[const AIndex: Integer]: TServiceInfo read GetService;
    { Find services by name (case insensitive). Works only while active. If no service can be found
      an exception will be raised. }
    property ServiceByName[const SerciveName: string]: TServiceInfo read GetServiceByName;
    { Activate / deactivate the service manager. In active state can you access the individual
      service }
    property Active: Boolean read GetActive write SetActive;
    { The machine name for which you want the services list. }
    property MachineName: string read FMachineName write SetMachineName;
    { Allow locking... Is needed only when changing several properties in TServiceInfo.
      Property can only be set while inactive. }
    property AllowLocking: Boolean read FAllowLocking write SetAllowLocking;

    procedure SortByDisplayName;
    function GetServicesByDisplayName: TArray<TServiceInfo>;
  end;

implementation
uses
  System.Generics.Collections, System.Generics.Defaults;

procedure RaiseIndexOutOfBounds;
begin
  raise EIndexOutOfBounds.Create('Index out of bounds');
end;

{ TServiceManager }

procedure TServiceManager.RebuildServicesList;
var
  LServices: PEnumServiceStatus;
  LServicesLoopPointer: PEnumServiceStatus;
  LBytesNeeded: DWORD;
  LServicesReturned: DWORD;
  LResumeHandle: DWORD;
  LIndex: Integer;
begin
  if not Active then raise
    Exception.Create('BuildServicesList only works when active');

  // Cleanup
  CleanupServices;

  // Get the amount of memory we need...
  LServicesReturned := 0;
  LResumeHandle := 0;
  LServices := nil;

  if EnumServicesStatus(FManagerHandle, SERVICE_WIN32, SERVICE_STATE_ALL, LServices, 0, LBytesNeeded, LServicesReturned,
    LResumeHandle) then
    Exit;

  if GetLastError <> ERROR_MORE_DATA then
    RaiseLastOSError;

  // And... Get all the data...
  GetMem(LServices, LBytesNeeded);
  try
    LServicesReturned := 0;
    LResumeHandle := 0;

    LServicesLoopPointer := LServices;

    if not EnumServicesStatus(FManagerHandle, SERVICE_WIN32, SERVICE_STATE_ALL, LServices , LBytesNeeded, LBytesNeeded,
      LServicesReturned, LResumeHandle) then
      Exit;

    SetLength(FServices, LServicesReturned);
    for LIndex := 0 to LServicesReturned - 1 do
    begin
      FServices[LIndex] := TServiceInfo.Create;
      FServices[LIndex].FServiceName := LServicesLoopPointer^.lpServiceName;
      FServices[LIndex].FDisplayName := LServicesLoopPointer^.lpDisplayName;
      FServices[LIndex].FServiceStatus := LServicesLoopPointer^.ServiceStatus;
      FServices[LIndex].FServiceManager := Self;
      FServices[LIndex].FIndex := LIndex;
      
      Inc(LServicesLoopPointer);
    end;
  finally
    FreeMem(LServices);
  end;
end;

procedure TServiceManager.CheckArrayBounds(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FServices)) then
    RaiseIndexOutOfBounds;
end;

procedure TServiceManager.CleanupServices;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FServices) do
    FServices[LIndex].Free;

  SetLength(FServices, 0);
end;

constructor TServiceManager.Create;
begin
  inherited Create;

  FManagerHandle := 0;
end;

destructor TServiceManager.Destroy;
begin
  Active := False;

  inherited Destroy;
end;

function TServiceManager.GetActive: Boolean;
begin
  Result := FManagerHandle <> 0;
end;

function TServiceManager.GetManagerHandle: SC_HANDLE;
begin
  Result := FManagerHandle;
end;

function TServiceManager.GetService(const AIndex: Integer): TServiceInfo;
begin
  // Sanity check
  CheckArrayBounds(AIndex);

  // Fetch the object of interest
  Result := FServices[AIndex];
end;

function TServiceManager.GetServiceByName(const AName: string): TServiceInfo;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FServices) do
  begin
    Result := FServices[LIndex];

    if SameText(Result.ServiceName, AName) then
      Exit;
  end;

  raise EServiceNotFound.Create('Service not found');
end;

function TServiceManager.GetServiceCount: Integer;
begin
  Result := Length(FServices);
end;

function TServiceManager.GetServicesByDisplayName: TArray<TServiceInfo>;
begin
  Result := FServices;

  TArray.Sort<TServiceInfo>(Result, TDelegatedComparer<TServiceInfo>.Construct(
    function(const Left, Right:TServiceInfo): Integer
    begin
      Result := TComparer<string>.Default.Compare(Left.DisplayName, Right.DisplayName);
    end));
end;

procedure TServiceManager.SetActive(const ASetToActive: Boolean);
var
  LVersionInfo: TOSVersionInfo;
  LDesiredAccess: DWORD;
begin
  if ASetToActive then
  begin
    if Active then
      Exit;

    // Check that we are NT, 2000, XP or above...
    LVersionInfo.dwOSVersionInfoSize := sizeof(LVersionInfo);

    if not GetVersionEx(LVersionInfo) then
      RaiseLastOSError;

    if LVersionInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT then
      raise EOSNotSupoerted.Create('This program only works on Windows NT, 2000 or XP');

    // Open service manager
    LDesiredAccess := SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE;
    if FAllowLocking then
      Inc(LDesiredAccess, SC_MANAGER_LOCK);

    FManagerHandle := OpenSCManager(PChar(FMachineName), nil, LDesiredAccess);
    if not Active then
      RaiseLastOSError;

    // Fetch the srvices list
    RebuildServicesList;
  end
  else
  begin
    if not Active then
      Exit;
    // CleanupServices
    CleanupServices;
    // Close service manager
    if Assigned(FLockHandle) then
      Unlock;

    CloseServiceHandle(FManagerHandle);
    FManagerHandle := 0;
  end;
end;

procedure TServiceManager.SetMachineName(const AMachineName: string);
begin
  if Active then
    raise EOperationNotAllowedWhileActive.Create('Cannot change machine name while active');

  FMachineName := AMachineName;
end;

(*
procedure TServiceManager.DeleteService(Index: Integer);
begin
  // todo: implementation
  raise Exception.Create('Not implemented');
end;
*)

procedure TServiceManager.Lock;

  procedure CheckRaiseLockingNotAllowed;
  begin
    if not FAllowLocking then
      raise ELockingNotAllowed.Create('Locking of the service manager not allowed!');
  end;

begin
  CheckRaiseLockingNotAllowed;

  FLockHandle := LockServiceDatabase(FManagerHandle);

  if FLockHandle = nil then
    RaiseLastOSError;
end;

procedure TServiceManager.Unlock;
begin
  // We are unlocked already
  if FLockHandle = nil then
    Exit;

  // Unlock...
  if not UnlockServiceDatabase(FLockHandle) then
    RaiseLastOSError;

  FLockHandle := nil;
end;

procedure TServiceManager.SetAllowLocking(const AValue: Boolean);
begin
  if Active then
    raise EOperationNotAllowedWhileActive.Create('Cannot change allow locking while active');

  FAllowLocking := AValue;
end;

procedure TServiceManager.SortByDisplayName;
var
  LIndex: Integer;
begin
  TArray.Sort<TServiceInfo>(FServices, TDelegatedComparer<TServiceInfo>.Construct(
    function(const Left, Right:TServiceInfo): Integer
    begin
      Result := TComparer<String>.Default.Compare(Left.DisplayName, Right.DisplayName);
    end));

  for LIndex := Low(FServices) to High(FServices) do
    FServices[LIndex].Index := LIndex;     
end;

{ TServiceInfo }

procedure TServiceInfo.CleanupHandle;
begin
  if FServiceHandle = 0 then
    Exit;

  CloseServiceHandle(FServiceHandle);
  FServiceHandle := 0;
end;

constructor TServiceInfo.Create;
begin
  inherited Create;

  FDependentsSearched := False;
  FConfigQueried := False;
  FServiceHandle := 0;
  FLive := False;
end;

destructor TServiceInfo.Destroy;
begin
  CleanupHandle;

  inherited Destroy;
end;

function TServiceInfo.GetDependent(const AIndex: Integer): TServiceInfo;
begin
  SearchDependents;

  if (AIndex < 0) or (AIndex >= Length(FDependents)) then
    RaiseIndexOutOfBounds;

  Result := FDependents[AIndex];
end;

function TServiceInfo.GetDependentCount: Integer;
begin
  SearchDependents;

  Result := Length(FDependents);
end;

procedure TServiceInfo.GetHandle(const AAccess: DWORD);
begin
  if FServiceHandle <> 0 then
    Exit;

  FServiceHandle := OpenService(FServiceManager.GetManagerHandle, PChar(FServiceName), AAccess);

  if FServiceHandle = 0 then
    RaiseLastOSError;
end;

function TServiceInfo.GetState: TServiceState;

  procedure RaiseServiceStateUnkown;
  begin
    raise EServiceStateUnkown.Create('Service State unknown');
  end;

begin
  if FLive then
    Query;

  case FServiceStatus.dwCurrentState of
    SERVICE_STOPPED:          Result := ssStopped;
    SERVICE_START_PENDING:    Result := ssStartPending;
    SERVICE_STOP_PENDING:     Result := ssStopPending;
    SERVICE_RUNNING:          Result := ssRunning;
    SERVICE_CONTINUE_PENDING: Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:    Result := ssPausePending;
    SERVICE_PAUSED:           Result := ssPaused;
    else
    begin
      RaiseServiceStateUnkown;
      Result := ssStopped; // Make compiler happy
    end;
  end;
end;

procedure TServiceInfo.Query;
var
  LStatus: TServiceStatus;
begin
  if FServiceHandle <> 0 then
  begin
    if not QueryServiceStatus(FServiceHandle, LStatus) then
      RaiseLastOSError;
  end
  else
  begin
    GetHandle(SERVICE_QUERY_STATUS);
    try
      if not QueryServiceStatus(FServiceHandle, LStatus) then
        RaiseLastOSError;
    finally
      CleanupHandle;
    end;
  end;

  FServiceStatus := LStatus;
end;

procedure TServiceInfo.ServiceContinue(const AWait: Boolean);

  procedure RaiseServiceCannotBeContinued;
  begin
    raise EServiceCannotBeContinued.Create('Service cannot be continued');
  end;

var
  LStatus: TServiceStatus;
begin
  GetHandle(SERVICE_QUERY_STATUS or SERVICE_PAUSE_CONTINUE);
  try
    if not (saPauseContinue in ServiceAccepts) then
      RaiseServiceCannotBeContinued;

    if not ControlService(FServiceHandle, SERVICE_CONTROL_CONTINUE, LStatus) then
      RaiseLastOSError;

    if AWait then
      WaitFor(SERVICE_RUNNING);
  finally
    CleanupHandle;
  end;
end;

procedure TServiceInfo.ServicePause(const AWait: Boolean);

  procedure RaiseServiceCannotBePaused;
  begin
    raise EServiceCannotBePaused.Create('Service cannot be paused');
  end;

var
  LStatus: TServiceStatus;
begin
  GetHandle(SERVICE_QUERY_STATUS or SERVICE_PAUSE_CONTINUE);
  try
    if not (saPauseContinue in ServiceAccepts) then
      RaiseServiceCannotBePaused;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_PAUSE, LStatus) then
      RaiseLastOSError;

    if AWait then
      WaitFor(SERVICE_PAUSED);
  finally
    CleanupHandle;
  end;
end;

procedure TServiceInfo.ServiceStart(const AWait: Boolean);
var
  LServiceArgumentVectors: PCHar;
begin
  GetHandle(SERVICE_QUERY_STATUS or SERVICE_START);
  try
    LServiceArgumentVectors := nil;
    if not StartService(FServiceHandle, 0, LServiceArgumentVectors) then
      RaiseLastOSError;

    if AWait then
      WaitFor(SERVICE_RUNNING);
  finally
    CleanupHandle;
  end;
end;

procedure TServiceInfo.ServiceStop(const AWait: Boolean);

  procedure RaiseServiceCannotBeStopped;
  begin
    raise EServiceCannotBeStopped.Create('Service cannot be Stopped');
  end;

var
  LStatus: TServiceStatus;
begin
  GetHandle(SERVICE_QUERY_STATUS or SERVICE_STOP);
  try
    if not (saStop in ServiceAccepts) then
      RaiseServiceCannotBeStopped;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_STOP, LStatus) then
      RaiseLastOSError;

    if AWait then
      WaitFor(SERVICE_STOPPED);
  finally
    CleanupHandle;
  end;
end;

procedure TServiceInfo.WaitFor(const AState: DWORD);

  procedure RaiseServiceDidNotRespond;
  begin
    raise EServiceDidNotRespond.Create('Service did not react within timeframe given');
  end;

var
  LOldCheckPoint: DWORD;
  LWait: DWORD;
begin
  Query;

  while AState <> FServiceStatus.dwCurrentState do
  begin
    LOldCheckPoint := FServiceStatus.dwCheckPoint;
    LWait := FServiceStatus.dwWaitHint;

    if LWait <= 0 then
      LWait := 5000;
    Sleep(LWait);

    Query;

    if AState = FServiceStatus.dwCurrentState then
      Break;

    if FServiceStatus.dwCheckPoint <> LOldCheckPoint then
      RaiseServiceDidNotRespond;
  end;
end;

procedure TServiceInfo.WaitForPendingServiceState(const AServiceState: TServiceState);
begin
  case AServiceState of
    ssStartPending:    WaitFor(SERVICE_RUNNING);
    ssStopPending:     WaitFor(SERVICE_STOPPED);
    ssContinuePending: WaitFor(SERVICE_RUNNING);
    ssPausePending:    WaitFor(SERVICE_PAUSED);
  end;
end;

procedure TServiceInfo.SearchDependents;
var
  LServicesStatus: PEnumServiceStatus;
  LTempStatus: PEnumServiceStatus;
  LBytesNeeded: DWORD;
  LServicesReturned: DWORD;
  LIndex: Integer;
begin
  if FDependentsSearched then
    Exit;

  // No dependents found...
  SetLength(FDependents,0);
  // We need a handle to the service to do any good...
  GetHandle(SERVICE_ENUMERATE_DEPENDENTS);
  try
    // See how many dependents we have...
    LServicesStatus := nil;
    LBytesNeeded := 0;
    LServicesReturned := 0;

    if EnumDependentServices(FServiceHandle, SERVICE_ACTIVE + SERVICE_INACTIVE, LServicesStatus, 0, LBytesNeeded,
      LServicesReturned) then
      Exit;

    if GetLastError <> ERROR_MORE_DATA then
      RaiseLastOSError;

    // Allocate the buffer needed and fetch all info...
    GetMem(LServicesStatus,LBytesNeeded);
    try
      if not EnumDependentServices(FServiceHandle,SERVICE_ACTIVE + SERVICE_INACTIVE, LServicesStatus, LBytesNeeded, LBytesNeeded,
        LServicesReturned) then
        RaiseLastOSError;

      // Now process it...
      LTempStatus := LServicesStatus;
      SetLength(FDependents,LServicesReturned);
      for LIndex := 0 to High(FDependents) do begin
        FDependents[LIndex] := FServiceManager.ServiceByName[LTempStatus^.lpServiceName];
        Inc(LTempStatus);
      end;
    finally
      FreeMem(LServicesStatus);
    end;
  finally
    CleanupHandle;
  end;
  FDependentsSearched := True;
end;

procedure TServiceInfo.QueryConfig;

  procedure RaiseServiceServiceStartTypeUnknown;
  begin
    raise EServiceServiceStartTypeUnknown.Create('Service Start Type unknown');
  end;

var
  LBuffer: LPQUERY_SERVICE_CONFIG;
  LBytesNeeded: DWORD;
begin
  GetHandle(SERVICE_QUERY_CONFIG);
  try
    // See how large our buffer must be...
    Assert(not QueryServiceConfig(FServiceHandle, nil, 0, LBytesNeeded), 'Could not get buffer size');

    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      RaiseLastOSError;

    GetMem(LBuffer,LBytesNeeded);
    try
      // Perform the query...
      if not QueryServiceConfig(FServiceHandle,LBuffer,LBytesNeeded,LBytesNeeded) then
        RaiseLastOSError;

      // Analyze the query...
      Assert(LBuffer^.dwServiceType and SERVICE_WIN32 <> 0); // It must be a WIN32 service
      FOwnProcess := (LBuffer^.dwServiceType and SERVICE_WIN32) = SERVICE_WIN32_OWN_PROCESS;
      FInteractive := (LBuffer^.dwServiceType and SERVICE_INTERACTIVE_PROCESS) = SERVICE_INTERACTIVE_PROCESS;

      case LBuffer^.dwStartType of
        SERVICE_AUTO_START:    FStartType := ssAutomatic;
        SERVICE_DEMAND_START:  FStartType := ssManual;
        SERVICE_DISABLED:      FStartType := ssDisabled;
        else
          RaiseServiceServiceStartTypeUnknown;
      end;

      FBinaryPathName := LBuffer^.lpBinaryPathName;
      FUsername := LBuffer^.lpServiceStartName;
      FConfigQueried := True;
    finally
      FreeMem(LBuffer);
    end;
  finally
    CleanupHandle;
  end;
end;

function TServiceInfo.GetOwnProcess: Boolean;
begin
  if FLive or not FConfigQueried then
    QueryConfig;

  Result := FOwnProcess;
end;

function TServiceInfo.GetInteractive: Boolean;
begin
  if FLive or not FConfigQueried then
    QueryConfig;

  Result := FInteractive;
end;

function TServiceInfo.GetStartType: TServiceStartup;
begin
  if FLive or not FConfigQueried then
    QueryConfig;

  Result := FStartType;
end;

function TServiceInfo.GetBinaryPathName: string;
begin
  if FLive or not FConfigQueried then
    QueryConfig;

  Result := FBinaryPathName;
end;

function TServiceInfo.GetServiceAccepts: TServiceAccepts;
begin
  Result := [];

  if FLive then
    Query;

  if FServiceStatus.dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE <> 0 then
    Result := Result + [saPauseContinue];

  if FServiceStatus.dwControlsAccepted and SERVICE_ACCEPT_STOP <> 0 then
    Result := Result + [saStop];

  if FServiceStatus.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0 then
    Result := Result + [saShutdown];
end;



procedure TServiceInfo.SetState(const AServiceState: TServiceState);
var
  LOldState: TServiceState;
begin
  // Make sure we have the latest current state and that it is not a transitional state.
  if not FLive then
    Query;

  WaitForPendingServiceState(GetState);

  LOldState := GetState;
  // See what we need to do...
  case AServiceState of
    ssStopped:
      if LOldState <> ssStopped then
        ServiceStop(True);
    ssRunning:
      case LOldState of
        ssStopped: ServiceStart(True);
        ssPaused:  ServiceContinue(True);
      end;
    ssPaused:
      case LOldState of
        ssStopped:
          begin
            ServiceStart(True);
            try
              ServicePause(True); // some services do not support pause/continue!
            except
              ServiceStop(True);
              raise;
            end;
          end;
        ssRunning: ServicePause(True);
      end;
    else
      raise Exception.Create('Cannot set a transitional state in TServiceInfo.');
  end;
end;


procedure TServiceInfo.SetStartType(const AValue: TServiceStartup);
const
  NEW_START_TYPES: array [TServiceStartup] of DWORD = (SERVICE_AUTO_START, SERVICE_DEMAND_START, SERVICE_DISABLED);
begin
  // Check if it is not a change?
  QueryConfig;

  if AValue = FStartType then
    Exit;

  // Alter it...
  FServiceManager.Lock;
  try
    GetHandle(SERVICE_CHANGE_CONFIG);
    try
      // We locked the manager and are allowed to change the configuration...
      if not ChangeServiceConfig(FServiceHandle, SERVICE_NO_CHANGE, NEW_START_TYPES[AValue], SERVICE_NO_CHANGE,
        nil, nil, nil, nil, nil, nil, nil) then
        RaiseLastOSError;

      // well... we changed it, mark as such
      FStartType := AValue;
    finally
      CleanupHandle;
    end;
  finally
    FServiceManager.Unlock;
  end;
end;

end.
