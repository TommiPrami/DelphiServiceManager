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
  Winapi.Windows, Winapi.Winsvc, System.Generics.Collections, System.SysUtils, Windows.ServiceManager.Types;

type
  // Forward declaration of Service manager class
  TServiceManager = class;

  { Gives information of and controls a single Service. Can be accessed via @link(TServiceManager). }
  TServiceInfo = class(TObject)
  private
    FIndex: Integer;
    FServiceManager: TServiceManager;
    FServiceHandle: SC_HANDLE;
    FServiceStatus: TServiceStatus;
    FServiceName: string;
    FDisplayName: string;
    { Placeholder for the live}
    FLive: Boolean;
    FConfigQueried: Boolean;
    FOwnProcess: Boolean;
    FInteractive: Boolean;
    FStartType: TServiceStartup;
    FBinaryPathName: string;
    FUserName: string;
    function DependenciesToList(const AQServicesStatus: PEnumServiceStatus; const AServiceInfoCount: Integer): TArray<TServiceInfo>;
    function GetState: TServiceState;
    function GetOwnProcess: Boolean;
    function GetInteractive: Boolean;
    function GetStartType: TServiceStartup;
    function GetBinaryPathname: string;
    function WaitForPendingServiceState(const AServiceState: TServiceState): Boolean;
    procedure SetState(const AServiceState: TServiceState);
    function GetServiceAccepts: TServiceAccepts;
    procedure SetStartType(const AValue: TServiceStartup);
  protected
    { Cleanup the handle created with @link(GetHandle). }
    procedure CleanupHandle;
    { Open a handle to the service with the given access rights.
      This handle can be deleted via @link(CleanupHandle). }
    function GetHandle(const AAccess: DWORD): Boolean;
    { Query the current status of this service }
    function Query: Boolean;
    { Wait for a given status of this service... }
    function WaitFor(const AState: DWORD): Boolean;
    { Fetch the configuration information }
    function QueryConfig: Boolean;
  public
    constructor Create(const AParentServiceManager: TServiceManager);
    destructor Destroy; override;

    { Get array of services that depent on this service, might return nil items, at least for now }
    // TODO: Handle nil item issue some way
    function Dependents: TArray<TServiceInfo>;
    { Action: Pause a running service. }
    function Pause(const AWait: Boolean = True): Boolean;
    { Action: Continue a paused service. }
    function Continue(const AWait: Boolean = True): Boolean;
    { Action: Stop a running service. }
    function Stop(const AWait: Boolean = True): Boolean;
    { Action: Start a not running service.
      You can use the @link(State) property to change the state from ssStopped to ssRunning }
    function Start(const AWait: Boolean = True): Boolean;
    { Name of this service. }
    property Name: string read FServiceName;
    { Display name of this service }
    property DisplayName: string read FDisplayName;
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
    { }
    property UserName: string read FUserName;
  end;

  { A service manager allows the services of a particular machine to be explored and modified. }
  TServiceManager = class(TObject)
  strict private
    FAllowLocking: Boolean;
    FLastErrorCode: Integer;
    FLastErrorMessage: string;
    FLastSystemErrorCode: DWord;
    FLastSystemErrorMessage: string;
    FLockHandle: SC_LOCK;
    FMachineName: string;
    FManagerHandle: SC_HANDLE;
    FRaiseExceptions: Boolean;
    FServicesByName: TDictionary<string, TServiceInfo>;
    FServicesList: TObjectList<TServiceInfo>;
    function GetActive: Boolean;
    function GetService(const AIndex: Integer): TServiceInfo;
    function GetServiceCount: Integer;
    procedure CleanupServices;
    procedure EnumerateAndAddServices(const AServices: PEnumServiceStatus; const AByesNeeded: DWORD);
    procedure ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
    procedure SetActive(const ASetToActive: Boolean);
    procedure SetAllowLocking(const AValue: Boolean);
    procedure SetMachineName(const AMachineName: string);
  protected
    { using classic protected visibility to give TServiceInfo access to TServiceManager services that nare not public }
    function GetManagerHandle: SC_HANDLE;
    function Lock: Boolean;
    function Unlock: Boolean;
    procedure HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
    procedure ResetLastError;
  public
    constructor Create;
    destructor Destroy; override;

    // Begin- and EndLockingProcess, so can easily do propcess between try..finally, which need locking
    procedure BeginLockingProcess(const AActivateServiceManager: Boolean = True);
    procedure EndLockingProcess;
    //
    function Open: Boolean;
    function Close: Boolean;
    { Requeries the states, names etc of all services on the given @link(MachineName).
      Works only while active. }
    function RebuildServicesList: Boolean;
    { Find services by name (case insensitive). Works only while active. If no service can be found
      an exception will be raised. }
    function ServiceByName(const AServiceName: string; const AAllowUnkown: Boolean = False): TServiceInfo;
    { Delete a service... }
    // procedure DeleteService(Index: Integer);
    { Get the number of services. This number is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. }
    property ServiceCount: Integer read GetServiceCount;
    { Find a servce by index in the services list. This list is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. Valid Index
      values are 0..@link(ServiceCount) - 1. }
    property Services[const AIndex: Integer]: TServiceInfo read GetService;
    { Activate / deactivate the service manager. In active state can you access the individual
      service, check RaiseExceptions property and open and close methods, thiose will affect on how this property
      works }
    property Active: Boolean read GetActive write SetActive;
    { The machine name for which you want the services list. }
    property MachineName: string read FMachineName write SetMachineName;
    { Allow locking... Is needed only when changing several properties in TServiceInfo.
      Property can only be set while inactive. }
    property AllowLocking: Boolean read FAllowLocking write SetAllowLocking;
    { Raise Exceptions, if all functions should return False if it fails, then more info at Last*Error* properties}
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions;
    // Error properties, check HandleError()
    property LastErrorCode: Integer read FLastErrorCode;
    property LastSystemErrorCode: DWord read FLastSystemErrorCode;
    property LastSystemErrorMessage: string read FLastSystemErrorMessage;
    property LastErrorMessage: string read FLastErrorMessage;
    { Get array of services, sorted by display name, Serrvice manager owns objects, so handle with care. }
    function GetServicesByDisplayName: TArray<TServiceInfo>;
  end;

  function ServiceStateToString(const AServiceState: TServiceState): string;

implementation

uses
  System.Generics.Defaults, System.SysConst;

function ServiceStateToString(const AServiceState: TServiceState): string;
begin
  // TODO: Might be better to use "Starting[...]" instead of "Start pending..."
  // TODO: Should make this easier to localize, if needed.
  case AServiceState of
    ssStopped: Result := 'Stopped';
    ssStartPending: Result := 'Start pending...';
    ssStopPending: Result := 'Stop pending...';
    ssRunning: Result := 'Running';
    ssContinuePending: Result := 'Continue pending...';
    ssPausePending: Result := 'Pause pending...';
    ssPaused: Result := 'Paused';
  end;
end;

{ TServiceManager }

function TServiceManager.RebuildServicesList: Boolean;
var
  LServices: PEnumServiceStatus;
  LBytesNeeded: DWORD;
  LServicesReturned: DWORD;
  LResumeHandle: DWORD;
begin
  Result := False;

  if not Active then
  begin
    HandleError(1);
    Exit;
  end;

  // Cleanup
  ResetLastError;
  CleanupServices;

  LServicesReturned := 0;
  LResumeHandle := 0;
  LServices := nil;

  // Get the amount of memory needed...
  if EnumServicesStatus(FManagerHandle, SERVICE_WIN32, SERVICE_STATE_ALL, LServices, 0, LBytesNeeded, LServicesReturned,
    LResumeHandle) then
    Exit;

  if GetLastError <> ERROR_MORE_DATA then
  begin
    HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end;

  // And... Get all the data...
  GetMem(LServices, LBytesNeeded); // will raise EOutOfMemory if fails
  try
    EnumerateAndAddServices(LServices, LBytesNeeded);
  finally
    FreeMem(LServices);
  end;

  Result := True;
end;

procedure TServiceManager.ResetLastError;
begin
  FLastErrorCode := 0;
  FLastSystemErrorCode := 0;
  FLastErrorMessage := '';
  FLastSystemErrorMessage := '';
end;

procedure TServiceManager.BeginLockingProcess(const AActivateServiceManager: Boolean = True);
begin
  AllowLocking := True;

  if not Active and AActivateServiceManager then
    Active := True;
end;

procedure TServiceManager.CleanupServices;
begin
  FServicesList.Clear;
  FServicesByName.Clear;
end;

function TServiceManager.Close: Boolean;
begin
  if not Active then
    Exit(True);

  Result := False;

  // CleanupServices
  ResetLastError;
  CleanupServices;
  // Close service manager
  if Assigned(FLockHandle) then
    if not Unlock then
      Exit;

  CloseServiceHandle(FManagerHandle);
  FManagerHandle := 0;

  Result := FManagerHandle = 0;
end;

constructor TServiceManager.Create;
begin
  inherited Create;

  FServicesList := TObjectList<TServiceInfo>.Create(True);
  FServicesByName := TDictionary<string, TServiceInfo>.Create;
  ResetLastError;
  FRaiseExceptions := True;
  FManagerHandle := 0;
end;

destructor TServiceManager.Destroy;
begin
  Active := False;

  FServicesList.Free;
  FServicesByName.Free;

  inherited Destroy;
end;

procedure TServiceManager.EndLockingProcess;
begin
  if Active then
    Active := False;

  AllowLocking := False;
end;

procedure TServiceManager.EnumerateAndAddServices(const AServices: PEnumServiceStatus; const AByesNeeded: DWORD);
var
  LIndex: DWORD;
  LServicesLoopPointer: PEnumServiceStatus;
  LServicesReturned: DWORD;
  LResumeHandle: DWORD;
  LBytesNeeded: DWORD;
begin
  LServicesReturned := 0;
  LResumeHandle := 0;
  LBytesNeeded := AByesNeeded;

  if not EnumServicesStatus(FManagerHandle, SERVICE_WIN32, SERVICE_STATE_ALL, AServices, LBytesNeeded, LBytesNeeded,
    LServicesReturned, LResumeHandle) then
    Exit;

  LServicesLoopPointer := AServices;
  LIndex := 0;
  while LIndex <= LServicesReturned - 1 do
  begin
    ServiceToLists(LServicesLoopPointer^);

    Inc(LServicesLoopPointer);
    Inc(LIndex);
  end;
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
  // Fetch the object of interest
  Result := FServicesList[AIndex];
end;

function TServiceManager.ServiceByName(const AServiceName: string; const AAllowUnkown: Boolean = False): TServiceInfo;
begin
  if not FServicesByName.TryGetValue(AServiceName.ToLower, Result) then
  begin
    Result := nil;

    if not AAllowUnkown then
      HandleError(2);
  end;
end;

function TServiceManager.GetServiceCount: Integer;
begin
  Result := FServicesList.Count;
end;

function TServiceManager.GetServicesByDisplayName: TArray<TServiceInfo>;
begin
  Result := FServicesList.ToArray;

  TArray.Sort<TServiceInfo>(Result, TDelegatedComparer<TServiceInfo>.Construct(
    function(const ALeft, ARight:TServiceInfo): Integer
    begin
      Result := TComparer<string>.Default.Compare(ALeft.DisplayName, ARight.DisplayName);
    end)
  );
end;

procedure TServiceManager.HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
var
  LErrorInfo: TErrorInfo;
  LOSError: EOSError;
begin
  FLastErrorCode := AErrorCode;

  if FLastErrorCode = RAISE_LAST_OS_ERROR then
  begin
    FLastSystemErrorCode := GetLastError;
    if FLastSystemErrorCode <> 0 then
      FLastSystemErrorMessage := SysErrorMessage(FLastSystemErrorCode)
    else
      FLastSystemErrorMessage := SUnkOSError;

    if FRaiseExceptions or AForceException then
    begin
      if FLastSystemErrorCode <> 0 then
        LOSError := EOSError.CreateResFmt(@SOSError, [FLastSystemErrorCode, FLastSystemErrorMessage, ''])
      else
        LOSError := EOSError.CreateRes(@SUnkOSError);

      LOSError.ErrorCode := FLastSystemErrorCode;

      raise LOSError at ReturnAddress;
    end;
  end
  else
  begin
    LErrorInfo := ErrorInfoArray[AErrorCode - 1];

    FLastErrorMessage := LErrorInfo.ErrorMessage;

    if FRaiseExceptions or AForceException then
      raise LErrorInfo.ExceptionClass.Create(FLastErrorMessage) at ReturnAddress;
  end;
end;

procedure TServiceManager.ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
var
  LSereviceInfo: TServiceInfo;
begin
  LSereviceInfo := TServiceInfo.Create(Self);
  LSereviceInfo.FServiceName := AServiceEnumStatus.lpServiceName;
  LSereviceInfo.FDisplayName := AServiceEnumStatus.lpDisplayName;
  LSereviceInfo.FServiceStatus := AServiceEnumStatus.ServiceStatus;

  LSereviceInfo.FIndex := FServicesList.Add(LSereviceInfo);;
  FServicesByName.Add(LSereviceInfo.FServiceName.ToLower, LSereviceInfo);
end;

procedure TServiceManager.SetActive(const ASetToActive: Boolean);
begin
  if ASetToActive then
    Open
  else
    Close;
end;

procedure TServiceManager.SetMachineName(const AMachineName: string);
begin
  if Active then
  begin
    HandleError(4, True);
    Exit;
  end;

  FMachineName := AMachineName;
end;

(*
procedure TServiceManager.DeleteService(Index: Integer);
begin
  // todo: implementation
  raise Exception.Create('Not implemented');
end;
*)

function TServiceManager.Lock: Boolean;
begin
  if not FAllowLocking then
  begin
    HandleError(5);
    Exit(False);
  end;

  Result := False;
  ResetLastError;

  FLockHandle := LockServiceDatabase(FManagerHandle);

  if FLockHandle = nil then
  begin
    HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end
  else
    Result := True;
end;

function TServiceManager.Open: Boolean;
var
  LVersionInfo: TOSVersionInfo;
  LDesiredAccess: DWORD;
begin
  if Active then
    Exit(True);

  Result := False;

  ResetLastError;
  // Check that we are NT, 2000, XP or above...
  LVersionInfo.dwOSVersionInfoSize := sizeof(LVersionInfo);

  if not GetVersionEx(LVersionInfo) then
  begin
    HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end;

  if LVersionInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT then
  begin
    HandleError(3);
    Exit;
  end;

  // Open service manager
  LDesiredAccess := SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE;
  if FAllowLocking then
    Inc(LDesiredAccess, SC_MANAGER_LOCK);

  FManagerHandle := OpenSCManager(PChar(FMachineName), nil, LDesiredAccess);
  if not Active then
  begin
    HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end;

  // Fetch the srvices list
  Result := RebuildServicesList;
end;

function TServiceManager.Unlock: Boolean;
begin
  // We are unlocked already
  if FLockHandle = nil then
    Exit(True);

  Result := False;
  ResetLastError;
  // Unlock...
  if not UnlockServiceDatabase(FLockHandle) then
  begin
    HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end;

  FLockHandle := nil;
  Result := FLockHandle = nil;
end;

procedure TServiceManager.SetAllowLocking(const AValue: Boolean);
begin
  if Active then
  begin
    HandleError(6, True);
    Exit;
  end;

  FAllowLocking := AValue;
end;

{ TServiceInfo }

procedure TServiceInfo.CleanupHandle;
begin
  if FServiceHandle = 0 then
    Exit;

  CloseServiceHandle(FServiceHandle);
  FServiceHandle := 0;
end;

constructor TServiceInfo.Create(const AParentServiceManager: TServiceManager);
begin
  inherited Create;

  FServiceManager := AParentServiceManager;

  FConfigQueried := False;
  FServiceHandle := 0;
  FLive := False;
end;

function TServiceInfo.DependenciesToList(const AQServicesStatus: PEnumServiceStatus; const AServiceInfoCount: Integer): TArray<TServiceInfo>;
var
  LServiceName: string;
  LIndex: Integer;
  LLoopStatusPointer: PEnumServiceStatus;
  LServiceInfo: TServiceInfo;
begin
  SetLength(Result, AServiceInfoCount);

  LLoopStatusPointer := AQServicesStatus;

  for LIndex := 0 to High(Result) do
  begin
    LServiceName := LLoopStatusPointer^.lpServiceName;

    // TODO: Here we have weird issue, asking for Windwos audio dependencies, we get dirrent name (AarSvc) than
    //       than expected AudioEndpointBuilder for Windows Audio Endpoint Builder, hence True parameter
    //       it is about, Agent Activation Runtime (AarSvc) Service, maybe it is not true service some how, but possible
    //       that we have here service which is not in the list
    LServiceInfo := FServiceManager.ServiceByName(LServiceName, True);

    if Assigned(LServiceInfo) then
      Result[LIndex] := LServiceInfo;

    Inc(LLoopStatusPointer);
  end;
end;

function TServiceInfo.Dependents: TArray<TServiceInfo>;
var
  LServicesStatus: PEnumServiceStatus;
  LBytesNeeded: DWORD;
  LServicesReturned: DWORD;
begin
  Result := [];

  if GetHandle(SERVICE_ENUMERATE_DEPENDENTS) then
  try
    // See how many dependents we have...
    LServicesStatus := nil;
    LBytesNeeded := 0;
    LServicesReturned := 0;

    if EnumDependentServices(FServiceHandle, SERVICE_ACTIVE + SERVICE_INACTIVE, LServicesStatus, 0, LBytesNeeded,
      LServicesReturned) then
      Exit;

    if GetLastError <> ERROR_MORE_DATA then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR, True);
      Exit;
    end;

    // Allocate the buffer needed and fetch all info...
    GetMem(LServicesStatus, LBytesNeeded);
    try
      if not EnumDependentServices(FServiceHandle, SERVICE_ACTIVE + SERVICE_INACTIVE, LServicesStatus, LBytesNeeded,
        LBytesNeeded, LServicesReturned) then
      begin
        FServiceManager.HandleError(RAISE_LAST_OS_ERROR, True);
        Exit;
      end;

      Result := DependenciesToList(LServicesStatus, LServicesReturned);
    finally
      FreeMem(LServicesStatus);
    end;
  finally
    CleanupHandle;
  end;
end;

destructor TServiceInfo.Destroy;
begin
  CleanupHandle;

  inherited Destroy;
end;

function TServiceInfo.GetHandle(const AAccess: DWORD): Boolean;
begin
  { TODO: here is a bug or missing sanity check, I think.

          Should check the AAccess, error if hadle allocated but Access
          is different than previously used. Code logic/structure most likely wrong.

          all operations should be nested bet ween GetHandle try..finally CleanupHandle }
  if FServiceHandle <> 0 then
    Exit(True);

  Result := False;

  FServiceManager.ResetLastError;

  FServiceHandle := OpenService(FServiceManager.GetManagerHandle, PChar(FServiceName), AAccess);

  if FServiceHandle = 0 then
  begin
    FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
    Exit;
  end
  else
    Result := True;
end;

function TServiceInfo.GetState: TServiceState;
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
      FServiceManager.HandleError(7, True);
      Result := ssStopped; // Make compiler happy
    end;
  end;
end;

function TServiceInfo.Query: Boolean;
var
  LStatus: TServiceStatus;
begin
  Result := False;
  FServiceManager.ResetLastError;

  if FServiceHandle <> 0 then
  begin
    if not QueryServiceStatus(FServiceHandle, LStatus) then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;
  end
  else
  begin
    if not GetHandle(SERVICE_QUERY_STATUS) then
      Exit;

    try
      if not QueryServiceStatus(FServiceHandle, LStatus) then
      begin
        FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
        Exit;
      end;
    finally
      CleanupHandle;
    end;
  end;

  FServiceStatus := LStatus;
  Result := True;
end;

function TServiceInfo.Continue(const AWait: Boolean = True): Boolean;
var
  LStatus: TServiceStatus;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_STATUS or SERVICE_PAUSE_CONTINUE) then
  try
    if not (saPauseContinue in ServiceAccepts) then
    begin
      FServiceManager.HandleError(8);
      Exit;
    end;

    if not ControlService(FServiceHandle, SERVICE_CONTROL_CONTINUE, LStatus) then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;

    if AWait then
      if not WaitFor(SERVICE_RUNNING) then
        Exit;

    Result := True;
  finally
    CleanupHandle;
  end;
end;

function TServiceInfo.Pause(const AWait: Boolean = True): Boolean;
var
  LStatus: TServiceStatus;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_STATUS or SERVICE_PAUSE_CONTINUE) then
  try
    if not (saPauseContinue in ServiceAccepts) then
    begin
      FServiceManager.HandleError(9);
      Exit;
    end;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_PAUSE, LStatus) then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;

    if AWait then
      if not WaitFor(SERVICE_PAUSED) then
        Exit;

    Result := True;
  finally
    CleanupHandle;
  end;
end;

function TServiceInfo.Start(const AWait: Boolean = True): Boolean;
var
  LServiceArgumentVectors: PCHar;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_STATUS or SERVICE_START) then
  try
    LServiceArgumentVectors := nil;
    if not StartService(FServiceHandle, 0, LServiceArgumentVectors) then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;

    if AWait then
      if not WaitFor(SERVICE_RUNNING) then
        Exit;

    Result := True;
  finally
    CleanupHandle;
  end;
end;

function TServiceInfo.Stop(const AWait: Boolean = True): Boolean;
var
  LStatus: TServiceStatus;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_STATUS or SERVICE_STOP) then
  try
    if not (saStop in ServiceAccepts) then
    begin
      FServiceManager.HandleError(10);
      Exit;
    end;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_STOP, LStatus) then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;

    if AWait then
      if not WaitFor(SERVICE_STOPPED) then
        Exit;

    Result := True;
  finally
    CleanupHandle;
  end;
end;

function TServiceInfo.WaitFor(const AState: DWORD): Boolean;
var
  LOldCheckPoint: DWORD;
  LWait: DWORD;
begin
  Result := Query;

  if Result then
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
      begin
        FServiceManager.HandleError(11);
        Exit(False);
      end;
    end;

  Result := AState = FServiceStatus.dwCurrentState;
end;

function TServiceInfo.WaitForPendingServiceState(const AServiceState: TServiceState): Boolean;
begin
  case AServiceState of
    ssStartPending: Result := WaitFor(SERVICE_RUNNING);
    ssStopPending: Result := WaitFor(SERVICE_STOPPED);
    ssContinuePending: Result := WaitFor(SERVICE_RUNNING);
    ssPausePending: Result := WaitFor(SERVICE_PAUSED);
    else
      Exit(True); // supress FixInsight warning
  end;
end;

function TServiceInfo.QueryConfig: Boolean;
var
  LBuffer: LPQUERY_SERVICE_CONFIG;
  LBytesNeeded: DWORD;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_CONFIG) then
  try
    // See how large our buffer must be...
    Assert(not QueryServiceConfig(FServiceHandle, nil, 0, LBytesNeeded), 'Could not get buffer size');

    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
    begin
      FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
      Exit;
    end;

    GetMem(LBuffer,LBytesNeeded);
    try
      // Perform the query...
      if not QueryServiceConfig(FServiceHandle,LBuffer,LBytesNeeded,LBytesNeeded) then
      begin
        FServiceManager.HandleError(RAISE_LAST_OS_ERROR);
        Exit;
      end;

      // Analyze the query...
      Assert(LBuffer^.dwServiceType and SERVICE_WIN32 <> 0); // It must be a WIN32 service
      FOwnProcess := (LBuffer^.dwServiceType and SERVICE_WIN32) = SERVICE_WIN32_OWN_PROCESS;
      FInteractive := (LBuffer^.dwServiceType and SERVICE_INTERACTIVE_PROCESS) = SERVICE_INTERACTIVE_PROCESS;

      case LBuffer^.dwStartType of
        SERVICE_AUTO_START:    FStartType := ssAutomatic;
        SERVICE_DEMAND_START:  FStartType := ssManual;
        SERVICE_DISABLED:      FStartType := ssDisabled;
        else
        begin
          FServiceManager.HandleError(12);
          Exit
        end;
      end;

      FBinaryPathName := LBuffer^.lpBinaryPathName;
      FUsername := LBuffer^.lpServiceStartName;
      FConfigQueried := True;

      Result := True;
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

  if not WaitForPendingServiceState(GetState) then
    FServiceManager.HandleError(11, True);

  LOldState := GetState;
  // See what we need to do...
  case AServiceState of
    ssStopped:
      if LOldState <> ssStopped then
        Stop(True);
    ssRunning:
      case LOldState of // FI:W535 Enumerated constant(s) missing in case statement
        ssStopped: Start(True);
        ssPaused:  Continue(True);
      end;
    ssPaused:
      case LOldState of // FI:W535 Enumerated constant(s) missing in case statement
        ssStopped:
          begin
            Start(True);
            try
              Pause(True); // some services do not support pause/continue!
            except
              Stop(True);
              raise;
            end;
          end;
        ssRunning: Pause(True);
      end;
    else
    begin
      FServiceManager.HandleError(13, True);
      Exit;
    end;
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
  if FServiceManager.Lock then
  try
    if GetHandle(SERVICE_CHANGE_CONFIG) then
    try
      // We locked the manager and are allowed to change the configuration...
      if not ChangeServiceConfig(FServiceHandle, SERVICE_NO_CHANGE, NEW_START_TYPES[AValue], SERVICE_NO_CHANGE,
        nil, nil, nil, nil, nil, nil, nil) then
      begin
        FServiceManager.HandleError(RAISE_LAST_OS_ERROR, True);
        Exit;
      end;

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
