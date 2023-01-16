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
    FBinaryPathName: string;
    FCommandLine: string;
    FConfigQueried: Boolean;
    FDisplayName: string;
    FFileName: string;
    FIndex: Integer;
    FInteractive: Boolean;
    FLive: Boolean;
    FOwnProcess: Boolean;
    FPath: string;
    FServiceHandle: SC_HANDLE;
    FServiceHandleAccess: DWORD;
    FServiceManager: TServiceManager;
    FServiceName: string;
    FServiceStatus: TServiceStatus;
    FStartType: TServiceStartup;
    FUserName: string;
    function DependenciesToList(const AQServicesStatus: PEnumServiceStatus; const AServiceInfoCount: Integer): TArray<TServiceInfo>;
    function GetServiceStartType(const AServiceConfig: QUERY_SERVICE_CONFIG; var AStartType: TServiceStartup): Boolean;
    function GetState: TServiceState;
    function GetOwnProcess: Boolean;
    function GetInteractive: Boolean;
    function GetStartType: TServiceStartup;
    function GetBinaryPathname: string;
    function WaitForPendingServiceState(const AServiceState: TServiceState): Boolean;
    procedure SetState(const AServiceState: TServiceState);
    function GetServiceAccepts: TServiceAccepts;
    procedure SetStartType(const AValue: TServiceStartup);
    procedure CleanupHandle;
    function GetHandle(const AAccess: DWORD): Boolean;
    procedure ParseBinaryPath;
    { Query the current status of this service }
    function Query: Boolean;
    { Wait for a given status of this service... }
    function WaitFor(const AState: DWORD): Boolean;
    { Fetch the configuration information }
    function QueryConfig: Boolean;
    function GetCommandLine: string;
    function GetFileName: string;
    function GetPath: string;
    procedure RefreshIfNeeded;
  protected
    function InitializeByName(const AServiceName: string): Boolean;
  public
    constructor Create(const AParentServiceManager: TServiceManager);
    destructor Destroy; override;

    { Get array of services that depent on this service }
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
    property Path: string read GetPath;
    property FileName: string read GetFileName;
    property CommandLine: string read GetCommandLine;
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
    FGetServiceListOnActive: Boolean;
    function CheckOS: Boolean;
    function GetActive: Boolean;
    function GetService(const AIndex: Integer): TServiceInfo;
    function GetServiceCount: Integer;
    procedure CleanupServices;
    procedure EnumerateAndAddServices(const AServices: PEnumServiceStatus; const AByesNeeded: DWORD);
    procedure AddServiceInfoToLists(const AServiceInfo: TServiceInfo);
    procedure ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
    procedure SetActive(const ASetToActive: Boolean);
    procedure SetAllowLocking(const AValue: Boolean);
    procedure SetMachineName(const AMachineName: string);
    function InitializeSingleService(const AServiceName: string): TServiceInfo;
  protected
    { using classic protected visibility to give TServiceInfo access to TServiceManager services that nare not public }
    function GetManagerHandle: SC_HANDLE;
    function Lock: Boolean;
    function Unlock: Boolean;
    procedure HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
    procedure ResetLastError;
    procedure SortArray(var AServiceInfoArray: TArray<TServiceInfo>);
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
    { Get array of services, sorted by display name, Serrvice manager owns objects, so handle with care. }
    function GetServicesByDisplayName: TArray<TServiceInfo>;
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
    property GetServiceListOnActive: Boolean read FGetServiceListOnActive write FGetServiceListOnActive;
  end;

  function ServiceStateToString(const AServiceState: TServiceState): string;

implementation

uses
  System.Generics.Defaults, System.SysConst, Windows.ServiceManager.Consts;

function ServiceStateToString(const AServiceState: TServiceState): string;
begin
  // TODO: Should make this easier to localize, if needed.
  case AServiceState of
    ssStopped: Result := 'Stopped';
    ssStartPending: Result := 'Starting...';
    ssStopPending: Result := 'Stopping...';
    ssRunning: Result := 'Running';
    ssContinuePending: Result := 'Continuing...';
    ssPausePending: Result := 'Pausing...';
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
    HandleError(SERVICELIST_NOT_ACTIVE);
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
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

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

procedure TServiceManager.AddServiceInfoToLists(const AServiceInfo: TServiceInfo);
begin
  AServiceInfo.FIndex := FServicesList.Add(AServiceInfo);;
  FServicesByName.Add(AServiceInfo.FServiceName.ToLower, AServiceInfo);
end;

procedure TServiceManager.BeginLockingProcess(const AActivateServiceManager: Boolean = True);
begin
  AllowLocking := True;

  if not Active and AActivateServiceManager then
    Active := True;
end;

function TServiceManager.CheckOS: Boolean;
var
  LVersionInfo: TOSVersionInfo;
begin
  Result := False;
  // Check that we are NT, 2000, XP or above...
  LVersionInfo.dwOSVersionInfoSize := sizeof(LVersionInfo);

  if not GetVersionEx(LVersionInfo) then
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  if LVersionInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT then
  begin
    HandleError(OS_NOT_CUPPOORTED);
    Exit;
  end;

  Result := True;
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
  FGetServiceListOnActive := True;
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
  Result := FServicesList[AIndex];
end;

function TServiceManager.ServiceByName(const AServiceName: string; const AAllowUnkown: Boolean = False): TServiceInfo;
begin
  if not FServicesByName.TryGetValue(AServiceName.ToLower, Result) then
  begin
    Result := nil;

    if not FGetServiceListOnActive  then
    begin
      if not Active then
      begin
        HandleError(SERVICELIST_NOT_ACTIVE);
        Exit;
      end;

      Result := InitializeSingleService(AServiceName);
      if Assigned(Result) then
        AddServiceInfoToLists(Result);
    end;


    if not AAllowUnkown and not Assigned(Result) then
      HandleError(SERVICE_NOT_FOUND);
  end;
end;

function TServiceManager.GetServiceCount: Integer;
begin
  Result := FServicesList.Count;
end;

function TServiceManager.GetServicesByDisplayName: TArray<TServiceInfo>;
begin
  Result := FServicesList.ToArray;

  SortArray(Result);
end;

procedure TServiceManager.HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
var
  LErrorInfo: TErrorInfo;
  LOSError: EOSError;
begin
  FLastErrorCode := AErrorCode;

  if FLastErrorCode = LAST_OS_ERROR then
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

function TServiceManager.InitializeSingleService(const AServiceName: string): TServiceInfo;
begin
  Result := TServiceInfo.Create(Self);
  try
    if not Result.InitializeByName(AServiceName) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TServiceManager.ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
var
  LServiceInfo: TServiceInfo;
begin
  LServiceInfo := TServiceInfo.Create(Self);
  LServiceInfo.FServiceName := AServiceEnumStatus.lpServiceName;
  LServiceInfo.FDisplayName := AServiceEnumStatus.lpDisplayName;
  LServiceInfo.FServiceStatus := AServiceEnumStatus.ServiceStatus;

  AddServiceInfoToLists(LServiceInfo);
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
    HandleError(IS_ACTIVE);
    Exit;
  end;

  FMachineName := AMachineName;
end;

procedure TServiceManager.SortArray(var AServiceInfoArray: TArray<TServiceInfo>);
begin
  TArray.Sort<TServiceInfo>(AServiceInfoArray, TDelegatedComparer<TServiceInfo>.Construct(
    function(const ALeft, ARight:TServiceInfo): Integer
    begin
      Result := TComparer<string>.Default.Compare(ALeft.DisplayName, ARight.DisplayName);
    end)
  );
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
  Result := False;

  if not FAllowLocking then
  begin
    HandleError(LOCKING_NOT_ALLOWED);
    Exit;
  end;

  ResetLastError;

  FLockHandle := LockServiceDatabase(FManagerHandle);

  if FLockHandle = nil then
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end
  else
    Result := True;
end;

function TServiceManager.Open: Boolean;
var
  LDesiredAccess: DWORD;
begin
  if Active then
    Exit(True);

  Result := False;

  ResetLastError;
  if not CheckOS then
    Exit;

  // Open service manager
  LDesiredAccess := SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE;
  if FAllowLocking then
    Inc(LDesiredAccess, SC_MANAGER_LOCK);

  FManagerHandle := OpenSCManager(PChar(FMachineName), nil, LDesiredAccess);
  if not Active then
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  // Fetch the srvices list
  if FGetServiceListOnActive then
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
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  FLockHandle := nil;
  Result := FLockHandle = nil;
end;

procedure TServiceManager.SetAllowLocking(const AValue: Boolean);
begin
  if Active then
  begin
    HandleError(OPERATION_NOT_ALLOWED_WHILE_ACTIVE);
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
  FServiceHandleAccess := 0;
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
  LDependentSerevices: TList<TServiceInfo>;
begin
  Result := [];

  // SetLength(Result, AServiceInfoCount);
  LDependentSerevices := TList<TServiceInfo>.Create;
  try
    LLoopStatusPointer := AQServicesStatus;

    LIndex := 0;
    while LIndex <= AServiceInfoCount - 1 do
    begin
      LServiceName := LLoopStatusPointer^.lpServiceName;

      { Here we have weird issue.

        Getting dependencies of "Windows audio" -service.
        we get dirrent name (AarSvc) than than expected AudioEndpointBuilder for the
        "Windows Audio Endpoint Builder" - service, hence True parameter for ServiceByName call.

        This is about, Agent Activation Runtime (AarSvc) Service, maybe it is not true service some how,
        but possible, did not dig up info. Services manager shows 3 dependencies, two of them is returned
        here as expected.

        So we need to have the True parameter at ServiceByName call, that there might be service name
        that could not be found. Until fixed, if even possible.
      }
      LServiceInfo := FServiceManager.ServiceByName(LServiceName, True);

      if Assigned(LServiceInfo) then
        LDependentSerevices.Add(LServiceInfo);

      Inc(LLoopStatusPointer);
      Inc(LIndex);
    end;

    Result := LDependentSerevices.ToArray;

    FServiceManager.SortArray(Result);
  finally
    LDependentSerevices.Free;
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
      FServiceManager.HandleError(LAST_OS_ERROR);
      Exit;
    end;

    // Allocate the buffer needed and fetch all info...
    GetMem(LServicesStatus, LBytesNeeded);
    try
      if not EnumDependentServices(FServiceHandle, SERVICE_ACTIVE + SERVICE_INACTIVE, LServicesStatus, LBytesNeeded,
        LBytesNeeded, LServicesReturned) then
      begin
        FServiceManager.HandleError(LAST_OS_ERROR);
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
  if FServiceHandle <> 0 then
  begin
    if AAccess = FServiceHandleAccess then
      Exit(True)
    else
    begin
      FServiceManager.HandleError(SERVICE_ACCESS_DIFFERS);
      Exit(False);
    end;
  end;

  FServiceManager.ResetLastError;

  FServiceHandle := OpenService(FServiceManager.GetManagerHandle, PChar(FServiceName), AAccess);

  Result := FServiceHandle > 0;
  if not Result then
  begin
    FServiceManager.HandleError(LAST_OS_ERROR);
    Exit;
  end
  else
    FServiceHandleAccess := AAccess;
end;

function TServiceInfo.GetState: TServiceState;
begin
  if FLive then
    Query;

  case FServiceStatus.dwCurrentState of
    SERVICE_STOPPED: Result := ssStopped;
    SERVICE_START_PENDING: Result := ssStartPending;
    SERVICE_STOP_PENDING: Result := ssStopPending;
    SERVICE_RUNNING: Result := ssRunning;
    SERVICE_CONTINUE_PENDING: Result := ssContinuePending;
    SERVICE_PAUSE_PENDING: Result := ssPausePending;
    SERVICE_PAUSED: Result := ssPaused;
    else
    begin
      FServiceManager.HandleError(SERVICE_STATE_UNKNOWN, True);
      Result := ssStopped; // Make compiler happy
    end;
  end;
end;

function TServiceInfo.InitializeByName(const AServiceName: string): Boolean;
begin
  FServiceName := AServiceName;

  Result := QueryConfig;
  if Result then
    Result := Query;
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
      FServiceManager.HandleError(LAST_OS_ERROR);
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
        FServiceManager.HandleError(LAST_OS_ERROR);
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
      FServiceManager.HandleError(SERVICE_CANNOT_CONTINUE);
      Exit;
    end;

    if not ControlService(FServiceHandle, SERVICE_CONTROL_CONTINUE, LStatus) then
    begin
      FServiceManager.HandleError(LAST_OS_ERROR);
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

procedure TServiceInfo.ParseBinaryPath;
var
  LCommanlineStart: Integer;
begin
  FPath := '';
  FFileName := '';
  FCommandLine := '';

  if FBinaryPathName <> '' then
  begin
    LCommanlineStart := FBinaryPathName.IndexOf('" ');
    if LCommanlineStart < 0 then
      LCommanlineStart := FBinaryPathName.IndexOf(' ');

    if LCommanlineStart > 0 then
    begin
      FCommandLine := FBinaryPathName.Substring(LCommanlineStart + 2);
      FFileName := FBinaryPathName.Substring(0, LCommanlineStart + 1);
    end
    else
      FFileName := FBinaryPathName;

    FFileName := FFileName.DeQuotedString('"');

    FPath := ExtractFilePath(FFileName);
    FFileName := ExtractFileName(FFileName);
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
      FServiceManager.HandleError(SERVICE_CANNOT_PAUSE);
      Exit;
    end;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_PAUSE, LStatus) then
    begin
      FServiceManager.HandleError(LAST_OS_ERROR);
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
      FServiceManager.HandleError(LAST_OS_ERROR);
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
      FServiceManager.HandleError(SERVICE_CANNOT_STOP);
      Exit;
    end;

    if not ControlService(FServiceHandle,SERVICE_CONTROL_STOP, LStatus) then
    begin
      FServiceManager.HandleError(LAST_OS_ERROR);
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
        Break
      else if FServiceStatus.dwCheckPoint <> LOldCheckPoint then
      begin
        FServiceManager.HandleError(SERVICE_TIMEOUT);
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
  LServiceConfig: LPQUERY_SERVICE_CONFIG;
  LBytesNeeded: DWORD;
begin
  Result := False;

  if GetHandle(SERVICE_QUERY_CONFIG) then
  try
    // See how large our buffer must be...
    Assert(not QueryServiceConfig(FServiceHandle, nil, 0, LBytesNeeded), 'Could not get buffer size');

    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
    begin
      FServiceManager.HandleError(LAST_OS_ERROR);
      Exit;
    end;

    GetMem(LServiceConfig, LBytesNeeded);
    try
      // Perform the query...
      if not QueryServiceConfig(FServiceHandle, LServiceConfig, LBytesNeeded, LBytesNeeded) then
      begin
        FServiceManager.HandleError(LAST_OS_ERROR);
        Exit;
      end;

      // Analyze the query...
      Assert(LServiceConfig^.dwServiceType and SERVICE_WIN32 <> 0); // It must be a WIN32 service
      FOwnProcess := (LServiceConfig^.dwServiceType and SERVICE_WIN32) = SERVICE_WIN32_OWN_PROCESS;
      FInteractive := (LServiceConfig^.dwServiceType and SERVICE_INTERACTIVE_PROCESS) = SERVICE_INTERACTIVE_PROCESS;

      if not GetServiceStartType(LServiceConfig^, FStartType) then
        Exit;

      FBinaryPathName := LServiceConfig^.lpBinaryPathName;
      ParseBinaryPath;

      FUsername := LServiceConfig^.lpServiceStartName;

      if FDisplayName = '' then
        FDisplayName := LServiceConfig^.lpDisplayName;

      FConfigQueried := True;

      Result := True;
    finally
      FreeMem(LServiceConfig);
    end;
  finally
    CleanupHandle;
  end;
end;

procedure TServiceInfo.RefreshIfNeeded;
begin
  if FLive or not FConfigQueried then
    QueryConfig;
end;

function TServiceInfo.GetOwnProcess: Boolean;
begin
  RefreshIfNeeded;

  Result := FOwnProcess;
end;

function TServiceInfo.GetPath: string;
begin
  RefreshIfNeeded;

  Result := FPath;
end;

function TServiceInfo.GetInteractive: Boolean;
begin
  RefreshIfNeeded;

  Result := FInteractive;
end;

function TServiceInfo.GetStartType: TServiceStartup;
begin
  RefreshIfNeeded;

  Result := FStartType;
end;

function TServiceInfo.GetBinaryPathName: string;
begin
  RefreshIfNeeded;

  Result := FBinaryPathName;
end;

function TServiceInfo.GetCommandLine: string;
begin
  RefreshIfNeeded;

  Result := FCommandLine;
end;

function TServiceInfo.GetFileName: string;
begin
  RefreshIfNeeded;

  Result := FFileName;
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



function TServiceInfo.GetServiceStartType(const AServiceConfig: QUERY_SERVICE_CONFIG; var AStartType: TServiceStartup): Boolean;
begin
  Result := True;

  case AServiceConfig.dwStartType of
    SERVICE_AUTO_START: AStartType := ssAutomatic;
    SERVICE_DEMAND_START: AStartType := ssManual;
    SERVICE_DISABLED: AStartType := ssDisabled;
    else
    begin
      FServiceManager.HandleError(SERVICE_STARTTYPE_UNKNOWN);
      Exit(False);
    end;
  end;
end;

procedure TServiceInfo.SetState(const AServiceState: TServiceState);
var
  LOldState: TServiceState;
begin
  // Make sure we have the latest current state and that it is not a transitional state.
  if not FLive then
    Query;

  if not WaitForPendingServiceState(GetState) then
    FServiceManager.HandleError(SERVICE_TIMEOUT);

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

              if FServiceManager.RaiseExceptions then
                raise;
            end;
          end;
        ssRunning: Pause(True);
      end;
    else
    begin
      FServiceManager.HandleError(SERVICE_CANNOT_SET_STATE, True);
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
        FServiceManager.HandleError(LAST_OS_ERROR);
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
