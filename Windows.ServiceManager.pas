unit Windows.ServiceManager;

{ --------------------------------------------------------------------------- }
{                                                                             }
{ Written with                                                                }
{   - Delphi XE3 Pro                                                          }
{   - Refactored with 12.3                                                    }
{                                                                             }
{ Created Nov 24, 2012 by Darian Miller                                       }
{   - Additional work by                                                      }
{     - c-michail - https://github.com/c-michail                              }
{     - Tommi Prami                                                           }
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
  { Helpers }
  TDSMServiceStateHelper = record helper for TDSMServiceState
    function ToString: string;
  end;

  TDSMServiceStartupHelper = record helper for TDSMServiceStartup
    function ToString: string;
  end;

  { General service information record }
  TDSMServiceInfo = record
  private
    FStatus: TServiceStatus; { (Internal) Status of this service. }
    FDescription: string; { Description of this service. }
    FDisplayName: string; { Display name of this service }
    FFileName: string;
    FInteractive: Boolean; { Is the service capable of interacting with the desktop. Possible: The logon must the Local System Account. }
    FLive: Boolean; { Are various properties using live information or historic information. }
    FName: string; { Name of this service. }
    FOwnProcess: Boolean; { When service is running, does it run as a separate process (own process) or combined with other services under svchost. }
    FPath: string;
    FCommandLine: string;
    FBinaryPathName: string; { Path to the binary that implements the service. }
  public
    State: TDSMServiceState; { State of this service. }
    ServiceAccepts: TDSMServiceAccepts; { See what controls the service accepts. }
    StartType: TDSMServiceStartup;
    UserName: string; { User name of this service }
    property Description: string read FDescription; { Description of this service. }
    property DisplayName: string read FDisplayName; { Display name of this service }
    property FileName: string read FFileName;
    property Interactive: Boolean read FInteractive; { Is the service capable of interacting with the desktop. Possible: The logon must the Local System Account. }
    property Live: Boolean read FLive; { Are various properties using live information or historic information. }
    property Name: string read FName; { Name of this service. }
    property OwnProcess: Boolean read FOwnProcess; { When service is running, does it run as a separate process (own process) or combined with other services under svchost. }
    property Path: string read FPath;
    property CommandLine: string read FCommandLine;
    property BinaryPathName: string read FBinaryPathName; { Path to the binary that implements the service. }
  end;

  TDSMServiceManager = class;

  { Information of and controls a single Service. Can be accessed via @link(TDSMServiceManager). }
  TDSMService = class(TObject)
  strict private
    FConfigQueried: Boolean;
    FServiceHandle: SC_HANDLE;
    FServiceHandleAccess: DWORD;
    FServiceManager: TDSMServiceManager;
    function DependenciesToList(const AQServicesStatus: PEnumServiceStatus; const AServiceCount: Integer): TArray<TDSMService>;
    function GetBinaryPathname: string;
    function GetCommandLine: string;
    function GetFileName: string;
    function GetHandle(const AAccess: DWORD): Boolean;
    function GetInteractive: Boolean;
    function GetOwnProcess: Boolean;
    function GetPath: string;
    function GetServiceAccepts: TDSMServiceAccepts;
    function GetServiceStartType(const AServiceConfig: QUERY_SERVICE_CONFIG; var AStartType: TDSMServiceStartup): Boolean;
    function GetStartType: TDSMServiceStartup;
    function GetState: TDSMServiceState;
    function HandleOK: Boolean;
    function QueryConfig: Boolean;
    function QueryStatus: Boolean;
    function WaitFor(const AState: DWORD): Boolean;
    function WaitForPendingServiceState(const AServiceState: TDSMServiceState): Boolean;
    procedure CleanupHandle;
    procedure ParseBinaryPath;
    procedure SetDelayedAutoStart(const AValue: Boolean);
    procedure SetStartType(const AValue: TDSMServiceStartup);
    procedure SetState(const AServiceState: TDSMServiceState);
  private
    FInfo: TDSMServiceInfo;
    FIndex: Integer;
    function InternalServiceStateToState(const ACurrentState: DWORD): TDSMServiceState;
    function InitializeByName(const AServiceName: string): Boolean;
    procedure RefreshIfNeeded;
  protected
  public
    constructor Create(const AParentServiceManager: TDSMServiceManager);
    destructor Destroy; override;

    { Get array of services that depent on this service }
    function Dependents: TArray<TDSMService>;
    { Action: Pause a running service. }
    function Pause(const AWait: Boolean = True): Boolean;
    { Action: Continue a paused service. }
    function Continue(const AWait: Boolean = True): Boolean;
    { Action: Stop a running service. }
    function Stop(const AWait: Boolean = True): Boolean;
    { Action: Start a not running service.
      You can use the @link(State) property to change the state from ssStopped to ssRunning }
    function Start(const AWait: Boolean = True): Boolean;
    { The current state of the service. You can set the service only to the non-transitional states.
      You can restart the service by first setting the State to first ssStopped and second ssRunning. }
    property State: TDSMServiceState read GetState write SetState;
    { How is this service started. See @link(TDSMServiceStartup) for a description of startup types.
      If you want to set this property, the manager must be activeted with AllowLocking set to True. }
    property StartType: TDSMServiceStartup read GetStartType write SetStartType;
    { See what controls the service accepts. }
    property ServiceAccepts: TDSMServiceAccepts read GetServiceAccepts;
    { Index in ServiceManagers list }
    property Index: Integer read FIndex write FIndex;
    { General service information property }
    property Info: TDSMServiceInfo read FInfo;
  end;

  { A service manager allows the services of a particular machine to be explored and modified. }
  TDSMServiceManager = class(TObject)
  strict private
    FAllowLocking: Boolean;
    FGetServiceListOnActive: Boolean;
    FLastErrorCode: Integer;
    FLastErrorMessage: string;
    FLastSystemErrorCode: DWord;
    FLastSystemErrorMessage: string;
    FLockHandle: SC_LOCK;
    FHostName: string;
    FManagerHandle: SC_HANDLE;
    FRaiseExceptions: Boolean;
    FServicesByName: TDictionary<string, TDSMService>;
    FServicesList: TObjectList<TDSMService>;
    function CheckOS: Boolean;
    function GetActive: Boolean;
    function GetService(const AIndex: Integer): TDSMService;
    function GetServiceCount: Integer;
    function InitializeSingleService(const AServiceName: string): TDSMService;
    procedure AddServiceToLists(const AService: TDSMService);
    procedure CleanupServices;
    procedure EnumerateAndAddServices(const AServices: PEnumServiceStatus; const AByesNeeded: DWORD);
    procedure ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
    procedure SetActive(const ASetToActive: Boolean);
    procedure SetAllowLocking(const AValue: Boolean);
    procedure SetHostName(const AHostName: string);
  private
    function GetError: Boolean;
    function GetErrorMessage: string;
  protected
    { using classic protected visibility to give TDSMService access to TDSMServiceManager services that are not public }
    function GetManagerHandle: SC_HANDLE;
    function Lock: Boolean;
    function Unlock: Boolean;
    procedure HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
    procedure ResetLastError;
    procedure SortArray(var AServiceArray: TArray<TDSMService>);
  public
    constructor Create(const AHostName: string = ''; const AGetServiceListOnActive: Boolean = True;
      const ARaiseExceptions: Boolean = True; const AAllowLocking: Boolean = False);
    destructor Destroy; override;

    // Begin- and EndLockingProcess, so can easily do propcess between try..finally, which need locking
    procedure BeginLockingProcess(const AActivateServiceManager: Boolean = True);
    procedure EndLockingProcess;
    //
    function Open: Boolean;
    function Close: Boolean;
    { Requeries the states, names etc of all services on the given @link(HostName).
      Works only while active. }
    function RebuildServicesList: Boolean;
    { Find services by name (case insensitive). Works only while active. If no service can be found
      an exception will be raised. }
    function ServiceByName(const AServiceName: string; const AAllowUnknown: Boolean = False): TDSMService;
    { Get array of services, sorted by display name, Service manager owns objects, so handle with care. }
    function GetServicesByDisplayName: TArray<TDSMService>;
    { Delete a service... }
    // procedure DeleteService(Index: Integer);
    { Get the number of services. This number is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. }
    property ServiceCount: Integer read GetServiceCount;
    { Find a service by index in the services list. This list is refreshed when the @link(Active) is
      set to True or @link(RebuildServicesList) is called. Works only while active. Valid Index
      values are 0..@link(ServiceCount) - 1. }
    property Services[const AIndex: Integer]: TDSMService read GetService;
    { Activate / deactivate the service manager. In active state can you access the individual
      service, check RaiseExceptions property and open and close methods, thiose will affect on how this property
      works }
    property Active: Boolean read GetActive write SetActive;
    { The machine name for which you want the services list. }
    property HostName: string read FHostName write SetHostName;
    { Allow locking... Is needed only when changing several properties in TDSMService.
      Property can only be set while inactive. }
    property AllowLocking: Boolean read FAllowLocking write SetAllowLocking;
    { Raise Exceptions, if all functions should return False if it fails, then more info at Last*Error* properties}
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions;
    // Error properties, check HandleError()
    property Error: Boolean read GetError;
    property ErrorMessage: string read GetErrorMessage;
    property LastErrorCode: Integer read FLastErrorCode;
    property LastSystemErrorCode: DWord read FLastSystemErrorCode;
    property LastSystemErrorMessage: string read FLastSystemErrorMessage;
    property LastErrorMessage: string read FLastErrorMessage;
    property GetServiceListOnActive: Boolean read FGetServiceListOnActive write FGetServiceListOnActive;
  end;

implementation

uses
  System.Generics.Defaults, System.SysConst, Windows.ServiceManager.Consts;

{ Helper for TDSMServiceState }

function TDSMServiceStateHelper.ToString: string;
begin
  Result := '';

  // TODO: Should make this easier to localize, if needed.
  case Self of
    ssStopped: Result := 'Stopped';
    ssStartPending: Result := 'Starting...';
    ssStopPending: Result := 'Stopping...';
    ssRunning: Result := 'Running';
    ssContinuePending: Result := 'Continuing...';
    ssPausePending: Result := 'Pausing...';
    ssPaused: Result := 'Paused';
    else
      Result := ''; // Make compiler happy
  end;
end;

{ Helper for TDSMServiceStartup }

function TDSMServiceStartupHelper.ToString: string;
begin
  case Self of
    ssAutomatic:Result := 'Automatic';
    ssManual: Result := 'Manual';
    ssDisabled: Result := 'Disabled';
    ssAutomaticDelayed: Result := 'Automatic (Delayed Start)';
    else
      Result := ''; // Make compiler happy
  end;
end;


{ TDSMServiceManager }

function TDSMServiceManager.RebuildServicesList: Boolean;
var
  LIndex: NativeUInt;
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
  begin
    { No nedd for error handling here, almost always fall for this.

      this is
        "if EnumServicesStatus()" NOT -> "if not EnumServicesStatus()" -case

      If needs nore memory, continue, if error is something ense, handled below.
    }
    Exit;
  end;

  if GetLastError <> ERROR_MORE_DATA then
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  GetMem(LServices, LBytesNeeded);
  try
    EnumerateAndAddServices(LServices, LBytesNeeded);
  finally
    FreeMem(LServices);
  end;

  // Update services info
  RaiseExceptions := False; // Silent exceptions to avoid breaking the loop on a bad service
  try
    for LIndex := 0 to GetServiceCount -1 do
      Services[LIndex].RefreshIfNeeded;
  finally
    RaiseExceptions := True; // Restore back raising
  end;

  Result := True;
end;

procedure TDSMServiceManager.ResetLastError;
begin
  FLastErrorCode := 0;
  FLastSystemErrorCode := 0;
  FLastErrorMessage := '';
  FLastSystemErrorMessage := '';
end;

procedure TDSMServiceManager.AddServiceToLists(const AService: TDSMService);
begin
  AService.FIndex := FServicesList.Add(AService);
  FServicesByName.Add(AService.Info.FName.ToLower, AService);
end;

procedure TDSMServiceManager.BeginLockingProcess(const AActivateServiceManager: Boolean = True);
begin
  AllowLocking := True;

  if not Active and AActivateServiceManager then
    Active := True;
end;

function TDSMServiceManager.CheckOS: Boolean;
var
  LVersionInfo: TOSVersionInfo;
begin
  Result := False;

  ResetLastError;

  // Check that we are NT, 2000, XP or above, hopefully Always...
  LVersionInfo.dwOSVersionInfoSize := SizeOf(LVersionInfo);

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

procedure TDSMServiceManager.CleanupServices;
begin
  FServicesList.Clear;
  FServicesByName.Clear;
end;

function TDSMServiceManager.Close: Boolean;
begin
  if not Active then
    Exit(True);

  Result := False;

  ResetLastError;
  CleanupServices;

  if Assigned(FLockHandle) then
    if not Unlock then
      Exit;

  CloseServiceHandle(FManagerHandle);
  FManagerHandle := 0;

  Result := not GetActive;
end;

constructor TDSMServiceManager.Create(const AHostName: string = ''; const AGetServiceListOnActive: Boolean = True;
  const ARaiseExceptions: Boolean = True; const AAllowLocking: Boolean = False);
begin
  inherited Create;

  FServicesList := TObjectList<TDSMService>.Create(True);
  FServicesByName := TDictionary<string, TDSMService>.Create;
  ResetLastError;
  FManagerHandle := 0;
  FHostName := AHostName;
  FRaiseExceptions := ARaiseExceptions;
  FGetServiceListOnActive := AGetServiceListOnActive;
  FAllowLocking := AAllowLocking;
end;

destructor TDSMServiceManager.Destroy;
begin
  Active := False;

  FServicesList.Free;
  FServicesByName.Free;

  inherited Destroy;
end;

procedure TDSMServiceManager.EndLockingProcess;
begin
  if Active then
    Active := False;

  AllowLocking := False;
end;

procedure TDSMServiceManager.EnumerateAndAddServices(const AServices: PEnumServiceStatus; const AByesNeeded: DWORD);
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
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  LServicesLoopPointer := AServices;
  LIndex := 0;
  while LIndex <= LServicesReturned - 1 do
  begin
    ServiceToLists(LServicesLoopPointer^);

    Inc(LServicesLoopPointer);
    Inc(LIndex);
  end;
end;

function TDSMServiceManager.GetActive: Boolean;
begin
  Result := FManagerHandle <> 0;
end;

function TDSMServiceManager.GetError: Boolean;
begin
  Result := (FLastErrorCode <> 0) or (FLastSystemErrorCode <> 0);
end;

function TDSMServiceManager.GetErrorMessage: string;
begin
  Result := '';

  if FLastErrorCode <> 0 then
    Result := Format('Error (%d) with message:', [FLastErrorCode, FLastErrorMessage])
  else if FLastSystemErrorCode <> 0 then
    Result := Format('System error (%d) with message:', [FLastSystemErrorCode, FLastSystemErrorMessage]);
end;

function TDSMServiceManager.GetManagerHandle: SC_HANDLE;
begin
  Result := FManagerHandle;
end;

function TDSMServiceManager.GetService(const AIndex: Integer): TDSMService;
begin
  Result := FServicesList[AIndex];
end;

function TDSMServiceManager.ServiceByName(const AServiceName: string; const AAllowUnknown: Boolean = False): TDSMService;
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
    end;

    if Assigned(Result) then
      AddServiceToLists(Result)
    else if not AAllowUnknown then
      HandleError(SERVICE_NOT_FOUND);
  end;
end;

function TDSMServiceManager.GetServiceCount: Integer;
begin
  Result := FServicesList.Count;
end;

function TDSMServiceManager.GetServicesByDisplayName: TArray<TDSMService>;
begin
  Result := FServicesList.ToArray;

  SortArray(Result);
end;

procedure TDSMServiceManager.HandleError(const AErrorCode: Integer; const AForceException: Boolean = False);
var
  LErrorInfo: TDSMErrorInfo;
  LOSError: EOSError;
begin
  if AErrorCode = LAST_OS_ERROR then
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
    FLastErrorCode := AErrorCode;
    LErrorInfo := ErrorInfoArray[AErrorCode - 1];

    FLastErrorMessage := LErrorInfo.ErrorMessage;

    if FRaiseExceptions or AForceException then
      raise LErrorInfo.ExceptionClass.Create(FLastErrorMessage) at ReturnAddress;
  end;
end;

function TDSMServiceManager.InitializeSingleService(const AServiceName: string): TDSMService;
begin
  Result := TDSMService.Create(Self);
  try
    if not Result.InitializeByName(AServiceName) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TDSMServiceManager.ServiceToLists(const AServiceEnumStatus:  ENUM_SERVICE_STATUS);
var
  LService: TDSMService;
begin
  LService := TDSMService.Create(Self);
  LService.FInfo.FName := AServiceEnumStatus.lpServiceName;
  LService.FInfo.FDisplayName := AServiceEnumStatus.lpDisplayName;
  LService.FInfo.FStatus := AServiceEnumStatus.ServiceStatus;

  //Get current state of service
  LService.FInfo.State := LService.InternalServiceStateToState(LService.FInfo.FStatus.dwCurrentState);

  AddServiceToLists(LService);
end;

procedure TDSMServiceManager.SetActive(const ASetToActive: Boolean);
begin
  if ASetToActive then
    Open
  else
    Close;
end;

procedure TDSMServiceManager.SetHostName(const AHostName: string);
begin
  if Active then
  begin
    HandleError(IS_ACTIVE);
    Exit;
  end;

  FHostName := AHostName;
end;

procedure TDSMServiceManager.SortArray(var AServiceArray: TArray<TDSMService>);
begin
  TArray.Sort<TDSMService>(AServiceArray, TDelegatedComparer<TDSMService>.Construct(
    function(const ALeft, ARight:TDSMService): Integer
    begin
      Result := TComparer<string>.Default.Compare(ALeft.FInfo.FDisplayName, ARight.FInfo.FDisplayName);
    end)
  );
end;

(*
procedure TDSMServiceManager.DeleteService(Index: Integer);
begin
  // todo: implementation
  raise Exception.Create('Not implemented');
end;
*)

function TDSMServiceManager.Lock: Boolean;
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

function TDSMServiceManager.Open: Boolean;
var
  LDesiredAccess: DWORD;
begin
  if Active then
    Exit(True);

  Result := False;

  if not CheckOS then
    Exit;

  // Open service manager
  LDesiredAccess := SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE;
  if FAllowLocking then
    Inc(LDesiredAccess, SC_MANAGER_LOCK);

  FManagerHandle := OpenSCManager(PChar(FHostName), nil, LDesiredAccess);
  if not Active then
  begin
    HandleError(LAST_OS_ERROR);
    Exit;
  end;

  // Fetch the services list
  Result :=  GetActive;
  if Result and FGetServiceListOnActive then
    Result := RebuildServicesList;
end;

function TDSMServiceManager.Unlock: Boolean;
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

procedure TDSMServiceManager.SetAllowLocking(const AValue: Boolean);
begin
  if Active then
  begin
    HandleError(OPERATION_NOT_ALLOWED_WHILE_ACTIVE);
    Exit;
  end;

  FAllowLocking := AValue;
end;

{ TDSMService }

procedure TDSMService.CleanupHandle;
begin
  if FServiceHandle = 0 then
    Exit;

  CloseServiceHandle(FServiceHandle);
  FServiceHandle := 0;
  FServiceHandleAccess := 0;
end;

constructor TDSMService.Create(const AParentServiceManager: TDSMServiceManager);
begin
  inherited Create;

  FServiceManager := AParentServiceManager;

  FConfigQueried := False;
  FServiceHandle := 0;
  FServiceHandleAccess := 0;
  FInfo.FLive := False;
end;

function TDSMService.DependenciesToList(const AQServicesStatus: PEnumServiceStatus; const AServiceCount: Integer): TArray<TDSMService>;
var
  LServiceName: string;
  LIndex: Integer;
  LLoopStatusPointer: PEnumServiceStatus;
  LServiceInfo: TDSMService;
  LDependentSerevices: TList<TDSMService>;
begin
  Result := [];

  LDependentSerevices := TList<TDSMService>.Create;
  try
    LLoopStatusPointer := AQServicesStatus;

    LIndex := 0;
    while LIndex <= AServiceCount - 1 do
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

function TDSMService.Dependents: TArray<TDSMService>;
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

destructor TDSMService.Destroy;
begin
  CleanupHandle;

  inherited Destroy;
end;

function TDSMService.GetHandle(const AAccess: DWORD): Boolean;
begin
  if HandleOK then
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

  FServiceHandle := OpenService(FServiceManager.GetManagerHandle, PChar(FInfo.FName), AAccess);

  Result := HandleOK;
  if not Result then
  begin
    FServiceManager.HandleError(LAST_OS_ERROR);
    Exit;
  end
  else
    FServiceHandleAccess := AAccess;
end;

function TDSMService.GetState: TDSMServiceState;
begin
  if FInfo.FLive then
    QueryStatus;

  Result := InternalServiceStateToState(FInfo.FStatus.dwCurrentState);
end;

function TDSMService.HandleOK: Boolean;
begin
  Result := FServiceHandle <> 0;
end;

function TDSMService.InitializeByName(const AServiceName: string): Boolean;
begin
  FInfo.FName := AServiceName;

  Result := QueryConfig;

  if Result then
    Result := QueryStatus;
end;

function TDSMService.InternalServiceStateToState(const ACurrentState: DWORD): TDSMServiceState;
begin
  case ACurrentState of
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

function TDSMService.QueryStatus: Boolean;
var
  LStatus: TServiceStatus;
begin
  Result := False;
  FServiceManager.ResetLastError;

  if HandleOK then
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

  FInfo.FStatus := LStatus;
  Result := True;
end;

function TDSMService.Continue(const AWait: Boolean = True): Boolean;
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

procedure TDSMService.ParseBinaryPath;
var
  LCommanlineStart: Integer;
begin
  FInfo.FPath := '';
  FInfo.FFileName := '';
  FInfo.FCommandLine := '';

  if FInfo.FBinaryPathName <> '' then
  begin
    LCommanlineStart := FInfo.FBinaryPathName.IndexOf('" ');
    if LCommanlineStart < 0 then
      LCommanlineStart := FInfo.FBinaryPathName.IndexOf(' ');

    if LCommanlineStart > 0 then
    begin
      FInfo.FCommandLine := FInfo.FBinaryPathName.Substring(LCommanlineStart + 2);
      FInfo.FFileName := FInfo.FBinaryPathName.Substring(0, LCommanlineStart + 1);
    end
    else
      FInfo.FFileName := FInfo.FBinaryPathName;

    FInfo.FFileName := FInfo.FFileName.DeQuotedString('"');

    FInfo.FPath := ExtractFilePath(FInfo.FFileName);
    FInfo.FFileName := ExtractFileName(FInfo.FFileName);
  end;
end;

function TDSMService.Pause(const AWait: Boolean = True): Boolean;
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

    if not ControlService(FServiceHandle, SERVICE_CONTROL_PAUSE, LStatus) then
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

function TDSMService.Start(const AWait: Boolean = True): Boolean;
var
  LServiceArgumentVectors: PChar;
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

function TDSMService.Stop(const AWait: Boolean = True): Boolean;
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

    if not ControlService(FServiceHandle, SERVICE_CONTROL_STOP, LStatus) then
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

function TDSMService.WaitFor(const AState: DWORD): Boolean;
var
  LOldCheckPoint: DWORD;
  LWait: DWORD;
begin
  Result := QueryStatus;

  if Result then
    while AState <> FInfo.FStatus.dwCurrentState do
    begin
      LOldCheckPoint := FInfo.FStatus.dwCheckPoint;
      LWait := FInfo.FStatus.dwWaitHint;

      if LWait <= 0 then
        LWait := 5000;

      Sleep(LWait);

      QueryStatus;

      if AState = FInfo.FStatus.dwCurrentState then
        Break
      else if FInfo.FStatus.dwCheckPoint <> LOldCheckPoint then
      begin
        FServiceManager.HandleError(SERVICE_TIMEOUT);
        Exit(False);
      end;
    end;

  Result := AState = FInfo.FStatus.dwCurrentState;
end;

function TDSMService.WaitForPendingServiceState(const AServiceState: TDSMServiceState): Boolean;
begin
  case AServiceState of
    ssStartPending: Result := WaitFor(SERVICE_RUNNING);
    ssStopPending: Result := WaitFor(SERVICE_STOPPED);
    ssContinuePending: Result := WaitFor(SERVICE_RUNNING);
    ssPausePending: Result := WaitFor(SERVICE_PAUSED);
    else
      Exit(True); // suppress FixInsight warning
  end;
end;

function TDSMService.QueryConfig: Boolean;
var
  LBuffer: PByte;
  LServiceConfig: LPQUERY_SERVICE_CONFIG;
  LBytesNeeded: DWORD;
begin
  Result := False;
  LBytesNeeded := 0;

  if GetHandle(SERVICE_QUERY_CONFIG) then
  try
    // See how large our buffer must be...
    if QueryServiceConfig(FServiceHandle, nil, 0, LBytesNeeded) then
    begin
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      begin
        FServiceManager.HandleError(LAST_OS_ERROR);
        Exit;
      end;
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
      FInfo.FOwnProcess := (LServiceConfig.dwServiceType and SERVICE_WIN32) = SERVICE_WIN32_OWN_PROCESS;
      FInfo.FInteractive := (LServiceConfig.dwServiceType and SERVICE_INTERACTIVE_PROCESS) = SERVICE_INTERACTIVE_PROCESS;

      if not GetServiceStartType(LServiceConfig^, FInfo.StartType) then
        Exit;

      FInfo.FBinaryPathName := LServiceConfig.lpBinaryPathName;
      ParseBinaryPath;

      FInfo.Username := LServiceConfig^.lpServiceStartName;

      if FInfo.FDisplayName = '' then
        FInfo.FDisplayName := LServiceConfig^.lpDisplayName;

      FConfigQueried := True;

      Result := True;
    finally
      FreeMem(LServiceConfig);
    end;

    // Get DelayedAutoStart state
    QueryServiceConfig2(FServiceHandle, SERVICE_CONFIG_DELAYED_AUTO_START_INFO, nil, 0, @LBytesNeeded);
    GetMem(LBuffer, LBytesNeeded);
    try
      if QueryServiceConfig2(FServiceHandle, SERVICE_CONFIG_DELAYED_AUTO_START_INFO, LBuffer, LBytesNeeded, @LBytesNeeded) then
        if LPSERVICE_DELAYED_AUTO_START_INFO(LBuffer).fDelayedAutostart then
          FInfo.StartType := ssAutomaticDelayed;
    finally
      FreeMem(LBuffer);
    end;

    // Get Description
    QueryServiceConfig2(FServiceHandle, SERVICE_CONFIG_DESCRIPTION, nil, 0, @LBytesNeeded); // Get Buffer Length
    GetMem(LBuffer, LBytesNeeded);
    try
    if QueryServiceConfig2(FServiceHandle, SERVICE_CONFIG_DESCRIPTION, LBuffer, LBytesNeeded, @LBytesNeeded) then
      FInfo.FDescription := LPSERVICE_DESCRIPTION(LBuffer)^.lpDescription;
    finally
      FreeMem(LBuffer);
    end;
  finally
    CleanupHandle;
  end;
end;

procedure TDSMService.RefreshIfNeeded;
begin
  if FInfo.FLive or not FConfigQueried then
    QueryConfig;
end;

function TDSMService.GetOwnProcess: Boolean;
begin
  RefreshIfNeeded;

  Result := FInfo.FOwnProcess;
end;

function TDSMService.GetPath: string;
begin
  RefreshIfNeeded;

  Result := FInfo.FPath;
end;

function TDSMService.GetInteractive: Boolean;
begin
  RefreshIfNeeded;

  Result := FInfo.FInteractive;
end;

function TDSMService.GetStartType: TDSMServiceStartup;
begin
  RefreshIfNeeded;

  Result := FInfo.StartType;
end;

function TDSMService.GetBinaryPathName: string;
begin
  RefreshIfNeeded;

  Result := FInfo.FBinaryPathName;
end;

function TDSMService.GetCommandLine: string;
begin
  RefreshIfNeeded;

  Result := FInfo.FCommandLine;
end;

function TDSMService.GetFileName: string;
begin
  RefreshIfNeeded;

  Result := FInfo.FFileName;
end;

function TDSMService.GetServiceAccepts: TDSMServiceAccepts;
begin
  Result := [];

  if FInfo.FLive then
    QueryStatus;

  if FInfo.FStatus.dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE <> 0 then
    Result := Result + [saPauseContinue];

  if FInfo.FStatus.dwControlsAccepted and SERVICE_ACCEPT_STOP <> 0 then
    Result := Result + [saStop];

  if FInfo.FStatus.dwControlsAccepted and SERVICE_ACCEPT_SHUTDOWN <> 0 then
    Result := Result + [saShutdown];
end;

function TDSMService.GetServiceStartType(const AServiceConfig: QUERY_SERVICE_CONFIG; var AStartType: TDSMServiceStartup): Boolean;
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

procedure TDSMService.SetState(const AServiceState: TDSMServiceState);
var
  LOldState: TDSMServiceState;
begin
  // Make sure we have the latest current state and that it is not a transitional state.
  if not FInfo.FLive then
    QueryStatus;

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


procedure TDSMService.SetStartType(const AValue: TDSMServiceStartup);
const
  SERVICE_AUTO_START_DELAYED = 0; // dummy
  NEW_START_TYPES: array [TDSMServiceStartup] of DWORD = (SERVICE_AUTO_START, SERVICE_DEMAND_START, SERVICE_DISABLED, SERVICE_AUTO_START_DELAYED);
begin
  // Check if it is not a change?
  QueryConfig;

  if AValue = FInfo.StartType then
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
      if AValue = ssAutomaticDelayed then
      begin
        FInfo.StartType := ssAutomaticDelayed;
        SetDelayedAutoStart(True);
      end
      else
        FInfo.StartType := AValue;
    finally
      CleanupHandle;
    end;
  finally
    FServiceManager.Unlock;
  end;
end;

procedure TDSMService.SetDelayedAutoStart(const AValue: Boolean);
var
  DelayedInfo: SERVICE_DELAYED_AUTO_START_INFO;
begin
  // Check if it's not changed?
  QueryConfig;

  if AValue = (FInfo.StartType = ssAutomaticDelayed) then
    Exit;

  // Alter it...
  if FServiceManager.Lock then
  try
    if GetHandle(SERVICE_CHANGE_CONFIG) then
    try
      DelayedInfo.fDelayedAutostart := AValue;

      // We locked the manager and are allowed to change the configuration...
      if not ChangeServiceConfig2(FServiceHandle, SERVICE_CONFIG_DELAYED_AUTO_START_INFO, @DelayedInfo) then
      begin
        FServiceManager.HandleError(LAST_OS_ERROR);
        Exit;
      end;

      // well... we changed it, mark as such
      FInfo.StartType := ssAutomaticDelayed;
    finally
      CleanupHandle;
    end;
  finally
    FServiceManager.Unlock;
  end;
end;

end.
