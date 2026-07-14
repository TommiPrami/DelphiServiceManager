unit DWSMUnit.TestServiceUtils;

{ Install/uninstall helpers for the fake services used by the DelphiServiceManager unit tests.

  The services live in the TestUtils subfolders and are registered with their own /install and
  /uninstall command lines (standard Delphi TService behavior). All routines here use plain WinAPI
  on purpose, so the cleanup does not depend on the code under test.

  Intended lifecycle (see WindowsServiceManagerUnitTests.dpr):
    - PrepareTestServices: uninstall possible leftovers first (e.g. a test run killed in the
      debugger leaves the services registered), then install fresh copies.
    - CleanupTestServices: stop and uninstall everything, best effort. }

interface

type
  TTestServiceInfo = record
    ServiceName: string;
    ExeName: string;
    DisplayName: string;
  end;

const
  DEPENDENT_TEST_SERVICE_NAME = 'DSMTestDependent';
  FAST_TEST_SERVICE_NAME = 'DSMTestFast';
  PAUSABLE_TEST_SERVICE_NAME = 'DSMTestPausable';
  SLOW_TEST_SERVICE_NAME = 'DSMTestSlow';

  // Keep in sync with the timing constants in DSMTestSlowService.Main.pas
  SLOW_TEST_SERVICE_OPERATION_MS = 4 * 750;

  { Stopping and uninstalling iterate in array order, so dependent services must come before the
    services they depend on (DSMTestDependent depends on DSMTestFast). }
  TEST_SERVICES: array [0 .. 3] of TTestServiceInfo =
    (
      (ServiceName: DEPENDENT_TEST_SERVICE_NAME; ExeName: 'DSMTestDependentService.exe'; DisplayName: 'DSM Test Dependent Service'),
      (ServiceName: FAST_TEST_SERVICE_NAME; ExeName: 'DSMTestFastService.exe'; DisplayName: 'DSM Test Fast Service'),
      (ServiceName: PAUSABLE_TEST_SERVICE_NAME; ExeName: 'DSMTestPausableService.exe'; DisplayName: 'DSM Test Pausable Service'),
      (ServiceName: SLOW_TEST_SERVICE_NAME; ExeName: 'DSMTestSlowService.exe'; DisplayName: 'DSM Test Slow Service')
    );

function IsProcessElevated: Boolean;
function IsServiceInstalled(const AServiceName: string): Boolean;
function TestServiceBinDir: string;
function TestServiceExePath(const AExeName: string): string;
function TestServiceInfoByName(const AServiceName: string): TTestServiceInfo;
procedure CleanupTestServices;
procedure PrepareTestServices;
procedure SetServiceBinaryPath(const AServiceName, ABinaryPathName: string);
procedure StartTestServiceSilently(const AServiceName: string);
procedure StopTestServiceSilently(const AServiceName: string);

implementation

uses
  Winapi.Windows, Winapi.WinSvc, System.SysUtils;

const
  POLL_INTERVAL_MS = 250;
  PROCESS_WAIT_TIMEOUT_MS = 30000;
  SERVICE_WAIT_TIMEOUT_MS = 30000;

type
  ETestServiceSetupError = class(Exception);

procedure LogLine(const AMessage: string);
begin
  // Only meaningful for the console runner; under TestInsight there is no console
  if IsConsole then
    Writeln(AMessage);
end;

function IsProcessElevated: Boolean;
var
  LElevation: TTokenElevation;
  LReturnLength: DWORD;
  LToken: THandle;
begin
  Result := False;

  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, LToken) then
    Exit;
  try
    if GetTokenInformation(LToken, TokenElevation, @LElevation, SizeOf(LElevation), LReturnLength) then
      Result := LElevation.TokenIsElevated <> 0;
  finally
    CloseHandle(LToken);
  end;
end;

function TestServiceBinDir: string;
begin
  // Test exe lives in UnitTests\<Platform>\<Config>, the service exes are built to UnitTests\Bin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\Bin');
end;

function TestServiceExePath(const AExeName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(TestServiceBinDir) + AExeName;
end;

function TestServiceInfoByName(const AServiceName: string): TTestServiceInfo;
var
  LTestService: TTestServiceInfo;
begin
  for LTestService in TEST_SERVICES do
    if SameText(LTestService.ServiceName, AServiceName) then
      Exit(LTestService);

  raise ETestServiceSetupError.CreateFmt('Unknown test service "%s"', [AServiceName]);
end;

function IsServiceInstalled(const AServiceName: string): Boolean;
var
  LManager: SC_HANDLE;
  LService: SC_HANDLE;
begin
  Result := False;

  LManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if LManager = 0 then
    Exit;
  try
    LService := OpenService(LManager, PChar(AServiceName), SERVICE_QUERY_STATUS);

    if LService <> 0 then
    begin
      CloseServiceHandle(LService);

      Result := True;
    end;
  finally
    CloseServiceHandle(LManager);
  end;
end;

procedure SetServiceBinaryPath(const AServiceName, ABinaryPathName: string);
var
  LManager: SC_HANDLE;
  LService: SC_HANDLE;
begin
  LManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if LManager = 0 then
    RaiseLastOSError;
  try
    LService := OpenService(LManager, PChar(AServiceName), SERVICE_CHANGE_CONFIG);
    if LService = 0 then
      RaiseLastOSError;
    try
      if not ChangeServiceConfig(LService, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
        PChar(ABinaryPathName), nil, nil, nil, nil, nil, nil) then
        RaiseLastOSError;
    finally
      CloseServiceHandle(LService);
    end;
  finally
    CloseServiceHandle(LManager);
  end;
end;

procedure StartTestServiceSilently(const AServiceName: string);
var
  LDeadline: UInt64;
  LManager: SC_HANDLE;
  LService: SC_HANDLE;
  LServiceArgumentVectors: PChar;
  LStatus: TServiceStatus;
begin
  LManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if LManager = 0 then
    RaiseLastOSError;
  try
    LService := OpenService(LManager, PChar(AServiceName), SERVICE_START or SERVICE_QUERY_STATUS);
    if LService = 0 then
      RaiseLastOSError;
    try
      LServiceArgumentVectors := nil;
      if not StartService(LService, 0, LServiceArgumentVectors) then
        RaiseLastOSError;

      LDeadline := GetTickCount64 + SERVICE_WAIT_TIMEOUT_MS;

      while GetTickCount64 < LDeadline do
      begin
        if not QueryServiceStatus(LService, LStatus) then
          RaiseLastOSError;

        if LStatus.dwCurrentState = SERVICE_RUNNING then
          Exit;

        Sleep(POLL_INTERVAL_MS);
      end;

      raise ETestServiceSetupError.CreateFmt('Service "%s" did not start within %d ms',
        [AServiceName, SERVICE_WAIT_TIMEOUT_MS]);
    finally
      CloseServiceHandle(LService);
    end;
  finally
    CloseServiceHandle(LManager);
  end;
end;

procedure StopTestServiceSilently(const AServiceName: string);
var
  LDeadline: UInt64;
  LManager: SC_HANDLE;
  LService: SC_HANDLE;
  LStatus: TServiceStatus;
begin
  LManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if LManager = 0 then
    Exit;
  try
    LService := OpenService(LManager, PChar(AServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);
    if LService = 0 then
      Exit;
    try
      if not QueryServiceStatus(LService, LStatus) then
        Exit;

      if LStatus.dwCurrentState = SERVICE_STOPPED then
        Exit;

      // Ignore the result; if the stop is refused we still poll below in case the service is
      // already on its way down
      ControlService(LService, SERVICE_CONTROL_STOP, LStatus);

      LDeadline := GetTickCount64 + SERVICE_WAIT_TIMEOUT_MS;

      while GetTickCount64 < LDeadline do
      begin
        if not QueryServiceStatus(LService, LStatus) then
          Break;

        if LStatus.dwCurrentState = SERVICE_STOPPED then
          Break;

        Sleep(POLL_INTERVAL_MS);
      end;
    finally
      CloseServiceHandle(LService);
    end;
  finally
    CloseServiceHandle(LManager);
  end;
end;

procedure DeleteServiceSilently(const AServiceName: string);
var
  LManager: SC_HANDLE;
  LService: SC_HANDLE;
begin
  LManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if LManager = 0 then
    Exit;
  try
    LService := OpenService(LManager, PChar(AServiceName), _DELETE);
    if LService = 0 then
      Exit;
    try
      DeleteService(LService);
    finally
      CloseServiceHandle(LService);
    end;
  finally
    CloseServiceHandle(LManager);
  end;
end;

function RunProcessAndWait(const AExePath, AParameters: string): DWORD;
var
  LCommandLine: string;
  LProcessInfo: TProcessInformation;
  LStartupInfo: TStartupInfo;
begin
  FillChar(LStartupInfo, SizeOf(LStartupInfo), 0);
  LStartupInfo.cb := SizeOf(LStartupInfo);

  LCommandLine := '"' + AExePath + '" ' + AParameters;
  UniqueString(LCommandLine); // CreateProcessW may modify the command line buffer

  if not CreateProcess(nil, PChar(LCommandLine), nil, nil, False, CREATE_NO_WINDOW, nil,
    PChar(ExtractFilePath(AExePath)), LStartupInfo, LProcessInfo) then
    RaiseLastOSError;

  try
    if WaitForSingleObject(LProcessInfo.hProcess, PROCESS_WAIT_TIMEOUT_MS) <> WAIT_OBJECT_0 then
    begin
      TerminateProcess(LProcessInfo.hProcess, DWORD(-1));

      raise ETestServiceSetupError.CreateFmt('"%s %s" did not finish within %d ms',
        [AExePath, AParameters, PROCESS_WAIT_TIMEOUT_MS]);
    end;

    if not GetExitCodeProcess(LProcessInfo.hProcess, Result) then
      RaiseLastOSError;
  finally
    CloseHandle(LProcessInfo.hThread);
    CloseHandle(LProcessInfo.hProcess);
  end;
end;

procedure InstallTestServices;
var
  LExePath: string;
  LTestService: TTestServiceInfo;
begin
  if not IsProcessElevated then
    raise ETestServiceSetupError.Create('Installing the test services requires an elevated process. ' +
      'Run the tests as Administrator.');

  for LTestService in TEST_SERVICES do
  begin
    LExePath := TestServiceExePath(LTestService.ExeName);

    if not FileExists(LExePath) then
      raise ETestServiceSetupError.CreateFmt('Test service binary "%s" not found. ' +
        'Build the test service projects in UnitTests\TestUtils first.', [LExePath]);

    LogLine(Format('Installing test service "%s"...', [LTestService.ServiceName]));

    RunProcessAndWait(LExePath, '/install /silent');

    if not IsServiceInstalled(LTestService.ServiceName) then
      raise ETestServiceSetupError.CreateFmt('Installing test service "%s" failed.', [LTestService.ServiceName]);
  end;
end;

procedure UninstallTestServices;
var
  LExePath: string;
  LTestService: TTestServiceInfo;
begin
  for LTestService in TEST_SERVICES do
  begin
    if not IsServiceInstalled(LTestService.ServiceName) then
      Continue;

    LogLine(Format('Uninstalling test service "%s"...', [LTestService.ServiceName]));

    // A service marked for deletion only really goes away once it is stopped
    StopTestServiceSilently(LTestService.ServiceName);

    LExePath := TestServiceExePath(LTestService.ExeName);

    if FileExists(LExePath) then
    try
      RunProcessAndWait(LExePath, '/uninstall /silent');
    except
      // Fall through to the API based removal below
    end;

    if IsServiceInstalled(LTestService.ServiceName) then
      DeleteServiceSilently(LTestService.ServiceName);
  end;
end;

procedure PrepareTestServices;
begin
  { Uninstall first: if a test run was killed in the debugger, the services are left registered.
    Starting from a clean slate also makes sure we test against the freshly built binaries. }
  UninstallTestServices;
  InstallTestServices;
end;

procedure CleanupTestServices;
begin
  UninstallTestServices;
end;

end.
