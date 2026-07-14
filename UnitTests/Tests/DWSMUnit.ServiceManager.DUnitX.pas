unit DWSMUnit.ServiceManager.DUnitX;

{ Unit tests for Windows.ServiceManager.

  The tests run against the fake services in UnitTests\TestUtils (DSMTestFast, DSMTestSlow,
  DSMTestPausable and DSMTestDependent, which depends on DSMTestFast). The services are installed
  before and uninstalled after the test run by the project file, see
  WindowsServiceManagerUnitTests.dpr and DWSMUnit.TestServiceUtils.pas.

  NOTE: The tests need an elevated process (installing and controlling services requires it),
        so run the test executable / IDE as Administrator. }

interface

uses
  DUnitX.TestFramework, Windows.ServiceManager, Windows.ServiceManager.Types;

type
  [TestFixture]
  THelperToStringTests = class
  public
    [Test]
    procedure StartupToStringCoversAllValues;
    [Test]
    procedure StateToStringCoversAllValues;
  end;

  [TestFixture]
  TServiceManagerBasicTests = class
  strict private
    FServiceManager: TDSMServiceManager;
    function ReadServiceInfo(const AServiceName: string): TDSMServiceInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ActiveManagerHasServices;
    [Test]
    procedure AllowLockingCannotBeChangedWhileActive;
    [Test]
    procedure CloseDeactivatesAndClearsList;
    [Test]
    procedure ConstructorParametersAreApplied;
    [Test]
    procedure ErrorInfoIsAvailableWhenNotRaisingExceptions;
    [Test]
    procedure ErrorMessageContainsCodeAndMessageText;
    [Test]
    procedure HostNameCanBeChangedWhileInactive;
    [Test]
    procedure HostNameCannotBeChangedWhileActive;
    [Test]
    procedure OpenAndCloseAreIdempotent;
    [Test]
    procedure ParsesQuotedBinaryPathWithSpaces;
    [Test]
    procedure RaiseExceptionsSettingSurvivesOpen;
    [Test]
    procedure RebuildServicesListRefreshesTheList;
    [Test]
    procedure RebuildServicesListWhileInactiveRaises;
    [Test]
    procedure ServiceByNameFindsTestService;
    [Test]
    procedure ServiceByNameIsCaseInsensitive;
    [Test]
    procedure ServiceByNameUnknownRaises;
    [Test]
    procedure ServiceByNameUnknownWithAllowUnknownReturnsNil;
    [Test]
    procedure ServiceByNameWhileInactiveRaises;
    [Test]
    procedure ServiceByNameWorksWithoutPrefetchedList;
    [Test]
    procedure ServiceIndexMatchesListPosition;
    [Test]
    procedure ServicesByDisplayNameAreSorted;
    [Test]
    procedure TestServiceInfoIsParsed;
    [Test]
    procedure WindowsServiceDescriptionIsRead;
  end;

  [TestFixture]
  TServiceControlTests = class
  strict private
    FServiceManager: TDSMServiceManager;
    function GetTestService(const AServiceName: string): TDSMService;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure DependentsListsTheDependentService;
    [Test]
    procedure DependentsReturnsEmptyArrayForTestService;
    [Test]
    procedure LiveServiceReflectsOutOfBandStateChanges;
    [Test]
    procedure PauseAndContinuePausableService;
    [Test]
    procedure PauseIsRefusedWhenServiceDoesNotAcceptIt;
    [Test]
    procedure ServiceAcceptsReflectRunningService;
    [Test]
    procedure SetStateHandlesPausedTransitions;
    [Test]
    procedure SetStateStartsAndStopsService;
    [Test]
    procedure SettingTransitionalStateRaises;
    [Test]
    procedure StartAndStopFastService;
    [Test]
    procedure StartAndStopSlowService;
    [Test]
    procedure StartingRunningServiceRaisesOSError;
    [Test]
    procedure StopIsRefusedWhenServiceIsStopped;
    [Test]
    procedure StopIsRefusedWhileDependentIsRunning;
  end;

  [TestFixture]
  TServiceStartTypeTests = class
  strict private
    FServiceManager: TDSMServiceManager;
    function GetTestService(const AServiceName: string): TDSMService;
    function ReadStartTypeFromFreshManager(const AServiceName: string): TDSMServiceStartup;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ChangingStartTypeRequiresLocking;
    [Test]
    procedure DelayedFlagIsClearedWhenSetBackToAutomatic;
    [Test]
    procedure DisabledServiceCannotBeStarted;
    [Test]
    procedure SetStartTypeToAutomatic;
    [Test]
    procedure SetStartTypeToAutomaticDelayed;
    [Test]
    procedure SetStartTypeToDisabled;
    [Test]
    procedure SetStartTypeToManual;
    [Test]
    procedure SettingSameStartTypeIsNoOp;
  end;

implementation

uses
  System.Diagnostics, System.Generics.Defaults, System.SysUtils, Windows.ServiceManager.Consts,
  DWSMUnit.TestServiceUtils;

const
  UNKNOWN_SERVICE_NAME = 'DSMTestServiceThatDoesNotExist';
  WINDOWS_EVENTLOG_SERVICE_NAME = 'EventLog';

{ THelperToStringTests }

procedure THelperToStringTests.StartupToStringCoversAllValues;
var
  LStartup: TDSMServiceStartup;
begin
  for LStartup := Low(TDSMServiceStartup) to High(TDSMServiceStartup) do
    Assert.IsTrue(LStartup.ToString <> '', 'Every startup type should have a text');

  Assert.AreEqual('Automatic', ssAutomatic.ToString);
  Assert.AreEqual('Automatic (Delayed Start)', ssAutomaticDelayed.ToString);
  Assert.AreEqual('Manual', ssManual.ToString);
  Assert.AreEqual('Disabled', ssDisabled.ToString);
end;

procedure THelperToStringTests.StateToStringCoversAllValues;
var
  LState: TDSMServiceState;
begin
  for LState := Low(TDSMServiceState) to High(TDSMServiceState) do
    Assert.IsTrue(LState.ToString <> '', 'Every service state should have a text');

  Assert.AreEqual('Stopped', ssStopped.ToString);
  Assert.AreEqual('Running', ssRunning.ToString);
  Assert.AreEqual('Paused', ssPaused.ToString);
end;

{ TServiceManagerBasicTests }

procedure TServiceManagerBasicTests.Setup;
begin
  FServiceManager := TDSMServiceManager.Create;
  FServiceManager.Active := True;
end;

procedure TServiceManagerBasicTests.TearDown;
begin
  FServiceManager.Free;
end;

function TServiceManagerBasicTests.ReadServiceInfo(const AServiceName: string): TDSMServiceInfo;
var
  LManager: TDSMServiceManager;
begin
  // A fresh manager queries the SCM from scratch, so the result is not affected by anything the
  // fixture manager has cached
  LManager := TDSMServiceManager.Create('', False);
  try
    LManager.Active := True;

    Result := LManager.ServiceByName(AServiceName).Info;
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ActiveManagerHasServices;
begin
  Assert.IsTrue(FServiceManager.Active, 'Manager should be active after Setup');
  Assert.IsTrue(FServiceManager.ServiceCount > 0, 'Active manager should have enumerated services');
end;

procedure TServiceManagerBasicTests.AllowLockingCannotBeChangedWhileActive;
begin
  Assert.WillRaise(
    procedure
    begin
      FServiceManager.AllowLocking := True;
    end,
    EOperationNotAllowedWhileActive);
end;

procedure TServiceManagerBasicTests.CloseDeactivatesAndClearsList;
begin
  Assert.IsTrue(FServiceManager.Close, 'Close should succeed');
  Assert.IsFalse(FServiceManager.Active, 'Manager should not be active after Close');
  Assert.AreEqual(0, FServiceManager.ServiceCount, 'Services list should be cleared on Close');
end;

procedure TServiceManagerBasicTests.ConstructorParametersAreApplied;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create('SOMEHOST', False, False, True);
  try
    Assert.AreEqual('SOMEHOST', LManager.HostName, 'HostName should come from the constructor');
    Assert.IsFalse(LManager.GetServiceListOnActive, 'GetServiceListOnActive should come from the constructor');
    Assert.IsFalse(LManager.RaiseExceptions, 'RaiseExceptions should come from the constructor');
    Assert.IsTrue(LManager.AllowLocking, 'AllowLocking should come from the constructor');
    Assert.IsFalse(LManager.Active, 'Manager should not activate itself on Create');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ErrorInfoIsAvailableWhenNotRaisingExceptions;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create('', True, False);
  try
    Assert.IsFalse(LManager.RebuildServicesList, 'RebuildServicesList should fail while inactive');
    Assert.IsTrue(LManager.Error, 'Error should be set');
    Assert.AreEqual(SERVICELIST_NOT_ACTIVE, LManager.LastErrorCode, 'LastErrorCode should be set');
    Assert.IsTrue(LManager.LastErrorMessage <> '', 'LastErrorMessage should be set');
    Assert.IsTrue(LManager.ErrorMessage <> '', 'ErrorMessage should be built');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ErrorMessageContainsCodeAndMessageText;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create('', True, False);
  try
    LManager.RebuildServicesList; // Fails, manager is not active

    Assert.IsTrue(LManager.ErrorMessage.Contains(IntToStr(LManager.LastErrorCode)),
      'ErrorMessage should include the error code');
    Assert.IsTrue(LManager.ErrorMessage.Contains(LManager.LastErrorMessage),
      'ErrorMessage should include the error text');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.HostNameCanBeChangedWhileInactive;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create;
  try
    LManager.HostName := 'SOMEHOST';

    Assert.AreEqual('SOMEHOST', LManager.HostName, 'HostName should be changeable while inactive');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.HostNameCannotBeChangedWhileActive;
begin
  Assert.WillRaise(
    procedure
    begin
      FServiceManager.HostName := 'SOMEHOST';
    end,
    EOperationNotAllowedWhileActive);
end;

procedure TServiceManagerBasicTests.OpenAndCloseAreIdempotent;
begin
  Assert.IsTrue(FServiceManager.Open, 'Open while already active should return True');
  Assert.IsTrue(FServiceManager.Active, 'Manager should stay active');

  Assert.IsTrue(FServiceManager.Close, 'Close should succeed');
  Assert.IsTrue(FServiceManager.Close, 'Close while already inactive should return True');
  Assert.IsFalse(FServiceManager.Active, 'Manager should stay inactive');
end;

procedure TServiceManagerBasicTests.ParsesQuotedBinaryPathWithSpaces;
const
  QUOTED_BINARY_PATH = '"C:\Program Files\DSM Test\Fake Service.exe" -param1 -param2';
  UNQUOTED_BINARY_PATH = 'C:\WINDOWS\system32\FakeService.exe -param1';
var
  LInfo: TDSMServiceInfo;
  LOriginalBinaryPath: string;
begin
  { ChangeServiceConfig does not validate the binary path, so we can point the test service at
    fake paths, check how they are parsed and restore the real path afterwards. }
  LOriginalBinaryPath := FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME).Info.BinaryPathName;

  SetServiceBinaryPath(FAST_TEST_SERVICE_NAME, QUOTED_BINARY_PATH);
  try
    LInfo := ReadServiceInfo(FAST_TEST_SERVICE_NAME);

    Assert.AreEqual('Fake Service.exe', LInfo.FileName, 'File name of a quoted path with spaces');
    Assert.AreEqual('C:\Program Files\DSM Test\', LInfo.Path, 'Path of a quoted path with spaces');
    Assert.AreEqual('-param1 -param2', LInfo.CommandLine, 'Command line of a quoted path with spaces');

    SetServiceBinaryPath(FAST_TEST_SERVICE_NAME, UNQUOTED_BINARY_PATH);

    LInfo := ReadServiceInfo(FAST_TEST_SERVICE_NAME);

    Assert.AreEqual('FakeService.exe', LInfo.FileName, 'File name of an unquoted path');
    Assert.AreEqual('C:\WINDOWS\system32\', LInfo.Path, 'Path of an unquoted path');
    Assert.AreEqual('-param1', LInfo.CommandLine, 'Command line of an unquoted path');
  finally
    SetServiceBinaryPath(FAST_TEST_SERVICE_NAME, LOriginalBinaryPath);
  end;
end;

procedure TServiceManagerBasicTests.RaiseExceptionsSettingSurvivesOpen;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create('', True, False);
  try
    LManager.Active := True;

    Assert.IsTrue(LManager.Active, 'Manager should be active');
    Assert.IsFalse(LManager.RaiseExceptions,
      'Open/RebuildServicesList must not clobber the RaiseExceptions setting');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.RebuildServicesListRefreshesTheList;
begin
  Assert.IsTrue(FServiceManager.RebuildServicesList, 'RebuildServicesList should succeed while active');
  Assert.IsTrue(FServiceManager.ServiceCount > 0, 'Rebuilt list should have services');
  Assert.IsNotNull(FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME),
    'Test service should be found after a rebuild');
end;

procedure TServiceManagerBasicTests.RebuildServicesListWhileInactiveRaises;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LManager.RebuildServicesList;
      end,
      ENotActive);
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ServiceByNameFindsTestService;
var
  LService: TDSMService;
begin
  LService := FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME);

  Assert.IsNotNull(LService, 'Test service should be found');
  Assert.AreEqual(FAST_TEST_SERVICE_NAME, LService.Info.Name, 'Service name should match');
end;

procedure TServiceManagerBasicTests.ServiceByNameIsCaseInsensitive;
var
  LService: TDSMService;
begin
  LService := FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME.ToUpper);

  Assert.IsNotNull(LService, 'Lookup should be case insensitive');
  Assert.AreSame(FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME), LService,
    'Both lookups should return the same instance');
end;

procedure TServiceManagerBasicTests.ServiceByNameUnknownRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FServiceManager.ServiceByName(UNKNOWN_SERVICE_NAME);
    end,
    EServiceNotFound);
end;

procedure TServiceManagerBasicTests.ServiceByNameUnknownWithAllowUnknownReturnsNil;
begin
  Assert.IsNull(FServiceManager.ServiceByName(UNKNOWN_SERVICE_NAME, True),
    'Unknown service with AAllowUnknown should return nil, not raise');
end;

procedure TServiceManagerBasicTests.ServiceByNameWhileInactiveRaises;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create('', False);
  try
    Assert.WillRaise(
      procedure
      begin
        LManager.ServiceByName(FAST_TEST_SERVICE_NAME);
      end,
      ENotActive);
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ServiceByNameWorksWithoutPrefetchedList;
var
  LManager: TDSMServiceManager;
  LService: TDSMService;
begin
  LManager := TDSMServiceManager.Create('', False);
  try
    LManager.Active := True;

    Assert.AreEqual(0, LManager.ServiceCount, 'No services should be listed before the first lookup');

    LService := LManager.ServiceByName(FAST_TEST_SERVICE_NAME);

    Assert.IsNotNull(LService, 'Single service initialization should find the test service');
    Assert.AreEqual(FAST_TEST_SERVICE_NAME, LService.Info.Name, 'Service name should match');
    Assert.AreEqual(1, LManager.ServiceCount, 'The initialized service should be added to the list');
  finally
    LManager.Free;
  end;
end;

procedure TServiceManagerBasicTests.ServiceIndexMatchesListPosition;
var
  LService: TDSMService;
begin
  LService := FServiceManager.ServiceByName(FAST_TEST_SERVICE_NAME);

  Assert.AreSame(FServiceManager.Services[LService.Index], LService,
    'Service Index should point back to the service in the list');
end;

procedure TServiceManagerBasicTests.ServicesByDisplayNameAreSorted;
var
  LComparer: IComparer<string>;
  LIndex: Integer;
  LServices: TArray<TDSMService>;
begin
  LServices := FServiceManager.GetServicesByDisplayName;

  Assert.AreEqual(FServiceManager.ServiceCount, Length(LServices), 'Array should contain all services');

  LComparer := TComparer<string>.Default;

  for LIndex := 1 to High(LServices) do
    Assert.IsTrue(LComparer.Compare(LServices[LIndex - 1].Info.DisplayName, LServices[LIndex].Info.DisplayName) <= 0,
      Format('Services should be sorted by display name ("%s" > "%s")',
        [LServices[LIndex - 1].Info.DisplayName, LServices[LIndex].Info.DisplayName]));
end;

procedure TServiceManagerBasicTests.TestServiceInfoIsParsed;
var
  LService: TDSMService;
  LTestService: TTestServiceInfo;
begin
  LTestService := TestServiceInfoByName(FAST_TEST_SERVICE_NAME);

  LService := FServiceManager.ServiceByName(LTestService.ServiceName);

  Assert.AreEqual(LTestService.DisplayName, LService.Info.DisplayName, 'DisplayName should match');
  Assert.AreEqual(LTestService.ExeName, LService.Info.FileName, 'ParseBinaryPath should extract the file name');
  Assert.IsTrue(SameText(IncludeTrailingPathDelimiter(TestServiceBinDir), LService.Info.Path),
    Format('ParseBinaryPath should extract the path (expected "%s", got "%s")',
      [IncludeTrailingPathDelimiter(TestServiceBinDir), LService.Info.Path]));
  Assert.IsTrue(SameText(TestServiceExePath(LTestService.ExeName), LService.Info.BinaryPathName),
    'BinaryPathName should be the installed exe path');
  Assert.IsTrue(SameText('LocalSystem', LService.Info.UserName), 'Test services run as LocalSystem');
  Assert.IsTrue(LService.Info.OwnProcess, 'Delphi services run in their own process');
  Assert.IsTrue(LService.StartType = ssManual, 'Test services are installed with manual start type');
end;

procedure TServiceManagerBasicTests.WindowsServiceDescriptionIsRead;
var
  LService: TDSMService;
begin
  // Our fake services have no description, so use a Windows service that always has one
  LService := FServiceManager.ServiceByName(WINDOWS_EVENTLOG_SERVICE_NAME);

  Assert.IsNotNull(LService, 'Windows Event Log service should exist');
  Assert.IsTrue(LService.Info.Description <> '', 'Windows Event Log service should have a description');
end;

{ TServiceControlTests }

procedure TServiceControlTests.Setup;
begin
  FServiceManager := TDSMServiceManager.Create;
  FServiceManager.Active := True;
end;

procedure TServiceControlTests.TearDown;
var
  LTestService: TTestServiceInfo;
begin
  { Make sure no test leaves a service running behind, whatever happened above. TEST_SERVICES is
    ordered so that dependent services are stopped before the services they depend on. }
  for LTestService in TEST_SERVICES do
    StopTestServiceSilently(LTestService.ServiceName);

  FServiceManager.Free;
end;

function TServiceControlTests.GetTestService(const AServiceName: string): TDSMService;
begin
  Result := FServiceManager.ServiceByName(AServiceName);

  Assert.IsNotNull(Result, Format('Test service "%s" should be installed', [AServiceName]));
end;

procedure TServiceControlTests.DependentsListsTheDependentService;
var
  LDependents: TArray<TDSMService>;
  LFound: Boolean;
  LService: TDSMService;
begin
  LDependents := GetTestService(FAST_TEST_SERVICE_NAME).Dependents;

  Assert.IsTrue(Length(LDependents) > 0, 'Fast test service should have dependents');

  LFound := False;

  for LService in LDependents do
    if SameText(LService.Info.Name, DEPENDENT_TEST_SERVICE_NAME) then
      LFound := True;

  Assert.IsTrue(LFound, 'Dependents should list the dependent test service');
end;

procedure TServiceControlTests.DependentsReturnsEmptyArrayForTestService;
var
  LService: TDSMService;
begin
  LService := GetTestService(SLOW_TEST_SERVICE_NAME);

  Assert.AreEqual(0, Length(LService.Dependents), 'Slow test service should have no dependents');
end;

procedure TServiceControlTests.LiveServiceReflectsOutOfBandStateChanges;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.State = ssStopped, 'Test service should be stopped initially');
  Assert.IsFalse(LService.Live, 'Live should default to False');

  // Start the service behind the manager's back, with plain WinAPI
  StartTestServiceSilently(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.State = ssStopped,
    'With Live=False the State property should keep returning the snapshot');

  LService.Live := True;

  Assert.IsTrue(LService.State = ssRunning,
    'With Live=True the State property should reflect the out-of-band start');
end;

procedure TServiceControlTests.PauseAndContinuePausableService;
var
  LService: TDSMService;
begin
  LService := GetTestService(PAUSABLE_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.Start(True), 'Start should succeed');
  Assert.IsTrue(saPauseContinue in LService.ServiceAccepts, 'Running pausable service should accept pause/continue');

  Assert.IsTrue(LService.Pause(True), 'Pause should succeed');
  Assert.IsTrue(LService.State = ssPaused, 'Service should be paused after Pause');

  Assert.IsTrue(LService.Continue(True), 'Continue should succeed');
  Assert.IsTrue(LService.State = ssRunning, 'Service should be running after Continue');

  Assert.IsTrue(LService.Stop(True), 'Stop should succeed');
end;

procedure TServiceControlTests.PauseIsRefusedWhenServiceDoesNotAcceptIt;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.Start(True), 'Start should succeed');

  Assert.WillRaise(
    procedure
    begin
      LService.Pause(True);
    end,
    EServiceCannotBePaused);
end;

procedure TServiceControlTests.ServiceAcceptsReflectRunningService;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.ServiceAccepts = [], 'Stopped service should accept no controls');

  Assert.IsTrue(LService.Start(True), 'Start should succeed');

  Assert.IsTrue(saStop in LService.ServiceAccepts, 'Running service should accept stop');
  Assert.IsFalse(saPauseContinue in LService.ServiceAccepts, 'Fast test service should not accept pause/continue');
end;

procedure TServiceControlTests.SetStateHandlesPausedTransitions;
var
  LService: TDSMService;
begin
  LService := GetTestService(PAUSABLE_TEST_SERVICE_NAME);

  LService.State := ssPaused; // Starts the stopped service first, then pauses it
  Assert.IsTrue(LService.State = ssPaused, 'Setting State to ssPaused should start and pause the service');

  LService.State := ssRunning; // Continues the paused service
  Assert.IsTrue(LService.State = ssRunning, 'Setting State to ssRunning should continue the paused service');

  LService.State := ssStopped;
  Assert.IsTrue(LService.State = ssStopped, 'Setting State to ssStopped should stop the service');
end;

procedure TServiceControlTests.SetStateStartsAndStopsService;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  LService.State := ssRunning;
  Assert.IsTrue(LService.State = ssRunning, 'Setting State to ssRunning should start the service');

  LService.State := ssStopped;
  Assert.IsTrue(LService.State = ssStopped, 'Setting State to ssStopped should stop the service');
end;

procedure TServiceControlTests.SettingTransitionalStateRaises;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.WillRaise(
    procedure
    begin
      LService.State := ssStartPending;
    end,
    ECannotSetTransitionalState);
end;

procedure TServiceControlTests.StartAndStopFastService;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.State = ssStopped, 'Test service should be stopped initially');

  Assert.IsTrue(LService.Start(True), 'Start should succeed');
  Assert.IsTrue(LService.State = ssRunning, 'Service should be running after a waited Start');

  Assert.IsTrue(LService.Stop(True), 'Stop should succeed');
  Assert.IsTrue(LService.State = ssStopped, 'Service should be stopped after a waited Stop');
end;

procedure TServiceControlTests.StartAndStopSlowService;
const
  // Generous slack: the service reports progress the whole time, so WaitFor must not time out,
  // but returning much earlier than the operation takes would mean waiting is broken.
  MINIMUM_EXPECTED_MS = SLOW_TEST_SERVICE_OPERATION_MS - 500;
var
  LService: TDSMService;
  LStopwatch: TStopwatch;
begin
  LService := GetTestService(SLOW_TEST_SERVICE_NAME);

  LStopwatch := TStopwatch.StartNew;
  Assert.IsTrue(LService.Start(True), 'Start of the slow service should succeed');
  LStopwatch.Stop;

  Assert.IsTrue(LService.State = ssRunning, 'Slow service should be running after a waited Start');
  Assert.IsTrue(LStopwatch.ElapsedMilliseconds >= MINIMUM_EXPECTED_MS,
    Format('Waited Start should take at least %d ms, took %d ms', [MINIMUM_EXPECTED_MS, LStopwatch.ElapsedMilliseconds]));

  LStopwatch := TStopwatch.StartNew;
  Assert.IsTrue(LService.Stop(True), 'Stop of the slow service should succeed');
  LStopwatch.Stop;

  Assert.IsTrue(LService.State = ssStopped, 'Slow service should be stopped after a waited Stop');
  Assert.IsTrue(LStopwatch.ElapsedMilliseconds >= MINIMUM_EXPECTED_MS,
    Format('Waited Stop should take at least %d ms, took %d ms', [MINIMUM_EXPECTED_MS, LStopwatch.ElapsedMilliseconds]));
end;

procedure TServiceControlTests.StartingRunningServiceRaisesOSError;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.Start(True), 'First start should succeed');

  Assert.WillRaise(
    procedure
    begin
      LService.Start(False);
    end,
    EOSError);
end;

procedure TServiceControlTests.StopIsRefusedWhenServiceIsStopped;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.State = ssStopped, 'Test service should be stopped initially');

  Assert.WillRaise(
    procedure
    begin
      LService.Stop(True);
    end,
    EServiceCannotBeStopped);
end;

procedure TServiceControlTests.StopIsRefusedWhileDependentIsRunning;
var
  LDependentService: TDSMService;
  LService: TDSMService;
begin
  LDependentService := GetTestService(DEPENDENT_TEST_SERVICE_NAME);
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  // Starting the dependent makes the SCM start the fast test service too
  Assert.IsTrue(LDependentService.Start(True), 'Start of the dependent service should succeed');

  Assert.WillRaise(
    procedure
    begin
      LService.Stop(True);
    end,
    EOSError);
end;

{ TServiceStartTypeTests }

procedure TServiceStartTypeTests.Setup;
begin
  FServiceManager := TDSMServiceManager.Create('', True, True, True); // AllowLocking for StartType changes
  FServiceManager.Active := True;
end;

procedure TServiceStartTypeTests.TearDown;
begin
  // Restore the install default, whatever the test did
  try
    GetTestService(FAST_TEST_SERVICE_NAME).StartType := ssManual;
  except
    // Best effort only; the services get uninstalled after the run anyway
  end;

  FServiceManager.Free;
end;

function TServiceStartTypeTests.GetTestService(const AServiceName: string): TDSMService;
begin
  Result := FServiceManager.ServiceByName(AServiceName);

  Assert.IsNotNull(Result, Format('Test service "%s" should be installed', [AServiceName]));
end;

function TServiceStartTypeTests.ReadStartTypeFromFreshManager(const AServiceName: string): TDSMServiceStartup;
var
  LManager: TDSMServiceManager;
begin
  // A fresh manager queries the SCM from scratch, so this reflects what really was written,
  // not what the writing manager cached
  LManager := TDSMServiceManager.Create('', False);
  try
    LManager.Active := True;

    Result := LManager.ServiceByName(AServiceName).StartType;
  finally
    LManager.Free;
  end;
end;

procedure TServiceStartTypeTests.ChangingStartTypeRequiresLocking;
var
  LManager: TDSMServiceManager;
begin
  LManager := TDSMServiceManager.Create; // AllowLocking defaults to False
  try
    LManager.Active := True;

    Assert.WillRaise(
      procedure
      begin
        LManager.ServiceByName(FAST_TEST_SERVICE_NAME).StartType := ssAutomatic;
      end,
      ELockingNotAllowed);
  finally
    LManager.Free;
  end;
end;

procedure TServiceStartTypeTests.DelayedFlagIsClearedWhenSetBackToAutomatic;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  LService.StartType := ssAutomaticDelayed;
  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssAutomaticDelayed,
    'Service should be delayed automatic first');

  LService.StartType := ssAutomatic;
  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssAutomatic,
    'Setting ssAutomatic should also clear the delayed flag');
end;

procedure TServiceStartTypeTests.DisabledServiceCannotBeStarted;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  LService.StartType := ssDisabled;

  Assert.WillRaise(
    procedure
    begin
      LService.Start(True);
    end,
    EOSError);
end;

procedure TServiceStartTypeTests.SetStartTypeToAutomatic;
begin
  GetTestService(FAST_TEST_SERVICE_NAME).StartType := ssAutomatic;

  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssAutomatic,
    'Start type should be automatic');
end;

procedure TServiceStartTypeTests.SetStartTypeToAutomaticDelayed;
begin
  GetTestService(FAST_TEST_SERVICE_NAME).StartType := ssAutomaticDelayed;

  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssAutomaticDelayed,
    'Start type should be delayed automatic');
end;

procedure TServiceStartTypeTests.SetStartTypeToDisabled;
begin
  GetTestService(FAST_TEST_SERVICE_NAME).StartType := ssDisabled;

  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssDisabled,
    'Start type should be disabled');
end;

procedure TServiceStartTypeTests.SetStartTypeToManual;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  // Away from the install default first, so setting it back is a real change
  LService.StartType := ssAutomatic;
  LService.StartType := ssManual;

  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssManual,
    'Start type should be manual');
end;

procedure TServiceStartTypeTests.SettingSameStartTypeIsNoOp;
var
  LService: TDSMService;
begin
  LService := GetTestService(FAST_TEST_SERVICE_NAME);

  Assert.IsTrue(LService.StartType = ssManual, 'Install default should be manual');

  LService.StartType := ssManual; // No change, should be a silent no-op

  Assert.IsTrue(ReadStartTypeFromFreshManager(FAST_TEST_SERVICE_NAME) = ssManual,
    'Start type should still be manual');
end;

initialization
  TDUnitX.RegisterTestFixture(THelperToStringTests);
  TDUnitX.RegisterTestFixture(TServiceManagerBasicTests);
  TDUnitX.RegisterTestFixture(TServiceControlTests);
  TDUnitX.RegisterTestFixture(TServiceStartTypeTests);

end.
