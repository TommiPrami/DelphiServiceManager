unit FirebirdService.MainForm;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils, System.Variants, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Windows.ServiceManager;

type
  TFormFirebirdServiceMain = class(TForm)
    ButtonEnumerateServices: TButton;
    ButtonEumerateWindowsAudioServiceDependencies: TButton;
    ButtonFixFirebirdService: TButton;
    ButtonQueryFirebird: TButton;
    ButtonStartFirebirdService: TButton;
    ButtonStopFirebirdService: TButton;
    ButtonTestErrorHandler: TButton;
    MemoLog: TMemo;
    PanelRight: TPanel;
    procedure ButtonEnumerateServicesClick(Sender: TObject);
    procedure ButtonEumerateWindowsAudioServiceDependenciesClick(Sender: TObject);
    procedure ButtonFixFirebirdServiceClick(Sender: TObject);
    procedure ButtonQueryFirebirdClick(Sender: TObject);
    procedure ButtonStartFirebirdServiceClick(Sender: TObject);
    procedure ButtonStopFirebirdServiceClick(Sender: TObject);
    procedure ButtonTestErrorHandlerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FServiceManager: TDSMServiceManager;
    function GetLogIndent(const AIndent: Integer): string;
    function GetServiceNamesString(const AService: TDSMService): string;
    function RemoveLineBreaks(const AMessage: string): string;
    procedure ActivateOrRefreshServiceManager(const AGetServiceListOnActive: Boolean);
    procedure Log(const AException: Exception; const AIndent: Integer = 0); overload;
    procedure Log(const AMessage: string; const AIndent: Integer = 0); overload;
  public
    { Public declarations }
  end;

var
  FormFirebirdServiceMain: TFormFirebirdServiceMain;

implementation

uses
  System.StrUtils, Windows.ServiceManager.Types;

const
  FIREBIRD_DEFAULT_SERVICE_NAME = 'FirebirdServerDefaultInstance';

{$R *.dfm}

procedure TFormFirebirdServiceMain.ActivateOrRefreshServiceManager(const AGetServiceListOnActive: Boolean);
begin
  if not FServiceManager.Active then
  begin
    FServiceManager.GetServiceListOnActive := AGetServiceListOnActive;
    FServiceManager.Active := True
  end
  else
    FServiceManager.RebuildServicesList;
end;

procedure TFormFirebirdServiceMain.ButtonEnumerateServicesClick(Sender: TObject);
var
  LServices: TArray<TDSMService>;
  LService: TDSMService;
begin
  Log('Enumerate services...');

  try
    ActivateOrRefreshServiceManager(True);

    LServices := FServiceManager.GetServicesByDisplayName;

    MemoLog.Lines.BeginUpdate;
    try
      for LService in LServices do
      begin
        Log(GetServiceNamesString(LService), 1);
      end;

      Log('', 0);
      Log('Service Count: ' + Length(LServices).ToString, 1);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.ButtonEumerateWindowsAudioServiceDependenciesClick(Sender: TObject);
const
  WINDOWS_AUDIO_SERVICE_NAME = 'Audiosrv';
var
  LDependencies: TArray<TDSMService>;
  LService: TDSMService;
  LDependentService: TDSMService;
  LDependencyCount: Integer;
begin
  LDependencyCount := 0;

  try
    ActivateOrRefreshServiceManager(True);

    LService := FServiceManager.ServiceByName(WINDOWS_AUDIO_SERVICE_NAME);
    if not Assigned(LService) then
      raise Exception.Create('Service not found');

    Log('Enumerate "' + LService.Info.DisplayName + '" service Dependencies...');
    MemoLog.Lines.BeginUpdate;
    try
      LDependencies := LService.Dependents;

      for LDependentService in LDependencies do
      begin
        if Assigned(LDependentService) then
        begin
          Log(GetServiceNamesString(LDependentService), 1);
          Inc(LDependencyCount);
        end;
      end;

      Log('', 0);
      Log('Dependent service(s) Count: ' + LDependencyCount.ToString, 1);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.ButtonFixFirebirdServiceClick(Sender: TObject);
var
  LServiceManager: TDSMServiceManager;
  LFirebirdService: TDSMService;
begin
  Log('Fixing Firebird service...');

  LServiceManager := TDSMServiceManager.Create;
  try
    try
      try
        LServiceManager.BeginLockingProcess;
        LFirebirdService := LServiceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

        if LFirebirdService.State = ssRunning then
        begin
          Log('Stopping service...', 1);
          LFirebirdService.Stop;
        end;

        if LFirebirdService.StartType <> ssAutomatic then
        begin
          Log('Changing Service Start Type to: Automatic', 1);
          LFirebirdService.StartType := ssAutomatic;
        end;

        if LFirebirdService.State <> ssRunning then
        begin
          Log('Starting service...', 1);
          LFirebirdService.Start;
        end;

        Log('Service state: ' + LFirebirdService.State.ToString, 1);
      finally
        LServiceManager.EndLockingProcess;
      end;
    finally
      LServiceManager.Free;
    end;

    Log('');
  except
    on E: Exception do
      Log(E, 2);
  end;
end;

procedure TFormFirebirdServiceMain.ButtonQueryFirebirdClick(Sender: TObject);
var
  LServiceRunning: Boolean;
  LFirebirdService: TDSMService;
begin
  Log('Query Firebird service status...');

  try
    ActivateOrRefreshServiceManager(False);

    LFirebirdService := FServiceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    LServiceRunning := LFirebirdService.State = ssRunning;
    Log(IfThen(LServiceRunning, 'Firebird Service Running', 'Firebird Service NOT Running'), 1);

    Log('DisplayName: "' + LFirebirdService.Info.DisplayName + '"', 2);
    Log('Path: "' + LFirebirdService.Info.Path + '"', 2);
    Log('File name: "' + LFirebirdService.Info.FileName + '"', 2);
    Log('Command line: "' + LFirebirdService.Info.CommandLine + '"', 2);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.ButtonStartFirebirdServiceClick(Sender: TObject);
var
  LFirebirdService: TDSMService;
begin
  Log('Starting Firebird service...');

  try
    ActivateOrRefreshServiceManager(True);

    LFirebirdService := FServiceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    if LFirebirdService.State <> ssRunning then
    begin
      Log('Starting service...', 1);
      LFirebirdService.Start;

      Log('Service state: ' + LFirebirdService.State.ToString, 1);
    end
    else
      Log('Firebird service already running', 1);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.ButtonStopFirebirdServiceClick(Sender: TObject);
var
  LFirebirdService: TDSMService;
begin
  Log('Stopping Firebird service...');

  try
    ActivateOrRefreshServiceManager(True);

    LFirebirdService := FServiceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    if LFirebirdService.State <> ssStopped then
    begin
      Log('Stopping service...', 1);
      LFirebirdService.Stop;

      Log('Service state: ' + LFirebirdService.State.ToString, 1);
    end
    else
      Log('Firebird service already stopped', 1);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.ButtonTestErrorHandlerClick(Sender: TObject);
var
  LServiceManager: TDSMServiceManager;
  LExceptionRaised: Boolean;
begin
  Log('Testing error handler...');

  Log('Raise Exceptions', 1);
  LServiceManager := TDSMServiceManager.Create;
  try
    try
      LExceptionRaised := False;
      LServiceManager.RaiseExceptions := True;

      LServiceManager.RebuildServicesList;
    except
      on E: Exception do
      begin
        LExceptionRaised := True;
        Log(E, 2);
      end;
    end;

    if not LExceptionRaised or (LServiceManager.LastErrorCode = 0) or (LServiceManager.LastErrorMessage.IsEmpty) then
      raise Exception.Create('Error handler did not work');

    Log('Do not raise Exceptions', 1);
    try
      LExceptionRaised := False;
      LServiceManager.RaiseExceptions := False;

      LServiceManager.RebuildServicesList;
    except
      on E: Exception do
      begin
        LExceptionRaised := True;
        Log(E, 2);
      end;
    end;

    if LExceptionRaised or (LServiceManager.LastErrorCode = 0) or (LServiceManager.LastErrorMessage.IsEmpty) then
      raise Exception.Create('Error handler did not work')
    else
      Log(LServiceManager.LastErrorMessage, 2);
  finally
    LServiceManager.Free;
  end;

  Log('');
end;

procedure TFormFirebirdServiceMain.FormCreate(Sender: TObject);
begin
  FServiceManager := TDSMServiceManager.Create;
end;

procedure TFormFirebirdServiceMain.FormDestroy(Sender: TObject);
begin
  FServiceManager.Free;
end;

function TFormFirebirdServiceMain.GetLogIndent(const AIndent: Integer): string;
begin
  if AIndent > 0 then
    Result := StringOfChar(' ', AIndent * 2) + '- '
  else
    Result := '';
end;

function TFormFirebirdServiceMain.GetServiceNamesString(const AService: TDSMService): string;
begin
  Result := AService.Info.DisplayName + ' (' + AService.Info.Name + ')';
end;

procedure TFormFirebirdServiceMain.Log(const AMessage: string; const AIndent: Integer = 0);
var
  LIndent: string;
begin
  LIndent := GetLogIndent(AIndent);

  MemoLog.Lines.Add(LIndent + RemoveLineBreaks(AMessage));
end;

procedure TFormFirebirdServiceMain.Log(const AException: Exception; const AIndent: Integer = 0);
begin
  Log('Exception ' + AException.ClassName + ' occurred, with message: "' + AException.Message + '"', AIndent);
  Log('');
end;

function TFormFirebirdServiceMain.RemoveLineBreaks(const AMessage: string): string;
begin
  // Just quick and dirty way to get error message into one log line
  Result := AMessage.Replace(#13, ' ', [rfReplaceAll]);
  Result := Result.Replace(#10, '', [rfReplaceAll]);
  Result := Result.Replace('  ', ' ', [rfReplaceAll]); // Duplicate spaces to one
end;

end.
