unit FirbirdService.MainForm;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils, System.Variants, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, IdBaseComponent, IdComponent, IdTCPClient, IdTCPConnection,
  Windows.ServiceManager;

type
  TFormFirebierdServiceMain = class(TForm)
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
    FServciceManager: TServiceManager;
    function GetLogIndent(const AIndent: Integer): string;
    function GetServiceNamesString(const AServiceInfo: TServiceInfo): string;
    function RemoveLineBreaks(const AMessage: string): string;
    procedure ActivateOrRefreshServiceManager(const AGetServiceListOnActive: Boolean);
    procedure Log(const AException: Exception; const AIndent: Integer = 0); overload;
    procedure Log(const AMessage: string; const AIndent: Integer = 0); overload;
  public
    { Public declarations }
  end;

var
  FormFirebierdServiceMain: TFormFirebierdServiceMain;

implementation

uses
  System.StrUtils, Windows.ServiceManager.Types;

const
  FIREBIRD_DEFAULT_SERVICE_NAME = 'FirebirdServerDefaultInstance';

{$R *.dfm}

procedure TFormFirebierdServiceMain.ActivateOrRefreshServiceManager(const AGetServiceListOnActive: Boolean);
begin
  if not FServciceManager.Active then
  begin
    FServciceManager.GetServiceListOnActive := AGetServiceListOnActive;
    FServciceManager.Active := True
  end
  else
    FServciceManager.RebuildServicesList;
end;

procedure TFormFirebierdServiceMain.ButtonEnumerateServicesClick(Sender: TObject);
var
  LServices: TArray<TServiceInfo>;
  LService: TServiceInfo;
begin
  Log('Enumerate services...');

  try
    ActivateOrRefreshServiceManager(True);

    LServices := FServciceManager.GetServicesByDisplayName;

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

procedure TFormFirebierdServiceMain.ButtonEumerateWindowsAudioServiceDependenciesClick(Sender: TObject);
const
  WINDOWS_AUDIO_SERVICE_NAME = 'Audiosrv';
var
  LDepedencies: TArray<TServiceInfo>;
  LService: TServiceInfo;
  LDependantService: TServiceInfo;
  LDepedencyCount: Integer;
begin
  LDepedencyCount := 0;

  try
    ActivateOrRefreshServiceManager(True);

    LService := FServciceManager.ServiceByName(WINDOWS_AUDIO_SERVICE_NAME);
    if not Assigned(LService) then
      raise Exception.Create('Service not found');

    Log('Eumerate "' + LService.DisplayName + '" service Dependencies...');
    MemoLog.Lines.BeginUpdate;
    try
      LDepedencies := LService.Dependents;

      for LDependantService in LDepedencies do
      begin
        if Assigned(LDependantService) then
        begin
          Log(GetServiceNamesString(LDependantService), 1);
          Inc(LDepedencyCount);
        end;
      end;

      Log('', 0);
      Log('Dependent service(s) Count: ' + LDepedencyCount.ToString, 1);
   finally
      MemoLog.Lines.EndUpdate;
    end;
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.ButtonFixFirebirdServiceClick(Sender: TObject);
var
  LServciceManager: TServiceManager;
  LFirebirdService: TServiceInfo;
begin
  Log('Fixing Firebird service...');

  LServciceManager := TServiceManager.Create;
  try
    try
      try
        LServciceManager.BeginLockingProcess;
        LFirebirdService := LServciceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

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

        Log('Service state: ' + ServiceStateToString(LFirebirdService.State), 1);
      finally
        LServciceManager.EndLockingProcess;
      end;
    finally
      LServciceManager.Free;
    end;

    Log('');
  except
    on E: Exception do
      Log(E, 2);
  end;
end;

procedure TFormFirebierdServiceMain.ButtonQueryFirebirdClick(Sender: TObject);
var
  LServiceRunning: Boolean;
  LFirebirdService: TServiceInfo;
begin
  Log('Query Firebird service status...');

  try
    ActivateOrRefreshServiceManager(False);

    LFirebirdService := FServciceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    LServiceRunning := LFirebirdService.State = ssRunning;
    Log(IfThen(LServiceRunning, 'Firebird Service Runnin', 'Firebird Service NOT Runnin'), 1);

    Log('DisplayName: "' + LFirebirdService.DisplayName + '"', 2);
    Log('Path: "' + LFirebirdService.Path + '"', 2);
    Log('File name: "' + LFirebirdService.FileName + '"', 2);
    Log('Command line: "' + LFirebirdService.CommandLine + '"', 2);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.ButtonStartFirebirdServiceClick(Sender: TObject);
var
  LFirebirdService: TServiceInfo;
begin
  Log('Starting Firebird service...');

  try
   ActivateOrRefreshServiceManager(True);

    LFirebirdService := FServciceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    if LFirebirdService.State <> ssRunning then
    begin
      Log('Starting service...', 1);
      LFirebirdService.Start;

      Log('Service state: ' + ServiceStateToString(LFirebirdService.State), 1);
    end
    else
      Log('Firebird service already running', 1);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.ButtonStopFirebirdServiceClick(Sender: TObject);
var
  LFirebirdService: TServiceInfo;
begin
  Log('Stopping Firebird service...');

  try
    ActivateOrRefreshServiceManager(True);

    LFirebirdService := FServciceManager.ServiceByName(FIREBIRD_DEFAULT_SERVICE_NAME);

    if LFirebirdService.State <> ssStopped then
    begin
      Log('Stopping service...', 1);
      LFirebirdService.Stop;

      Log('Service state: ' + ServiceStateToString(LFirebirdService.State), 1);
    end
    else
      Log('Firebird service already stopped', 1);
  except
    on E: Exception do
      Log(E, 2);
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.ButtonTestErrorHandlerClick(Sender: TObject);
var
  LServciceManager: TServiceManager;
  LExceptionRaised: Boolean;
begin
  Log('Testing error handler...');

  Log('Raise Exceptions', 1);
  LServciceManager := TServiceManager.Create;
  try
    try
      LExceptionRaised := False;
      LServciceManager.RaiseExceptions := True;

      LServciceManager.RebuildServicesList;
    except
      on E: Exception do
      begin
        LExceptionRaised := True;
        Log(E, 2);
      end;
    end;

    if not LExceptionRaised or (LServciceManager.LastErrorCode = 0) or (LServciceManager.LAstErrorMessage.IsEmpty) then
      raise Exception.Create('Error handler did not work');

    Log('Do not raise Exceptions', 1);
    try
      LExceptionRaised := False;
      LServciceManager.RaiseExceptions := False;

      LServciceManager.RebuildServicesList;
    except
      on E: Exception do
      begin
        LExceptionRaised := True;
        Log(E, 2);
      end;
    end;

    if LExceptionRaised or (LServciceManager.LastErrorCode = 0) or (LServciceManager.LAstErrorMessage.IsEmpty) then
      raise Exception.Create('Error handler did not work')
    else
      Log(LServciceManager.LastErrorMessage, 2);
  finally
    LServciceManager.Free;
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.FormCreate(Sender: TObject);
begin
  FServciceManager := TServiceManager.Create;
end;

procedure TFormFirebierdServiceMain.FormDestroy(Sender: TObject);
begin
  FServciceManager.Free;
end;

function TFormFirebierdServiceMain.GetLogIndent(const AIndent: Integer): string;
begin
  if AIndent > 0 then
    Result := StringOfChar(' ', AIndent * 2) + '- '
  else
    Result := '';
end;

function TFormFirebierdServiceMain.GetServiceNamesString(const AServiceInfo: TServiceInfo): string;
begin
  Result := AServiceInfo.DisplayName + ' (' + AServiceInfo.Name + ')';
end;

procedure TFormFirebierdServiceMain.Log(const AMessage: string; const AIndent: Integer = 0);
var
  LIndent: string;
begin
  LIndent := GetLogIndent(AIndent);

  MemoLog.Lines.Add(LIndent + RemoveLineBreaks(AMessage));
end;

procedure TFormFirebierdServiceMain.Log(const AException: Exception; const AIndent: Integer = 0);
begin
  Log('Exception ' + AException.ClassName + ' occurent, with message: "' + AException.Message + '"', AIndent);
  Log('');
end;

function TFormFirebierdServiceMain.RemoveLineBreaks(const AMessage: string): string;
begin
  // Just quick and dirty way to get error message into one log line
  Result := AMessage.Replace(#13, ' ', [rfReplaceAll]);
  Result := Result.Replace(#10, '', [rfReplaceAll]);
  Result := Result.Replace('  ', ' ', [rfReplaceAll]); // Duplicate spaces to one
end;

end.
