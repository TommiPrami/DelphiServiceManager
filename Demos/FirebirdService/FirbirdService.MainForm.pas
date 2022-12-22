unit FirbirdService.MainForm;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils, System.Variants, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, IdBaseComponent, IdComponent, IdTCPClient, IdTCPConnection,
  Windows.ServiceManager;

type
  TFormFirebierdServiceMain = class(TForm)
    ButtonEnumerateServices: TButton;
    ButtonFixFirebirdService: TButton;
    ButtonQueryFirebird: TButton;
    ButtonStartFirebirdService: TButton;
    ButtonStopFirebirdService: TButton;
    MemoLog: TMemo;
    PanelRight: TPanel;
    procedure ButtonEnumerateServicesClick(Sender: TObject);
    procedure ButtonFixFirebirdServiceClick(Sender: TObject);
    procedure ButtonQueryFirebirdClick(Sender: TObject);
    procedure ButtonStartFirebirdServiceClick(Sender: TObject);
    procedure ButtonStopFirebirdServiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FServciceManager: TServiceManager;
    procedure ActivateOrRefreshServiceManager;
    procedure Log(const AException: Exception; const AIndent: Integer = 0); overload;
    procedure Log(const AMessage: string; const AIndent: Integer = 0); overload;
    function GetLogIndent(const AIndent: Integer): string;
    function RemoveLineBreaks(const AMessage: string): string;
  public
    { Public declarations }
  end;

var
  FormFirebierdServiceMain: TFormFirebierdServiceMain;

implementation

uses
  System.StrUtils;

const
  FIREBIRD_DEFAULT_SERVICE_NAME = 'FirebirdServerDefaultInstance';

{$R *.dfm}

procedure TFormFirebierdServiceMain.ActivateOrRefreshServiceManager;
begin
  if not FServciceManager.Active then
    FServciceManager.Active := True
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
    ActivateOrRefreshServiceManager;

    LServices := FServciceManager.GetServicesByDisplayName;

    MemoLog.Lines.BeginUpdate;
    try
      for LService in LServices do
        Log(LService.DisplayName, 1);
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

      LServciceManager.BeginLockingProcess;
      try
        LFirebirdService := LServciceManager.ServiceByName[FIREBIRD_DEFAULT_SERVICE_NAME];

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
      except
        on E: Exception do
          Log(E, 2);
      end;
    finally
      LServciceManager.EndLockingProcess;
    end;
  finally
    LServciceManager.Free;
  end;

  Log('');
end;

procedure TFormFirebierdServiceMain.ButtonQueryFirebirdClick(Sender: TObject);
var
  LServiceRunning: Boolean;
  LFirebirdService: TServiceInfo;
begin
  Log('Query Firebird service status...');

  try
    ActivateOrRefreshServiceManager;

    LFirebirdService := FServciceManager.ServiceByName[FIREBIRD_DEFAULT_SERVICE_NAME];

    LServiceRunning := LFirebirdService.State = ssRunning;
    Log(IfThen(LServiceRunning, 'Firebird Service Runnin', 'Firebird Service NOT Runnin'), 1);

    if LServiceRunning then
      Log('Service located at: "' + LFirebirdService.BinaryPathName + '"', 1);
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
   ActivateOrRefreshServiceManager;

    LFirebirdService := FServciceManager.ServiceByName[FIREBIRD_DEFAULT_SERVICE_NAME];

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
    ActivateOrRefreshServiceManager;

    LFirebirdService := FServciceManager.ServiceByName[FIREBIRD_DEFAULT_SERVICE_NAME];

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
end;

function TFormFirebierdServiceMain.RemoveLineBreaks(const AMessage: string): string;
begin
  // Just quick and dirty way to get error message into one log line
  Result := AMessage.Replace(#13, ' ', [rfReplaceAll]);
  Result := Result.Replace(#10, '', [rfReplaceAll]);
  Result := Result.Replace('  ', ' ', [rfReplaceAll]); // Duplicate spaces to one
end;

end.
