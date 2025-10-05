unit Windows.ServiceManager.Types;

interface

uses
  System.SysUtils;

type
  ECustomServiceManagerException = class(Exception);

  ENotActive = class(ECustomServiceManagerException);
  EServiceNotFound = class(ECustomServiceManagerException);
  EOSNotSupported = class(ECustomServiceManagerException);
  EOperationNotAllowedWhileActive = class(ECustomServiceManagerException);
  ELockingNotAllowed = class(ECustomServiceManagerException);
  EServiceStateUnknown = class(ECustomServiceManagerException);
  EServiceCannotBeContinued = class(ECustomServiceManagerException);
  EServiceCannotBePaused = class(ECustomServiceManagerException);
  EServiceCannotBeStopped = class(ECustomServiceManagerException);
  EServiceDidNotRespond = class(ECustomServiceManagerException);
  EServiceServiceStartTypeUnknown = class(ECustomServiceManagerException);
  ECannotSetTransitionalState = class(ECustomServiceManagerException);
  EServiceAccessDiffers = class(ECustomServiceManagerException);

  TDSMErrorInfo = record
    ErrorCode: Integer;
    ExceptionClass: ExceptClass;
    ErrorMessage: string;
  end;

  { The states a service can be in. }
  TDSMServiceState = (ssStopped,
                   ssStartPending,
                   ssStopPending,
                   ssRunning,
                   ssContinuePending,
                   ssPausePending,
                   ssPaused);

  { Enumeration of the standard "controls" a service can accept. The shutdown control, if not
    accepted is ignored. The shutdown control can only be sent when a shutdown occurs. }
  TDSMServiceAccept = (saStop,
                    saPauseContinue,
                    saShutdown);

  { The set of "controls" a service can accept. }
  TDSMServiceAccepts = set of TDSMServiceAccept;

  { The service startup enumeration determines how a service is started. ssAutomatic will start the
    service automatically at startup. ssManual will allow applications and other services to start
    this service manually and ssDisabled will disallow the service to be started altogether (but it
    will be kept in the service database). }
  TDSMServiceStartup = (ssAutomatic,
                     ssAutomaticDelayed,
                     ssManual,
                     ssDisabled);

implementation

end.
