unit Windows.ServiceManager.Types;

interface

uses
  System.SysUtils;

type
  ECustomServiceManagerException = class(Exception);
  EIndexOutOfBounds = class(ECustomServiceManagerException);
  EServiceNotFound = class(ECustomServiceManagerException);
  EOSNotSupported = class(ECustomServiceManagerException);
  EOperationNotAllowedWhileActive = class(ECustomServiceManagerException);
  ELockingNotAllowed = class(ECustomServiceManagerException);
  EServiceStateUnkown = class(ECustomServiceManagerException);
  EServiceCannotBeContinued = class(ECustomServiceManagerException);
  EServiceCannotBePaused = class(ECustomServiceManagerException);
  EServiceCannotBeStopped = class(ECustomServiceManagerException);
  EServiceDidNotRespond = class(ECustomServiceManagerException);
  EServiceServiceStartTypeUnknown = class(ECustomServiceManagerException);
  ECannotSetTransitionalState = class(ECustomServiceManagerException);

  TErrorInfo = record
    ErrorCode: Integer;
    ExceptionClass: ExceptClass;
    ErrorMessage: string;
  end;

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

const
  RAISE_LAST_OS_ERROR = -1;
  ErrorInfoArray: array[0..12] of TErrorInfo =
    (
      (ErrorCode: 1; ExceptionClass: Exception; ErrorMessage: 'BuildServicesList only works when Active.'),
      (ErrorCode: 2; ExceptionClass: EServiceNotFound; ErrorMessage: 'Service not found.'),
      (ErrorCode: 3; ExceptionClass: EOSNotSupported; ErrorMessage: 'This program only works on Windows NT, 2000, XP or later.'),
      (ErrorCode: 4; ExceptionClass: EOperationNotAllowedWhileActive; ErrorMessage: 'Cannot change machine name while Active.'),
      (ErrorCode: 5; ExceptionClass: ELockingNotAllowed; ErrorMessage: 'Locking of the service manager not allowed.'),
      (ErrorCode: 6; ExceptionClass: EOperationNotAllowedWhileActive; ErrorMessage: 'Cannot change allow locking while active.'),
      (ErrorCode: 7; ExceptionClass: EServiceStateUnkown; ErrorMessage: 'Service State unknown.'),
      (ErrorCode: 8; ExceptionClass: EServiceCannotBeContinued; ErrorMessage: 'Service cannot be continued.'),
      (ErrorCode: 9; ExceptionClass: EServiceCannotBeContinued; ErrorMessage: 'Service cannot be paused.'),
      (ErrorCode: 10; ExceptionClass: EServiceCannotBeStopped; ErrorMessage: 'Service cannot be Stopped.'),
      (ErrorCode: 11; ExceptionClass: EServiceDidNotRespond; ErrorMessage: 'Service did not react within timeframe given.'),
      (ErrorCode: 12; ExceptionClass: EServiceServiceStartTypeUnknown; ErrorMessage: 'Service Start Type unknown.'),
      (ErrorCode: 13; ExceptionClass: ECannotSetTransitionalState; ErrorMessage: 'Cannot set a transitional state.')
   );


implementation

end.
