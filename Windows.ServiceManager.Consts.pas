unit Windows.ServiceManager.Consts;

interface

uses
  Windows.ServiceManager.Types;

const
  // OS Error
  LAST_OS_ERROR = -1;
  // Rest of the error codes
  SERVICELIST_NOT_ACTIVE = 1;
  SERVICE_NOT_FOUND = 2;
  OS_NOT_CUPPOORTED = 3;
  IS_ACTIVE = 4;
  LOCKING_NOT_ALLOWED = 5;
  OPERATION_NOT_ALLOWED_WHILE_ACTIVE = 6;
  SERVICE_STATE_UNKNOWN = 7;
  SERVICE_CANNOT_CONTINUE = 8;
  SERVICE_CANNOT_PAUSE = 9;
  SERVICE_CANNOT_STOP = 10;
  SERVICE_TIMEOUT = 11;
  SERVICE_STARTTYPE_UNKNOWN = 12;
  SERVICE_CANNOT_SET_STATE = 13;
  SERVICE_ACCESS_DIFFERS = 14;

  ErrorInfoArray: array[0..13] of TErrorInfo =
    (
      (ErrorCode: SERVICELIST_NOT_ACTIVE; ExceptionClass: ENotActive; ErrorMessage: 'BuildServicesList only works when Active.'),
      (ErrorCode: SERVICE_NOT_FOUND; ExceptionClass: EServiceNotFound; ErrorMessage: 'Service not found.'),
      (ErrorCode: OS_NOT_CUPPOORTED; ExceptionClass: EOSNotSupported; ErrorMessage: 'This program only works on Windows NT, 2000, XP or later.'),
      (ErrorCode: IS_ACTIVE; ExceptionClass: EOperationNotAllowedWhileActive; ErrorMessage: 'Cannot change machine name while Active.'),
      (ErrorCode: LOCKING_NOT_ALLOWED; ExceptionClass: ELockingNotAllowed; ErrorMessage: 'Locking of the service manager not allowed.'),
      (ErrorCode: OPERATION_NOT_ALLOWED_WHILE_ACTIVE; ExceptionClass: EOperationNotAllowedWhileActive; ErrorMessage: 'Cannot change allow locking while active.'),
      (ErrorCode: SERVICE_STATE_UNKNOWN; ExceptionClass: EServiceStateUnknown; ErrorMessage: 'Service State unknown.'),
      (ErrorCode: SERVICE_CANNOT_CONTINUE; ExceptionClass: EServiceCannotBeContinued; ErrorMessage: 'Service cannot be continued.'),
      (ErrorCode: SERVICE_CANNOT_PAUSE; ExceptionClass: EServiceCannotBeContinued; ErrorMessage: 'Service cannot be paused.'),
      (ErrorCode: SERVICE_CANNOT_STOP; ExceptionClass: EServiceCannotBeStopped; ErrorMessage: 'Service cannot be Stopped.'),
      (ErrorCode: SERVICE_TIMEOUT; ExceptionClass: EServiceDidNotRespond; ErrorMessage: 'Service did not react within timeframe given.'),
      (ErrorCode: SERVICE_STARTTYPE_UNKNOWN; ExceptionClass: EServiceServiceStartTypeUnknown; ErrorMessage: 'Service Start Type unknown.'),
      (ErrorCode: SERVICE_CANNOT_SET_STATE; ExceptionClass: ECannotSetTransitionalState; ErrorMessage: 'Cannot set a transitional state.'),
      (ErrorCode: SERVICE_ACCESS_DIFFERS; ExceptionClass: EServiceAccessDiffers; ErrorMessage: 'Service access differs.')
   );

implementation

end.
