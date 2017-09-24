unit ErrorCodes;

interface

  const
    errNoError                 = 0;
    errMalformedQuery          = 1;
    errIllegalObject           = 2;
    errUnexistentProperty      = 3;
    errIllegalPropValue        = 4;
    errUnexistentMethod        = 5;
    errIllegalParamList        = 6;
    errIllegalPropType         = 7;
    errQueryTimedOut           = 8;
    errIllegalFunctionRes      = 9;
    errSendError               = 10;
    errReceiveError            = 11;
    errMalformedResult         = 12;
    errQueryQueueOverflow      = 13;
    errRDOServerNotInitialized = 14;
    errUnknownError            = 15;
    errNoResult                = 16;
    errServerBusy              = 17;

  function CreateErrorMessage( ErrorCode : integer ) : string;

implementation

  uses
    SysUtils;

  function CreateErrorMessage( ErrorCode : integer ) : string;
    begin
      if ErrorCode <> errNoError
        then
          Result := 'error ' + IntToStr( ErrorCode )
        else
          Result := ''
    end;

end.
