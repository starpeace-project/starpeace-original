unit LoggedUserData;

interface

  procedure SetUserData(data : pointer);
  procedure LogSystem;
  function  GetUserData : pointer;
  function  CheckAuthenticity(data : pointer) : boolean;

implementation

  uses
    Windows, Classes, Messages, RDOInterfaces, SmartThreads;

  const
    SYSTEM_SIM : pointer = pointer($FFFFFFF0);

  procedure SetUserData(data : pointer);
    var
      Thread : TThread;
      Msg    : TMessage;
    begin
      Thread := SmartThreads.FindThread(GetCurrentThreadID);
      if Thread <> nil
        then
          begin
            Msg.Msg := MSG_SETTHREADDATA;
            Msg.LParam := integer(data);
            Thread.Dispatch(Msg);
          end;
    end;

  procedure LogSystem;
    begin
      SetUserData(SYSTEM_SIM);
    end;

  function GetUserData : pointer;
    var
      Thread : TThread;
      Msg    : TMessage;
    begin
      Thread := SmartThreads.FindThread(GetCurrentThreadID);
      if Thread <> nil
        then
          begin
            Msg.Msg := MSG_GETTHREADDATA;
            Msg.Result := integer(nil);
            Thread.Dispatch(Msg);
            result := pointer(Msg.Result);
          end
        else result := nil;
    end;

  function CheckAuthenticity(data : pointer) : boolean;
    var
      usrData : pointer;
    begin
      usrData := GetUserData;
      result  := (usrData = data) or (usrData = SYSTEM_SIM);
    end;

end.
