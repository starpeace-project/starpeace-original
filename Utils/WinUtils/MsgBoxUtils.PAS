unit MsgBoxUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows;

  const
    idOK      = 1;
    idCancel  = 2;
    idAbort   = 3;
    idRetry   = 4;
    idIgnore  = 5;
    idYes     = 6;
    idNo      = 7;

  const
    ttYesNo      = MB_YESNO +  MB_ICONQUESTION;
    ttAbort      = MB_ABORTRETRYIGNORE + MB_ICONSTOP;
    ttRetry      = MB_RETRYCANCEL + MB_ICONSTOP;
    ttCancel     = MB_YESNOCANCEL + MB_ICONSTOP;
    ttErrorInfo  = MB_OK + MB_ICONSTOP;
    ttInfo       = MB_OK + MB_ICONINFORMATION;

  function ShowInfoBox( const Msg : string; args : array of const;
                        Caption : string; TextType : cardinal) : integer;
  function ShowMessageBox( const Msg, Caption : string; TextType : cardinal) : integer;

implementation

  uses
    SysUtils;

  function ShowInfoBox( const Msg : string; args : array of const;
                        Caption : string; TextType : cardinal) : integer;
    var
      Buf : string;
    begin
      FmtStr( Buf, Msg, args);
      Result := MessageBox( 0, pchar(Buf), pchar(Caption), TextType);
    end;

  function ShowMessageBox( const Msg, Caption : string; TextType : cardinal) : integer;
    begin
      Result := MessageBox( 0, pchar(msg), pchar(Caption), TextType);
    end;

end.

