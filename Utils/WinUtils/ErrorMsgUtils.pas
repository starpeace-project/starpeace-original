unit ErrorMsgUtils;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  function SysErrorString( ErrorId : integer ) : string;

implementation

  uses
    Windows;
    
  function SysErrorString( ErrorId : integer ) : string;
    var
      Len : integer;
    begin
      SetLength( Result, MAX_PATH );
      Len := FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, ErrorId, 0, pchar( Result ), length( Result ), nil );
      SetLength( Result, Len );
    end;


end.
