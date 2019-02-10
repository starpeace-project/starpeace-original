unit MailData;

interface

  const
    MailKey = '\Software\Oceanus\Five\Mail\';

  procedure SaveMailRoot(aRoot : string);
  procedure ReadMailRoot;
  function  GetMailRoot : string;

implementation

  uses
    Windows, Registry;

  var
    MailRoot : string = '';

  procedure SaveMailRoot(aRoot : string);
    var
      Reg : TRegistry;
    begin
      if (aRoot <> '') and (aRoot[length(aRoot)] = '\')
        then MailRoot := aRoot
        else MailRoot := aRoot + '\';
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( MailKey, true )
          then
            begin
              Reg.WriteString( 'MailRoot', aRoot );
              Reg.RegistryConnect( '' );
            end;
      finally
        Reg.Free;
      end;
    end;

  procedure ReadMailRoot;
    var
      Reg : TRegistry;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( MailKey, true )
            then MailRoot := Reg.ReadString( 'MailRoot' );
          if (MailRoot <> '') and (MailRoot[length(MailRoot)] <> '\')
            then MailRoot := MailRoot + '\';
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  function GetMailRoot : string;
    begin
      if MailRoot = ''
        then ReadMailRoot;
      result := MailRoot;
    end;

end.
