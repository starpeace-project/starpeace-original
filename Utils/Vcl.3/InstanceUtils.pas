unit InstanceUtils;

interface

  uses
    Windows, WinUtils, Forms;

  function AppIsAlreadyRunning : boolean;

implementation

  function AppIsAlreadyRunning : boolean;
    var
      ClassName : string;
      WinHandle : HWND;
    begin
      WinHandle := Application.MainForm.Handle;
      SetLength( ClassName, MAX_PATH );
      SetLength( ClassName, GetClassName( WinHandle, pchar( ClassName ), length( ClassName ) ) );
      Result := (GetWindowOfClass( ClassName ) <> 0);
    end;

end.
