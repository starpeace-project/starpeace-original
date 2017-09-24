unit WinEnum;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils, WinUtils;

  type
    TEnumWindowsProc = function( Window : HWND; lParam : dword ) : Bool;

  // Find a window with 'Owner' as owner
  function FindWindowByCaption( const Caption : string;
                                const ClassNames : array of string; var WinClass : string ) : HWND;

  // Find a window with 'Owner' as owner
  function FindOwnedWindow( Owner : HWND ) : HWND;

  // Crazy stuff: Find a child window with two scrollbars
  function FindViewerClient( Viewer : HWND ) : HWND;

implementation

  //  FindWindowByCaption

  type
    PClassNameArray = ^TClassNameArray;
    TClassNameArray = array[0..0] of string;

  type
    PEnum1Params = ^TEnum1Params;
    TEnum1Params =
      record
        NeededWindow  : HWND;
        ClassesLow    : integer;
        ClassesHigh   : integer;
        NeededClasses : PClassNameArray;
        NeededCaption : string;
        WinClassName  : string;
      end;

  function WinEnumerator1( Window : HWND; lParam : dword ) : Bool; stdcall;
    var
      Params    : PEnum1Params absolute lParam;
      Caption   : string;
      ClassName : string;
      i         : integer;
    begin
      Caption   := UpperCase( GetWindowCaption( Window ) );
      ClassName := GetWindowClass( Window );

      with Params^ do
        begin
          if Pos( NeededCaption, Caption ) > 0
            then
              for i := ClassesLow to ClassesHigh do
                if ClassName = NeededClasses[i]
                  then
                    begin
                      NeededWindow := Window;
                      WinClassName := NeededClasses[i];
                    end;

          Result := ( NeededWindow = 0 );
        end;
    end;

  function FindWindowByCaption( const Caption : string;
                                const ClassNames : array of string; var WinClass : string ) : HWND;
    var
      Params : TEnum1Params;
    begin
      with Params do
        begin
          NeededWindow             := 0;
          pointer( NeededClasses ) := @ClassNames;
          ClassesLow               := Low( ClassNames );
          ClassesHigh              := High( ClassNames );

          NeededCaption := UpperCase( ChangeFileExt( ExtractFilename( Caption ), '.' ) );
          SetLength( NeededCaption, length( NeededCaption ) - 1 );

          EnumWindows( @WinEnumerator1, dword( @Params ) );

          WinClass := WinClassName;
          Result   := NeededWindow;
        end;
    end;

  // FindOwnedWindow

  type
    PEnum2Params = ^TEnum2Params;
    TEnum2Params =
      record
        NeededWindow   : HWND;
        Owner          : HWND;
      end;

  function WinEnumerator2( Window : HWND; lParam : dword ) : Bool; stdcall;
    var
      Params : PEnum2Params absolute lParam;
      Owner  : HWND;
    begin
      Owner := GetWindowOwner( Window );
      if Owner = Params.Owner
        then
          begin
            Params.NeededWindow := Window;
            Result := false;
          end
        else Result := true;
    end;

  function FindOwnedWindow( Owner : HWND ) : HWND;
    var
      Params : TEnum2Params;
    begin
      with Params do
        begin
          NeededWindow := 0;
          EnumWindows( @WinEnumerator2, dword( @Params ) );
          Result := NeededWindow;
        end;
    end;

  // FindViewerClient

  type
    PEnum3Params = ^TEnum3Params;
    TEnum3Params =
      record
        NeededWindow   : HWND;
        ScrollbarCount : integer;
      end;

  function WinEnumerator3( Window : HWND; lParam : dword ) : Bool; stdcall;
    var
      Params    : PEnum3Params absolute lParam;
      WinStyle  : integer;
      ClassName : string;
    begin
      ClassName := GetWindowClass( Window );
      WinStyle  := GetWindowStyle( Window );

      if ( ClassName = 'ScrollBar' ) or ( WinStyle and ( WS_VSCROLL or WS_HSCROLL ) <> 0 )
        then inc( Params.ScrollbarCount );
      Result := true;
    end;

  function WinEnumerator4( Window : HWND; lParam : dword ) : Bool; stdcall;
    var
      Params : PEnum3Params absolute lParam;
    begin
      with Params^ do
        begin
          ScrollbarCount := 0;
          EnumChildWindows( Window, @WinEnumerator3, dword( Params ) );
          if ScrollbarCount = 2
            then NeededWindow := Window;
          Result := ( NeededWindow = 0 );
        end;
    end;

  function FindViewerClient( Viewer : HWND ) : HWND;
    var
      Params : TEnum3Params;
    begin
      with Params do
        begin
          NeededWindow := 0;
          EnumChildWindows( Viewer, @WinEnumerator4, dword( @Params ) );
          Result := NeededWindow;
        end;
    end;

end.
