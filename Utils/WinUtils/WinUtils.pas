unit WinUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils;

  // Message Queue Inspection ------------------------------------------------------

  function WindowHasMessageWaiting( WinHandle : HWND ) : boolean;

  const // See QS_XXX flags in GetQueueStatus
    QS_USERINPUT = QS_KEY or QS_MOUSEBUTTON or QS_HOTKEY;
    QS_IMPORTANT = QS_USERINPUT or QS_POSTMESSAGE;

  function ThreadHasInputWaiting : boolean;
  function ThreadHasMessageWaiting : boolean;
  function ThreadHasImportantMessageWaiting : boolean;
  function ThreadHasTheseMessagesWaiting( Mask : integer ) : boolean; // Mask = QS_XXX flags

  // Window searching -----------------------------------------------------------------

  function GetWindowOfClass( const ClassName : string ) : HWND;
  function GetWindowOfCaption( const Caption : string ) : HWND;

  // Window info -----------------------------------------------------------------

  function  GetWindowCaption( WinHandle : HWND ) : string;
  function  GetWindowClass( WinHandle : HWND ) : string;

  function  GetWindowInstance( WinHandle : HWND ) : HMODULE;
  function  GetWindowID( WinHandle : HWND ) : integer;

  // Window styles ----------------------------------------------------------------

  procedure ExcludeWindowStyle( WinHandle : HWND; NewStyle : integer );
  procedure IncludeWindowStyle( WinHandle : HWND; NewStyle : integer );
  procedure SetWindowStyle( WinHandle : HWND; NewStyle : integer );
  function  GetWindowStyle( WinHandle : HWND ) : integer;

  procedure ExcludeWindowExStyle( WinHandle : HWND; NewExStyle : integer );
  procedure IncludeWindowExStyle( WinHandle : HWND; NewExStyle : integer );
  procedure SetWindowExStyle( WinHandle : HWND; NewExStyle : integer );
  function  GetWindowExStyle( WinHandle : HWND ) : integer;

  // Window list -----------------------------------------------------------------

  function  GetWindowOwner( WinHandle : HWND ) : HWND;
  function  GetFirstChild( WinHandle : HWND ) : HWND;
  function  GetFirstSibling( WinHandle : HWND ) : HWND;
  function  GetLastSibling( WinHandle : HWND ) : HWND;
  function  GetNextSibling( WinHandle : HWND ) : HWND;
  function  GetPrevSibling( WinHandle : HWND ) : HWND;

  // Window Visuals -----------------------------------------------------------------

  procedure SetWindowRedraw( WinHandle : HWND; Redraw : boolean );    // For compatibility with Windowsx.h
  procedure LockWindowPainting( WinHandle : HWND; Locked : boolean ); // Use this ones instead
  procedure UnlockPainting( WinHandle : HWND );

  function  IsMinimized( WinHandle : HWND ) : boolean;
  function  IsMaximized( WinHandle : HWND ) : boolean;
  function  IsRestored( WinHandle : HWND ) : boolean;

  procedure MaximizeWindow( WinHandle : HWND );
  procedure MinimizeWindow( WinHandle : HWND );
  procedure RestoreWindow( WinHandle : HWND );

  procedure ActivateWindow( WinHandle : HWND );         // Brings window to front, and gives it the focus
  procedure ShowDontActivateWindow( WinHandle : HWND ); // Only make sure it's visible
  procedure TopmostWindow( WinHandle : HWND );          // Make window topmost
  procedure HideWindow( WinHandle : HWND );
  procedure ShowWindow( WinHandle : HWND );

  procedure UpdateWindowFrame( WinHandle : HWND );                        // Repaint window borders & caption
  procedure SetWindowSizeable( WinHandle : HWND; WinSizeable : boolean ); // Allow/Disable resizing
  function  WindowIsSizeable( WinHandle : HWND) : boolean;

  function  GetRealClientRect( WinHandle : HWND ) : TRect;

  function  WindowIsDropTarget( WinHandle : HWND) : boolean; // True if this window accepts files dropping

  // Window subclassing -----------------------------------------------------------------

  type
    EWndProcCannotBeRestored = class( Exception );

  function SubclassWindow( WinHandle : integer; NewWndProc : pointer ) : pointer; // For compatibility with Windowsx.h
  function CurrentWndProc( WinHandle : HWND ) : pointer;

  function ChangeWndProc( WinHandle : HWND; NewWndProc : pointer ) : integer;
  function RestoreWndProc( WinHandle : HWND; Data : integer ) : pointer;
  function PrevWndProc( Data : dword ) : pointer;

implementation

  // Window Messages -----------------------------------------------------------------

  function WindowHasMessageWaiting( WinHandle : HWND ) : boolean;
    var
      Msg : TMsg;
    begin
      Result := PeekMessage( Msg, WinHandle, 0, 0, PM_NOREMOVE );
    end;

  function ThreadHasInputWaiting : boolean;
    begin
      Result := GetInputState;
    end;

  function ThreadHasMessageWaiting : boolean;
    begin
      Result := GetQueueStatus( QS_ALLINPUT ) <> 0;
    end;

  function ThreadHasImportantMessageWaiting : boolean;
    begin
      Result := GetQueueStatus( QS_IMPORTANT ) <> 0;
    end;

  function ThreadHasTheseMessagesWaiting( Mask : integer ) : boolean;
    begin
      Result := GetQueueStatus( Mask ) <> 0;
    end;

  // Window info -----------------------------------------------------------------

  function GetWindowCaption( WinHandle : HWND ) : string;
    begin
      SetLength( Result, MAX_PATH );
      SetLength( Result, GetWindowText( WinHandle, pchar( Result ), length( Result ) ) );
    end;

  function GetWindowClass( WinHandle : HWND ) : string;
    begin
      SetLength( Result, MAX_PATH );
      SetLength( Result, GetClassName( WinHandle, pchar( Result ), length( Result ) ) );
    end;

  function GetWindowStyle( WinHandle : HWND ) : integer;
    begin
      Result := GetWindowLong( WinHandle, GWL_STYLE );
    end;

  procedure SetWindowStyle( WinHandle : HWND; NewStyle : integer );
    begin
      SetWindowLong( WinHandle, GWL_STYLE, NewStyle );
    end;

  procedure IncludeWindowStyle( WinHandle : HWND; NewStyle : integer );
    begin
      NewStyle := GetWindowStyle( WinHandle ) or NewStyle;
      SetWindowLong( WinHandle, GWL_STYLE, NewStyle );
    end;

  procedure ExcludeWindowStyle( WinHandle : HWND; NewStyle : integer );
    begin
      NewStyle := GetWindowStyle( WinHandle ) and (not NewStyle);
      SetWindowLong( WinHandle, GWL_STYLE, NewStyle );
    end;

  function GetWindowExStyle( WinHandle : HWND ) : integer;
    begin
      Result := GetWindowLong( WinHandle, GWL_EXSTYLE );
    end;

  procedure SetWindowExStyle( WinHandle : HWND; NewExStyle : integer );
    begin
      SetWindowLong( WinHandle, GWL_EXSTYLE, NewExStyle );
    end;

  procedure IncludeWindowExStyle( WinHandle : HWND; NewExStyle : integer );
    begin
      NewExStyle := GetWindowExStyle( WinHandle ) or NewExStyle;
      SetWindowLong( WinHandle, GWL_EXSTYLE, NewExStyle );
    end;

  procedure ExcludeWindowExStyle( WinHandle : HWND; NewExStyle : integer );
    begin
      NewExStyle := GetWindowExStyle( WinHandle ) and (not NewExStyle);
      SetWindowLong( WinHandle, GWL_EXSTYLE, NewExStyle );
    end;

  function GetWindowOwner( WinHandle : HWND ) : HWND;
    begin
      Result := GetWindow( GW_OWNER, WinHandle );
    end;

  function GetWindowInstance( WinHandle : HWND ) : HMODULE;
    begin
      Result := GetWindowLong( WinHandle, GWL_HINSTANCE);
    end;

  function GetFirstChild( WinHandle : HWND ) : HWND;
    begin
      Result := GetTopWindow( WinHandle );
    end;

  function GetFirstSibling( WinHandle : HWND ) : HWND;
    begin
      Result := GetWindow( WinHandle, GW_HWNDFIRST );
    end;

  function GetLastSibling( WinHandle : HWND ) : HWND;
    begin
      Result := GetWindow( WinHandle, GW_HWNDLAST );
    end;

  function GetNextSibling( WinHandle : HWND ) : HWND;
    begin
      Result := GetWindow( WinHandle, GW_HWNDNEXT );
    end;

  function GetPrevSibling( WinHandle : HWND ) : HWND;
    begin
      Result := GetWindow( WinHandle, GW_HWNDPREV );
    end;

  function GetWindowID( WinHandle : HWND ) : integer;
    begin
      Result := GetDlgCtrlID( WinHandle );
    end;

  // Window searching -----------------------------------------------------------------

  function GetWindowOfCaption( const Caption : string ) : HWND;
    begin
      Result := FindWindow( nil, pchar(Caption) );
    end;

  function GetWindowOfClass( const ClassName : string ) : HWND;
    begin
      Result := FindWindow( pchar(ClassName), nil );
    end;

  // Window Visuals -----------------------------------------------------------------

  procedure SetWindowRedraw( WinHandle : HWND; Redraw : boolean );
    begin
      SendMessage( WinHandle, WM_SETREDRAW, WPARAM( Redraw ), 0 );
    end;

  procedure LockWindowPainting( WinHandle : HWND; Locked : boolean );
    begin
      SendMessage( WinHandle, WM_SETREDRAW, WPARAM( not Locked ), 0 );
    end;

  procedure UnlockPainting( WinHandle : HWND );
    begin
      LockWindowPainting( WinHandle, false );
      InvalidateRect( WinHandle, nil, false );
    end;

  function IsMinimized( WinHandle : HWND ) : boolean;
    begin
      Result := IsIconic( WinHandle );
    end;

  function IsMaximized( WinHandle : HWND ) : boolean;
    begin
      Result := IsZoomed( WinHandle );
    end;

  function IsRestored( WinHandle : HWND ) : boolean;
    begin
      Result := GetWindowStyle( WinHandle ) and ( WS_MINIMIZE or WS_MAXIMIZE ) = 0;
    end;

  procedure MaximizeWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_MAXIMIZE );
    end;

  procedure MinimizeWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_MINIMIZE );
    end;

  procedure RestoreWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_RESTORE );
    end;

  procedure HideWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_HIDE );
    end;

  procedure ShowWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_SHOWNORMAL );
    end;

  procedure ActivateWindow( WinHandle : HWND );
    begin
      Windows.ShowWindow( WinHandle, SW_SHOWNORMAL );
      SetForegroundWindow( WinHandle );
    end;

  procedure TopmostWindow( WinHandle : HWND );
    begin
      SetWindowPos( WinHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE );
    end;

  procedure ShowDontActivateWindow( WinHandle : HWND );
    begin
      SetWindowPos( WinHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE );
    end;

  procedure UpdateWindowFrame( WinHandle : HWND );
    begin
      SetWindowPos( WinHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOZORDER );
    end;

  procedure SetWindowSizeable( WinHandle : HWND; WinSizeable : boolean );
    begin
      if WinSizeable
        then IncludeWindowStyle( WinHandle, WS_SIZEBOX )
        else ExcludeWindowStyle( WinHandle, WS_SIZEBOX );
      UpdateWindowFrame( WinHandle );
    end;

  function WindowIsSizeable( WinHandle : HWND ) : boolean;
    begin
      Result := ( GetWindowStyle( WinHandle ) and WS_SIZEBOX ) <> 0;
    end;

  function GetRealClientRect( WinHandle : HWND ) : TRect;
    var
      WinStyle : dword;
    begin
      WinStyle := GetWindowStyle( WinHandle );
      GetClientRect( WinHandle, Result );
      with Result do
        begin
          if WinStyle and WS_HSCROLL <> 0
            then inc( Bottom, GetSystemMetrics( SM_CYHSCROLL ) );
          if WinStyle and WS_VSCROLL <> 0
            then inc( Right, GetSystemMetrics( SM_CXVSCROLL ) );
        end;
    end;

  function WindowIsDropTarget( WinHandle : HWND) : boolean;
    begin
      Result := ( GetWindowExStyle( WinHandle ) and WS_EX_ACCEPTFILES ) <> 0;
    end;

  function SubclassWindow( WinHandle : integer; NewWndProc : pointer ) : pointer; 
    begin
      Result := pointer( SetWindowLong( WinHandle, GWL_WNDPROC, longint(NewWndProc) ) );
    end;

  function CurrentWndProc( WinHandle : HWND ) : pointer;
    begin
      Result := pointer( GetWindowLong( WinHandle, GWL_WNDPROC ) );
    end;

  // -----------------------------------------------------------------

  type
    PDoubleLinkedNode = ^TDoubleLinkedNode;
    TDoubleLinkedNode =
      record
        Prev, Next : PDoubleLinkedNode;
        Data       : integer;
      end;

  function NewNode( aPrev, aNext : pointer; aData : dword ) : PDoubleLinkedNode;
    begin
      new( Result );
      with Result^ do
        begin
          Prev := aPrev;
          Next := aNext;
          Data := aData;
        end;
    end;

  procedure FreeNode( Node : PDoubleLinkedNode );
    begin
      with Node^ do
        begin                // Chain Prev & Next
          if Prev <> nil
            then Prev.Next := Next;
          if Next <> nil
            then Next.Prev := Prev;
        end;
      Dispose( Node );
    end;

  function PrevWndProc( Data : dword ) : pointer;
    var
      Node : PDoubleLinkedNode absolute Data;
    begin
      Result := pointer( Node.Prev.Data )
    end;

  function ChangeWndProc( WinHandle : HWND; NewWndProc : pointer ) : integer;
    var
      ListAtom : pchar;
      List     : PDoubleLinkedNode;
      Node     : PDoubleLinkedNode absolute Result;
    begin
      ListAtom := pchar( GlobalAddAtom( 'WndProc_List' ) );
      List     := PDoubleLinkedNode( GetProp( WinHandle, ListAtom ) );
      if List = nil
        then
          begin  // Create list
            List := NewNode( nil, nil, integer( CurrentWndProc( WinHandle ) ) );
            SetProp( WinHandle, ListAtom, dword( List ) );
          end;

      while ( List.Next <> nil ) and (List.Data <> integer( NewWndProc ) ) do
        List := List.Next;
      if List.Next = nil
        then
          begin
            Node := NewNode( List, nil, integer( NewWndProc ) );
            List.Next := Node;

            SubclassWindow( WinHandle, NewWndProc );
          end;
    end;

  function RestoreWndProc( WinHandle : HWND; Data : integer ) : pointer;
    var
      ListAtom : pchar;
      List     : PDoubleLinkedNode;
      Node     : PDoubleLinkedNode absolute Data;
    begin
      if Data <> 0
        then
          begin
            ListAtom := pchar( GlobalAddAtom( 'WndProc_List' ) );
            List     := PDoubleLinkedNode( GetProp( WinHandle, ListAtom ) );
            with Node^ do
              begin
                if Next = nil
                  then Result := SubclassWindow( WinHandle, pointer( Prev.Data ) )
                  else Result := CurrentWndProc( WinHandle );

                if Prev = List  // Free list
                  then
                    begin
                      FreeNode( List );
                      RemoveProp( WinHandle, ListAtom );
                    end;
              end;
            FreeNode( Node );
          end
        else Result := nil;
    end;

end.
