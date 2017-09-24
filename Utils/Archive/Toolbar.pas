unit Toolbar;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CommCtrl,
    Buffer, Dibs, GDI, CommCtrl4;

  type
    TToolImage  = TSpeedBitmap;
    TAxlSortKey = integer;

  type
    TToolbarTool = class;

    TToolAction   = procedure ( Which  : TToolbarTool; Info : pointer ) of object;
    TToolbarEvent = procedure ( Sender : TToolbarTool ) of object;

    TToolbar =
      class( TWinControl )
        private
          fWndProcData   : dword;
          fLockCount     : integer;
          fToolcount     : integer;
          fTooltips      : TStringList;
          fTextLabels    : TStringList;
          fOnClickEvents : TList;

        protected
          function  GetTool( Indx : integer ) : TToolbarTool;
          procedure SetStyle( aStyle : integer );
          procedure SetShowText( aShowText : boolean );
          procedure SetFlatMode( aFlatMode : boolean );
          procedure SetListMode( aListMode : boolean );
          procedure SetTooltip( Indx : integer; aTooltip : string );
          function  GetStyle : integer;
          function  GetShowText : boolean;
          function  GetFlatMode : boolean;
          function  GetListMode : boolean;
          function  GetTooltip( Indx : integer ) : string;
          function  GetItem( Indx : integer ) : TToolbarItem;
          function  GetTool( Indx : integer ) : TToolbarTool;

          procedure SetParent( AParent : TWinControl ); override;

          property Tooltips : TStringList read GetTooltip  write SetTooltip;

        public
          property ShowText  : boolean    read GetShowText write SetShowText;
          property FlatMode  : boolean    read GetFlatMode write SetFlatMode;
          property ListMode  : boolean    read GetListMode write SetListMode;
          property Style     : integer    read GetStyle    write SetStyle;

          property ToolCount : integer                    read fToolCount;
          property Items[ Indx : integer ] : TToolbarItem read GetItem; 
          property Tools[ Indx : integer ] : TToolbarTool read GetTool;

        public
          procedure Add( Which : TToolbarTool );
          procedure Delete( Which : TToolbarTool );
          procedure Remove( Indx : integer );
          procedure ForEach( Action : TToolAction; Info : pointer );

        public
          procedure Lock;
          procedure Unlock;
          function  Locked : boolean;
          procedure Update;

        public
          function  SearchByCommand( CommandId : dword ) : TToolbarTool;

        public
          constructor Create( AOwner : TComponent ); override;
      end;

    TToolbarItem =
      class
        protected
          fOwner    : TToolbar;
          fItemIndx : integer;

        public
          property Owner : TToolbar    read fOwner;

        public
          constructor Create( anOwner : TToolbar ); override;
      end;

    TToolbarTool =
      class( TToolbarItem )
        protected
          fOwner     : TToolbar;
          fToolIndx  : integer;

          function  GetEnabled : boolean;
          function  GetVisible : boolean;
          function  GetTextLabel : string;
          function  GetSelected : boolean;
          function  GetCommandId : boolean;
          function  GetTooltip : string;
          function  GetOnClick : TToolbarEvent;

          procedure SetEnabled( aEnabled : boolean );
          procedure SetVisible( aVisible  : boolean );
          procedure SetTextLabel( aTextLabel  : string );
          procedure SetSelected( aSelected  : boolean );
          procedure SetCommandId( aCommandId  : boolean );
          procedure SetTooltip( aTooltip  : string );
          procedure SetOnClick( anEvent : TToolbarEvent );

          property CommandId : dword       read GetCommandId write SetCommandId;

        public
          property Enabled   : boolean     read GetEnabled   write SetEnabled;
          property Visible   : boolean     read GetVisible   write SetVisible;
          property Pressed   : boolean     read GetPressed   write SetPressed;
          property TextLabel : string      read GetTextLabel ;//write ...;
          property Selected  : boolean     read GetSelected  write SetSelected;
//          property SortKey   : TAxlSortKey    read ...;
          property ToolTip   : string      read GetTooltip   write SetTooltip;
//          property Image     : TToolImage  read ... write ...;
          property OnClick   : TToolbarEvent read GetOnClick write SetOnClick;

        public
          constructor Create( anOwner : TToolbar ); override;
      end;

  type
    // A separator between two tools
    TToolbarSeparator =
      class( TToolbarItem )
      end;

  type
    TToolbarButton =
      class( TToolbarTool )
      end;

  // A standard combobox
  type
    TToolCombobox =
      class( TToolbarTool )
      end;

  // A button that shows a window when pressed
  type
    TToolDropDownButton =
      class( TToolbarButton )
      end;

  // A button that drops down a list when pressed
  type
    TToolDropDownList =
      class( TToolbarDropDownButton )
      end;

  // A button that shows a window when pressed in the arrow, and does an action when pressed in the glyph
  type
    TToolComboButton =
      class( TToolbarButton )
      end;

implementation

  uses
    Debug, WinUtils;

  procedure TToolbar.SetParent( AParent : TWinControl );
    begin
      RestoreWndProc( Parent.Handle, WndProcData ); 
      inherited;
      Toolbar_SetParent( Parent.Handle );
(*
      if Parent <> nil
        then
          WndProcData := ChangeWndProc( Parent.Handle,
*)
    end;

  function TToolbar.Locked : boolean;
    begin
      Result := fLockCount <> 0;
    end;

  procedure TToolbar.Lock;
    begin
      inc( fLockCount );
    end;

  procedure TToolbar.Unlock;
    begin
      if fLockCount > 0
        then dec( fLockCount );
    end;

  function TToolbar.GetShowText : boolean;
    begin
      
    end;

  function TToolbar.GetFlatMode : boolean;
    begin
      Result := Style and TBSTYLE_FLAT <> 0;
    end;

  function TToolbar.GetListMode : boolean;
    begin
      Result := Style and TBSTYLE_LIST <> 0;
    end;

  procedure TToolbar.SetShowText( aShowText : boolean );
    begin
    end;

  procedure TToolbar.SetFlatMode( aFlatMode : boolean );
    begin
      Style := Style or TBSTYLE_FLAT;
    end;

  procedure TToolbar.SetListMode( aListMode : boolean );
    begin
      Style := Style or TBSTYLE_LIST;
    end;

  function TToolbar.GetStyle : integer;
    begin
      Result := Toolbar_GetStyle( Owner.Handle );
    end;

  procedure TToolbar.SetStyle( aStyle : integer );
    begin
      Toolbar_SetStyle( Owner.Handle, aStyle );
    end;

  procedure TToolbar.Add( Which : TToolbarTool );
    begin
    end;

  procedure TToolbar.Delete( Which : TToolbarTool );
    begin
    end;

  procedure TToolbar.Remove( Indx : integer );
    begin
    end;

  procedure TToolbar.ForEach( Action : TToolAction; Info : pointer );
    begin
    end;

  constructor TToolbar.Create(AOwner: TComponent);
    begin
      inherited;
      ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
        csSetCaption, csDoubleClicks, csReplicatable];
      Width  := 185;
      Height := 105;
    end;

  destructor TToolbar.Destroy;
    begin
      inherited;
    end;

  procedure TToolbar.CreateWindowHandle( const Params : TCreateParams );
    begin
      with Params do
        begin
          WindowHandle := CreateToolbarEx( WndParent, Style, ID_TOOLBAR, ,

          ExStyle, WinClassName, '', Style,
            X, Y, Width, Height, WndParent, 0, HInstance, Param);
          SendMessage(WindowHandle, WM_SETTEXT, 0, Longint(Caption));
        end;
    end;

  constructor TToolbarItem.Create( anOwner : TToolbar );
   begin
     inherited Create;

     fOwner := anOwner;
   end;

  constructor TToolbarTool.Create( anOwner : TToolbar );
   begin
     inherited;
   end;

  function TToolbarTool.GetEnabled : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_IsButtonEnabled( Parent.Handle, Data.idCommand );
    end;

  function TToolbarTool.GetVisible : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := not Toolbar_IsButtonHidden( Parent.Handle, Data.idCommand );
    end;

  function TToolbarTool.GetSelected : boolean;
    begin
    end;

  function TToolbarTool.GetCommandId : dword;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      //!!
    end;

  function TToolbarTool.GetTextLabel : string;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_GetButtonText( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarTool.SetEnabled( aEnabled : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_EnableButton( Parent.Handle, Data.idCommand, aEnabled );
    end;

  procedure TToolbarTool.SetVisible( aEnabled : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_HideButton( Parent.Handle, Data.idCommand, not aVisible );
    end;

  procedure TToolbarTool.SetSelected( aEnabled : boolean );
    begin
    end;

  procedure TToolbarTool.SetCommandId( aCommandId : dword );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_SetCmdId( Parent.Handle, Indx, aCommand );
      GetButtonData;  // Update data
    end;

  function TToolbarTool.GetTooltip : string;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Tooltips[fToolIndx];
    end;

  procedure TToolbarTool.SetTooltip( aTooltip : string );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Tooltips[fToolIndx] := aTooltip;
    end;

  procedure TToolbarTool.SetOnClick( anEvent : TToolbarEvent );
    begin
      Owner.OnClick[fToolIndx] := anEvent;
    end;

  function TToolbarTool.GetOnClick : TToolbarEvent;
    begin
      Result := Owner.OnClick[fToolIndx];
    end;

end.

(*
  type
    PButtonsData = ^TButtonsData;
    TButtonsData = array[0..0] of TTBButton;

  const
    stChecked       = TBSTATE_CHECKED;
    stPressed       = TBSTATE_PRESSED;
    stEnabled       = TBSTATE_ENABLED;
    stHidden        = TBSTATE_HIDDEN;
    stIndeterminate = TBSTATE_INDETERMINATE;
    stWrap          = TBSTATE_WRAP;

  type
    TButtonList = class;
    TCustomSpeedbar = class;

    TToolbarButton =
      class
        protected
          fParent : TCustomSpeedbar;
          fData   : PTBButton;
          fIndx   : integer;

        published
          property Data : PTBButton         read fData;
          property Parent : TCustomSpeedbar read fParent;
          property Indx : integer           read fIndx;

          property Enabled : boolean        read GetEnabled       write SetEnabled       default true;
          property Visible : boolean        read GetVisible       write SetVisible       default true;
          property Checked : boolean        read GetChecked       write SetChecked       default false;
          property Pressed : boolean        read GetPressed       write SetPressed       default false;
          property Indeterminate : boolean  read GetIndeterminate write SetIndeterminate default false;
          property LineBreak : boolean      read GetLineBreak     write SetLineBreak     default false;
          property StateFlags : integer     read GetStateFlags    write SetStateFlags;
          property BitmapIndx : integer     read GetBitmapIndx    write SetBitmapIndx;
          property Command : integer        read GetCommand       write SetCommand;
          property LabelStr : string        read GetLabelStr;
      end;

    TButtonList =
      class
        protected
          Owner : TCustomSpeedbar;

      end;

    TCustomSpeedbar =
      class( TWinControl )
        private

        protected
          fButtonList     : TButtonList;
          fButtonsData    : PButtonsData;
          fRegistryKey    : HKEY;
          fRegistrySubKey : string;
          fRegistryValue  : string;

          fNormalImageList   : HIMAGELIST;
          fHotImageList      : HIMAGELIST;
          fDisabledImageList : HIMAGELIST;

          procedure CreateParams( var Params : TCreateParams );                                   override;
          procedure CreateWnd;                                                                    override;
          procedure DestroyWnd;                                                                   override;

          function  CommandToIndx( Command : integer ) : integer;
          function  GetButton( Indx : integer ) : TToolbarButton;
          function  GetButtonByCommand( Command : integer ) : TToolbarButton;

        public
          property Buttons[ Indx : integer ] : TToolbarButton             read GetButton;
          property ButtonsByCommand[ Command : integer ] : TToolbarButton read GetButtonByCommand;

          property NormalImageList   : HIMAGELIST read fNormalImageList;
          property HotImageList      : HIMAGELIST read fHotImageList;
          property DisabledImageList : HIMAGELIST read fDisabledImageList;

          procedure AutoSize;

          procedure DeleteButton( Indx : integer );
          procedure AddButtons( Count : integer; ButtonsData : PButtonsData );

          procedure AddResString( ResId : integer );
          procedure AddString( ToolbarStr : string );
          procedure AddStrings( ToolbarStrs : pchar );

          procedure SaveState;
          procedure RestoreState;

        public
          property ButtonList : TButtonList read fButtonList;

          property RegistryKey    : HKEY   read fRegistryKey    write fRegistryKey;
          property RegistrySubKey : string read fRegistrySubKey write fRegistrySubKey;
          property RegistryValue  : string read fRegistryValue  write fRegistryValue;
      end;

implementation

  function TToolbarButton.GetIndeterminate : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_IsButtonIndeterminate( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarButton.SetIndeterminate( aIndeterminate : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_IndeterminateButton( Parent.Handle, Data.idCommand, aIndeterminate );
    end;

  function TToolbarButton.GetChecked : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_IsButtonChecked( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarButton.SetChecked( aChecked : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_CheckButton( Parent.Handle, Data.idCommand, aChecked );
    end;

  function TToolbarButton.GetPressed : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_IsButtonPressed( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarButton.SetPressed( aPressed : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_PressButton( Parent.Handle, Data.idCommand, aPressed );
    end;

  function  TToolbarButton.GetBitmapIndx : integer;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_GetBitmap( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarButton.SetBitmapIndx( aBitmapIndx : integer );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_ChangeBitmap( Parent.Handle, Data.idCommand, aBitmapIndx );
    end;

  function TToolbarButton.GetLineBreak : boolean;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := ( StateFlags and not stWrap ) <> 0;
    end;

  procedure TToolbarButton.SetLineBreak( aLineBreak : boolean );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
     if aLineBreak
       then StateFlags := StateFlags or stWrap
       else StateFlags := StateFlags and not stWrap;
    end;

  function TToolbarButton.GetStateFlags : integer;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_GetButtonState( Parent.Handle, Data.idCommand );
    end;

  procedure TToolbarButton.SetStateFlags( aStateFlags : integer );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_SetButtonState( Parent.Handle, Data.idCommand, aStateFlags );
    end;

  function TToolbarButton.GetButtonData : PTBButton;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := fData;
      Toolbar_GetButton( Parent.Handle, Data.idCommand, Result^ );
    end;

  // TSpeedbar

  function TCustomSpeedbar.CommandToIndx( Command : integer ) : integer;
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      Result := Toolbar_CommandToIndex( Handle, Command );
    end;

  function TCustomSpeedbar.GetButton( Indx : integer ) : TToolbarButton;
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      //Result := @ButtonsData[ Indx ];
    end;

  function TCustomSpeedbar.GetButtonByCommand( Command : integer ) : TToolbarButton;
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      //Result := @ButtonsData[ CommandToIndx( Command ) ];
    end;

  function TCustomSpeedbar.GetStyleFlags : integer;
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Result := Toolbar_GetStyle( Parent.Handle );
    end;

  procedure TCustomSpeedbar.SetStyleFlags( aStyleFlags : integer );
    begin
      if Debugging
        then assert( Parent.Handle <> 0, 'NULL handle in TToolbarButton.xxx' );
      Toolbar_SetStyle( Parent.Handle, aStyleFlags );
    end;

  procedure TCustomSpeedbar.AutoSize;
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      Toolbar_Autosize( Handle );
    end;

  procedure TCustomSpeedbar.DeleteButton( Indx : integer );
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      Toolbar_DeleteButton( Handle, Indx );
    end;

  procedure TCustomSpeedbar.AddButtons( Count : integer; ButtonsData : PButtonsData );
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      Toolbar_AddButtons( Handle, Count, ButtonsData );
    end;

  procedure TCustomSpeedbar.AddResString( ResId : integer );
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      Toolbar_AddString( Handle, hInstance, ResId );
    end;

  procedure TCustomSpeedbar.AddString( ToolbarStr : string );
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
      //Toolbar_AddStrings(
    end;

  procedure TCustomSpeedbar.AddStrings( ToolbarStrs : pchar );
    begin
      if Debugging
        then assert( Handle <> 0, 'NULL handle in TCustomSpeedbar.xxx' );
    end;

==================================================================================

RESULT CMfctoolView::WindowProc (UINT message, WPARAM wParam,
   LPARAM lParam)
{
static CHAR szBuf [128];
LPTOOLTIPTEXT lpToolTipText;

if (message == WM_NOTIFY)
{
   switch (((LPNMHDR)lParam)->code)
   {
      case TTN_NEEDTEXT:
         // Display the ToolTip text.
         lpToolTipText = (LPTOOLTIPTEXT)lParam;
         ::LoadString (AfxGetResourceHandle (),
            lpToolTipText->hdr.idFrom,   // string ID == cmd ID
            szBuf,
            sizeof (szBuf));
         lpToolTipText->lpszText = szBuf;
         break;

==================================================================================

// This code is in the main window procedure after the combo box
// has been created.
// Set the window procedure for the combo box.
lpfnDefCombo = (WNDPROC) GetWindowLong (hWndCombo, GWL_WNDPROC);
SetWindowLong (hWndCombo, GWL_WNDPROC, (LONG)ComboWndProc);

// Get the handle to the ToolTip window.
hWndTT = (HWND) SendMessage (hWndToolbar, TB_GETTOOLTIPS, 0, 0);

if (hWndTT)
{
   // Fill out the TOOLINFO structure.
   lpToolInfo.cbSize = sizeof (lpToolInfo);
   // The uID is the handle of the tool (the combo box).
   lpToolInfo.uFlags = TTF_IDISHWND | TTF_CENTERTIP;
   // The string ID in the resource
   lpToolInfo.lpszText = (LPSTR)IDM_COMBO;
   // The window that gets the ToolTip messages
   lpToolInfo.hwnd = hWnd;
   // The tool
   lpToolInfo.uId = (UINT)hWndCombo;
   // The instance that owns the string resource
   lpToolInfo.hinst = hInst;

   // Set up the ToolTip for the combo box.
   SendMessage (hWndTT, TTM_ADDTOOL, 0,
      (LPARAM)(LPTOOLINFO)&lpToolInfo);
}

==================================================================================

// This function relays the mouse messages from the combo box
// to get the ToolTip to work.
LRESULT CALLBACK ComboWndProc (HWND hWnd, UINT uMessage, WPARAM wParam,
   LPARAM lParam)
{
switch (uMessage)
{
   case WM_MOUSEMOVE:
   case WM_LBUTTONDOWN:
   case WM_LBUTTONUP:
   {
      MSG msg;
      HWND hWndTT;
      msg.lParam = lParam;
      msg.wParam = wParam;
      msg.message = uMessage;
      msg.hwnd = hWnd;
      hWndTT = (HWND) SendMessage (hWndToolbar, TB_GETTOOLTIPS, 0, 0);
      SendMessage (hWndTT, TTM_RELAYEVENT, 0, (LPARAM)(LPMSG)&msg);
      break;
   }
}
return CallWindowProc (lpfnDefCombo, hWnd, uMessage, wParam, lParam);
}

The corresponding MFC procedure is similar. One change I made was to create a class for my combo-box
control derived from CComboBox and use ClassWizard to create a message map to WindowProc. Within
this function, I did the same type of processing—except that it was less tedious to fill out the
message structure. Instead, I was able to call CWnd::GetCurrentMessage.

==================================================================================

Call this function to resize the entire toolbar control. You should call this function when the
size of the parent window changes or when the size of the toolbar changes (such as when you set
the button or bitmap size, or add strings).

==================================================================================

*)




