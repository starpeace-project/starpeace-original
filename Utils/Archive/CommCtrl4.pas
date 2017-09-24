unit CommCtrl4;

interface

  uses
    Windows, Messages, CommCtrl, Classes;

  const
    cctrl = 'comctl32.dll';
    
  type
    PInitCommonControlsEx = ^TInitCommonControlsEx;
    TInitCommonControlsEx =
      packed record
        dwSize : longint;
        dwICC  : longint;
      end;

  const
    ICC_LISTVIEW_CLASSES = $00000001; // listview, header
    ICC_TREEVIEW_CLASSES = $00000002; // treeview, tooltips
    ICC_BAR_CLASSES      = $00000004; // toolbar, statusbar, trackbar, tooltips
    ICC_TAB_CLASSES      = $00000008; // tab, tooltips
    ICC_UPDOWN_CLASS     = $00000010; // updown
    ICC_PROGRESS_CLASS   = $00000020; // progress
    ICC_HOTKEY_CLASS     = $00000040; // hotkey
    ICC_ANIMATE_CLASS    = $00000080; // animate
    ICC_WIN95_CLASSES    = $0000000F;
    ICC_DATE_CLASSES     = $00000100; // month picker, date picker, time picker, updown
    ICC_USEREX_CLASSES   = $00000200; // comboex
    ICC_COOL_CLASSES     = $00000400; // rebar ( coolbar ) control

  function InitCommonControlsEx( icc : PINITCOMMONCONTROLSEX ) : BOOL; stdcall;

  //====== Generic WM_NOTIFY notification codes =================================

  const
    NM_CUSTOMDRAW = ( NM_FIRST - 12 );
    NM_HOVER = ( NM_FIRST - 13 );

  //====== WM_NOTIFY codes ( NMHDR.code values ) ==================================

  const
    MCN_FIRST  = 0 - 750; // monthcal
    MCN_LAST   = 0 - 759;

    DTN_FIRST  = 0 - 760; // datetimepick
    DTN_LAST   = 0 - 799;

    CBEN_FIRST = 0 - 800; // combo box ex
    CBEN_LAST  = 0 - 830;

    RBN_FIRST  = 0 - 831; // rebar
    RBN_LAST   = 0 - 859;

  //==================== CUSTOM DRAW ==========================================

  // custom draw return flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  const
    CDRF_DODEFAULT = $00000000;
    CDRF_NEWFONT = $00000002;
    CDRF_SKIPDEFAULT = $00000004;

    CDRF_NOTIFYPOSTPAINT = $00000010;
    CDRF_NOTIFYITEMDRAW = $00000020;
    CDRF_NOTIFYPOSTERASE = $00000040;
    CDRF_NOTIFYITEMERASE = $00000080;

  // drawstage flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  const
    CDDS_PREPAINT = $00000001;
    CDDS_POSTPAINT = $00000002;
    CDDS_PREERASE = $00000003;
    CDDS_POSTERASE = $00000004;

  // the 0x000010000 bit means it's individual item specific
  const
    CDDS_ITEM = $00010000;
    CDDS_ITEMPREPAINT  = CDDS_ITEM or CDDS_PREPAINT;
    CDDS_ITEMPOSTPAINT = CDDS_ITEM or CDDS_POSTPAINT;
    CDDS_ITEMPREERASE  = CDDS_ITEM or CDDS_PREERASE;
    CDDS_ITEMPOSTERASE = CDDS_ITEM or CDDS_POSTERASE;

  // itemState flags
  const
    CDIS_SELECTED = $0001;
    CDIS_GRAYED   = $0002;
    CDIS_DISABLED = $0004;
    CDIS_CHECKED  = $0008;
    CDIS_FOCUS    = $0010;
    CDIS_DEFAULT  = $0020;
    CDIS_HOT      = $0040;

  type
    PNMCustomDraw = ^TNMCustomDraw;
    TNMCustomDraw =
      packed record
        hdr         : TNMHDR;
        dwDrawStage : longint;
        hdc         : HDC;
        rc          : TRect;
        dwItemSpec  : longint;
        uItemState  : word;
        lItemlParam : longint;
      end;

  // for tooltips

  type
    PNMTTCustomDraw = ^TNMTTCustomDraw;
    TNMTTCustomDraw =
      packed record
        nmcd       : TNMCustomDraw;
        uDrawFlags : word;
      end;

  //====== IMAGE APIS ===========================================================

  type
    PImageListDrawParams = ^TImageListDrawParams;
    TImageListDrawParams =
      packed record
        cbSize     : longint;
        himl       : HIMAGELIST;
        i          : integer;
        hdcDst     : HDC;
        x          : integer;
        y          : integer;
        cx         : integer;
        cy         : integer;
        xBitmap    : integer;
        yBitmap    : integer;
        rgbBk      : TColorRef;
        rgbFg      : TColorRef;
        fStyle     : word;
        dwRop      : longint;
      end;

  function ImageList_SetImageCount( himl : HIMAGELIST;
                                    uNewCount : word ) : BOOL; stdcall;

  function ImageList_DrawIndirect( pimldp : PIMAGELISTDRAWPARAMS ) : BOOL; stdcall;

  const
    ILD_ROP   = $0040;

  const
    ILCF_MOVE = $00000000;
    ILCF_SWAP = $00000001;

  function ImageList_Copy( himlDst : HIMAGELIST; iDst : integer; himlSrc : HIMAGELIST; iSrc : integer;
                           uFlags : word ) : BOOL; stdcall;

  type
    PImageInfo = ^TImageInfo;
    TImageInfo =
      packed record
        hbmImage : HBITMAP;
        hbmMask  : HBITMAP;
        Unused1  : integer;
        Unused2  : integer;
        rcImage  : TRect;
      end;

  //====== TOOLBAR CONTROL ======================================================

  const
    TBSTATE_ELLIPSES    = $40;

  const
    TBSTYLE_DROPDOWN    = $08;

    TBSTYLE_FLAT        = $0800;
    TBSTYLE_LIST        = $1000;
    TBSTYLE_CUSTOMERASE = $2000;

  const
    IDB_HIST_SMALL_COLOR = 8;
    IDB_HIST_LARGE_COLOR = 9;

  // icon indexes for standard view bitmap

  const
    HIST_BACK           = 0;
    HIST_FORWARD        = 1;
    HIST_FAVORITES      = 2;
    HIST_ADDTOFAVORITES = 3;
    HIST_VIEWTREE       = 4;

  type
    PTBSaveParamsA = ^TTBSaveParamsA;
    TTBSaveParamsA =
      packed record
        hkr          : HKEY;
        pszSubKey    : pchar;
        pszValueName : pchar;
      end;

  type
    TBSaveParams = TTBSaveParamsA;

  const
    TB_SETIMAGELIST         = WM_USER + 48;
    TB_GETIMAGELIST         = WM_USER + 49;
    TB_LOADIMAGES           = WM_USER + 50;
    TB_GETRECT              = WM_USER + 51; // wParam is the Cmd instead of index
    TB_SETHOTIMAGELIST      = WM_USER + 52;
    TB_GETHOTIMAGELIST      = WM_USER + 53;
    TB_SETDISABLEDIMAGELIST = WM_USER + 54;
    TB_GETDISABLEDIMAGELIST = WM_USER + 55;
    TB_SETSTYLE             = WM_USER + 56;
    TB_GETSTYLE             = WM_USER + 57;
    TB_GETBUTTONSIZE        = WM_USER + 58;
    TB_SETBUTTONWIDTH       = WM_USER + 59;
    TB_SETMAXTEXTROWS       = WM_USER + 60;
    TB_GETTEXTROWS          = WM_USER + 61;
    TB_GETBUTTONTEXT        = TB_GETBUTTONTEXTA;
    TB_SAVERESTORE          = TB_SAVERESTOREA;
    TB_ADDSTRING            = TB_ADDSTRINGA;

  const
    TBN_DROPDOWN = TBN_FIRST - 10;
    TBN_CLOSEUP  = TBN_FIRST - 11;

  type
    PNMToolbarA = ^TNMToolbarA;
    TNMToolbarA =
      packed record
        hdr      : TNMHDR;
        iItem    : integer;
        tbButton : TTBButton;
        cchText  : integer;
        pszText  : pchar;
      end;

  type
    TNMTOOLBAR   = TNMTOOLBARA;

  type
    TBNOTIFYA   = TNMTOOLBARA;
    TBNOTIFY    = TNMTOOLBAR;

  function Toolbar_AddBitmap( Toolbar : HWND; ButtonCount : integer; Data : PTBAddBitmap ) : BOOL;
  function Toolbar_AddButtons( Toolbar : HWND; ButtonCount : integer; var Buttons ) : BOOL;
  function Toolbar_AddString( Toolbar : HWND; hInst, idString : integer ) : integer;
  function Toolbar_AutoSize( Toolbar : HWND ) : BOOL;
  function Toolbar_ButtonCount( Toolbar : HWND ) : integer;
  function Toolbar_ButtonStructSize( Toolbar : HWND; StructSize : integer ) : BOOL;
  function Toolbar_ChangeBitmap( Toolbar : HWND; idButton, idxBitmap : integer ) : BOOL;
  function Toolbar_CheckButton( Toolbar : HWND; idButton : integer; Checked : BOOL ) : BOOL;
  function Toolbar_CommandToIndex( Toolbar : HWND; idButton : integer ) : integer;
  function Toolbar_Customize( Toolbar : HWND ) : BOOL;
  function Toolbar_DeleteButton( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_EnableButton( Toolbar : HWND; idButton : integer; Enabled : BOOL ) : BOOL;
  function Toolbar_GetBitmap( Toolbar : HWND; idButton : integer ) : integer;
  function Toolbar_GetBitmapFlags( Toolbar : HWND ) : BOOL;
  function Toolbar_GetButton( Toolbar : HWND; idButton : integer; var ButtonData : TTBButton ) : BOOL;
  function Toolbar_GetButtonSize( Toolbar : HWND ) : TPoint;
  function Toolbar_GetButtonState( Toolbar : HWND; idButton : integer ) : integer;
  function Toolbar_GetButtonText( Toolbar : HWND; idButton : integer ) : string;
  function Toolbar_GetDisabledImageList( Toolbar : HWND ) : HIMAGELIST;
  function Toolbar_GetHotImageList( Toolbar : HWND ) : HIMAGELIST;
  function Toolbar_GetImageList( Toolbar : HWND ) : HIMAGELIST;
  function Toolbar_GetItemRect( Toolbar : HWND; idButton : integer; var Rect : TRect ) : BOOL;
  function Toolbar_GetRect( Toolbar : HWND; idButton : integer; var Rect : TRect ) : BOOL;
  function Toolbar_GetRows( Toolbar : HWND ) : integer;
  function Toolbar_GetStyle( Toolbar : HWND ) : integer;
  function Toolbar_GetTextRows( Toolbar : HWND ) : integer;
  function Toolbar_GetToolTips( Toolbar : HWND ) : HWND;
  function Toolbar_HideButton( Toolbar : HWND; idButton : integer; Shown : BOOL ) : BOOL;
  function Toolbar_IndeterminateButton ( Toolbar : HWND; idButton : integer; Indeterminate : BOOL ) : BOOL;
  function Toolbar_InsertButton( Toolbar : HWND; idButton : integer; ButtonData : PTBButton ) : BOOL;
  function Toolbar_IsButtonChecked( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_IsButtonEnabled( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_IsButtonHidden( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_IsButtonIndeterminate( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_IsButtonPressed( Toolbar : HWND; idButton : integer ) : BOOL;
  function Toolbar_LoadImages( Toolbar : HWND; hInst, idBitmap : integer ) : BOOL;
  function Toolbar_PressButton( Toolbar : HWND; idButton : integer; Pressed : BOOL ) : BOOL;
  function Toolbar_SaveRestore( Toolbar : HWND; Save : BOOL; Params : TTBSaveParams ) : BOOL;
  function Toolbar_SetBitmapSize( Toolbar : HWND; Width, Height : word ) : BOOL;
  function Toolbar_SetButtonSize( Toolbar : HWND; Width, Height : word ) : BOOL;
  function Toolbar_SetButtonState( Toolbar : HWND; idButton, State : integer ) : BOOL;
  function Toolbar_SetButtonWidth( Toolbar : HWND; idButton : integer; MinWidth, MaxWidth : word ) : BOOL;
  function Toolbar_SetCmdId( Toolbar : HWND; idxButton, idButton : integer ) : BOOL;
  function Toolbar_SetDisabledImageList( Toolbar : HWND ) : HIMAGELIST;
  function Toolbar_SetHotImageList( Toolbar : HWND; ImageList : HIMAGELIST ) : HIMAGELIST;
  function Toolbar_SetImageList( Toolbar : HWND; ImageList : HIMAGELIST ) : BOOL;
  function Toolbar_SetMaxTextRows( Toolbar : HWND; MaxTextRows : integer ) : BOOL;
  function Toolbar_SetParent( Toolbar : HWND; Parent : HWND ) : BOOL;
  function Toolbar_SetRows( Toolbar : HWND; Larger : wordbool; RowCount : integer; var Rect : TRect ) : BOOL;
  function Toolbar_SetStyle( Toolbar : HWND; NewStyle : integer ) : BOOL;
  function Toolbar_SetToolTips( Toolbar : HWND; ToolTips : HWND ) : BOOL;

  //====== REBAR CONTROL ========================================================

  const
    REBARCLASSNAMEA = 'ReBarWindow32';

  const
    REBARCLASSNAME  = REBARCLASSNAMEA;

  const
    RBIM_IMAGELIST = $00000001;

  const
    RBS_TOOLTIPS    = $00000100;
    RBS_VARHEIGHT   = $00000200;
    RBS_BANDBORDERS = $00000400;
    RBS_FIXEDORDER  = $00000800;

  type
    TRebarInfo =
      packed record
        cbSize : word;
        fMask  : word;
        himl   : HIMAGELIST;
      end;

  const
    RBBS_BREAK     = $00000001; // break to new line
    RBBS_FIXEDSIZE = $00000002; // band can't be sized
    RBBS_CHILDEDGE = $00000004; // edge around top & bottom of child window
    RBBS_HIDDEN    = $00000008; // don't show
    RBBS_NOVERT    = $00000010; // don't show when vertical
    RBBS_FIXEDBMP  = $00000020; // bitmap doesn't move during band resize

  const
    RBBIM_STYLE      = $00000001;
    RBBIM_COLORS     = $00000002;
    RBBIM_TEXT       = $00000004;
    RBBIM_IMAGE      = $00000008;
    RBBIM_CHILD      = $00000010;
    RBBIM_CHILDSIZE  = $00000020;
    RBBIM_SIZE       = $00000040;
    RBBIM_BACKGROUND = $00000080;
    RBBIM_ID         = $00000100;

  type
    TRebarBandInfoA =
      packed record
        cbSize     : word;
        fMask      : word;
        fStyle     : word;
        clrFore    : TColorRef;
        clrBack    : TColorRef;
        lpText     : pchar;
        cch        : word;
        iImage     : integer;
        hwndChild  : HWND;
        cxMinChild : word;
        cyMinChild : word;
        cx         : word;
        hbmBack    : HBITMAP;
        wID        : word;
      end;

  type
    TRebarBandInfo = TRebarBandInfoA;

  const
    RB_INSERTBANDA  = WM_USER + 1;
    RB_DELETEBAND   = WM_USER + 2;
    RB_GETBARINFO   = WM_USER + 3;
    RB_SETBARINFO   = WM_USER + 4;
    RB_GETBANDINFO  = WM_USER + 5;
    RB_SETBANDINFOA = WM_USER + 6;
    RB_SETPARENT    = WM_USER + 7;
    RB_INSERTBANDW  = WM_USER + 10;
    RB_SETBANDINFOW = WM_USER + 11;
    RB_GETBANDCOUNT = WM_USER + 12;
    RB_GETROWCOUNT  = WM_USER + 13;
    RB_GETROWHEIGHT = WM_USER + 14;

    RB_INSERTBAND  = RB_INSERTBANDA;
    RB_SETBANDINFO = RB_SETBANDINFOA;

  const
    RBN_HEIGHTCHANGE = RBN_FIRST - 0;

  //====== COMMON CONTROL STYLES ================================================

  const
    CCS_VERT    = $00000080;
    CCS_LEFT    = CCS_VERT or CCS_TOP;
    CCS_RIGHT   = CCS_VERT or CCS_BOTTOM;
    CCS_NOMOVEX = CCS_VERT or CCS_NOMOVEY;

  //====== TrackMouseEvent =====================================================

  const
    WM_MOUSEHOVER = $02A1;
    WM_MOUSELEAVE = $02A3;

  const
    TME_HOVER  = $00000001;
    TME_LEAVE  = $00000002;
    TME_QUERY  = $40000000;
    TME_CANCEL = $80000000;

  const
    HOVER_DEFAULT = $FFFFFFF;

  type
    PTrackMouseEvent = ^TTrackMouseEvent;
    TTrackMouseEvent =
      packed record
        cbSize      : longint;
        dwFlags     : longint;
        hwndTrack   : HWND;
        dwHoverTime : longint;
      end;

  // Declare _TrackMouseEvent. This API tries to use the window manager's
  // implementation of TrackMouseEvent if it is present, otherwise it emulates.

  function _TrackMouseEvent( lpEventTrack : PTRACKMOUSEEVENT ) : BOOL; stdcall;

implementation

  function InitCommonControlsEx( icc : PINITCOMMONCONTROLSEX ) : BOOL;
    external cctrl name 'InitCommonControlsEx';

  function ImageList_SetImageCount( himl : HIMAGELIST;
                                   uNewCount : word ) : BOOL;
    external cctrl name 'ImageList_SetImageCount';

  function ImageList_DrawIndirect( pimldp : PIMAGELISTDRAWPARAMS ) : BOOL;
    external cctrl name 'ImageList_DrawIndirect';

  function ImageList_Copy( himlDst : HIMAGELIST; iDst : integer; himlSrc : HIMAGELIST; iSrc : integer; uFlags : word ) : BOOL;
    external cctrl name 'ImageList_Copy';

  function _TrackMouseEvent( lpEventTrack : PTRACKMOUSEEVENT ) : BOOL;
    external cctrl name '_TrackMouseEvent';

  function Toolbar_EnableButton( Toolbar : HWND; idButton : integer; Enabled : BOOL ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ENABLEBUTTON, idButton, longint(Enabled) ) );
    end;

  function Toolbar_CheckButton( Toolbar : HWND; idButton : integer; Checked : BOOL ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_CHECKBUTTON, idButton, longint(Checked) ) );
    end;

  function Toolbar_PressButton( Toolbar : HWND; idButton : integer; Pressed : BOOL ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_PRESSBUTTON, idButton, longint(Pressed) ) );
    end;

  function Toolbar_HideButton( Toolbar : HWND; idButton : integer; Shown : BOOL ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_HIDEBUTTON, idButton, longint(Shown) ) );
    end;

  function Toolbar_IndeterminateButton ( Toolbar : HWND; idButton : integer; Indeterminate : BOOL ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_INDETERMINATE, idButton, longint(Indeterminate) ) );
    end;

  function Toolbar_IsButtonEnabled( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ISBUTTONENABLED, idButton, 0 ) );
    end;

  function Toolbar_IsButtonChecked( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ISBUTTONCHECKED, idButton, 0 ) );
    end;

  function Toolbar_IsButtonPressed( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ISBUTTONPRESSED, idButton, 0 ) );
    end;

  function Toolbar_IsButtonHidden( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ISBUTTONHIDDEN, idButton, 0 ) );
    end;

  function Toolbar_IsButtonIndeterminate( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ISBUTTONINDETERMINATE, idButton, 0 ) );
    end;

  function Toolbar_SetButtonState( Toolbar : HWND; idButton, State : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETSTATE, idButton, State ) );
    end;

  function Toolbar_GetButtonState( Toolbar : HWND; idButton : integer ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_GETSTATE, idButton, 0 ) );
    end;

  function Toolbar_AddBitmap( Toolbar : HWND; ButtonCount : integer; Data : PTBAddBitmap ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ADDBITMAP, ButtonCount, longint(Data) ) );
    end;

  function Toolbar_SetImageList( Toolbar : HWND; ImageList : HIMAGELIST ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETIMAGELIST, 0, ImageList ) );
    end;

  function Toolbar_GetImageList( Toolbar : HWND ) : HIMAGELIST;
    begin
      Result := HIMAGELIST( SendMessage( Toolbar, TB_GETIMAGELIST, 0, 0 ) );
    end;

  function Toolbar_LoadImages( Toolbar : HWND; hInst, idBitmap : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_LOADIMAGES, idBitmap, hInst ) );
    end;

  function Toolbar_GetRect( Toolbar : HWND; idButton : integer; var Rect : TRect ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_GETRECT, idButton, longint(@Rect) ) );
    end;

  function Toolbar_SetHotImageList( Toolbar : HWND; ImageList : HIMAGELIST ) : HIMAGELIST;
    begin
      Result := HIMAGELIST( SendMessage( Toolbar, TB_SETHOTIMAGELIST, 0, ImageList ) );
    end;

  function Toolbar_GetHotImageList( Toolbar : HWND ) : HIMAGELIST;
    begin
      Result := HIMAGELIST( SendMessage( Toolbar, TB_GETHOTIMAGELIST, 0, 0 ) );
    end;

  function Toolbar_SetDisabledImageList( Toolbar : HWND ) : HIMAGELIST;
    begin
      Result := HIMAGELIST( SendMessage( Toolbar, TB_SETDISABLEDIMAGELIST, 0, 0 ) );
    end;

  function Toolbar_GetDisabledImageList( Toolbar : HWND ) : HIMAGELIST;
    begin
      Result := HIMAGELIST( SendMessage( Toolbar, TB_GETDISABLEDIMAGELIST, 0, 0 ) );
    end;

  function Toolbar_SetStyle( Toolbar : HWND; NewStyle : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETSTYLE, 0, NewStyle ) );
    end;

  function Toolbar_GetStyle( Toolbar : HWND ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_GETSTYLE, 0, 0 ) );
    end;

  function Toolbar_GetButtonSize( Toolbar : HWND ) : TPoint;
    var
      Size : integer;
    begin
      Size := integer( SendMessage( Toolbar, TB_GETBUTTONSIZE, 0, 0 ) );
      Result := Point( word( Size ), Size shr 16 );
    end;

  function Toolbar_SetButtonWidth( Toolbar : HWND; idButton : integer; MinWidth, MaxWidth : word ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETBUTTONWIDTH, 0, MaxWidth shl 16 or MinWidth ) ); 
    end;

  function Toolbar_SetMaxTextRows( Toolbar : HWND; MaxTextRows : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETMAXTEXTROWS, MaxTextRows, 0 ) );
    end;

  function Toolbar_GetTextRows( Toolbar : HWND ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_GETTEXTROWS, 0, 0 ) );
    end;

  function Toolbar_GetButtonText( Toolbar : HWND; idButton : integer ) : string;
    var
      Length : integer;
    begin
      SetLength( Result, MAX_PATH );
      Length := SendMessage( Toolbar, TB_GETBUTTONTEXT, idButton, longint( pchar(Result) ) );
      SetLength( Result, Length );
    end;

  function Toolbar_SaveRestore( Toolbar : HWND; Save : BOOL; Params : TTBSaveParams ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SAVERESTORE, longint( Save ), longint( @Params ) ) );
    end;

  function Toolbar_AddString( Toolbar : HWND; hInst, idString : integer ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_ADDSTRING, hInst, idString ) );
    end;

  function Toolbar_AddButtons( Toolbar : HWND; ButtonCount : integer; var Buttons ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_ADDBUTTONS, ButtonCount, longint( @Buttons ) ) );
    end;

  function Toolbar_InsertButton( Toolbar : HWND; idButton : integer; ButtonData : PTBButton ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_INSERTBUTTON, idButton, longint( ButtonData ) ) );
    end;

  function Toolbar_DeleteButton( Toolbar : HWND; idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_DELETEBUTTON, idButton, 0 ) );
    end;

  function Toolbar_GetButton( Toolbar : HWND; idButton : integer; var ButtonData : TTBButton ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_GETBUTTON, idButton, longint( @ButtonData ) ) );
    end;

  function Toolbar_ButtonCount( Toolbar : HWND ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_BUTTONCOUNT, 0, 0 ) );
    end;

  function Toolbar_CommandToIndex( Toolbar : HWND; idButton : integer ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_COMMANDTOINDEX, idButton, 0 ) );
    end;

  function Toolbar_Customize( Toolbar : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_CUSTOMIZE, 0, 0 ) );
    end;

  function Toolbar_GetItemRect( Toolbar : HWND; idButton : integer; var Rect : TRect ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_GETITEMRECT, idButton, longint( @Rect ) ) );
    end;

  function Toolbar_ButtonStructSize( Toolbar : HWND; StructSize : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_BUTTONSTRUCTSIZE, StructSize, 0 ) );
    end;

  function Toolbar_SetButtonSize( Toolbar : HWND; Width, Height : word ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETBUTTONSIZE, 0, (Height shl 16) or Width ) );
    end;

  function Toolbar_SetBitmapSize( Toolbar : HWND; Width, Height : word ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETBITMAPSIZE, 0, (Height shl 16) or Width ) );
    end;

  function Toolbar_AutoSize( Toolbar : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_AUTOSIZE, 0, 0 ) );
    end;

(*
  function Toolbar_SetButtonType( Toolbar : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETBUTTONTYPE
    end;
*)
  function Toolbar_GetToolTips( Toolbar : HWND ) : HWND;
    begin
      Result := HWND( SendMessage( Toolbar, TB_GETTOOLTIPS, 0, 0 ) );
    end;

  function Toolbar_SetToolTips( Toolbar : HWND; ToolTips : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETTOOLTIPS, ToolTips, 0 ) );
    end;

  function Toolbar_SetParent( Toolbar : HWND; Parent : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETPARENT, Parent, 0 ) );
    end;

  function Toolbar_SetRows( Toolbar : HWND; Larger : wordbool; RowCount : integer; var Rect : TRect ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETROWS, (word(Larger) shl 16) or RowCount, longint( @Rect ) ) );
    end;

  function Toolbar_GetRows( Toolbar : HWND ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_GETROWS, 0, 0 ) );
    end;

  function Toolbar_SetCmdId( Toolbar : HWND; idxButton, idButton : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_SETCMDID, idxButton, idButton ) );
    end;

  function Toolbar_ChangeBitmap( Toolbar : HWND; idButton, idxBitmap : integer ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_CHANGEBITMAP, idButton, idxBitmap ) );
    end;

  function Toolbar_GetBitmap( Toolbar : HWND; idButton : integer ) : integer;
    begin
      Result := integer( SendMessage( Toolbar, TB_GETBITMAP, idButton, 0 ) );
    end;

  function Toolbar_GetBitmapFlags( Toolbar : HWND ) : BOOL;
    begin
      Result := BOOL( SendMessage( Toolbar, TB_GETBITMAPFLAGS, 0, 0 ) );
    end;

end.
(*
MenuHelp
ShowHideMenuCtl
GetEffectiveClientRect
DrawStatusTextA
CreateStatusWindowA
CreateToolbar
CreateMappedBitmap
Cctl1632_ThunkData32
CreatePropertySheetPage
CreatePropertySheetPageA
CreatePropertySheetPageW
MakeDragList
LBItemFromPt
DrawInsert
CreateUpDownControl
InitCommonControls
CreateStatusWindow
CreateStatusWindowW
CreateToolbarEx
DestroyPropertySheetPage
DllGetVersion
DrawStatusText
DrawStatusTextW
ImageList_Add
ImageList_AddIcon
ImageList_AddMasked
ImageList_BeginDrag
ImageList_Copy
ImageList_Create
ImageList_Destroy
ImageList_DragEnter
ImageList_DragLeave
ImageList_DragMove
ImageList_DragShowNolock
ImageList_Draw
ImageList_DrawEx
ImageList_EndDrag
ImageList_GetBkColor
ImageList_GetDragImage
ImageList_GetIcon
ImageList_GetIconSize
ImageList_GetImageCount
ImageList_GetImageInfo
ImageList_GetImageRect
ImageList_LoadImage
ImageList_LoadImageA
ImageList_LoadImageW
ImageList_Merge
ImageList_Read
ImageList_Remove
ImageList_Replace
ImageList_ReplaceIcon
ImageList_SetBkColor
ImageList_SetDragCursorImage
ImageList_SetFilter
ImageList_SetIconSize
ImageList_SetImageCount
ImageList_SetOverlayImage
ImageList_Write
InitCommonControlsEx
PropertySheet
PropertySheetA
PropertySheetW
_TrackMouseEvent
*)


