unit Toolbars;

{$TYPEINFO ON}

interface

  uses
    FlicPlayer, 
    SysUtils, Windows, Classes, Messages, Graphics, Controls, Forms, Menus, CommCtrl, ToolWin, ComCtrls;

  // TSpeedbar =======================================================================================

  const
    CN_REQUESTALIGN = WM_USER + $1000;

  type
    TSpeedbarButtonStyle = ( tbsButton, tbsCheck, tbsDropDown, tbsSeparator, tbsDivider );

    TSpeedbarButtonState = ( tbsChecked, tbsPressed, tbsEnabled, tbsHidden, tbsIndeterminate, tbsWrap );

    TSpeedbar = class;

    TSpeedbarButton =
      class( TGraphicControl )
        private
          fIndeterminate : boolean;
          fAllowAllUp    : boolean;
          fDown          : boolean;
          fGrouped       : boolean;
          fImageIndex    : integer;
          fDropdownMenu  : TPopupMenu;
          fWrap          : boolean;
          fStreamedDown  : boolean;
          fStyle         : TSpeedbarButtonStyle;
          fUpdateCount   : integer;

          function  CheckMenuDropdown : boolean;
          function  GetButtonState : byte;
          function  GetIndex : integer;
          procedure SetButtonState( State : byte );
          procedure SetDown( Value : boolean );
          procedure SetDropdownMenu( Value : TPopupMenu );
          procedure SetGrouped( Value : boolean );
          procedure SetImageIndex( Value : integer );
          procedure SetIndeterminate( Value : boolean );
          procedure SetStyle( Value : TSpeedbarButtonStyle );
          procedure SetWrap( Value : boolean );
          procedure CMEnabledChanged( var Message : TMessage );                                        message CM_ENABLEDCHANGED;
          procedure CMTextChanged( var Message : TMessage );                                           message CM_TEXTCHANGED;
          procedure CMVisibleChanged( var Message : TMessage );                                        message CM_VISIBLECHANGED;

        protected
          fToolbar : TSpeedbar;

          procedure BeginUpdate; virtual;
          procedure EndUpdate; virtual;
          procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X, Y : integer );           override;
          procedure MouseMove( Shift : TShiftState; X, Y : integer );                                  override;
          procedure MouseUp( Button : TMouseButton; Shift : TShiftState; X, Y : integer );             override;
          procedure Notification( aComponent : TComponent; Operation : TOperation );                   override;
          procedure Paint;                                                                             override;
          procedure SetBounds( aLeft, aTop, aWidth, aHeight : integer );                               override;
          procedure SetToolbar( AToolbar : TSpeedbar );
          procedure UpdateControl;                                                                     virtual;

          property Index : integer read GetIndex;

        public
          constructor Create( AOwner : TComponent );                                                   override;
          procedure   Click;                                                                           override;

        published
          property Indeterminate : boolean      read fIndeterminate write SetIndeterminate default false;
          property Style : TSpeedbarButtonStyle read fStyle         write SetStyle         default tbsButton;
          property AllowAllUp : boolean         read fAllowAllUp    write fAllowAllUp      default false;
          property Down : boolean               read fDown          write SetDown          default false;
          property Grouped : boolean            read fGrouped       write SetGrouped       default false;
          property Wrap : boolean               read fWrap          write SetWrap          default false;
          property ImageIndex : integer         read fImageIndex    write SetImageIndex;
          property DropdownMenu : TPopupMenu    read fDropdownMenu  write SetDropdownMenu;

        published
          property Caption;
          property DragCursor;
          property DragMode;
          property Enabled;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property Visible;
          property OnClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnStartDrag;
      end;

    TSpeedbar =
      class( TToolWindow )
        private
          fAutoSize                : boolean;
          fButtonWidth             : integer;
          fButtonHeight            : integer;
          fButtons                 : TList;
          fShowCaptions            : boolean;
          fList                    : boolean;
          fFlat                    : boolean;
          fWrapable                : boolean;
          fImages                  : TImageList;
          fImageChangeLink         : TChangeLink;
          fDisabledImages          : TImageList;
          fDisabledImageChangeLink : TChangeLink;
          fHotImages               : TImageList;
          fHotImageChangeLink      : TChangeLink;
          fIndent                  : integer;
          fNewStyle                : boolean;
          fNullBitmap              : TBitmap;
          fOldHandle               : HBitmap;
          fUpdateCount             : integer;
          fHeightMargin            : integer;
          fOnResize                : TNotifyEvent;

          procedure AdjustSize;
          function  ButtonIndex( OldIndex, aLeft, aTop : integer ) : integer;
          procedure LoadImages( aImages : TImageList );
          procedure SetAutoSize( Value : boolean );
          function  GetButton( Index : integer ) : TSpeedbarButton;
          function  GetButtonCount : integer;
          procedure GetButtonSize( var aWidth, aHeight : integer );
          function  GetRowCount : integer;
          procedure SetList( Value : boolean );
          procedure SetShowCaptions( Value : boolean );
          procedure SetFlat( Value : boolean );
          procedure SetWrapable( Value : boolean );
          procedure InsertButton( Control : TControl );
          procedure RemoveButton( Control : TControl );
          procedure UpdateButton( Index : integer );
          procedure UpdateButtons;
          procedure UpdateButtonState( Index : integer );
          procedure UpdateButtonStates;
          procedure UpdateItem( Message, FromIndex, ToIndex : integer );
          procedure CreateButtons( NewWidth, NewHeight : integer );
          procedure SetButtonWidth( Value : integer );
          procedure SetButtonHeight( Value : integer );
          procedure UpdateImages;
          procedure ImageListChange( Sender : TObject );
          procedure SetImageList( Value : HImageList );
          procedure SetImages( Value : TImageList );
          procedure DisabledImageListChange( Sender : TObject );
          procedure SetDisabledImageList( Value : HImageList );
          procedure SetDisabledImages( Value : TImageList );
          procedure HotImageListChange( Sender : TObject );
          procedure SetHotImageList( Value : HImageList );
          procedure SetHotImages( Value : TImageList );
          procedure SetIndent( Value : integer );
          procedure AdjustControl( Control : TControl );
          procedure RecreateButtons;
          procedure BeginUpdate;
          procedure EndUpdate;
          procedure ResizeButtons;
          function  InternalButtonCount : integer;
          function  ReorderButton( OldIndex, aLeft, aTop : integer ) : boolean;

          procedure WMEraseBkgnd( var Message : TWMEraseBkgnd );                                   message WM_ERASEBKGND;
          procedure WMNotifyFormat( var Message : TMessage );                                      message WM_NOTIFYFORMAT;
          procedure WMSize( var Message : TWMSize );                                               message WM_SIZE;
          procedure WMWindowPosChanged( var Message : TWMWindowPosChanged );                       message WM_WINDOWPOSCHANGED;
          procedure WMWindowPosChanging( var Message : TWMWindowPosChanging );                     message WM_WINDOWPOSCHANGING;
          procedure CMColorChanged( var Message : TMessage );                                      message CM_COLORCHANGED;
          procedure CMControlChange( var Message : TCMControlChange );                             message CM_CONTROLCHANGE;
          procedure CMEnabledChanged( var Message : TMessage );                                    message CM_ENABLEDCHANGED;
          procedure CMSysFontChanged( var Message : TMessage );                                    message CM_SYSFONTCHANGED;
          procedure CNRequestAlign( var Message : TMessage );                                      message CN_REQUESTALIGN;

        protected
          procedure AlignControls( AControl : TControl; var Rect : TRect );                        override;
          procedure CreateParams( var Params : TCreateParams );                                    override;
          procedure CreateWnd;                                                                     override;
          procedure GetChildren( Proc : TGetChildProc; Root : TComponent );                        override;
          procedure Loaded;                                                                        override;
          procedure Notification( aComponent : TComponent; Operation : TOperation );               override;
          procedure WndProc( var Message : TMessage );                                             override;
          procedure Resize;                                                                        dynamic;
          procedure RepositionButton( Index : integer );
          procedure RepositionButtons( Index : integer );

        public
          constructor Create( AOwner : TComponent );                                               override;
          destructor Destroy;                                                                      override;

          property Buttons[Index : integer] : TSpeedbarButton read GetButton;
          property ButtonCount : integer                      read GetButtonCount;
          property RowCount : integer                         read GetRowCount;

        published
          property AutoSize : boolean          read fAutoSize       write SetAutoSize       default false;
          property ButtonHeight : integer      read fButtonHeight   write SetButtonHeight   default 22;
          property ButtonWidth : integer       read fButtonWidth    write SetButtonWidth    default 23;
          property Flat : boolean              read fFlat           write SetFlat           default false;
          property Indent : integer            read fIndent         write SetIndent         default 0;
          property List : boolean              read fList           write SetList           default false;
          property ShowCaptions : boolean      read fShowCaptions   write SetShowCaptions   default false;
          property Wrapable : boolean          read fWrapable       write SetWrapable       default true;
          property DisabledImages : TImageList read fDisabledImages write SetDisabledImages;
          property HotImages : TImageList      read fHotImages      write SetHotImages;
          property Images : TImageList         read fImages         write SetImages;
          property OnResize : TNotifyEvent     read fOnResize       write fOnResize;

        published
          property Align       default alTop;
          property EdgeBorders default [ebTop];
          property Height      default 32;
         
          property Color;
          property Ctl3D;
          property DragCursor;
          property DragMode;
          property EdgeInner;
          property EdgeOuter;
          property Enabled;
          property Font;
          property BorderWidth;
          property ParentColor;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnStartDrag;
      end;

  // TReBar =======================================================================================

  const
    CN_BANDCHANGE = WM_USER + $1000;

  type
    TReBar  = class;

    TReBand =
      class( TCollectionItem )
        private
          fHorizontalOnly  : boolean;
          fBorderStyle     : TBorderStyle;
          fBreak           : boolean;
          fFixedSize       : boolean;
          fVisible         : boolean;
          fImageIndex      : integer;
          fFixedBackground : boolean;
          fMinHeight       : integer;
          fMinWidth        : integer;
          fColor           : TColor;
          fControl         : TWinControl;
          fParentColor     : boolean;
          fParentBitmap    : boolean;
          fBitmap          : TBitmap;
          fText            : string;
          fWidth           : integer;
          fDDB             : TBitmap;
          fID              : integer;

          function  ReBar : TReBar;
          function  IsColorStored : boolean;
          function  IsBitmapStored : boolean;
          procedure BitmapChanged( Sender : TObject );
          function  GetHeight : integer;
          function  GetVisible : boolean;
          procedure SetBorderStyle( Value : TBorderStyle );
          procedure SetBreak( Value : boolean );
          procedure SetFixedSize( Value : boolean );
          procedure SetMinHeight( Value : integer );
          procedure SetMinWidth( Value : integer );
          procedure SetVisible( Value : boolean );
          procedure SetHorizontalOnly( Value : boolean );
          procedure SetImageIndex( Value : integer );
          procedure SetFixedBackground( Value : boolean );
          procedure SetColor( Value : TColor );
          procedure SetControl( Value : TWinControl );
          procedure SetParentColor( Value : boolean );
          procedure SetParentBitmap( Value : boolean );
          procedure SetBitmap( Value : TBitmap );
          procedure SetText( const Value : string );
          procedure SetWidth( Value : integer );

        protected
          function  GetDisplayName : string;                                                        override;
          procedure ParentColorChanged;                                                             dynamic;
          procedure ParentBitmapChanged;                                                            dynamic;

        public
          constructor Create( Collection : TCollection );                                           override;
          destructor Destroy;                                                                       override;
          procedure Assign( Source : TPersistent );                                                 override;

          property BorderStyle : TBorderStyle read fBorderStyle write SetBorderStyle default bsSingle;
          property Height : integer           read GetHeight;

        published
          property Bitmap : TBitmap           read fBitmap          write SetBitmap          stored IsBitmapStored;
          property Break : boolean            read fBreak           write SetBreak           default true;
          property Color : TColor             read fColor           write SetColor           stored IsColorStored default clBtnFace;
          property FixedBackground : boolean  read fFixedBackground write SetFixedBackground default true;
          property FixedSize : boolean        read fFixedSize       write SetFixedSize       default false;
          property HorizontalOnly : boolean   read fHorizontalOnly  write SetHorizontalOnly  default false;
          property ParentColor : boolean      read fParentColor     write SetParentColor     default true;
          property ParentBitmap : boolean     read fParentBitmap    write SetParentBitmap    default true;
          property Visible : boolean          read GetVisible       write SetVisible         default true;
          property MinHeight : integer        read fMinHeight       write SetMinHeight       default 25;
          property MinWidth : integer         read fMinWidth        write SetMinWidth        default 0;
          property Text : string              read fText            write SetText;
          property Width : integer            read fWidth           write SetWidth;
          property Control : TWinControl      read fControl         write SetControl;
          property ImageIndex : integer       read fImageIndex      write SetImageIndex;
      end;

    TReBands =
      class( TCollection )
        private
          fReBar : TReBar;
          fVisibleCount : integer;

          function GetItem( Index : integer ) : TReBand;
          procedure SetItem( Index : integer; Value : TReBand );

        protected
          function GetOwner : TPersistent;                                                          override;
          procedure Update( Item : TCollectionItem );                                               override;
          function FindBand( AControl : TControl ) : TReBand;
          function HaveGraphic : boolean;

        public
          constructor Create( ReBar : TReBar );
          function Add : TReBand;

          property ReBar : TReBar                   read fReBar;
          property Items[Index : integer] : TReBand read GetItem write SetItem; default;
      end;

    TReBar =
      class( TToolWindow )
        private
          fAutoSize          : boolean;
          fBands             : TReBands;
          fBandBorderStyle   : TBorderStyle;
          fBitmap            : TBitmap;
          fCaptionFont       : TFont;
          fCaptionFontHeight : integer;
          fDDB               : TBitmap;
          fFixedSize         : boolean;
          fFixedOrder        : boolean;
          fImages            : TImageList;
          fImageChangeLink   : TChangeLink;
          fShowText          : boolean;
          fVertical          : boolean;
          fTrackDrag         : TSmallPoint;
          fUpdateCount       : integer;
          fOnChange          : TNotifyEvent;
          fOnResize          : TNotifyEvent;

          procedure AdjustSize;
          procedure BeginUpdate;
          procedure BitmapChanged( Sender : TObject );
          procedure DisableBands;
          procedure EndUpdate;
          function  IsAutoSized : boolean;
          function  IsBackgroundDirty : boolean;
          function  GetAlign : TAlign;
          function  GetCaptionFont : HFONT;
          function  GetCaptionFontHeight : integer;
          function  GetCaptionSize( Band : TReBand ) : integer;
          function  GetRowHeight( Index : integer ) : integer;
          procedure SetAlign( Value : TAlign );
          procedure SetAutoSize( Value : boolean );
          procedure SetBands( Value : TReBands );
          procedure SetBandBorderStyle( Value : TBorderStyle );
          procedure SetBitmap( Value : TBitmap );
          procedure SetFixedSize( Value : boolean );
          procedure SetFixedOrder( Value : boolean );
          procedure SetImageList( Value : HImageList );
          procedure SetImages( Value : TImageList );
          procedure SetShowText( Value : boolean );
          procedure SetVertical( Value : boolean );
          procedure ImageListChange( Sender : TObject );
          function  PtInGripRect( const Pos : TPoint ) : integer;
          function  ReadBands : boolean;
          function  UpdateItem( Message, FromIndex, ToIndex : integer ) : boolean;
          procedure UpdateBand( Index : integer );
          procedure UpdateBands;

          procedure WMCaptureChanged( var Message : TMessage );                                     message WM_CAPTURECHANGED;
          procedure WMEraseBkgnd( var Message : TWMEraseBkgnd );                                    message WM_ERASEBKGND;
          procedure WMLButtonDown( var Message : TWMLButtonDown );                                  message WM_LBUTTONDOWN;
          procedure WMLButtonUp( var Message : TWMLButtonUp );                                      message WM_LBUTTONUP;
          procedure WMNotifyFormat( var Message : TMessage );                                       message WM_NOTIFYFORMAT;
          procedure WMSetCursor( var Message : TWMSetCursor );                                      message WM_SETCURSOR;
          procedure WMSize( var Message : TWMSize );                                                message WM_SIZE;
          procedure WMWindowPosChanged( var Message : TWMWindowPosChanged );                        message WM_WINDOWPOSCHANGED;
          procedure CMColorChanged( var Message : TMessage );                                       message CM_COLORCHANGED;
          procedure CMControlChange( var Message : TCMControlChange );                              message CM_CONTROLCHANGE;
          procedure CMDesignHitTest( var Message : TCMDesignHitTest );                              message CM_DESIGNHITTEST;
          procedure CNBandChange( var Message : TMessage );                                         message CN_BANDCHANGE;
          procedure CNNotify( var Message : TWMNotify );                                            message CN_NOTIFY;
          procedure CMSysColorChange( var Message : TMessage );                                     message CM_SYSCOLORCHANGE;
          procedure CMSysFontChanged( var Message : TMessage );                                     message CM_SYSFONTCHANGED;
          procedure CMWinIniChange( var Message : TWMWinIniChange );                                message CM_WININICHANGE;

        protected
          procedure AlignControls( AControl : TControl; var Rect : TRect );                         override;
          procedure Change;                                                                         dynamic;
          procedure CreateParams( var Params : TCreateParams );                                     override;
          procedure CreateWnd;                                                                      override;
          function GetPalette : HPALETTE;                                                           override;
          procedure Loaded;                                                                         override;
          procedure Notification( aComponent : TComponent; Operation : TOperation );                override;
          procedure Resize;                                                                         dynamic;
          procedure WndProc( var Message : TMessage );                                              override;

        public
          constructor Create( AOwner : TComponent );                                                override;
          destructor Destroy;                                                                       override;

        published
          property Align                          read GetAlign         write SetAlign           default alTop;
          property AutoSize : boolean             read fAutoSize        write SetAutoSize        default false;
          property BandBorderStyle : TBorderStyle read fBandBorderStyle write SetBandBorderStyle default bsSingle;
          property FixedSize : boolean            read fFixedSize       write SetFixedSize       default false;
          property FixedOrder : boolean           read fFixedOrder      write SetFixedOrder      default false;
          property ShowText : boolean             read fShowText        write SetShowText        default true;
          property Vertical : boolean             read fVertical        write SetVertical        default false;
          property Bands : TReBands               read fBands           write SetBands;
          property Images : TImageList            read fImages          write SetImages;
          property Bitmap : TBitmap               read fBitmap          write SetBitmap;
          property OnChange : TNotifyEvent        read fOnChange        write fOnChange;
          property OnResize : TNotifyEvent        read fOnResize        write fOnResize;

        published
          property BorderWidth;
          property Color;
          property Ctl3D;
          property DragCursor;
          property DragMode;
          property EdgeBorders;
          property EdgeInner;
          property EdgeOuter;
          property Enabled;
          property Font;
          property ParentColor;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnStartDrag;
      end;

  // Helper functions =======================================================================================

  function CreateNewButton( Speedbar : TSpeedbar; aStyle : TSpeedbarButtonStyle ) : TSpeedbarButton;

  // VCL Registration =======================================================================================

  procedure Register;

implementation

  uses
    VclUtils;

  // Helper functions =======================================================================================

  function CreateNewButton( Speedbar : TSpeedbar; aStyle : TSpeedbarButtonStyle ) : TSpeedbarButton;
    var
      LastButton : TSpeedbarButton;
    begin
      Result := TSpeedbarButton.Create( Speedbar.Owner );

      with Speedbar do
        begin
          if ButtonCount = 0
            then LastButton := nil
            else LastButton := Buttons[ButtonCount - 1];
          Result.Name := UniqueComponentName( Owner, 'ToolButton' );
        end;

      if Assigned( LastButton )
        then
          with LastButton do
            begin
              Result.Left       := Left + Width;
              Result.ImageIndex := ImageIndex + 1;
            end;

      with Result do
        begin
          Style := aStyle;
          if Style = tbsSeparator
            then Width := 8;
          Caption := Name;
          Parent := Speedbar;
        end;
    end;

  // TSpeedbarButton =======================================================================================

  constructor TSpeedbarButton.Create( AOwner : TComponent );
    begin
      inherited;

      ControlStyle := [csCaptureMouse, csSetCaption];

      Width  := 23;
      Height := 22;
      fStyle := tbsButton;
    end;

  procedure TSpeedbarButton.MouseDown( Button : TMouseButton; Shift : TShiftState; X, Y : integer );
    begin
      if ( Style = tbsDropDown ) and ( Button = mbLeft ) and Enabled
        then Down := not Down;
      inherited;
    end;

  procedure TSpeedbarButton.MouseMove( Shift : TShiftState; X, Y : integer );
    begin
      inherited;
      if ( Style = tbsDropDown ) and MouseCapture
        then Down := ( X >= 0 ) and ( X < ClientWidth ) and
                     ( Y >= 0 ) and ( Y <= ClientHeight );
    end;

  procedure TSpeedbarButton.MouseUp( Button : TMouseButton; Shift : TShiftState; X, Y : integer );
    begin
      inherited;

      if ( Button = mbLeft ) and
         ( X >= 0 ) and ( X < ClientWidth ) and
         ( Y >= 0 ) and ( Y <= ClientHeight )
        then
          begin
            if Style = tbsDropDown
              then Down := false;
            Click;
          end;
    end;

  procedure TSpeedbarButton.Click;
    begin
      inherited;
    end;

  procedure TSpeedbarButton.Notification( aComponent : TComponent; Operation : TOperation );
    begin
      inherited;
      if ( aComponent = DropdownMenu ) and ( Operation = opRemove )
        then DropdownMenu := nil;
    end;

  procedure TSpeedbarButton.CMTextChanged( var Message : TMessage );
    begin
      inherited;
      UpdateControl;
      if Assigned( fToolbar ) and fToolbar.ShowCaptions
        then fToolbar.RecreateButtons;
    end;

  procedure TSpeedbarButton.SetBounds( aLeft, aTop, aWidth, aHeight : integer );
    var
      Pos                       : integer;
      Reordered, NeedsUpdate    : boolean;
      ResizeWidth, ResizeHeight : boolean;
    begin
      if ( fUpdateCount = 0 ) and not ( csLoading in ComponentState ) and Assigned( fToolbar )
        then
          begin
            Pos       := Index;
            Reordered := fToolbar.ReorderButton( Index, aLeft, aTop );
            if Reordered
              then
                begin
                  NeedsUpdate := false;
                  if Index < Pos
                    then Pos := Index;
                end
              else
                begin
                  NeedsUpdate := ( Style in [tbsDropDown, tbsSeparator, tbsDivider] ) and ( aWidth <> Width );
                  Reordered   := NeedsUpdate;
                end;

            if {not fToolbar.Flat and} ( Style = tbsDropDown )
              then aWidth := ( Width div 3 ) * 2 + ( Width mod 3 ) + aWidth - Width;

            ResizeWidth  := not ( Style in [tbsDropDown, tbsSeparator, tbsDivider] ) and
                             ( aWidth <> fToolbar.ButtonWidth );
            ResizeHeight := aHeight <> fToolbar.ButtonHeight;

            if NeedsUpdate
              then inherited SetBounds( aLeft, aTop, aWidth, aHeight );
            if ResizeWidth
              then fToolbar.ButtonWidth := aWidth;
            if ResizeHeight
              then fToolbar.ButtonHeight := aHeight;
            if Reordered and not ResizeWidth and not ResizeHeight
              then
                begin
                  if NeedsUpdate
                    then fToolbar.UpdateButton( Pos );
                  fToolbar.ResizeButtons;
                  fToolbar.RepositionButtons( 0 );
                end
              else
                fToolbar.RepositionButton( Pos );
          end
        else
          inherited SetBounds( aLeft, aTop, aWidth, aHeight );
    end;

  procedure TSpeedbarButton.Paint;
    var
      R : TRect;
    begin
      if Assigned( fToolbar )
        then
          begin
            if Style = tbsDivider
              then
                with Canvas do
                  begin
                    R := Rect( Width div 2 - 1, 1, Width, Height - 1 );
                    DrawEdge( Handle, R, EDGE_ETCHED, BF_LEFT );
                  end;
            if csDesigning in ComponentState
              then
                if Style in [tbsSeparator, tbsDivider] // Draw separator outline
                  then
                    with Canvas do
                      begin
                        Pen.Style := psDot;
                        Pen.Color := clBtnShadow;
                        Brush.Style := bsClear;
                        Rectangle( 0, 0, ClientWidth, ClientHeight );
                      end
                  else // Draw Flat button face
                    if fToolbar.Flat
                      then
                        with Canvas do
                          begin
                            R := Rect( 0, 0, Width, Height );
                            if not Down
                              then DrawEdge( Handle, R, BDR_RAISEDINNER, BF_RECT );
                          end;
          end;
    end;

  const
    ButtonStates : array[TSpeedbarButtonState] of word =
      (
        TBSTATE_CHECKED, TBSTATE_PRESSED, TBSTATE_ENABLED, 
        TBSTATE_HIDDEN, TBSTATE_INDETERMINATE, TBSTATE_WRAP
      );

    ButtonStyles : array[TSpeedbarButtonStyle] of word =
      (
        TBSTYLE_BUTTON, TBSTYLE_CHECK, TBSTYLE_DROPDOWN, TBSTYLE_SEP, TBSTYLE_SEP
      );

  function TSpeedbarButton.GetButtonState : byte;
    begin
      Result := 0;
      if fDown
        then
          if Style = tbsCheck
            then Result := Result or ButtonStates[tbsChecked]
            else Result := Result or ButtonStates[tbsPressed];
      if Enabled and ( not Assigned( fToolbar ) or fToolbar.Enabled )
        then Result := Result or ButtonStates[tbsEnabled];
      if not Visible
        then Result := Result or ButtonStates[tbsHidden];
      if fIndeterminate
        then Result := Result or ButtonStates[tbsIndeterminate];
      if fWrap
        then Result := Result or ButtonStates[tbsWrap];
    end;

  procedure TSpeedbarButton.SetButtonState( State : byte );
    begin
      fDown          := State and ( TBSTATE_CHECKED or TBSTATE_PRESSED ) <> 0;
      Enabled        := State and TBSTATE_ENABLED <> 0;
      Visible        := State and TBSTATE_HIDDEN = 0;
      fIndeterminate := not fDown and ( State and TBSTATE_INDETERMINATE <> 0 );
      fWrap          := State and TBSTATE_WRAP <> 0;
    end;

  procedure TSpeedbarButton.SetToolbar( AToolbar : TSpeedbar );
    begin
      if fToolbar <> AToolbar
        then
          begin
            if fToolbar <> nil
              then fToolbar.RemoveButton( Self );
            Parent := AToolbar;
            if AToolbar <> nil
              then AToolbar.InsertButton( Self );
          end;
    end;

  procedure TSpeedbarButton.CMVisibleChanged( var Message : TMessage );
    begin
      UpdateControl;
    end;

  procedure TSpeedbarButton.CMEnabledChanged( var Message : TMessage );
    begin
      UpdateControl;
    end;

  procedure TSpeedbarButton.SetDown( Value : boolean );
    begin
      if ( csReading in ComponentState )
        then
          begin
            if Value
              then fStreamedDown := Value;
          end
        else
          if Value <> fDown
            then
              begin
                fIndeterminate := false;
                fDown          := Value;
                UpdateControl;

                // Invalidate background only when Speedbar is flat and button is changing
                //  from down to up position.
                if not Value and Assigned( fToolbar ) and fToolbar.Flat
                  then Invalidate;
              end;
    end;

  procedure TSpeedbarButton.SetDropdownMenu( Value : TPopupMenu );
    begin
      if Value <> fDropdownMenu
        then
          begin
            fDropdownMenu := Value;
            if Value <> nil
              then Value.FreeNotification( Self );
          end;
    end;

  procedure TSpeedbarButton.SetGrouped( Value : boolean );
    begin
      if fGrouped <> Value
        then
          begin
            fGrouped := Value;
            UpdateControl;
          end;
    end;

  procedure TSpeedbarButton.SetImageIndex( Value : integer );
    begin
      if fImageIndex <> Value
        then
          begin
            fImageIndex := Value;
            UpdateControl;
          end;
    end;

  procedure TSpeedbarButton.SetIndeterminate( Value : boolean );
    begin
      if fIndeterminate <> Value
        then
          begin
            fDown := false;
            fIndeterminate := Value;
            UpdateControl;
          end;
    end;

  procedure TSpeedbarButton.SetStyle( Value : TSpeedbarButtonStyle );
    begin
      if fStyle <> Value
        then
          begin
            fStyle := Value;
            if Value in [tbsSeparator, tbsDivider]
              then ControlStyle := ControlStyle + [csNoDesignVisible]
              else ControlStyle := ControlStyle - [csNoDesignVisible];
            if Assigned( fToolbar )
              then
                begin
                  UpdateControl;
                  fToolbar.ResizeButtons;
                  fToolbar.RepositionButtons( Index );
                end;
          end;
    end;

  procedure TSpeedbarButton.SetWrap( Value : boolean );
    begin
      if fWrap <> Value
        then
          begin
            fWrap := Value;
            UpdateControl;
            if Assigned( fToolbar )
              then fToolbar.RepositionButtons( Index );
          end;
    end;

  procedure TSpeedbarButton.BeginUpdate;
    begin
      Inc( fUpdateCount );
    end;

  procedure TSpeedbarButton.EndUpdate;
    begin
      Dec( fUpdateCount );
    end;

  function TSpeedbarButton.GetIndex : integer;
    begin
      if Assigned( fToolbar )
        then Result := fToolbar.fButtons.IndexOf( Self )
        else Result := -1;
    end;

  procedure TSpeedbarButton.UpdateControl;
    begin
      if Assigned( fToolbar )
        then fToolbar.UpdateButton( Index );
    end;

  function TSpeedbarButton.CheckMenuDropdown : boolean;
    begin
      Result := false;
      if not ( csDesigning in ComponentState ) and
         Assigned( DropdownMenu ) and DropdownMenu.AutoPopup
        then
          begin
            SendCancelMode( nil );
            DropdownMenu.PopupComponent := Self;
            with ClientToScreen( Point( 0, ClientHeight ) ) do
              DropdownMenu.Popup( X, Y );
            Result := true;
          end;
    end;

  // TSpeedbar ======================================================================================= 

  constructor TSpeedbar.Create( AOwner : TComponent );
    begin
      inherited;

      ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks];

      Height := 32;
      Align := alTop;

      fNewStyle    := true;
      fWrapable    := true;
      fButtons     := TList.Create;
      fButtonWidth := 23;
      fButtonHeight := 22;
      EdgeBorders  := [ebTop];

      fImageChangeLink          := TChangeLink.Create;
      fImageChangeLink.OnChange := ImageListChange;

      fDisabledImageChangeLink          := TChangeLink.Create;
      fDisabledImageChangeLink.OnChange := DisabledImageListChange;

      fHotImageChangeLink          := TChangeLink.Create;
      fHotImageChangeLink.OnChange := HotImageListChange;

      fNullBitmap := TBitmap.Create;
      with fNullBitmap do
        begin
          Width := 1;
          Height := 1;
          Canvas.Brush.Color := clBtnFace;
          Canvas.FillRect( Rect( 0, 0, 1, 1 ) );
        end;
    end;

  destructor TSpeedbar.Destroy;
    var
      i : integer;
    begin
      fNullBitmap.Free;
      fHotImageChangeLink.Free;
      fDisabledImageChangeLink.Free;
      fImageChangeLink.Free;
      for i := 0 to fButtons.Count - 1 do
        if TControl( fButtons[i] ) is TSpeedbarButton
          then TSpeedbarButton( fButtons[i] ).fToolbar := nil;
      fButtons.Free;
      inherited;
    end;

  procedure TSpeedbar.CreateParams( var Params : TCreateParams );
    const
      DefaultStyles = CCS_NOPARENTALIGN or CCS_NOMOVEY or CCS_NORESIZE or CCS_NODIVIDER;
    const
      ListStyles : array[boolean] of integer = ( 0, TBSTYLE_LIST );
      FlatStyles : array[boolean] of integer = ( 0, TBSTYLE_FLAT );
      WrapStyles : array[boolean] of integer = ( 0, TBSTYLE_WRAPABLE );
    begin
      fNewStyle := InitCommonControl( ICC_BAR_CLASSES );

      inherited;
      CreateSubClass( Params, ToolbarCLASSNAME );

      with Params do
        begin
          Style := Style or DefaultStyles or FlatStyles[fFlat] or ListStyles[fList] or WrapStyles[fWrapable];
          WindowClass.Style := WindowClass.Style and not ( CS_HREDRAW or CS_VREDRAW );
        end;
    end;

  procedure TSpeedbar.CreateWnd;
    var
      DisplayDC           : HDC;
      SaveFont, StockFont : HFONT;
      TxtMetric           : TTextMetric;
    begin
      inherited;

      fOldHandle := 0;
      StockFont  := GetStockObject( SYSTEM_FONT );
      if StockFont <> 0
        then
          begin
            DisplayDC := GetDC( 0 );
            if ( DisplayDC <> 0 )
              then
                begin
                  SaveFont := SelectObject( DisplayDC, StockFont );
                  if ( GetTextMetrics( DisplayDC, TxtMetric ) )
                    then
                      with TxtMetric do
                        fHeightMargin := tmHeight - tmInternalLeading - tmExternalLeading + 1;
                  SelectObject( DisplayDC, SaveFont );
                  ReleaseDC( 0, DisplayDC );
                end;
          end;
      RecreateButtons;
      Invalidate;
    end;

  procedure TSpeedbar.CreateButtons( NewWidth, NewHeight : integer );
    var
      ImageWidth, ImageHeight : integer;
    begin
      BeginUpdate;
      try
        HandleNeeded;
        Perform( TB_BUTTONSTRUCTSIZE, SizeOf( TTBButton ), 0 );
        Perform( TB_SETINDENT, fIndent, 0 );
        if Assigned( fImages )
          then
            begin
              ImageWidth  := fImages.Width;
              ImageHeight := fImages.Height;
            end
          else
            if Assigned( fDisabledImages )
              then
                begin
                  ImageWidth  := fDisabledImages.Width;
                  ImageHeight := fDisabledImages.Height;
                end
              else
                if Assigned( fHotImages )
                  then
                    begin
                      ImageWidth  := fHotImages.Width;
                      ImageHeight := fHotImages.Height;
                    end
                  else
                    begin
                      ImageWidth  := 0;
                      ImageHeight := 0;
                    end;
        Perform( TB_SETBITMAPSIZE, 0, MakeLParam( ImageWidth, ImageHeight ) );
        if ShowCaptions
          then Dec( NewHeight, fHeightMargin );
        Perform( TB_SETBUTTONSIZE, 0, MakeLParam( NewWidth, NewHeight ) );
      finally
        EndUpdate;
      end;

      // Retrieve current button sizes
      GetButtonSize( fButtonWidth, fButtonHeight );
      UpdateButtons;
      UpdateImages;
    end;

  procedure TSpeedbar.AdjustSize;
    begin
      if HandleAllocated
        then SetWindowPos( Handle, 0, 0, 0, Width, Height, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOZORDER );
    end;

  procedure TSpeedbar.RepositionButton( Index : integer );
    var
      TBButton : TTBButton;
      Button   : TControl;
      R        : TRect;
      AdjustY  : integer;
    begin
      if not ( csLoading in ComponentState ) and
        ( Perform( TB_GETBUTTON, Index, Longint( @TBButton ) ) <> 0 )
          then
            begin
              Button := TControl( TBButton.dwData );
              if Button is TSpeedbarButton
                then TSpeedbarButton( Button ).BeginUpdate;

              try
                Perform( TB_GETITEMRECT, Index, Longint( @R ) );
                if ( Button is TWinControl )
                  then
                    with TWinControl( Button ) do
                      begin
                        HandleNeeded;
                        // Check for a control that doesn't size and center it
                        BoundsRect := R;
                        if Height < R.Bottom - R.Top
                          then
                            begin
                              AdjustY := ( R.Bottom - R.Top - Height ) div 2;
                              SetBounds( R.Left, R.Top + AdjustY, R.Right - R.Left, Height );
                            end;
                      end
                  else
                    Button.BoundsRect := R;
              finally
                if Button is TSpeedbarButton
                  then TSpeedbarButton( Button ).EndUpdate;
              end;
            end;
    end;

  procedure TSpeedbar.RepositionButtons( Index : integer );
    var
      i : integer;
    begin
      if not ( csLoading in ComponentState ) and ( fUpdateCount <= 0 )
        then
          begin
            BeginUpdate;
            try
              for i := InternalButtonCount - 1 downto Index do
                RepositionButton( i );
            finally
              EndUpdate;
            end;
          end;
    end;

  procedure TSpeedbar.GetButtonSize( var aWidth, aHeight : integer );
    var
      LastIndex : integer;
      R         : TRect;
      TBButton  : TTBButton;
    begin
      aWidth  := fButtonWidth;
      aHeight := fButtonHeight;
      if HandleAllocated then
      begin
        LastIndex := InternalButtonCount - 1;
        if LastIndex >= 0
          then
            begin
              while ( LastIndex >= 0 ) and
                ( Perform( TB_GETBUTTON, LastIndex, integer( @TBButton ) ) <> 0 ) and
                ( TBButton.fsStyle and ( TBSTYLE_SEP ) <> 0 ) do
                Dec( LastIndex );
              if LastIndex < 0
                then
                  begin
                    if Perform( TB_GETITEMRECT, 0, Longint( @R ) ) <> 0
                      then
                        begin
                          aHeight := R.Bottom - R.Top;
                          if ShowCaptions
                            then Inc( aHeight, fHeightMargin );
                        end;
                  end
                else
                  if Perform( TB_GETITEMRECT, LastIndex, Longint( @R ) ) <> 0
                    then
                      begin
                        aHeight := R.Bottom - R.Top;
                        if ShowCaptions
                          then Inc( aHeight, fHeightMargin );

                        // Adjust size for drop-down and separator buttons !!
                        if {not Flat and }( TBButton.fsStyle and ( TBSTYLE_DROPDOWN ) <> 0 )
                          then aWidth := ( ( R.Right - R.Left ) div 3 ) * 2 + ( ( R.Right - R.Left ) mod 3 )
                          else aWidth := R.Right - R.Left;
                      end;
            end;
      end;
    end;

  procedure TSpeedbar.SetButtonHeight( Value : integer );
    begin
      if Value <> fButtonHeight
        then
          begin
            fButtonHeight := Value;
            RecreateButtons;
          end;
    end;

  procedure TSpeedbar.SetButtonWidth( Value : integer );
    begin
      if Value <> fButtonWidth
        then
          begin
            fButtonWidth := Value;
            RecreateButtons;
          end;
    end;

  procedure TSpeedbar.InsertButton( Control : TControl );
    var
      Pos : integer;
    begin
      if Control is TSpeedbarButton
        then TSpeedbarButton( Control ).fToolbar := Self;
      Pos := fButtons.Add( Control );
      UpdateButton( Pos );
      ResizeButtons;
      if Wrapable
        then RepositionButtons( 0 )
        else RepositionButtons( Pos );
    end;

  procedure TSpeedbar.RemoveButton( Control : TControl );
    var
      i, Pos : integer;
    begin
      i := fButtons.IndexOf( Control );
      if i >= 0
        then
          begin
            if Control is TSpeedbarButton
              then TSpeedbarButton( Control ).fToolbar := nil;
            Pos := fButtons.Remove( Control );
            Perform( TB_DELETEBUTTON, Pos, 0 );
            ResizeButtons;
            if Wrapable
              then RepositionButtons( 0 )
              else RepositionButtons( Pos );
          end;
    end;

  procedure TSpeedbar.UpdateItem( Message, FromIndex, ToIndex : integer );
    var
      Button : TTBButton;
      Buffer : array[0..4095] of Char;
    begin
      with TControl( fButtons[FromIndex] ) do
        begin
          if ClassType = TSpeedbarButton
            then
              with TSpeedbarButton( fButtons[FromIndex] ) do
                begin
                  FillChar( Button, SizeOf( Button ), 0 );
                  if Style in [tbsSeparator, tbsDivider]
                    then
                      begin
                        Button.iBitmap   := Width;
                        Button.idCommand := -1;
                      end
                    else
                      begin
                        Button.iBitmap   := ImageIndex;
                        Button.idCommand := FromIndex;
                      end;
                  Button.fsStyle := ButtonStyles[Style];
                  Button.fsState := GetButtonState;
                  if fGrouped
                    then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
                  Button.dwData := Longint( fButtons[FromIndex] );
                  if ShowCaptions
                    then
                      begin
                        StrPCopy( Buffer, Caption );
                        // TB_ADDSTRING requires two null terminators
                        Buffer[Length( Caption ) + 1] := #0;
                        Button.iString := Self.Perform( TB_ADDSTRING, 0, Longint( @Buffer ) );
                      end
                    else Button.iString := -1;
                end
            else
              begin
                FillChar( Button, SizeOf( Button ), 0 );
                Button.fsStyle := ButtonStyles[tbsSeparator];
                Button.iBitmap := Width;
                Button.idCommand := -1;
                Button.dwData := Longint( fButtons[FromIndex] );
                Button.iString := -1;
              end;
          Self.Perform( Message, ToIndex, integer( @Button ) );
        end;
    end;

  procedure TSpeedbar.UpdateButton( Index : integer );
  begin
    if fUpdateCount <= 0
      then
        begin
          BeginUpdate;
          try
            HandleNeeded;
            Perform( WM_SETREDRAW, 0, 0 );
            try
              if Perform( TB_DELETEBUTTON, Index, 0 ) = 1
                then UpdateItem( TB_INSERTBUTTON, Index, Index )
                else UpdateItem( TB_ADDBUTTONS, Index, 1 )
            finally
              Perform( WM_SETREDRAW, 1, 0 );
            end;
            RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN );
          finally
            EndUpdate;
          end;
        end;
  end;

  procedure TSpeedbar.UpdateButtons;
    var
      i : integer;
    begin
      if fUpdateCount <= 0
        then
          begin
            BeginUpdate;
            try
              HandleNeeded;
              Perform( WM_SETREDRAW, 0, 0 );
              try
                for i := 0 to InternalButtonCount - 1 do
                  Perform( TB_DELETEBUTTON, 0, 0 );
                for i := 0 to fButtons.Count - 1 do
                  UpdateItem( TB_ADDBUTTONS, i, 1 );
              finally
                Perform( WM_SETREDRAW, 1, 0 );
              end;
              RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN );
            finally
              EndUpdate;
            end;
            RepositionButtons( 0 );
          end;
    end;

  procedure TSpeedbar.UpdateButtonState( Index : integer );
    var
      TBButton : TTBButton;
    begin
      if ( Perform( TB_GETBUTTON, Index, integer( @TBButton ) ) <> 0 )
        then
          with TSpeedbarButton( TBButton.dwData ) do
            begin
              SetButtonState( TBButton.fsState );
              Self.Perform( TB_SETSTATE, Index, MakeLong( GetButtonState, 0 ) );
            end;
    end;

  procedure TSpeedbar.UpdateButtonStates;
    var
      i : integer;
    begin
      for i := 0 to fButtons.Count - 1 do
        if TControl( fButtons[i] ).ClassType = TSpeedbarButton
          then UpdateButtonState( i );
    end;

  procedure TSpeedbar.SetAutoSize( Value : boolean );
    begin
      if fAutoSize <> Value
        then
          begin
            fAutoSize := Value;
            if Value and not ( csLoading in ComponentState )
              then AdjustSize;
          end;
    end;

  procedure TSpeedbar.SetShowCaptions( Value : boolean );
    begin
      if fShowCaptions <> Value
        then
          begin
            fShowCaptions := Value;
            RecreateWnd;
          end;
    end;

  function TSpeedbar.GetButton( Index : integer ) : TSpeedbarButton;
    begin
      Result := fButtons[Index];
    end;

  function TSpeedbar.GetButtonCount : integer;
    begin
      Result := fButtons.Count;
    end;

  function TSpeedbar.GetRowCount : integer;
    begin
      Result := Perform( TB_GETROWS, 0, 0 );
    end;

  procedure TSpeedbar.SetList( Value : boolean );
    begin
      if fList <> Value
        then
          begin
            fList := Value;
            RecreateWnd;
          end;
    end;

  procedure TSpeedbar.SetFlat( Value : boolean );
    begin
      if fFlat <> Value
        then
          begin
            fFlat := Value;
            RecreateWnd;
          end;
    end;

  procedure TSpeedbar.SetWrapable( Value : boolean );
    begin
      if fWrapable <> Value
        then
          begin
            fWrapable := Value;
            RecreateWnd;
          end;
    end;

  procedure TSpeedbar.Resize;
    begin
      if Assigned( fOnResize )
        then fOnResize( Self );
    end;

  procedure TSpeedbar.Notification( aComponent : TComponent; Operation : TOperation );
    begin
      inherited Notification( aComponent, Operation );
      if Operation = opRemove
        then
          begin
            if aComponent = fImages
              then Images := nil;
            if aComponent = fHotImages
              then HotImages := nil;
            if aComponent = fDisabledImages
              then DisabledImages := nil;
          end;
    end;

  procedure TSpeedbar.LoadImages( aImages : TImageList );
    var
      AddBitmap     : TTBAddBitmap;
      ReplaceBitmap : TTBReplaceBitmap;
      NewHandle     : HBITMAP;

      function GetImageBitmap( ImageList : TImageList ) : HBITMAP;
        var
          i      : integer;
          Bitmap : TBitmap;
          R      : TRect;
        begin
          Bitmap := TBitmap.Create;
          try
            Bitmap.Width  := ImageList.Width * ImageList.Count;
            Bitmap.Height := ImageList.Height;
            R := Rect( 0, 0, Width, Height );
            with Bitmap.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect( R );
              end;
            for i := 0 to ImageList.Count - 1 do
              ImageList_Draw( ImageList.Handle, i, Bitmap.Canvas.Handle, i * ImageList.Width, 0, ILD_TRANSPARENT );
            Result := Bitmap.ReleaseHandle;
          finally
            Bitmap.Free;
          end;
        end;

    begin
      if Assigned( aImages )
        then NewHandle := GetImageBitmap( aImages )
        else
          with TBitmap.Create do
            try
              Assign( fNullBitmap );
              NewHandle := ReleaseHandle;
            finally
              Free;
            end;
      if fOldHandle = 0
        then
          begin
            AddBitmap.hInst := 0;
            AddBitmap.nID   := NewHandle;
            Perform( TB_ADDBITMAP, ButtonCount, Longint( @AddBitmap ) );
          end
        else
          begin
            with ReplaceBitmap do
              begin
                hInstOld := 0;
                nIDOld   := fOldHandle;
                hInstNew := 0;
                nIDNew   := NewHandle;
                nButtons := ButtonCount;
              end;
            Perform( TB_REPLACEBITMAP, 0, Longint( @ReplaceBitmap ) );
            if fOldHandle <> 0
              then DeleteObject( fOldHandle );
          end;
      fOldHandle := NewHandle;
    end;

  procedure TSpeedbar.UpdateImages;
    begin
      if fNewStyle
        then
          begin
            if fImages <> nil
              then SetImageList( fImages.Handle );
            if fDisabledImages <> nil
              then SetDisabledImageList( fDisabledImages.Handle );
            if fHotImages <> nil
              then SetHotImageList( fHotImages.Handle );
          end
        else
          if HandleAllocated
            then LoadImages( fImages );
    end;

  procedure TSpeedbar.ImageListChange( Sender : TObject );
    begin
      if HandleAllocated and ( Sender = Images )
        then RecreateButtons;
    end;

  procedure TSpeedbar.SetImageList( Value : HImageList );
    begin
      if HandleAllocated
        then Perform( TB_SETIMAGELIST, 0, Value );
      Invalidate;
    end;

  procedure TSpeedbar.SetImages( Value : TImageList );
    begin
      if fImages <> nil
        then fImages.UnRegisterChanges( fImageChangeLink );
      fImages := Value;
      if fImages <> nil
        then fImages.RegisterChanges( fImageChangeLink )
        else SetImageList( 0 );
      RecreateButtons;
    end;

  procedure TSpeedbar.DisabledImageListChange( Sender : TObject );
    begin
      if HandleAllocated and ( Sender = DisabledImages )
        then RecreateButtons;
    end;

  procedure TSpeedbar.SetDisabledImageList( Value : HImageList );
    begin
      if HandleAllocated
        then Perform( TB_SETDISABLEDIMAGELIST, 0, Value );
      Invalidate;
    end;

  procedure TSpeedbar.SetDisabledImages( Value : TImageList );
    begin
      if fDisabledImages <> nil
        then fDisabledImages.UnRegisterChanges( fDisabledImageChangeLink );
      fDisabledImages := Value;
      if fDisabledImages <> nil
        then fDisabledImages.RegisterChanges( fDisabledImageChangeLink )
        else SetDisabledImageList( 0 );
      RecreateButtons;
    end;

  procedure TSpeedbar.HotImageListChange( Sender : TObject );
    begin
      if HandleAllocated and ( Sender = HotImages )
        then RecreateButtons;
    end;

  procedure TSpeedbar.SetHotImageList( Value : HImageList );
    begin
      if HandleAllocated
        then Perform( TB_SETHOTIMAGELIST, 0, Value );
      Invalidate;
    end;

  procedure TSpeedbar.SetHotImages( Value : TImageList );
    begin
      if fHotImages <> nil
        then fHotImages.UnRegisterChanges( fHotImageChangeLink );
      fHotImages := Value;
      if fHotImages <> nil
        then fHotImages.RegisterChanges( fHotImageChangeLink )
        else SetHotImageList( 0 );
      RecreateButtons;
    end;

  procedure TSpeedbar.SetIndent( Value : integer );
    begin
      if fIndent <> Value
        then
          begin
            fIndent := Value;
            RecreateWnd;
          end;
    end;

  procedure TSpeedbar.RecreateButtons;
    begin
      CreateButtons( fButtonWidth, fButtonHeight );
      ResizeButtons;
    end;

  procedure TSpeedbar.GetChildren( Proc : TGetChildProc; Root : TComponent );
    var
      i       : integer;
      Control : TControl;
    begin
      for i := 0 to fButtons.Count - 1 do
        Proc( TComponent( fButtons[i] ) );
      for i := 0 to ControlCount - 1 do
        begin
          Control := Controls[i];
          if ( Control.Owner = Root ) and ( fButtons.IndexOf( Control ) = -1 )
            then Proc( Control );
        end;
    end;

  procedure TSpeedbar.Loaded;
    var
      i : integer;
    begin
      inherited;

      ResizeButtons;
      RepositionButtons( 0 );

      for i := 0 to fButtons.Count - 1 do
        if ( TControl( fButtons[i] ) is TSpeedbarButton )
          then
            with TSpeedbarButton( fButtons[i] ) do
              if fStreamedDown
                then
                  begin
                    Down := true;
                    fStreamedDown := false;
                  end;
    end;

  procedure TSpeedbar.BeginUpdate;
    begin
      Inc( fUpdateCount );
    end;

  procedure TSpeedbar.EndUpdate;
    begin
      Dec( fUpdateCount );
    end;

  procedure TSpeedbar.ResizeButtons;
    begin
      if Wrapable and not ( csLoading in ComponentState ) and HandleAllocated
        then
          begin
            Perform( TB_AUTOSIZE, 0, 0 );
            if AutoSize
              then AdjustSize;
          end;
    end;

  function TSpeedbar.InternalButtonCount : integer;
    begin
      Result := Perform( TB_BUTTONCOUNT, 0, 0 );
    end;

  function TSpeedbar.ButtonIndex( OldIndex, aLeft, aTop : integer ) : integer;
    var
      Adjust            : boolean;
      PosTop, PosBottom : integer;
      Candidates        : TList;
      RowTop, RowBottom : integer;
    begin
      Candidates := TList.Create;
      try
        PosTop    := 0;
        PosBottom := 0;
        Adjust    := false;
        for Result := 0 to fButtons.Count - 1 do
          begin
            if aLeft <= TControl( fButtons[Result] ).Left
              then
                begin
                  RowTop    := TControl( fButtons[Result] ).Top;
                  RowBottom := RowTop + TControl( fButtons[Result] ).Height;
                  Adjust    := true;

                  if ( Candidates.Count > 0 ) and
                     ( ( PosTop in [RowTop..RowBottom] ) or ( PosBottom in [RowTop..RowBottom] ) )
                    then Continue;
                  Candidates.Add( Pointer( Result ) );

                  PosTop    := TControl( fButtons[Result] ).Top;
                  PosBottom := PosTop + TControl( fButtons[Result] ).Height;
                end;
          end;
        if Adjust
          then
            begin
              if ( Candidates.Count > 1 )
                then
                  begin
                    for Result := Candidates.Count - 1 downto 0 do
                      if ( aTop >= TControl( fButtons[integer( Candidates[Result] )] ).Top )
                        then Break;
                    if Result >= 0
                      then Result := integer( Candidates[Result] );
                  end
                else Result := integer( Candidates[0] );
            end
          else
            if aLeft > TControl( fButtons.Last ).Left
              then Result := fButtons.Count
              else Result := OldIndex; // No re-ordering
      finally
        Candidates.Free;
      end;
    end;

  function TSpeedbar.ReorderButton( OldIndex, aLeft, aTop : integer ) : boolean;
    var
      NewIndex : integer;
      Control  : TControl;
    begin
      Result   := false;
      NewIndex := ButtonIndex( OldIndex, aLeft, aTop );

      if NewIndex <> OldIndex
        then
          begin
            // If we are inserting to the right of our deletion then account for shift
            if OldIndex < NewIndex
              then Dec( NewIndex );
            Control := fButtons[OldIndex];
            fButtons.Delete( OldIndex );
            fButtons.Insert( NewIndex, Control );
            BeginUpdate;
            try
              Perform( TB_DELETEBUTTON, OldIndex, 0 );
              UpdateItem( TB_INSERTBUTTON, NewIndex, NewIndex );
            finally
              EndUpdate;
            end;
            Result := true;
          end;
    end;

  procedure TSpeedbar.AdjustControl( Control : TControl );
    var
      i, Pos                 : integer;
      R                      : TRect;
      Reordered, NeedsUpdate : boolean;
    begin
      Pos := fButtons.IndexOf( Control );
      if Pos <> -1
        then
          begin
            Reordered := ReorderButton( Pos, Control.Left, Control.Top );
            NeedsUpdate := false;
            if Reordered
              then
                begin
                  i := fButtons.IndexOf( Control );
                  if i < Pos then Pos := i;
                end
              else
                if Perform( TB_GETITEMRECT, Pos, Longint( @R ) ) <> 0
                  then
                    begin
                      NeedsUpdate := Control.Width <> R.Right - R.Left;
                      Reordered := NeedsUpdate;
                    end;
            if Control.Height <> ButtonHeight
              then ButtonHeight := Control.Height
              else
                if Reordered
                  then
                    begin
                      if NeedsUpdate
                        then UpdateButton( Pos );
                      ResizeButtons;
                      RepositionButtons( 0 );
                    end
                  else RepositionButton( Pos );
          end;
    end;

  procedure TSpeedbar.AlignControls( AControl : TControl; var Rect : TRect );
    begin
      if (fUpdateCount <= 0) and Assigned( AControl ) and not ( AControl is TSpeedbarButton )
        then AdjustControl( AControl );
    end;

  procedure TSpeedbar.WMEraseBkgnd( var Message : TWMEraseBkgnd );
    begin
      if Flat and ( Parent is TReBar )
        then
          begin
            DefaultHandler( Message );
            RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN );
          end
        else inherited;
    end;

  procedure TSpeedbar.WMNotifyFormat( var Message : TMessage );
    begin
      with Message do
        Result := DefWindowProc( Handle, Msg, WParam, LParam );
    end;

  procedure TSpeedbar.WMSize( var Message : TWMSize );
    var
      LastIndex   : integer;
      PrevR, NewR : TRect;
      Resized     : boolean;
    begin
      if Wrapable
        then
          begin
            // Detect change in height and adjust if necessary
            LastIndex := InternalButtonCount - 1;
            if LastIndex >= 0
              then Perform( TB_GETITEMRECT, LastIndex, integer( @PrevR ) );
            inherited;
            if LastIndex >= 0
              then
                begin
                  Perform( TB_GETITEMRECT, LastIndex, integer( @NewR ) );
                  if ( NewR.Right <> PrevR.Right ) or ( NewR.Bottom <> PrevR.Bottom )
                    then
                      begin
                        Resized := ( Align in [alNone, alLeft, alRight] ) and ( NewR.Right <> PrevR.Right ) or
                          not ( Align in [alNone, alLeft, alRight] ) and ( NewR.Bottom <> PrevR.Bottom );
                        if Resized and AutoSize
                          then AdjustSize;
                        RepositionButtons( 0 );
                      end;
                end;
          end
        else inherited;
      if Flat and HandleAllocated
        then RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME );
    end;

  procedure TSpeedbar.WMWindowPosChanged( var Message : TWMWindowPosChanged );
    begin
      inherited;
      if not ( csLoading in ComponentState ) and ( Message.WindowPos^.flags and SWP_NOSIZE = 0 )
        then Resize;
    end;

  procedure TSpeedbar.WMWindowPosChanging( var Message : TWMWindowPosChanging );
    var
      LastIndex : integer;
      OldSize   : integer;
      Resized   : boolean;
      R         : TRect;

    function BorderSize( Vertical : boolean ) : integer;
      const
        EdgeWidths : array[boolean, boolean] of integer = ( ( 0, 1 ), ( 1, 2 ) );
      var
        Adjust : integer;
      begin
        Result := 2 * BorderWidth;
        Adjust := EdgeWidths[EdgeInner <> esNone, EdgeOuter <> esNone];
        if Adjust <> 0
          then
            if Vertical
              then
                begin
                  if ebTop in EdgeBorders
                    then Inc( Result, Adjust );
                  if ebBottom in EdgeBorders
                    then Inc( Result, Adjust );
                end
              else
                begin
                  if ebLeft in EdgeBorders
                    then Inc( Result, Adjust );
                  if ebRight in EdgeBorders
                    then Inc( Result, Adjust );
                end;
      end;

    begin
      if AutoSize
        then
          begin
            LastIndex := InternalButtonCount - 1;
            if ( ( LastIndex >= 0 ) or
               not ( csDesigning in ComponentState ) ) and ( Message.WindowPos^.flags and SWP_NOSIZE = 0 )
              then
                begin
                  if LastIndex >= 0
                    then Perform( TB_GETITEMRECT, LastIndex, integer( @R ) )
                    else R := Rect( 0, 0, 0, 0 );
                  with Message.WindowPos^ do
                    case Align of
                      alNone :
                        begin
                          OldSize := cx;
                          cx      := R.Right + BorderSize( false );
                          // Take maximum size
                          if cx > OldSize
                            then Resized := true
                            else
                              begin
                                cx := OldSize;
                                Resized := false;
                              end;
                          OldSize := cy;
                          cy := R.Bottom + BorderSize( true );
                          // Take maximum size
                          if cy > OldSize
                            then Resized := true
                            else cy := OldSize;
                        end;
                      alLeft, alRight :
                        begin
                          OldSize := cx;
                          cx      := R.Right + BorderSize( false );
                          Resized := cx <> OldSize;
                        end;
                      alTop, alBottom :
                        begin
                          OldSize := cy;
                          cy      := R.Bottom + BorderSize( true );
                          Resized := cy <> OldSize;
                        end;
                    else
                      Resized := false;
                    end;
                  if Resized
                    then PostMessage( Handle, CN_REQUESTALIGN, 0, 0 );
                end;
          end;
      inherited;
    end;

  procedure TSpeedbar.WndProc( var Message : TMessage );
    var
      Control : TControl;
      CapControl : TControl;

    function IsSpeedbarButtonMouseMsg( var Message : TWMMouse ) : boolean;
      begin
        if GetCapture = Handle
          then
            begin
              CapControl := GetCaptureControl;
              if Assigned( CapControl ) and ( CapControl.Parent <> Self )
                then CapControl := nil;
            end;
        Control := ControlAtPos( SmallPointToPoint( Message.Pos ), false );
        Result  := Assigned( Control ) and ( Control is TSpeedbarButton ) and not Control.Dragging;
      end;

    begin
      if not ( csDesigning in ComponentState )
        then
          case Message.Msg of
            WM_MOUSEMOVE :
              if Flat
                then
                  begin
                    // Default hit-test for flat style buttons in Speedbar is off by 1
                    //  pixel in x and y. Adjust here so that default painting will occur
                    //  for tool buttons.
                    Inc( Message.LParamLo );
                    Inc( Message.LParamHi );
                    DefaultHandler( Message );
                    Dec( Message.LParamLo );
                    Dec( Message.LParamHi );
                  end
                else DefaultHandler( Message );
            WM_LBUTTONUP :
              if IsSpeedbarButtonMouseMsg( TWMMouse( Message ) ) and ( CapControl = Control )
                then
                  begin
                    DefaultHandler( Message );
                    UpdateButtonStates;
                  end;
            WM_LBUTTONDOWN, WM_LBUTTONDBLCLK :
              if IsSpeedbarButtonMouseMsg( TWMMouse( Message ) )
                then
                  begin
                    inherited WndProc( Message );
                    if not Control.Dragging
                      then
                        begin
                          DefaultHandler( Message );
                          if TSpeedbarButton( Control ).CheckMenuDropDown
                            then
                              begin
                                Message.Msg := WM_LBUTTONUP;
                                DefaultHandler( Message );
                                inherited;
                              end;
                        end;
                    exit;
                  end;
          end;
      inherited;
    end;

  procedure TSpeedbar.CMControlChange( var Message : TCMControlChange );
    begin
      with Message do
        if Inserting
          then InsertButton( Control )
          else RemoveButton( Control );
    end;

  procedure TSpeedbar.CMEnabledChanged( var Message : TMessage );
    begin
      inherited;
      Broadcast( Message );
    end;

  procedure TSpeedbar.CMColorChanged( var Message : TMessage );
    begin
      inherited;
      RecreateWnd;
    end;

  procedure TSpeedbar.CMSysFontChanged( var Message : TMessage );
    begin
      inherited;
      RecreateWnd;
    end;

  procedure TSpeedbar.CNRequestAlign( var Message : TMessage );
    begin
      RequestAlign;
    end;

  // TReBand =======================================================================================

  constructor TReBand.Create( Collection : TCollection );
    begin
      fWidth           := 40;
      fBreak           := true;
      fColor           := clBtnFace;
      fFixedBackground := true;
      fImageIndex      := -1;
      fMinHeight       := 25;
      fParentColor     := true;
      fParentBitmap    := true;
      fBitmap          := TBitmap.Create;
      fBitmap.OnChange := BitmapChanged;
      fVisible         := true;
      fDDB             := TBitmap.Create;

      inherited;

      ParentColorChanged;
      ParentBitmapChanged;
    end;

  destructor TReBand.Destroy;
    var
      AControl : TWinControl;
    begin
      fDDB.Free;
      fBitmap.Free;
      AControl := Control;
      fControl := nil;

      inherited;

      if Assigned( AControl ) and
         not ( csDestroying in AControl.ComponentState ) and AControl.HandleAllocated
        then
          begin
            AControl.BringToFront;
            AControl.Perform( CM_SHOWINGCHANGED, 0, 0 );
          end;
    end;

  procedure TReBand.Assign( Source : TPersistent );

    function FindControl( AControl : TWinControl ) : TWinControl;
      begin
        if Assigned( AControl )
          then Result := ReBar.Owner.FindComponent( AControl.Name ) as TWinControl
          else Result := nil;
      end;

  begin
    if Source is TReBand
      then
        begin
          Bitmap          := TReBand( Source ).Bitmap;
          Break           := TReBand( Source ).Break;
          Color           := TReBand( Source ).Color;
          FixedBackground := TReBand( Source ).FixedBackground;
          FixedSize       := TReBand( Source ).FixedSize;
          HorizontalOnly  := TReBand( Source ).HorizontalOnly;
          ImageIndex      := TReBand( Source ).ImageIndex;
          MinHeight       := TReBand( Source ).MinHeight;
          MinWidth        := TReBand( Source ).MinWidth;
          ParentBitmap    := TReBand( Source ).ParentBitmap;
          ParentColor     := TReBand( Source ).ParentColor;
          Text            := TReBand( Source ).Text;
          Visible         := TReBand( Source ).Visible;
          Width           := TReBand( Source ).Width;
          Control         := FindControl( TReBand( Source ).Control );
        end
      else
        inherited;
  end;

  function TReBand.GetDisplayName : string;
    begin
      Result := fText;
      if Result = ''
        then Result := inherited GetDisplayName;
    end;

  function TReBand.GetVisible : boolean;
    begin
      Result := fVisible and ( not ReBar.Vertical or not fHorizontalOnly );
    end;

  function TReBand.ReBar : TReBar;
    begin
      Result := TReBands( Collection ).fReBar;
    end;

  procedure TReBand.ParentColorChanged;
    begin
      if fParentColor
        then
          begin
            SetColor( ReBar.Color );
            fParentColor := true;
          end;
    end;

  procedure TReBand.ParentBitmapChanged;
    begin
      BitmapChanged( Self );
    end;

  procedure TReBand.BitmapChanged( Sender : TObject );
    begin
      if not ParentBitmap
        then
          begin
            fDDB.Assign( fBitmap );
            if not fDDB.Empty
              then fDDB.HandleType := bmDDB;
          end
        else fDDB.Assign( nil );
      Changed( false );
    end;

  procedure TReBand.SetBitmap( Value : TBitmap );
    begin
      fParentBitmap := false;
      fBitmap.Assign( Value );
      Changed( true );
    end;

  function TReBand.GetHeight : integer;
    begin
      if Visible
        then Result := ReBar.GetRowHeight( fID )
        else Result := 0;
    end;

  procedure TReBand.SetBorderStyle( Value : TBorderStyle );
    begin
      if fBorderStyle <> Value
        then
          begin
            fBorderStyle := Value;
            Changed( false );
          end;
    end;

  procedure TReBand.SetBreak( Value : boolean );
    begin
      if fBreak <> Value
        then
          begin
            fBreak := Value;
            Changed( false );
          end;
    end;

  procedure TReBand.SetFixedSize( Value : boolean );
    begin
      if fFixedSize <> Value
        then
          begin
            if Value
              then
                begin
                  fBreak := false;
                  fFixedSize := true;
                  Changed( true );
                end
              else
                begin
                  fFixedSize := false;
                  Changed( false );
                end;
          end;
    end;

  procedure TReBand.SetMinHeight( Value : integer );
    begin
      if fMinHeight <> Value
        then
          begin
            fMinHeight := Value;
            Changed( false );
          end;
    end;

  procedure TReBand.SetMinWidth( Value : integer );
    begin
      if fMinWidth <> Value
        then
          begin
            fMinWidth := Value;
            Changed( FixedSize );
          end;
    end;

  procedure TReBand.SetVisible( Value : boolean );
    begin
      if fVisible <> Value
        then
          begin
            fVisible := Value;
            Changed( true );
          end;
    end;

  procedure TReBand.SetHorizontalOnly( Value : boolean );
    begin
      if fHorizontalOnly <> Value
        then
          begin
            fHorizontalOnly := Value;
            Changed( ReBar.Vertical );
          end;
    end;

  procedure TReBand.SetImageIndex( Value : integer );
    begin
      if fImageIndex <> Value
        then
          begin
            fImageIndex := Value;
            Changed( false );
          end;
    end;

  procedure TReBand.SetFixedBackground( Value : boolean );
    begin
      if fFixedBackground <> Value
        then
          begin
            fFixedBackground := Value;
            Changed( false );
          end;
    end;

  procedure TReBand.SetColor( Value : TColor );
    begin
      if fColor <> Value
        then
          begin
            fColor := Value;
            fParentColor := false;
            Changed( false );
          end;
    end;

  procedure TReBand.SetControl( Value : TWinControl );
    var
      Band        : TReBand;
      PrevControl : TWinControl;
    begin
      if fControl <> Value
        then
          begin
            if Assigned( Value )
              then
                begin
                  Band := TReBands( Collection ).FindBand( Value );
                  if Assigned( Band ) and ( Band <> Self )
                    then Band.SetControl( nil );
                end;
            PrevControl := fControl;
            fControl    := Value;
            Changed( true );
            if Assigned( PrevControl )
              then PrevControl.Perform( CM_SHOWINGCHANGED, 0, 0 );
          end;
    end;

  procedure TReBand.SetText( const Value : string );
    begin
      if fText <> Value
        then
          begin
            fText := Value;
            Changed( true );
          end;
    end;

  function TReBand.IsColorStored : boolean;
    begin
      Result := not ParentColor;
    end;

  procedure TReBand.SetParentColor( Value : boolean );
    begin
      if fParentColor <> Value
        then
          begin
            fParentColor := Value;
            Changed( false );
          end;
    end;

  function TReBand.IsBitmapStored : boolean;
    begin
      Result := not ParentBitmap;
    end;

  procedure TReBand.SetParentBitmap( Value : boolean );
    begin
      if fParentBitmap <> Value
        then
          begin
            fParentBitmap := Value;
            ParentBitmapChanged;
          end;
    end;

  procedure TReBand.SetWidth( Value : integer );
    begin
      if fWidth <> Value
        then
          begin
            fWidth := Value;
            Changed( false );
          end;
    end;

  // TReBands =======================================================================================

  constructor TReBands.Create( ReBar : TReBar );
    begin
      inherited Create( TReBand );

      fReBar := ReBar;
    end;

  function TReBands.Add : TReBand;
    begin
      Result := TReBand( inherited Add );
    end;

  function TReBands.FindBand( AControl : TControl ) : TReBand;
    var
      i : integer;
    begin
      for i := 0 to Count - 1 do
        begin
          Result := TReBand( inherited GetItem( i ) );
          if Result.fControl = AControl
            then Exit;
        end;
      Result := nil;
    end;

  function TReBands.HaveGraphic : boolean;
    var
      i : integer;
    begin
      Result := false;

      for i := 0 to Count - 1 do
        if not Items[i].fDDB.Empty
          then
            begin
              Result := true;
              Exit;
            end;
    end;

  function TReBands.GetItem( Index : integer ) : TReBand;
    begin
      Result := TReBand( inherited GetItem( Index ) );
    end;

  function TReBands.GetOwner : TPersistent;
    begin
      Result := fReBar;
    end;

  procedure TReBands.SetItem( Index : integer; Value : TReBand );
    begin
      inherited SetItem( Index, Value );
    end;

  procedure TReBands.Update( Item : TCollectionItem );
    begin
      if ( Item <> nil )
        then fReBar.UpdateBand( Item.Index )
        else fReBar.UpdateBands;
    end;

  // TReBar =======================================================================================

  const
    GripSize = 6;

    // Results for PtInGripRect
    grNone = 0;
    grGrip = 1;
    grCaption = 2;

  constructor TReBar.Create( AOwner : TComponent );
    begin
      CheckCommonControl( ICC_COOL_CLASSES );

      inherited;

      FComponentStyle := ComponentStyle - [csInheritable];
      ControlStyle   := [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];

      Height      := 75;
      Align       := alTop;
      ParentColor := true;
      ParentFont  := true;

      fBandBorderStyle := bsSingle;
      fBitmap          := TBitmap.Create;
      fBitmap.OnChange := BitmapChanged;
      fCaptionFont     := TFont.Create;
      fShowText        := true;
      fDoubleBuffered  := true;
      fDDB             := TBitmap.Create;
      fBands           := TReBands.Create( Self );

      fImageChangeLink          := TChangeLink.Create;
      fImageChangeLink.OnChange := ImageListChange;
    end;

  destructor TReBar.Destroy;
    begin
      fBands.Free;
      fImageChangeLink.Free;
      fDDB.Free;
      fCaptionFont.Free;
      fBitmap.Free;

      inherited;
    end;

  procedure TReBar.CreateParams( var Params : TCreateParams );
    const
      DefaultStyles    = CCS_NOPARENTALIGN or CCS_NODIVIDER;

      AutoSizeStyles   : array[boolean] of integer = ( CCS_NORESIZE, 0 );
      BandBorderStyles : array[TBorderStyle] of integer = ( 0, RBS_BANDBORDERS );
      FixedStyles      : array[boolean] of integer = ( 0, RBS_FIXEDORDER );
      HeightStyles     : array[boolean] of integer = ( RBS_VARHEIGHT, 0 );
      VerticalStyles   : array[boolean] of integer = ( 0, CCS_VERT );
    begin
      inherited;
      CreateSubClass( Params, REBARCLASSNAME );
      with Params do
        begin
          Style := Style or DefaultStyles or
            AutoSizeStyles[IsAutoSized] or BandBorderStyles[fBandBorderStyle] or
            FixedStyles[fFixedOrder] or HeightStyles[fFixedSize] or
            VerticalStyles[fVertical];
          WindowClass.style := WindowClass.style and not ( CS_HREDRAW or CS_VREDRAW ) or CS_DBLCLKS;
        end;
    end;

  procedure TReBar.CreateWnd;
    begin
      inherited;
      fCaptionFont.Handle := GetCaptionFont;
      fCaptionFontHeight  := GetCaptionFontHeight;
      if not ( csLoading in ComponentState )
        then UpdateBands;
    end;

  procedure TReBar.AdjustSize;
    begin
      Perform( WM_SIZE, SIZE_RESTORED, word( Width ) or word( Height ) shl 16 );
    end;

  procedure TReBar.Loaded;
    begin
      inherited;
      
      UpdateBands;
    end;

  procedure TReBar.AlignControls( AControl : TControl; var Rect : TRect );
    var
      Band                                      : TReBand;
      NewWidth, NewMinHeight, CaptionSize, W, H : integer;
      DoUpdate                                  : boolean;

    function IsBandCurrent : boolean;
      var
        BandInfo : TReBarBandInfo;
      begin
        BandInfo.cbSize := SizeOf( TReBarBandInfo );
        BandInfo.fMask  := RBBIM_CHILD;
        Result := TWinControl( AControl ).HandleAllocated and
          ( Perform( RB_GETBANDINFO, Band.fID, integer( @BandInfo ) ) <> 0 ) and
          ( BandInfo.hwndChild = TWinControl( AControl ).Handle );
      end;

    begin
      if not ( csDestroying in ComponentState ) and ( not Assigned( AControl ) or
        ( AControl is TWinControl ) ) and ( fUpdateCount = 0 )
        then
          begin
            // Refresh bands if any control changed
            if Assigned( AControl )
              then
                begin
                  ReadBands;
                  Band := fBands.FindBand( AControl as TWinControl );
                  if Assigned( Band )
                    then
                      begin
                        BeginUpdate;
                        try
                          CaptionSize := GetCaptionSize( Band );
                          if Vertical
                            then
                              begin
                                W := AControl.Height;
                                H := AControl.Width;
                              end
                            else
                              begin
                                W := AControl.Width;
                                H := AControl.Height;
                              end;
                          NewWidth     := W + CaptionSize;
                          NewMinHeight := H;
                          if ( NewWidth <> Band.Width ) or ( NewMinHeight <> Band.MinHeight ) or not IsBandCurrent
                            then
                              begin
                                DoUpdate := true;
                                Band.Width := NewWidth;
                                Band.MinHeight := NewMinHeight;
                              end
                            else
                              DoUpdate := false;
                        finally
                          EndUpdate;
                        end;
                        if DoUpdate
                          then
                            begin
                              Bands.Update( Band );
                              ReadBands;
                            end
                          else AdjustSize; // Let Rebar reposition child windows
                      end;
                end;
          end;
    end;

  procedure TReBar.Change;
    var
      Form : TCustomForm;
    begin
      if csDesigning in ComponentState
        then
          begin
            Form := GetParentForm( Self );
            if ( Form <> nil ) and ( Form.Designer <> nil )
              then Form.Designer.Modified;
          end;
      if Assigned( fOnChange )
        then fOnChange( Self );
    end;

  procedure TReBar.Resize;
    begin
      if Assigned( fOnResize )
        then fOnResize( Self );
    end;

  function TReBar.GetAlign : TAlign;
    begin
      Result := inherited Align;
    end;

  // Rebars take their text font from Windows' caption font minus any bold
  //  characteristics it may have.
  function TReBar.GetCaptionFont : HFONT;
    var
      NonClientMetrics : TNonClientMetrics;
    begin
      with NonClientMetrics do
        begin
          cbSize := sizeof( TNonClientMetrics );
          if not SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0 )
            then GetObject( GetStockObject( SYSTEM_FONT ), SizeOf( lfCaptionFont ), @lfCaptionFont );
          // Remove any bold styles
          lfCaptionFont.lfWeight := FW_NORMAL;
          Result := CreateFontIndirect( lfCaptionFont )
        end;
    end;

  function TReBar.GetCaptionFontHeight : integer;
    var
      TxtMetric : TTextMetric;
    begin
      Result := 0;
      if HandleAllocated
        then
          with TControlCanvas.Create do
            try
              Control := Self;
              Font := fCaptionFont;
              if ( GetTextMetrics( Handle, TxtMetric ) )
                then Result := TxtMetric.tmHeight;
            finally
              Free;
            end;
    end;

  // Return height/width ( depending on Vertical property ) of Rebar grip area
  function TReBar.GetCaptionSize( Band : TReBand ) : integer;
    const
      Margin = 10;
    var
      Text   : string;
      Adjust : boolean;
    begin
      Result := 0;
      Adjust := false;
      if Assigned( Band ) and ( ( csDesigning in ComponentState ) or Band.Visible )
        then
          begin
            if ( csDesigning in ComponentState ) and not Assigned( Band.Control ) and ( Band.Text = '' )
              then Text := Band.DisplayName
              else Text := Band.Text;
            if ShowText and ( Text <> '' )
              then
                begin
                  Adjust := true;
                  with TControlCanvas.Create do
                    try
                      Control := Self;
                      Font := fCaptionFont;
                      if Vertical
                        then Result := fCaptionFontHeight
                        else Result := TextWidth( Text );
                    finally
                      Free;
                    end;
                end;
            if Band.ImageIndex >= 0
              then
                begin
                  if Adjust
                    then Inc( Result, 2 );
                  if Assigned( fImages )
                    then
                      begin
                        Adjust := true;
                        if Vertical
                          then Inc( Result, fImages.Height )
                          else Inc( Result, fImages.Width );
                      end
                    else
                      if not Adjust
                        then Inc( Result, GripSize - 2 );
                end;
            if Adjust
              then Inc( Result, GripSize - 2 );
            if ( not FixedOrder or ( Band.fID > 0 ) ) and not Band.FixedSize
              then Inc( Result, GripSize + Margin );
          end;
    end;

  procedure TReBar.SetAlign( Value : TAlign );
    var
      PrevAlign, NewAlign : TAlign;
    begin
      PrevAlign := inherited Align;
      inherited Align := Value;
      NewAlign := inherited Align;
      if NewAlign <> PrevAlign
        then
          begin
            case NewAlign of
              alLeft, alRight :
                fVertical := true;
              alTop, alBottom :
                fVertical := false;
            end;
            if not ( csLoading in ComponentState )
              then
                begin
                  Perform( WM_SETREDRAW, 0, 0 );
                  try
                    RecreateWnd;
                  finally
                    Perform( WM_SETREDRAW, 1, 0 );
                  end;
                end;
          end;
    end;

  procedure TReBar.SetAutoSize( Value : boolean );
    begin
      if fAutoSize <> Value
        then
          begin
            RequestAlign;
            fAutoSize := Value;
            if not ( csLoading in ComponentState )
              then
                begin
                  Perform( WM_SETREDRAW, 0, 0 );
                  try
                    RecreateWnd;
                  finally
                    Perform( WM_SETREDRAW, 1, 0 );
                  end;
                end;
          end;
    end;

  procedure TReBar.SetBands( Value : TReBands );
    begin
      fBands.Assign( Value );
    end;

  procedure TReBar.SetBandBorderStyle( Value : TBorderStyle );
    begin
      if fBandBorderStyle <> Value
        then
          begin
            fBandBorderStyle := Value;
            RecreateWnd;
          end;
    end;

  procedure TReBar.SetFixedSize( Value : boolean );
    begin
      if fFixedSize <> Value
        then
          begin
            fFixedSize := Value;
            RecreateWnd;
          end;
    end;

  procedure TReBar.SetFixedOrder( Value : boolean );
    begin
      if fFixedOrder <> Value
        then
          begin
            fFixedOrder := Value;
            RecreateWnd;
          end;
    end;

  procedure TReBar.ImageListChange( Sender : TObject );
    begin
      if HandleAllocated and ( Sender = Images )
        then SetImageList( Images.Handle );
    end;

  procedure TReBar.SetImageList( Value : HImageList );
    var
      BarInfo : TReBarInfo;
    begin
      if HandleAllocated
        then
          begin
            if Value = 0
              then RecreateWnd
              else
                begin
                  BarInfo.cbSize := SizeOf( TReBarInfo );
                  BarInfo.fMask  := RBIM_IMAGELIST;
                  BarInfo.himl   := Value;
                  Perform( RB_SETBARINFO, 0, integer( @BarInfo ) );
                  Invalidate;
                end;
          end;
    end;

  procedure TReBar.SetImages( Value : TImageList );
    begin
      if fImages <> nil
        then fImages.UnRegisterChanges( fImageChangeLink );
      fImages := Value;
      if fImages <> nil
        then
          begin
            fImages.RegisterChanges( fImageChangeLink );
            SetImageList( fImages.Handle );
          end
        else SetImageList( 0 );
    end;

  procedure TReBar.SetShowText( Value : boolean );
    begin
      if fShowText <> Value
        then
          begin
            fShowText := Value;
            if not ( csLoading in ComponentState )
              then UpdateBands;
          end;
    end;

  procedure TReBar.Notification( aComponent : TComponent; Operation : TOperation );
    var
      Band : TReBand;
    begin
      inherited;
      if not ( csDestroying in ComponentState ) and ( Operation = opRemove )
        then
          begin
            if ( aComponent is TWinControl )
              then
                begin
                  Band := Bands.FindBand( TControl( aComponent ) );
                  if Assigned( Band ) then Band.fControl := nil;
                end
              else
                if aComponent = fImages
                  then Images := nil;
          end;
    end;

  function TReBar.GetPalette : HPALETTE;
    begin
      if not fDDB.Empty
        then Result := fDDB.Palette
        else Result := inherited GetPalette;
    end;

  procedure TReBar.BitmapChanged( Sender : TObject );
    var
      i : integer;
    begin
      fDDB.Assign( fBitmap );
      if not fDDB.Empty
        then fDDB.HandleType := bmDDB;
      for i := 0 to fBands.Count - 1 do
        Bands[i].ParentBitmapChanged;
      if HandleAllocated
        then RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN );
    end;

  procedure TReBar.BeginUpdate;
    begin
      Inc( fUpdateCount );
    end;

  procedure TReBar.EndUpdate;
    begin
      Dec( fUpdateCount );
    end;

  function TReBar.IsAutoSized : boolean;
    begin
      Result := fAutoSize and ( ( fVertical and ( Align in [alLeft, alRight] ) ) or
        not fVertical and ( Align in [alTop, alBottom] ) );
    end;

  function TReBar.IsBackgroundDirty : boolean;
    begin
      Result := HandleAllocated and not IsAutoSized;
    end;

  procedure TReBar.SetBitmap( Value : TBitmap );
    begin
      fBitmap.Assign( Value );
    end;

  procedure TReBar.SetVertical( Value : boolean );
    var
      R : TRect;
    begin
      if fVertical <> Value
        then
          begin
            fVertical := Value;
            R := BoundsRect;
            Perform( WM_SETREDRAW, 0, 0 );
            try
              RecreateWnd;
            finally
              Perform( WM_SETREDRAW, 1, 0 );
            end;
            BoundsRect := R;
            Invalidate;
          end;
    end;

  procedure TReBar.DisableBands;
    var
      i        : integer;
      BandInfo : TReBarBandInfo;
    begin
      if HandleAllocated
        then
          begin
            BandInfo.cbSize := SizeOf( TReBarBandInfo );
            BandInfo.fMask := RBBIM_CHILD;
            BandInfo.hwndChild := 0;
            for i := 0 to fBands.fVisibleCount - 1 do
              Perform( RB_SETBANDINFO, i, integer( @BandInfo ) );
          end;
    end;

  function TReBar.UpdateItem( Message, FromIndex, ToIndex : integer ) : boolean;
    const
      BorderStyles    : array[TBorderStyle] of integer = ( 0, RBBS_CHILDEDGE );
      BreakStyles     : array[boolean] of integer = ( 0, RBBS_BREAK );
      FixedBmpStyles  : array[boolean] of integer = ( 0, RBBS_FIXEDBMP );
      FixedSizeStyles : array[boolean] of integer = ( 0, RBBS_FIXEDSIZE );
    var
      BandInfo : TReBarBandInfo;
      Band     : TReBand;
      Text     : string;
    begin
      Result := false;
      if HandleAllocated
        then
          begin
            Band := Bands[FromIndex];
            if Assigned( Band.Control )
              then
                with Band.Control do
                  begin
                    BeginUpdate;
                    try
                      Parent := Self;
                    finally
                      EndUpdate;
                    end;
                    if not ( csDesigning in Self.ComponentState )
                      then Visible := Band.Visible;
                  end;
            if not ( csDesigning in ComponentState ) and not Band.Visible
              then Exit;
            FillChar( BandInfo, SizeOf( BandInfo ), 0 );
            with BandInfo do
              begin
                cbSize := SizeOf( TReBarBandInfo );
                wID := integer( Band );
                if Band.ParentColor
                  then clrBack := ColorToRGB( Color )
                  else clrBack := ColorToRGB( Band.Color );
                with Band do
                  fStyle := BreakStyles[Break] or FixedSizeStyles[FixedSize] or
                    BorderStyles[BorderStyle] or FixedBmpStyles[FixedBackground];
                fMask := RBBIM_STYLE or RBBIM_COLORS or RBBIM_SIZE or RBBIM_BACKGROUND or RBBIM_IMAGE or RBBIM_ID;
                if Band.ParentBitmap
                  then hbmBack := fDDB.Handle
                  else hbmBack := Band.fDDB.Handle;
                iImage := Band.ImageIndex;
                if Assigned( Band.Control ) and Band.Control.Visible
                  then hwndChild := Band.Control.Handle;
                cx := Band.Width;
                if Band.FixedSize and ( Band.MinWidth <= 0 ) and Assigned( Band.Control )
                  then cxMinChild := Band.Control.Width
                  else cxMinChild := Band.MinWidth;
                cyMinChild := Band.MinHeight;
                fMask := fMask or RBBIM_CHILD or RBBIM_CHILDSIZE;
                if ShowText
                  then
                    begin
                      if ( csDesigning in ComponentState ) and not Assigned( Band.Control ) and
                        ( Band.Text = '' )
                        then Text := Band.DisplayName
                        else Text := Band.Text;
                      lpText := PChar( Text );
                      fMask := fMask or RBBIM_TEXT;
                    end;
              end;
            Result := Perform( Message, ToIndex, integer( @BandInfo ) ) <> 0;
          end;
    end;

  procedure TReBar.UpdateBand( Index : integer );
    begin
      if HandleAllocated and ( fUpdateCount = 0 )
        then
          with Bands[Index] do
            UpdateItem( RB_SETBANDINFO, fID, fID )
    end;

  function TReBar.ReadBands : boolean;
    var
      i        : integer;
      BandInfo : TReBarBandInfo;
      Band     : TReBand;
      NewBreak : boolean;
      NewWidth : integer;
      NewIndex : integer;

    function FindNextVisible( Position : integer ) : integer;
      begin
        if Position >= fBands.Count - 1
          then Result := Position
          else
            for Result := Position to fBands.Count - 2 do
              if fBands[Result].Visible
                then Break;
      end;

    begin
      Result := false;
      if HandleAllocated and ( fUpdateCount = 0 )
        then
          begin
            // Retrieve current band settings
            BandInfo.cbSize := SizeOf( TReBarBandInfo );
            BandInfo.fMask := RBBIM_STYLE or RBBIM_SIZE or RBBIM_ID;
            BeginUpdate;
            try
              for i := 0 to fBands.fVisibleCount - 1 do
                if ( Perform( RB_GETBANDINFO, i, integer( @BandInfo ) ) <> 0 ) and ( BandInfo.wID <> 0 )
                  then
                    with BandInfo do
                      begin
                        Band := TReBand( wID );
                        with Band do
                          begin
                            fID := i;
                            NewBreak := fStyle and RBBS_BREAK <> 0;
                            if Visible
                              then NewIndex := FindNextVisible( i )
                              else NewIndex := i;
                            NewWidth := cx;
                            if ( Break <> NewBreak ) or ( Index <> NewIndex ) or ( Width <> NewWidth )
                              then
                                begin
                                  Result := true;
                                  Break  := NewBreak;
                                  Index  := NewIndex;
                                  Width  := NewWidth;
                                end;
                          end;
                      end;
            finally
              EndUpdate;
            end;
          end;
      if Result
        then Change;
    end;

  procedure TReBar.UpdateBands;
    var
      i : integer;
    begin
      if HandleAllocated and ( fUpdateCount = 0 )
        then
          begin
            BeginUpdate;
            Perform( WM_SETREDRAW, 0, 0 );
            try
              DisableBands;
              for i := 0 to Perform( RB_GETBANDCOUNT, 0, 0 ) - 1 do
                Perform( RB_DELETEBAND, 0, 0 );
              if FixedOrder
                then // Add bands from first to last
                  for i := 0 to Bands.Count - 1 do
                    UpdateItem( RB_INSERTBAND, i, -1 )
                else
                  // Add bands from last to first
                  for i := Bands.Count - 1 downto 0 do
                    UpdateItem( RB_INSERTBAND, i, 0 );
              if Assigned( fImages )
                then SetImageList( fImages.Handle );
            finally
              Perform( WM_SETREDRAW, 1, 0 );
              EndUpdate;
            end;
            fBands.fVisibleCount := Perform( RB_GETBANDCOUNT, 0, 0 );
            ReadBands;
            if IsAutoSized
              then AdjustSize;
            RedrawWindow( Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN );
          end;
    end;

  // Return height of row for given band
  function TReBar.GetRowHeight( Index : integer ) : integer;
    var
      BandInfo                        : TReBarBandInfo;
      i, Len, MaxMinHeight, BandCount : integer;
    begin
      Result := 0;
      if ( fBands.fVisibleCount > 0 ) and ( Index < fBands.fVisibleCount )
        then
          begin
            BandInfo.cbSize := SizeOf( TReBarBandInfo );
            BandInfo.fMask  := RBBIM_STYLE;
            BandCount       := 0;
            MaxMinHeight    := 0;
            for i := 0 to fBands.fVisibleCount - 1 do
              if Perform( RB_GETBANDINFO, i, integer( @BandInfo ) ) <> 0
                then
                  begin
                    Len := Perform( RB_GETROWHEIGHT, i, 0 );
                    if ( i = 0 ) or ( BandInfo.fStyle and RBBS_BREAK <> 0 )
                      then
                        begin
                          if Index < i
                            then
                              begin
                                if ( BandCount > 0 ) and ( Len = Result ) then
                                  Result := MaxMinHeight;
                                Break;
                              end
                            else
                              begin
                                BandCount := 0;
                                MaxMinHeight := Len;
                                Result := 0
                              end;
                        end
                      else
                        begin
                          Inc( BandCount );
                          if ( Index > i ) and ( Len > MaxMinHeight )
                            then MaxMinHeight := Len;
                        end;
                    if Len > Result
                      then Result := Len;
                  end;
          end;
    end;

  // Return true if given point is within one of the grip areas
  function TReBar.PtInGripRect( const Pos : TPoint ) : integer;
    const
      BandBorderSize : array[TBorderStyle] of integer = ( 0, 2 );
    var
      BandInfo           : TReBarBandInfo;
      i                  : integer;
      W, H               : integer;
      x, y, PrevW, PrevH : integer;
      R                  : TRect;
    begin
      Result := grNone;
      if fBands.fVisibleCount > 0
        then
          begin
            x := 0;
            y := 0;
            PrevW := 0;
            PrevH := 0;
            BandInfo.cbSize := SizeOf( TReBarBandInfo );
            BandInfo.fMask := RBBIM_STYLE or RBBIM_SIZE or RBBIM_ID;
            for i := 0 to fBands.fVisibleCount - 1 do
              if ( Perform( RB_GETBANDINFO, i, integer( @BandInfo ) ) <> 0 ) and
                ( BandInfo.wID <> 0 )
                  then
                    begin
                      if ( i = 0 ) or ( BandInfo.fStyle and RBBS_BREAK <> 0 )
                        then
                          begin
                            x := 2;
                            Inc( y, PrevH );
                          end
                        else Inc( x, PrevW );
                      W := GetCaptionSize( TReBand( BandInfo.wID ) );
                      H := GetRowHeight( i );
                      if Vertical
                        then R := Rect( y, x, y + H, x + W )
                        else R := Rect( x, y, x + W, y + H );
                      if PtInRect( R, Pos )
                        then
                          begin
                            // Check if point is in caption only
                            if Vertical
                              then Inc( R.Top, GripSize )
                              else Inc( R.Left, GripSize );
                            if PtInRect( R, Pos )
                              then Result := grCaption
                              else Result := grGrip;
                            Exit;
                          end;
                      PrevH := H + BandBorderSize[BandBorderStyle];
                      if BandBorderStyle = bsNone
                        then Inc( PrevH );
                      PrevW := BandInfo.cx + BandBorderSize[BandBorderStyle];
                    end;
          end;
    end;

  procedure TReBar.WMCaptureChanged( var Message : TMessage );
    begin
      inherited;
      // Synchronize band properties - something may have changed
      if not ( csClicked in ControlState )
        then PostMessage( Handle, CN_BANDCHANGE, 0, 0 )
    end;

  procedure TReBar.WMEraseBkgnd( var Message : TWMEraseBkgnd );
    begin
      if IsBackgroundDirty or ( IsAutoSized and ( Bands.Count = 0 ) )
        then inherited;
      DefaultHandler( Message );
    end;

  procedure TReBar.WMLButtonDown( var Message : TWMLButtonDown );
    begin
      if ( PtInGripRect( SmallPointToPoint( Message.Pos ) ) = grNone )
        then inherited
        else
          begin
            fTrackDrag := Message.Pos;
            DefaultHandler( Message );
          end;
    end;

  procedure TReBar.WMLButtonUp( var Message : TWMLButtonUp );
    begin
      if not ( csDesigning in ComponentState ) or ( csClicked in ControlState ) or
        ( ( fTrackDrag.x < Message.XPos - 1 ) and ( fTrackDrag.x > Message.XPos + 1 ) and
        ( fTrackDrag.y < Message.YPos - 1 ) and ( fTrackDrag.y > Message.YPos + 1 ) )
        then inherited
        else MouseCapture := false;
    end;

  procedure TReBar.WMNotifyFormat( var Message : TMessage );
    begin
      with Message do
        Result := DefWindowProc( Handle, Msg, WParam, LParam );
    end;

  procedure TReBar.WMSetCursor( var Message : TWMSetCursor );
    var
      P    : TPoint;
      Grip : integer;
    begin
      with Message do
        if ( CursorWnd = Handle ) and ( Smallint( HitTest ) = HTCLIENT )
          then
            begin
              Result := 1;
              GetCursorPos( P );
              Grip := PtInGripRect( ScreenToClient( P ) );
              if Grip <> grNone
                then
                  begin
                    if Grip = grCaption
                      then Windows.SetCursor( Screen.Cursors[crHandPoint] )
                      else
                        if Vertical
                          then Windows.SetCursor( Screen.Cursors[crSizeNS] )
                          else Windows.SetCursor( Screen.Cursors[crSizeWE] );
                  end
                else Windows.SetCursor( Screen.Cursors[crDefault] );
            end
          else inherited;
    end;

  procedure TReBar.WMSize( var Message : TWMSize );
    begin
      inherited;
      if IsAutoSized
        then RequestAlign;
      ReadBands;
      if IsBackgroundDirty
        then Invalidate;
    end;

  procedure TReBar.WMWindowPosChanged( var Message : TWMWindowPosChanged );
    var
      R : TRect;
      Wnd : HWnd;
    begin
      if IsAutoSized
        then R := BoundsRect;
      inherited;
      if IsAutoSized
        then
          begin
            Wnd := GetParentHandle;
            if Wnd <> 0
              then InvalidateRect( Wnd, @R, true );
          end;
      if not ( csLoading in ComponentState ) and
        ( Message.WindowPos^.flags and SWP_NOSIZE = 0 )
        then Resize;
    end;

  procedure TReBar.WndProc( var Message : TMessage );
    begin
      if ( csDesigning in ComponentState )
        then
          case Message.Msg of
            WM_MOUSEMOVE, WM_RBUTTONDBLCLK :
              begin
                // Enabled csDesignInteractive temporarily so that we may handle the
                //  design-time dragging of bands
                ControlStyle := ControlStyle + [csDesignInteractive];
                try
                  inherited;
                finally
                  ControlStyle := ControlStyle - [csDesignInteractive];
                end;
                Exit;
              end;
            // We just dragged a band disable any drag events
            WM_LBUTTONUP :
              MouseCapture := false;
          end;
      inherited;
    end;

  procedure TReBar.CMColorChanged( var Message : TMessage );
    var
      i : integer;
    begin
      inherited;
      if Assigned( fBands )
        then
          for i := 0 to fBands.Count - 1 do
            Bands[i].ParentColorChanged;
      if HandleAllocated
        then InvalidateRect( Handle, nil, true );
    end;

  procedure TReBar.CMControlChange( var Message : TCMControlChange );
    var
      Band : TReBand;
    begin
      if fUpdateCount = 0
        then
          begin
            // Can only accept TWinControl descendants
            if not ( csLoading in ComponentState ) and ( Message.Control is TWinControl )
              then
                if Message.Inserting
                  then
                    with TReBand( Bands.Add ) do
                      SetControl( TWinControl( Message.Control ) )
                  else
                    begin
                      Band := Bands.FindBand( Message.Control );
                      if Assigned( Band )
                        then
                          begin
                            Band.Free;
                            Change;
                          end;
                    end;
          end;
    end;

  procedure TReBar.CMDesignHitTest( var Message : TCMDesignHitTest );
    begin
      if PtInGripRect( SmallPointToPoint( Message.Pos ) ) <> grNone
        then Message.Result := 1
        else inherited;
    end;

  procedure TReBar.CMSysColorChange( var Message : TMessage );
    begin
      inherited;
      if not ( csLoading in ComponentState )
        then
          begin
            Message.Msg := WM_SYSCOLORCHANGE;
            DefaultHandler( Message );
          end;
    end;

  procedure TReBar.CMSysFontChanged( var Message : TMessage );
    begin
      inherited;
      RecreateWnd;
    end;

  procedure TReBar.CMWinIniChange( var Message : TWMWinIniChange );
    begin
      inherited;
      fCaptionFont.Handle := GetCaptionFont;
      fCaptionFontHeight  := GetCaptionFontHeight;
    end;

  procedure TReBar.CNBandChange( var Message : TMessage );
    begin
      ReadBands;
    end;

  procedure TReBar.CNNotify( var Message : TWMNotify );
    begin
      if ( Message.NMHdr^.code = RBN_HEIGHTCHANGE ) and IsBackgroundDirty then
        Invalidate;
    end;

  // VCL Registration =======================================================================================

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TSpeedbar, TRebar] );
      RegisterNoIcon( [TSpeedbarButton] );
      RegisterClasses( [TSpeedbarButton, TReBand, TReBands] );
    end;

end.
