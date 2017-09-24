unit ListViewers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

  type
    TSizeOptions = (soClientHeight, soSystemLarge, soSystemSmall, soImageListSize);

  type
    TPaintMode = (pmNormal, pmSelected);

    EIconListViewerError = class (Exception);
    {
      TIconListViewer => Shows icons from ImageList object.

        function  IconAt(x, y : integer) : integer;
          > Returns the index, in the ImageList object, of the icon at the position "x", "y" in the
            control's area;

        procedure PaintIcon(const Index : integer; const Mode : TPaintMode);
          > Paints the icon specified by index(if it is Visible). "Mode" describes how to paint the
            icon : selected or not (pmSelected, pmNormal)
        procedure DrawIcons;
          > Paints all visibles icons
        function ItemVisible(const Item : integer) : boolean;
          > Returns true if the Item is completely visible
        function  VisiblesIcons : integer;
          > Returns the number of icons that can be painted in the control
        procedure UpdateRect(Rect : TRect);
          > Paints all icons that are, partially o completely, contained in "Rect"
        property ColorSelected : TColor;
          > Color used for the background of the selected icon
        property FirstVisibleIcon : integer;
          > The first icon
        property ImageList    : TImageList;
          > ImageList contains the images of the icons
        property ItemIndex    : integer;
          > The index of the selected Item
        property HorzIconSeparation : integer default 8;
          > The horizontal separation between icons
        property VertIconSeparation : integer default 4;
          > The vertical separation between icons and the control's ClientRect
        property IconSize     : TPoint;
          > The size used to paint the icons
        property SizeOption   : TSizeOptions;
          > soClientHeight  = IconSize := Point(ClientRect.Height, ClientRect.Height)
          > soSystemLarge   = IconSize := Point(GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON))
          > soSystemSmall   = IconSize := Point(GetSystemMetrics(SM_CXSICON), GetSystemMetrics(SM_CYSICON))
          > soImageListSize = IconSize := Point(fImageList.Width, fImageList.Height)
        property OnSelection  : TNotifyEvent;
          > Occurs when an item is selected
        property OnIconSizing : TNotifyEvent;
          > Occurs when IconSize value changes
    }

    TIconListViewer =
      class (TCustomControl)
        private
          LButtonDown : boolean;
          procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GetDlgCode;
          procedure WMHSCROLL(var Message : TWMScroll); message WM_HSCROLL;
          procedure WMSIZE(var Message); message WM_SIZE;
        protected
          procedure CreateParams(var Params: TCreateParams);                            override;
          procedure CreateWnd;                                                          override;
        protected
          procedure UpdateHeight;                                                       virtual;
          procedure UpdateScrollBar;                                                    virtual;
          procedure SetScrollBarPos(Position : integer);
          function  GetScrollBarPos : integer;
          function  ScrollBarHeight : integer;
        protected
          procedure Paint;                                                              override;
          procedure Loaded;                                                             override;
          procedure KeyDown(var Key: Word; Shift: TShiftState);                         override;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer);                       override;
        public
          constructor Create(aOwner : TComponent);                                      override;
          destructor  Destroy;                                                          override;
          property Align default alClient;
        private
          fColorSel     : TColor;
          fImageList    : TImageList;
          fIconSize     : TPoint;
          fXIconSeparation : integer;
          fYIconSeparation : integer;
          fItemIndex    : integer;
          f1stVisIcon   : integer;
          fSizeOption   : TSizeOptions;
          fOnIconSizing : TNotifyEvent;
          fOnSelection  : TNotifyEvent;
          procedure Set1stVisIcon(value : integer);
          procedure SetColorSel(value : TColor);
          procedure SetImageList(value : TImageList);
          procedure SetItemIndex(value : integer);
          procedure SetIconSeparation(index : integer; value : integer);
          procedure SetIconSize(value : TPoint);
          procedure SetSizeOption(value : TSizeOptions);
        protected
          function  IconAt(x, y : integer) : integer;
          procedure PaintIcon(const Index : integer; const Mode : TPaintMode);
          procedure DrawIcons;
          property FirstVisibleIcon : integer  read f1stVisIcon   write Set1stVisIcon;
          property IconSize     : TPoint       read fIconSize     write SetIconSize;
        public
          function  ItemPartialVisible(const Item : integer) : boolean;
          function  ItemCompleteVisible(const Item : integer) : boolean;
          function  VisiblesIcons : integer;
          procedure UpdateRect(Rect : TRect);
        public
          property ItemIndex    : integer      read fItemIndex    write SetItemIndex;
        published
          property ColorSelected : TColor      read fColorSel     write SetColorSel    default clActiveCaption;
          property ImageList    : TImageList   read fImageList    write SetImageList;
          property HorzIconSeparation : integer index 0 read fXIconSeparation write SetIconSeparation default 8; //Must be Even
          property VertIconSeparation : integer index 1 read fYIconSeparation write SetIconSeparation default 4; //Must be Even
          property SizeOption   : TSizeOptions read fSizeOption   write SetSizeOption default soSystemLarge;
          property OnSelection  : TNotifyEvent read fOnSelection  write fOnSelection;
          property OnIconSizing : TNotifyEvent read fOnIconSizing write fOnIconSizing;
        published
          property DragCursor;
          property DragMode;
          property Enabled;
          property Color           default clWindow;
          property Ctl3D;
          property Font;
          property ParentColor     default false;
          property ParentCtl3D;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
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

  procedure Register;

implementation

  {$BOOLEVAL OFF}

  uses
    CommCtrl;

  const
    SInvalidIconSize = 'Invalid icon sizew :(%d, %d)';

  { TIconListViewer }
  constructor TIconListViewer.Create(aOwner : TComponent);
    begin
      inherited;
      ControlStyle := ControlStyle - [csAcceptsControls] + [csCaptureMouse, csClickEvents];
      Color := clWindow;
      ParentColor := false;
      fImageList := nil;
      fColorSel := clActiveCaption;
      fItemIndex := -1;
      fSizeOption := soSystemLarge;
      fIconSize.X := GetSystemMetrics(SM_CXICON);
      fIconSize.Y := GetSystemMetrics(SM_CYICON);
      fXIconSeparation := 8;
      fYIconSeparation := 4;
      Width := fIconSize.X * 6;
    end;

  destructor TIconListViewer.Destroy;
    begin
      inherited;
    end;

  procedure TIconListViewer.Loaded;
    begin
      UpdateHeight;
    end;

  procedure TIconListViewer.CreateParams(var Params: TCreateParams);
    begin
      inherited;
      with Params do
        begin
          Style := Style or WS_HSCROLL;
          if NewStyleControls and Ctl3D
            then ExStyle := ExStyle or WS_EX_CLIENTEDGE ;
        end;
    end;

  procedure TIconListViewer.CreateWnd;
    begin
      inherited;
      UpdateScrollBar;
    end;

  procedure TIconListViewer.UpdateScrollBar;
    var
     ScrollInfo : TScrollInfo;
    begin
      with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL or SIF_DISABLENOSCROLL;
          nMin := 0;
          if fImageList <> nil
            then
              begin
                nMax  := fImageList.Count - VisiblesIcons;
                nPage := 1;
              end
            else
              begin
                nMax  := 0;
                nPage := 0;
              end;
          nPos := f1stVisIcon;
          nTrackPos := f1stVisIcon;
        end;
      Windows.SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    end;

  procedure TIconListViewer.UpdateHeight;
    begin
      if (not(csReading in ComponentState))
        then
          begin
            if(fSizeOption <> soClientHeight)
              then
                begin
                  if (ClientHeight <> (fIconSize.Y + VertIconSeparation))
                    then ClientHeight := fIconSize.Y + VertIconSeparation
                end
              else
                if (fIconSize.X <> (ClientHeight - VertIconSeparation)) or (fIconSize.Y <> (ClientHeight - VertIconSeparation))
                  then
                    begin
                      fIconSize := Point(ClientHeight - VertIconSeparation, ClientHeight - VertIconSeparation);
                      DrawIcons;
                    end;
          end;
    end;

  procedure TIconListViewer.WMSIZE(var Message);
    begin
      inherited;
      if (not(csReading in ComponentState))
        then
          begin
            UpdateHeight;
            UpdateScrollBar;
          end;
    end;

  procedure TIconListViewer.SetScrollBarPos(Position : integer);
    begin
      Windows.SetScrollPos(Handle, SB_HORZ, Position, True);
    end;

  function TIconListViewer.GetScrollBarPos : integer;
    begin
      result := Windows.GetScrollPos(Handle, SB_HORZ);
    end;

  function TIconListViewer.ScrollBarHeight : integer;
    begin
      result := Windows.GetSystemMetrics(SM_CYHSCROLL);
    end;

  procedure TIconListViewer.WMHSCROLL(var Message : TWMScroll);
    begin
      Message.Result := 1;
      with Message do
        case TScrollCode(ScrollCode) of
          scLineUp:
            FirstVisibleIcon :=  f1stVisIcon - 1;
          scLineDown:
            FirstVisibleIcon :=  f1stVisIcon + 1;
          scPageUp:
            FirstVisibleIcon :=  f1stVisIcon - VisiblesIcons;
          scPageDown:
            FirstVisibleIcon :=  f1stVisIcon + VisiblesIcons;
          scPosition, scTrack:
            FirstVisibleIcon :=  Pos;
          scTop:
            FirstVisibleIcon :=  0;
          scBottom:
            if fImageList <> nil
              then FirstVisibleIcon :=  GetScrollBarPos;
          else
            Result := 0;
        end;
    end;

  function TIconListViewer.IconAt(x, y : integer) : integer;
    begin
      result := x div (fIconSize.X+fXIconSeparation) + f1stVisIcon;
    end;

  procedure TIconListViewer.WMGetDlgCode(var Msg: TWMGetDlgCode);
    begin
      Msg.Result := DLGC_WANTARROWS;
    end;

  procedure TIconListViewer.KeyDown(var Key: Word; Shift: TShiftState);
    var
      LastItem : integer;
    begin
      case Key of
        VK_PRIOR :
          begin
            LastItem := ItemIndex;
            ItemIndex := -1;
            FirstVisibleIcon := FirstVisibleIcon + VisiblesIcons;
            ItemIndex := LastItem + VisiblesIcons;
          end;
        VK_NEXT  :
          if (ItemIndex - VisiblesIcons) >= 0
            then
              begin
                LastItem := ItemIndex;
                ItemIndex := -1;
                FirstVisibleIcon := FirstVisibleIcon - VisiblesIcons;
                ItemIndex := LastItem - VisiblesIcons
              end
            else ItemIndex := 0;
        VK_HOME  : ItemIndex := 0;
        VK_END   :
          if fImageList <> nil
            then
              begin
                ItemIndex := -1;
                FirstVisibleIcon := pred(fImageList.Count);
                ItemIndex := pred(fImageList.Count);
              end;
        VK_LEFT  :
          if ItemIndex > 0
            then ItemIndex := pred(ItemIndex);
        VK_RIGHT :
          begin
            if not ItemCompleteVisible(succ(ItemIndex))
              then FirstVisibleIcon := succ(FirstVisibleIcon);
            ItemIndex := succ(ItemIndex);
          end;
        else inherited;
      end;
    end;

  procedure TIconListViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if Button = mbLeft
        then
          begin
            ItemIndex := IconAt(X, Y);
            LButtonDown := true;
            SetFocus;
          end;
    end;

  procedure TIconListViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if LButtonDown
       then
         begin
           LButtonDown := false;
           if Assigned(fOnSelection)
              then fOnSelection(Self);
         end;
    end;

  procedure TIconListViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
    var
      Item : integer;
    begin
      inherited;
      if LButtonDown
        then
          begin
            Item := IconAt(X, Y);
            if Item < 0
              then Item := 0;
            if X > Width
              then FirstVisibleIcon := FirstVisibleIcon + 1
              else
                if (X < 0)
                  then FirstVisibleIcon := FirstVisibleIcon - 1;
            ItemIndex := Item;
          end
    end;

  function TIconListViewer.ItemPartialVisible(const Item : integer) : boolean;
    begin
      result := ((Item - f1stVisIcon) >=0) and ((Item - f1stVisIcon) <= VisiblesIcons)
    end;

  function TIconListViewer.ItemCompleteVisible(const Item : integer) : boolean;
    begin
      result := ((Item - f1stVisIcon) >=0) and ((Item - f1stVisIcon) < VisiblesIcons)
    end;

  function TIconListViewer.VisiblesIcons : integer;
    begin
      with ClientRect do
        result := (Right - Left) div (fIconSize.X + fXIconSeparation);
    end;

  procedure TIconListViewer.PaintIcon(const Index: integer; const Mode : TPaintMode);
    function IconPosition(const Index : integer) : integer;
      begin
        result := Index - f1stVisIcon;
      end;
    function IconRect(const Position : integer) : TRect;
      begin
        result := ClientRect;
        with result do
          begin
            Inc(Left, (Position*fIconSize.X) + (Position*fXIconSeparation));
            Right := Left + fIconSize.X + fXIconSeparation;
          end;
      end;
    var
      Icon : HICON;
      R    : TRect;
    begin
      if fImageList <> nil
        then
          begin
            if ItemPartialVisible(index)
              then
                with fIconSize do
                  begin
                    Icon :=  ImageList_GetIcon(fImageList.Handle, Index, ILD_TRANSPARENT);
                    R := IconRect(IconPosition(Index));
                    if Mode = pmSelected
                      then Canvas.Brush.Color := ColorSelected
                      else Canvas.Brush.Color := Color;
                    Canvas.FillRect(R);
                    with R do
                      begin
                        Inc(Left, fXIconSeparation div 2);
                        Inc(Top, fYIconSeparation div 2);
                        Dec(Right, fXIconSeparation div 2);
                        Dec(Bottom, fYIconSeparation div 2);
                        DrawIconEx(Canvas.Handle, Left, Top, Icon, X, Y, 0, 0, DI_NORMAL);
                      end;
                    DestroyIcon(Icon);
                  end;
          end;
    end;

  procedure TIconListViewer.DrawIcons;
    var
      IconIndex : integer;
      IconPos   : integer;
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
      if (fImageList <> nil) and (fImageList.Count <> 0)
        then
          begin
            IconIndex := FirstVisibleIcon;
            IconPos  := 0;
            while (IconPos <= VisiblesIcons) and (IconIndex < fImageList.Count) do
              begin
                if ItemIndex = IconIndex
                  then PaintIcon(IconIndex, pmSelected)
                  else PaintIcon(IconIndex, pmNormal);
                Inc(IconPos);
                Inc(IconIndex);
              end;
          end;
    end;

  procedure TIconListViewer.Paint;
    begin
      DrawIcons;
    end;

  procedure TIconListViewer.SetColorSel(value : TColor);
    begin
      fColorSel := value;
      PaintIcon(fItemIndex, pmSelected);
    end;

  procedure TIconListViewer.SetItemIndex(value : integer);
    begin
      if (fImageList <> nil) and (fImageList.Count > 0)
        then
          begin
            if value >= fImageList.Count
              then value := pred(fImageList.Count)
              else if value < 0
                then value := -1;
            if (value <> fItemIndex) and (fImagelist <> nil)
              then
                begin
                  PaintIcon(fItemIndex, pmNormal);
                  fItemIndex := value;
                  if fItemIndex <> -1
                    then
                      if ItemPartialVisible(fItemIndex)
                        then PaintIcon(fItemIndex, pmSelected)
                        else FirstVisibleIcon := ItemIndex;
                  if Assigned(fOnSelection)
                    then fOnSelection(Self);
                end;
          end;
    end;

  procedure TIconListViewer.SetImageList(value : TImageList);
    begin
      f1stVisIcon := 0;
      fItemIndex := -1;
      fImageList := value;
      if (fSizeOption = soImageListSize) and (fImageList <> nil)
        then IconSize := Point(fImageList.Width, fImageList.Height)
        else
          begin
            UpdateHeight;
            UpdateScrollBar;
            Invalidate;
          end;
    end;

  procedure TIconListViewer.SetIconSeparation(index : integer; value : integer);
    begin
      if value < 0
        then value := 0;
      if (value mod 2) <> 0
        then inc(value);
      case index of
        0 : fXIconSeparation := value;
        1 :
           begin
             fYIconSeparation := value;
             UpdateHeight;
             if assigned(OnIconSizing)
               then OnIconSizing(Self);
           end;
      end;
      UpdateScrollBar;
      Invalidate;
    end;

  procedure TIconListViewer.UpdateRect(Rect : TRect);
    function IconRect(const Position : integer) : TRect;
      begin
        result := ClientRect;
        with result do
          begin
            Inc(Left, (Position*fIconSize.X) + (Position*fXIconSeparation));
            Right := Left + fIconSize.X + fXIconSeparation;
          end;
      end;
    var
      R : TRect;
      i : integer;
    begin
      for i := 0 to VisiblesIcons do
        if IntersectRect(R, Rect, IconRect(i))
          then
            if fItemIndex = (i + f1stVisIcon)
              then PaintIcon(i + f1stVisIcon, pmSelected)
              else PaintIcon(i + f1stVisIcon, pmNormal);
    end;

  procedure TIconListViewer.Set1stVisIcon(value : integer);
    procedure ScrollX(dx : integer);
      var
        RS, RD, RU : TRect;
      begin

        RS := ClientRect;
        RD := ClientRect;
        RU := ClientRect;

        if abs(dx) < (ClientRect.Right - ClientRect.Left)
          then
            begin
              if dx > 0
                then
                  begin
                    dec(RS.Right, dx);
                    inc(RD.Left, dx);
                    RU.Right := RU.Left + dx;
                  end
                else
                  begin
                    dec(RS.Left, dx);
                    inc(RD.Right, dx);
                    RU.Left := RU.Right + dx;
                  end;

              Canvas.CopyRect(RD, Canvas, RS);

              UpdateRect(RU);
            end
          else
            DrawIcons;
      end;
    var
      dIcons : integer;
    begin
      if fImageList <> nil
        then
          begin
            if value > fImageList.Count - VisiblesIcons
              then value := fImageList.Count - VisiblesIcons;
            if value < 0
              then value := 0;
            dIcons := f1stVisIcon - value;
            if dIcons <> 0
              then
                begin
                  f1stVisIcon := value;
                  SetScrollBarPos(value);
                  ScrollX(dIcons*(fIconSize.X + fXIconSeparation));
                end;
          end
          else f1stVisIcon := 0;
    end;

  procedure TIconListViewer.SetIconSize(value : TPoint);
    begin
      if (value.X < 1) or (value.Y < 1)
        then
          if csDesigning in ComponentState
            then
              begin
                ShowMessage(Format(SInvalidIconSize, [value.X, value.Y]));
              end
            else EIconListViewerError.CreateFmt(SInvalidIconSize, [value.X, value.Y])
        else
          begin
            fIconSize := value;
            if assigned(OnIconSizing)
              then OnIconSizing(Self);
            UpdateHeight;
            UpdateScrollBar;
            Invalidate;
          end;
    end;

  procedure TIconListViewer.SetSizeOption(value : TSizeOptions);
    begin
      if fSizeOption <> value
        then
          begin
            fSizeOption := value;
            case fSizeOption of
              soClientHeight  : IconSize := Point(ClientHeight - VertIconSeparation, ClientHeight - VertIconSeparation);
              soSystemLarge   : IconSize := Point(GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON));
              soSystemSmall   : IconSize := Point(GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
              soImageListSize : if fImageList <> nil then IconSize := Point(fImageList.Width, fImageList.Height);
            end;
          end;
    end;

  procedure Register;
    begin
      RegisterComponents('WISE', [TIconListViewer]);
    end;

end.
