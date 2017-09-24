unit TabbedPages;

interface

uses Windows, Classes, Graphics, Forms, Controls, Messages, Buttons;

type
  TScrollBtn = (sbLeft, sbRight);

type
  TScroller =
    class(TCustomControl)
      private
        { property usage }
        FMin: longint;
        FMax: longint;
        FPosition: longint;
        FOnClick: TNotifyEvent;
        FChange: integer;

        { private usage }
        sbLeft: TSpeedButton;
        sbRight: TSpeedButton;

        { property access methods }
        procedure SetMin(Value: longint);
        procedure SetMax(Value: longint);
        procedure SetPosition(Value: longint);

        { private methods }
        procedure sbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        function CreateSpeedButton: TSpeedButton;
        function CanScrollLeft: boolean;
        function CanScrollRight: boolean;
        procedure WMSize(var Message: TWMSize); message WM_SIZE;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
      published
        property OnClick: TNotifyEvent read FOnClick write FOnClick;
        property Min: longint read FMin write SetMin default 0;
        property Max: longint read FMax write SetMax default 0;
        property Position: longint read FPosition write SetPosition default 0;
        property Change: integer read FChange write FChange default 1;
    end;

  TTabbedPages = class;
  TMerchPage = class;
  TTab =
    class(TCustomControl)
      private
        FText: string;
        FCaption: string;
        FTabWidth: integer;
        FTabLeft: integer;
        TabbedPages: TTabbedPages;
        Page: TMerchPage;
        FSelected: boolean;
        FWMin: boolean;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure SetSelected(Value: boolean);
        procedure SetText(Value: string);
        procedure SetTabWidth(const Value: integer);
        procedure SetTabLeft(const Value: integer);
        procedure SetCaption(const Value: string);
        procedure SetTabBounds;
        function IdealWidth: integer;
        function MinWidth: integer;
      protected
        procedure Paint; override;
        constructor Create(AOwner: TComponent); override;
      published
        property TabWidth: integer read FTabWidth write SetTabWidth;
        property TabLeft: integer read FTabLeft write SetTabLeft;
        property Selected: boolean read FSelected write SetSelected;
        property Text: string read FText write SetText;
        property Caption: string read FCaption write SetCaption;
    end;

  TMerchPage =
    class(TCustomControl)
      private
        FGlyph: TPicture;
        procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
        procedure CreateTab;
        procedure SetGlyph(Value: TPicture);
      protected
        procedure ReadState(Reader: TReader); override;
      public
        constructor Create(AOwner: TComponent); override;
      published
        Tab: TTab;
        property Caption;
        property Height stored False;
        property TabOrder stored False;
        property Visible stored False;
        property Width stored False;
        property Glyph: TPicture read FGlyph write SetGlyph;
      end;

  TGlyphs = class;
  TTabChangingEvent = procedure(Sender: TObject;
    var AllowChange: boolean) of object;
  TKind = (kdDown, kdUp);

  TTabbedPages =
    class(TCustomControl)
      private
        FPageList: TList;
        FAccess: TStrings;
        FGlyphs: TGlyphs;
        FPageIndex: integer;
        FOnChange: TNotifyEvent;
        FOnChanging: TTabChangingEvent;
        FOnPageChanged: TNotifyEvent;
        FKind: TKind;
        FMargin: integer;
        FVisibleTabs: integer;
        FTabHeight: integer;
        FFirstIndex: integer;
        FAutoScroll: boolean;
        Scroller: TScroller;
        FDoFix: boolean;
        FGlyphW: integer;
        FGlyphH: integer;
        FInserting: boolean;
        procedure SetKind(Value: TKind);
        procedure SetPages(Value: TStrings);
        procedure SetActivePage(const Value: string);
        procedure UpdateActivePage;
        function GetActivePage: string;
        procedure SetPageIndex(Value: integer);
        procedure SetGlyphList(GlyphList: TGlyphs);
        procedure PutGlyph(index: integer; Glyph: TPicture);
        procedure SetFirstIndex(Value: integer);
        function GetGlyph(index: integer): TPicture;
        procedure SetMargin(const Value: integer);
        procedure SetAutoScroll(Value: boolean);
        procedure AdjustSize;
        procedure FixTabWidth;
        procedure SetTabPositions;
        function CalcNumTabs(Start, Stop: integer; First: integer): integer;
        procedure WMSize(var Message: TWMSize);  message WM_SIZE;
        procedure FontChange(Sender: TObject);
        procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
        procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
        procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
        procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
        procedure CreateScroller;
        procedure ScrollClick(Sender: TObject);
        property GlyphW: integer read FGlyphW;
        property GlyphH: integer read FGlyphH;
      protected
        procedure CreateParams(var Params: TCreateParams); override;
        function GetChildOwner: TComponent; override;
        procedure GetChildren(Proc: TGetChildProc); override;
        procedure ReadState(Reader: TReader); override;
        procedure ShowControl(AControl: TControl); override;
        function CanChange: boolean; dynamic;
        procedure Change; dynamic;
        procedure Paint; override;
      public
        procedure SelectNextPage(GoForward: boolean);
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        property Glyph[index: integer]: TPicture read GetGlyph write PutGlyph stored False;
        property FirstIndex: integer read FFirstIndex write SetFirstIndex default 0;
        property AutoScroll: boolean read FAutoScroll write SetAutoScroll default True;
      published
        property ActivePage: string read GetActivePage write SetActivePage stored False;
        property Align;
        property DragCursor;
        property DragMode;
        property Enabled;
        property Font;
        property Glyphs: TGlyphs read FGlyphs write SetGlyphList stored False;
        property Margin: integer read FMargin write SetMargin default 4;
        property PageIndex: integer read FPageIndex write SetPageIndex default 0;
        property Pages: TStrings read FAccess write SetPages stored False;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property Kind: TKind read FKind write SetKind default kdDown;
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
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
        property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
        property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
        property OnStartDrag;
      end;

  TGlyphs =
    class(TStrings)
      private
        PageList: TList;
      protected
        function GetCount: integer; override;
        function Get(Index: integer): string; override;
        procedure Put(Index: integer; const S: string); override;
        function GetObject(Index: integer): TObject; override;
        procedure PutObject(Index: integer; AObject: TObject); override;
        procedure SetUpdateState(Updating: boolean); override;
      public
        constructor Create(APageList: TList);
        procedure Clear; override;
        procedure Delete(Index: integer); override;
        procedure Insert(Index: integer; const S: string); override;
        procedure Move(CurIndex, NewIndex: integer); override;
    end;

    procedure Register;

implementation

uses Consts, SysUtils, Dialogs;

const
  LeftMargin = 2;
  EndMargin = 2;
  EdgeWidth = 12;
  TabMargin = 4;
  ImplicitTabWidth = 50;
  ImplicitTabHeight = 21;

{ TGlyphs }

function TGlyphs.GetCount: integer;
  begin
    Result := PageList.Count;
  end;

function TGlyphs.Get(Index: integer): string;
  begin
    Result := TMerchPage(PageList[Index]).Caption;
  end;

procedure TGlyphs.Put(Index: integer; const S: string);
  begin
  end;

function TGlyphs.GetObject(Index: integer): TObject;
  begin
    Result := TMerchPage(PageList[Index]).Glyph;
  end;

procedure TGlyphs.PutObject(Index: integer; AObject: TObject);
  begin
    TMerchPage(PageList[Index]).Glyph := TPicture(AObject);
  end;

procedure TGlyphs.SetUpdateState(Updating: boolean);
  begin
  end;

constructor TGlyphs.Create(APageList: TList);
  begin
    inherited Create;
    PageList := APageList;
  end;

procedure TGlyphs.Clear;
  var
    I: integer;
  begin
    for I := 0 to PageList.Count - 1 do
      TPicture(TMerchPage(PageList[I]).Glyph).Free;
  end;

procedure TGlyphs.Delete(Index: integer);
  begin
    TPicture(TMerchPage(PageList[Index]).Glyph).Free;
  end;

procedure TGlyphs.Insert(Index: integer; const S: string);
  begin
  end;

procedure TGlyphs.Move(CurIndex, NewIndex: integer);
  begin
  end;

{ TScroller }

constructor TScroller.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    ControlStyle := ControlStyle + [csOpaque];
    sbRight := CreateSpeedButton;
    sbLeft := CreateSpeedButton;
    sbLeft.Left := 0;
    sbRight.Left := (Width div 2) - 1;
    sbRight.Glyph.Handle := LoadBitMap(HInstance, 'RIGHTARROW');
    sbLeft.Glyph.Handle := LoadBitMap(HInstance, 'LEFTARROW');
    FMin := 0;
    FMax := 0;
    FPosition := 0;
    FChange := 1;
  end;

destructor TScroller.Destroy;
  begin
    sbLeft.Free;
    sbRight.Free;
    inherited Destroy;
  end;

function TScroller.CreateSpeedButton: TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    Result.Parent := Self;
    Result.OnMouseUp := sbMouseUp;
    Result.Top := 0;
    Result.Height := Height;
    Result.NumGlyphs := 1;
    Result.Width := (Width div 2) - 1;
  end;

procedure TScroller.sbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  var
    NewPos: longint;
  begin
    NewPos := Position;
    if Sender = sbLeft
      then Dec(NewPos, Change)
      else Inc(NewPos, Change);
    Position := NewPos;
  end;

procedure TScroller.WMSize(var Message: TWMSize);
  begin
    inherited;
    sbRight.Left := (Width div 2) - 1;
    sbRight.Height := Height;
    sbRight.Width := (Width div 2) - 1;
    sbLeft.Height := Height;
    sbLeft.Width := (Width div 2) - 1;
  end;

procedure TScroller.SetMin(Value: longint);
  begin
    if Value < FMax
      then FMin := Value;
  end;

procedure TScroller.SetMax(Value: longint);
  begin
    if Value > FMin
      then FMax := Value;
  end;

procedure TScroller.SetPosition(Value: longint);
  begin
    if Value <> FPosition
      then
        begin
          if Value < Min
            then Value := Min;
          if Value > Max
            then Value := Max;
          FPosition := Value;
          Invalidate;
          if Assigned(FOnClick)
            then FOnClick(Self);
        end;
  end;

function TScroller.CanScrollLeft: boolean;
  begin
    Result := Position > Min;
  end;

function TScroller.CanScrollRight: boolean;
  begin
    Result := Position < Max;
  end;

{ TTab }

constructor TTab.Create(AOwner: TComponent);
  begin
    inherited;
    ControlStyle := ControlStyle + [csOpaque];
    ShowHint := true;
    visible := false;
  end;

procedure TTab.SetText(Value: string);
  begin
    if Value <> FText
      then
        begin
          FText := Value;
          Caption := Value;
          Hint := Value;
          Invalidate;
        end;
  end;

procedure TTab.SetTabLeft(const Value: integer);
  begin
    if Value <> FTabLeft
      then
        begin
          FTabLeft := Value;
          SetTabBounds;
        end;
  end;

procedure TTab.SetTabBounds;
  begin
    if Selected = true
      then
        begin
          BringToFront;
          Height := TabbedPages.FTabHeight;
          Left := TabLeft - 2;
          Width := TabWidth + 4;
          if TabbedPages.Kind = kdUp
            then Top := 0;
        end
      else
        begin
          Height := TabbedPages.FTabHeight - 2;
          Left := TabLeft;
          Width := TabWidth;
          if TabbedPages.Kind = kdUp
            then Top := 2;
        end;
    Invalidate;
  end;

procedure TTab.SetSelected(Value: boolean);
  begin
    if Value <> FSelected
      then
        begin
          FSelected := Value;
          SetTabBounds;
          Invalidate;
        end;
  end;

procedure TTab.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    Index: integer;
  begin
    inherited;
    Index := TabbedPages.Pages.IndexOfObject(Page);
    TabbedPages.PageIndex := Index;
  end;

procedure TTab.SetCaption(const Value: string);
  var
    W, I: integer;
    S: string;
  begin
    with Canvas do
      begin
        if Page.Glyph.Graphic <> nil
          then
            with TabbedPages do
              W := FGlyphW + Margin
          else W := 0;
        inc(W, TabbedPages.Margin);
        inc(W, TabbedPages.Margin);
        if W + TextWidth(Value) < TabWidth
          then FCaption := Value
          else
            begin
              I := 0;
              S := '';
              inc(W, TextWidth('O...'));
              while (W < TabWidth) and (I <= length(Value)) do
                begin
                  inc(I);
                  inc(W,TextWidth(Value[I]));
                  S := S + Value[I];
                end;
              if S <> ''
                then S := S + '...';
              FCaption := S;
            end;
      end;
  end;

procedure TTab.SetTabWidth(const Value: integer);
  begin
    if Value <> FTabWidth
      then
        begin
          if Value > MinWidth
            then FTabWidth := Value
            else FTabWidth := MinWidth;
          SetTabBounds;
          SetCaption(Text);
          invalidate;
        end;
  end;

function TTab.IdealWidth: integer;
  begin
    Result := Canvas.TextWidth(Text);
    if Page.Glyph.Graphic <> nil
      then inc(Result, TabbedPages.FGlyphW + TabbedPages.Margin);
    {
    if Result < ImplicitTabWidth
      then Result := ImplicitTabWidth;
    }
    inc(Result, EdgeWidth);
  end;

function TTab.MinWidth: integer;
  begin
    if Page.Glyph.Graphic <> nil
      then Result := TabbedPages.FGlyphW 
      else Result := Canvas.TextWidth('O...');
    inc(Result, EdgeWidth);
  end;

procedure TTab.Paint;
  var
    R: TRect;
  begin
    R := GetClientRect;
    with Canvas do
      begin
        Brush.Color := clBtnFace;
        FillRect(Rect(0, 0, Width, Height));
        Brush.Color := clBtnFace;
        CopyMode := cmSrcCopy;
        if Page.Glyph.Graphic <> nil
          then
            begin
              StretchDraw(Rect(R.Left + TabbedPages.Margin, R.Top + 2, R.Left + TabbedPages.FGlyphW + TabbedPages.Margin,
                          R.Top + 2 + TabbedPages.FGlyphH), Page.Glyph.Graphic);  
              Inc(R.Left, TabbedPages.FGlyphW + TabbedPages.Margin);
            end;
        DrawText(Handle, PChar(Caption),
                 Length(Caption), R, DT_VCENTER + DT_CENTER + DT_SINGLELINE);
        if TabbedPages.Kind = kdDown
          then
            if Selected
              then
                begin
                  Pen.Color := clWindowFrame;
                  PolyLine([Point(2, Height - 2),
                            Point(Width - 3, Height - 2),
                            Point(Width - 1, Height - 4),
                            Point(Width - 1, 0)]);

                  Pen.Color := clBtnShadow;
                  PolyLine([Point(1, Height - 4),
                            Point(1, Height - 3),
                            Point(Width - 3, Height - 3),
                            Point(Width - 3, Height - 4),
                            Point(Width - 2, Height - 4),
                            Point(Width - 2, -1)]);
                  MoveTo(Width -2, 0);
                  LineTo(Width, 0);

                  Pen.Color := clBtnHighlight;
                  MoveTo(0, Height - 4);
                  LineTo(0, -1);
                end
              else
                begin
                  Pen.Color := clWindowFrame;
                  PolyLine([Point(2, Height - 2),
                            Point(Width - 3, Height - 2),
                            Point(Width - 1, Height - 4),
                            Point(Width - 1, 1),
                            Point(-1, 1)]);

                  Pen.Color := clBtnShadow;
                  PolyLine([Point(1, Height - 4),
                            Point(1, Height - 3),
                            Point(Width - 3, Height - 3),
                            Point(Width - 3, Height - 4),
                            Point(Width - 2, Height - 4),
                            Point(Width - 2, 1)]);
                  MoveTo(Width - 1, 0);
                  LineTo(-1, 0);

                  Pen.Color := clBtnHighlight;
                  MoveTo(0, Height - 4);
                  LineTo(0, 1);
                end
          else
            if Selected
              then
                begin
                  Pen.Color := clBtnHighlight;
                  PolyLine([Point(0, Height - 1),
                            Point(0, 4),
                            Point(2, 2),
                            Point(Width - 2, 2)]);

                  Pen.Color := clBtnShadow;
                  MoveTo(Width - 2, 4);
                  LineTo(Width - 2, Height);

                  Pen.Color := clWindowFrame;
                  PolyLine([Point(Width - 2, 3),
                            Point(Width - 1, 4),
                            Point(Width - 1, Height)]);
                end
              else
                begin
                  Pen.Color := clBtnHighlight;
                  PolyLine([Point(Width - 1,Height - 1),
                            Point(0, Height - 1),
                            Point(0, 4),
                            Point(2, 2),
                            Point(Width - 2, 2)]);

                  Pen.Color := clBtnShadow;
                  MoveTo(Width - 2, 4);
                  LineTo(Width - 2, Height - 1);

                  Pen.Color := clWindowFrame;
                  PolyLine([Point(Width - 2, 3),
                            Point(Width - 1, 4),
                            Point(Width - 1, Height - 1)]);
                end;
      end;
  end;

{ TPageAccess }

type
  TPageAccess =
    class(TStrings)
      private
        PageList: TList;
        Notebook: TTabbedPages;
      protected
        function GetCount: integer; override;
        function Get(Index: integer): string; override;
        procedure Put(Index: integer; const S: string); override;
        function GetObject(Index: integer): TObject; override;
        procedure SetUpdateState(Updating: boolean); override;
      public
        constructor Create(APageList: TList; ANotebook: TTabbedPages);
        procedure Clear; override;
        procedure Delete(Index: integer); override;
        procedure Insert(Index: integer; const S: string); override;
        procedure Move(CurIndex, NewIndex: integer); override;
    end;

constructor TPageAccess.Create(APageList: TList; ANotebook: TTabbedPages);
  begin
    inherited Create;
    PageList := APageList;
    Notebook := ANotebook;
  end;

function TPageAccess.GetCount: integer;
  begin
    Result := PageList.Count;
  end;

function TPageAccess.Get(Index: integer): string;
  begin
    Result := TMerchPage(PageList[Index]).Caption;
  end;

procedure TPageAccess.Put(Index: integer; const S: string);
  begin
    TMerchPage(PageList[Index]).Caption := S;
    TMerchPage(PageList[Index]).Tab.Text := S;
  end;

function TPageAccess.GetObject(Index: integer): TObject;
  begin
    Result := TMerchPage(PageList[Index]);
  end;

procedure TPageAccess.SetUpdateState(Updating: boolean);
  begin
    { do nothing }
  end;

procedure TPageAccess.Clear;
  var
    I: integer;
  begin
    for I := 0 to PageList.Count - 1 do
      TMerchPage(PageList[I]).Free;
    PageList.Clear;
  end;

procedure TPageAccess.Delete(Index: integer);
  var
    Form: TForm;
  begin
    TMerchPage(PageList[Index]).Free;
    PageList.Delete(Index);
    NoteBook.PageIndex := 0;
    NoteBook.FixTabWidth;
    if csDesigning in NoteBook.ComponentState
      then
        begin
          Form := GetParentForm(NoteBook);
          if (Form <> nil) and (Form.Designer <> nil)
            then Form.Designer.Modified;
        end;
  end;

procedure TPageAccess.Insert(Index: integer; const S: string);
  var
    Page: TMerchPage;
    Form: TForm;
  begin
    Page := TMerchPage.Create(Notebook);
    with Page do
      begin
        Parent := Notebook;
        Caption := S;
      end;
    PageList.Insert(Index, Page);
    Page.CreateTab;
    with NoteBook do
      begin
        PageIndex := Index;
        FInserting := True;
        Click;
        Invalidate;
      end;
    if csDesigning in NoteBook.ComponentState
      then
        begin
          Form := GetParentForm(NoteBook);
          if (Form <> nil) and (Form.Designer <> nil)
            then Form.Designer.Modified;
        end;
    NoteBook.FixTabWidth;
  end;

procedure TPageAccess.Move(CurIndex, NewIndex: integer);
  var
    AObject: TObject;
  begin
    if CurIndex <> NewIndex
      then
        begin
          AObject := PageList[CurIndex];
          PageList[CurIndex] := PageList[NewIndex];
          PageList[NewIndex] := AObject;
        end;
  end;

{ TMerchPage }

constructor TMerchPage.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    Visible := False;
    ControlStyle := ControlStyle + [csAcceptsControls];
    FGlyph := TPicture.Create;
    Tab := TTab.Create(Self);
  end;

procedure TMerchPage.ReadState(Reader: TReader);
  begin
    if Reader.Parent is TTabbedPages
      then TTabbedPages(Reader.Parent).FPageList.Add(Self);
    inherited;
    CreateTab;
  end;

procedure TMerchPage.CreateTab;
  begin
    with Tab do
      begin
        TabbedPages := TTabbedPages(Self.Owner);
        Parent := TabbedPages;
        Page := Self;
        FTabWidth := ImplicitTabWidth;
        Height := TabbedPages.FTabHeight;
        Text := Self.Caption;
      end;
    with TTabbedPages(Owner) do
      if Kind = kdDown
        then Tab.Top := Height - FTabHeight;
  end;

procedure TMerchPage.SetGlyph(Value: TPicture);
  begin
    FGlyph.Assign(Value);
    TTabbedPages(Owner).FixTabWidth;
  end;

procedure TMerchPage.WMNCHitTest(var Message: TWMNCHitTest);
  begin
    if not (csDesigning in ComponentState)
      then Message.Result := HTTRANSPARENT
      else inherited;
  end;

{ TTabbedPages }

var
  Registered: boolean = False;

constructor TTabbedPages.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    Width := 150;
    Height := 100;
    ControlStyle := [csDoubleClicks];
    FTabHeight := ImplicitTabHeight;
    ParentFont := False;
    Font.Name := 'MS Sans Serif';
    Font.Size := 8;
    Font.Style := [];
    FKind := kdDown;
    Font.OnChange := FontChange;
    FPageList := TList.Create;
    FAccess := TPageAccess.Create(FPageList, Self);
    FGlyphs := TGlyphs.Create(FPageList);
    FPageIndex := 0;
    FFirstIndex := 0;
    FVisibleTabs := 0;
    FGlyphW := GetSystemMetrics(SM_CXSMICON);
    FGlyphH := GetSystemMetrics(SM_CYSMICON);
    FAutoScroll := True;
    FMargin := 4;
    ShowHint := true;
    CreateScroller;
    Exclude(FComponentStyle, csInheritable);
    if not Registered
      then
        begin
          Classes.RegisterClasses([TMerchPage, TTab]);
          Registered := True;
        end;
  end;

destructor TTabbedPages.Destroy;
  begin
    FAccess.Free;
    FPageList.Free;
    inherited Destroy;
  end;

procedure TTabbedPages.CreateParams(var Params: TCreateParams);
  begin
    inherited CreateParams(Params);
    with Params do
      Style := Style or WS_CLIPCHILDREN;
  end;

function TTabbedPages.GetChildOwner: TComponent;
  begin
    Result := Self;
  end;

procedure TTabbedPages.GetChildren(Proc: TGetChildProc);
  var
    I: integer;
  begin
    for I := 0 to FPageList.Count - 1 do
      Proc(TControl(FPageList[I]));
  end;

procedure TTabbedPages.SetKind(Value: TKind);
  begin
    if Value <> FKind
      then
        begin
          FKind := Value;
          AdjustSize;
          Invalidate;
        end;
  end;

procedure TTabbedPages.SetGlyphList(GlyphList: TGlyphs);
  begin
    FGlyphs.Assign(GlyphList);
    Invalidate;
  end;

procedure TTabbedPages.PutGlyph(index: integer; Glyph: TPicture);
  begin
    FGlyphs.Objects[index] := Glyph;
    FixTabWidth;
    Invalidate;
  end;

function TTabbedPages.GetGlyph(index: integer): TPicture;
  begin
    Result := FGlyphs.Objects[index] as TPicture;
  end;

procedure TTabbedPages.ReadState(Reader: TReader);
  begin
    Pages.Clear;
    inherited ReadState(Reader);
    if (FPageIndex <> -1) and (FPageIndex >= 0) and (FPageIndex < FPageList.Count)
      then
        with TMerchPage(FPageList[FPageIndex]) do
          begin
            BringToFront;
            Visible := True;
            Tab.Selected := True;
          end
      else FPageIndex := -1;
    AdjustSize;
    FixTabWidth;
  end;

procedure TTabbedPages.ShowControl(AControl: TControl);
  var
    I: integer;
  begin
    for I := 0 to FPageList.Count - 1 do
      if FPageList[I] = AControl
        then
          begin
            SetPageIndex(I);
            Exit;
          end;
    inherited ShowControl(AControl);
  end;

procedure TTabbedPages.SetPages(Value: TStrings);
  begin
    FAccess.Assign(Value);
    Invalidate;
  end;

procedure TTabbedPages.SetPageIndex(Value: integer);
  var
    ParentForm: TForm;
  begin
    if csLoading in ComponentState
      then FPageIndex := Value
      else
        if (Value >= 0) and (Value < FPageList.Count)
          then
            if Value = FPageIndex
              then TMerchPage(FPageList[Value]).Tab.Selected := true
              else
                begin
                  ParentForm := GetParentForm(Self);
                  if ParentForm <> nil
                    then
                      if ContainsControl(ParentForm.ActiveControl)
                        then ParentForm.ActiveControl := Self;
                  with TMerchPage(FPageList[Value]) do
                    begin
                      BringToFront;
                      Visible := True;
                      Tab.Selected := true;
                    end;
                  if (FPageIndex >= 0) and (FPageIndex < FPageList.Count)
                    then
                      begin
                        TMerchPage(FPageList[FPageIndex]).Visible := False;
                        TMerchPage(FPageList[FPageIndex]).Tab.Selected := False;
                      end;
                  FPageIndex := Value;
                  if ParentForm <> nil
                    then
                      if ParentForm.ActiveControl = Self
                        then SelectFirst;
                  if Assigned(FOnPageChanged)
                    then FOnPageChanged(Self);
                end;
  end;

procedure TTabbedPages.AdjustSize;
  var
    I: integer;
  begin
    if Kind = kdDown
      then
        for I := 0 to FPageList.Count - 1 do
          begin
            TMerchPage(FPageList[I]).SetBounds(4, 4, Width - 8, Height - FTabHeight - 6);
            TMerchPage(FPageList[I]).Tab.Top := Height - FTabHeight;
            TMerchPage(FPageList[I]).Tab.SetTabBounds;
          end
      else
        for I := 0 to FPageList.Count - 1 do
          begin
            TMerchPage(FPageList[I]).SetBounds(4, FTabHeight + 3, Width - 8, Height - FTabHeight - 7);
            TMerchPage(FPageList[I]).Tab.Top := 0;
            TMerchPage(FPageList[I]).Tab.SetTabBounds;
          end;
  end;

procedure TTabbedPages.WMSize(var Message: TWMSize);
  begin
    inherited;
    FixTabWidth;
    AdjustSize;
    Invalidate;
    Message.Result := 0;
  end;

procedure TTabbedPages.SetActivePage(const Value: string);
  begin
    SetPageIndex(FAccess.IndexOf(Value));
    Invalidate;
  end;

function TTabbedPages.GetActivePage: string;
  begin
    Result := FAccess[FPageIndex];
  end;

procedure TTabbedPages.FontChange(Sender: TObject);
  begin
    AdjustSize;
    FixTabWidth;
    Refresh;
  end;

procedure TTabbedPages.CMDialogKey(var Message: TCMDialogKey);
  begin
    if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0)
      then
        begin
          SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
          Message.Result := 1;
        end
      else
        inherited;
  end;

procedure TTabbedPages.SelectNextPage(GoForward: boolean);
  var
    NewIndex: integer;
  begin
    if Pages.Count > 1
      then
        begin
          NewIndex := PageIndex;
          if GoForward
            then Inc(NewIndex)
            else Dec(NewIndex);
          if NewIndex = Pages.Count
            then NewIndex := 0
            else
              if NewIndex < 0
                then NewIndex := Pages.Count - 1;
          SetPageIndex(NewIndex);
        end;
  end;

procedure TTabbedPages.Change;
  var
    Form: TForm;
  begin
    UpdateActivePage;
    if csDesigning in ComponentState
      then
        begin
          Form := GetParentForm(Self);
          if (Form <> nil) and (Form.Designer <> nil)
            then Form.Designer.Modified;
        end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;

procedure TTabbedPages.UpdateActivePage;
  begin
    if PageIndex >= 0
      then SetActivePage(FAccess[PageIndex]);
  end;

function TTabbedPages.CanChange: boolean;
  begin
    Result := True;
    if Assigned(FOnChanging)
      then FOnChanging(Self, Result);
  end;

procedure TTabbedPages.SetMargin(const Value: integer);
  begin
    if Value <> FMargin
      then
        begin
          FMargin := Value;
          Invalidate;
        end;
  end;

procedure TTabbedPages.WMDestroy(var Message: TWMDestroy);
  var
    FocusHandle: HWnd;
  begin
    FocusHandle := GetFocus;
    if (FocusHandle <> 0) and ((FocusHandle = Handle) or IsChild(Handle, FocusHandle))
      then Windows.SetFocus(0);
    inherited;
  end;

procedure TTabbedPages.CMTabStopChanged(var Message: TMessage);
  begin
    if not (csDesigning in ComponentState)
      then RecreateWnd;
  end;

procedure TTabbedPages.FixTabWidth;
  var
    i, AskedWidth, MinWidth: integer;
    TabSetWidth, Slice: integer;
  begin
    TabSetWidth := Width - LeftMargin;
    AskedWidth := 0;
    for i := 0 to pred(Pages.Count) do
      AskedWidth := AskedWidth + TMerchPage(Pages.Objects[i]).Tab.IdealWidth;
    if AskedWidth > TabSetWidth
      then
        begin
          MinWidth := 0;
          for i := 0 to pred(Pages.Count) do
            MinWidth := MinWidth + TMerchPage(Pages.Objects[i]).Tab.MinWidth;
          if MinWidth > TabSetWidth
            then // Scroller
              begin
                {
                Scroller.Visible := True;
                ShowWindow(Scroller.Handle, SW_SHOW);
                Scroller.Min := 0;
                Scroller.Max := Pages.Count - FVisibleTabs;
                Scroller.Position := FirstIndex;
                }
                {
                Beep;
                MessageDlg('Ya es hora de sacar el Scroller', mtInformation, [mbOk], 0);
                }
                Slice := (TabSetWidth - MinWidth) div Pages.Count - 1;
                for i := 0 to pred(Pages.Count) do
                  with TMerchPage(Pages.Objects[i]).Tab do
                    TabWidth := MinWidth + Slice;
              end
            else
              begin
                Slice := (TabSetWidth - MinWidth) div Pages.Count - 1;
                for i := 0 to pred(Pages.Count) do
                  with TMerchPage(Pages.Objects[i]).Tab do
                    TabWidth := MinWidth + Slice;
              end;
        end
      else
        for i := 0 to pred(Pages.Count) do
          with TMerchPage(Pages.Objects[i]).Tab do
            TabWidth := IdealWidth;
  end;

procedure TTabbedPages.SetFirstIndex(Value: integer);
  begin
    if (Value >= 0) and (Value < Pages.Count)
      then
        begin
          FFirstIndex := Value;
          Invalidate;
        end;
  end;

procedure TTabbedPages.SetTabPositions;
  var
    I, TabPos: integer;
  begin
    TabPos := LeftMargin;
    for I := 0 to Pred(Pages.Count) do
      if (I >= FirstIndex) and (I < FirstIndex + FVisibleTabs)
        then
          begin
            TMerchPage(Pages.Objects[I]).Tab.TabLeft := TabPos;
            inc(TabPos, TMerchPage(Pages.Objects[I]).Tab.TabWidth);
            TMerchPage(Pages.Objects[I]).Tab.visible := True;
          end
        else
          TMerchPage(Pages.Objects[I]).Tab.visible := False;
  end;

function TTabbedPages.CalcNumTabs(Start, Stop: integer; First: integer): integer;
  begin
    Result := First;
    while (Start < Stop) and (Result < Pages.Count) do
      begin
        Inc(Start, TMerchPage(Pages.Objects[Result]).Tab.TabWidth);
        if Start <= Stop
          then Inc(Result);
      end;
    Result := Result - First;
  end;

procedure TTabbedPages.Paint;
  var
    TabStart, LastTabPos: integer;
  begin
    TabStart := LeftMargin;
    LastTabPos := Width - EndMargin;
    Scroller.Left := Width - Scroller.Width - 2;
    FVisibleTabs := CalcNumTabs(TabStart, LastTabPos, FirstIndex);
    if AutoScroll and (FVisibleTabs < Pages.Count)
      then
        begin
          Dec(LastTabPos, Scroller.Width + 4);
          FVisibleTabs := CalcNumTabs(TabStart, LastTabPos, FirstIndex);
          if Kind = kdUp
            then Scroller.Top := 3
            else Scroller.Top := Height - FTabHeight + 3;
          if FInserting
            then FirstIndex := Pages.Count - FVisibleTabs;
          Scroller.Visible := True;
          ShowWindow(Scroller.Handle, SW_SHOW);
          Scroller.Min := 0;
          Scroller.Max := Pages.Count - FVisibleTabs;
          Scroller.Position := FirstIndex;
        end
      else
        if FVisibleTabs >= Pages.Count
          then
            begin
              FFirstIndex := 0;
              Scroller.Visible := False;
              ShowWindow(Scroller.Handle, SW_HIDE);
            end;
    FInserting := False;
    SetTabPositions;
    with Canvas do
      begin
        Brush.Color := clBtnFace;
        FillRect(Rect(0, 0, Width, Height));
        if Kind = kdDown
          then
            begin
              Pen.Color := clBtnHighlight;           { Left side }
              MoveTo(0, Height - FTabHeight);
              LineTo(0, 0);
              LineTo(Width - 1, 0);                      { Top }

              Pen.Color := clBtnShadow;              { Right side }
              MoveTo(Width - 2, 1);
              LineTo(Width - 2, Height - FTabHeight);
              LineTo(0, Height - FTabHeight);

              Pen.Color := clWindowFrame;
              MoveTo(Width - 1, 0);
              LineTo(Width - 1, Height - FTabHeight + 1);
              LineTo(-1, Height - FTabHeight + 1);
            end
          else
            begin
              Pen.Color := clBtnHighlight;           { Left side }
              MoveTo(0, Height - 1);
              LineTo(0, FTabHeight - 1);
              LineTo(Width - 1, FTabHeight - 1);

              Pen.Color := clBtnShadow;              { Right side }
              MoveTo(Width - 2, FTabHeight);
              LineTo(Width - 2, Height);

              MoveTo(Width - 2, Height - 2);         { Bottom }
              LineTo(0, Height - 2);

              Pen.Color := clWindowFrame;            { Right side }
              MoveTo(Width - 1, FTabHeight - 1);
              LineTo(Width - 1, Height-1);

              LineTo(-1, Height - 1);                { Bottom }
            end;
      end;
  end;

procedure TTabbedPages.SetAutoScroll(Value: boolean);
  begin
    if Value <> FAutoScroll
      then
        begin
          FAutoScroll := Value;
          Scroller.Visible := False;
          ShowWindow(Scroller.Handle, SW_HIDE);
          Invalidate;
        end;
  end;

procedure TTabbedPages.CreateScroller;
  begin
    Scroller := TScroller.Create(Self);
    with Scroller do
      begin
        Parent := Self;
        Min := 0;
        Max := 0;
        Height := FTabHeight - 6;
        Width := 2 * Height;
        Position := 0;
        Visible := False;
        OnClick := ScrollClick;
      end;
    if Kind = kdUp
      then Scroller.Top := 3
      else Scroller.Top := Height - FTabHeight + 3;
  end;

procedure TTabbedPages.ScrollClick(Sender: TObject);
  begin
    FirstIndex := TScroller(Sender).Position;
  end;

procedure TTabbedPages.CMDialogChar(var Message: TCMDialogChar);
  var
    I: integer;
  begin
    for I := 0 to pred(Pages.Count) do
      begin
        if IsAccel(Message.CharCode, Pages[I])
          then
            begin
              Message.Result := 1;
              if FPageIndex <> I
                then SetPageIndex(I);
              Exit;
            end;
      end;
    inherited;
  end;

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TTabbedPages] );
    end;

end.
