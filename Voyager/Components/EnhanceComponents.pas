unit EnhanceComponents;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, Commctrl;

type
  TOnImagenList = procedure (const sender: TObject; const index: integer; const image: TBitMap) of object;
  TOnImagenCache = function (const sender: TObject; const index: integer): TGraphic of object;
  TCustomSyntaxWriter = class;
  TCustomMerchiseListBox =
    class(TCustomListBox)
        private
          fImages: TCustomImageList;
          //fBackGroundImage : TBitMap;
          fImagesChangeLink: TChangeLink;
          fEnhance : boolean;
          fBackGround: TPicture;
          fMaxSizeFont: integer;
          fColorEven : TColor;
          fColorSelect: TColor;
          fSyntaxWriter : TCustomSyntaxWriter;
          fOnImagenList: TOnImagenList;
          fOnImagenCache: TOnImagenCache;
          fImgenGutter  : integer;
          fUnselectWendLostFocus: boolean;
        protected
          procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
          procedure MeasureItem(Index: Integer; var Height: Integer); override;
          //
          procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
        protected
          procedure SetPicture(const Value: TPicture);
          procedure SetImages(const Value: TCustomImageList);
          procedure ImageListChange(Sender: TObject);
          procedure SetImageList(const Value: HImageList; const Flags: Integer);
          procedure MakeTag(const tag: string; const index: integer; const oRect: TRect; var rect: TRect; var MaxHeight : integer);
          procedure SetEnhance(const value: boolean);
          function  GetSyntaxWriter: TCustomSyntaxWriter;
       public
          constructor Create(AOwner: TComponent); override;
          destructor  Destroy; override;
          procedure   AddSyntaxWriter;
          procedure   AddSyntaxWriterSorted;
          function    SelectSyntax(const index: integer):TCustomSyntaxWriter;
        protected
          property Enhance: boolean read fEnhance write SetEnhance;
          property ImgenGutter: integer read fImgenGutter write fImgenGutter;
          property BackGround: TPicture read fBackGround write SetPicture;
          property ColorEven : TColor read fColorEven write fColorEven;
          property ColorSelect: TColor read fColorSelect write fColorSelect;
          property MaxSizeFont: integer read fMaxSizeFont write fMaxSizeFont;
          property Images: TCustomImageList read fImages write SetImages;
          property SyntaxWriter: TCustomSyntaxWriter read GetSyntaxWriter;
          property OnImagenList: TOnImagenList read fOnImagenList write fOnImagenList;
          property OnImagenCache: TOnImagenCache read fOnImagenCache write fOnImagenCache;
          property Unselect: boolean read fUnselectWendLostFocus write fUnselectWendLostFocus;
        public
    end;

  TMerchiseListBox =
    class(TCustomMerchiseListBox)
      published
        property Align;
        property Anchors;
        property BiDiMode;
        property BorderStyle;
        property Color;
        property Columns;
        property Constraints;
        property Ctl3D;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property ExtendedSelect;
        property Font;
        property ImeMode;
        property ImeName;
        property IntegralHeight;
        property ItemHeight;
        property Items;
        property MultiSelect;
        property ParentBiDiMode;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property Sorted;
        property Style;
        property TabOrder;
        property TabStop;
        property TabWidth;
        property Visible;
        property UnSelect;
        property ColorSelect;

        property ColorEven;
        property MaxSizeFont;
        property BackGround;
        property Enhance;
        property Images;
        property SyntaxWriter;
        property ImgenGutter;
        property OnClick;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnDrawItem;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMeasureItem;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnStartDock;
        property OnStartDrag;
        property OnImagenList;
        property OnImagenCache;
      end;

  TCustomMerchiseChatEdit =
    class(TCustomControl)
      private
        fSyntaxWriter : TCustomSyntaxWriter;
        function GetTextChat: string;
        procedure SetTextChat(const Value: string);
      protected
        procedure Paint; override;
      public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
      protected
        property Text: string read  GetTextChat write SetTextChat;
    end;

  TMerchiseChatEdit =
    class(TCustomMerchiseChatEdit)
      property Anchors;
      //property AutoSelect;
      property AutoSize;
      property BiDiMode;
      //property BorderStyle;
      //property CharCase;
      property Color;
      property Constraints;
      property Ctl3D;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property Font;
      //property HideSelection;
      property ImeMode;
      property ImeName;
     //property MaxLength;
      //property OEMConvert;
      property ParentBiDiMode;
      property ParentColor;
      property ParentCtl3D;
      property ParentFont;
      property ParentShowHint;
     // property PasswordChar;
      property PopupMenu;
      //property ReadOnly;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Text;
      property Visible;
      //property OnChange;

      property OnClick;
      property OnContextPopup;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDock;
      property OnStartDrag;
    end;

  TCustomSyntaxWriter =
    class
      private
        fStringSyntax: string;
        fImagenList : TOnImagenList;
        procedure SetColorStr(const Value: string);
        procedure SetColor(const value: TColor);
        procedure SetHeight(const Value: Integer);
        procedure SetImage(const Value: integer);
        procedure SetFontName(const Value: TFontName);
        procedure SetStyle(const Index: Integer; const Value: boolean);
        function  GetLength: integer;
        function  GetRawText: string;
      public
        procedure Add(const s: string);
        procedure Return;
        procedure CharForImage(const pos, ImagenIndex: integer);
        function  GetImagenIn(const pos: integer): integer;
        function  IsReturn(const pos: integer): boolean;
        procedure Clear;
        // DrawProcess
        procedure ProcessDraw(const Canvas: TCanvas; const DefaltFont: TFont; const WinRect: TRect; var Height: integer);
        procedure MeasureItem(const Canvas: TCanvas; const width: integer; var Height: integer);
      private
        procedure MakeTag(const tag: string; const oRect: TRect; var rect: TRect; var MaxHeight : integer);
      public
        property Bold: boolean          index 0 write SetStyle;
        property Italic: boolean        index 1 write SetStyle;
        property Underline: boolean     index 2 write SetStyle;
        property StrikeOut: boolean     index 3 write SetStyle;
        property ColorStr: string               write SetColorStr;
        property Color: TColor                  write SetColor;
        property Height: Integer                write SetHeight;
        property FontName: TFontName            write SetFontName;
        property Image: integer                 write SetImage;
        property Text: string  read fStringSyntax write fStringSyntax;
        property RawText     : string  read GetRawText;
        property Length: integer                read GetLength;
        property ImagenIn[const index: integer]: integer  read GetImagenIn;

        property ImagenList : TOnImagenList read fImagenList write fImagenList;
    end;


procedure Register;

implementation

{$R *.dcr}

procedure Register;
  begin
    RegisterComponents('Merchise', [TMerchiseListBox, TMerchiseChatEdit]);
  end;

{ TMerchiseListBox }
constructor TCustomMerchiseListBox.Create(AOwner: TComponent);
  begin
    inherited;
    fImagesChangeLink := TChangeLink.Create;
    fImagesChangeLink.OnChange := ImageListChange;
    fBackGround  := TPicture.Create;
    fMaxSizeFont := 18;
    fColorEven := clWindow;
    fColorSelect := clHighlight;
  end;

destructor TCustomMerchiseListBox.Destroy;
  begin
    fBackGround.Assign(nil);
    fBackGround.Free;
    fImagesChangeLink.free;
    fSyntaxWriter.free;
    inherited;
  end;

function TCustomMerchiseListBox.GetSyntaxWriter: TCustomSyntaxWriter;
  begin
    if fSyntaxWriter=nil
      then fSyntaxWriter := TCustomSyntaxWriter.Create;
    result := fSyntaxWriter;
  end;

function TCustomMerchiseListBox.SelectSyntax(const index: integer):TCustomSyntaxWriter;
  begin
    if fSyntaxWriter=nil
      then fSyntaxWriter := TCustomSyntaxWriter.Create;
    fSyntaxWriter.fStringSyntax := Items[index];
    result := fSyntaxWriter;
  end;

procedure TCustomMerchiseListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    Flags: Longint;
    p, q : pchar;
    Size : TSize;
    s    : string;
    Rct  : TRect;
    MaxHeight : integer;
  begin
    if Assigned(OnDrawItem)
      then OnDrawItem(Self, Index, Rect, State)
      else
        begin
          if fEnhance
            then
              begin
                if (odSelected in State)
                  then Canvas.Brush.Color := fColorSelect
                  else
                    if (Index and 1=0)
                      then Canvas.Brush.Color := Color
                      else Canvas.Brush.Color := fColorEven;
              end;
          Canvas.FillRect(Rect);
          Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_NOPREFIX);
          if Index < Items.Count
            then
              begin
                {if (fBackGround.Bitmap<>nil) and (Canvas.Brush.Color <> clHighlight)
                  then
                    begin
                      Canvas.FillRect(Rect);
                      Canvas.CopyRect(Rect, fBackGround.Bitmap.Canvas, Rect);
                      Canvas.Brush.Style := bsClear;
                    end
                  else}
                //if not UseRightToLeftAlignment
                 // then Inc(Rect.Left, 2)
                  //else Dec(Rect.Right, 2);
                if fEnhance
                  then
                    begin
                      Canvas.Font := Font;
                      Rct := Rect;
                      MaxHeight := 0;

                      p := PChar(Items[Index]);
                      while (p[0]<>#0) do
                        begin
                          q := p;
                          while ((p[0]<>'<') or ((p[0]='<') and (p[1]='<'))) and (p[0]<>#0) do
                            if (p[0]='<') and (p[1]='<')
                              then inc(p, 2)
                              else inc(p);
                          if p<>q
                            then
                              begin
                                Size.cX := 0;
                                Size.cY := 0;
                                Windows.GetTextExtentPoint32(Canvas.Handle, q, p-q, Size);
                                DrawText(Canvas.Handle, q, p-q, Rect, Flags);
                                with Size do
                                  begin
                                    inc(Rect.Left, succ(cX));
                                    if MaxHeight<cy
                                      then MaxHeight := cy;
                                  end;
                              end;
                          if p[0]<>#0
                            then
                              begin
                                q := p;
                                while (p[0]<>#0) and (p[0]<>'>') do
                                  inc(p);
                                if p<>q
                                  then
                                    begin
                                      if p[0]='>'
                                        then inc(p);
                                      setlength(s, p-q);
                                      move(q^, s[1], p-q);
                                      MakeTag(s, Index, Rct, rect, MaxHeight);
                                    end;
                              end;
                        end
                    end
                  else DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, Flags);
              end;
        end;
  end;

procedure TCustomMerchiseListBox.MakeTag(const tag: string; const index: integer; const oRect: TRect; var rect: TRect; var MaxHeight: integer);
  var
    a : char;
    Image: TBitMap;
    Im : TGraphic;
    i : integer;
    s : string;
  begin
    a := UpCase(tag[2]);
    if (a='R')
      then
        begin
          rect.Left := oRect.Left;
          inc(rect.Top, MaxHeight);
          MaxHeight := 0;
        end
      else
        if (a>='0') and (a<='9')
          then
            begin
              try
                s := copy(Tag, 2, length(tag)-2);
                i := strtoint(s);
                if (fImages<>nil) or assigned(fOnImagenList) or assigned(fOnImagenCache)
                  then
                    begin
                      Im := nil;
                      try
                        if assigned(fOnImagenCache)
                          then
                            begin
                              Im := fOnImagenCache(self, i);
                              if (Im is TBitMap)
                                then Image := Im as TBitMap
                                else
                                  begin
                                    Image := TBitMap.Create;
                                    with Image do
                                      begin
                                        Width := Im.Width;
                                        Height := Im.Height;
                                        Canvas.Draw(0, 0, Im);
                                      end;
                                    Im := nil;
                                  end;
                            end
                          else
                            begin
                              Image := TBitMap.Create;
                              if (fImages<>nil)
                                then fImages.GetBitmap(i, Image as TBitMap);
                              if assigned(fOnImagenList)
                                then fOnImagenList(self, i, Image as TBitMap);
                            end;
                        if Image<>nil
                          then
                            begin
                              Image.Transparent := true;
                              Canvas.Draw(rect.Left, rect.Top, Image);
                              with Image.Canvas.ClipRect do
                                begin
                                  inc(rect.Left, Right-Left+fImgenGutter);
                                  i := Bottom-Top;
                                  if MaxHeight<i
                                    then MaxHeight:=i;
                                end;
                            end;
                      finally
                        if Im=nil
                          then Image.free;
                      end;
                    end;
              except
              end;
            end
          else
            begin
              with Canvas.Font do
                case a of
                  'I': // Italic
                    Style := Style+[fsItalic];
                  'B': // Bold
                    Style := Style+[fsBold];
                  'U': // UnderLine
                    Style := Style+[fsUnderline];
                  'S': // StrikeOut
                    Style := Style+[fsStrikeOut];
                  'C': // color
                    begin
                      try
                        s := copy(Tag, 3, length(tag)-3);
                        i := TColor(StrToInt(s));
                      except
                        if not IdentToColor('cl'+s, integer(i))
                          then i := Color;
                      end;
                      Color := i;
                    end;
                  'N': //Font Name
                      Name := copy(Tag, 3, length(tag)-3);
                  'Z'://Font Size
                    begin
                      try
                        s := copy(Tag, 3, length(tag)-3);
                        i := StrToInt(s);
                        if i<=fMaxSizeFont
                          then Size := i;
                      except
                      end;
                    end;
                  '/':
                  case UpCase(tag[3]) of
                    'I': // Italic
                      Style := Style-[fsItalic];
                    'B': // Bold
                      Style := Style-[fsBold];
                    'U': // UnderLine
                      Style := Style-[fsUnderline];
                    'S': // StrikeOut
                      Style := Style-[fsStrikeOut];
                    'C':
                      Color := self.font.color;
                    'N':
                      Name := self.font.Name;
                    'Z':
                      Size := self.font.Size;
                  end;
                end;
            end;
  end;

procedure TCustomMerchiseListBox.MeasureItem(Index: Integer; var Height: Integer);
  var
    p, q : pchar;
    Size : TSize;
    Item : string;
    s : string;
    lHeight: integer;
    tHeight : integer;
  procedure TagHeight;
    var
      a : char;
      i : integer;
      Image: TGraphic;
    begin
      a := UpCase(s[2]);
      if (a='R')
        then
          begin
            inc(tHeight, lHeight);
            lHeight := 0;
          end
        else
          if (a>='0') and (a<='9')
            then
              try
                i := strtoint(copy(s, 2, length(s)-2));
                if (fImages<>nil) or assigned(fOnImagenList) or assigned(fOnImagenCache)
                  then
                    begin
                      if assigned(fOnImagenCache)
                        then
                          begin
                            Image := fOnImagenCache(self, i);
                            if Image.Height>lHeight
                              then lHeight := Image.Height;
                          end
                        else
                          begin
                            Image:= TBitMap.Create;
                            try
                              if (fImages<>nil)
                                then fImages.GetBitmap(i, (Image as TBitMap));
                              if assigned(fOnImagenList)
                                then fOnImagenList(self, i, (Image as TBitMap));
                              with (Image as TBitMap).Canvas.ClipRect do
                                if Bottom-Top>lHeight
                                  then lHeight := Bottom-Top;
                            finally
                              Image.free;
                            end;
                          end;
                    end;
              except
              end
            else
              with Canvas.Font do
                case a of
                  'N': //Font Name
                      Name := copy(s, 3, length(s)-3);
                  'Z'://Font Size
                    begin
                      try
                        s := copy(s, 3, length(s)-3);
                        i := StrToInt(s);
                        if i<=fMaxSizeFont
                          then Size := i;
                      except
                      end;
                    end;
                  '/':
                    case UpCase(s[3]) of
                      'N':
                        Name := self.font.Name;
                      'Z':
                        Size := self.font.Size;
                    end;
                end;
    end;

  begin
    if fEnhance
      then
        begin
          Item := Items[Index];
          Canvas.Font := Font;
          p := PChar(Item);
          // lHeight := Height;
          tHeight := 0;
          lHeight := 0;
          while (p[0]<>#0) do
            begin
              q := p;
              while ((p[0]<>'<') or ((p[0]='<') and (p[1]='<'))) and (p[0]<>#0) do
                if (p[0]='<') and (p[1]='<')
                  then inc(p, 2)
                  else inc(p);
                    if p<>q
                      then
                        begin
                          Size.cX := 0;
                          Size.cY := 0;
                          Windows.GetTextExtentPoint32(Canvas.Handle, q, p-q, Size);
                          if lHeight< Size.cY
                            then lHeight := Size.cY;
                        end;
                    if p[0]<>#0
                      then
                        begin
                          q := p;
                          while (p[0]<>#0) and (p[0]<>'>') do
                            inc(p);
                          if p<>q
                            then
                              begin
                                if p[0]='>'
                                  then inc(p);
                                setlength(s, p-q);
                                move(q^, s[1], p-q);
                                TagHeight;
                              end;
                        end;
            end;
        end;
      inc(tHeight, lHeight);
      if tHeight>Height
        then Height := tHeight;
      inherited;
  end;

procedure TCustomMerchiseListBox.ImageListChange(Sender: TObject);
  var
    ImageHandle: HImageList;
  begin
    if HandleAllocated
      then
        begin
          if TCustomImageList(Sender).HandleAllocated
            then ImageHandle := TCustomImageList(Sender).Handle
            else ImageHandle := 0;
          if Sender = fImages
            then SetImageList(ImageHandle, LVSIL_NORMAL);
        end;
  end;

procedure TCustomMerchiseListBox.SetImageList(const Value: HImageList; const Flags: Integer);
  begin
    if HandleAllocated
      then ListView_SetImageList(Handle, Value, Flags);
  end;

procedure TCustomMerchiseListBox.CMFocusChanged( var Message: TCMFocusChanged);
  begin
    inherited;
    if fUnselectWendLostFocus
      then ItemIndex := -1;
  end;

procedure TCustomMerchiseListBox.SetImages(const Value: TCustomImageList);
  begin
    if Value <> fImages
      then
        begin
          if fImages <> nil
            then fImages.UnRegisterChanges(fImagesChangeLink);
          fImages := Value;
          if fImages <> nil
            then
              begin
                fImages.RegisterChanges(fImagesChangeLink);
                fImages.FreeNotification(Self);
                SetImageList(fImages.Handle, LVSIL_SMALL)
              end
            else
              SetImageList(0, LVSIL_SMALL);
          Invalidate;
        end;
  end;

procedure TCustomMerchiseListBox.SetEnhance(const value: boolean);
  begin
    fEnhance := value;
    if value
      then Style := lbOwnerDrawVariable;
  end;

procedure TCustomMerchiseListBox.SetPicture(const Value: TPicture);
  begin
    fBackGround.Assign(Value);
  end;

procedure TCustomMerchiseListBox.AddSyntaxWriter;
  begin
    if fSyntaxWriter<>nil
      then
        begin
          Items.Add(fSyntaxWriter.fStringSyntax);
          fSyntaxWriter.fStringSyntax := '';
        end;
  end;

procedure TCustomMerchiseListBox.AddSyntaxWriterSorted;
  var
    s1, s2 : string;
    i : integer;
  begin
    if fSyntaxWriter<>nil
      then
        begin
          s1 := fSyntaxWriter.fStringSyntax;
          s2 := fSyntaxWriter.RawText;
          i := 0;
          while (i<Items.Count) and (CompareText(s2, SelectSyntax(i).RawText)>=0) do
            inc(i);
          Items.insert(i, s1);
          fSyntaxWriter.fStringSyntax := '';
        end;
  end;

{ TCustomSyntaxWriter }
procedure TCustomSyntaxWriter.Add(const s: string);
  begin
    fStringSyntax := fStringSyntax + s;
  end;

procedure TCustomSyntaxWriter.Return;
  begin
    fStringSyntax := fStringSyntax + '<R>';
  end;

procedure TCustomSyntaxWriter.SetColor(const value: TColor);
  begin
    fStringSyntax := fStringSyntax+'<C'+inttostr(value)+'>';
  end;

procedure TCustomSyntaxWriter.SetHeight(const Value: Integer);
  var
    s : string;
  begin
    if Value=0
      then s := '</z>'
      else s := '<z'+inttostr(Value)+'>';
    fStringSyntax := fStringSyntax+s;
  end;

procedure TCustomSyntaxWriter.SetImage(const Value: integer);
  begin
    fStringSyntax := fStringSyntax+'<'+inttostr(Value)+'>';
  end;

procedure TCustomSyntaxWriter.SetFontName(const Value: TFontName);
  var
    s : string;
  begin
    if Value =''
      then s := '</N>'
      else s := '<N'+Value+'>';
    fStringSyntax := fStringSyntax + s;
  end;

procedure TCustomSyntaxWriter.SetStyle(const Index: Integer; const Value: boolean);
  const
    CStyle  : array[boolean] of array[0..3] of string =
      (('</B>','</I>','</U>','/S'),
       ('<B>','<I>','<U>','S'));
  begin
    fStringSyntax := fStringSyntax + CStyle[Value, Index];
  end;

procedure TCustomSyntaxWriter.SetColorStr(const Value: string);
  var
    s : string;
    i : integer;
  begin
    try
      s := '';
      i := StrToInt(value);
    except
      if not IdentToColor('cl'+Value, i)
        then s:='</C>';
    end;
    if s=''
      then s:= '<C'+inttostr(i)+'>';
    fStringSyntax := fStringSyntax+s;
  end;

procedure TCustomSyntaxWriter.CharForImage(const pos, ImagenIndex: integer);
  var
    i : integer;
    p : pchar;
    q : pchar;
    c : pchar;
    Stemp : string;
    temp : string;
    a : char;
  begin
    p := pchar(fStringSyntax);
    setlength(sTemp, system.length(fStringSyntax)+8);
    c :=pchar(sTemp);
    i := 0;
    q := c;
    while (i<pos) and (p[0]<>#0) do
      begin
        c[0] := p[0];
        q:=c;
        if (p[0]='<') and (p[1]<>'<')
          then
            begin
              a := Upcase(p[1]);
              if ((a>='0') and (a<='9')) or (a='R')
                then inc(i);

              while (p[0]<>'>') and (p[0]<>#0) do
                begin
                  inc(p);
                  inc(c);
                  c[0]:=p[0];
                end;
              if p[0]='>'
                then
                  begin
                    inc(p);
                    inc(c);
                  end;
            end
          else
            begin
              if (p[0]='<') and (p[1]='<')
                then
                  begin
                    inc(c);
                    inc(p);
                    c[0]:=p[0];
                  end;
              inc(i);
              inc(p);
              inc(c);
            end;
      end;

    if (i=pos)
      then c:=q;

    temp := '<'+inttostr(ImagenIndex)+'>';
    q := pchar(temp);
    while (q[0]<>#0) do
      begin
        c[0] := q[0];
        inc(q);
        inc(c);
      end;

    while (p[0]<>#0) do
      begin
        c[0] := p[0];
        inc(p);
        inc(c);
      end;

    setlength(sTemp, c-pchar(sTemp));
    fStringSyntax := sTemp;
  end;

function TCustomSyntaxWriter.GetImagenIn(const pos: integer): integer;
  var
    j : integer;
    p, q : pchar;
    b : boolean;
    s : string;
    a : char;
  begin
    p := pchar(fStringSyntax);
    j := 1;
    result := -1;
    while (p[0]<>#0) and (j<=pos) do
      begin
       if (p[0]='<') and (p[1]<>'<')
          then
            begin
              inc(p);
              a := Upcase(p[0]);
              b := (a>='0') and (a<='9');
              if (j=pos)
                then q := p;
              while (p[0]<>'>') and (p[0]<>#0) do
                inc(p);
              if b
                then
                  begin
                    if (j=pos)
                      then
                        begin
                          s := copy(fStringSyntax, q-pchar(fStringSyntax)+1, p-q);
                          result := StrToInt(s);
                        end;
                    inc(j);
                  end
                else
                 if a='R'
                   then inc(j);
              if p[0]<>#0
                then inc(p);
            end
          else
            begin
              if (p[0]='<') and (p[1]='<')
                then inc(p);
              inc(result);
              inc(p);
              inc(j);
             end;
     end;
  end;

function  TCustomSyntaxWriter.IsReturn(const pos: integer): boolean;
  var
    j : integer;
    p, q : pchar;
    b : boolean;
    s : string;
    a : char;
  begin
    p := pchar(fStringSyntax);
    j := 1;
    result := false;
    while (p[0]<>#0) and (j<=pos) do
      begin
       if (p[0]='<') and (p[1]<>'<')
          then
            begin
              inc(p);
              a := Upcase(p[0]);
              result := (j=pos) and (a='R');
              if not result
                then
                  begin
                    while (p[0]<>'>') and (p[0]<>#0) do
                      inc(p);
                    if ((a>='0') and (a<='9')) or (a='R')
                      then inc(j);
                    if p[0]<>#0
                      then inc(p);
                  end;
            end
          else
            begin
              if (p[0]='<') and (p[1]='<')
                then inc(p);
              inc(p);
              inc(j);
             end;
     end;
  end;

procedure TCustomSyntaxWriter.Clear;
  begin
    fStringSyntax := '';
  end;

function TCustomSyntaxWriter.GetLength: integer;
  var
    p : pchar;
    a : char;
  begin
    result := 0;
    p := pchar(fStringSyntax);
    while (p[0]<>#0) do
      begin
        if (p[0]='<') and (p[1]<>'<')
          then
            begin
              a := Upcase(p[1]);
              if (a>='0') and (a<='9')
                then inc(result);
              while (p[0]<>'>') and (p[0]<>#0) do
                inc(p);
              if p[0]<>#0
                then inc(p);
            end
          else
            begin
              if (p[0]='<') and (p[1]='<')
                then inc(p);
              inc(result);
              inc(p);
            end;
      end;
  end;

function TCustomSyntaxWriter.GetRawText: string;
  var
    p : pchar;
    q : pchar;
  begin
    setlength(result, system.length(fStringSyntax));
    p := pchar(fStringSyntax);
    q := pchar(result);
    while (p[0]<>#0) do
      begin
        if (p[0]='<') and (p[1]<>'<')
          then
            begin
              while (p[0]<>'>') and (p[0]<>#0) do
                inc(p);
              if p[0]<>#0
                then inc(p);
            end
          else
            begin
              if (p[0]='<') and (p[1]='<')
                then inc(p);
              q[0] := p[0];
              inc(q);
              inc(p);
            end;
      end;
    Setlength(result, q-pchar(result));
  end;

{ TCustomMerchiseEdit }

constructor TCustomMerchiseChatEdit.Create(AOwner: TComponent);
  begin
    inherited;
    fSyntaxWriter := TCustomSyntaxWriter.Create;
  end;

destructor TCustomMerchiseChatEdit.Destroy;
  begin
    fSyntaxWriter.free;
    inherited;
  end;

function TCustomMerchiseChatEdit.GetTextChat: string;
  begin
    result := fSyntaxWriter.fStringSyntax;
  end;

procedure TCustomMerchiseChatEdit.Paint;
  begin
  end;

procedure TCustomMerchiseChatEdit.SetTextChat(const Value: string);
  begin
    fSyntaxWriter.fStringSyntax := Value;
  end;

procedure TCustomSyntaxWriter.MeasureItem(const Canvas: TCanvas; const width: integer; var Height: integer);
  begin

  end;

procedure TCustomSyntaxWriter.MakeTag(const tag: string; const oRect: TRect; var rect: TRect; var MaxHeight : integer);
  begin
  end;

procedure TCustomSyntaxWriter.ProcessDraw(const Canvas: TCanvas; const DefaltFont: TFont; const WinRect: TRect; var Height: integer);
  var
    Flags: Longint;
    p, q : pchar;
    Size : TSize;
    s    : string;
    Rct  : TRect;
    MaxHeight : integer;
    Rect : TRect;
  begin
    Canvas.Font := DefaltFont;
    Rct := WinRect;
    Rect := WinRect;
    MaxHeight := 0;

    p := PChar(fStringSyntax);
    while (p[0]<>#0) do
      begin
        q := p;
        while ((p[0]<>'<') or ((p[0]='<') and (p[1]='<'))) and (p[0]<>#0) do
          if (p[0]='<') and (p[1]='<')
            then inc(p, 2)
            else inc(p);
        if p<>q
          then
            begin
              Size.cX := 0;
              Size.cY := 0;
              Windows.GetTextExtentPoint32(Canvas.Handle, q, p-q, Size);
              DrawText(Canvas.Handle, q, p-q, Rect, Flags);
              with Size do
                begin
                  inc(Rect.Left, succ(cX));
                  if MaxHeight<cy
                    then MaxHeight := cy;
                end;
            end;
        if p[0]<>#0
          then
            begin
              q := p;
              while (p[0]<>#0) and (p[0]<>'>') do
                inc(p);
              if p<>q
                then
                  begin
                    if p[0]='>'
                      then inc(p);
                    setlength(s, p-q);
                    move(q^, s[1], p-q);
                    MakeTag(s, Rct, rect, MaxHeight);
                  end;
            end;
      end
  end;



end.
