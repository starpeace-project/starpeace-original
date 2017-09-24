unit CoolSB;

interface

  uses
    Windows, comctrls, graphics;

function InitSkin( SkinFile: PChar): boolean; stdcall;
procedure CustomizeListViewHeader( ListView: TListView;
             HeaderBk: TColor = $00505939;
             HeaderFg: TColor = $00DDFFFF;
             FirstBk: TColor  = $F0F0F0;
             Firstfg: TColor  = clBlack;
             SecndBk: TColor  = clWhite;
             SecndFg: TColor  = clBlack;
             HighLC: TColor   = $00BBBBBB
             );
function InitializeCoolSB( hwnd: HWND ): boolean; stdcall;
function CoolSBEnableBar( hwnd: HWND; bEnableHorzBar: boolean{=FALSE}; bEnableVertBar: boolean{=TRUE}) : boolean; stdcall;
function CoolSBAutoHideBar( hwnd: HWND; bAutoHideHorzBar: boolean; bAutoHideVertBar: boolean): boolean; stdcall;
function UninitializeCoolSB( hwnd: HWND) : HRESULT; stdcall;
function CoolSB_IsScrollBarVisible(hwnd: HWND; wBar: integer): boolean; stdcall;

var
  hThemeLib: HINST;
var
  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  {$EXTERNALSYM SetWindowTheme}
  IsThemeActive: function: BOOL; stdcall;
  {$EXTERNALSYM IsThemeActive}

var
  InitSkinImage : boolean = false;

implementation
  uses
    messages, Commctrl, Extctrls, classes, Sysutils;
{$L coolsblib.obj}
{$L coolscroll.obj}

type
  TDummyObject = class( TObject)
    procedure ListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure ListViewDrawItem(Sender: TCustomListView;
      Item: TListItem; Rect: TRect; State: TOwnerDrawState);
  end;

var
  DummyO : TDummyObject;

const
  headerprocprop = 'headerprocprop';
  listviewprocprop = 'listviewprocprop';
  listviewinstance = 'listviewinstance';
  headerbkprop = 'headerbkprop';
  headerfgprop = 'headerfgprop';
  firstbkprop  = 'firstbkprop';
  firstfgprop  = 'firstfgprop';
  secndbkprop  = 'secndbkprop';
  secndfgprop  = 'secndfgprop';
  highlcprop   = 'highlcprop';
  imgidxprop   = 'imgidxprop';

const
  themelib = 'uxtheme.dll';

const
  SubItemV : integer = -1;

procedure TDummyObject.ListViewCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
begin
  SubItemV := SubItem;
  ListViewCustomDrawItem(Sender, Item, State, DefaultDraw);
  SubItemV := -1;
end;

procedure TDummyObject.ListViewDrawItem(Sender: TCustomListView;
      Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
 ItemRect: TRect;
 ItemText : string;
 FirstBk : TColor;
 FirstFg : TColor;
 SecndBk : TColor;
 SecndFg : TColor;
 HighLC : TColor;
 i : integer;
 Alignment : integer;
begin
  if (Item.Index < TListView(Sender).TopItem.Index)
    then exit;
  FirstBk := TColor(GetProp(TListview(Sender).Handle, firstbkprop));
  FirstFg := TColor(GetProp(TListview(Sender).Handle, firstfgprop));
  SecndBk := TColor(GetProp(TListview(Sender).Handle, secndbkprop));
  SecndFg := TColor(GetProp(TListview(Sender).Handle, secndfgprop));
  HighLC  := TColor(GetProp(TListview(Sender).Handle, highlcprop));
  with TListview(Sender).Canvas do
    begin
      if Odd(Item.Index)
        then
          begin
            Brush.Color := FirstBk;//clmoneygreen;
            Font.Color := FirstFg;
          end
        else
          begin
            Brush.Color := SecndBk;
            Font.Color := SecndFg;
          end;
      //DefaultDraw := False; //user draw
      ItemRect := Rect;
      ItemText := Item.Caption;
{      if SubItemV = -1
        then
          begin
            ItemText := Item.Caption;
            ItemRect.Right := ItemRect.Left+Sender.Column[0].Width;
          end
        else
          begin
            if SubItemV < Item.SubItems.Count
              then
                ItemText := Item.SubItems[SubItemV]
              else
                ItemText := '';
            for i := 1 to SubItemV do
              begin
                ItemRect.Left := ItemRect.Right;
                ItemRect.Right := ItemRect.Right+Sender.Column[i].Width;
              end;
          end;}
      if (odSelected in state)
        then
          Brush.Color := HighLC;
{      if Item.Selected
        then
          begin
            Brush.Color := clHighlight;
            Font.Color := clHighlightText;
            //Rectangle(Rect);
            //Font.Style:= [fsBold] else  Font.Style:= [];
          end;}
      FillRect(ItemRect);
      if Item.ImageIndex <> -1
        then
          begin
            if assigned(TListview(Sender).SmallImages)
              then
                TListView(Sender).SmallImages.Draw(TListview(Sender).Canvas, ItemRect.Left, ItemRect.Top, Item.ImageIndex, true);
            TextOut(ItemRect.Left+17, ItemRect.top, item.caption);
          end
        else
          TextOut(ItemRect.Left+17, ItemRect.top, item.caption);
      for i := 0 to Item.SubItems.Count-1 do
        begin
          ItemRect.Left := ItemRect.Right;
          ItemRect.Right := ItemRect.Right + Sender.Column[i+1].Width;
          case Sender.Column[i+1].Alignment of
            taLeftJustify:
              Alignment := DT_LEFT;
            taRightJustify:
              Alignment := DT_RIGHT;
            taCenter:
              Alignment := DT_CENTER;
          end;
          DrawText(Sender.Canvas.Handle, PChar(Item.SubItems[i]), length(Item.SubItems[i]), ItemRect, Alignment);
        end;
{      if SubItemV <> Item.SubItems.Count
        then
          DefaultDraw := True; // AI takes over}
    end;
end;

procedure TDummyObject.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
 ItemRect, ARect: TRect;
 ItemText : string;
 FirstBk : TColor;
 FirstFg : TColor;
 SecndBk : TColor;
 SecndFg : TColor;
 HighLC : TColor;
 i : integer;
 Alignment : integer;
begin
  if (Item.Index < TListView(Sender).TopItem.Index)
    then exit;
  FirstBk := TColor(GetProp(TListview(Sender).Handle, firstbkprop));
  FirstFg := TColor(GetProp(TListview(Sender).Handle, firstfgprop));
  SecndBk := TColor(GetProp(TListview(Sender).Handle, secndbkprop));
  SecndFg := TColor(GetProp(TListview(Sender).Handle, secndfgprop));
  HighLC  := TColor(GetProp(TListview(Sender).Handle, highlcprop));
//  TListView(Sender).Items.BeginUpdate;
  with TListview(Sender).Canvas do
    begin
      if Odd(Item.Index)
        then
          begin
            Brush.Color := FirstBk;//clmoneygreen;
            Font.Color := FirstFg;
          end
        else
          begin
            Brush.Color := SecndBk;
            Font.Color := SecndFg;
          end;
      DefaultDraw := False; //user draw
      ItemRect := Item.DisplayRect(drBounds);
      ItemText := Item.Caption;
      if (Item.Selected) //cdsSelected in state)
        then
          begin
            ARect := ItemRect;
            ARect.Right := 16;
            FillRect(ARect);
            Refresh;
            Brush.Color := HighLC;
            Inc(ItemRect.Left, 16);
            FillRect(ItemRect);
            //Inc(ItemRect.Left, 16);
          end
        else
          begin
            //Inc(ItemRect.Left, 16);
            FillRect(ItemRect);
            Inc(ItemRect.Left, 16);///
          end;
      ItemRect.Right := ItemRect.Left+Sender.Column[0].Width-16;
      case Sender.Column[0].Alignment of
        taLeftJustify:
          Alignment := DT_LEFT;
        taRightJustify:
          Alignment := DT_RIGHT;
        taCenter:
          Alignment := DT_CENTER;
      end;
      Inc(ItemRect.Top,1);
      Brush.Color := clBlue;
      if (Item.ImageIndex <> -1) or (Item.StateIndex <> -1)
        then
          begin
            Dec(ItemRect.Top,1);
            if assigned(TListview(Sender).SmallImages)
              then
                TListView(Sender).SmallImages.Draw(TListview(Sender).Canvas, ItemRect.Left-16, ItemRect.Top, Item.ImageIndex, true)
              else
                if assigned(TListview(Sender).StateImages)
                  then
                    TListView(Sender).StateImages.Draw(TListview(Sender).Canvas, ItemRect.Left-16, ItemRect.Top, Item.StateIndex, true);
            Inc(ItemRect.Left, 1);
            Inc(ItemRect.Top,1);
            DrawText(Sender.Canvas.Handle, PChar(Item.caption), length(Item.caption), ItemRect, Alignment);
            Dec(ItemRect.Left, 1);
            //TextOut(ItemRect.Left+17, ItemRect.top, item.caption);
          end
        else
          DrawText(Sender.Canvas.Handle, PChar(Item.caption), length(Item.caption), ItemRect, Alignment);
          //TextOut(ItemRect.Left+17, ItemRect.top, item.caption);
      for i := 0 to Item.SubItems.Count-1 do
        begin
          if i < TListView(Sender).Columns.Count-1
            then
              begin
                ItemRect.Left := ItemRect.Right;
                if Sender.Column[i+1].Width < 0
                  then
                    ItemRect.Right := ItemRect.Right + (Sender.Width-ItemRect.Left-18)
                  else
                    ItemRect.Right := ItemRect.Right + Sender.Column[i+1].Width;
                case Sender.Column[i+1].Alignment of
                  taLeftJustify:
                    Alignment := DT_LEFT;
                  taRightJustify:
                    Alignment := DT_RIGHT;
                  taCenter:
                    Alignment := DT_CENTER;
                end;
                if i = Item.SubItems.Count-1
                  then Dec(ItemRect.Right, 2);
                if (Item.Selected) //cdsSelected in state)
                  then
                   Brush.Color := HighLC;
                DrawText(Sender.Canvas.Handle, PChar(Item.SubItems[i]), length(Item.SubItems[i]), ItemRect, Alignment);
              end;
        end;

//      if SubItemV <> Item.SubItems.Count
//        then
          DefaultDraw := false; // AI takes over
    end;
  TListView(Sender).Items.EndUpdate;
end;

procedure DrawHeader(TL: TList; TLV: TListView; Index: integer; ARect: TRect;
    AState: integer; DC: HDC; FgColor, BkColor: TColor);
var
  HeaderCanvas : TCanvas;
  text : string;
  Alignment : integer;
  Left : Integer;
  Column: TLVColumn;
  IndexI : integer ;
begin
//  hdn.clrText := clRed;
//  hdn.clrTextBk := clYellow;
//  {Msg.}Result := CDRF_NEWFONT;
//  if Index < TLV.Columns.Count
//    then exit;
  HeaderCanvas := TCanvas.Create;
  HeaderCanvas.Handle := dc;
//  rect := ARect;

//  Index := hdn.nmcd.dwItemSpec;
//  State := hdn.nmcd.uItemState;
  text := TLV.Columns.Items[Index].Caption;
  HeaderCanvas.Font.Color := FgColor;
  case TLV.Columns.Items[Index].Alignment of
    taLeftJustify:
      Alignment := DT_LEFT;
    taRightJustify:
      Alignment := DT_RIGHT;
    taCenter:
      Alignment := DT_CENTER;
  end;
  //HeaderCanvas.Font.Name := 'Tahoma';
  //HeaderCanvas.Font.Style := TFontStyles() << fsBold;
  HeaderCanvas.Brush.Color := BkColor;//clBlue;
  HeaderCanvas.Pen.Width := 2;
  HeaderCanvas.Pen.Color := clBlack;
  ARect.Bottom := ARect.Bottom;
  HeaderCanvas.Rectangle(ARect);//FillRect(Rect);
  if TL <> nil
    then
      if integer(TL.Items[Index]) <> -1
        then
          begin
{           HeaderCanvas.Font.Color := BkColor;
            HeaderCanvas.TextRect(ARect, ARect.Left + 17, ARect.Top+2,
                                   text);
            HeaderCanvas.Font.Color := FgColor;}
            HeaderCanvas.FillRect(ARect);
            InflateRect(ARect, 2, 2);
            HeaderCanvas.FrameRect(ARect);
            InflateRect(ARect, -2, -2);
            Inc(ARect.Top,2);
            Dec(ARect.Right, 4);
            Inc(ARect.Left, 17);
            DrawText(HeaderCanvas.Handle, PChar(text), length(text), ARect, Alignment);
            Dec(ARect.Left, 17);
            if Alignment = DT_RIGHT
              then
                begin
                  Left := ARect.Left;
                  Inc(Left, ARect.Right-ARect.Left-HeaderCanvas.TextWidth(text)-TLV.SmallImages.Width-4 );
                  if Left > ARect.Left
                    then
                      ARect.Left := Left;
                end;
            if assigned(TLV.SmallImages)
              then
                TLV.SmallImages.Draw(HeaderCanvas, ARect.Left, ARect.Top, integer(TL.Items[Index]), true);
          end
        else
          begin
            HeaderCanvas.FillRect(ARect);
            InflateRect(ARect, 2, 2);
            HeaderCanvas.FrameRect(ARect);
            InflateRect(ARect, -2, -2);
            Inc(ARect.Top,2);
            Dec(ARect.Right, 4);
            Inc(ARect.Left, 4);
            DrawText(HeaderCanvas.Handle, PChar(text), length(text), ARect, Alignment);
            //HeaderCanvas.TextRect(ARect, ARect.Left + 6, ARect.Top+2,
            //                   text);
          end;
  HeaderCanvas.Handle := 0;
  HeaderCanvas.Free;
end;

function ListViewHeaderProc( H : HWND; Msg : Cardinal; wParam, LParam : Cardinal) : Integer; stdcall;
var
  oldproc : pointer;
  hdn: ^TNMLVCUSTOMDRAW;
  hDCMem : HDC;
  Rect : TRect;
  hBmp : THandle;
  WDC : THandle;
  hOld : THandle;
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;  
begin
  hdn := Pointer({Msg.}LParam);
  oldproc := pointer(GetProp(h, headerprocprop));
  case Msg of
{    WM_TIMER:
      begin
        InvalidateRect(h, nil, true);
      end;}
    WM_ERASEBKGND:  //avoiding flickering on the header
      begin
        Result := 1;
        exit;
      end;
(*    WM_PAINT:
      begin
        {DC := GetDC(0);
        MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
        ReleaseDC(0, DC);
        MemDC := CreateCompatibleDC(0);
        OldBitmap := SelectObject(MemDC, MemBitmap);}

        hDCMem := CreateCompatibleDC(0);
        GetWindowRect(h, Rect);
        hBmp := 0;
        DC := GetDC(0);
        MemBitmap := CreateCompatibleBitmap(DC, rect.right - rect.left, rect.bottom - rect.top);
        ReleaseDC(0, DC);
        hOld := SelectObject(hDCMem, MemBitmap);
        result := CallWindowProc(oldproc, h, WM_PRINT, hDCMem, PRF_CLIENT);
        DC := BeginPaint(H, PS);
        BitBlt(DC, 0, 0, rect.right - rect.left, rect.bottom - rect.top, MemDC, 0, 0, SRCCOPY);
        EndPaint(H, PS);
        SelectObject(hDCMem, hOld);
        DeleteObject(hDCMem);
        DeleteObject(MemBitmap);
        exit;
      end;*)
{    WM_PAINT:
      begin
       result := CallWindowProc(oldproc, h, Msg, wParam, lParam);
       exit;
      end;}
    WM_DESTROY:
      begin
        SetWindowLong(H, GWL_WNDPROC, longint(oldproc));
      end;
{    WM_USER+3595:
      begin
        InvalidateRect(h, nil, true);
      end;}
{    WM_DRAWITEM:
      begin
        hdn.nmcd.hdr.code := hdn.nmcd.hdr.code;
      end;}
{    WM_NOTIFY:
      case hdn.nmcd.hdr.code of
        NM_CUSTOMDRAW:
          hdn.nmcd.hdr.code := hdn.nmcd.hdr.code;
      end;}
{        if (hdn.nmcd.hdr.code = HDN_TRACKA)
          then
            begin
              InvalidateRect(h, nil, true);
            end
          else
        if (hdn.nmcd.hdr.code = HDN_TRACKW)
          then
            begin
              InvalidateRect(h, nil, true);
            end;}
  end;
  result := CallWindowProc(oldproc, h, Msg, wParam, lParam);
end;

function ListViewProc( H : HWND; Msg : Cardinal; wParam, LParam : Cardinal) : Integer; stdcall;
var
  dis: ^TDrawItemStruct;
  hdnn: TNMLVCUSTOMDRAW;
  hdn: ^TNMLVCUSTOMDRAW;
  headern : PHDNotify;
  i : integer;
  hdi: THDItem;
  text : string;
  Index, State : integer;
  rect : TRect;
  point : TPoint;
  HeaderCanvas : TCanvas;
  oldproc : pointer;
  TLV : TListView;
  TL  : TList;
  BkColor, FgColor : TColor;
  Column: TLVColumn;
  TotalWidth : integer;
  AWidth, sum : integer;
  style : DWORD;
  hHeader : THandle;
  pStyle : PStyleStruct;
begin
  hdn := Pointer({Msg.}LParam);
  headern := Pointer(LParam);
  oldproc := pointer(GetProp(h, listviewprocprop));
  TLV := TListView(GetProp(h, listviewinstance));
  TL  := TList(GetProp(h, imgidxprop));
  BkColor := TColor(GetProp(h, headerbkprop));
  FgColor := TColor(GetProp(h, headerfgprop));
  case Msg of
{    WM_WINDOWPOSCHANGING:
      begin
        InvalidateRect(h, nil, true);
      end;}
{    WM_STYLECHANGING:
      begin
        if (wParam and GWL_STYLE) <> 0
          then
            begin
              pStyle := pointer(LParam);
              if ((pStyle.styleOld and pStyle.styleNew) and WS_HSCROLL)<> 0
                then
                  begin
                    result := 0;
                    exit;
                  end;
            end;
      end;}
    WM_DRAWITEM:
      begin
        dis := Pointer(LParam);
        Index := dis.ItemID;
        hHeader := SendMessage(TLV.Handle, LVM_GETHEADER, 0, 0);
        if dis.hwndItem = hHeader
          then
            begin
              if TLV.Columns[Index].ImageIndex <> -1
                then
                  begin
                    if (TL <> nil) and (TLV.Columns[Index].ImageIndex >= 0)
                      then
                        TL.Items[Index] := pointer(TLV.Columns[Index].ImageIndex);
                    TLV.Columns[Index].ImageIndex := -1;
                    for i := 0 to TLV.Columns.Count -1 do
                      begin
                        with Column do
                          begin
                            mask := LVCF_TEXT or LVCF_FMT or LVCF_IMAGE or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES;
                            fmt := 0;//LVCFMT_LEFT;
                            cx := TLV.Columns[i].Width;
                            pszText := '';//PChar(ListView1.Columns[0].Caption);
                            iImage := -1;
                          end;
                        ListView_SetColumn(TLV.Handle, i, Column);
                      end;
                    InvalidateRect(h, nil, true);
                  end
                else
                  TLV.Columns[Index].ImageIndex := -2;
              //if (TLV<> nil) and (dis.ItemID < TLV.Columns.Count) then
              //  TLV.Columns[dis.ItemID].ImageIndex := -1;
              dis.ItemState := CDIS_SELECTED;
              DrawHeader(TL, TLV, dis.ItemID, dis.rcItem, dis.ItemState, dis.hdc, FgColor, BkColor);
      (*        dis := Pointer(LParam);
              hdnn.nmcd.dwDrawStage := CDDS_ITEMPREPAINT;
              hdnn.nmcd.hdc := dis.hDC;
              hdnn.nmcd.rc := dis.rcItem;
              hdnn.nmcd.dwItemSpec := dis.ItemID;
              hdnn.nmcd.uItemState := dis.ItemState;
              SendMessage(h, WM_NOTIFY, 0, Integer(@hdnn));*)
              result := 1;
              exit;
            end;
      end;
{     LVM_SETITEM:
       begin
       end;}
{    LVM_SETCOLUMNWIDTH:
      begin
        InvalidateRect(h, nil, true);
        //TLV.Columns[TLV.Columns.count-1].Width := -2;
      end;}
    WM_DESTROY:
      begin
        SetWindowLong(TLV.Handle{hwnd}, GWL_WNDPROC, longint(oldproc));
        TL.Free;
      end;
{    WM_PAINT:
      begin
        hDCMem := CreateCompatibleDC(0);
        GetWindowRect(h, wRect);
        hBmp := 0;
        WDC := GetDC(h);
        hBmp := CreateCompatibleBitmap(WDC, rect.right - rect.left, rect.bottom - rect.top);
        ReleaseDC(h, WDC);
        hOld := SelectObject(hDCMem, hBmp);
        result := CallWindowProc(oldproc, h, WM_PRINT, hDCMem, PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT or PRF_OWNED);
        SelectObject(hDCMem, hOld);
        DeleteObject(hDCMem);
        exit;
      end;}
{    WM_SIZE:
      begin
{        AWidth := TLV.Width - 20;
        sum := 0;
        for i := 0 to pred(TLV.Columns.Count) do
          inc(sum, TLV.Columns[i].Width);
        for i := 0 to pred(TLV.Columns.Count) do
          if sum > 0
            then TLV.Columns[i].Width := round(TLV.Columns[i].Width*AWidth/sum)
            else TLV.Columns[i].Width := 0;
      end;}
{    WM_USER+3595:
      begin
        InvalidateRect(h, nil, true);
      end;}
{    HDM_LAYOUT:
      begin
        InvalidateRect(h, nil, true);
      end;}
    WM_NOTIFY:
      begin
        hHeader := SendMessage(TLV.Handle, LVM_GETHEADER, 0, 0);
        //InvalidateRect(h, nil, true);
{        if (hdn.nmcd.hdr.code = LVN_HOTTRACK)
          then
            begin
              TLV.Columns[TLV.Columns.count-1].Width := -2;
            end
          else
         if (hdn.nmcd.hdr.code = LVN_COLUMNCLICK)
           then
             begin
               TLV.Columns[TLV.Columns.count-1].Width := -2;
             end
          else  //////}

        if (hdn.nmcd.hdr.code = HDN_DIVIDERDBLCLICKA) or (hdn.nmcd.hdr.code = HDN_DIVIDERDBLCLICKW)
          then
            begin
              if hdn.nmcd.dwItemSpec <> TLV.Columns.count-1
                then
                  begin
                    result := CallWindowProc(oldproc, h, Msg, wParam, lParam);
                    TotalWidth := 0;
                    Style := GetWindowLong(H, GWL_STYLE);
                    if (Style and WS_VSCROLL) <> 0
                      then sum := GetSystemMetrics(SM_CXVSCROLL)
                      else sum := 0;
                    for i := 0 to TLV.Columns.count-2 do
                      begin
                        Inc(TotalWidth, TLV.Columns[i].Width);
                      end;
                    if (TotalWidth < TLV.Width-sum)
                      then
                        TLV.Columns[TLV.Columns.count-1].Width := TLV.Width - TotalWidth-sum
                      else
                        TLV.Columns[TLV.Columns.count-1].Width := TLV.Canvas.TextWidth(TLV.Columns[TLV.Columns.count-1].Caption)+sum;//24
                    InvalidateRect(h, nil, true);
                  end
                else
                  exit;
            end
          else
        if (headern.hdr.code = HDN_BEGINTRACKW) or (headern.hdr.code = HDN_BEGINTRACKA)
          then
            begin
              if hdn.nmcd.dwItemSpec = TLV.Columns.count-1
                then
                  begin
                    result := 1;
                    exit;
                  end;
            end
          else
        if (headern.hdr.code = HDN_ENDTRACKW) or (headern.hdr.code = HDN_ENDTRACKA)
          then
            begin
              result := CallWindowProc(oldproc, h, Msg, wParam, lParam);

              TotalWidth := 0;
              //if hdn.nmcd.
              Style := GetWindowLong(H, GWL_STYLE);
              if (Style and WS_VSCROLL) <> 0
                then sum := GetSystemMetrics(SM_CXVSCROLL)
                else sum := 0;
              if CoolSB_IsScrollBarVisible(H, SB_VERT)
                then sum := GetSystemMetrics(SM_CXVSCROLL);
//              if (headern.PItem.Mask and HDI_WIDTH)<>0
//                then
              for i := 0 to TLV.Columns.count-2 do
                begin
                  if (i = headern.Item) and ((headern.PItem.Mask and HDI_WIDTH)<>0)
                    then
                      Inc(TotalWidth, headern.PItem.cxy)
                    else
                      Inc(TotalWidth, TLV.Columns[i].Width);
                end;
//              if (TotalWidth < TLV.Width-sum)
//                then
                  TLV.Columns[TLV.Columns.count-1].Width := TLV.Width - TotalWidth-sum;
//                else
//                  TLV.Columns[TLV.Columns.count-1].Width := TLV.Canvas.TextWidth(TLV.Columns[TLV.Columns.count-1].Caption)+sum;//24;
              InvalidateRect(h, nil, true);
            end
          else
{        if (hdn.nmcd.hdr.code = HDN_TRACK)
          then
            begin
              InvalidateRect(h, nil, true);
            end
          else}
        if (hdn.nmcd.hdr.code = NM_CUSTOMDRAW) and (hHeader = hdn.nmcd.hdr.hwndFrom )
          then
            begin
              case hdn.nmcd.dwDrawStage of
                CDDS_PREPAINT:
                  {Msg.}Result := CDRF_NOTIFYITEMDRAW;
                CDDS_ITEMPREPAINT://, CDDS_POSTPAINT:
                  begin
  //                  TLV.Columns[0].ImageIndex := -1;
                    Index := hdn.nmcd.dwItemSpec;
                    if Index < TLV.Columns.Count
                      then
                        begin
                          if TLV.Columns[Index].ImageIndex <> -1
                            then
                              begin
                                if (TL <> nil) and (TLV.Columns[Index].ImageIndex >= 0)
                                  then
                                    TL.Items[Index] := pointer(TLV.Columns[Index].ImageIndex);
                                TLV.Columns[Index].ImageIndex := -1;
                                for i := 0 to TLV.Columns.Count -1 do
                                  begin
                                    with Column do
                                      begin
                                        mask := LVCF_TEXT or LVCF_FMT or LVCF_IMAGE or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES;
                                        fmt := 0;//LVCFMT_LEFT;
                                        cx := TLV.Columns[i].Width;
                                        pszText := '';//PChar(ListView1.Columns[0].Caption);
                                        iImage := -1;
                                      end;
                                    ListView_SetColumn(TLV.Handle, i, Column);
                                  end;
                              end;
                            //else TLV.Columns[Index].ImageIndex := -2; never activate this line, endless loop
                        end;
                    rect := hdn.nmcd.rc;
                    State := hdn.nmcd.uItemState;
                    DrawHeader(TL, TLV, Index, Rect, State, hdn.nmcd.hdc, FgColor, BkColor);

  (*                  hdn.clrText := clRed;
                    hdn.clrTextBk := clYellow;
                    {Msg.}Result := CDRF_NEWFONT;
                    HeaderCanvas := TCanvas.Create;
                    HeaderCanvas.Handle := hdn.nmcd.hdc;
                    State := hdn.nmcd.uItemState;
                    text := TLV.Columns.Items[Index].Caption;
                    HeaderCanvas.Font.Color := FgColor;
                    //HeaderCanvas.Font.Name := 'Tahoma';
                    //HeaderCanvas.Font.Style := TFontStyles() << fsBold;
                    HeaderCanvas.Brush.Color := BkColor;//clBlue;
                    HeaderCanvas.Pen.Width := 3;
                    HeaderCanvas.Pen.Color := clBlack;
                    // if depressed
                    {if (State and CDIS_SELECTED <> 0)
                      then
                        Frame3D(HeaderCanvas, Rect, clBtnShadow,
                                clBtnHighlight, 2)
                      else
                        Frame3D(HeaderCanvas, Rect, clBtnHighlight,
                                clBtnShadow, 2);}
                    Rect.Bottom := Rect.Bottom-1;
                    HeaderCanvas.Rectangle(Rect);//FillRect(Rect);
                    if TLV.Columns.Items[Index].ImageIndex <> -1
                      then
                        begin
                          if assigned(TLV.SmallImages)
                            then
                              TLV.SmallImages.Draw(HeaderCanvas, Rect.Left, Rect.Top, TLV.Columns.Items[Index].ImageIndex, true);
                          HeaderCanvas.TextRect(Rect, Rect.Left + 17, Rect.Top,
                                                 text);
                        end
                      else
                        HeaderCanvas.TextRect(Rect, Rect.Left + 6, Rect.Top+2,
                                           text);
                    HeaderCanvas.Handle := 0;
                    HeaderCanvas.Free;*)

                    // tell Windows the item has already been drawn
{                    if (hdn.nmcd.dwDrawStage = CDDS_ITEMPREPAINT) and (TLV.Columns.Items[Index].ImageIndex <> -1)
                      then
                        Result := CDRF_NOTIFYPOSTPAINT
                      else
                        if (hdn.nmcd.dwDrawStage <> CDDS_POSTPAINT)
                          then}
                            Result := CDRF_SKIPDEFAULT;

                  end;
              end;
              exit;
            end;
          end;
  end;
  result := CallWindowProc(oldproc, h, Msg, wParam, lParam);
end;

procedure CustomizeListViewHeader( ListView: TListView; HeaderBk, HeaderFg, FirstBk, Firstfg, SecndBk, SecndFg, HighLC: TColor);
var
  oldproc : pointer;
  TL  : TList;
  i : integer;
  hHeader, style : integer;
  dw : Cardinal;
begin
  ListView.OnCustomDrawItem := DummyO.ListViewCustomDrawItem;
//  ListView.OnCustomDrawSubItem := DummyO.ListViewCustomDrawSubItem;
//  ListView.OnDrawItem := DummyO.ListViewDrawItem;
  TL := TList.Create;
  for i := 0 to ListView.Columns.Count-1 do
    begin
      TL.Add( pointer(ListView.Columns[i].ImageIndex) );
    end;
  SetProp( ListView.Handle, imgidxprop, cardinal(TL));
  SetProp( ListView.Handle, listviewinstance, cardinal(ListView));
  SetProp( ListView.Handle, headerbkprop, cardinal(HeaderBk) );
  SetProp( ListView.Handle, headerfgprop, cardinal(HeaderFg) );
  SetProp( ListView.Handle, firstbkprop, cardinal(FirstBk) );
  SetProp( ListView.Handle, firstfgprop, cardinal(FirstFg) );
  SetProp( ListView.Handle, secndbkprop, cardinal(SecndBk) );
  SetProp( ListView.Handle, secndfgprop, cardinal(secndFg) );
  SetProp( ListView.Handle, highlcprop, cardinal(HighLC) );
  ListView.Color := FirstBk;
  hHeader := SendMessage(ListView.Handle, LVM_GETHEADER, 0, 0);
  style := GetWindowLong(hHeader, GWL_STYLE);
//  SetWindowPos(ListView.handle, ListView.Parent.handle, 0, 0, 0, 0, SWP_NOZORDER Or
//                                 SWP_NOSIZE Or
//                                 SWP_NOMOVE );
  //modify the style by toggling the HDS_BUTTONS style
  //style := style and (not HDS_BUTTONS);
  //style := style or $200; HDS_FLAT version 6 Comctl32.dll
  style := style and (not HDS_FULLDRAG);
  SetWindowLong(hHeader, GWL_STYLE, style);
  oldproc := Pointer(SetWindowLong(hHeader{hwnd}, GWL_WNDPROC, longint(@ListViewHeaderProc)) );
  SetProp( hHeader, headerprocprop, cardinal(oldproc));
  Style := GetWindowLong(ListView.Handle, GWL_STYLE);
  style := style and (not WS_HSCROLL);
  oldproc := Pointer(SetWindowLong(ListView.Handle{hwnd}, GWL_WNDPROC, longint(@ListViewProc)) );
  SetProp( ListView.Handle, listviewprocprop, cardinal(oldproc));
end;

const
  MAX_COOLSB_BUTS = 16;

type
  PNMCSBCUSTOMDRAW = ^NMCSBCUSTOMDRAW;
  NMCSBCUSTOMDRAW= record
    hdr : NMHDR;
    dwDrawStage : DWORD;
    hdc : HDC;
    rect : TRECT;
    uItem : UINT;
    uState : UINT;
    nBar : UINT;
  end;

  PSCROLLBUT = ^SCROLLBUT;
  SCROLLBUT = record
    fMask : UINT;			//which members are in use
    uPlacement : UINT;		//is this button to the left/right (above/below) of the scrollbar??
    uCmdId : UINT;			//command identifier (WM_COMMAND value to send)
    uButType : UINT;		//
    uState : UINT;			//toggled etc
    nSize : integer;			//size in pixels. -1 for autosize

    hBmp : HBITMAP;			//handle to a bitmap to use as the button face
    hEmf :HENHMETAFILE;			//handle to an enhanced metafile

    hCurs : HCURSOR;			//handle to a user-supplied mouse cursor to apply
                    //to this button

    nSizeReserved : integer				;	//internal variable used for resizing
    nMinSize : integer				;		//min size
    nMaxSize : integer				;		//max size
  end;

  PSCROLLBAR = ^SCROLLBAR;
  SCROLLBAR = record
    fScrollFlags : UINT;
    fScrollVisible : BOOL;		//if this scrollbar visible?
    scrollInfo : SCROLLINFO;			//positional data (range, position, page size etc)

    nArrowLength : integer;		//perpendicular size (height of a horizontal, width of a vertical)
    nArrowWidth : integer;		//parallel size (width of horz, height of vert)

    //data for inserted buttons
    sbButtons : array [0..MAX_COOLSB_BUTS-1] of SCROLLBUT;
    nButtons : integer			;
    nButSizeBefore : integer			;		//size to the left / above the bar
    nButSizeAfter : integer			;		//size to the right / below the bar

    fButVisibleBefore : BOOL		;	//if the buttons to the left are visible
    fButVisibleAfter : BOOL		;	//if the buttons to the right are visible

    nBarType : integer			;			//SB_HORZ / SB_VERT

    fFlatScrollbar : UINT		;		//do we display flat scrollbars?
    nMinThumbSize : integer			;
  end;

//
//	Container structure for a cool scrollbar window.
//
PSCROLLWND = ^SCROLLWND;
SCROLLWND = record
	bars : UINT ;				//which of the scrollbars do we handle? SB_VERT / SB_HORZ / SB_BOTH
	oldproc : pointer ;//WNDPROC ;		//old window procedure to call for every message

	sbarHorz : SCROLLBAR ;		//one scrollbar structure each for
	sbarVert : SCROLLBAR ;		//the horizontal and vertical scrollbars

	fThumbTracking : BOOL ;	// are we currently thumb-tracking??
	fLeftScrollbar : BOOL ;	// support the WS_EX_LEFTSCROLLBAR style

	hwndToolTip : HWND ;		// tooltip support!!!

	//size of the window borders
	cxLeftEdge, cxRightEdge : integer ;
	cyTopEdge,  cyBottomEdge : integer ;

	// To prevent calling original WindowProc in response
	// to our own temporary style change (fixes TreeView problem)
	bPreventStyleChange : BOOL ;
end;


function CoolSB_IsScrollBarVisible(hwnd: HWND; wBar: integer): boolean; stdcall; external;
function CoolSBEnableBar( hwnd: HWND; bEnableHorzBar: boolean{=FALSE}; bEnableVertBar: boolean{=TRUE}) : boolean; stdcall; external;
function CoolSBAutoHideBar( hwnd: HWND; bAutoHideHorzBar: boolean; bAutoHideVertBar: boolean): boolean; stdcall; external;
function InitializeCoolSB( hwnd: HWND) : boolean; stdcall; external;
function UninitializeCoolSB( hwnd: HWND) : HRESULT; stdcall; external;
function InitSkin( SkinFile: PChar): boolean; stdcall; external;
function HandleCustomDraw(ctrlid: integer; nm : PNMCSBCUSTOMDRAW) : LRESULT; stdcall; external;
function GetScrollWndFromHwnd(hwnd: HWND ): PSCROLLWND; stdcall; external;
//procedure CoolSB_ZeroMemory( ptr: pointer; bytes: DWORD); stdcall; external;
procedure SetupScrollbars(hwnd : HWND); stdcall; external;
function CoolSB_GetScrollInfo{ (HWND hwnd, int fnBar, LPSCROLLINFO lpsi)}: boolean; stdcall; external;
function CoolSB_SetScrollPos : boolean ; stdcall; external;
function CoolSB_SetScrollInfo{ (HWND hwnd, int fnBar, LPSCROLLINFO lpsi)}: boolean; stdcall; external;




initialization
  DummyO := TDummyObject.Create;
  hThemeLib := 0;
  if (Win32Platform  = VER_PLATFORM_WIN32_NT) and
     (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or
     (Win32MajorVersion > 5))
    then
      begin
        hThemeLib := LoadLibrary(themelib);
        if hThemeLib <> 0
          then
            begin
              IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');
              SetWindowTheme := GetProcAddress(hThemeLib, 'SetWindowTheme');
            end;
      end;
finalization
  DummyO.Free;
  if hThemeLib <> 0
    then
      begin
        FreeLibrary(hThemeLib);
        hThemeLib := 0;
      end;
end.



















