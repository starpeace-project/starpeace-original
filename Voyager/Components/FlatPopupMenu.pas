unit FlatPopupMenu;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Menus;

  type
    TFlatPopupMenu = class(TPopupMenu)
      procedure DrawMenu(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
      procedure MeasureMenu(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
      procedure MenuPopup(Sender: TObject);
    private
      { Private declarations }
      FExtraWidth : integer;
      FMenuColor  : TColor;
      FItemSelectedColor : TColor;
      FMenuBorderColor : TColor;
      FTextColor : TColor;
    protected
      { Protected declarations }
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
    published
      { Published declarations }
      property ExtraWidth : integer read FExtraWidth write FExtraWidth;
      property MenuColor : TColor read FMenuColor write FMenuColor;
      property ItemSelectedColor : TColor read FItemSelectedColor write FItemSelectedColor;
      property MenuBorderColor : TColor read FMenuBorderColor write FMenuBorderColor;
      property TextColor : TColor read FTextColor write FTextColor;
    end;

  procedure Register;

implementation

  procedure Register;
    begin
      RegisterComponents('Voyager', [TFlatPopupMenu]);
    end;

  { TFlatPopupMenu }

  constructor TFlatPopupMenu.Create(AOwner: TComponent);
    begin
      inherited Create(AOwner);
      MenuAnimation := [maNone];
      AutoPopup := true;
      OwnerDraw := true;
      OnPopup := MenuPopup;
      MenuColor := RGB(240, 240, 240);
      ItemSelectedColor := RGB(110, 131, 184);
      MenuBorderColor := RGB(240, 240, 240);
    end;

  procedure TFlatPopupMenu.DrawMenu(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    var
      cTemp:  TCanvas;
      sText:  String;
      mWnd:   HWND;
      rMenu:  TRect;
    begin
      sText := TMenuItem(Sender).Caption;
      with ACanvas do
        begin
          // Clear
          if odSelected in State
            then
              begin
                Brush.Color := FItemSelectedColor;//RGB(110, 131, 184);
                Pen.Color   := RGB(47, 60, 93);
              end
            else
              begin
                Brush.Color := FMenuColor;//RGB(240, 240, 240);
                Pen.Color   := FMenuColor;//RGB(240, 240, 240);
              end;
          Rectangle(ARect);
          if sText = '-'
            then
              begin
                // Draw line
                ACanvas.Pen.Color := RGB(0, 0, 0);
                MoveTo(ARect.Left, ARect.Top + ((ARect.Bottom - ARect.Top) div 2));
                LineTo(ARect.Right, ARect.Top + ((ARect.Bottom - ARect.Top) div 2));
              end
            else
              begin
                // Draw text
                ACanvas.Font.Color := FTextColor;
                Inc(ARect.Left, 12);
                DrawText(Handle, PChar(sText), Length(sText), ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
              end;
        end;
      // menu border
      mWnd := WindowFromDC(ACanvas.Handle);
      //SendMessage(mWnd, WM_NCPAINT, 1, 0);
      if mWnd <> Self.Handle
        then
          begin
            cTemp := TCanvas.Create();
            cTemp.Handle := GetDC(0);
            Windows.GetWindowRect(mWnd, rMenu);
            cTemp.Brush.Color := RGB(20, 20, 20);
            cTemp.FrameRect(rMenu);
            InflateRect(rMenu, -1, -1);
            cTemp.Brush.Color := FMenuBorderColor;//RGB(240, 240, 240);
            cTemp.FrameRect(rMenu);
            InflateRect(rMenu, -1, -1);
            cTemp.FrameRect(rMenu);
            ReleaseDC(0, cTemp.Handle);
            cTemp.Free();
          end;
    end;

  procedure TFlatPopupMenu.MeasureMenu(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    begin
      Inc(Width, 25);
    end;

  procedure TFlatPopupMenu.MenuPopup(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to Items.Count-1 do
        begin
          TMenuItem(Items[i]).OnMeasureItem := MeasureMenu;
          TMenuItem(Items[i]).OnAdvancedDrawItem := DrawMenu;
        end;
    end;

end.
