unit GradientBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  type
    TGradientDirection = (gdHorizontal, gdVertical);

  type
    TGradientBox =
      class(TGraphicControl)
        public
          constructor Create(AOwner : TComponent); override;
          destructor  Destroy; override;
        protected
          fFromColor  : TColor;
          fToColor    : TColor;
          fDirection  : TGradientDirection;
          fBitmap     : TBitMap;
        private
          procedure ReDrawBmp;
        protected
          procedure Paint; override;
          procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
        private
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
        private
          procedure SetDirection(aDirection : TGradientDirection);
          procedure SetFromColor(aColor : TColor);
          procedure SetToColor(aColor : TColor);
        published
          property  Direction : TGradientDirection read fDirection write SetDirection;
          property  FromColor : TColor  read fFromColor write SetFromColor;
          property  ToColor   : TColor  read fToColor   write SetToColor;
          property  Align;
          property  Text;
          property  DragCursor;
          property  DragMode;
          property  Enabled;
          property  Font;
          property  ParentFont;
          property  ParentShowHint;
          property  PopupMenu;
          property  ShowHint;
          //property TabOrder;
          //property TabStop;
          property  Visible;
          property  OnClick;
          property  OnDragDrop;
          property  OnDragOver;
          property  OnEndDrag;
          property  OnMouseDown;
          property  OnMouseMove;
          property  OnMouseUp;
          property  OnStartDrag;
      end;

  procedure Register;

implementation

  uses
    GradientUtils;

  constructor TGradientBox.Create(AOwner : TComponent);
    begin
      inherited;
      fBitmap    := TBitmap.Create;
      fFromColor := clWhite;
      fToColor   := clBlack;
    end;

  destructor TGradientBox.Destroy;
    begin
      fBitmap.Free;
      inherited;
    end;

  procedure TGradientBox.ReDrawBmp;
    begin
      GradientUtils.Gradient(fBitmap.Canvas, fFromColor, fToColor, fDirection = gdHorizontal, ClientRect);
    end;

  procedure TGradientBox.Paint;
    begin
      Canvas.CopyRect(ClientRect, fBitmap.Canvas, ClientRect);
    end;

  procedure TGradientBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    begin
      inherited;
      fBitmap.Width := Width;
      fBitmap.Height := Height;
      ReDrawBmp;
    end;

  procedure TGradientBox.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TGradientBox.SetDirection(aDirection : TGradientDirection);
    begin
      fDirection := aDirection;
      ReDrawBmp;
      Invalidate;
    end;

  procedure TGradientBox.SetFromColor(aColor : TColor);
    begin
      fFromColor := ColorToRGB(aColor);
      ReDrawBmp;
      Invalidate;
    end;

  procedure TGradientBox.SetToColor(aColor : TColor);
    begin
      fToColor := ColorToRGB(aColor);
      ReDrawBmp;
      Invalidate;
    end;


  procedure Register;
    begin
      RegisterComponents('Five', [TGradientBox]);
    end;

end.
