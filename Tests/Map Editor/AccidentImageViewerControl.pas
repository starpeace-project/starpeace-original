unit AccidentImageViewerControl;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    GameTypes;

  type
    TAccidentImageViewerControl =
      class(TGraphicControl)
        public
          constructor Create(Owner : TComponent); override;
          destructor  Destroy; override;
        private
          fVClass : integer;
          fBuffer : TCanvasImage;
          procedure SetGameImage(Img : TGameImage);
        private
          fOnClick : TNotifyEvent;
        protected
          procedure Paint; override;
        protected
          fMouseDownTicks : integer;
          procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); override;
          procedure MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer); override;
          procedure MouseClicked;
        published
          property Image   : TGameImage   write SetGameImage;
          property VClass  : integer      read  fVClass write fVClass;
        published
          property OnClick : TNotifyEvent write fOnClick;
      end;

implementation

  const
    cCanvasBitCount = 16;

  constructor TAccidentImageViewerControl.Create(Owner : TComponent);
    begin
      inherited Create(Owner);
      fBuffer := TCanvasImage.CreateSized(1, 1, cCanvasBitCount);
    end;

  destructor TAccidentImageViewerControl.Destroy;
    begin
      fBuffer.Free;
      inherited;
    end;

  procedure TAccidentImageViewerControl.SetGameImage(Img : TGameImage);
    begin
      if (fBuffer.Width <> Img.Width) or (fBuffer.Height <> Img.Width)
        then fBuffer.NewSize(Img.Width, Img.Height, cCanvasBitCount);
      Img.Draw(0, 0, 0, 0, Rect(0, 0, Img.Width, Img.Height), fBuffer, nil);
    end;

  procedure TAccidentImageViewerControl.Paint;
    begin
      inherited;
      if fBuffer <> nil
        then fBuffer.StretchDraw(Canvas, ClientRect);
    end;

  procedure TAccidentImageViewerControl.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
    begin
      inherited;
      if button = mbLeft
        then fMouseDownTicks := GetTickCount;
    end;

  procedure TAccidentImageViewerControl.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer);
    begin
      inherited;
      if (button = mbLeft) and (GetTickCount - fMouseDownTicks < 100)
        then MouseClicked;
    end;

  procedure TAccidentImageViewerControl.MouseClicked;
    begin
      if assigned(fOnClick)
        then fOnClick(Self);
    end;

end.

