unit MultiBMPButton;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls;

  type
    TButtonState = (bsNormal, bsLighted, bsPushed, bsDisabled);

  type
    TMultiBMPButton =
      class(TGraphicControl)
        public
          constructor Create(AOwner: TComponent); override;
        private
          fCurState      : TButtonState;
          fNormalImage   : TBitMap;
          fLightedImage  : TBitMap;
          fPushedImage   : TBitMap;
          fDisabledImage : TBitMap;
        private
          procedure SetState(aState : TButtonState);
        public
          property State : TButtonState read fCurState write SetState;
        protected
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
          procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        private
          function GetCurrentImage : TBitMap;
        protected
          procedure Paint; override;
        private
          function  GetNormalImage : TBitMap;
          function  GetLightedImage : TBitMap;
          function  GetPushedImage : TBitMap;
          function  GetDisabledImage : TBitMap;
          procedure SetNormalImage(Image : TBitMap);
          procedure SetLightedImage(Image : TBitMap);
          procedure SetPushedImage(Image : TBitMap);
          procedure SetDisabledImage(Image : TBitMap);
        published
          property NormalImage   : TBitMap read GetNormalImage   write SetNormalImage;
          property LightedImage  : TBitMap read GetLightedImage  write SetLightedImage;
          property PushedImage   : TBitMap read GetPushedImage   write SetPushedImage;
          property DisabledImage : TBitMap read GetDisabledImage write SetDisabledImage;
        published
          property Caption;
          property DragCursor;
          property DragMode;
          property Enabled;
          property Font;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          //property TabOrder;
          //property TabStop;
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

  procedure Register;

implementation


  // TMultiBMPButton

  constructor TMultiBMPButton.Create(AOwner: TComponent);
    begin
      inherited;
      fNormalImage   := TBitMap.Create;
      fLightedImage  := TBitMap.Create;
      fPushedImage   := TBitMap.Create;
      fDisabledImage := TBitMap.Create;
      fCurState      := bsNormal;
      Width          := 64;
      Height         := 64;
    end;

  procedure TMultiBMPButton.SetState(aState : TButtonState);
    begin
      if aState <> fCurState
        then
          begin
            fCurState := aState;
            Repaint;
          end;
    end;

  procedure TMultiBMPButton.CMMouseEnter(var Message: TMessage);
    begin
      if Enabled
        then State := bsLighted;
    end;

  procedure TMultiBMPButton.CMMouseLeave(var Message: TMessage);
    begin
      if Enabled
        then State := bsNormal;
    end;

  procedure TMultiBMPButton.CMEnabledChanged(var Message: TMessage);
    begin
      inherited;
      if Enabled
        then State := bsNormal
        else State := bsDisabled;
    end;

  procedure TMultiBMPButton.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TMultiBMPButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if Enabled
        then State := bsPushed;
    end;

  procedure TMultiBMPButton.MouseMove(Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
    end;

  procedure TMultiBMPButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      if Enabled and (fCurState = bsPushed)
        then State := bsLighted;
    end;

  function TMultiBMPButton.GetCurrentImage : TBitMap;
    begin
      case fCurState of
        bsNormal :
          result := fNormalImage;
        bsLighted :
          result := fLightedImage;
        bsPushed :
          result := fPushedImage;
        bsDisabled :
          result := fDisabledImage;
        else result := nil;
      end;
    end;

  procedure TMultiBMPButton.Paint;
    var
      Image : TBitMap;
      xSize : word;
      ySize : word;
    begin
      Image := GetCurrentImage;
      xSize := Image.Width;
      ySize := Image.Height;
      if (xSize > 0) and (ySize > 0)
        then
          begin
            Width  := xSize;
            Height := ySize;
            Canvas.CopyRect(Canvas.ClipRect, Image.Canvas, Image.Canvas.ClipRect)
          end;
    end;

  function TMultiBMPButton.GetNormalImage : TBitMap;
    begin
      result := fNormalImage;
    end;

  function TMultiBMPButton.GetLightedImage : TBitMap;
    begin
      result := fLightedImage;
    end;

  function TMultiBMPButton.GetPushedImage : TBitMap;
    begin
      result := fPushedImage;
    end;

  function TMultiBMPButton.GetDisabledImage : TBitMap;
    begin
      result := fDisabledImage;
    end;

  procedure TMultiBMPButton.SetNormalImage(Image : TBitMap);
    begin
      fNormalImage.Assign(Image);
      Invalidate;
    end;

  procedure TMultiBMPButton.SetLightedImage(Image : TBitMap);
    begin
      fLightedImage.Assign(Image);
      Invalidate;
    end;

  procedure TMultiBMPButton.SetPushedImage(Image : TBitMap);
    begin
      fPushedImage.Assign(Image);
      Invalidate;
    end;

  procedure TMultiBMPButton.SetDisabledImage(Image : TBitMap);
    begin
      fDisabledImage.Assign(Image);
      Invalidate;
    end;

  // Register component

  procedure Register;
    begin
      RegisterComponents('Five', [TMultiBMPButton]);
    end;

end.
