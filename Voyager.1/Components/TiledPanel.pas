unit TiledPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

  type
    TTiledPanel =
      class(TPanel)
        public
          constructor Create(AOwner : TComponent); override;
          destructor Destroy;                      override;
        private
          fTileImage : TBitMap;
        protected
          procedure Paint; override;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
        private
          function GetTileImage : TBitmap;
          procedure SetTileImage(aImage : TBitmap);
        published
          property TileImage : TBitmap read GetTileImage write SetTileImage;
      end;

  procedure Register;

implementation

  // TTiledPanel

  constructor TTiledPanel.Create(AOwner : TComponent);
    begin
      inherited;
      fTileImage := TBitMap.Create;
    end;

  destructor TTiledPanel.Destroy;
    begin
      fTileImage.Free;
      inherited;
    end;

  procedure TTiledPanel.Paint;
    const
      Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
    var
      xPixels    : word;
      yPixels    : word;
      Rect       : TRect;
      FontHeight : integer;
      Buffer     : TBitmap;
      tmpCanvas  : TCanvas;
    begin
      if Caption <> ''
        then
          begin
            Buffer := TBitmap.Create;
            Buffer.Width  := Width;
            Buffer.Height := Height;
            tmpCanvas := Buffer.Canvas;
          end
        else
          begin
            Buffer    := nil;
            tmpCanvas := Canvas;
          end;
      if (fTileImage.Width > 0) and (fTileImage.Height > 0)
        then
          begin
            yPixels := 0;
            repeat
              xPixels := 0;
              repeat
                tmpCanvas.Draw(xPixels, yPixels, fTileImage);
                inc(xPixels, fTileImage.Width);
              until xPixels >= Width;
              inc(yPixels, fTileImage.Height);
            until yPixels >= Height
          end;
      Rect := GetClientRect;
      inc( Rect.Left, BorderWidth );
      dec( Rect.Right, BorderWidth );
      with tmpCanvas do
        begin
          Brush.Color := Color;
          Brush.Style := bsClear;
          Font        := Self.Font;
          FontHeight  := TextHeight('W');
          with Rect do
            begin
              Top    := ((Bottom + Top) - FontHeight) div 2;
              Bottom := Top + FontHeight;
            end;
          DrawText(Handle, PChar(Caption), -1, Rect, (DT_EXPANDTABS or
            DT_VCENTER) or Alignments[Alignment]);
        end;
      if Buffer <> nil
        then
          begin
            Rect := Bounds( 0, 0, Width, Height );
            Canvas.CopyRect( Rect, tmpCanvas, Rect );
            Buffer.Free;
          end;
    end;

  procedure TTiledPanel.WMEraseBkgnd(var Message: TMessage); 
    begin
      Message.Result := 1;
    end;
    
  function TTiledPanel.GetTileImage : TBitmap;
    begin
      result := fTileImage;
    end;

  procedure TTiledPanel.SetTileImage(aImage : TBitmap);
    begin
      fTileImage.Assign(aImage);
    end;


  // Register component

  procedure Register;
    begin
      RegisterComponents('Five', [TTiledPanel]);
    end;

end.
