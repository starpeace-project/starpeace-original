unit ColoredGauge;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TColorGauge =
    class(TCustomControl)
      private
        fPosition   : integer;
        fSpacing    : integer;
        fYellowPerc : integer;
        fRedPerc    : integer;
        //fBuffer     : TBitmap;
      protected
        procedure Paint; override;
      private
        procedure SetPosition  ( Value : integer );
        procedure SetSpacing   ( Value : integer );
        procedure SetYellowPerc( Value : integer );
        procedure SetRedPerc   ( Value : integer );
      published
        property Position   : integer read fPosition   write SetPosition;
        property Spacing    : integer read fSpacing    write SetSpacing;
        property YellowPerc : integer read fYellowPerc write SetYellowPerc;
        property RedPerc    : integer read fRedPerc    write SetRedPerc;
    private
      procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    end;

procedure Register;

implementation

  procedure TColorGauge.Paint;
    var
      i   : integer;
      sp  : integer;
      col : TColor;
    begin
      sp := 0;
      Canvas.Pen.Style   := psClear;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clBlack;
      col := clBlack;
      for i := 0 to pred(Width) do
        begin
          if sp > 0
            then
              begin
                dec( sp );
                if sp = 0
                  then Canvas.Brush.Color := col;
              end;
          Canvas.Rectangle( i, 0, i + 2, Height + 1);
          if i mod fSpacing = 0
            then
              begin
                if (100*i) div Width < fYellowPerc
                  then
                    if (100*i) div Width < Position
                      then col := clLime
                      else col := clGreen
                  else
                    if (100*i) div Width < fRedPerc
                      then
                        if (100*i) div Width < Position
                          then col := clYellow
                          else col := clOlive
                      else
                        if (100*i) div Width < Position
                          then col := clRed
                          else col := clMaroon;
                sp := 4;
                Canvas.Brush.Color := clBlack;
              end;
        end;
    end;

  procedure TColorGauge.SetPosition( Value : integer );
    begin
      if fPosition <> Value
        then
          begin
            fPosition := Value;
            Invalidate;
          end;
    end;

  procedure TColorGauge.SetSpacing( Value : integer );
    begin
      fSpacing := Value;
      Invalidate;
    end;

  procedure TColorGauge.SetYellowPerc( Value : integer );
    begin
      fYellowPerc := Value;
      Invalidate;
    end;

  procedure TColorGauge.SetRedPerc( Value : integer );
    begin
      fRedPerc := Value;
      Invalidate;
    end;

  procedure TColorGauge.WMEraseBkgnd(var Message: TMessage); 
    begin
      Message.Result := 1;
    end;
    
  procedure Register;
    begin
      RegisterComponents('Five', [TColorGauge]);
    end;

end.

