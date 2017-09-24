unit FiveControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  GameTypes, GameControl, LocalCacheManager, MapTypes, Map, LanderTypes;

const
  cMaxZoomLevel = ord(high(TZoomRes));
  cMinZoomLevel = ord(low(TZoomRes));

type
  TFiveControl =
    class(TGameControl)
      public
        constructor Create(anOwner : TComponent); override;
        destructor  Destroy; override;
      public
        procedure ZoomIn;
        procedure ZoomOut;
        procedure MoveTo(i, j : integer);
    end;


procedure Register;


implementation

// TFiveControl

constructor TFiveControl.Create( anOwner : TComponent );
  begin
    inherited;
    ZoomLevel := ord(zr32x64);
  end;

destructor TFiveControl.Destroy;
  begin
    inherited;
  end;

procedure TFiveControl.ZoomIn;
  var
    msg : TViewZoomedMsg;
  begin
    if (TZoomRes(ZoomLevel) < high(TZoomRes)) and (fFocus <> nil)
      then
        begin
          msg.id := msgViewZoomed;
          msg.Zoom := ZoomLevel + 1;
          fFocus.Dispatch(msg);
        end;
  end;

procedure TFiveControl.ZoomOut;
  var
    msg : TViewZoomedMsg;
  begin
    if (TZoomRes(ZoomLevel) > low(TZoomRes)) and (fFocus <> nil)
      then
        begin
          msg.id := msgViewZoomed;
          msg.Zoom := ZoomLevel - 1;
          fFocus.Dispatch(msg);
        end;
  end;

procedure TFiveControl.MoveTo(i, j : integer);
  var
    msg : TMoveToMsg;
  begin
    msg.id := msgMoveTo;
    msg.i  := i;
    msg.j  := j;
    Focus.Dispatch(msg);
  end;

procedure Register;
  begin
    RegisterComponents('Games', [TFiveControl]);
  end;

end.
