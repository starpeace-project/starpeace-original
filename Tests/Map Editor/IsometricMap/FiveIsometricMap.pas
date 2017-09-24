unit FiveIsometricMap;

interface

uses
  Classes, IsometricMap, IsometricMapTypes, FocusTypes;

type
  TZoomLevel = IsometricMap.TZoomLevel;

const
  cDefaultZoomDelta = 10;

type
  TFiveIsometricMap =
    class(TIsometricMap)
      public
        constructor Create(Owner : TComponent);   override;
      private
        fZoomDelta : integer;
      published
        property ZoomDelta : integer read fZoomDelta write fZoomDelta default cDefaultZoomDelta;
      public
        procedure ZoomIn;
        procedure ZoomOut;
        procedure ZoomMax;
        procedure ZoomMin;
      private
        procedure ViewRegionUpdated(var msg);   message verbViewRegionUpdated;
    end;


implementation


// TFiveIsometricMap

constructor TFiveIsometricMap.Create(Owner : TComponent);
  begin
    inherited;
    Width  := 202;
    Height := 102;
    fZoomDelta := cDefaultZoomdelta;
  end;

procedure TFiveIsometricMap.ZoomIn;
  var
    zoom : TZoomLevel;
  begin
    zoom := ZoomLevel;
    if zoom < high(zoom) - fZoomDelta
      then ZoomLevel := zoom + fZoomDelta
      else ZoomLevel := high(zoom);
  end;

procedure TFiveIsometricMap.ZoomOut;
  var
    zoom : TZoomLevel;
  begin
    zoom := ZoomLevel;
    if zoom > low(zoom) + fZoomDelta
      then ZoomLevel := zoom - fZoomDelta
      else ZoomLevel := low(zoom);
  end;

procedure TFiveIsometricMap.ZoomMax;
  begin
    ZoomLevel := high(ZoomLevel);
  end;

procedure TFiveIsometricMap.ZoomMin;
  begin
    ZoomLevel := low(ZoomLevel);
  end;
  
procedure TFiveIsometricMap.ViewRegionUpdated(var msg);
  begin
    InvalidateIsometric(true);
  end;


end.
