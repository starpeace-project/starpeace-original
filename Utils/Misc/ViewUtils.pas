unit ViewUtils;

interface

  uses
    Windows;

  const
    // Stretch Mode internal flags
    smiOperationMask    = $FF00;
    smiOperationFit     = $4000;
    smiOperationStretch = $8000;
    smiOperationZoom    = $0000;
    smiOperationShrink  = $0080;
    smiMaxAllowedZoom   = $007F;

    // Don't go mad, just use these constants & functions:  -------------------------------------
    // Examples: StretchSize( smFitClientArea, OriginalSize, Area );
    //           StretchSize( smZoomBy(2), OriginalSize, Area );

    // Fit
    smFitClientArea     = smiOperationFit + $00;
    smFitWidth          = smiOperationFit + $01;
    smFitHeight         = smiOperationFit + $02;

    // Stretch
    smStretchClientArea = smiOperationStretch + $00;
    smStretchWidth      = smiOperationStretch + $01;
    smStretchHeight     = smiOperationStretch + $02;

    // Zoom & Shrink
    function smZoomBy( Times : integer ) : integer;
    function smShrinkBy( Times : integer ) : integer;

  function StretchSize( StretchMode : integer; OriginalSize : TPoint; const Area : TRect ) : TPoint;

  procedure Center( const Size : TPoint; const Area : TRect; var Rect : TRect );

implementation

  uses
    Rects;

  function smZoomBy( Times : integer ) : integer;
    begin
      assert( Times <= smiMaxAllowedZoom, 'Invalid zoom requested in ViewUtils.smZoomBy' );

      Result := smiOperationZoom + Times;
    end;

  function smShrinkBy( Times : integer ) : integer;
    begin
      assert( Times <= smiMaxAllowedZoom, 'Invalid zoom requested in ViewUtils.smShrinkBy' );
      
      Result := smiOperationShrink + Times;
    end;

  function StretchSize( StretchMode : integer; OriginalSize : TPoint; const Area : TRect ) : TPoint;
    var
      ClientSize : TPoint;
    begin
      ClientSize := RectSize( Area );
      if StretchMode and smiOperationMask = smiOperationZoom
        then // Do Zoom/Shrink
          begin
            if StretchMode and smiOperationShrink <> 0
              then
                begin
                  Result.X := OriginalSize.X  div ( StretchMode and smiMaxAllowedZoom );
                  Result.Y := OriginalSize.Y  div ( StretchMode and smiMaxAllowedZoom );
                end
              else
                begin
                  Result.X := OriginalSize.X  * ( StretchMode and smiMaxAllowedZoom );
                  Result.Y := OriginalSize.Y  * ( StretchMode and smiMaxAllowedZoom );
                end;
          end
        else
          case StretchMode of
            smFitWidth :
              begin
                Result.X := ClientSize.X;
                Result.Y := (OriginalSize.Y * ClientSize.X div OriginalSize.X)
              end;
            smFitHeight :
              begin
                Result.X := OriginalSize.X * ClientSize.Y div OriginalSize.Y;
                Result.Y := ClientSize.Y;
              end;
            smFitClientArea :
              if ClientSize.X * OriginalSize.Y < ClientSize.Y * OriginalSize.X
                then
                  begin
                    Result.X := ClientSize.X;
                    Result.Y := OriginalSize.Y * ClientSize.X div OriginalSize.X;
                  end
                else
                  begin
                    Result.X := OriginalSize.X * ClientSize.Y div OriginalSize.Y;
                    Result.Y := ClientSize.Y;
                  end;
            smStretchWidth :
              begin
                Result.X := ClientSize.X;
                Result.Y := OriginalSize.Y;
              end;
            smStretchHeight :
              begin
                Result.X  := OriginalSize.X;
                Result.Y := ClientSize.Y;
              end;
            smStretchClientArea :
              begin
                Result.X := ClientSize.X;
                Result.Y := ClientSize.Y;
              end;
            else
              begin
                Result.X := OriginalSize.X;
                Result.Y := OriginalSize.Y;
              end;
          end;
    end;

  procedure Center( const Size : TPoint; const Area : TRect; var Rect : TRect );
    var
      ClientSize : TPoint;
    begin
      // Place control in ClientArea's center
      ClientSize := RectSize( Area );
      with Area do
        begin
          if Size.X < ClientSize.X
            then Rect.Left := ( ClientSize.X - Size.X ) div 2 + Left
            else Rect.Left := Left;

          if Size.Y < ClientSize.Y
            then Rect.Top := ( ClientSize.Y - Size.Y ) div 2 + Top
            else Rect.Top := Top;
        end;

      with Rect do
        begin
          Right  := Left + Size.X;
          Bottom := Top + Size.Y;
        end;
    end;

end.
