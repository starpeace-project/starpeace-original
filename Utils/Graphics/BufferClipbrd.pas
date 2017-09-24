unit BufferClipbrd;

interface

  uses
    Windows, Clipbrd, Buffer, SpeedBmp;

  procedure BitmapSaveToClipboard( Source : TBuffer );
  procedure BitmapLoadFromClipboard( Dest : TBuffer );

implementation

  procedure BitmapSaveToClipboard( Source : TBuffer );
    var
      Data    : THandle;
      Format  : word;
      Palette : HPALETTE;
    begin
      with Clipboard do
        begin
          Open;
          try
            Clear;
            Palette := 0;
            Source.SaveToClipboardFormat( Format, Data, Palette );
            SetClipboardData( Format, Data );
            if Palette <> 0
              then SetClipboardData( CF_PALETTE, Palette );
          finally
            Close;
          end;
        end;
    end;

  procedure BitmapLoadFromClipboard( Dest : TBuffer );
    begin
      with Clipboard do
        begin
          Open;
          try
            Dest.LoadFromClipboardFormat( CF_DIB, GetClipboardData( CF_DIB ), 0 );
          finally
            Close;
          end;
        end;
    end;

end.
