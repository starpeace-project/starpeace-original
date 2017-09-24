unit ImageLoader;

interface

uses
  GameTypes;

function LoadGameImage(const FileName : string) : TGameImage;

implementation


uses
  SysUtils, Classes, SpeedBmp, Dib2Frames, SpriteLoaders, ImageLoaders, GifLoader;


function LoadGameImage(const FileName : string) : TGameImage;

  function HasGifExtension(const FileName : string) : boolean;
    const
      GifExt = '.GIF';
    var
      i, j : integer;
    begin
      i := Length(FileName);
      j := Length(GifExt);
      while (j > 0) and (i > 0) and (UpCase( FileName[i] ) = GifExt[j]) do
        begin
          dec(i);
          dec(j);
        end;
      Result := j = 0;
    end;

  var
    aux       : TSpeedBitmap;
    ImgStream : TMemoryStream;
  begin
    if HasGifExtension(FileName)
      then
        begin
          try
            ImgStream := TMemoryStream.Create;
            try
              ImgStream.LoadFromFile(FileName);
              LoadFrameImage(Result, ImgStream);
            finally
              ImgStream.Free;
            end;
          except
            Result := nil;
          end;
        end
      else
        begin
          aux := TSpeedBitmap.Create;
          try
            try
              aux.LoadFromFile(FileName);
              Result := FrameFromDib(aux.DibHeader, aux.ScanLines);
            finally
              aux.Free;
            end;
          except
            Result := nil;
          end;
        end;
  end;

initialization
  RegisterLoader( GetGifLoader, 0 );
end.
