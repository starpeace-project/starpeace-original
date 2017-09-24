unit ImageLoader;

interface

  uses
    GameTypes;

  function  LoadGameImage(const FileName : string) : TGameImage;
  procedure ImageReleased(const FileName : string);

implementation

  uses
    SysUtils, Classes, SpeedBmp, Dib2Frames, SpriteLoaders, ImageLoaders, GifLoader;

  var
    vLoadedImages : TStringList;

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
      index     : integer;
    begin
      index := vLoadedImages.IndexOf(FileName);
      if index = -1
        then
          begin
            if HasGifExtension(FileName)
              then
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
                end
              else
                try
                  aux := TSpeedBitmap.Create;
                  try
                    aux.LoadFromFile(FileName);
                    Result := FrameFromDib(aux.DibHeader, aux.ScanLines);
                  finally
                    aux.Free;
                  end;
                except
                  Result := nil;
                end;
            if Result <> nil
              then vLoadedImages.AddObject(FileName, Result);
          end
        else
          begin
            Result := TGameImage(vLoadedImages.Objects[index]);
            Result.AddRef;
          end;
    end;

  procedure ImageReleased(const FileName : string);
    begin
      vLoadedImages.Delete(vLoadedImages.IndexOf(FileName));
    end;

initialization
  RegisterLoader( GetGifLoader, 0 );
  vLoadedImages := TStringList.Create;
finalization
  vLoadedImages.Free;
end.
