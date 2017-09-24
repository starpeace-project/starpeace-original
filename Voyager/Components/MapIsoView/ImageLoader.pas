unit ImageLoader;

interface

  uses
    GameTypes;

  function  LoadGameImage(const FileName : string) : TGameImage;
  procedure ImageReleased(const FileName : string);

implementation

  uses
    SysUtils, Classes, SpeedBmp, Dib2Frames, SpriteLoaders, ImageLoaders, GifLoader,
    SyncObjs, VCLUtils;

  var
    vLoadedImages : TStringList;
    vLoaderLock   : TCriticalSection;

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
      vLoaderLock.Enter;
      try
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
                then 
                  begin
                    vLoadedImages.AddObject(FileName, Result);
                    Result.AddRef;
                  end;
            end
          else
            begin
              Result := TGameImage(vLoadedImages.Objects[index]);
              Result.AddRef;
            end;
      finally
        vLoaderLock.Leave;
      end;
      {
      if Result <> nil
        then
          begin
            getmem(tmp, Result.Width*Result.Height);
            try
              try
                BltCopyOpaque(Result.PixelAddr[0, 0, 0], tmp, Result.Width, Result.Height, Result.StorageWidth, Result.Width);
              except
                raise Exception.Create('Hehe. I got you!');
              end;
            finally
              freemem(tmp);
            end;
          end;
      }
    end;

  procedure ImageReleased(const FileName : string);
    var
      i : integer;
    begin
      vLoaderLock.Enter;
      try
        i := vLoadedImages.IndexOf(FileName);
        if i <> -1
          then vLoadedImages.Delete(i);
      finally
        vLoaderLock.Leave;
      end;
    end;

procedure FreeLoadedImages;
  var
    i : integer;
  begin
    with vLoadedImages do
      for i:=0 to pred(Count-1) do
        begin
          try
            (Objects[i] as TGameImage).ReleaseRef;
          except
          end;
          Objects[i] := nil;
        end;
    vLoadedImages.free;
    vLoadedImages := nil;
  end;

initialization
  RegisterLoader( GetGifLoader, 0 );
  vLoadedImages := TStringList.Create;
  vLoaderLock   := TCriticalSection.Create;
finalization
   FreeLoadedImages;
//  vLoadedImages.free;
  vLoaderLock.Free;
end.
