unit SurfaceSpriteImages;

interface

  uses
    Classes, Windows, DDrawPaletteFilters, DirectDraw, Direct3D;

  type
    PPixel = ^TPixel;
    TPixel = byte;        // 256 colors

  type
    TFrame =
      record
        surface : IDirectDrawSurface7;
        ddsdesc : TDDSurfaceDesc2;
        delay   : integer;
      end;

  type
    PFrameList = ^TFrameList;
    TFrameList = array[0..0] of TFrame;

  // TSurfaceFrameImage

  type
    TSurfaceFrameImage =
      class
        public
          constructor Create(aWidth, aHeight : integer);
          destructor  Destroy; override;
        private
          fRefCount : integer;
        public
          function AddRef : integer;
          function ReleaseRef : integer;
        protected
          fSize             : TPoint;
          fFrameCount       : integer;
          fFrames           : PFrameList;
          fTranspIndx       : byte;
          fPalette          : IDirectDrawPalette;
          fOwnsPalette      : boolean;
          fFilteredPalettes : TList;
        public
          function  LockFrame(frame : integer; wait : boolean) : boolean;
          function  UnlockFrame(frame : integer) : boolean;
        protected
          function  GetPixelAddr(x, y : integer; frame : integer) : pointer;
          function  GetStorageWidth(frame : integer) : integer;
          procedure SetPixel(x, y : integer; frame : integer; Color : TPixel);
          function  GetPixel(x, y : integer; frame : integer) : TPixel;
          procedure SetFrameDelay(frame : integer; Delay : integer);
          function  GetFrameDelay(frame : integer) : integer;
          function  GetImageSize : integer;
          function  GetClientRect : TRect;
          procedure SetPalette(const Palette : IDirectDrawPalette);
          procedure SetTranspIndx(TranspIndx : byte);
        public
          property  Palette : IDirectDrawPalette                            read fPalette         write SetPalette;
          property  OwnsPalette : boolean                                   read fOwnsPalette     write fOwnsPalette;
          property  PixelAddr[ x, y : integer; frame : integer ] : pointer  read GetPixelAddr;
          property  Pixel[x, y : integer; frame : integer] : TPixel         read GetPixel         write SetPixel;
          property  FrameDelay[frame : integer] : integer                   read GetFrameDelay    write SetFrameDelay;
          property  FrameCount : integer                                    read fFrameCount;
          property  ClientRect : TRect                                      read GetClientRect;
          property  Size : TPoint                                           read fSize;
          property  Width : longint                                         read fSize.x;
          property  Height : longint                                        read fSize.y;
          property  StorageWidth[frame : integer] : integer                 read GetStorageWidth;
          property  ImageSize : integer                                     read GetImageSize;
          property  TranspIndx : byte                                       read fTranspIndx      write SetTranspIndx;
        public
          procedure NewFrames(Count : integer);
          procedure DeleteFrames(Indx : integer; Count : integer);
          procedure AddFrames(Value : PFrameList; Count : integer);
          procedure AddFrame(Value : TFrame);
          procedure Release;
        public
          function  GetFilteredPalette(Filter : TPaletteFilter; Data : array of const) : IDirectDrawPalette;
        public
          function  IsSolid(x, y : integer; frame : integer) : boolean;
          function  PixelOfs(x, y : integer; frame : integer) : integer;
        public
          procedure Draw(x, y, frame, alpha : integer; scale : single; const ClipArea : TRect; const Device : IDirect3DDevice7; const aPalette : IDirectDrawPalette);
          procedure DrawOpaque(x, y, frame : integer; scale : single; const ClipArea : TRect; const Device : IDirect3DDevice7; const aPalette : IDirectDrawPalette);
      end;

  {$IFDEF MEMREPORTS}
  const
    cSpriteAllocCause = 'Sprites';
  {$ENDIF}

implementation

  uses
    SysUtils, DDrawD3DManager, DirectDrawUtils, D3DXSprite, D3DXCore, AxlDebug
    {$IFDEF PROFILES}, Profiler, IsoProfile{$ENDIF}
    {$IFDEF RENDERREPORTS}, RenderReports{$ENDIF}
    {$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  type
    PFilteredPaletteInfo = ^TFilteredPaletteInfo;
    TFilteredPaletteInfo =
      record
        filter  : TPaletteFilter;
        palette : IDirectDrawPalette;
      end;

  // TSurfaceFrameImage

  constructor TSurfaceFrameImage.Create(aWidth, aHeight : integer);
    begin
      inherited Create;
      fRefCount := 1;
      fSize := Point( aWidth, aHeight );
      fFilteredPalettes := TList.Create;
    end;

  destructor TSurfaceFrameImage.Destroy;
    var
      i           : integer;
      filtpalinfo : PFilteredPaletteInfo;
    begin
      Release;
      if fOwnsPalette
        then fPalette := nil;
      for i := 0 to pred(fFilteredPalettes.Count) do
        begin
          filtpalinfo := PFilteredPaletteInfo(fFilteredPalettes[i]);
          dispose(filtpalinfo);
          fFilteredPalettes[i] := nil;
        end;
      fFilteredPalettes.Free;
      inherited;
    end;

  function TSurfaceFrameImage.AddRef : integer;
    begin
      inc(fRefCount);
      Result := fRefCount;
    end;

  function TSurfaceFrameImage.ReleaseRef : integer;
    begin
      dec(fRefCount);
      Result := fRefCount;
      if fRefCount = 0
        then Free;
    end;

  function TSurfaceFrameImage.LockFrame(frame : integer; wait : boolean) : boolean;
    var
      flags : integer;
    begin
      if wait
        then flags := DDLOCK_WAIT
        else flags := DDLOCK_DONOTWAIT;
      with fFrames[frame] do
        begin
          InitRecord(ddsdesc, sizeof(ddsdesc));
          Result := surface.Lock(nil, ddsdesc, flags, 0) = DD_OK;
        end;
    end;

  function TSurfaceFrameImage.UnlockFrame(frame : integer) : boolean;
    begin
      with fFrames[frame] do
        Result := surface.Unlock(nil) = DD_OK;
    end;

  function TSurfaceFrameImage.GetPixelAddr( x, y : integer; frame : integer ) : pointer;
    begin
      with fFrames[frame] do
        Result := pchar(ddsdesc.lpSurface) + (y*ddsdesc.lPitch + x);
    end;

  function TSurfaceFrameImage.GetStorageWidth(frame : integer) : integer;
    begin
      with fFrames[frame] do
        Result := ddsdesc.lPitch;
    end;

  procedure TSurfaceFrameImage.SetPixel(x, y : integer; frame : integer; color : TPixel);
    begin
      PPixel(PixelAddr[x, y, frame])^ := color;
    end;

  function TSurfaceFrameImage.GetPixel(x, y : integer; frame : integer) : TPixel;
    begin
      Result := PPixel(PixelAddr[x, y, frame])^;
    end;

  procedure TSurfaceFrameImage.SetFrameDelay(frame : integer; Delay : integer);
    begin
      fFrames[frame].delay := Delay;
    end;

  function TSurfaceFrameImage.GetFrameDelay(frame : integer) : integer;
    begin
      Result := fFrames[frame].delay;
    end;

  function TSurfaceFrameImage.GetImageSize : integer;
    begin
      Result := Width*Height;
    end;

  function TSurfaceFrameImage.GetClientRect : TRect;
    begin
      with Result do
        begin
          TopLeft     := Point( 0, 0 );
          BottomRight := Size;
        end;
    end;

  procedure TSurfaceFrameImage.SetPalette(const Palette : IDirectDrawPalette);
    var
      i  : integer;
      hr : HRESULT;
    begin
      fPalette := Palette;
      for i := 0 to pred(fFrameCount) do
        with fFrames[i] do
          begin
            hr := surface.SetPalette(Palette);
            if Failed(hr)
              then WriteDebugStr('Set Palette Failed');
          end;
    end;

  procedure TSurfaceFrameImage.SetTranspIndx(TranspIndx : byte);
    var
      colorkey : TDDColorKey;
      i        : integer;
    begin
      fTranspIndx := TranspIndx;
      colorkey.dwColorSpaceHighValue := TranspIndx;
      colorkey.dwColorSpaceLowValue := TranspIndx;
      for i := 0 to pred(fFrameCount) do
        with fFrames[i] do
          if Failed(surface.SetColorKey(DDCKEY_SRCBLT, @colorkey))
            then raise Exception.Create('Error setting color key');
    end;

  procedure TSurfaceFrameImage.NewFrames(Count : integer);
    var
      i : integer;
    begin
      reallocmem(fFrames, (FrameCount + Count)*sizeof(fFrames[0]));
      initialize(fFrames[fFrameCount], Count);
      {$IFDEF MEMREPORTS}
      ReportMemoryAllocation(cSpriteAllocCause, (FrameCount + Count)*sizeof(fFrames[0]));
      {$ENDIF}
      for i := 0 to pred(Count) do
        begin
          fFrames[FrameCount + i].surface := DDrawD3DMgr.CreateTextureSurface(Width, Height, 8, true);
          if fFrames[FrameCount + i].surface = nil
            then raise Exception.Create('Error creating frame surface');
          {$IFDEF MEMREPORTS}
          ReportMemoryAllocation(cSpriteAllocCause, ImageSize);
          {$ENDIF}
        end;
      inc(fFrameCount, Count);
    end;

  procedure TSurfaceFrameImage.DeleteFrames(Indx : integer; Count : integer);
    var
      i : integer;
    begin
      assert( (FrameCount > 0 ) and (Indx >= 0 ) and (Indx < FrameCount ), 'Bad index or Count in TFrameImage.DeleteFrames' );

      if Indx + Count > FrameCount
        then Count := FrameCount - Indx;
      for i := Indx to Indx + Count - 1 do
        fFrames[i].surface := nil;
      if Count <> FrameCount // >>> problem here, can't do move !!!
        then Move(fFrames[Indx + Count], fFrames[Indx], Count*sizeof(fFrames[0]));
       reallocmem(fFrames, (FrameCount - Count)*sizeof(fFrames[0]));
       dec(fFrameCount, Count);
    end;

  procedure TSurfaceFrameImage.AddFrames(Value : PFrameList; Count : integer);
    var
      i : integer;
    begin
      reallocmem(fFrames, (FrameCount + Count)*sizeof(fFrames[0]));
      initialize(fFrames[fFrameCount], Count);
      {$IFDEF MEMREPORTS}
      ReportMemoryAllocation(cSpriteAllocCause, (FrameCount + Count ) * sizeof(TFrameList ));
      {$ENDIF}
      for i := 0 to Count - 1 do
        fFrames[FrameCount + i] := Value[i];
      inc(fFrameCount, Count);
    end;

  procedure TSurfaceFrameImage.AddFrame(Value : TFrame);
    begin
      AddFrames(@Value, 1);
    end;

  procedure TSurfaceFrameImage.Release;
    begin
      DeleteFrames(0, FrameCount);
    end;

  function TSurfaceFrameImage.GetFilteredPalette(Filter : TPaletteFilter; Data : array of const) : IDirectDrawPalette;
    var
      i           : integer;
      found       : boolean;
      filtpalinfo : PFilteredPaletteInfo;
    begin
      i := 0;
      found := false;
      while (i < fFilteredPalettes.Count) and not found do
        begin
          filtpalinfo := PFilteredPaletteInfo(fFilteredPalettes[i]);
          found := filtpalinfo.Filter = Filter;
          if not found
            then inc(i);
        end;
      if found
        then Result := PFilteredPaletteInfo(fFilteredPalettes[i]).Palette
        else
          begin
            Result := FilterPalette(fPalette, Filter, Data);
            new(filtpalinfo);
            filtpalinfo.Filter := Filter;
            filtpalinfo.Palette := Result;
          end;
    end;

  // TSurfaceFrameImage drawing

  function TSurfaceFrameImage.IsSolid(x, y : integer; frame : integer) : boolean;
    begin
      Result := Pixel[x, y, frame] <> TranspIndx;
    end;

  function TSurfaceFrameImage.PixelOfs(x, y : integer; frame : integer) : integer;
    begin
      with fFrames[frame] do
        Result := (y*ddsdesc.lPitch + x);
    end;

  procedure TSurfaceFrameImage.Draw(x, y, frame, alpha : integer; scale : single; const ClipArea : TRect; const Device : IDirect3DDevice7; const aPalette : IDirectDrawPalette);
    const
      cMaxAlpha = 8;
    var
      ImageArea : TRect;
      //PalToUse  : IDirectDrawPalette;
      drawpoint : TD3DXVECTOR3;
      hr        : HRESULT;
      //errstr    : string;
    begin
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_Rendering);
      Profiler.ProcStarted(prfKind_Main, prfId_ImageDrawing);
      {$ENDIF}
      if (frame >= 0) and (frame < fFrameCount)
        then //raise Exception.Create('Bad frame specified in TSpriteImage.Draw!!');
          begin
            with ImageArea do
              begin
                Left   := x;
                Right  := x + Width;
                Top    := y;
                Bottom := y + Height;
              end;
            IntersectRect(ImageArea, ImageArea, ClipArea);
            OffsetRect(ImageArea, -x, -y);
            if ImageArea.Right > Width
              then ImageArea.Right := Width;
            if ImageArea.Bottom > Height
              then ImageArea.Bottom := Height;
            drawpoint.x := x + ImageArea.Left + (ImageArea.Right - ImageArea.Left)/2;
            drawpoint.y := y + ImageArea.Top + (ImageArea.Bottom - ImageArea.Top)/2;
            drawpoint.z := 0;
            {
            PalToUse := aPalette;
            if PalToUse = nil
              then PalToUse := Palette;
            }
            {$IFDEF RENDERREPORTS}
            IncImgCount;
            {$ENDIF}
            with fFrames[frame] do
              begin
                //hr := surface.SetPalette(PalToUse); // >>> this might be inefficient, try not doing it more than once
                {if Succeeded(hr)
                  then} hr := D3DXDrawSpriteSimple(surface, Device, @drawpoint, 1 - Alpha/cMaxAlpha, scale, 0.0, nil, @ImageArea);
              end;
            {
            if Failed(hr)
              then
                begin
                  SetLength(errstr, 1000);
                  D3DXGetErrorString(hr, 1000, @errstr[1]);
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('D3DXDrawSpriteSimple call Failed');
                  //raise Exception.Create('D3DXDrawSpriteSimple call Failed');
                end;

            }
          end;
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_ImageDrawing);
      Profiler.ProcStarted(prfKind_Main, prfId_Rendering);
      {$ENDIF}
    end;

  procedure TSurfaceFrameImage.DrawOpaque(x, y, frame : integer; scale : single; const ClipArea : TRect; const Device : IDirect3DDevice7; const aPalette : IDirectDrawPalette);
    var
      ImageArea : TRect;
      //PalToUse  : IDirectDrawPalette;
      drawpoint : TD3DXVECTOR3;
      hr        : HRESULT;
      //errstr    : string;
    begin
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_Rendering);
      Profiler.ProcStarted(prfKind_Main, prfId_ImageDrawing);
      {$ENDIF}
      if (frame >= 0) and (frame < fFrameCount)
        then //raise Exception.Create('Bad frame specified in TSpriteImage.Draw!!');
          begin
            with ImageArea do
              begin
                Left   := x;
                Right  := x + Width;
                Top    := y;
                Bottom := y + Height;
              end;
            IntersectRect(ImageArea, ImageArea, ClipArea);
            OffsetRect(ImageArea, -x, -y);
            if ImageArea.Right > Width
              then ImageArea.Right := Width;
            if ImageArea.Bottom > Height
              then ImageArea.Bottom := Height;
            drawpoint.x := x + ImageArea.Left + (ImageArea.Right - ImageArea.Left)/2;
            drawpoint.y := y + ImageArea.Top + (ImageArea.Bottom - ImageArea.Top)/2;
            drawpoint.z := 0;
            {
            PalToUse := aPalette;
            if PalToUse = nil
              then PalToUse := Palette;
            }
            {$IFDEF RENDERREPORTS}
            IncImgCount;
            {$ENDIF}
            with fFrames[frame] do
              begin
                //hr := surface.SetPalette(PalToUse); // >>> this might be inefficient, try not doing it more than once
                {if Succeeded(hr)
                  then} hr := D3DXDrawSpriteSimple(surface, Device, @drawpoint, 1.0, scale, 0.0, nil, @ImageArea);
              end;
            {
            if Failed(hr)
              then
                begin
                  SetLength(errstr, 1000);
                  D3DXGetErrorString(hr, 1000, @errstr[1]);
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('Error!');
                  WriteDebugStr('D3DXDrawSpriteSimple call Failed');
                  //raise Exception.Create('D3DXDrawSpriteSimple call Failed');
                end;

            }
          end;
      {$IFDEF PROFILES}
      Profiler.ProcEnded(prfKind_Main, prfId_ImageDrawing);
      Profiler.ProcStarted(prfKind_Main, prfId_Rendering);
      {$ENDIF}
    end;

end.
