unit WaveConvert;

interface

  uses
    Windows, MMSystem, ACM,
    SysUtils;

  const
    strConvNotPossible = 'Conversion is not possible';
    strConversionError = 'Conversion error';

  type
    EACMConverter = class(Exception);

    TDataProc    = function(var Data; Size : integer) : integer of object;
    TConvertProc = procedure(SourceInc, DestInc : integer) of object;

    TACMConverter =
      class
        public
          constructor Create(const aSourceFormat, aDestFormat : TWaveFormatEx);
          destructor  Destroy;                                                  override;
        private
          function OpenMethod : boolean;
        public
          procedure CreateBuffers(Frames : dword);  // buffersize = 1 second / Frames
          procedure Reset;                                                      virtual;
        private
          procedure Prepare;
          procedure Unprepare;
        private
          fHandle       : hACMStream;
          fSourceFormat : TWaveFormatEx;
          fDestFormat   : TWaveFormatEx;
          fSourceSize   : dword;
          fDestSize     : dword;
          fSourceBuffer : PByteArray;
          fDestBuffer   : PByteArray;
          fHeader       : TACMStreamHeader;
      end;

    TACMConverterIn =
      class(TACMConverter)
        public
          procedure Reset;                                                      override;
          function  Read(var Data; Size : integer) : integer;
        private
          fDestPos    : dword;
          fSourceRead : dword;
        private
          procedure Feed;
        private
          fOnRead    : TDataProc;
          fOnConvert : TConvertProc;
        public
          property OnRead : TDataProc       read fOnRead    write fOnRead;
          property OnConvert : TConvertProc read fOnConvert write fOnConvert;
      end;

    TACMConverterOut =
      class(TACMConverter)
        public
          destructor  Destroy;                                                  override;
        public
          procedure Reset;                                                      override;
          function  Write(const Data; Size : integer) : boolean;
        private
          fSourcePos : dword;
        private
          function Flush : boolean;
        private
          fOnWrite   : TDataProc;
          fOnConvert : TConvertProc;
        public
          property OnWrite : TDataProc      read fOnWrite   write fOnWrite;
          property OnConvert : TConvertProc read fOnConvert write fOnConvert;
      end;

implementation

  uses
    Math;
  // TACMConverter

  constructor TACMConverter.Create(const aSourceFormat, aDestFormat : TWaveFormatEx);
    var
      opened : boolean;
    begin
      inherited Create;
      fSourceFormat := aSourceFormat;
      fDestFormat   := aDestFormat;

      opened := OpenMethod;
      if not opened
        then raise EACMConverter.Create(strConvNotPossible);

      CreateBuffers(16); // Keep the buffer as small as possible
    end;

  destructor TACMConverter.Destroy;
    begin
      Unprepare;

      if fSourceBuffer <> nil
        then freemem(fSourceBuffer);
      if fDestBuffer <> nil
        then freemem(fDestBuffer);

      if fHandle <> 0
        then acmStreamClose(fHandle, 0);

      inherited;
    end;

  function TACMConverter.OpenMethod : boolean;
    var
      r : integer;
    begin
      r := acmStreamOpen(fHandle, 0, @fSourceFormat, @fDestFormat, nil, 0, 0, 0);
      if r <> MMSYSERR_NOERROR
        then r := acmStreamOpen(fHandle, 0, @fSourceFormat, @fDestFormat, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME);
      Result := r = MMSYSERR_NOERROR;
    end;

  procedure TACMConverter.CreateBuffers(Frames : dword);
    var
      ReqSize : dword;
    begin
      //...Here we have the implementation for "One second of source data" (tested)

      ReqSize := fSourceFormat.nAvgBytesPerSec div Frames;
      fSourceSize := ReqSize - (ReqSize mod fSourceFormat.nBlockAlign);
      if fSourceSize = 0
        then fSourceSize := fSourceFormat.nBlockAlign;

      if acmStreamSize(fHandle, fSourceSize, fDestSize, ACM_STREAMSIZEF_SOURCE) <> MMSYSERR_NOERROR
        then raise EACMConverter.Create(strConvNotPossible);

      {
      //...Here we have the implementation for "One second of destination data" (tested)

      ReqSize := fDestFormat.nAvgBytesPerSec;
      fDestSize := ReqSize - (ReqSize mod fDestFormat.nBlockAlign);

      if acmStreamSize(fHandle, fDestSize, fSourceSize, ACM_STREAMSIZEF_DESTINATION) <> MMSYSERR_NOERROR
        then raise EACMConverter.Create(strConvNotPossible);
      }

      if fSourceBuffer <> nil
        then freemem(fSourceBuffer);
      if fDestBuffer <> nil
        then freemem(fDestBuffer);

      getmem(fSourceBuffer, fSourceSize);
      getmem(fDestBuffer, fDestSize);

      Reset;
    end;

  procedure TACMConverter.Reset;
    begin
      Unprepare;
      Prepare;
    end;

  procedure TACMConverter.Prepare;
    begin
      Assert(fHeader.fdwStatus and ACMSTREAMHEADER_STATUSF_PREPARED = 0, 'TACMConverter.Prepare: Stream already prepared');

      with fHeader do
        begin
          cbStruct        := sizeof(fHeader);
          fdwStatus       := 0;
          dwUser          := 0;
          pbSrc           := fSourceBuffer;
          cbSrcLength     := fSourceSize;
          cbSrcLengthUsed := fSourceSize;  // Buffer is dirty at start
          dwSrcUser       := 0;
          pbDst           := fDestBuffer;
          cbDstLength     := fDestSize;
          cbDstLengthUsed := fDestSize;    // Buffer is dirty at start
          dwDstUser       := 0;
        end;

      if acmStreamPrepareHeader(fHandle, @fHeader, 0) <> MMSYSERR_NOERROR
        then raise EACMConverter.Create(strConversionError);
   end;

  procedure TACMConverter.Unprepare;
    begin
      if fHeader.fdwStatus and ACMSTREAMHEADER_STATUSF_PREPARED <> 0
        then
          begin
            fHeader.cbSrcLength := fSourceSize;
            fHeader.cbDstLength := fDestSize;
            acmStreamUnprepareHeader(fHandle, @fHeader, 0);
          end;
    end;

  // TACMConverterIn

  procedure TACMConverterIn.Reset;
    begin
      inherited;
      fSourceRead := fSourceSize;
      fDestPos    := fDestSize;
    end;

  function TACMConverterIn.Read(var Data; Size : integer) : integer;
    var
      DataBytes  : TByteArray absolute Data;
      SizeLeft   : integer;
      BufferLeft : integer;
      ToCopy     : integer;
      DataPos    : integer;
    begin
      DataPos := 0;
      SizeLeft := Size;
      repeat
        BufferLeft := fHeader.cbDstLengthUsed - fDestPos;
        if BufferLeft = 0
          then
            begin
              Feed;
              BufferLeft := fHeader.cbDstLengthUsed;
            end;

        ToCopy := Min(SizeLeft, BufferLeft);
        move(PByteArray(fHeader.pbDst)[fDestPos], DataBytes[DataPos], ToCopy);
        inc(DataPos, ToCopy);
        inc(fDestPos, ToCopy);
        dec(SizeLeft, ToCopy);
      until (SizeLeft = 0) or (BufferLeft = 0);
      Result := Size - SizeLeft;
    end;

  procedure TACMConverterIn.Feed;
    var
      LastBytes : dword;
      Flags     : dword;
    begin
      Assert(assigned(fOnRead), 'TACMConverter.Feed: No data source');

      LastBytes := fSourceRead - fHeader.cbSrcLengthUsed;
      move(fSourceBuffer[fHeader.cbSrcLengthUsed], fSourceBuffer[0], LastBytes);
      fSourceRead := LastBytes + dword(fOnRead(fSourceBuffer[LastBytes], fSourceSize - LastBytes));

      fHeader.cbSrcLength     := fSourceRead;
      fHeader.cbDstLengthUsed := 0;

      if fSourceRead <> 0
        then Flags := ACM_STREAMCONVERTF_BLOCKALIGN
        else Flags := ACM_STREAMCONVERTF_END;
      if acmStreamConvert(fHandle, @fHeader, Flags) <> MMSYSERR_NOERROR
        then raise EACMConverter.Create(strConversionError);

      if assigned(fOnConvert)
        then fOnConvert(fHeader.cbSrcLengthUsed, fHeader.cbDstLengthUsed);

      fDestPos := 0;
    end;

  // TACMConverterOut

  destructor TACMConverterOut.Destroy;
    begin
      try
        if fSourcePos > 0
          then Flush;
      except
        // Conversion error, ignored
      end;
      inherited;
    end;

  procedure TACMConverterOut.Reset;
    begin
      inherited;
      fSourcePos := 0;
    end;

  function TACMConverterOut.Write(const Data; Size : integer) : boolean;
    var
      DataBytes  : TByteArray absolute Data;
      SizeLeft   : integer;
      BufferLeft : dword;
      ToCopy     : integer;
      DataPos    : integer;
    begin
      Result := true;
      DataPos := 0;
      SizeLeft := Size;
      repeat
        BufferLeft := fSourceSize - fSourcePos;
        if BufferLeft = 0
          then
            begin
              Result := Flush;
              BufferLeft := fSourceSize - fSourcePos;
            end;
        ToCopy := Min(SizeLeft, BufferLeft);
        move(DataBytes[DataPos], PByteArray(fHeader.pbSrc)[fSourcePos], ToCopy);
        inc(DataPos, ToCopy);
        inc(fSourcePos, ToCopy);
        dec(SizeLeft, ToCopy);
      until (SizeLeft = 0) or (BufferLeft = 0) or not Result;
    end;

  function TACMConverterOut.Flush : boolean;
    var
      Flags : DWORD;
    begin
      Assert(assigned(fOnWrite), 'TACMConverter.Flush: No data destination');

      fHeader.cbSrcLength     := fSourcePos;
      fHeader.cbDstLengthUsed := 0;

      if fSourcePos = fSourceSize
        then Flags := ACM_STREAMCONVERTF_BLOCKALIGN
        else Flags := ACM_STREAMCONVERTF_END;

      if acmStreamConvert(fHandle, @fHeader, Flags) <> MMSYSERR_NOERROR
        then raise EACMConverter.Create(strConversionError);

      if assigned(fOnConvert)
        then fOnConvert(fHeader.cbSrcLengthUsed, fHeader.cbDstLengthUsed);

      Result := fOnWrite(fDestBuffer^, fHeader.cbDstLengthUsed) = integer(fHeader.cbDstLengthUsed);
      fSourcePos := fSourcePos - fHeader.cbSrcLengthUsed;
      move(fSourceBuffer[fHeader.cbSrcLengthUsed], fSourceBuffer[0], fSourcePos);
    end;

end.

