unit Gifs;

interface

  uses
    Classes, Windows, SysUtils, GDI, Dibs;

  const
    idGifSignature  = 'GIF';
    idGifVersion87a = '87a';
    idGifVersion89a = '89a';

  const
    // Logical screen descriptor bit masks
    lbmGlobalColorTable = $80; // set if global color table follows L.S.D.
    lbmColorResolution  = $70; // Color resolution - 3 bits
    lbmSort             = $08; // set if global color table is sorted - 1 bit
    lbmColorTableSize   = $07; // size of global color table - 3 bits
                               // Actual size = 2^value+1    - value is 3 bits

  type
    PLogicalScreenDescriptor = ^TLogicalScreenDescriptor;
    TLogicalScreenDescriptor = //  7 6 5 4 3 2 1 0
      packed record            // +---------------+     Raster width in pixels ( LSB first )
        Width     : word;      // + Screen Width  + 0
        Height    : word;      // +---------------+     Raster height in pixels ( LSB first )
        BitMask   : byte;      // + Screen Height + 2
        BackColor : byte;      // +-+-----+-+-----+     M = 1, Global color map follows Descriptor
        Ratio     : byte;      // |M|  cr |0|pixel| 4   cr + 1 = # bits of color resolution
      end;                     // +-+-----+-+-----+     pixel + 1 = # bits/pixel in image
                               // |   background  | 5
                               // +---------------+     background=Color index of screen background
                               // |  Pixel Ratio  | 6       (color is defined from the Global color
                               // +---------------+          map or default map if none specified)
                               //
                               //  Aspect Ratio = (Pixel Aspect Ratio + 15) / 64
                               //
                               //  The Pixel Aspect Ratio is defined to be the quotient of the pixel's
                               //  width over its height.  The value range in this field allows
                               //  specification of the widest pixel of 4:1 to the tallest pixel of
                               //  1:4 in increments of 1/64th.
                               //
                               //  Values :        0 -   No aspect ratio information is given.
                               //             1..255 -   Value used in the computation.

  const
    sigGIF       = 'GIF';
    sigVersion89 = '89a';
    sigVersion87 = '87a';

  type
    PGifFileHeader = ^TGifFileHeader;
    TGifFileHeader =
      packed record
        strSignature : array[0..2] of char;
        strVersion   : array[0..2] of char;
        Header       : TLogicalScreenDescriptor;
      end;

  type
    PGifColorTable = ^TGifColorTable;
    TGifColorTable = array[0..255] of TDosRgb; // 0..2^(BitCount)

  const // Gif block IDs
    gifSignature        = ord( 'G' );
    gifExtensionId      = ord( '!' );
    gifImageId          = ord( ',' );
    gifTerminatorId     = ord( ';' );
    gifPlainTextId      = $01;
    gifGraphicControlId = $f9;
    gifCommentId        = $fe;
    gifApplicationId    = $ff;

  const // Image descriptor bit masks
    ibmLocalColorTable    = $80; // set if a local color table follows
    ibmInterlaced         = $40; // set if image is interlaced
    ibmSort               = $20; // set if color table is sorted
    ibmReserved           = $0C; // reserved - must be set to $00
    ibmColorTableSize     = $07; // size of color table as in LSD

  type
    PGifImageDescriptor = ^TGifImageDescriptor;
    TGifImageDescriptor =
      packed record
        Signature : byte;   // Block signature, ',' for images
        Left      : word;   // Column in pixels in respect to left edge of logical screen
        Top       : word;   // row in pixels in respect to top of logical screen
        Width     : word;   // width of image in pixels
        Height    : word;   // height of image in pixels
        BitMask   : byte;   // See image descriptor bitmasks
      end;

  const // Graphic control extension bit masks
    gbmReserved       = $e0; // For future use
    gbmDisposalMethod = $1c; //
    gbmUserInput      = $02;
    gbmTransparent    = $01;

  const // Disposal methods
    dmIgnore            = 0;
    dmDontDispose       = 1;
    dmRestoreBackground = 2;
    dmRestorePrevios    = 3;

  type
    PGraphicControlExtension = ^TGraphicControlExtension;
    TGraphicControlExtension =
      packed record
        Signature       : byte; // Block signature, '!' for extensions
        ExtensionId     : byte; // Id for extensions, $F9 for graphic control extensions
        BlockSize       : byte; // Size is 4 for graphic control extensions (Count starts after BlockSize but w/out Terminator field)
        BitMask         : byte;
        DelayTime       : word;
        TransparentIndx : byte;
        Terminator      : byte; // A zero
      end;

  type
    PCommentExtensionHeader = ^TCommentExtensionHeader;
    TCommentExtensionHeader =
      packed record
        Signature       : byte;  // Block signature, '!' for extensions
        ExtensionId     : byte;  // Id for extensions, $fe for comment extensions
      end;

    PCommentExtension = ^TCommentExtension;
    TCommentExtension =
      record
        Text   : string;
        Header : TCommentExtensionHeader;
      end;

  type
    PPlainTextExtensionHeader = ^TPlainTextExtensionHeader;
    TPlainTextExtensionHeader =
      packed record
        Signature      : byte;        // Block signature, '!' for extensions
        ExtensionId    : byte;        // Id for extensions, $01 for plain text extensions
        BlockSize      : byte;
        GridLeft       : word;
        GridTop        : word;
        GridWidth      : word;
        GridHeight     : word;
        CellWidth      : byte;
        CellHeight     : byte;
        ForegroundIndx : byte;
        BackgroundIndx : byte;
      end;

    PPlainTextExtension = ^TPlainTextExtension;
    TPlainTextExtension =
      record
        Text   : string;
        Header : TPlainTextExtensionHeader;
      end;

  type
    PAppExtensionHeader = ^TAppExtensionHeader;
    TAppExtensionHeader =
      packed record
        Signature         : byte;        // Block signature, '!' for extensions
        ExtensionId       : byte;        // Id for extensions, $ff for application extensions
        BlockSize         : byte;
        AppId             : array[0..7] of char;
        AppAuthentication : array[0..2] of byte;
      end;

    PAppExtension = ^TAppExtension;
    TAppExtension =
      record
        DataSize : integer;
        Data     : pointer;
        Header   : TAppExtensionHeader;
      end;

  // GIF Error Handling ===========================================================

  type
    EGifError =
      class( Exception )
        private
          fErrorCode : integer;
        public
          constructor Create( const ErrorMessage : string; ErrorCode : integer );
        public
          property ErrorCode : integer read fErrorCode;
      end;

  const
    errNoError               = 0;
    errIllegalGIFStream      = 1;
    errIOError               = 2;
    errOutOfMemory           = 3;
    errLZWStackOverFlow      = 4;
    errLZWCircularTableEntry = 5;
    errMissingEOD            = 6;
    errIllegalPixelValue     = 7;
    errNotAGIFStream         = 8;
    errIllegalGIFVersion     = 9;
    errUnknownError          = 10;

  // Gif Decoding

  procedure UnpackGIFPixels( aStream : TStream; GifWidth, GifHeight : integer; DestPixels : pointer; DestWidth : integer; Interlaced : boolean );

  // Helper functions
  
  const
    bidGifTerminator      = 0;
    bidGifHeader          = 1;
    bidGifImageDescriptor = 2;
    bidGraphControlExt    = 3;
    bidCommentExt         = 4;
    bidPlainTextExt       = 5;
    bidAppExt             = 6;
    bidUnknown            = $ff;

  type
    TBlockIds = set of byte;

  procedure GifSkipSubBlocks( Stream : TStream );
  function  GifSkipBlocksExcept( Stream : TStream; BlockIds : TBlockIds ) : integer;
  function  GifReadBlockId( Stream : TStream ) : integer;
  procedure GifReadBlockInfo( Stream : TStream; BlockId : integer; var Data );
  function  GifReadSubBlocks( Stream : TStream; var Data : pointer ) : integer;

  function  GifDibHeaderFromGlobal( GlobalHeader : TLogicalScreenDescriptor ) : PDib;
  function  GifDibHeaderFromLocal( LocalHeader : TGifImageDescriptor ) : PDib;

implementation

  function GifDibHeaderFromGlobal( GlobalHeader : TLogicalScreenDescriptor ) : PDib;
    var
      BitCount, DibBitCount : integer;
    begin
      with GlobalHeader do
        begin
          BitCount := ( BitMask and lbmColorTableSize ) + 1;
          case BitCount of
            2..4 :
              DibBitCount := 4;
            5..8 :
              DibBitCount := 8;
            else
              DibBitCount := 1;
          end;
          Result := DibNewHeader( Width, Height, DibBitCount );
        end;
      Result.biClrUsed := 1 shl BitCount;
    end;

  function GifDibHeaderFromLocal( LocalHeader : TGifImageDescriptor ) : PDib;
    var
      BitCount, DibBitCount : integer;
    begin
      with LocalHeader do
        begin
          BitCount := ( BitMask and ibmColorTableSize ) + 1;
          case BitCount of
            2..4 :
              DibBitCount := 4;
            5..8 :
              DibBitCount := 8;
            else
              DibBitCount := 1;
          end;
          Result := DibNewHeader( Width, Height, DibBitCount );
        end;
      Result.biClrUsed := 1 shl BitCount;
    end;

  function GifReadSubBlocks( Stream : TStream; var Data : pointer ) : integer;
    var
      BlockSize : byte;
    begin
      Result := 0;
      Data   := nil;
      repeat
        Stream.ReadBuffer( BlockSize, sizeof( BlockSize ) );
        if BlockSize > 0
          then
            begin
              inc( Result, BlockSize );
              reallocmem( Data, Result );
              Stream.ReadBuffer( pchar(Data)[Result - BlockSize], BlockSize );
            end;
      until BlockSize = 0;
    end;

  procedure GifSkipSubBlocks( Stream : TStream );
    var
      BlockSize : byte;
    begin
      with Stream do
        repeat
          ReadBuffer( BlockSize, sizeof( BlockSize ) );
          Seek( BlockSize, soFromCurrent );
        until BlockSize = 0;
    end;

  function GifSkipBlocksExcept( Stream : TStream; BlockIds : TBlockIds ) : integer;
    var
      Found : boolean;
    begin
      repeat
        Result := GifReadBlockId( Stream );
        Found  := Result in BlockIds;
        if not Found
          then // Skip
            begin
              if Result = bidGifImageDescriptor
                then
                  begin
                    //
                  end
                else
                  begin
                    Stream.Seek( 2, soFromCurrent );
                    GifSkipSubBlocks( Stream );
                  end;  
            end;
      until Found;
    end;

  function GifReadBlockId( Stream : TStream ) : integer;
    var
      StartPos  : integer;
      Signature : byte;
      SigRest   : word;
      ExtId     : byte;
    begin
      StartPos := Stream.Position;
      try
        Stream.ReadBuffer( Signature, sizeof( Signature ) );
        case Signature of
          gifSignature :
            begin // OK, first letter was 'G', let's find out if rest is 'IF'
              Stream.ReadBuffer( SigRest, sizeof( SigRest ) );
              if SigRest <> $4649
                then Result := bidGifHeader
                else Result := bidUnknown;
            end;
          gifExtensionId :
            begin
              Stream.ReadBuffer( ExtId, sizeof( ExtId ) );
              case ExtId of
                gifPlainTextId :
                  Result := bidPlainTextExt;
                gifGraphicControlId :
                  Result := bidGraphControlExt;
                gifCommentId :
                  Result := bidCommentExt;
                gifApplicationId :
                  Result := bidAppExt;
                else Result := bidUnknown;
              end;
            end;
          gifImageId :
            Result := bidGifImageDescriptor;
          gifTerminatorId :
            Result := bidGifTerminator;
          else Result := bidUnknown;
        end;
      finally
        Stream.Position := StartPos;
      end;
    end;

  procedure GifReadBlockInfo( Stream : TStream; BlockId : integer; var Data );
    var
      GifHeaderData : TGifFileHeader            absolute Data;
      GifImageData  : TGifImageDescriptor       absolute Data;
      ControlData   : TGraphicControlExtension  absolute Data;
      CommentData   : TCommentExtension         absolute Data;
      PlainTextData : TPlainTextExtension       absolute Data;
      AppExtData    : TAppExtension             absolute Data;
      TmpData       : pchar;
      TmpCount      : integer;
    begin
      if BlockId <> bidGifHeader
        then GifSkipBlocksExcept( Stream, [BlockId] );
      case BlockId of
        bidGifHeader :
          Stream.ReadBuffer( GifHeaderData, sizeof( GifHeaderData ) );
        bidGifImageDescriptor :
           Stream.ReadBuffer( GifImageData, sizeof( GifImageData ) );
        bidGraphControlExt :
          Stream.ReadBuffer( ControlData, sizeof( ControlData ) );
        bidCommentExt :
          begin
            Stream.ReadBuffer( CommentData.Header, sizeof( CommentData.Header ) );
            TmpCount := GifReadSubBlocks( Stream, pointer( TmpData ) );
            SetString( CommentData.Text, TmpData, TmpCount );
          end;
        bidPlainTextExt :
          begin
            Stream.ReadBuffer( PlainTextData.Header, sizeof( PlainTextData.Header ) );
            TmpCount := GifReadSubBlocks( Stream, pointer( TmpData ) );
            SetString( PlainTextData.Text, TmpData, TmpCount );
          end;
        bidAppExt :
          begin
            Stream.ReadBuffer( AppExtData.Header, sizeof( AppExtData.Header ) );
            AppExtData.DataSize := GifReadSubBlocks( Stream, AppExtData.Data );
          end;
      end;
    end;

  // EGifError

  constructor EGifError.Create( const ErrorMessage : string; ErrorCode : integer );
    begin
      inherited Create( ErrorMessage );
      fErrorCode := ErrorCode
    end;

  // The meat stuff

  const
    MaxLzwBits = 12;

  const
    PassIncTable        : array[0..3] of byte = ( 8, 8, 4, 2 );
    PassInitialPosTable : array[0..3] of byte = ( 0, 4, 2, 1 );

  const
    MaskTable : array[0..pred( 16 )] of word =
      (
        $0000, $0001, $0003, $0007, $000f, $001f, $003f, $007f,
        $00ff, $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff
      );

  var
    ZeroDataBlock : boolean = false;

  procedure UnpackGIFPixels( aStream : TStream; GifWidth, GifHeight : integer; DestPixels : pointer; DestWidth : integer; Interlaced : boolean );
    const
      PassIncrements : array[1..4] of byte = ( 8, 8, 4, 2 );
      PassStartRows  : array[1..4] of byte = ( 0, 4, 2, 1 );
    var
      XPos        : integer;
      YPos        : integer;
      ImageBufPtr : pchar;
      CurPixVal   : smallint;
      Pass        : byte;
      PassInc     : byte;

    function ReadDataBlock( out Buffer ) : integer;
      var
        Count : byte;
      begin
        Count := 0;
        if aStream.Read( Count, sizeof( Count ) ) <> sizeof( Count )
          then raise EGifError.Create( 'Corrupt GIF stream', errIllegalGIFStream )
          else
            begin
              Result        := Count;
              ZeroDataBlock := ( Count = 0 );
              if ( Count <> 0 ) and ( aStream.Read( Buffer, Count ) <> Count )
                then raise EGifError.Create( 'Corrupt GIF stream', errIllegalGIFStream );
            end;
      end;

    var
      CC : smallint;

    //  Pulled out of NextCode
    var
      CurBit   : smallint;
      LastBit  : smallint;
      LastByte : smallint;
      ReturnCC : boolean;
      GetDone  : boolean;
      Buffer   : array[0..279] of byte;

    function NextCode( CodSize : integer ) : integer;
      var
        i        : integer;
        j        : integer;
        Done     : integer;
        Count    : integer;
        RetCode  : longint;
      var  
        ExitFlag : boolean;
      begin
        if ReturnCC
          then
            begin
              ReturnCC := false;
              Result   := CC;
            end
          else
            begin
              Result   := -1;
              ExitFlag := false;
              Done     := CurBit + CodSize;
              if Done >= LastBit
                then
                  begin
                    if GetDone
                      then
                        begin
                          ExitFlag := true;
                          if CurBit >= LastBit
                            then raise EGifError.Create( 'Corrupt GIF stream', errUnknownError );
                        end
                      else
                        begin
                          Buffer[0] := Buffer[LastByte - 2];
                          Buffer[1] := Buffer[LastByte - 1];
                          Count     := ReadDataBlock( Buffer[2] );
                          if Count = 0
                            then GetDone := true;
                          if Count < 0
                            then
                              begin
                                Result   := -1;
                                ExitFlag := true;
                              end
                            else
                              begin
                                LastByte := 2 + Count;
                                CurBit   := ( CurBit - LastBit ) + 16;
                                LastBit  := ( 2 + Count ) * 8;
                                Done     := CurBit + CodSize;
                              end;
                        end;
                  end;
              if not ExitFlag
                then
                  begin
                    j := Done div 8;
                    i := CurBit div 8;
                    if i = j
                      then RetCode := longint( Buffer[i] )
                      else
                        if i + 1 = j
                          then RetCode := longint( Buffer[i] ) or ( longint( Buffer[i + 1] ) shl 8 )
                          else RetCode := longint( Buffer[i] ) or ( longint( Buffer[i + 1] ) shl 8 ) or ( longint( Buffer[i + 2] ) shl 16 );
                    RetCode := ( RetCode shr ( CurBit mod 8 ) ) and MaskTable[CodSize];
                    inc( CurBit, CodSize );
                    Result := integer( RetCode );
                  end;
            end;
      end;

    // Out of NextLZW
    var
      Stack       : array [0..1 shl MaxLZWBits * 2 - 1] of smallint;
      SP          : integer;

    var
      CodeSize    : smallint;
      SetCodeSize : smallint;
      MaxCode     : smallint;
      MaxCodeSize : smallint;
      EOI         : smallint;

    procedure InitLZW( InputCodeSize : integer );
      begin
        SetCodeSize := InputCodeSize;
        CodeSize    := SetCodeSize + 1;
        CC          := 1 shl SetCodeSize;
        EOI         := CC + 1;
        MaxCodeSize := 2 * CC;
        MaxCode     := CC + 2;
        LastBit     := 0;
        CurBit      := 0;
        LastByte    := 2;
        GetDone     := false;
        ReturnCC    := true;
        SP          := 0
      end;

    var
      Table     : array [0..1, 0..1 shl MaxLZWBits - 1] of smallint;
      FirstCode : smallint;
      OldCode   : smallint;

    function ReadLZW : smallint;
      var
        i        : integer;
        Count    : integer;
        Code     : smallint;
        InCode   : smallint;
        Buffer   : array [0..259] of byte;
        ExitFlag : boolean;
      begin
        if SP > 0
          then
            begin
              dec( SP );
              Result := Stack[SP];
            end
          else
            begin
              ExitFlag := false;
              Code     := NextCode( CodeSize );
              while ( Code >= 0 ) and not ExitFlag do
                begin
                  if Code = CC
                    then
                      begin
                        // corrupt GIFs can make this happen
                        if CC >= 1 shl MaxLZWBits
                          then raise EGifError.Create( 'Corrupt GIF file', errIllegalGIFStream )
                          else
                            begin
                              for i := 0 to CC - 1 do
                                begin
                                  Table[0][i] := 0;
                                  Table[1][i] := i;
                                end;
                              for i := CC to 1 shl MaxLZWBits - 1 do
                                begin
                                  Table[1][i] := 0;
                                  Table[0][i] := 0;
                                end;
                              CodeSize    := SetCodeSize + 1;
                              MaxCodeSize := 2 * CC;
                              MaxCode     := CC + 2;
                              SP          := 0;
                              repeat
                                OldCode   := NextCode( CodeSize );
                                FirstCode := OldCode;
                              until FirstCode <> CC;
                              Result   := FirstCode;
                              ExitFlag := true;
                            end;
                      end
                    else
                      if Code = EOI
                        then
                          begin
                            if not ZeroDataBlock
                              then
                                begin
                                  repeat
                                    Count := ReadDataBlock( Buffer );
                                  until Count <= 0;
                                  if Count <> 0
                                    then raise EGifError.Create( 'Missing EOD in data stream', errMissingEOD );
                                end;
                            Result   := -2;
                            ExitFlag := true;
                          end
                        else
                          begin
                            InCode := Code;
                            if Code >= MaxCode
                              then
                                begin
                                  Stack[SP] := FirstCode;
                                  inc( SP );
                                  Code := OldCode;
                                end;
                            while Code >= CC do
                              begin
                                Stack[SP] := Table[1][Code];
                                inc( SP );
                                if Code = Table[0][Code]
                                  then raise EGifError.Create( 'Circular table entry BIG ERROR', errLZWCircularTableEntry )
                                  else
                                    if SP >= sizeof( Stack )
                                      then raise EGifError.Create( 'Circular table Stack OVERFLOW!', errLZWStackOverflow )
                                      else Code := Table[0][Code];
                              end;
                            if not ExitFlag
                              then
                                begin
                                  FirstCode := Table[1][Code];
                                  Stack[SP] := FirstCode;
                                  inc( SP );
                                  Code := MaxCode;
                                  if Code < 1 shl MaxLZWBits
                                    then
                                      begin
                                        Table[0][Code] := OldCode;
                                        Table[1][Code] := FirstCode;
                                        inc( MaxCode );
                                        if ( MaxCode >= MaxCodeSize ) and ( MaxCodeSize < 1 shl MaxLZWBits )
                                          then
                                             begin
                                               MaxCodeSize := MaxCodeSize * 2;
                                               inc( CodeSize );
                                             end;
                                      end;
                                  OldCode := InCode;
                                  if SP > 0
                                    then
                                      begin
                                        dec( SP );
                                        Result   := Stack[SP];
                                        ExitFlag := true;
                                      end
                                    else Code := NextCode( CodeSize );
                                end;
                          end;
                end;
              if not ExitFlag
                then Result := Code;
            end;
      end;

    var
      InputCodeSize : byte;
    begin
      YPos    := 0;
      Pass    := 1;
      PassInc := PassIncrements[1];
      aStream.Read( InputCodeSize, sizeof( InputCodeSize ) );

      // Initialize the compression routines
      InitLZW( InputCodeSize );
      while YPos < GifHeight do
        begin
          ImageBufPtr := pchar(DestPixels) + YPos * DestWidth;
          for XPos := 0 to GifWidth - 1 do
            begin
              CurPixVal := ReadLZW;
              if CurPixVal >= 0
                then ImageBufPtr[XPos] := char( CurPixVal )
                else raise EGifError.Create( 'Illegal pixel value', errIllegalPixelValue );
            end;
          if Interlaced
            then
              begin
                inc( YPos, PassInc );
                if YPos >= GifHeight
                  then
                    begin
                      inc( Pass );
                      if Pass <= 4
                        then
                          begin
                            YPos    := PassStartRows[Pass];
                            PassInc := PassIncrements[Pass];
                          end;
                    end;
              end
            else inc( YPos );
        end;
      //while ReadLZW >= 0 do
      //  ;
    end;

end.
