unit Flics;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    GDI;

  const
    // Magic signatures 
    idHeaderFLI = $AF11;
    idHeaderFLC = $AF12;
    idPrefix    = $F100;
    idFrame     = $F1FA;

    // types of chunk in a FLI frame 
    idColor  = 11;
    idLC     = 12;
    idBlack  = 13;
    idBRun   = 15;
    idCopy   = 16;

    // types of chunk added by FLC 
    idColor256 = 4;
    idSS2      = 7;
    idXlat256  = 18; // Internal to a PSTAMP chunk 
    idPStamp   = 18;

    // bit defines for flags field
    idFinished = $01;   // finished writing FLI 
    idLooped   = $02;   // FLI has a loop frame 

  type
    TOldFliHeader =
      packed record
        Size     : longint; // size of entire animation file 
        Magic    : word;    // always $AF11 
        Frames   : word;    // Should be < 4000 
        Width    : word;    // 320 
        Height   : word;    // 200 
        Depth    : word;    // always 8 
        Flags    : word;    // always 0 
        Speed    : word;    // # of jiffies to delay between frames (1/70s) 
        Reserved : array[0..109] of byte;
      end;

  type
    TFliHeader =
      packed record
        Size      : longint; // size of entire animation file
        Magic     : word;    // always $AF12
        Frames    : word;    // Should be < 4000, doesn't include ring frame
        Width     : word;
        Height    : word;
        Depth     : word;    // always 8
        Flags     : word;    // Set to 3 after ring frame is written and header
                             // updated. Indicates that the file was properly
                             // finished and closed
        Speed     : longint; // # of miliseconds to delay between frames (1/1000s)
        Reserved1 : word;
        Created   : longint; // MSDOS-formatted datetime of file's creation
        Creator   : longint; // Serial # of fli creator (FlicLib = 'FLIB')
        Updated   : longint; // MSDOS-formatted datetime of file's most recent update
        Updater   : longint; // Serial # of fli updater (FlicLib = 'FLIB')
        AspectX   : word;
        AspectY   : word;    // A 320x200 flic is usually 6:5
        Reserved2 : array[0..37] of byte;
        oFrame1   : longint; // Offset from the beginning of the file to the first frame chunk
        oFrame2   : longint; // Offset from the beginning of the file to the 2nd frame chunk
        Reserved3 : array[0..39] of byte;
      end;

  type
    TFliPrefix =
      packed record
        Size     : longint;
        Magic    : word;
        Chunks   : word;
        Reserved : array[0..7] of byte;
      end;

  type
    PFliFrame = ^TFliFrame;
    TFliFrame =
      packed record
        Size     : longint;
        Magic    : word;
        Chunks   : word;
        Reserved : array[0..7] of byte;
      end;

  type
    TFliChunkGeneric =
      packed record
        Size  : longint;
        Magic : word;
      end;

  type
    PFliChunk = ^TFliChunk;
    TFliChunk = TFliChunkGeneric;

  type
    PColorPacket = ^TColorPacket;
    TColorPacket =
      packed record
        Skip  : byte;
        Count : byte;
        Rgb   : array[0..0] of TDosRgb;
      end;

    PFliChunkColor256 = ^TFliChunkColor256;
    TFliChunkColor256 =      // FLI_COLOR256: 256-level colors
      packed record
        Size    : longint;
        Magic   : word;
        Count   : word;
        Packets : array[0..0] of TColorPacket;
      end;

  type                       // FLI_COLOR: 64-level colors
    PFliChunkColor = PFliChunkColor256;
    TFliChunkColor = TFliChunkColor256;

  type
    PFliChunkBlack = ^TFliChunkBlack;
    TFliChunkBlack =         // FLI_BLACK: No data
      packed record
        Size  : longint;
        Magic : word;
      end;

  type
    PFliChunkCopy = ^TFliChunkCopy;
    TFliChunkCopy =          // FLI_COPY: No compression 
      packed record
        Size   : longint;
        Magic  : word;
        Pixels : array[0..0] of byte;
      end;

  type
    TBRunPacket =
      packed record
        TypeSize : byte;
        Pixels   : array[0..0] of byte;
      end;

    TBRunLinePacket =
      packed record
        Count   : byte;      // Not useful, just ignore this field
        Packets : array[0..0] of TBRunPacket;
      end;

    PFliChunkBRun = ^TFliChunkBRun;
    TFliChunkBRun =          // FLI_BRUN: Byte run length compression
      packed record
        Size  : longint;
        Magic : word;
        Lines : array[0..0] of TBRunLinePacket;
      end;

  type
    TLCPacket =
      packed record
        Skip     : byte;
        TypeSize : byte;
        Pixels   : array[0..0] of byte;
      end;

    TLCLinePacket =
      packed record
        x       : byte;
        Count   : byte;
        Packets : array[0..0] of TLCPacket;
      end;

    PFliChunkLC = ^TFliChunkLC;
    TFliChunkLC =            // FLI_LC: Byte aligned delta compression
      packed record
        Size   : longint;
        Magic  : word;
        DeltaY : word;
        Count  : word;
        Lines  : array[0..0] of TLCLinePacket;
      end;

  type
    TSS2Packet =
      packed record
        Skip     : byte;
        TypeSize : byte;
        Pixels   : array[0..0] of word;
      end;

    TSS2HeaderPacket =
      packed record
        case TypeSize : word of
          1 : // TypeSize & 3FFF has Packet Count
            (Packets : array[0..0] of TSS2Packet);
          2 : // Low order byte is last byte of line
            ();
          3 : // TypeSize & 3FFF has Line Skip Count
            ();
      end;

    TSS2LinePacket =
      packed record
        Header : array[0..0] of TSS2HeaderPacket;
      end;

    PFliChunkSS2 = ^TFliChunkSS2; 
    TFliChunkSS2 =           // FLI_SS2: Word aligned delta compression
      packed record
        Size  : longint;
        Magic : word;
        Count : word;
        Lines : array[0..0] of TSS2LinePacket;
      end;

  type
    PStampChunkXlat256 = ^TStampChunkXlat256;
    TStampChunkXlat256 =     // Color translation table
      packed record
        Size  : longint;
        Magic : word;
        Table : array[0..255] of byte;
      end;

  type
    PFliChunkPStamp = ^TFliChunkPStamp;
    TFliChunkPStamp =        // FLI_PSTAMP: Postage Stamp Image
      packed record
        Size   : longint;
        Magic  : word;
        Height : word;
        Width  : word;
        xlate  : word;
      end;

  const
    FliTimerResolution = 14; // 14.27ms = 1jiffie

  var
    SixCubePalette : T256LogPalette;

  procedure InitSixCubePalette;
  function  SixCubeIndex( r, g, b : byte) : integer;

implementation

  function SixCubeIndex( r, g, b : byte) : integer;
    begin
      Result := 10 +
        ((r * 6) div 256) * 36 +
        ((6 * g) div 256) * 6 +
        ((6 * b) div 256);
    end;

  procedure InitSixCubePalette;
    var
      r, g, b, i : byte;
    begin
      i := 10;
      for r := 0 to 5 do
        for g := 0 to 5 do
          for b := 0 to 5 do
            begin
              with SixCubePalette.Entries[i] do
                begin
                  peRed   := r * 256 div 6;
                  peGreen := g * 256 div 6;
                  peBlue  := b * 256 div 6;
                  peFlags := 0;
                end;
              inc( i);
            end;
    end;

end.
