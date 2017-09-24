// Wave files low level structure. Copyright Merchise Group [PastelCAD]
unit WaveLow;

interface

  uses
    Classes;

  type
    TWavFormat =
      packed record
        fID             : longint;  // fmt
        fLen            : longint;  // Data length
        fmtTag          : word;     // 1 = PCM ...
        nChannels       : word;
        nSamplesPerSec  : longint;
        nAvgBytesPerSec : longint;
        nBlockAlign     : word;     // nChannels * (nBitsPerSample div 8)
        FormatSpecific  : word;     // Bits per sample
      end;

    PWaveDataChunk = ^TWaveDataChunk;
    TWaveDataChunk =
      packed record
        dID     : longint;   // "data"
        dLen    : longint;   // data length + sizeof(TWaveDataChunk)

        { here comes Data }
      end;

    TWavData =
      packed record
        wID     : longint;   // "WAVE"
        Format  : TWavFormat;
        Chunk   : TWaveDataChunk;
      end;

    TWavFileFormat =
      packed record
        rID    : longint;   // "RIFF"
        rLen   : longint;   // Length of the data in the next chunk
        rData  : TWavData;  // rLen bytes length
      end;


    // All mixed
    PWaveHeader = ^TWaveHeader;
    TWaveHeader =
      packed record
        rID             : longint;  // "RIFF"
        rLen            : longint;  // Length of the data in the next chunk

        // Wave chunk
        wID             : longint;  // "WAVE"
        fID             : longint;  // fmt
        fLen            : longint;  // Data length

        fmtTag          : word;     // 1 = PCM ...
        nChannels       : word;
        nSamplesPerSec  : longint;
        nAvgBytesPerSec : longint;
        nBlockAlign     : word;     // nChannels * (nBitsPerSample div 8)
        nBitsPerSample  : word;     // Bits per sample
      end;

  const
    s_RIFF : longint = $46464952;
    s_WAVE : longint = $45564157;
    s_fmt  : longint = $20746D66;
    s_data : longint = $61746164;

  function  GetDataFromBuffer(aBuffer : pointer; var Size : integer) : pointer;
  procedure GetDataFromStream(aStream : TStream; var Size : integer);

implementation

  uses
    SysUtils;

  function GetDataFromBuffer(aBuffer : pointer; var Size : integer) : pointer;
    type
      PInteger = ^integer;
    var
      s : array[0..4] of char;
      Buffer : pchar absolute aBuffer;
    begin
      fillchar(s, sizeof(s), 0);
      while strcomp(s, 'data') <> 0 do
        begin
          move(Buffer^, s, 4);
          inc(Buffer);
        end;
      inc(Buffer, 3);
      Size := PInteger(Buffer)^;
      Result := Buffer + sizeof(integer);
    end;

  procedure GetDataFromStream(aStream : TStream; var Size : integer);
    var
      s : array[0..4] of char;
    begin
      fillchar(s, sizeof(s), 0);
      while strcomp(s, 'data') <> 0 do
        begin
          move(s[1], s[0], 3);
          aStream.Read(s[3], sizeof(char));
        end;
      aStream.Read(Size, sizeof(Size));
    end;

end.
