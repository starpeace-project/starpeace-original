unit SpoolPackets;

interface

  type
    TSpooledPacket =
      class
        public
          constructor Create(aBuffer : pchar; aSize : integer; OwnsPacket : boolean);
          destructor  Destroy; override;
        private
          fBuffer     : pchar;
          fSize       : integer;
          fIndex      : integer;
          fOwnsPacket : boolean;
        public
          procedure Move(count : integer);
          function  GetBuffer : pchar;
          function  GetCount : integer;
        public
          property Count  : integer read GetCount;
          property Buffer : pchar   read GetBuffer;
      end;

implementation

  // TSpooledPacket

  constructor TSpooledPacket.Create(aBuffer : pchar; aSize : integer; OwnsPacket : boolean);
    begin
      inherited Create;
      fOwnsPacket := OwnsPacket;
      if not OwnsPacket
        then fBuffer := aBuffer
        else
          begin
            GetMem(fBuffer, aSize);
            System.move(aBuffer[0], fBuffer[0], aSize);
          end;
      fSize := aSize;
    end;

  destructor TSpooledPacket.Destroy;
    begin
      if fOwnsPacket
        then FreeMem(fBuffer);
      inherited;
    end;

  procedure TSpooledPacket.Move(count : integer);
    begin
      inc(fIndex, count);
    end;

  function TSpooledPacket.GetBuffer : pchar;
    begin
      if fIndex < fSize
        then result := fBuffer + fIndex
        else result := nil;
    end;

  function TSpooledPacket.GetCount : integer;
    begin
      if fIndex < fSize
        then result := fSize - fIndex
        else result := 0;
    end;

end.
