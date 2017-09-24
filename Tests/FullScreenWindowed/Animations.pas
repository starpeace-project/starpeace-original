unit Animations;

interface

  uses
    Windows, SpriteImages;

  type
    TAnimation =
      class
        public
          constructor Create(Image : TFrameImage; x, y : integer);
          destructor  Destroy; override;
        public
          procedure Tick;
        private
          fImage           : TFrameImage;
          fPos             : TPoint;
          fFrame           : dword;
          fLastFrameUpdate : dword;
        public
          property Image : TFrameImage read fImage;
          property Pos   : TPoint      read fPos;
          property Frame : dword       read fFrame;
      end;

implementation

  uses
    Classes;

  constructor TAnimation.Create(Image : TFrameImage; x, y : integer);
    begin
      inherited Create;
      fImage := Image;
      fPos := Point(x, y);
      fLastFrameUpdate := GetTickCount;
    end;

  destructor TAnimation.Destroy;
    begin
      inherited;
    end;

  procedure TAnimation.Tick;
    var
      curtickcount : cardinal;
    begin
      curtickcount := GetTickCount;
      if curtickcount - fLastFrameUpdate > fImage.FrameDelay[fFrame]
        then
          begin
            if fFrame < pred(fImage.FrameCount)
              then inc(fFrame)
              else fFrame := 0;
            fLastFrameUpdate := curtickcount;
          end;
    end;

end.
