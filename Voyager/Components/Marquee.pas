unit Marquee;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  type
    TMarquee =
      class(TCustomControl)
        public
          constructor Create(AOwner: TComponent); override;
          destructor  Destroy; override;
        private
          fMarqueeText  : string;
          fCacheText    : string;
          fMarqueeWidth : integer;
          fCacheWidth   : integer;
          fOffset       : integer;
        private
          function MatchStrings(perc : byte) : boolean;
        protected
          procedure Paint; override;
          procedure WMEraseBkgnd(var Message: TMessage);
        public
          procedure Tick;
        published
          { Published declarations }
        end;

  procedure Register;

implementation

  uses
    MathUtils;

  // TMarquee

  constructor TMarquee.Create(AOwner: TComponent);
    begin
      inherited;
      Width := 200;
      Height := 50;
    end;

  destructor TMarquee.Destroy;
    begin
      inherited;
    end;

  function TMarquee.MatchStrings(perc : byte) : boolean;
    var
      maxLen : integer;
      minLen : integer;
      mrqLen : integer;
      chLen  : integer;
    begin
      mrqLen := length(fMarqueeText);
      chLen  := length(fCacheText);
      maxLen := max(mrqLen, chLen);
      minLen := min(mrqLen, chLen);
      result := 100*(maxLen - minLen)/maxLen >= perc;
    end;

  procedure TMarquee.Paint;
    begin
      Canvas.TextOut(fOffset, 0, fMarqueeText);
    end;

  procedure TMarquee.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TMarquee.Tick;
    begin
      if Width + fOffset = 0
        then fOffset := Width
        else dec(fOffset, 10); // >> 
    end;

  procedure Register;
    begin
      RegisterComponents('Five', [TMarquee]);
    end;

end.
