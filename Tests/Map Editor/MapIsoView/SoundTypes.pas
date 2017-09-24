unit SoundTypes;

interface

  type
    TSoundInfo =
      record
        name     : string;
        kind     : integer;
        priority : integer;
        looped   : boolean;
        volume   : single; //  0 .. 1
        pan      : single; // -1 .. 1
      end;

implementation

end.


