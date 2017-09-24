unit VoyagerUIEvents;

interface

  uses
    Windows;

  const
    evnBase_UIEvents = 100;

  const
    evnScrollStart = evnBase_UIEvents + 0;
    evnScroll      = evnBase_UIEvents + 1;
    evnScrollEnd   = evnBase_UIEvents + 2;

  type
    TScrollDirection = (scrHorizontal, scrVertical);
    TScrollBias      = (sbsNone, sbsNegative, sbsPositive);
    TScrollDirInfo   = array[TScrollDirection] of TScrollBias;

  type
    TEvnScrollStartInfo =
      record
        MousePos : TPoint;
      end;

    TEvnScrollInfo =
      record
        DirInfo  : TScrollDirInfo;
        MousePos : TPoint;
      end;

    TEvnScrollEndInfo =
      record
        MousePos : TPoint;
      end;

implementation

end.


