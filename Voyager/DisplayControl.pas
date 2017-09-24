unit DisplayControl;

interface

  uses
    Display;

  procedure InitDisplay(const i: integer);
  procedure DoneDisplay;
  procedure Minimized;
  procedure Restore;
  procedure SetMode(const mo: integer);
  function GetPreferredDispInfoIndex : integer;

implementation

  uses
    Forms, Windows;

  var
    mode : integer;

//  {$IFOPT D-}
  function GetPreferredDispInfoIndex : integer;
    var
      i           : cardinal;
      backup      : integer;
      currMode    : cardinal;
      freq        : integer;
      needSecPass : boolean;
    begin
      if Display.DisplayManager.GetCurrentModeIndex( currMode )
        then freq := Display.DisplayManager.DispMode[currMode].DisplayFrequency
        else freq := -1;
      result := -1;
      backup := -1;
      i := 0;
      needSecPass := freq <> -1;
      repeat
        while (result = -1) and (i < DisplayManager.SupportedModeCount) do
          begin
            with DisplayManager.DispMode[i] do
              if (PelsWidth = 1024) and (PelsHeight = 768) and ((BitsPerPel = 32) or (BitsPerPel = 16)) and ((DisplayFrequency = freq) or (freq = -1))
                then
                  case BitsPerPel of
                    32 :
                      begin
                        result      := i;
                        freq        := -1;
                        needSecPass := false;
                      end;
                    16 :
                      begin
                        backup := i;
                        needSecPass := false;
                      end;
                  end;
            inc(i);
          end;
        if needSecPass and (freq <> -1)
          then freq := -1
          else needSecPass := false;

      until not needSecPass;

      if result = -1
        then Result := backup;
    end;

  procedure InitDisplay(const i: integer);
    var
      result : integer;
    begin
      Display.CreateDisplay( true );
      if i=-1
        then mode := GetPreferredDispInfoIndex
        else mode := i;
      if mode <> -1
        then
          if not DisplayManager.SetDispMode( mode, result ) or (result <> DISP_CHANGE_SUCCESSFUL)
            then mode := -1;
    end;

  procedure DoneDisplay;
    begin
      if mode <> -1
        then DisplayManager.Reset;
    end;

  procedure Minimized;
    begin
      if mode <> -1
        then
          begin
            DisplayManager.Reset;
          end;
    end;

  procedure Restore;
    var
      result : integer;
    begin
      if mode <> -1
        then DisplayManager.SetDispMode( mode, result );
    end;

  procedure SetMode(const mo: integer);
    var
      result : integer;
    begin
      if mo <> -1
        then
          begin
            mode := mo;
            DisplayManager.SetDispMode( mode, result );
          end;
    end;
  (*
  {$ELSE}

  procedure InitDisplay;
    begin
    end;

  procedure DoneDisplay;
    begin
    end;

  procedure Minimized;
    begin
    end;

  procedure Restore;
    begin
    end;


  {$ENDIF}

    *)
end.

