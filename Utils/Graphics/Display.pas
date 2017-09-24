unit Display;

// TDisplay handles, list and change between disp modes, (c) merchise, hcg

interface

  uses
    Windows;

  type  // we just keep a part of the DEVMODE structure (the usefull one)
    TDispInfo =
      packed record
        BitsPerPel        : DWORD;
        PelsWidth         : DWORD;
        PelsHeight        : DWORD;
        DisplayFlags      : DWORD;
        DisplayFrequency  : DWORD;
      end;

  type
    TDispInfoArray = array of TDispInfo;

  type
    TDisplay =
      class
        private
          fSupportedModes     : TDispInfoArray;
          fSupportedModeCount : cardinal;
        private
          fModesEnumerated  : boolean;
          fRestartAllowed   : boolean;
          fTestBeforeChange : boolean;
        private
          fCurrentModeKnown : boolean;
          fCurrentModeIndex : cardinal;  // Valid only if running on NT
        protected
          constructor Create(const EnumModes : boolean); // Can not be created by you. Call CreateDisplay instead
        public
          destructor  Destroy; override;
        public
          procedure EnumSupportedModes;
          function  SetDispMode(const aMode : TDispInfo; var ExtendedResult : longint) : boolean; overload;
          function  SetDispMode(const aIndex : cardinal; var ExtendedResult : longint) : boolean; overload;
          function  Reset : boolean;
        public // be carefull, use wisely, you can freeze the system (in win9x)
          function  SetDispModeEx(const aMode : TDispInfo; const aFlags : DWORD; var ExtendedResult : longint) : boolean; overload;
          function  SetDispModeEx(const aIndex : cardinal; const aFlags : DWORD; var ExtendedResult : longint) : boolean; overload;
        public
          function GetCurrentModeIndex(var aIndex : cardinal) : boolean;
        public
          function IndexOf(const aMode : TDispInfo) : cardinal;
        private
          function  DispInfoFromDevMode(const aMode : DEVMODE) : TDispInfo;
          procedure NewDispMode(const aMode : DEVMODE);
          function  ChangeDispMode(const aIndex : cardinal; const aFlags : DWORD) : longint;
          function  ObtainCurrentModeIndex(var aIndex : cardinal) : boolean;
        private
          function GetDispMode(Index : cardinal) : TDispInfo;
        public
          property DispMode[index : cardinal] : TDispInfo read GetDispMode;
          property SupportedModeCount : cardinal read FSupportedModeCount;
          property RestartAllowed : boolean read fRestartAllowed write fRestartAllowed;
          property TestBeforeChange : boolean read fTestBeforeChange write fTestBeforeChange;
      end;

  var
    DisplayManager : TDisplay;

  procedure CreateDisplay(EnumModes : boolean);
  procedure FreeDisplay;

implementation

  uses
    WinVersion;

  const
    ENUM_CURRENT_SETTINGS = cardinal(-1);

  procedure CreateDisplay(EnumModes : boolean);
    begin
      if DisplayManager = nil
        then DisplayManager := TDisplay.Create(EnumModes);
    end;

  procedure FreeDisplay;
    begin
      DisplayManager.Free;
      DisplayManager := nil;
    end;

  // TDisplay

  constructor TDisplay.Create(const EnumModes: boolean);
    begin
      inherited Create;
      fTestBeforeChange := true;
      if EnumModes
        then EnumSupportedModes;
    end;

  destructor TDisplay.Destroy;
    begin
      fSupportedModes := nil; // to deallocate the dinamic array memory;
      inherited;
    end;

  procedure TDisplay.EnumSupportedModes;
    var
      CurrentModeIdx : cardinal;
      CurrentMode    : DEVMODE;
    begin
      if not fModesEnumerated
        then
          begin
            CurrentModeIdx := 0;
            while EnumDisplaySettings(nil, CurrentModeIdx, CurrentMode) do
              begin
                NewDispMode(CurrentMode);
                Inc(CurrentModeIdx);
              end;
            fModesEnumerated := true;
          end;
      fCurrentModeKnown := ObtainCurrentModeIndex(fCurrentModeIndex);
    end;

  function TDisplay.SetDispMode(const aMode : TDispInfo; var ExtendedResult : longint) : boolean;
    begin
      Result := SetDispMode(IndexOf(aMode), ExtendedResult)
    end;

  function TDisplay.SetDispMode(const aIndex : cardinal; var ExtendedResult : longint) : boolean;
    var
      aFlags   : cardinal;
    begin
      Assert(fModesEnumerated and (aIndex < SupportedModeCount));
      aFlags := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
      ExtendedResult := ChangeDispMode(aIndex, aFlags);
      case ExtendedResult of
        DISP_CHANGE_SUCCESSFUL :
          Result := true;
        DISP_CHANGE_RESTART :
          Result := RestartAllowed;
        DISP_CHANGE_BADFLAGS, DISP_CHANGE_FAILED, DISP_CHANGE_BADMODE :
          Result := false;
        else
          Result := false;
      end;
    end;

  function TDisplay.Reset : boolean;
    var
      ErrCode : Longint;
      PDevMode : ^DEVMODE;
    begin
      PDevMode := nil;
      ErrCode := ChangeDisplaySettings(PDevMode^, CDS_FULLSCREEN);
      case ErrCode of
        DISP_CHANGE_SUCCESSFUL :
          Result := true;
        DISP_CHANGE_RESTART :
          Result := RestartAllowed;
        DISP_CHANGE_BADFLAGS, DISP_CHANGE_FAILED, DISP_CHANGE_BADMODE :
          Result := false;
        else
          Result := false;
      end;
    end;

  function  TDisplay.SetDispModeEx(const aMode : TDispInfo; const aFlags : DWORD; var ExtendedResult : longint) : boolean;
    begin
      Result := SetDispModeEx(IndexOf(aMode), aflags, ExtendedResult)
    end;

  function  TDisplay.SetDispModeEx(const aIndex : cardinal; const aFlags : DWORD; var ExtendedResult : longint) : boolean;
    begin
      Assert(fModesEnumerated and (aIndex < SupportedModeCount));
      ExtendedResult := ChangeDispMode(aIndex, aFlags);
      case ExtendedResult of
        DISP_CHANGE_SUCCESSFUL :
          Result := true;
        DISP_CHANGE_RESTART :
          Result := RestartAllowed;
        DISP_CHANGE_BADFLAGS, DISP_CHANGE_FAILED, DISP_CHANGE_BADMODE :
          Result := false;
        else
          Result := false;
      end;
    end;

  function TDisplay.GetCurrentModeIndex(var aIndex : cardinal) : boolean;
    begin
      Result := fCurrentModeKnown;
      if fCurrentModeKnown
        then aIndex := fCurrentModeIndex
        else aIndex := MaxInt;
    end;

  function TDisplay.IndexOf(const aMode : TDispInfo) : cardinal;
    var
      i : cardinal;
    begin
      Assert(fModesEnumerated);
      Result := MaxInt;
      i := 0;
      while (i < SupportedModeCount) and (Result = cardinal(MaxInt)) do
        begin
          if ( (fSupportedModes[i].BitsPerPel = aMode.BitsPerPel) and
               (fSupportedModes[i].PelsWidth  = aMode.PelsWidth)  and
               (fSupportedModes[i].PelsHeight = aMode.PelsHeight) )
            then Result := i;
          Inc(i);
        end;
    end;

  function  TDisplay.DispInfoFromDevMode(const aMode : DEVMODE) : TDispInfo;
    begin
      with Result, aMode do
        begin
          BitsPerPel        :=  dmBitsPerPel;
          PelsWidth         :=  dmPelsWidth;
          PelsHeight        :=  dmPelsHeight;
          DisplayFlags      :=  dmDisplayFlags;
          DisplayFrequency  :=  dmDisplayFrequency;
        end;
    end;

  procedure TDisplay.NewDispMode(const aMode : DEVMODE);
    begin
      if Pred(SupportedModeCount) = cardinal(High(fSupportedModes))
        then SetLength(fSupportedModes, Length(fSupportedModes) + 4);
      fSupportedModes[SupportedModeCount] := DispInfoFromDevMode(aMode);
      Inc(fSupportedModeCount);
    end;

  function  TDisplay.ChangeDispMode(const aIndex : cardinal; const aFlags : DWORD) : longint;
    var
      aDevMode : DEVMODE;
      Change : boolean;
    begin
      Result := DISP_CHANGE_FAILED; // to avoid the warn
      EnumDisplaySettings(nil, aIndex, aDEVMODE);
      aDEVMODE.dmFields := aFlags;
      if TestBeforeChange
        then
          begin
            Result := ChangeDisplaySettings(aDevMode,CDS_TEST);
            case Result of
              DISP_CHANGE_SUCCESSFUL :
                Change := true;
              DISP_CHANGE_RESTART :
                Change := RestartAllowed;
              DISP_CHANGE_BADFLAGS, DISP_CHANGE_FAILED, DISP_CHANGE_BADMODE :
                Change := false;
              else Change := false;
            end;
          end
        else Change := true;
      if Change
        then Result := ChangeDisplaySettings(aDevMode, CDS_FULLSCREEN);
    end;

  function TDisplay.ObtainCurrentModeIndex(var aIndex : cardinal) : boolean;
    var
      aDevMode : DEVMODE;
    begin
      Assert(fModesEnumerated);
      if IsNT and EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, aDevMode)
        then
          begin
            aIndex := IndexOf(DispInfoFromDevMode(aDevMode));
            Result := true;
          end
        else
          begin
            aIndex := MaxInt;
            Result := false;
          end;
    end;

  function TDisplay.GetDispMode(Index: cardinal): TDispInfo;
    begin
      Assert(fModesEnumerated and (Index < SupportedModeCount));
      Result := fSupportedModes[Index];
    end;

end.
