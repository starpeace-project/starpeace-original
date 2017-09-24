unit GraphEditors;

interface

  uses
    Classes, Forms, Dialogs, ExtDlgs, DsgnIntf, TypInfo, SysUtils,
    CanvasBmp, SpeedBmp, PlayerAnim, PlayerFlic, PlayerRle, PlayerGif;

  type
    TSpeedBitmapProperty =
      class( TClassProperty )
        public
          procedure SetValue( const Value: string );                                            override;
          procedure Edit;                                                                       override;
          function  GetAttributes : TPropertyAttributes;                                        override;
      end;

  type
    TFlicPlayerProperty =
      class( TClassProperty )
        public
          procedure SetValue( const Value: string );                                            override;
          procedure Edit;                                                                       override;
          function  GetAttributes : TPropertyAttributes;                                        override;
      end;

  type
    TRlePlayerProperty =
      class( TClassProperty )
        public
          procedure SetValue( const Value: string );                                            override;
          procedure Edit;                                                                       override;
          function  GetAttributes : TPropertyAttributes;                                        override;
      end;

  type
    TGifPlayerProperty =
      class( TClassProperty )
        public
          procedure SetValue( const Value: string );                                            override;
          procedure Edit;                                                                       override;
          function  GetAttributes : TPropertyAttributes;                                        override;
      end;

  type
    TStretchBltModeProperty =
      class( TOrdinalProperty )
        public
          function  GetAttributes : TPropertyAttributes;                                        override;
          procedure GetValues( Proc : TGetStrProc );                                            override;
          procedure SetValue( const Value: string );                                            override;
          function  GetValue: string;                                                           override;
      end;

  procedure Register;

implementation

  uses
    CommRez;
    
  // TStretchBltMode

  type
    TStretchValue =
      record
        Name  : string;
        Value : integer;
      end;

  const
    StretchValues : array[0..4] of TStretchValue =
      (
        ( Name : 'stUnknown'; Value : -1 ),
        ( Name : 'stAndScans'; Value : 1 ),
        ( Name : 'stOrScans'; Value : 2 ),
        ( Name : 'stDeleteScans'; Value : 3 ),
        ( Name : 'stHalftone'; Value : 4 )
      );

  procedure TStretchBltModeProperty.GetValues( Proc : TGetStrProc );
    var
      i : integer;
    begin
      for i := Low( StretchValues ) + 1 to High( StretchValues ) do
        Proc( StretchValues[i].Name );
    end;

  procedure TStretchBltModeProperty.SetValue( const Value: string );
    var
      LoCase : string;
      i      : integer;
    begin
      LoCase := LowerCase( Value );
      for i := Low( StretchValues ) + 1 to High( StretchValues ) do
        if LowerCase( StretchValues[i].Name ) = LoCase
          then SetOrdValue( StretchValues[i].Value );
    end;

  function TStretchBltModeProperty.GetValue: string;
    begin
      Result := StretchValues[GetOrdValue].Name;
    end;

  function TStretchBltModeProperty.GetAttributes : TPropertyAttributes;
    begin
      Result := [paValueList, paRevertable];
    end;

  // TSpeedBitmap

  procedure TSpeedBitmapProperty.Edit;
  var
    OpenDialog : TOpenPictureDialog;
  begin
    OpenDialog := TOpenPictureDialog.Create( Application );
    with OpenDialog do
      try
        Options    := [ofFileMustExist, ofHideReadOnly];
        Filter     := SBitmapFilesMask;
        DefaultExt := 'bmp';
        if Execute
          then SetValue( Filename );
      finally
         OpenDialog.Free;
      end;
  end;

  function TSpeedBitmapProperty.GetAttributes : TPropertyAttributes;
    begin
      if GetOrdValue <> 0
        then Result := [paDialog, paSubproperties, paRevertable]
        else Result := [paDialog, paRevertable];
    end;

  procedure TSpeedBitmapProperty.SetValue( const Value: string );
    var
      Bitmap : TSpeedBitmap;
    begin
      Bitmap := TSpeedBitmap( GetOrdValue );
      if not Assigned( Bitmap )
        then
          begin
            Bitmap := TSpeedBitmap.Create;
            SetOrdValue( integer( Bitmap ) );
          end;
      with Bitmap do
        if Value <> ''
          then LoadFromFile( Value )
          else Empty := true;
    end;

  // TFlicPlayer

  procedure TFlicPlayerProperty.Edit;
  var
    OpenDialog : TOpenPictureDialog;
  begin
    OpenDialog := TOpenPictureDialog.Create( Application );
    with OpenDialog do
      try
        Options    := [ofFileMustExist, ofHideReadOnly];
        Filter     := SBitmapFilesMask;
        DefaultExt := '.bmp';
        if Execute
          then SetValue( Filename );
      finally
         OpenDialog.Free;
      end;
  end;

  function TFlicPlayerProperty.GetAttributes : TPropertyAttributes;
    begin
      Result := [paDialog, paRevertable];
    end;

  procedure TFlicPlayerProperty.SetValue( const Value: string );
    var
      Flic : TFlicPlayer;
    begin
      Flic := TFlicPlayer( GetOrdValue );
      (*
      if not Assigned( Flic )
        then
          begin
            Flic := TFlicPlayer.Create;
            SetOrdValue( integer( Flic ) );
          end;
      *)
      with Flic do
        LoadFromFile( Value );
    end;

  // TGifPlayer

  procedure TGifPlayerProperty.Edit;
  var
    OpenDialog : TOpenPictureDialog;
  begin
    OpenDialog := TOpenPictureDialog.Create( Application );
    with OpenDialog do
      try
        Options    := [ofFileMustExist, ofHideReadOnly];
        Filter     := SGifFilesMask;
        DefaultExt := '.gif';
        if Execute
          then SetValue( Filename );
      finally
         OpenDialog.Free;
      end;
  end;

  function TGifPlayerProperty.GetAttributes : TPropertyAttributes;
    begin
      Result := [paDialog, paRevertable];
    end;

  procedure TGifPlayerProperty.SetValue( const Value: string );
    var
      Gif : TGifPlayer;
    begin
      Gif := TGifPlayer( GetOrdValue );
      (*
      if not Assigned( Gif )
        then
          begin
            Gif := TGifPlayer.Create;
            SetOrdValue( integer( Gif ) );
          end;
      *)
      with Gif do
        LoadFromFile( Value );
    end;

  // TRlePlayer

  procedure TRlePlayerProperty.Edit;
  var
    OpenDialog : TOpenPictureDialog;
  begin
    OpenDialog := TOpenPictureDialog.Create( Application );
    with OpenDialog do
      try
        Options    := [ofFileMustExist, ofHideReadOnly];
        Filter     := SRleAnimFilesMask;
        DefaultExt := '.rla';
        if Execute
          then SetValue( Filename );
      finally
         OpenDialog.Free;
      end;
  end;

  function TRlePlayerProperty.GetAttributes : TPropertyAttributes;
    begin
      Result := [paDialog, paRevertable];
    end;

  procedure TRlePlayerProperty.SetValue( const Value: string );
    var
      Rle : TRlePlayer;
    begin
      Rle := TRlePlayer( GetOrdValue );
      (*
      if not Assigned( Rle )
        then
          begin
            Rle := TRlePlayer.Create;
            SetOrdValue( integer( Rle ) );
          end;
      *)
      with Rle do
        LoadFromFile( Value );
    end;

  // VCL Registration

  procedure Register;
    begin
      RegisterPropertyEditor( TypeInfo( TStretchBltMode ), nil, '', TStretchBltModeProperty );
      RegisterPropertyEditor( TypeInfo( TSpeedBitmap ), nil, '', TSpeedBitmapProperty );
      RegisterPropertyEditor( TypeInfo( TFlicPlayer ), nil, '', TFlicPlayerProperty );
      RegisterPropertyEditor( TypeInfo( TGifPlayer ), nil, '', TGifPlayerProperty );
      RegisterPropertyEditor( TypeInfo( TRlePlayer ), nil, '', TRlePlayerProperty );
    end;

end.
