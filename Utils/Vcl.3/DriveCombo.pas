unit DriveCombo;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, Messages, SysUtils, Classes, Controls, StdCtrls, 
    Graphics;

  // TDriveComboBox

  type
    TFilterDrive = procedure( Drive : char; var SkipDrive : boolean ) of object;

  type
    TDriveType = ( dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM );

  type
    TTextCase = ( tcLowerCase, tcUpperCase );

  type  
    TDrivesCombo =
      class( TCustomComboBox )
        private
          fDrive       : char;
          fTextCase    : TTextCase;
          fOnFilterDrive : TFilterDrive;

          procedure CMFontChanged( var Message : TMessage );                                                                    message CM_FONTCHANGED;
          procedure SetDrive( NewDrive : char );
          procedure SetTextCase( NewTextCase : TTextCase );
          procedure ReadBitmaps;
          procedure ResetItemHeight;

        protected
          FloppyBMP, FixedBmp, NetworkBmp, CdromBmp, RamBmp : TBitmap;

          procedure CreateWnd;                                                                                                 override;
          procedure DrawItem( Index : integer; Rect : TRect; State : TOwnerDrawState );                                           override;
          procedure Click;                                                                                                     override;
          procedure BuildList;                                                                                                 virtual;

        public
          constructor Create( AOwner : TComponent );                                                                            override;
          destructor Destroy;                                                                                                  override;

        public
          property Drive : char                 read fDrive         write SetDrive;

        published
          property TextCase : TTextCase         read fTextCase      write SetTextCase default tcLowerCase;
          property OnFilterDrive : TFilterDrive read fOnFilterDrive write fOnFilterDrive;

        public
          property Text;
          
        published
          property Color;
          property Ctl3D;
          property DragMode;
          property DragCursor;
          property Enabled;
          property Font;
          property ImeMode;
          property ImeName;
          property ParentColor;
          property ParentCtl3D;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
          property OnChange;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnDropDown;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnKeyDown;
          property OnKeyPress;
          property OnKeyUp;
          property OnStartDrag;
        end;

  // Component registration

  procedure Register;

implementation

  uses
    GDI, FilenameUtils, NetUtils, NumUtils;

  {$R *.res}
  {$R *.dcr}

  // TDrivesCombo

  constructor TDrivesCombo.Create( AOwner : TComponent );
    var
      Temp : string;
    begin
      inherited;

      Style := csOwnerDrawFixed;
      ReadBitmaps;
      GetDir( 0, Temp );
      fDrive := Temp[1]; // Make default drive selected
      if fDrive = '\'
        then fDrive := #0;
      ResetItemHeight;
    end;

  destructor TDrivesCombo.Destroy;
    begin
      FloppyBmp.Free;
      FixedBmp.Free;
      NetworkBmp.Free;
      CdromBmp.Free;
      RamBmp.Free;

      inherited;
    end;

  procedure TDrivesCombo.BuildList;
    var
      DriveNum  : integer;
      DriveChar : char;
      DriveType : TDriveType;
      DriveBits : set of 0..25;
      SkipDrive : boolean;

      procedure AddDrive( const VolName : string; Obj : TObject );
        begin
          Items.AddObject( Format( '%s: %s', [DriveChar, VolName] ), Obj );
        end;

    begin
      // Fill list
      Clear;
      integer( DriveBits ) := GetLogicalDrives;
      for DriveNum := 0 to 25 do
        begin
          DriveChar := char( DriveNum + Ord( 'a' ) );
          SkipDrive := false;
          if Assigned( OnFilterDrive )
            then OnFilterDrive( DriveChar, SkipDrive )
            else SkipDrive := not ( DriveNum in DriveBits );
          if not SkipDrive
            then
              begin
                DriveType := TDriveType( GetDriveType( pchar( DriveChar + ':\' ) ) );
                if TextCase = tcUpperCase
                  then DriveChar := Upcase( DriveChar );

                case DriveType of
                  dtFloppy :
                    Items.AddObject( DriveChar + ':', FloppyBmp );
                  dtFixed :
                    AddDrive( VolumeID( DriveChar ), FixedBmp );
                  dtNetwork :
                    AddDrive( NetworkPath( DriveChar ), NetworkBmp );
                  dtCDROM :
                    AddDrive( VolumeID( DriveChar ), CdromBmp );
                  dtRAM :
                    AddDrive( VolumeID( DriveChar ), RamBmp );
                  else
                    Items.AddObject( DriveChar + ':', FixedBmp );
                end;
              end;
        end;
    end;

  procedure TDrivesCombo.SetDrive( NewDrive : char );
    var
      Item : integer;
      drv  : string;
    begin
      if ( ItemIndex < 0 ) or ( UpCase( NewDrive ) <> UpCase( fDrive ) )
        then
          begin
            if NewDrive = #0
              then
                begin
                  fDrive := NewDrive;
                  ItemIndex := -1;
                end
              else
                begin
                  if TextCase = tcUpperCase
                    then fDrive := UpCase( NewDrive )
                    else fDrive := Chr( ord( UpCase( NewDrive ) ) + 32 );

                  // Change selected item
                  for Item := 0 to Items.Count - 1 do
                    begin
                      drv := Items[Item];
                      if ( UpCase( drv[1] ) = UpCase( fDrive ) ) and ( drv[2] = ':' )
                        then
                          begin
                            ItemIndex := Item;
                            break;
                          end;
                    end;
                end;
            Change;
          end;
    end;

  procedure TDrivesCombo.SetTextCase( NewTextCase : TTextCase );
    var
      OldDrive : char;
    begin
      fTextCase := NewTextCase;
      OldDrive  := fDrive;
      BuildList;
      SetDrive( OldDrive );
    end;

  procedure TDrivesCombo.CreateWnd;
    var
      i : integer;
    begin
      inherited;

      BuildList;
      i := 0;
      while ( i < Items.Count ) and ( UpperCase( Items[i][1] ) <> UpperCase( fDrive ) ) do
        inc( i );
      if i < Items.Count
        then SetDrive( fDrive )
        else SetDrive( Items[0][1] );
    end;

  procedure TDrivesCombo.DrawItem( Index : integer; Rect : TRect; State : TOwnerDrawState );
    var
      Str       : pchar;
      Bitmap    : TBitmap;
      BufRect   : TRect;
      BufBmp    : TBitmap;
    begin
      BufRect := Rect;
      OffsetRect( BufRect, -Rect.Left, -Rect.Top );

      BufBmp := TBitmap.Create;
      try
        BufBmp.Width  := BufRect.Right;
        BufBmp.Height := BufRect.Bottom;

        with BufBmp.Canvas do
          begin
            Font.Assign( Canvas.Font );
            Pen.Assign( Canvas.Pen );
            Brush.Assign( Canvas.Brush );
            FillRect( BufRect );
            Bitmap   := TBitmap( Items.Objects[Index] );
            if Assigned( Bitmap )
              then
                with Bitmap do
                  begin
                    BrushCopy( Bounds( 2, ( BufRect.Bottom - Height ) div 2, Width, Height ),
                               Bitmap, Bounds( 0, 0, Width, Height ), Canvas.Pixels[0, Height - 1] );
                  end;
             // Uses DrawText instead of TextOut in order to get clipping against
             //  the combo box button
            Str := pchar( Items[Index] );
            BufRect.Left := 22;
            DrawText( Handle, Str, 2, BufRect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX );
            if Str[3] <> #0
              then
                begin
                  BufRect.Left := BufRect.Left + 35;
                  DrawText( Handle, Str + 2, -1, BufRect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX );
                end;
          end;
        Canvas.Draw( Rect.Left, Rect.Top, BufBmp );
      finally
        BufBmp.Free;
      end;
    end;

  procedure TDrivesCombo.Click;
    begin
      inherited;
      
      if ItemIndex >= 0
        then Drive := Items[ItemIndex][1];
    end;

  procedure TDrivesCombo.CMFontChanged( var Message : TMessage );
    begin
      inherited;
      
      ResetItemHeight;
      RecreateWnd;
    end;

  procedure TDrivesCombo.ResetItemHeight;
    var
      nuHeight : integer;
    begin
      nuHeight :=  GetTextHeight( Font.Handle );
      if nuHeight < ( FloppyBmp.Height )
        then nuHeight := FloppyBmp.Height;
      ItemHeight := nuHeight + 2;
    end;

  procedure TDrivesCombo.ReadBitmaps;
    begin
      // Assign bitmap glyphs
      FloppyBmp := TBitmap.Create;
      FloppyBmp.Handle := LoadBitmap( hInstance, 'ICO_FLOPPYDRIVE' );
      FixedBmp := TBitmap.Create;
      FixedBmp.Handle := LoadBitmap( hInstance, 'ICO_HARDDRIVE' );
      NetworkBmp := TBitmap.Create;
      NetworkBmp.Handle := LoadBitmap( hInstance, 'ICO_NETWORKDRIVE' );
      CdromBmp := TBitmap.Create;
      CdromBmp.Handle := LoadBitmap( hInstance, 'ICO_CDROMDRIVE' );
      RamBmp := TBitmap.Create;
      RamBmp.Handle := LoadBitmap( hInstance, 'ICO_RAMDRIVE' );
    end;

  // Component registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TDrivesCombo] );
    end;

end.
