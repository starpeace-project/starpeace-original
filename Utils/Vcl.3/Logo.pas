unit Logo;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Buffer, SpeedBmp;

  type
    TLogo =
      class( TCustomControl )
        private
          fMaskBitmap : string;
          fMask       : TSpeedBitmap;

          procedure SetMaskBitmap( const Value : string );

        protected
          procedure Loaded;                                                                                     override;
          procedure DefineProperties( Filer : TFiler );                                                         override;
          procedure ReadData( Stream : TStream );                                                               virtual;
          procedure WriteData( Stream : TStream );                                                              virtual;
          function  Equals( Value : TLogo ) : boolean;                                                          virtual;

        published
          property MaskBitmap : string read fMaskBitmap write SetMaskBitmap stored false;
      end;

  procedure Register;

implementation

  uses
    MemUtils;

  function TLogo.Equals( Value : TLogo ) : boolean;
    begin
      Result := ( Assigned( fMask ) = Assigned( Value.fMask ) ) and
                fMask.Equals( Value.fMask );
    end;

  procedure TLogo.DefineProperties( Filer : TFiler );
    function DoWrite : boolean;
      begin
        if Filer.Ancestor <> nil
          then Result := not (Filer.Ancestor is TLogo) or not Equals( TLogo( Filer.Ancestor ) )
          else Result := Assigned( fMask ) and not fMask.Empty;
      end;
    begin
      Filer.DefineBinaryProperty( 'Data', ReadData, WriteData, DoWrite );
    end;

  procedure TLogo.ReadData( Stream : TStream );
    var
      LoadMask : boolean;
    begin
      FreeObject( fMask );

      Stream.ReadBuffer( LoadMask, sizeof( LoadMask ) );
      if LoadMask
        then
          begin
            fMask := TSpeedBitmap.Create;
            fMask.LoadFromStream( Stream );
          end;
    end;

  procedure TLogo.WriteData( Stream : TStream );
    var
      SaveMask : boolean;
    begin
      SaveMask := Assigned( fMask );
      Stream.WriteBuffer( SaveMask, sizeof( SaveMask ) );
      if SaveMask
        then fMask.SaveToStream( Stream );
    end;

  procedure TLogo.Loaded;
    begin
      inherited;

      if Assigned( fMask ) and not (csDesigning in ComponentState)
        then
          begin
            SetWindowRgn( Handle, fMask.CreateMask( true ), Visible );
            FreeObject( fMask );
          end;
    end;

  procedure TLogo.SetMaskBitmap( const Value : string );
    begin
      fMaskBitmap := Value;
      fMask       := LoadBitmapFile( Value );
    end;

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TLogo] );
    end;

end.
