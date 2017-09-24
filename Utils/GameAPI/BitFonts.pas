unit BitFonts;

// Bitmapped Fonts. Copyright (c) 1997 Jorge Romero Gomez, Merchise.

interface

  uses
    SysUtils, Classes, Windows, Graphics, Buffer;

  type
    TBitFontChar = class;

    PCharList = ^TCharList;
    TCharList = array[0..0] of TBitFontChar;

    TBitFont =
      class( TPersistent )
        private
          fProportional : boolean;
          //fBitmap       : TBuffer;
          fFirstIndx    : integer;
          fCount        : integer;
          fCharacters   : PCharList;
          fCTT          : pointer;

        protected
          function  GetCharacter( Ch : char ) : TBitFontChar;
          procedure SelectCTT( Value : pointer );

        public
          property CTT : pointer                         read fCTT           write SelectCTT;
          property Count : integer                       read fCount;
          property FirstIndx : integer                   read fFirstIndx;
          property Character[ Ch : char ] : TBitFontChar read GetCharacter;  default;
          property Proportional : boolean                read fProportional;

          constructor Create;
          constructor CreateFromTrueType( aFont : TFont; aProportional : boolean; aFirstIndx, aCount : integer );
          destructor  Destroy;                                                                                     override;

          procedure   SaveToStream( Stream : TStream );                                                            virtual;
          procedure   SaveToFile( const Filename : string );                                                       virtual;
          procedure   LoadFromStream( Stream : TStream );                                                          virtual;
          procedure   LoadFromFile( const Filename : string );                                                     virtual;
          procedure   LoadFromResourceName( Instance : THandle; const ResourceName : string );
          procedure   LoadFromResourceID( Instance : THandle; ResourceId : Integer );

          procedure   TextOut( x, y : integer; const Text : string );
      end;

    TBitFontChar =
      class( TPersistent )
        private
          fImageAddr  : pointer;
          fImageCoord : TPoint;
          fWidth      : integer;
          fHeight     : integer;
          fVertOffset : integer;
          fFont       : TBitFont;

        protected

        public
          constructor Create;
          destructor  Destroy;                                                                                  override;

          procedure   LoadFromStream( Stream : TStream );                                                       virtual;

          function    Draw( x, y : integer ) : integer;

          property Font : TBitFont      read fFont;
          property ImageAddr : pointer  read fImageAddr;
          property ImageCoord : TPoint  read fImageCoord;
          property Width : integer      read fWidth;
          property Height : integer     read fHeight;
          property VertOffset : integer read fVertOffset;
      end;

implementation

  uses
    StreamUtils;

  // TBitFont

  procedure TBitFont.SelectCTT( Value : pointer );
    begin
      fCTT := Value;
    end;

  constructor TBitFont.CreateFromTrueType( aFont : TFont; aProportional : boolean; aFirstIndx, aCount : integer );
    begin
      Create;

    end;

  constructor TBitFont.Create;
    begin
      inherited;

      //
    end;

  destructor TBitFont.Destroy;
    begin
      //

      inherited;
    end;

  function TBitFont.GetCharacter( Ch : char ) : TBitFontChar;
    begin
      assert( Ch in [char(FirstIndx)..char(FirstIndx + Count)], 'Index out of bounds in BitFonts.TBitFont.GetCharacter!!' );

      Result := fCharacters[ord(Ch) - FirstIndx];
    end;

  procedure TBitFont.SaveToStream( Stream : TStream );
    begin
      try
        //
      except
        //
        raise;
      end;
    end;

  procedure TBitFont.SaveToFile( const Filename : string );
    begin
      StreamObject( TFileStream.Create( Filename, fmCreate or fmShareExclusive ), SaveToStream );
    end;

  procedure TBitFont.LoadFromStream( Stream : TStream );
    begin
      try
        //
      except
        //
        raise;
      end;
    end;

  procedure TBitFont.LoadFromFile( const Filename : string );
    begin
      StreamObject( TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite ), LoadFromStream );
    end;

  procedure TBitFont.LoadFromResourceName( Instance : THandle; const ResourceName : string );
    begin
      StreamObject( TResourceStream.Create( Instance, ResourceName, RT_BITMAP ), LoadFromStream );
    end;

  procedure TBitFont.LoadFromResourceID( Instance : THandle; ResourceId : Integer );
    begin
      StreamObject( TResourceStream.CreateFromID( Instance, ResourceId, RT_BITMAP ), LoadFromStream );
    end;

  procedure TBitFont.TextOut( x, y : integer; const Text : string );
    var
      i : integer;
    begin
      for i := 1 to length( Text ) do
        Inc( x, Character[Text[i]].Draw( x, y ) );
    end;

  // TBitFontChar

  function TBitFontChar.Draw( x, y : integer ) : integer;
    begin
      with Font do
        begin
          //
        end;

      Result := Width;
    end;

  constructor TBitFontChar.Create;
    begin
      inherited Create;

      //
    end;

  destructor TBitFontChar.Destroy;
    begin
      //

      inherited;
    end;

  procedure TBitFontChar.LoadFromStream( Stream : TStream );
    begin
      try
        //
      except
        //
        raise;
      end;
    end;

end.
