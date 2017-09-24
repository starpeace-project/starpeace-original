unit LandInfo;

interface

  uses
    Land, Windows, Graphics, Collection;

  const
    LandMax = 10000;

  type
    PLandMatrix = ^TLandMatrix;
    TLandMatrix = array[0..LandMax*LandMax] of TLandVisualClassId;

  type
    TLandClassInfo =
      class( TInterfacedObject, ILandClassInfo )
        public
          constructor Create( classfile : string );
        private
          fImageURL          : string;
          fAlterColor        : TColor;
          fLandVisualClassId : TLandVisualClassId;
        private
          function GetImageURL( zoomlevel : integer ) : string;
          function GetAlterColor : TColor;
          function GetLandVisualClassId : TLandVisualClassId;
      end;

    TLandClassesInfo =
      class( TInterfacedObject, ILandClassesInfo )
        public
          constructor Create( classdir : string );
          destructor  Destroy; override;
        private
          fLandClassesInfo : TCollection;
        private
          function GetClass( LandVisualClassId : TLandVisualClassId ) : ILandClassInfo;
      end;

    TLandInfo =
      class( TInterfacedObject, ILandInfo )
        public
          constructor Create( Bitmap : TBitmap );
          destructor  Destroy; override;
        private
          fLandMatrix : PLandMatrix;
          fLandSize   : TPoint;
        private
          function LandSize : TPoint;
          function LandVisualClassAt( x, y : integer ) : TLandVisualClassId;
          function LandClassAt( x, y : integer ) : TLandClass;
          function LandTypeAt( x, y : integer ) : TLandType;
      end;


implementation

  uses
    IniFiles, SysUtils;


  // TLandClassInfo

  constructor TLandClassInfo.Create( classfile : string );
    var
      IniFile : TIniFile;
    begin
      inherited Create;
      IniFile := TIniFile.Create( classfile );
      try
        fImageURL          := IniFile.ReadString( 'Images', '64x32', 'unknown.gif' );
        fAlterColor        := IniFile.ReadInteger( 'General', 'MapColor', 0 );
        fLandVisualClassId := IniFile.ReadInteger( 'General', 'Id', NoLand );
      finally
        IniFile.Free;
      end;
    end;

  function TLandClassInfo.GetImageURL( zoomlevel : integer ) : string;
    begin
      result := fImageURL;
    end;

  function TLandClassInfo.GetAlterColor : TColor;
    begin
      result := fAlterColor;
    end;

  function TLandClassInfo.GetLandVisualClassId : TLandVisualClassId;
    begin
      result := fLandVisualClassId;
    end;


  // TLandClassesInfo

  constructor TLandClassesInfo.Create( classdir : string );

    procedure LoadClasses;
      var
        SearchRec     : TSearchRec;
        LandClassInfo : TLandClassInfo;
      begin
        if FindFirst( classdir + '*.ini', faAnyFile, SearchRec ) = 0
          then
            repeat
              LandClassInfo := TLandClassInfo.Create( ClassDir + SearchRec.Name );
              fLandClassesInfo.Insert( LandClassInfo );
            until FindNext( SearchRec ) <> 0;
      end;

    begin
      inherited Create;
      try
        fLandClassesInfo := TCollection.Create( 0, rkBelonguer );
      except
        fLandClassesInfo.Free;
        raise;
      end;
    end;

  destructor TLandClassesInfo.Destroy;
    begin
      fLandClassesInfo.Free;
      inherited;
    end;

  function TLandClassesInfo.GetClass( LandVisualClassId : TLandVisualClassId ) : ILandClassInfo;
    var
      i : integer;
    begin
      i := 0;
      while (i < fLandClassesInfo.Count) and (TLandClassInfo(fLandClassesInfo[i]).GetLandVisualClassId <> LandVisualClassId) do
        inc( i );
      if i < fLandClassesInfo.Count
        then result := TLandClassInfo(fLandClassesInfo[i])
        else result := nil;
    end;


  // TLandInfo

  constructor TLandInfo.Create( Bitmap : TBitmap );

    procedure LoadLandInfo( Bitmap : TBitmap );
      var
        x, y : integer;
        line : PByteArray;
      begin
        for y := 0 to pred(fLandSize.y) do
          begin
            line := Bitmap.ScanLine[y];
            for x := 0 to pred(fLandSize.x) do
              fLandMatrix[fLandSize.x*y + x] := line[x];
          end;
      end;

    begin
      inherited Create;
      fLandSize.x := Bitmap.Width;
      fLandSize.y := Bitmap.Height;
      getmem( fLandMatrix, fLandSize.x*fLandSize.y*sizeof(fLandMatrix[0]) );
      try
        fillchar( fLandMatrix^, fLandSize.x*fLandSize.y*sizeof(fLandMatrix[0]), NoLand );
        LoadLandInfo( Bitmap );
      except
        freemem( fLandMatrix );
        raise;
      end;
    end;

  destructor TLandInfo.Destroy;
    begin
      freemem( fLandMatrix );
      inherited;
    end;

  function TLandInfo.LandSize : TPoint;
    begin
      result := fLandSize;
    end;
    
  function TLandInfo.LandVisualClassAt( x, y : integer ) : TLandVisualClassId;
    begin
      result := fLandMatrix[fLandSize.x*y + x];
    end;

  function TLandInfo.LandClassAt( x, y : integer ) : TLandClass;
    begin
      result := Land.LandPrimaryClassOf( LandVisualClassAt( x, y ));
    end;

  function TLandInfo.LandTypeAt( x, y : integer ) : TLandType;
    begin
      result := Land.LandTypeOf( LandVisualClassAt( x, y ));
    end;


    
end.


