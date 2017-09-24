unit Gradients;

// Copyright (c) 1997-98 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    GDI, GdiExt, Buffer,
    MemUtils, NumUtils;

  type
    TGradientEffect = ( gkVertical, gkHorizontal, gkNone, gkCustom );

  const // Default gradient colors
    defStartColor = clBlack;
    defEndColor   = clBlue;

  const // Default grid size
    gsDefault = 5;

  type
    TGradient =
      class( TGraphicControl )
        protected
          fGradientEffect  : TGradientEffect;
          fStartColor      : TColor;
          fEndColor        : TColor;
          fVertices        : PTriVertices;
          fVertCount       : integer;
          fCustomVertices  : PTriVertices;
          fCustomVertCount : integer;
          fCustomTriangles : PTriangles;
          fCustomTriCount  : integer;

          procedure SetupGradient;                                                                                             virtual;
          procedure Cleanup;                                                                                                   virtual;

          procedure SetGradientEffect( Value : TGradientEffect );                                                              virtual;
          procedure SetStartColor( Value : TColor );                                                                           virtual;
          procedure SetEndColor( Value : TColor );                                                                             virtual;

          procedure Changed;                                                                                                   virtual;
          procedure EraseBkgnd( var Msg : TWMEraseBkgnd );                                                                     message WM_ERASEBKGND;

        protected
          procedure DefineProperties( Filer : TFiler );                                                                        override;
          procedure ReadData( Stream : TStream );                                                                              virtual;
          procedure WriteData( Stream : TStream );                                                                             virtual;

        public
          function  Equals( Value : TGradient ) : boolean;                                                                     virtual;
          function  Empty : boolean;                                                                                           virtual;

          procedure SetVertexInfo( Info : TList );                                                                             virtual;
          procedure SetTriangleInfo( Info : TList );                                                                           virtual;
          procedure SetVertexAndTriangleInfo( Vertices, Triangles : TList );                                                   virtual;

        public
          procedure Paint;                                                                                                     override;
          procedure SetBounds( aLeft, aTop, aWidth, aHeight : integer );                                                       override;

          constructor Create( aOwner: TComponent );                                                                            override;
          destructor  Destroy;                                                                                                 override;

        published
          property GradientEffect : TGradientEffect read fGradientEffect write SetGradientEffect default gkVertical;
          property StartColor : TColor              read fStartColor     write SetStartColor     default defStartColor;
          property EndColor : TColor                read fEndColor       write SetEndColor       default defEndColor;

        published
          property Align;
          property Visible;
          property Enabled;
          property ParentShowHint;
          property ShowHint;
          property PopupMenu;

          property OnClick;
          property OnDblClick;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;

          property DragMode;
          property DragCursor;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnStartDrag;
      end;

  procedure Register;

implementation

  {$R *.DCR}
  
  // TGradient

  const
    fGradientRect : TGradientRect = ( UpperLeft : 0; LowerRight : 1 );
    
  procedure TGradient.EraseBkgnd( var Msg : TWMEraseBkgnd );
    begin
    end;

  constructor TGradient.Create( aOwner: TComponent );
    begin
      inherited;

      Width  := 40;
      Height := 40;
      ControlStyle := ControlStyle + [csOpaque, csDesignInteractive];

      fStartColor     := defStartColor;
      fEndColor       := defEndColor;
      fGradientEffect := gkVertical;
      SetupGradient;
    end;

  destructor TGradient.Destroy;
    begin
      Cleanup;

      inherited;
    end;

  procedure TGradient.Cleanup;
    begin
      if Assigned( fVertices )
        then freemem( fVertices );
      if Assigned( fCustomVertices )
        then freemem( fCustomVertices );
    end;

  function TGradient.Empty : boolean;
    begin
      Result := GradientEffect = gkNone;
    end;

  procedure TGradient.ReadData( Stream : TStream );
    var
      VertSize, TriSize : integer;
      tmpGradientEffect : TGradientEffect;
    begin
      Cleanup;
      with Stream do
        begin
          ReadBuffer( tmpGradientEffect, sizeof( tmpGradientEffect ) );
          if tmpGradientEffect = gkCustom
            then
              begin
                ReadBuffer( fCustomVertCount, sizeof( fCustomVertCount ) );
                VertSize := fCustomVertCount * sizeof( fCustomVertices[0] );
                getmem( fCustomVertices, VertSize );
                ReadBuffer( fCustomVertices^, VertSize );

                ReadBuffer( fCustomTriCount, sizeof( fCustomTriCount ) );
                TriSize := fCustomTriCount * sizeof( fCustomTriangles[0] );
                getmem( fCustomTriangles, TriSize );
                ReadBuffer( fCustomTriangles^, TriSize );
              end;
          GradientEffect := tmpGradientEffect;
          Changed;
        end;
    end;

  procedure TGradient.WriteData( Stream : TStream );
    begin
      with Stream do
        begin
          WriteBuffer( fGradientEffect, sizeof( fGradientEffect ) );
          if GradientEffect = gkCustom
            then
              begin
                WriteBuffer( fCustomVertCount, sizeof( fCustomVertCount ) );
                WriteBuffer( fCustomVertices^, fCustomVertCount * sizeof( fCustomVertices[0] ) );

                WriteBuffer( fCustomTriCount, sizeof( fCustomTriCount ) );
                WriteBuffer( fCustomTriangles^, fCustomTriCount * sizeof( fCustomTriangles[0] ) );
              end;
        end;
    end;

  function TGradient.Equals( Value : TGradient ) : boolean;
    var
      MyImage, ValueImage : TMemoryStream;
    begin
      if Value = nil
        then Result := false
        else
          if Empty or Value.Empty
            then Result := Empty and Value.Empty
            else
              begin
                Result := ClassType = Value.ClassType;
                if Result
                  then
                    begin
                      MyImage := TMemoryStream.Create;
                      try
                        WriteData( MyImage );
                        ValueImage := TMemoryStream.Create;
                        try
                          Value.WriteData( ValueImage );
                          Result := ( MyImage.Size = ValueImage.Size ) and
                            CompareMem( MyImage.Memory, ValueImage.Memory, MyImage.Size );
                        finally
                          ValueImage.Free;
                        end;
                      finally
                        MyImage.Free;
                      end;
                    end;
              end;
    end;

  procedure TGradient.DefineProperties( Filer : TFiler );
    function DoWrite : boolean;
      begin
        if Filer.Ancestor <> nil
          then Result := not (Filer.Ancestor is TGradient) or not Equals( TGradient( Filer.Ancestor ) )
          else Result := true;
      end;
    begin
      Filer.DefineBinaryProperty( 'Data', ReadData, WriteData, DoWrite );
    end;

  procedure TGradient.SetVertexInfo( Info : TList );
    begin
      freemem( fCustomVertices );
      fCustomVertCount := TriVerticesFromList( Info, fCustomVertices );
      Changed;
    end;

  procedure TGradient.SetTriangleInfo( Info : TList );
    begin
      freemem( fCustomTriangles );
      fCustomTriCount := TrianglesFromList( Info, fCustomTriangles );
      Changed;
    end;

  procedure TGradient.SetVertexAndTriangleInfo( Vertices, Triangles : TList );                                                  
    begin
      freemem( fCustomVertices );
      fCustomVertCount := TriVerticesFromList( Vertices, fCustomVertices );
      freemem( fCustomTriangles );
      fCustomTriCount := TrianglesFromList( Triangles, fCustomTriangles );
      Changed;
    end;
    
  procedure TGradient.SetupGradient;
    begin
      if fVertices <> fCustomVertices
        then FreePtr( fVertices ); // Free allocated vertex info
      case GradientEffect of
        gkNone :
          begin
            ControlStyle := ControlStyle - [csOpaque];
          end;
        gkCustom :
          begin
            ControlStyle := ControlStyle - [csOpaque]; // !! Esto se puede optimizar
            fVertCount   := fCustomVertCount;
            fVertices    := fCustomVertices;
          end;
        gkHorizontal, gkVertical :
          begin
            ControlStyle := ControlStyle + [csOpaque];
            fVertCount   := AllocTriVertices( [TriVertex( 0, 0, StartColor ), TriVertex( Width, Height, EndColor )], fVertices );
          end;
      end;
    end;

  procedure TGradient.Paint;
    begin
      case GradientEffect of
        gkHorizontal :
          begin
            GradientFill( Canvas.Handle, fVertices, 2, @fGradientRect, 1, gfHorizontalRect );
          end;
        gkVertical :
          begin
            GradientFill( Canvas.Handle, fVertices, 2, @fGradientRect, 1, gfVerticalRect );
          end;
        gkCustom :
          begin
            if Assigned( fVertices )
              then GradientFill( Canvas.Handle, fVertices, fVertCount, fCustomTriangles, fCustomTriCount, gfTriangle );
          end;
      end;
    end;

  procedure TGradient.Changed;
    begin
      SetupGradient;
      Invalidate;
    end;

  procedure TGradient.SetBounds( aLeft, aTop, aWidth, aHeight : integer );
    begin
      if (aWidth <> Width) or (aHeight <> Height)
        then
          begin
            inherited;
            Changed;
          end
        else inherited;
    end;

  procedure TGradient.SetGradientEffect( Value : TGradientEffect );
    begin
      if Value <> GradientEffect
        then
          begin
            fGradientEffect := Value;
            Changed;
          end;
    end;

  procedure TGradient.SetStartColor( Value : TColor );
    begin
      if Value <> StartColor
        then
          begin
            fStartColor := Value;
            Changed;
          end;
    end;

  procedure TGradient.SetEndColor( Value : TColor );
    begin
      if Value <> EndColor
        then
          begin
            fEndColor := Value;
            Changed;
          end;
    end;

  // VCL Registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TGradient] );
    end; 

end.
