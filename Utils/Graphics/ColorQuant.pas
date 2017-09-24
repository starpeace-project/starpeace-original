unit ColorQuant;

interface

  uses
    Classes, Windows, GDI, Dibs;

  //
  //  ColorQuant implements the Gervautz-Purgathofer octree
  //  color quantization algorithm that creates optimized color palettes for
  //  for 16, 24, and 32-bit DIBs
  //
  //  Taken from Jeff Prosise. Translated and modified by Jorge Romero, Merchise [JRG]

  procedure CreateOctreePalette( DibHeader : PDib; DibPixels : pointer; MaxColors : cardinal; ColorBits : cardinal; var LogPalette );
  
implementation

  uses
    NumUtils, MemUtils;

  type
    POctreeNode = ^TOctreeNode;
    TOctreeNode =
      record
        PixelCount : cardinal;                    // Number of pixels represented by this leaf
        RedSum     : cardinal;                    // Sum of red components
        GreenSum   : cardinal;                    // Sum of green components
        BlueSum    : cardinal;                    // Sum of blue components
        Children   : array[0..7] of POctreeNode;  // Pointers to child nodes
        Next       : POctreeNode;                 // Pointer to next reducible node
        IsLeaf     : boolean;                     // TRUE if node has no children
      end;

  type
    POctree = POctreeNode;

  type
    TOctreeNodes = array[0..0] of POctreeNode;

  function CreateNode( Level : cardinal; ColorBits : cardinal; var LeafCount : cardinal;
                       var ReducibleNodes : TOctreeNodes ) : POctreeNode;
    begin
      new( Result );
      with Result^ do
        begin
          IsLeaf := Level = ColorBits;
          if IsLeaf
            then Inc( LeafCount )
            else
              begin
                // Add the node to the reducible list for this level
                Next := ReducibleNodes[Level];
                ReducibleNodes[Level] := Result;
              end;
        end;
    end;

  procedure ReduceTree( ColorBits : cardinal; var LeafCount : cardinal; var ReducibleNodes : TOctreeNodes );
    var
      i                         : cardinal;
      Node                      : POctreeNode;
      RedSum, GreenSum, BlueSum : cardinal;
      Children                  : cardinal;
    begin
      // Find the deepest level containing at least one reducible node
      i := ColorBits - 1;
      while ( i > 0 ) and ( ReducibleNodes[i] = nil ) do
        dec( i );

      // Reduce the node most recently added to the list at level i
      Node              := ReducibleNodes[i];
      ReducibleNodes[i] := Node.Next;

      RedSum   := 0;
      GreenSum := 0;
      BlueSum  := 0;
      Children := 0;
      for i := 0 to 7 do
        if Node.Children[i] <> nil
          then
            begin
              inc( RedSum, Node.Children[i].RedSum );
              inc( GreenSum, Node.Children[i].GreenSum );
              inc( BlueSum, Node.Children[i].BlueSum );
              inc( Node.PixelCount, Node.Children[i].PixelCount );
              FreePtr( Node.Children[i] );
              inc( Children );
            end;
      Node.IsLeaf   := true;
      Node.RedSum   := RedSum;
      Node.GreenSum := GreenSum;
      Node.BlueSum  := BlueSum;
      dec( LeafCount, Children - 1 );
    end;

  procedure DeleteTree( var Node : POctree );
    var
      i : cardinal;
    begin
      if Node <> nil
        then
          begin
            for i := 0 to 7 do
              if Node.Children[i] <> nil
                then DeleteTree( Node.Children[i] );
            FreePtr( Node );
          end;
    end;

  procedure GetPaletteColors( Tree : POctree; var LogEntries; var Indx : cardinal );
    var
      i       : integer;
      Entries : T256PalEntries absolute LogEntries;
    begin
      with Tree^, Entries[Indx] do
        if IsLeaf
          then
            begin
              peRed   := RedSum div PixelCount;
              peGreen := GreenSum div PixelCount;
              peBlue  := BlueSum div PixelCount;
            end
          else
            for i := 0 to 7 do
              if Children[i] <> nil
                then GetPaletteColors( Children[i], Entries, Indx );
    end;

  procedure AddColor( var Node : POctreeNode; r, g, b : byte; ColorBits : cardinal; Level : cardinal;
                      var LeafCount : cardinal; var ReducibleNodes : TOctreeNodes );
    var
      Indx  : cardinal;
      Shift : cardinal;
    const
      Mask : array[0..7] of byte = ( $80, $40, $20, $10, $08, $04, $02, $01 );
    begin
      // If the node doesn't exist, create it
      if Node = nil
        then Node := CreateNode( Level, ColorBits, LeafCount, ReducibleNodes );

      // Update color information if it's a leaf node
      if Node.IsLeaf
        then
          begin
            inc( Node.PixelCount );
            inc( Node.RedSum, r );
            inc( Node.GreenSum, g );
            inc( Node.BlueSum, b );
          end
        else // Recurse a level deeper if the node is not a leaf
          begin
            Shift := 7 - Level;
            Indx  := (((r and Mask[Level]) shr Shift) shl 2) or
                     (((g and Mask[Level]) shr Shift) shl 1) or
                      ((b and Mask[Level]) shr Shift);
            AddColor( Node.Children[Indx], r, g, b, ColorBits, Level + 1, LeafCount, ReducibleNodes );
          end;
    end;

  procedure CreateOctreePalette( DibHeader : PDib; DibPixels : pointer; MaxColors : cardinal; ColorBits : cardinal; var LogPalette );
    var
      i, x, y        : integer;
      Tree           : POctree;
      ReducibleNodes : TOctreeNodes;
      LeafCount      : cardinal;
      Indx           : cardinal;
      RgbData        : pointer;
      ScanLine       : pchar;
      Palette        : T256LogPalette absolute LogPalette;
    begin
      Tree      := nil;
      LeafCount := 0;
      if ColorBits <= 8
        then
          try
            with DibHeader^ do
              begin
                for i := 0 to ColorBits do
                  ReducibleNodes[i] := nil;

                // Scan the DIB and build the octree
                DibGetRgbBegin( DibHeader, RgbData );
                for y := 0 to biHeight - 1 do
                  begin
                    ScanLine := DibScanLine( DibHeader, DibPixels, y );
                    for x := 0 to biWidth - 1 do
                      with DibGetRgb( ScanLine, x, RgbData ) do
                        begin
                          AddColor( Tree, rgbRed, rgbGreen, rgbBlue, ColorBits, 0, LeafCount, ReducibleNodes );
                          while LeafCount > MaxColors do
                            ReduceTree( ColorBits, LeafCount, ReducibleNodes );
                        end;
                  end;

                assert( LeafCount > MaxColors, 'Could not reduce enough colors in ColorQuant.CreateOctreePalette!!' );

                // Create logical palette
                with Palette do
                  begin
                    Version         := LogPaletteVersion;
                    NumberOfEntries := LeafCount;
                    Indx            := 0;
                    GetPaletteColors( Tree, Entries, Indx );
                  end;
              end;
          finally
            DeleteTree( Tree );
          end;
    end;
    
end.
