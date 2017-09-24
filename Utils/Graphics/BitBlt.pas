unit BitBlt;

// Low-level bitblt (Bit Block Transfer) routines. Copyright (c) 1996-97 Jorge Romero Gomez, Merchise.

interface

  const
    mskColorKey = $00ffffff;

  // Swaps Source.Line[0]^ with Source.Line[Height-1], and so on...
  procedure FlipVertical( Source : pointer; Width, Height : integer; WidthSource : integer ); 

  // 8-bit specific routines: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  // =============================================================================================

  // No comments needed
  procedure BltFill( Dest : pointer; Width, Height : integer; aColor : byte; WidthDest : integer );
//  procedure BltFill16( Dest : pointer; Width, Height : integer; aColor : word; WidthDest : integer );
  procedure BltFill24( Dest : pointer; Width, Height : integer; aColor : cardinal; WidthDest : integer );
  procedure BltFill32( Dest : pointer; Width, Height : integer; aColor : cardinal; WidthDest : integer );

  procedure BltCopyFlipHorizontal( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
  procedure BltCopyFlipVertical( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
  procedure BltCopyFlipBoth( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );

  procedure BltCopyOpaque( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );

  // Copy a sprite with color index Transparent as the transparent color
  procedure BltCopyTrans( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer );

  // Draws the image making it appear as if it was made of glass
  // Draws the source by blending it with destination
  procedure BltCopyGlassed( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // BltCopyShaded Note
  // ==================
  // Later you'll see a BltCopyShaded24 (which draws the image as a shadow over destination),
  // to implement the same functionality in 8 bits, please use BltCopyDestCTT using a CTT that
  // converts a color to a darker one. It's faster and easier

  // Draws using the CTT (color translation table) with source pixels, the table determines the effect to achieve
  // With this you can copy the source tinting it, shading it, changing some colors by other, etc
  procedure BltCopySourceCTT( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // Draws using the CTT with destination pixels, the table determines the effect to achieve
  // With this you can (using the source as a mask) tint the destination, shade it, change some colors by other, etc
  procedure BltCopyDestCTT( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // Copies the image, skipping every other pixel (0X0X0X0X0X)
  // Some kind of glass effect, but without having to mix colors
  procedure BltCopyGrid( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );

  // Draws the mask with specified color, skipping every other pixel (0X0X0X0X0X)
  // Use it to do grid shades
  procedure BltCopyMaskGrid( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : byte; StartWithX : boolean );

  // 24-bit specific routines: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  // =============================================================================================

  // Same as 8-bit, using Transparent as the transparent color
  procedure BltCopyTrans24( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer);
  procedure BltCopyGrid24(  Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );
  procedure BltCopyMaskGrid24( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : integer; StartWithX : boolean );
  procedure BltCopyShaded24( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // 32-bit specific routines: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  // =============================================================================================

  procedure BltCopyTrans32( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer);
  //procedure BltCopyGrid32(  Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );
  //procedure BltCopyMaskGrid32( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : integer; StartWithX : boolean );
  procedure BltCopyShaded32( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // 8 to 16/24/32 routines
  procedure BltCopyOpaqueCTT16( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );
  procedure BltCopyOpaqueCTT24( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );
  procedure BltCopyOpaqueCTT32( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );

  procedure BltCopySourceCTT16( Source, Dest : pointer; Width, Height : integer; Transparent : integer; WidthSource, WidthDest : integer; Info : pointer );
  procedure BltCopyGlassedCTT16( Source, Dest : pointer; Width, Height : integer; Transparent : integer; WidthSource, WidthDest : integer; Info : pointer );
  procedure BltCopySourceCTT24( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer; Info : pointer );
  procedure BltCopySourceCTT32( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer; Info : pointer );

  // Channel manipulation
  procedure BltInjectChannel24( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer );
  procedure BltExtractChannel24( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer ); 
  procedure BltInjectChannel32( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer ); 
  procedure BltExtractChannel32( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer ); 

implementation

  {$LINK BitBlt.obj}
  {$LINK BitBlt16.obj}
  {$LINK BitBlt24.obj}
  {$LINK BitBlt32.obj}
  {$LINK BitBltGrid.obj}
  {$LINK BitBltMisc.obj}

  procedure BltInjectChannel24( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltExtractChannel24( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltInjectChannel32( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltExtractChannel32( Source, Dest : pointer; aWidth, aHeight : integer; WidthSource, WidthDest : integer );
    external;

  procedure FlipVertical( Source : pointer; Width, Height : integer; WidthSource : integer );
    external;
  procedure BltCopyFlipHorizontal( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltCopyFlipVertical( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltCopyFlipBoth( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
    external;
  procedure BltCopyOpaque( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer );
    external;

  procedure BltFill( Dest : pointer; Width, Height : integer; aColor : byte; WidthDest : integer );
    external;
//  procedure BltFill16( Dest : pointer; Width, Height : integer; aColor : word; WidthDest : integer );
//    external;
  procedure BltFill24( Dest : pointer; Width, Height : integer; aColor : cardinal; WidthDest : integer );
    external;
  procedure BltFill32( Dest : pointer; Width, Height : integer; aColor : cardinal; WidthDest : integer );
    external;

  procedure BltCopySourceCTT( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyDestCTT( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;

  procedure BltCopyTrans( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer );
    external;
  procedure BltCopyGlassed( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyGrid( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );
    external;
  procedure BltCopyMaskGrid( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : byte; StartWithX : boolean );
    external;

  procedure BltCopyTrans24( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer);
    external;
  procedure BltCopyShaded24( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyGrid24(  Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );
    external;
  procedure BltCopyMaskGrid24( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : integer; StartWithX : boolean );
    external;
  procedure BltCopyTrans32( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer);
    external;
  procedure BltCopyShaded32( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;
{
  procedure BltCopyGrid32(  Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; StartWithX : boolean );
    external;
  procedure BltCopyMaskGrid32( Source, Dest : pointer; Width, Height : integer; Transparent : cardinal; WidthSource, WidthDest : integer; Color : integer; StartWithX : boolean );
    external;
}

  procedure BltCopyOpaqueCTT16( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopySourceCTT16( Source, Dest : pointer; Width, Height : integer; Transparent : integer; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyGlassedCTT16( Source, Dest : pointer; Width, Height : integer; Transparent : integer; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyOpaqueCTT24( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopySourceCTT24( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopyOpaqueCTT32( Source, Dest : pointer; Width, Height : integer; WidthSource, WidthDest : integer; Info : pointer );
    external;
  procedure BltCopySourceCTT32( Source, Dest : pointer; Width, Height : integer; Transparent, Alpha : cardinal; WidthSource, WidthDest : integer; Info : pointer );
    external;

end.
