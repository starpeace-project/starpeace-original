unit PaletteIO;

interface

  uses
    Palettes;

  procedure JascLoadFromFile( const Filename : string; var LogPalette );
  procedure JascSaveToFile( const Filename : string; const LogPalette );

implementation

  uses
    SysUtils, GDI;

  procedure JascLoadFromFile( const Filename : string; var LogPalette );
    var
      LogPal : T256LogPalette absolute LogPalette;
    var
      JascFile : text;
      Id       : string;
      Indx     : integer;
    begin
      LogPal.Version         := LogPaletteVersion;
      LogPal.NumberOfEntries := 0;

      AssignFile( JascFile, Filename );
      reset( JascFile );
      try
        readln( JascFile, Id );              // Read JASC id
        if UpperCase( Id ) = 'JASC-PAL'
          then
            begin
              readln;                        // Read JASC version
              readln( JascFile, LogPal.NumberOfEntries );
              for Indx := 0 to LogPal.NumberOfEntries - 1 do
                with LogPal.Entries[Indx] do
                  readln( JascFile, peRed, peGreen, peBlue );
            end;
      finally
        CloseFile( JascFile );
      end;
    end;

  procedure JascSaveToFile( const Filename : string; const LogPalette );
    var
      LogPal : T256LogPalette absolute LogPalette;
    var
      JascFile : text;
      Indx     : integer;
    begin
      AssignFile( JascFile, Filename );
      rewrite( JascFile );
      try
        writeln( JascFile, 'JASC-PAL' );     // Write JASC id
        writeln( JascFile, '0100' );         // Write JASC version
        writeln( JascFile, LogPal.NumberOfEntries );
        for Indx := 0 to LogPal.NumberOfEntries - 1 do
          with LogPal.Entries[Indx] do
            writeln( JascFile, peRed, peGreen, peBlue );
      finally
        CloseFile( JascFile );
      end;
    end;


end.

(*
      The Jasc palette file format is not the same as the Microsoft PAL format.

      Paint Shop Pro uses the Jasc palette file format (extension ".PAL") for saving 16 and 256 color
      palette files. The file type was created as a text file so that you can edit or create palettes
      with a text editor, such as Notepad. The file structure is as follows:

      Header:	JASC-PAL
      Version:	0100
      Number of Colors:	16 or 256
      Palette Data:	Made up of Red, Green and Blue color components. Color components are values from
      0 to 255. RGB values must be separated by one space. Each RGB series must be on a separate line.

      Example

      The following is Window's default colors in the Jasc PAL file format:

      JASC-PAL
      0100
      16
      0 0 0
      0 0 191
      0 191 0
      0 191 191
      191 0 0
      191 0 191
      191 191 0
      192 192 192
      128 128 128
      0 0 255
      0 255 0
      0 255 255
      255 0 0
      255 0 255
      255 255 0
      255 255 255
*)


