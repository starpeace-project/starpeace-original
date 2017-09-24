{$DEFINE EXCMAGIC_BADCOMPILER}

{$IFDEF VER80}            { Delphi 1.0 }
{$ENDIF}
{$IFDEF VER90}            { Delphi 2.0 }
{$ENDIF}

{$IFDEF VER100}           { Delphi 3.0 }
  {$DEFINE EXCMAGIC_Delphi3}
  {$DEFINE EXCMAGIC_Delphi3plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}

{$IFDEF VER120}           { Delphi 4.0 }
  {$DEFINE EXCMAGIC_Delphi4}
  {$DEFINE EXCMAGIC_Delphi3plus}
  {$DEFINE EXCMAGIC_Delphi4plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}

{$IFDEF VER130}           { Delphi 5.0 }
  {$DEFINE EXCMAGIC_Delphi5}
  {$DEFINE EXCMAGIC_Delphi3plus}
  {$DEFINE EXCMAGIC_Delphi4plus}
  {$DEFINE EXCMAGIC_Delphi5plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}

{$IFDEF VER140}           { Delphi 6.0 }
  {$DEFINE EXCMAGIC_Delphi5}
  {$DEFINE EXCMAGIC_Delphi3plus}
  {$DEFINE EXCMAGIC_Delphi4plus}
  {$DEFINE EXCMAGIC_Delphi5plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}

{$IFDEF VER110}           { Builder 3 }
  {$DEFINE EXCMAGIC_Delphi4plus}
  {$DEFINE EXCMAGIC_Builder3}
  {$DEFINE EXCMAGIC_Builder3plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}

{$IFDEF VER125}           { Builder 4 }
  {$DEFINE EXCMAGIC_Delphi4plus}
  {$DEFINE EXCMAGIC_Builder4}
  {$DEFINE EXCMAGIC_Builder3plus}
  {$DEFINE EXCMAGIC_Builder4plus}
  {$UNDEF  EXCMAGIC_BADCOMPILER}
{$ENDIF}
