unit StdEffects;

interface

  uses
    Effects, Languages;

  const
    fxLittering      = 1000;
    fxGraffiti       = 1001;
    fxWantonViolence = 1002;
    fxNoise          = 1003;
    fxVoyeurism      = 1004;
    fxSabotage       = 1005;
    fxAreaFear       = 1006;
    fxMaintIncrease  = 1007;
    fxUnDesirability = 1008;
    fxEfficHit       = 1009;
    fxProdQualityHit = 1010;

  // Effect names

  const
    mtidfxName_Littering      : TRegMultiString = nil;
    mtidfxName_Graffiti       : TRegMultiString = nil;
    mtidfxName_WantonViolence : TRegMultiString = nil;
    mtidfxName_Noise          : TRegMultiString = nil;
    mtidfxName_Voyeurism      : TRegMultiString = nil;
    mtidfxName_Sabotage       : TRegMultiString = nil;
    mtidfxName_AreaFear       : TRegMultiString = nil;
    mtidfxName_MaintIncrease  : TRegMultiString = nil;

  // Effect status reports

  const
    mtidEfxStrength : TRegMultiString = nil; // '%s attack strength %d%%.'
    mtidEfxBeauty   : TRegMultiString = nil; // 'Beauty loss of %d points.'
    mtidEfxCrime    : TRegMultiString = nil; // 'Crime increase of %d points.'
    mtidEfxQOL      : TRegMultiString = nil; // 'QOL loss of %d points.'
    mtidEfxPoll     : TRegMultiString = nil; // 'Pollution increase of %d points.'
    mtidEfxNBHQ     : TRegMultiString = nil; // 'Neighborhood quality loss of %d points.'
    mtidEfxMaintHit : TRegMultiString = nil; // 'Maintenance costs increased %d%%.'

  procedure InitMLS;

implementation

  procedure InitMLS;
    begin
      mtidfxName_Littering      := TRegMultiString.Create( 'mtidfxName_Littering', 'Citizens report several suspects were seen dumping waste on the area.' );
      mtidfxName_Graffiti       := TRegMultiString.Create( 'mtidfxName_Graffiti', 'Citizens report some suspects were seen making signs with spray cans.' );
      mtidfxName_WantonViolence := TRegMultiString.Create( 'mtidfxName_WantonViolence', 'Locals report various acts of wanton violence.' );
      mtidfxName_Noise          := TRegMultiString.Create( 'mtidfxName_Noise', 'Citizes complain about various forms of loud noise.' );
      mtidfxName_Voyeurism      := TRegMultiString.Create( 'mtidfxName_Voyeurism', 'Local girls reported they have been repeatedly spied upon by undentified persons.' );
      mtidfxName_Sabotage       := TRegMultiString.Create( 'mtidfxName_Sabotage', 'Property managers report the site was sabotaged.' );
      mtidfxName_AreaFear       := TRegMultiString.Create( 'mtidfxName_AreaFear', 'Citizens report they are afraid to leave home for their daily tasks.' );
      mtidfxName_MaintIncrease  := TRegMultiString.Create( 'mtidfxName_MaintIncrease', 'Property managers report an increase of maintenance costs.' );

      mtidEfxStrength           := TRegMultiString.Create( 'mtidEfxStrength', 'Attack strength %d%%.' );
      mtidEfxBeauty             := TRegMultiString.Create( 'mtidEfxBeauty', 'Beauty loss of %d points.' );
      mtidEfxCrime              := TRegMultiString.Create( 'mtidEfxCrime', 'Crime increase of %d points.' );
      mtidEfxQOL                := TRegMultiString.Create( 'mtidEfxQOL', 'QOL loss of %d points.' );
      mtidEfxPoll               := TRegMultiString.Create( 'mtidEfxPoll', 'Pollution increase of %d points.' );
      mtidEfxNBHQ               := TRegMultiString.Create( 'mtidEfxNBHQ', 'Neighborhood quality loss of %d points.' );
      mtidEfxMaintHit           := TRegMultiString.Create( 'mtidEfxMaintHit', 'Maintenance costs increased %d%%.' );
    end;


initialization

  InitMLS;
      
end.


