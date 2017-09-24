unit MagnaConst;

interface

  uses
    Standards;

  const
    tidClusterName_Magna = 'Magna';

  const
    tidInventionKind_Rulership        = 'MagnaRulership';
    tidInventionKind_Industrial       = 'MagnaIndustrial';
    tidInventionKind_Market           = 'MagnaMarket';
    tidInventionKind_Residential      = 'MagnaResidential';
    tidInventionKind_Public           = 'MagnaPublic';

  const
    nidInvention_MagnaLove             = 1;
    nidInvention_MagnaDynamics         = 2;
    nidInvention_MagnaLandLording      = 3;
    nidInvention_MagnaBasicIllusions   = 4;
    nidInvention_MagnaBasicCorrections = 5;

    nidInvention_MagnaOffices          = 10;

  const
    tidFacilityKind_MagnaSacred             = 'MagnaSacred';
    tidFacilityKind_MagnaIndustrial         = 'MagnaIndustrial';
    tidFacilityKind_MagnaResidentials       = 'MagnaResidentials';
    tidFacilityKind_MagnaMarkets            = 'MagnaMarkets';
    tidFacilityKind_MagnaCorrectionals      = 'MagnaCorrectionals';
    tidFacilityKind_MagnaBusinessFacilities = tidClusterName_Magna + tidFacilityKind_BusinessFacilities;
    tidFacilityKind_MagnaSpecial            = tidClusterName_Magna + tidFacilityKind_Special;

  const
    tmeInvention_VeryShort = 10;  // NOTE: Research time is in hours!
    tmeInvention_Short     = 30;
    tmeInvention_Normal    = 50;
    tmeInvention_Long      = 90;
    tmeInvention_VeryLong  = 120;

  const
    tidBlock_MainHQConstr = 'MagnaMainHQConstr';
    tidBlock_MainHQ       = 'MagnaMainHQ';
    tidFacility_MainHQ    = 'MagnaMainHQ';
    vidFacility_MainHQ    = 5001;

  const
    tidBlock_WhirlpoolConstr = 'MagnaWhirlpoolConstr';
    tidBlock_Whirlpool       = 'MagnaWhirlpool';
    tidFacility_Whirlpool    = 'MagnaWhirlpool';
    vidFacility_Whirlpool    = 5101;

  const
    tidBlock_SolariumConstr = 'MagnaSolariumConstr';
    tidBlock_Solarium       = 'MagnaSolarium';
    tidFacility_Solarium    = 'MagnaSolarium';
    vidFacility_Solarium    = 5111;

  const
    tidBlock_TulipConstr = 'MagnaTulipConstr';
    tidBlock_Tulip       = 'MagnaTulip';
    tidFacility_Tulip    = 'MagnaTulip';
    vidFacility_Tulip    = 5181;

  const
    tidBlock_IvoryTowerConstr = 'MagnaIvoryTowerConstr';
    tidBlock_IvoryTower       = 'MagnaIvoryTower';
    tidFacility_IvoryTower    = 'MagnaIvoryTower';
    vidFacility_IvoryTower    = 5121;

  const
    tidBlock_MayFlowerConstr = 'MagnaMayFlowerConstr';
    tidBlock_MayFlower       = 'MagnaMayFlower';
    tidFacility_MayFlower    = 'MagnaMayFlower';
    vidFacility_MayFlower    = 5131;

  const
    tidBlock_CloudCityConstr = 'MagnaCloudCityConstr';
    tidBlock_CloudCity       = 'MagnaCloudCity';
    tidFacility_CloudCity    = 'MagnaCloudCity';
    vidFacility_CloudCity    = 5141;

  const
    tidBlock_SkyDomeConstr = 'MagnaSkyDomeConstr';
    tidBlock_SkyDome       = 'MagnaSkyDome';
    tidFacility_SkyDome    = 'MagnaSkyDome';
    vidFacility_SkyDome    = 5191;

  const
    tidBlock_HeavenConstr = 'MagnaHeavenConstr';
    tidBlock_Heaven       = 'MagnaHeaven';
    tidFacility_Heaven    = 'MagnaHeaven';
    vidFacility_Heaven    = 5151;

  const
    tidBlock_HiveConstr = 'MagnaHiveConstr';
    tidBlock_Hive       = 'MagnaHive';
    tidFacility_Hive    = 'MagnaHive';
    vidFacility_Hive    = 5161;

  const
    tidBlock_OctopusConstr = 'MagnaOctopusConstr';
    tidBlock_Octopus       = 'MagnaOctopus';
    tidFacility_Octopus    = 'MagnaOctopus';
    vidFacility_Octopus    = 5171;

  const
    tidBlock_TheSpringConstr = 'MagnaTheSpringConstr';
    tidBlock_TheSpring       = 'MagnaTheSpring';
    tidFacility_TheSpring    = 'MagnaTheSpring';
    vidFacility_TheSpring    = 5201;

  const
    tidBlock_MagnaSupermarketConstrA = 'MagnaSupermarketConstrA';
    tidBlock_MagnaSupermarketA       = 'MagnaSupermarketA';
    tidFacility_MagnaSupermarketA    = 'MagnaSupermarketA';
    vidFacility_MagnaSupermarketA    = 5211;

  const
    tidBlock_MagnaSupermarketConstrB = 'MagnaSupermarketConstrB';
    tidBlock_MagnaSupermarketB       = 'MagnaSupermarketB';
    tidFacility_MagnaSupermarketB    = 'MagnaSupermarketB';
    vidFacility_MagnaSupermarketB    = 5221;


  // Research Center

  const
    tidBlock_MagnaResearchCenterConstr = 'MagnaResearchCenterConst';
    tidBlock_MagnaResearchCenter       = 'MagnaResearchCenter';
    tidFacility_MagnaResearchCenter    = 'MagnaResearchCenter';
    vidFacility_MagnaResearchCenter    = 5231;

  // Movie Studio

  const
    tidBlock_MagnaMovieStudioConstr = 'MagnaMovieStudioConst';
    tidBlock_MagnaMovieStudio       = 'MagnaMovieStudio';
    tidFacility_MagnaMovieStudio    = 'MagnaMovieStudio';
    vidFacility_MagnaMovieStudio    = 5241;


  // Other stuff

  const
    tidBlock_MagnaSmallParkConstr = 'MagnaSmallParkConstr';
    tidBlock_MagnaSmallPark       = 'MagnaSmallPark';
    tidFacility_MagnaSmallPark    = 'MagnaSmallPark';
    vidFacility_MagnaSmallPark    = 2831;

  const
    tidBlock_MagnaMediumParkConstr = 'MagnaMediumParkConstr';
    tidBlock_MagnaMediumPark       = 'MagnaMediumPark';
    tidFacility_MagnaMediumPark    = 'MagnaMediumPark';
    vidFacility_MagnaMediumPark    = 2841;

  const
    tidBlock_MagnaCentralParkConstr = 'MagnaCentralParkConstr';
    tidBlock_MagnaCentralPark       = 'MagnaCentralPark';
    tidFacility_MagnaCentralPark    = 'MagnaCentralPark';
    vidFacility_MagnaCentralPark    = 2851;

  const
    tidBlock_MagnaLibertyConstr = 'MagnaLibertyConstr';
    tidBlock_MagnaLiberty       = 'MagnaLiberty';
    tidFacility_MagnaLiberty    = 'MagnaLiberty';
    vidFacility_MagnaLiberty    = 6011;

  const
    tidBlock_MagnaTowerConstr = 'MagnaTowerConstr';
    tidBlock_MagnaTower       = 'MagnaTower';
    tidFacility_MagnaTower    = 'MagnaTower';
    vidFacility_MagnaTower    = 6021;


implementation

end.

