unit CrimeProtocol;

interface

// Error codes

  const
    CRIME_NOERROR                  = 0;
    CRIME_ERROR_Unknown            = 1;
    CRIME_ERROR_GetCriminals       = 2;
    CRIME_ERROR_GetCityList        = 3;
    CRIME_ERROR_WrongLeaderName    = 4;
    CRIME_ERROR_WrongTeamName      = 5;
    CRIME_ERROR_WrongCriminalName  = 6;
    CRIME_ERROR_MaxReached         = 7;

  type
    TTargetType =
      (ttpCoordinates, ttpRoad, ttpCriminal, ttpTeam, ttpPlayer, ttpLeader, ttpCity, ttpFacility, ttpCompany, ttpFacilitiesFromPlayer, ttpFacilitiesFromPlayerInRadius, ttpFacilitiesFromCompany, ttpFacilitiesFromCompanyInRadius, ttpFacilitiesInRadius);

    TCriminalState =
      (tcsNoState, tcsOnTheMarket, tcsOnTheMarketHidden, tcsInTeamStandBy, tcsInTeamTraining, tcsInTeamInMission, tcsInTeamInMissionDead, tcsInJail, tcsHiddenByPolice, tcsInTeamDead, tcsInTeamInJail);

implementation

end.
 