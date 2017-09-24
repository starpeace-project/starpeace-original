unit ServerCnxEvents;

interface

  uses
    Windows, Protocol, VoyagerServerInterfaces;


  const
    evnBase_CnxHandler = 1000;

  const
    evnRefreshArea           = evnBase_CnxHandler + 0;
    evnRefreshObject         = evnBase_CnxHandler + 1;
    evnRefreshTycoon         = evnBase_CnxHandler + 2;
    evnRefreshDate           = evnBase_CnxHandler + 3;
    evnEndOfPeriod           = evnBase_CnxHandler + 4;
    evnTycoonRetired         = evnBase_CnxHandler + 5;
    evnChatMsg               = evnBase_CnxHandler + 6;
    evnMoveTo                = evnBase_CnxHandler + 7;
    evnNotifyCompanionship   = evnBase_CnxHandler + 8;
    evnAnswerClientView      = evnBase_CnxHandler + 9;
    evnLogoff                = evnBase_CnxHandler + 10;
    evnSetCompany            = evnBase_CnxHandler + 11;
    evnUserListChanged       = evnBase_CnxHandler + 12;
    evnMsgCompositionChanged = evnBase_CnxHandler + 14;

  type
    TRefreshAreaInfo =
      record
        Area : TRect;
      end;

    TRefreshObjectInfo =
      record
        ObjId        : TObjId;
        KindOfChange : TFacilityChange;
      end;

    TRefreshTycoonInfo =
      record
        Money : string;
      end;

    TRefreshDateInfo =
      record
        Date : TDateTime;
      end;

    TEndOfPeriodInfo =
      record
        FailureLevel : integer;
      end;

    TTycoonRetiredInfo =
      record
        FailureLevel : integer;
      end;

    TChatMsgInfo =
      record
        From : string;
        Msg  : string;
      end;

    TMoveToInfo =
      record
        Pos : TPoint;
      end;

    TNotifyCompanionshipInfo =
      record
        Names : string;
      end;

    TSetCompanyInfo =
      record
        Name : string;
        Id   : TCompanyId;
      end;

    TUserListChangeInfo =
      record
        UserName : string;
        Change   : TUserListChange;
      end;

    TMsgCompositionChangeInfo =
      record
        UserName : string;
        State    : TMsgCompositionState;
      end;


implementation


end.
