unit ServerCnxEvents;

interface

uses
  Windows, Protocol, VoyagerServerInterfaces, ActorPool, Classes;

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
    evnChannelListChanged    = evnBase_CnxHandler + 13;
    evnChannelChanged        = evnBase_CnxHandler + 14;
    evnMsgCompositionChanged = evnBase_CnxHandler + 15;
    evnUserChaseStarted      = evnBase_CnxHandler + 16;
    evnUserChaseAborted      = evnBase_CnxHandler + 17;
    evnNewMail               = evnBase_CnxHandler + 18;
    evnActorPoolModified     = evnBase_CnxHandler + 19;
    evnVoiceMsg              = evnBase_CnxHandler + 20;
    evnVoiceMsgAuthorized    = evnBase_CnxHandler + 21;
    evnRefreshSeason         = evnBase_CnxHandler + 22;
    evnLogonStarted          = evnBase_CnxHandler + 23;
    evnLogonCompleted        = evnBase_CnxHandler + 24;
    evnGMMsg                 = evnBase_CnxHandler + 25;
    evnGMEvent               = evnBase_CnxHandler + 26;
    evnRenderInputSuppliers  = evnBase_CnxHandler + 46;
    evnRenderOutputClients   = evnBase_CnxHandler + 47;

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
        Money     : currency;
        NetProfit : currency;
        Ranking   : integer;
        FacCount  : integer;
        FacMax    : integer;
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

    TGMMsgInfo =
      record
        Msg  : string;
      end;

    TGMEvent =
      record
        notID : integer;
        info  : string;
      end;

    TNewMailInfo =
      record
        count : integer;
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
        Name  : string;
        Id    : TCompanyId;
        World : string;
      end;

    TUserListChangeInfo =
      record
        UserName : string;
        Change   : TUserListChange;
      end;

    TChannelListChangeInfo =
      record
        ChannelName : string;
        Password    : string;
        Change      : TUserListChange;
      end;

    TMsgCompositionChangeInfo =
      record
        UserName : string;
        State    : TMsgCompositionState;
      end;

    TMsgActorPoolModifiedInfo =
      record
        ActorPoolId : TActorPoolId;
        Data        : TStream;
      end;

    TVoiceBuffer = array[0..32*1024] of byte;
    PVoiceBuffer = ^TVoiceBuffer;
    TVoiceMsgInfo =
      record
        From   : string;
        TxId   : integer;
        NewTx  : boolean;
        len    : integer;
        buffer : TVoiceBuffer;
      end;

    TRefreshSeasonInfo =
      record
        Season : integer;
      end;


    TLinkSearchMsg =
      record
        Fluid  : string;
        Result : string;
      end;

implementation


end.



