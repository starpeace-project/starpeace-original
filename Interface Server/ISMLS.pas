unit ISMLS;

interface

  uses
    Languages;

  const                              
    mtidSystem        : TRegMultiString = nil; //  'SYSTEM';
    mtidClientAware   : TRegMultiString = nil; //  UserName + ' has entered ' + fServer.WorldName + '.'
    mtidClientUnAware : TRegMultiString = nil; //  UserName + ' has left ' + fServer.WorldName + '.'
    mtidChannelInfo   : TRegMultiString = nil; //  'Channel "' + Channel.fName + '", created by ' + Channel.fCreator + '. '
    mtidUser          : TRegMultiString = nil; // user
    mtidUsers         : TRegMultiString = nil; // users
    mtidAnd           : TRegMultiString = nil; // and
    mtidChanNeedsPass : TRegMultiString = nil; //' You need a password to enter this channel.'
    mtidNoChanInfo    : TRegMultiString = nil; // 'There is no information associated to this object.'
    mtidServerBusy    : TRegMultiString = nil; // 'Servers are busy creating backup files. Please wait.'
    mtidServerNotBusy : TRegMultiString = nil; // 'Backup to disk completed.'
    mtidPostMan       : TRegMultiString = nil; // 'THE POSTMAN'
    mtidYouHaveMail   : TRegMultiString = nil; // 'You have mail from %s (%s)'
    mtidEntersChannel : TRegMultiString = nil; // 'left channel'
    mtidEntersLobby   : TRegMultiString = nil; // 'left lobby'
    mtidLeftChannel   : TRegMultiString = nil; // 'left channel'
    mtidLeftLobby     : TRegMultiString = nil; // 'left lobby'
    mtidMaintDue      : TRegMultiString = nil; // 'Server will go down for maintenance in %s minutes. Expected downtime aprox. %s minutes.'

  procedure SaveMLS;
  procedure LoadMLS;

implementation

  uses
    SysUtils;

  procedure SaveMLS;
    var
      Dict : TDictionary;
    begin
      Dict := Languages.CreateDictionary;
      try
        Dict.Store(ExtractFilePath(paramstr(0)) + 'isdict.lang');
      finally
        Dict.Free;
      end;
    end;

  procedure LoadMLS;
    var
      Dict : TDictionary;
      i    : integer;
      root : string;
    begin
      root := ExtractFilePath(paramstr(0)) + 'languages\is\';
      Languages.LoadMLSFrom(root);
      for i := 0 to pred(LangList.Count) do
        begin
          Dict := TDictionary.Create(root + LangList[i] + '\isdict.lang');
          try
            Languages.ApplyDictionary(Dict, LangList[i]);
          finally
            Dict.Free;
          end;
        end;
    end;


initialization

    mtidSystem        := TRegMultiString.Create('mtidSystem', 'SYSTEM');
    mtidClientAware   := TRegMultiString.Create('mtidClientAware', '%s has entered %s');
    mtidClientUnAware := TRegMultiString.Create('mtidClientUnAware',  '%s has left %s.');
    mtidChannelInfo   := TRegMultiString.Create('mtidChannelInfo', 'Channel "%s", created by %s. ');
    mtidUser          := TRegMultiString.Create('mtidUser', 'user');
    mtidUsers         := TRegMultiString.Create('mtidUsers', 'users');
    mtidAnd           := TRegMultiString.Create('mtidAnd', 'and');
    mtidChanNeedsPass := TRegMultiString.Create('mtidChanNeedsPass', 'You need a password to enter this channel.');
    mtidNoChanInfo    := TRegMultiString.Create('mtidNoChanInfo', 'There is no information associated to this object.');
    mtidServerBusy    := TRegMultiString.Create('mtidServerBusy', 'Servers are busy creating backup files. Please wait.');
    mtidServerNotBusy := TRegMultiString.Create('mtidServerNotBusy', 'Backup to disk completed.');
    mtidPostMan       := TRegMultiString.Create('mtidPostMan', 'THE POSTMAN');
    mtidYouHaveMail   := TRegMultiString.Create('mtidYouHaveMail', 'You have mail from %s (%s).');
    mtidEntersChannel := TRegMultiString.Create('mtidEntersChannel', '%s entered channel %s.');
    mtidEntersLobby   := TRegMultiString.Create('mtidEntersLobby', '%s entered lobby.');
    mtidLeftChannel   := TRegMultiString.Create('mtidLeftChannel', '%s left channel %s.');
    mtidLeftLobby     := TRegMultiString.Create('mtidLeftLobby', '%s left lobby.');
    mtidMaintDue      := TRegMultiString.Create('mtidMaintDue', 'Server will go down for maintenance in %s minutes. Estimated downtime %s minutes.');

end.
