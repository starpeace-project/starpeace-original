unit SubscritionsCounter;

interface

  uses
    DirServerSession, Daemons;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;

implementation

  uses
    Windows, Classes, SysUtils, Logs;

  const
    cLogId = 'Subscriptions Daemon';

  type
    TUserHash = 'A'..'Z';

  type
    TSubscriptionsCouterDaemon =
      class(TBasicDaemon)
        private // IDaemon
          function  GetName : string;                      override;
          function  GetDescription : string;               override;
        private
          procedure Execute;                               override;
          function  GetLogId : string;                     override;
      end;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;
    begin
      Result := TSubscriptionsCouterDaemon.Create(DSAddr, DSPort);
    end;

  // TRankingsDaemon

  function TSubscriptionsCouterDaemon.GetName : string;
    begin
      Result := cLogId;
    end;

  function TSubscriptionsCouterDaemon.GetDescription : string;
    begin
      Result := 'Subscriptions counter daemon';
    end;

  procedure TSubscriptionsCouterDaemon.Execute;
    var
      hash      : TUserHash;
      usercount : integer;
      subscount : integer;
      Keys      : TStringList;
      //i         : integer;
    begin
      inherited;
      if fSession.SetCurrentKey('Root/Users')
        then
          begin
            Keys := TStringList.Create;
            try
              usercount := 0;
              subscount := 0;
              for hash := low(hash) to high(hash) do
                if fSession.SetCurrentKey('Root/Users' + '/' + hash)
                  then
                    begin
                      Keys.Text := fSession.GetKeyNames;
                      inc(usercount, Keys.Count);
                      {
                      for i := 0 to pred(Keys.Count) do
                        if fSession.SetCurrentKey('Root/Users' + '/' + hash + '/' + Keys[i])
                          then
                            if fSession.KeyExists('Subscription')
                              then inc(subscount);
                      }
                    end;
              Log(cLogId, IntToStr(usercount) + ' accounts exist');
              Log(cLogId, IntToStr(subscount) + ' have subscribed');
            finally
              Keys.Free;
            end;
          end;
    end;

  function TSubscriptionsCouterDaemon.GetLogId : string;
    begin
      Result := cLogId;
    end;

end.

