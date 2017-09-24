unit Actions;

interface

  uses
    IBSystem;

  type
    TModelServerMetaAction =
      class(TMetaAction)
        public
          procedure Create;
      end;

    TModelServerAction =
      class(TAction)
        public
          procedure Execute( const Info; var outcome : single ); override;
          procedure Init( const Info );              override;
        protected
          fId         : integer;
          fIntensity  : single;
          fRadius     : integer;
      end;

    TInternalAction =
      class(TAction)
        public
          procedure Execute( const Info; var outcome : single ); override;
        protected
          fCriminals     : array[0..MAXTEAM_MEMBERS - 1] of TCriminal;
          fCriminalCount : integer;
          fTeam          : TTeam;
      end;

    TAbort =
      class(TInternalAction)
        public
          procedure Execute( const Info; var outcome : single ); override;
      end;

implementation

  // TModelServerMetaAction

  procedure TModelServerMetaAction.Create;
    begin
      inherited Create( 'MSA', TModelServerAction, false );
    end;

  // TModelServerAction

  procedure TModelServerAction.Init( const Info );
    var
      data : TModelServerActionData absolute Info;
    begin
      fId        := data.Id;
      fIntensity := data.Intensity;
      fRadius    := data.Radius;
    end;

  procedure TModelServerAction.Execute( const Info; var outcome : single );
    begin
      //>> todo!
    end;

  // TInternalAction

  procedure TInternalAction.Execute( const Info; var outcome : single );
    var
      ActionData : TInternalActionData absolute Info;
      i          : integer;
    begin
      fTeam          := ActionData.Team;
      fCriminalCount := ActionData.CriminalCount;
      for i := 0 to pred(fCriminalCount) do
        fCriminals[i] := ActionData.Criminals[i]
    end;

  // TAbort

  procedure TAbort.Execute( const Info; var outcome : single );
    begin
      inherited;
      if fTeam.Mission <> nil
        then fTeam.Mission.Abort;
    end;

end.
