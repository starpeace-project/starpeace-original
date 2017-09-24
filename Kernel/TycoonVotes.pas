unit TycoonVotes;

interface

  uses
    SysUtils, Collection, Persistent, BackupInterfaces;

  type
    TVoteInfo =
      class
        fLocation : TObject;
        fTycoon   : TObject;
      end;

    TVoteSystem =
      class(TPersistent)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fVotes : TCollection;
        private
          function  FindInfo(Loc : TObject) : TVoteInfo;
          function  GetVote(Loc : TObject) : TObject;
          procedure SetVote(Loc, Tycoon : TObject);
        public
          property Votes[Location : TObject] : TObject read GetVote write SetVote; default;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        public
          procedure Clear;
          procedure ClearVotes(Tycoon : TObject);
          procedure UpdateLocation(OldLoc, NewLoc : TObject);
      end;

  procedure RegisterBackup;

implementation

  // TVoteSystem

  constructor TVoteSystem.Create;
    begin
      inherited Create;
      fVotes := TCollection.Create(0, rkBelonguer);
    end;

  destructor TVoteSystem.Destroy;
    begin
      fVotes.Free;
      inherited;
    end;

  function TVoteSystem.FindInfo(Loc : TObject) : TVoteInfo;
    var
      i : integer;
    begin
      i := pred(fVotes.Count);
      while (i >= 0) and (TVoteInfo(fVotes[i]).fLocation <> Loc) do
        dec(i);
      if i >= 0
        then result := TVoteInfo(fVotes[i])
        else result := nil;
    end;

  function TVoteSystem.GetVote(Loc : TObject) : TObject;
    var
      Info : TVoteInfo;
    begin
      Info := FindInfo(Loc);
      if Info <> nil
        then result := Info.fTycoon
        else result := nil;
    end;

  procedure TVoteSystem.SetVote(Loc, Tycoon : TObject);
    var
      Info : TVoteInfo;
    begin
      Info := FindInfo(Loc);
      if Info <> nil
        then Info.fTycoon := Tycoon
        else
          begin
            Info := TVoteInfo.Create;
            Info.fLocation := Loc;
            Info.fTycoon := Tycoon;
            fVotes.Insert(Info);
          end;
    end;

  procedure TVoteSystem.LoadFromBackup(Reader : IBackupReader);
    var
      i    : integer;
      aux  : string;
      cnt  : integer;
      Info : TVoteInfo;
    begin
      cnt := Reader.ReadInteger('vCount', 0);
      fVotes := TCollection.Create(cnt, rkBelonguer);
      for i := 0 to pred(cnt) do
        begin
          aux  := IntToStr(i);
          Info := TVoteInfo.Create;
          Reader.ReadObject('loc' + aux, Info.fLocation, nil);
          Reader.ReadObject('tyc' + aux, Info.fTycoon, nil);
          fVotes.Insert(Info);
        end;
    end;

  procedure TVoteSystem.StoreToBackup(Writer : IBackupWriter);
    var
      i   : integer;
      aux : string;
    begin
      Writer.WriteInteger('vCount', fVotes.Count);
      for i := 0 to pred(fVotes.Count) do
        with TVoteInfo(fVotes[i]) do
          begin
            aux := IntToStr(i);
            Writer.WriteObjectRef('loc' + aux, fLocation);
            Writer.WriteObjectRef('tyc' + aux, fTycoon);
          end;
    end;

  procedure TVoteSystem.Clear;
    begin
      fVotes.DeleteAll;
    end;

  procedure TVoteSystem.ClearVotes(Tycoon : TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(fVotes.Count) do
        if TVoteInfo(fVotes[i]).fTycoon = Tycoon
          then TVoteInfo(fVotes[i]).fTycoon := nil;
    end;

  procedure TVoteSystem.UpdateLocation(OldLoc, NewLoc : TObject);
    var
      VoteInfo : TVoteInfo;
    begin
      VoteInfo := FindInfo(OldLoc);
      if VoteInfo <> nil
        then VoteInfo.fLocation := NewLoc;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TVoteSystem);
    end;

end.
