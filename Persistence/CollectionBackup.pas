unit CollectionBackup;

interface

  uses
    Collection, BackupInterfaces;

  type
    TCollectionBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    TNotifiedCollectionBackupAgent =
      class(TCollectionBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    TLockableCollectionBackupAgent =
      class(TCollectionBackupAgent)
        protected
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    TSortedCollectionBackupAgent =
      class(TCollectionBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    TNotifiedSortedCollectionBackupAgent =
      class(TSortedCollectionBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils;

  // TCollectionBackupAgent

  class procedure TCollectionBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    var
      index : integer;
    begin
      with TCollection(Obj) do
        begin
          Stream.WriteInteger('Count', Count);
          Stream.WriteInteger('RelKind', integer(RelKind));
          if RelKind = rkBelonguer
            then
              for index := 0 to pred(Count) do
                Stream.WriteObject(IntToStr(index), Items[index])
            else
              for index := 0 to pred(Count) do
                Stream.WriteObjectRef(IntToStr(index), Items[index]);
        end;
    end;

  class procedure TCollectionBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    var
      cnt   : integer;
      rel   : TRelationshipKind;
      index : integer;
    begin
      cnt := Stream.ReadInteger('Count', 0);
      rel := TRelationshipKind(Stream.ReadInteger('RelKind', 0));
      with TCollection(Obj) do
        begin
          friend_RelKind  := rkUse;
          friend_Capacity := cnt;
          for index := 0 to pred(cnt) do
            Stream.ReadObject(IntToStr(index), friend_Items[index], nil);
          friend_Count   := cnt;
          friend_RelKind := rel;
        end;
    end;

  // TNotifiedCollectionBackupAgent

  class procedure TNotifiedCollectionBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      inherited Read(Stream, Obj);
      Stream.ReadMethod('OnModified', TMethod(TNotifiedCollection(Obj).friend_OnModified), NULLPROC);
    end;

  class procedure TNotifiedCollectionBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      inherited Write(Stream, Obj);
      Stream.WriteMethod('OnModified', TMethod(TNotifiedCollection(Obj).OnModified));
    end;

  // TLockableCollectionBackupAgent

  class procedure TLockableCollectionBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      inherited Read(Stream, Obj);
      TLockableCollection(Obj).InitLock;
    end;

  // TSortedCollectionBackupAgent

  class procedure TSortedCollectionBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      inherited Read(Stream, Obj);
      Stream.ReadMethod('CompFuct', TMethod(TSortedCollection(Obj).friend_CompFunct), NULLPROC);
    end;

  class procedure TSortedCollectionBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      inherited Write(Stream, Obj);
      Stream.WriteMethod('CompFuct', TMethod(TSortedCollection(Obj).friend_CompFunct));
    end;

  // TNotifiedSortedCollectionBackupAgent

  class procedure TNotifiedSortedCollectionBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      inherited Read(Stream, Obj);
      Stream.ReadMethod('OnModified', TMethod(TNotifiedCollection(Obj).friend_OnModified), NULLPROC);
    end;

  class procedure TNotifiedSortedCollectionBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      inherited Write(Stream, Obj);
      Stream.WriteMethod('OnModified', TMethod(TNotifiedCollection(Obj).OnModified));
    end;


  // RegisterCollections

  procedure RegisterBackup;
    begin
      TCollectionBackupAgent.Register([TCollection]);
      TNotifiedCollectionBackupAgent.Register([TNotifiedCollection]);
      TLockableCollectionBackupAgent.Register([TLockableCollection]);
      TSortedCollectionBackupAgent.Register([TSortedCollection]);
      TNotifiedSortedCollectionBackupAgent.Register([TNotifiedSortedCollection]);
    end;

end.
