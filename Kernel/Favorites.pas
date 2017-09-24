unit Favorites;

interface

  uses
    Collection, Persistent, BackupInterfaces;

  type
    TFavItem =
      class( TPersistent )
        public
          constructor Create( anId, aKind : integer; aName, aInfo : string );
          destructor  Destroy; override;
        private
          fId     : integer;
          fName   : string;
          fKind   : byte;
          fInfo   : string;
          fItems  : TCollection;
          fIsRoot : boolean;
        public
          property Id    : integer     read fId;
          property Name  : string      read fName  write fName;
          property Kind  : byte        read fKind;
          property Items : TCollection read fItems;
          property Info  : string      read fInfo;
        public
          function  LocateSubItem( Id : integer ) : TFavItem;
          function  Serialize : string;
          function  MatchCord(x, y : integer) : boolean;
          procedure DeleteCord(x, y : integer);
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fDeleted : boolean;
      end;

    TFavorites =
      class( TPersistent )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fRoot   : TFavItem;
          fLastId : integer;
        published
          function RDONewItem    ( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
          function RDODelItem    ( Location : widestring ) : OleVariant;
          function RDOMoveItem   ( ItemLoc : widestring; Dest : widestring ) : OleVariant;
          function RDORenameItem ( ItemLoc : widestring; Name : widestring ) : OleVariant;
          function RDOGetSubItems( ItemLoc : widestring ) : OleVariant;
        public
          procedure DeleteCord(x, y : integer);
        private
          function LocateItem( Location : string; out ParentLocation : string ) : TFavItem;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          property Root : TFavItem read fRoot;
        public
          function CheckIntegrity : boolean;
      end;

  procedure RegisterBackup;

implementation

  uses
    CompStringsParser, SysUtils, Protocol, FavProtocol, Logs, Kernel,
    MetaInstances;

  // TFavItem

  constructor TFavItem.Create( anId, aKind : integer; aName, aInfo : string );
    begin
      inherited Create;
      fId    := anId;
      fKind  := aKind;
      fInfo  := aInfo;
      fName  := aName;
      fItems := TCollection.Create( 0, rkBelonguer );
    end;

  destructor TFavItem.Destroy;
    begin
      fItems.Free;
      inherited;
    end;

  function TFavItem.LocateSubItem( Id : integer ) : TFavItem;
    var
      i : integer;
    begin
      i := 0;
      while (i < fItems.Count) and (TFavItem(fItems[i]).Id <> Id) do
        inc( i );
      if i < fItems.Count
        then result := TFavItem(fItems[i])
        else result := nil;
    end;

  function TFavItem.Serialize : string;
    var
      i, count : integer;
    begin
      count := 0;
      for i := 0 to pred(fItems.Count) do
        if TFavItem(fItems[i]).Kind = fvkFolder
          then inc( count );
      result := FavProtocol.SerializeItemProps( Id, Kind, Name, Info, count );
    end;

  function TFavItem.MatchCord(x, y : integer) : boolean;
    var
      name   : string;
      x1, y1 : integer;
      sel    : boolean;
    begin
      if Kind = fvkLink
        then
          if Protocol.ParseLinkCookie(fInfo, name, x1, y1, sel)
            then result := (x = x1) and (y1 = y)
            else result := false
        else result := false;
    end;

  procedure TFavItem.DeleteCord(x, y : integer);
    var
      i : integer;
      Item : TFavItem;
    begin
      for i := pred(fItems.Count) downto 0 do
        begin
          Item := TFavItem(fItems[i]);
          Item.DeleteCord(x, y);
          if Item.MatchCord(x, y)
            then
              begin
                Item.fDeleted := true;
                fItems.AtExtract(i); // >> Bugtrap!
              end;
        end;
    end;

  procedure TFavItem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fId   := Reader.ReadInteger( 'Id', 0 );
      fName := Reader.ReadString( 'Name', 'Unknown' );
      fKind := Reader.ReadInteger( 'Kind', 0 );
      fInfo := Reader.ReadString( 'Info', '' );
      Reader.ReadObject( 'Items', fItems, nil );
    end;

  procedure TFavItem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'Id', fId );
      Writer.WriteString( 'Name', fName );
      Writer.WriteInteger( 'Kind', fKind );
      Writer.WriteString( 'Info', fInfo );
      Writer.WriteLooseObject( 'Items', fItems );
      if fDeleted
        then
          try
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error storing favorites Item: ' + fInfo );
          except
            Logs.Log( tidLog_Survival, 'Error storing favorites Item!' );
          end;
    end;


  // TFavorites

  constructor TFavorites.Create;
    begin
      inherited Create;
      fRoot := TFavItem.Create( 0, fvkFolder, 'Favorites', '' );
      fRoot.fIsRoot := true;
    end;

  destructor TFavorites.Destroy;
    begin
      fRoot.Free;
      inherited;
    end;

  function TFavorites.RDONewItem( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
    var
      Parent  : TFavItem;
      Item    : TFavItem;
      Useless : string;
    begin
      Parent := LocateItem( Location, Useless );
      if Parent <> nil
        then
          begin
            inc( fLastId );
            Item := TFavItem.Create( fLastId, Kind, Name, Info );
            Parent.Items.Insert( Item );
            result := fLastId;
          end
        else result := -1;
    end;

  function TFavorites.RDODelItem( Location : widestring ) : OleVariant;
    var
      ParentLoc : string;
      Useless   : string;
      Parent    : TFavItem;
      Item      : TFavItem;
    begin
      Item := LocateItem( Location, ParentLoc );
      if (Item <> nil)
        then
          if not Item.fIsRoot
            then
              begin
                Parent := LocateItem( ParentLoc, Useless );
                if Parent <> nil
                  then
                    begin
                      Item.fDeleted := true;
                      Parent.Items.Extract( Item );
                      result := true;
                    end
                  else result := false
              end
          else
            begin
              Logs.Log('favorites', TimeToStr(Now) + ' WARNING!! Trying to delete Root ' + Location);
              result := false;
            end
        else result := false;
    end;

  function TFavorites.RDOMoveItem( ItemLoc : widestring; Dest : widestring ) : OleVariant;
    var
      ParentLoc : string;
      Useless   : string;
      Parent    : TFavItem;
      NewParent : TFavItem;
      Item      : TFavItem;
    begin
      if (ItemLoc <> '') and (system.pos(ItemLoc, Dest) <> 1)
        then
          begin
            Item := LocateItem( ItemLoc, ParentLoc );
            if (Item <> nil) and not Item.fIsRoot
              then
                begin
                  Parent := LocateItem( ParentLoc, Useless );
                  if Parent <> nil
                    then
                      begin
                        NewParent := LocateItem( Dest, Useless );
                        if NewParent <> nil
                          then
                            begin
                              Parent.Items.Extract( Item );
                              NewParent.Items.Insert( Item );
                              result := true;
                            end
                          else result := false
                      end
                    else result := false
                end
              else result := false;
          end
        else result := false;
    end;

  function TFavorites.RDORenameItem( ItemLoc : widestring; Name : widestring ) : OleVariant;
    var
      ParentLoc : string;
      Item      : TFavItem;
    begin
      Item := LocateItem( ItemLoc, ParentLoc );
      if Item <> nil
        then
          begin
            if length(Name) > 50
              then Item.Name := system.copy(Name, 1, 50)
              else Item.Name := Name;
            result := true;
          end
        else result := false
    end;

  function TFavorites.RDOGetSubItems( ItemLoc : widestring ) : OleVariant;
    var
      ParentLoc : string;
      Item      : TFavItem;
      Child     : TFavItem;
      i         : integer;
    begin
      Item := LocateItem( ItemLoc, ParentLoc );
      if Item <> nil
        then
          begin
            result := '';
            for i := 0 to pred(Item.Items.Count) do
              begin
                Child  := TFavItem(Item.Items[i]);
                result := result + Child.Serialize + chrItemSeparator;
              end;
          end
        else result := ''
    end;

  procedure TFavorites.DeleteCord(x, y : integer);
    begin
      fRoot.DeleteCord(x, y);
    end;

  function TFavorites.LocateItem( Location : string; out ParentLocation : string ) : TFavItem;
    var
      idStr : string;
      oldpos, pos : integer;
    begin
      result := fRoot;
      if Location <> ''
        then
          begin
            pos := 1;
            repeat
              oldpos := pos;
              idStr  := GetNextStringUpTo( Location, pos, chrPathSeparator );
              if idStr <> ''
                then
                  begin
                    ParentLocation := system.copy( Location, 1, oldpos - 1 );
                    result := result.LocateSubItem( StrToInt(idStr) );
                  end;
            until (idStr = '') or (result = nil);
          end
        else ParentLocation := '';
    end;

  procedure TFavorites.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Root', fRoot, nil );
      if fRoot <> nil
        then fRoot.fIsRoot := true;
      fLastId := Reader.ReadInteger( 'LastId', 0 );
    end;

  procedure TFavorites.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Root', fRoot );
      Writer.WriteInteger( 'LastId', fLastId );
    end;

  function TFavorites.CheckIntegrity : boolean;
    begin
      result := (fRoot <> nil) and MetaInstances.ObjectIs('TFavItem', fRoot); 
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TFavItem );
      RegisterClass( TFavorites );
    end;

end.



