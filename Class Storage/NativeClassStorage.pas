unit NativeClassStorage;

interface

  uses
    ClassStorageInt, Classes, Windows, SysUtils;

  type
    TNativeClassStorage =
      class( TClassStorage )
        public
          constructor Create;
          destructor  Destroy; override;
        protected
          function GetClassByIdx( ClassFamilyId : TClassFamilyId; Idx : integer  ) : TObject; override;
          function GetClassById ( ClassFamilyId : TClassFamilyId; Id  : TClassId ) : TObject; override;
          function GetClassCount( ClassFamilyId : TClassFamilyId ) : integer;                 override;
        published
          procedure RegisterClass( ClassFamilyId : TClassFamilyId; TheClassId : TClassId; TheClass : TObject ); override;
          procedure RegisterEqv( ClassFamilyId : TClassFamilyId; OldId, NewId : TClassId ); override;
        private
          fFamilies : TStringList;
          fEqvList  : TStringList;
          fLock     : TRTLCriticalSection;  
      end;

implementation


  // TNativeClassStorage

  constructor TNativeClassStorage.Create;
    begin
      inherited;
      InitializeCriticalSection( fLock ); 
      EnterCriticalSection( fLock );
      fFamilies := TStringList.Create;
      fFamilies.Sorted     := true;
      fFamilies.Duplicates := dupError;
      fEqvList := TStringList.Create;
      LeaveCriticalSection( fLock );
    end;
    
  destructor TNativeClassStorage.Destroy;
    var
      i      : integer;
      Family : TStringList;
    begin
      EnterCriticalSection( fLock );
      for i := 0 to pred(fFamilies.Count) do
        begin
          Family := TStringList(fFamilies.Objects[i]);
          { >> Pending for solution 
          for j := 0 to pred(Family.Count) do
            Family.Objects[j].Free;
          }
          Family.Clear;
          Family.Free;
        end;
      fFamilies.Clear;
      fFamilies.Free;
      fEqvList.Free;
      LeaveCriticalSection( fLock );
      DeleteCriticalSection( fLock );
      inherited;
    end;

  function TNativeClassStorage.GetClassByIdx( ClassFamilyId : TClassFamilyId; Idx : integer  ) : TObject;
    var
      Family : TStringList;
      FamIdx : integer;
    begin
      result := nil;
      EnterCriticalSection( fLock );
      try
        if fFamilies.Find( ClassFamilyId, FamIdx )
          then
            begin
              Family := fFamilies.Objects[FamIdx] as TStringList;
              if Idx < Family.Count
                then result := Family.Objects[Idx]
                else raise EClassStorageException.Create( 'Index out of range in family "' + ClassFamilyId + '".' );
            end
          else raise EClassStorageException.Create( 'Unknown class family "' + ClassFamilyId + '".');
      finally
        LeaveCriticalSection( fLock );
      end;
    end;

  function TNativeClassStorage.GetClassById( ClassFamilyId : TClassFamilyId; Id : TClassId ) : TObject;
    var
      Family : TStringList;
      Idx    : integer;
      NewId  : string;
    begin
      result := nil;
      EnterCriticalSection( fLock );
      try
        if fFamilies.Find( ClassFamilyId, Idx )
          then
            begin
              Family := fFamilies.Objects[Idx] as TStringList;
              if Family.Find( Id, Idx )
                then result := Family.Objects[Idx]
                else
                  begin
                    NewId := fEqvList.Values[ClassFamilyId + '.' + Id];
                    if NewId <> ''
                      then result := GetClassById( ClassFamilyId, NewId )
                      else result := nil //raise EClassStorageException.Create( 'Unknown class "' + Id + '" in family "' + ClassFamilyId + '".');
                  end
            end
          else raise EClassStorageException.Create( 'Unknown class family "' + ClassFamilyId + '".');
      finally
        LeaveCriticalSection( fLock );
      end;
    end;

  function TNativeClassStorage.GetClassCount( ClassFamilyId : TClassFamilyId ) : integer;
    var
      Idx : integer;
    begin
      EnterCriticalSection( fLock );
      try
        if fFamilies.Find( ClassFamilyId, Idx )
          then result := (fFamilies.Objects[Idx] as TStringList).Count
          else result := 0//raise EClassStorageException.Create( 'Unknown class family "' + ClassFamilyId + '".');
      finally
        LeaveCriticalSection( fLock );
      end;
    end;

  procedure TNativeClassStorage.RegisterClass( ClassFamilyId : TClassFamilyId; TheClassId : TClassId; TheClass : TObject );
    var
      Family : TStringList;
      FamIdx : integer;
    begin
      EnterCriticalSection( fLock );
      if fFamilies.Find( ClassFamilyId, FamIdx )
        then Family := fFamilies.Objects[FamIdx] as TStringList
        else
          begin
            Family := TStringList.Create;
            Family.Sorted     := true;
            Family.Duplicates := dupError;
            fFamilies.AddObject( ClassFamilyId, Family );
          end;
      Family.AddObject( TheClassId, TheClass );
      LeaveCriticalSection( fLock );
    end;

  procedure TNativeClassStorage.RegisterEqv( ClassFamilyId : TClassFamilyId; OldId, NewId : TClassId );
    begin
      fEqvList.Add( ClassFamilyId + '.' + OldId + '=' + NewId );
    end;

end.
