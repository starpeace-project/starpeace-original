unit ClassStorageInt;

interface

  uses
    SysUtils;

  type
    TClassFamilyId = string;
    TClassId       = string;

  type
    IConfigHandler =
      interface
        function GetInteger( id : integer ) : integer;
        function GetFloat( id : integer ) : double;
        function GetString( id : integer ) : string;
        function GetMoney( id : integer ) : currency;
        function GetIntegerArray( id : integer; i, j, k, l : integer ) : integer;
        function GetFloatArray( id : integer; i, j, k, l : integer ) : double;
        function GetStringArray( id : integer; i, j, k, l : integer ) : string;
        function GetMoneyArray( id : integer; i, j, k, l : integer ) : currency;
        function GetConfigParm( strId, defVal : string ) : string;
      end;

  type
    TClassStorage =
      class
        protected
          function GetClassByIdx( ClassFamilyId : TClassFamilyId; Idx : integer  ) : TObject; virtual; abstract;
          function GetClassById ( ClassFamilyId : TClassFamilyId; Id  : TClassId ) : TObject; virtual; abstract;
          function GetClassCount( ClassFamilyId : TClassFamilyId ) : integer;                 virtual; abstract;
        published
          procedure RegisterClass( ClassFamilyId : TClassFamilyId; TheClassId : TClassId; TheClass : TObject ); virtual; abstract;
          procedure RegisterEqv( ClassFamilyId : TClassFamilyId; OldId, NewId : TClassId ); virtual; abstract;
        public
          property  ClassByIdx[ClassFamilyId : TClassFamilyId; Idx : integer]  : TObject read GetClassByIdx;
          property  ClassById [ClassFamilyId : TClassFamilyId; Id  : TClassId] : TObject read GetClassById;
          property  ClassCount[ClassFamilyId : TClassFamilyId] : integer read GetClassCount;
      end;

    type
      EClassStorageException = class( Exception );


implementation

end.
