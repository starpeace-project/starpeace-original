unit Config;

interface

  uses
    Classes;

  const
    evnAnswerConfigHolder = 910;

  type
    IConfigHolder =
      interface
        // Read functions
        function ReadString ( System : boolean; DataOwner, DataId, DefaultValue : string ) : string;
        function ReadInteger( System : boolean; DataOwner, DataId : string; DefaultValue : integer ) : integer;
        function ReadBoolean( System : boolean; DataOwner, DataId : string; DefaultValue : boolean ) : boolean;

        // Write functions
        procedure WriteString ( System : boolean; DataOwner, DataId, Value : string );
        procedure WriteInteger( System : boolean; DataOwner, DataId : string; Value : integer );
        procedure WriteBoolean( System : boolean; DataOwner, DataId : string; Value : boolean );

        // Misc
        procedure RetrieveUserList( UserList : TStrings ); 
      end;

implementation

end.
