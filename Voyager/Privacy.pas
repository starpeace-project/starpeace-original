unit Privacy;

interface

  type
    IPrivacyHandler =
      interface
        procedure IgnoreUser( username : string );
        procedure ClearIgnoredUser( username : string );
        function  UserIsIgnored( username : string ) : boolean;
        procedure GetDefaultChannelData( out name, password : string );
        procedure SetDefaultChannelData( name, password : string );
      end;

  const
    evnAnswerPrivacyHandler = 5700;

implementation

end.
