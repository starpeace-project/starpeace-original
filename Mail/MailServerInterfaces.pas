unit MailServerInterfaces;

interface

  uses
    Classes;
    
  type
    IMailServer =
      interface
        function NewMailAccount(Account, Alias, FwdAddr : string; KeepMsg : boolean) : boolean;
        function DeleteAccount (Account : string) : boolean;
        function SetForwardRule(Account, FwdAddr : string; KeepMsg : boolean) : boolean;
        function SendMessage(From, Dest, Subject : string; Lines : TStringList; html : boolean) : boolean;
        function SendHTMLMessage(From, Dest, Subject, URL : string) : boolean;
        procedure UpdateDate(date : TDateTime);
      end;

implementation

end.
 