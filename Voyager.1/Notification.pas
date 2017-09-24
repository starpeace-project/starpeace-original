unit Notification;

interface

  type
    INotificator =
      interface
        procedure ShowNotification( Kind : integer; Title, Body : string; Options : integer );
      end;

  const
    evnAnswerNotificator = 3440;

implementation

end.


