unit Notifications;

interface

  const
    NullId = 0;

  type
    TEventClass = byte;

    IHook =
      interface
        procedure Handle( EventClass : TEventClass; var Info );
      end;

  procedure InitNotificationEngine;
  procedure DoneNotificationEngine;
  procedure RegisterEventClass( EventClass : TEventClass; Parent : TEventClass );
  procedure AddNotifier( EventClass : TEventClass; Hook : IHook );
  procedure DeleteNotifier( EventClass : TEventClass; Hook : IHook );
  procedure DispatchEvent( EventClass : TEventClass; var Info );

implementation

  uses
    Windows, InterfaceCollection;

  type
    TEventId = byte;

  type
    TEventClassInfo =
      class
        public
          constructor Create( aParent : TEventId );
          destructor  Destroy; override;
        private
          fParent    : TEventId;
          fNotifiers : TInterfaceCollection;
        public
          property Parent : TEventId read fParent;
          property Notifiers : TInterfaceCollection read fNotifiers;
      end;

  // TEventClassInfo

  constructor TEventClassInfo.Create( aParent : TEventId );
    begin
      inherited Create;
      fParent    := aParent;
      fNotifiers := TInterfaceCollection.Create;
    end;

  destructor TEventClassInfo.Destroy;
    begin
      fNotifiers.Free;
      inherited;
    end;

  var
    EventClasses : array[TEventId] of TEventClassInfo;

  procedure InitNotificationEngine;
    begin
      ZeroMemory( @EventClasses, sizeof(EventClasses) );
    end;

  procedure DoneNotificationEngine;
    var
      i : byte;
    begin
      for i := 0 to high(TEventId) do
        if EventClasses[i] <> nil
          then EventClasses[i].Free;
    end;

  procedure RegisterEventClass( EventClass : TEventClass; Parent : TEventClass );
    var
      EventId  : TEventId;
    begin
      EventId := TEventId(EventClass);
      EventClasses[EventId] := TEventClassInfo.Create( Parent );
    end;

  procedure AddNotifier( EventClass : TEventClass; Hook : IHook );
    var
      EventClassInfo : TEventClassInfo;
    begin
      EventClassInfo := EventClasses[EventClass];
      if EventClassInfo <> nil
        then EventClassInfo.Notifiers.Insert( Hook );
    end;

  procedure DeleteNotifier( EventClass : TEventClass; Hook : IHook );
    var
      EventClassInfo : TEventClassInfo;
    begin
      EventClassInfo := EventClasses[EventClass];
      if EventClassInfo <> nil
        then EventClassInfo.Notifiers.Delete( Hook );
    end;

  procedure DispatchEvent( EventClass : TEventClass; var Info );
    var
      EventClassInfo : TEventClassInfo;
      i              : integer;
    begin
      EventClassInfo := EventClasses[EventClass];
      if EventClassInfo <> nil
        then
          begin
            for i := 0 to pred(EventClassInfo.Notifiers.Count) do
              IHook(EventClassInfo.Notifiers[i]).Handle( EventClass, Info );
            if EventClassInfo.Parent <> NullId
              then DispatchEvent( EventClassInfo.Parent, Info );
          end;
    end;

end.

