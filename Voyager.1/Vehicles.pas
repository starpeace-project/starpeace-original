unit Vehicles;

interface

  uses
    ActorPool;

  type
    IVehicle      = interface;
    IVehicleArray = interface;

    TVehicleDirection      = (vdirN, vdirNE, vdirE, vdirSE, vdirS, vdirSW, vdirW, vdirNW);
    TArrayChange           = (achUpdate, achVehicleInsertion, achVehicleDeletion);
    TOnVehicleArrayChanged = procedure( VehicleArray : IVehicleArray; ArrayChange : TArrayChange; const Info ) of object;

    IVehicle =
      interface
        function  getX           : single;
        function  getY           : single;
        function  getAngle       : single;
        function  getDir         : TVehicleDirection;
        function  getNextDir     : TVehicleDirection;
        function  getVisualClass : word;
        function  getCustomData  : pointer;
        function  getGroupId     : integer;
        procedure setCustomData( data : pointer; datasize : integer );
      end;

    IVehicleArray =
      interface
        function  getVehicleCount : integer;
        function  getVehicle( index : integer ) : IVehicle;
        procedure RegisterNotificationProc  ( OnArrayChanged : TOnVehicleArrayChanged );
        procedure UnregisterNotificationProc( OnArrayChanged : TOnVehicleArrayChanged );
      end;

  const
    evnAnswerVehicleArray = 3425;
    msgAnswerVehicle      = 3426;

  type
    TMsgAnswerVehicleArrayInfo =
      record
        ActorPoolId  : TActorPoolId;
        VehicleArray : IVehicleArray;
      end;

    TMsgAnswerVehicle =
      record
        MsgId   : word;
        Vehicle : IVehicle;
      end;

implementation

end.



