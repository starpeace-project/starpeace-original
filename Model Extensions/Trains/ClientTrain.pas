unit ClientTrain;

interface

  uses
    Classes, StateEngine, DistributedStates, ActorTypes, ActorPool, Automaton, Train, Vehicles;

  const
    HighResFreqMult = 10;

  type
    TClientCar = class;

    TClientCarModification = (ccmInserted, ccmDeleted);
    TOnClientCarModified   = procedure( Car : TClientCar; Modification : TClientCarModification ) of object;

    TClientCar =
      class( TCar, IVehicle )
        public
          destructor Destroy; override;
        private
          fHigResX      : single;
          fHigResY      : single;
          fHigResAngle  : single;
          fHigResSpeedX : single;
          fHigResSpeedY : single;
          fNoSynchAct   : boolean;
          fLastAngle    : single;
        protected
          procedure Act( State : TStateId; Mode : TActMode );             override;
          function  HandleEvent( anEvent : TDistributedEvent ) : boolean; override;
          procedure Load( Stream : TStream );                             override;
        private
          fOnClientTrainModified : TOnClientCarModified;
        public
          property OnClientTrainModified : TOnClientCarModified read fOnClientTrainModified write fOnClientTrainModified;
        // IVehicle
        private
          function  getX           : single;
          function  getY           : single;
          function  getAngle       : single;
          function  getDir         : TVehicleDirection;
          function  getNextDir     : TVehicleDirection;
          function  getGroupId     : integer;
          function  getVisualClass : word;
          function  getCustomData  : pointer;
          procedure setCustomData( data : pointer; datasize : integer );
          procedure Deleted; override;
        private
          fCustomData : pointer;
          fDataSize   : integer;
        protected
          procedure MsgAnswerVehicle( var Msg : TMsgAnswerVehicle ); message msgAnswerVehicle;
      end;

implementation

  uses
    LogFile, SysUtils, MathUtils;


  // TClientCar

  destructor TClientCar.Destroy;
    begin
      if fCustomData <> nil
        then freemem( fCustomData, fDataSize );
      inherited;
    end;

  procedure TClientCar.Act( State : TStateId; Mode : TActMode );
    var
      HRAngleSpeed : single;
    begin
      if not fNoSynchAct
        then inherited
        else fNoSynchAct := false;
      case Mode of
        amdSynchronized :
          begin
            fHigResAngle := fLastAngle;
            fLastAngle   := Angle;
          end;
        amdHighRes :
          case State of
            carstRunning :
              begin
                fHigResSpeedX := (x - fHigResX)/HighResFreqMult;
                fHigResSpeedY := (y - fHigResY)/HighResFreqMult;
                if round(100*fHigResSpeedX) <> 0
                  then fHigResX := fHigResX + fHigResSpeedX
                  else fHigResX := x;
                if round(100*fHigResSpeedY) <> 0
                  then fHigResY := fHigResY + fHigResSpeedY
                  else fHigResY := y;
                HRAngleSpeed := (Angle - fHigResAngle)/HighResFreqMult;
                if round(100*HRAngleSpeed) <> 0
                  then fHigResAngle := fHigResAngle + HRAngleSpeed
                  else fHigResAngle := Angle;
                {
                fHigResAngle := fLastAngle;
                fLastAngle   := Angle;
                }
                {
                HRAngleSpeed := (Angle - fHigResAngle)/HighResFreqMult;
                fHigResAngle := fHigResAngle + HRAngleSpeed;
                }
              end;
          end;
      end;
    end;

  function TClientCar.HandleEvent( anEvent : TDistributedEvent ) : boolean;
    begin
      result := true;
      case anEvent.Id of
        evnAccelerationChange :
          Acceleration := single(anEvent.Data^);
        evnSpeedChange :
          Speed := single(anEvent.Data^);
        evnVisualStageChange :
          VisualStage := integer(anEvent.Data^);
        evnDirectionChange :
          begin
            {
            if (abs(fHigResX - x) > 1) or (abs(fHigResY - y) > 1)
              then
                case Direction of
                  cdirN : fHigResY := y + 1;
                  cdirE : fHigResX := x - 1;
                  cdirS : fHigResY := y - 1;
                  cdirW : fHigResX := x + 1;
                end;
            }
            Direction := TCarDirection(anEvent.Data^);
            case Direction of
              cdirN, cdirS :
                  x := round(x);
              else
                  y := round(y);
            end;
          end;
        evnNextDirectionChange :
          NextDirection := TCarDirection(anEvent.Data^);
        evnSyncInfo :
          begin
            x := TSyncInfo(anEvent.Data^).x;
            y := TSyncInfo(anEvent.Data^).y;
            Acceleration := TSyncInfo(anEvent.Data^).accel;
            Speed := TSyncInfo(anEvent.Data^).speed;
            Angle := TSyncInfo(anEvent.Data^).angle;
            {
            if abs(fHigResX - x) > 1.5
              then fHigResX := x;
            if abs(fHigResY - y) > 1.5
              then fHigResY := y;
            }
            LogThis( 'Synchronizing angle: ' + IntToStr(round(180*Angle/pi)) + ' and position (x: ' + FloatToStr(x) + ', y: ' + FloatToStr(y) + ')' );
            fNoSynchAct := true;
          end;
        else result := inherited HandleEvent( anEvent );
      end;
    end;

  procedure TClientCar.Load( Stream : TStream );
    begin
      inherited;
      fHigResX := x;
      fHigResY := y;
      fLastAngle := Angle;
      fHigResAngle := fLastAngle;
    end;

  function TClientCar.getX : single;
    begin
      result := fHigResX; //X //
    end;

  function TClientCar.getY : single;
    begin
      result := fHigResY; //Y //
    end;

  function TClientCar.getAngle : single;
    begin
      result := fHigResAngle;
      //result := Angle;
    end;

  function TClientCar.getDir : TVehicleDirection;
    begin
      case Direction of
        cdirN : result := vdirN;
        cdirS : result := vdirS;
        cdirE : result := vdirE;
        else    result := vdirW;
      end;
    end;

  function TClientCar.getNextDir : TVehicleDirection;
    begin
      case NextDirection of
        cdirN : result := vdirN;
        cdirS : result := vdirS;
        cdirE : result := vdirE;
        else    result := vdirW;
      end;
    end;

  function TClientCar.getGroupId : integer;
    begin
      result := 1;
    end;
    
  function TClientCar.getVisualClass : word;
    begin
      result := VisualClass;
    end;

  function TClientCar.getCustomData : pointer;
    begin
      result := fCustomData;
    end;

  procedure TClientCar.setCustomData( data : pointer; datasize : integer );
    begin
      fCustomData := data;
      fDataSize   := datasize;
    end;

  procedure TClientCar.Deleted;
    begin
      if assigned(fOnClientTrainModified)
        then fOnClientTrainModified( self, ccmDeleted );
    end;

  procedure TClientCar.MsgAnswerVehicle( var Msg : TMsgAnswerVehicle );
    begin
      Msg.Vehicle := self;
    end;

end.



