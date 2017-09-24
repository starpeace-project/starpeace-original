unit PyramidalModifier;

interface

  uses
    Windows, Surfaces;

  type
    TPyramidalModifier =
      class( TSurfaceModifier )
        protected
          function  GetValueAt( x, y : integer ) : TSurfaceValue; override;
          function  GetArea : TRect;                              override;
          procedure SetArea( anArea : TRect );                    override;
          procedure Update;                                       override;
        private
          fRadius   : single;
          fHeight   : single;
          fPositive : boolean;
      end;

  procedure RegisterBackup;

implementation

  uses
    Classes, Math, MathUtils, BackupInterfaces;

  function TPyramidalModifier.GetValueAt( x, y : integer ) : TSurfaceValue;
    var
      xrel, yrel : integer;
    begin
      if Strength > 0
        then
          begin
            xrel := x - Origin.x;
            yrel := y - Origin.y;
            if fPositive
              then result := realmax( 0, fHeight - sqrt(sqr(xrel) + sqr(yrel))/Strength )
              else result := realmin( 0, sqrt(sqr(xrel) + sqr(yrel))/Strength - fHeight)
          end
        else result := 0
    end;

  function TPyramidalModifier.GetArea : TRect;
    begin
      result := Classes.Bounds( Origin.x - round(fRadius), Origin.y - round(fRadius), round(2*fRadius), round(2*fRadius) );
    end;

  procedure TPyramidalModifier.SetArea( anArea : TRect );
    begin
      fRadius := abs(anArea.Right - anArea.Left)/2;
      if fRadius > 0
        then fHeight := 3*Value/sqr(fRadius)
        else fHeight := 0;
    end;

  procedure TPyramidalModifier.Update;
    begin                                         
      if (Value <> 0) and (Strength > 0)
        then
          begin                                                                                
            fPositive := Value >= 0;
            if not FixedArea
              then fRadius  := power( 3*abs(Value)*Strength/pi, 1/3 )
              else
                if (fRadius > 0) and (Value <> 0)
                  then Strength := (fRadius*fRadius*fRadius)*pi/(3*abs(Value))
                  else Strength := 0;
            if Strength > 0
              then fHeight := fRadius/Strength
              else fHeight := 0;
          end
        else
          begin
            fPositive := true;
            fRadius   := 0;
            fHeight   := 0;
          end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TPyramidalModifier );
    end;

end.


