unit Seasons;

interface

  uses
    MetaInstances;

  type
    TSeason = (seasWinter, seasSpring, seasSummer, seasFall);

  type
    TWeatherEnvelope =
      class( TMetaInstance )
        private
          fValues    : array[TSeason] of single;
          fDeviation : single;
          fCurrVal   : single;
        private
          function  GetValue( Season : TSeason ) : single;
          procedure SetValue( Season : TSeason; value : single );
        public
          property Value[Season : TSeason] : single read GetValue write SetValue;
          property Deviation : single read fDeviation write fDeviation;
          property CurrValue : single read fCurrVal;
        public
          procedure Update( Season : TSeason; SeasonProgress : single );
      end;

  const
    tidClassFamily_WeatherEnvelopes = 'WeatherEnvelopes';

implementation

  function TWeatherEnvelope.GetValue( Season : TSeason ) : single;
    begin
      result := fValues[Season];
    end;

  procedure TWeatherEnvelope.SetValue( Season : TSeason; value : single );
    begin
      fValues[Season] := value;
    end;

  procedure TWeatherEnvelope.Update( Season : TSeason; SeasonProgress : single );

    function PrevSeason( Season : TSeason ) : TSeason;
      begin
        if Season > low(Season)
          then result := pred(Season)
          else result := high(Season);
      end;

    begin
      fCurrVal := (1 - SeasonProgress)*fValues[PrevSeason( Season )] + SeasonProgress*fValues[Season] + fDeviation*(50 - random(100))/50;
    end;
    

end.
