unit DSoundUtils;

interface

  uses
    Windows, mmSystem, SysUtils,
    DirectSound;

  type
    EDSound = class(Exception);

  procedure FillPCMFormat(const aSampligRate, aBits, aChannels : integer; out aFormat : TWaveFormatEx);
  function CreateHelperDSound : IDirectSound;
  procedure DSoundCheck(aResult : hResult);

  function PercentToBufferVolume(aPercent : integer) : integer;
  function BufferVolumeToPercent(aVolume : integer) : integer;

implementation

  uses
    Forms,
    Math,
    D3Math;

  procedure FillPCMFormat(const aSampligRate, aBits, aChannels : integer; out aFormat : TWaveFormatEx);
    begin
      aFormat.wFormatTag      := WAVE_FORMAT_PCM;
      aFormat.nChannels       := aChannels;
      aFormat.nSamplesPerSec  := aSampligRate;
      aFormat.wBitsPerSample  := aBits;
      aFormat.nBlockAlign     := aFormat.wBitsPerSample div 8 * aFormat.nChannels;
      aFormat.nAvgBytesPerSec := aFormat.nSamplesPerSec * aFormat.nBlockAlign;
     end;

  function CreateHelperDSound : IDirectSound;
    var
      Desc    : TDSBUFFERDESC;
      Primary : IDirectSoundBuffer;
      Format  : TWaveFormatEx;
    begin
      DSoundCheck(DirectSoundCreate(nil, Result, nil));
      try
        DSoundCheck(Result.SetCooperativeLevel(Application.Handle, DSSCL_PRIORITY));
        fillchar(Desc, sizeof(Desc), 0);
        Desc.dwSize  := sizeof(Desc);
        Desc.dwFlags := DSBCAPS_PRIMARYBUFFER;
        DSoundCheck(Result.CreateSoundBuffer(Desc, Primary, nil));
        try
          FillPCMFormat(22050, 16, 2, Format);
          Primary.SetFormat(Format);
        finally
          Primary := nil;
        end;
      except
        Result := nil;
        raise;
      end;
    end;


  procedure DSoundCheck(aResult : hResult);
    begin
      if Failed(aResult)
        then raise EDSound.Create(DSErrorString(aResult));
    end;

  function PercentToBufferVolume(aPercent : integer) : integer;
    begin
      if aPercent > 0
        then Result := -round(100 * (40 - 20 * log10(aPercent)))
        else Result := -10000;
    end;

  function BufferVolumeToPercent(aVolume : integer) : integer;
    begin
      Result := round(power(10, 2 + aVolume / 2000));
      Result := min(max(Result, 0), 100);
    end;
    
end.
