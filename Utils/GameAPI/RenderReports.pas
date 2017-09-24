unit RenderReports;

interface

  procedure ResetImgCount;
  procedure ImgRendered(width, height : integer);
  function  GetImgCount : integer;
  procedure EnableReports;
  procedure DisableReports;

implementation

  uses
    Classes, SysUtils;

  var
    imgcount      : integer = 0;
    repsenabled   : boolean = false;
    minimgcount   : integer = high(integer);
    maximgcount   : integer = 0;
    totalimgcount : integer = 0;
    widthssum     : integer = 0;
    heightssum    : integer = 0;
    runs          : integer = 0;

  procedure ResetImgCount;
    begin
      inc(totalimgcount, imgcount);
      inc(runs);
      if imgcount > 0
        then
          if imgcount < minimgcount
            then minimgcount := imgcount
            else
              if imgcount > maximgcount
                then maximgcount := imgcount;
      imgcount := 0;
    end;

  procedure ImgRendered(width, height : integer);
    begin
      if repsenabled
        then
          begin
            inc(imgcount);
            inc(widthssum, width);
            inc(heightssum, height);
          end;
    end;

  function GetImgCount : integer;
    begin
      Result := imgcount;
    end;

  procedure EnableReports;
    begin
      repsenabled := true;
    end;

  procedure DisableReports;
    begin
      repsenabled := false;
    end;

  {$IFDEF RENDERREPORTS}
  var
    log : TStringList;
  {$ENDIF}

initialization
finalization
  {$IFDEF RENDERREPORTS}
  log := TStringList.Create;
  try
    log.Add('Average rendered sprite size is (' + IntToStr(round(widthssum/totalimgcount)) + 'X' + IntToStr(round(heightssum/totalimgcount)) + ')');
    log.Add('Average sprites rendered: ' + IntToStr(round(totalimgcount/runs)));
    log.Add('Minimum sprites rendered: ' + IntToStr(minimgcount));
    log.Add('Maximum sprites rendered: ' + IntToStr(maximgcount));
    log.SaveToFile('RenderReport.log');
  finally
    log.Free;
  end;
  {$ENDIF}
end.
