unit SaveMap;

interface

  procedure A;
    var
      report    : TObjectReport;
      segreport : TSegmentReport;
      errorcode : TErrorCode;
      buildings : TStringList;
      roadsegs  : TStringList;
    begin
      // code to save the world buildings, and road segments
      buildings := TStringList.Create;
      try
        roadsegs := TStringList.Create;
        try
          with fMap do
            begin
              report := fClientView.ObjectsInArea(0, 0, 500, 500, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    report.ObjectCount := 0;
                    report.Objects := nil;
                  end
                else
                  for i := 0 to pred(report.ObjectCount) do
                    buildings.Add(
                                  IntToStr(report.Objects[i].y) + ' ' +
                                  IntToStr(report.Objects[i].x) + ' ' +
                                  IntToStr(report.Objects[i].VisualClass) + ' ' +
                                  IntToStr(report.Objects[i].CompanyId)
                                 );
              fClientView.DisposeObjectReport(report);
              segreport := fClientView.SegmentsInArea(cirRoads, 0, 0, 500, 500, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    segreport.SegmentCount := 0;
                    segreport.Segments := nil;
                  end
                else
                  for i := 0 to pred(segreport.SegmentCount) do
                    roadsegs.Add(
                                 IntToStr(segreport.Segments[i].x1) + ' ' +
                                 IntToStr(segreport.Segments[i].y1) + ' ' +
                                 IntToStr(segreport.Segments[i].x2) + ' ' +
                                 IntToStr(segreport.Segments[i].y2)
                                );
              fClientView.DisposeSegmentReport(segreport);
              report := fClientView.ObjectsInArea(500, 0, 499, 500, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    report.ObjectCount := 0;
                    report.Objects := nil;
                  end
                else
                  for i := 0 to pred(report.ObjectCount) do
                    buildings.Add(
                                  IntToStr(report.Objects[i].y) + ' ' +
                                  IntToStr(report.Objects[i].x) + ' ' +
                                  IntToStr(report.Objects[i].VisualClass) + ' ' +
                                  IntToStr(report.Objects[i].CompanyId)
                                 );
              fClientView.DisposeObjectReport(report);
              segreport := fClientView.SegmentsInArea(cirRoads, 500, 0, 499, 500, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    segreport.SegmentCount := 0;
                    segreport.Segments := nil;
                  end
                else
                  for i := 0 to pred(segreport.SegmentCount) do
                    roadsegs.Add(
                                 IntToStr(segreport.Segments[i].x1) + ' ' +
                                 IntToStr(segreport.Segments[i].y1) + ' ' +
                                 IntToStr(segreport.Segments[i].x2) + ' ' +
                                 IntToStr(segreport.Segments[i].y2)
                                );
              fClientView.DisposeSegmentReport(segreport);
              report := fClientView.ObjectsInArea(0, 500, 500, 499, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    report.ObjectCount := 0;
                    report.Objects := nil;
                  end
                else
                  for i := 0 to pred(report.ObjectCount) do
                    buildings.Add(
                                  IntToStr(report.Objects[i].y) + ' ' +
                                  IntToStr(report.Objects[i].x) + ' ' +
                                  IntToStr(report.Objects[i].VisualClass) + ' ' +
                                  IntToStr(report.Objects[i].CompanyId)
                                 );
              fClientView.DisposeObjectReport(report);
              segreport := fClientView.SegmentsInArea(cirRoads, 0, 500, 500, 499, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    segreport.SegmentCount := 0;
                    segreport.Segments := nil;
                  end
                else
                  for i := 0 to pred(segreport.SegmentCount) do
                    roadsegs.Add(
                                 IntToStr(segreport.Segments[i].x1) + ' ' +
                                 IntToStr(segreport.Segments[i].y1) + ' ' +
                                 IntToStr(segreport.Segments[i].x2) + ' ' +
                                 IntToStr(segreport.Segments[i].y2)
                                );
              fClientView.DisposeSegmentReport(segreport);
              report := fClientView.ObjectsInArea(500, 500, 499, 499, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    report.ObjectCount := 0;
                    report.Objects := nil;
                  end
                else
                  for i := 0 to pred(report.ObjectCount) do
                    buildings.Add(
                                  IntToStr(report.Objects[i].y) + ' ' +
                                  IntToStr(report.Objects[i].x) + ' ' +
                                  IntToStr(report.Objects[i].VisualClass) + ' ' +
                                  IntToStr(report.Objects[i].CompanyId)
                                 );
              fClientView.DisposeObjectReport(report);
              segreport := fClientView.SegmentsInArea(cirRoads, 500, 500, 499, 499, errorcode);
              if errorcode <> NOERROR
                then
                  begin
                    segreport.SegmentCount := 0;
                    segreport.Segments := nil;
                  end
                else
                  for i := 0 to pred(segreport.SegmentCount) do
                    roadsegs.Add(
                                 IntToStr(segreport.Segments[i].x1) + ' ' +
                                 IntToStr(segreport.Segments[i].y1) + ' ' +
                                 IntToStr(segreport.Segments[i].x2) + ' ' +
                                 IntToStr(segreport.Segments[i].y2)
                                );
              fClientView.DisposeSegmentReport(segreport);
            end;
          buildings.SaveToFile('C:\Tmp\buildings.dat');
          roadsegs.SaveToFile('C:\Tmp\roads.dat');
        finally
          roadsegs.Free;
        end;
      finally
        buildings.Free;
      end;
    end;

  procedure B;
    var
      bmp   : TBitmap;
      dummy : boolean;
    begin
      // code to create bitmap
      fMap.DownloadRegion(0, 0, pred(fMap.fRows), pred(fMap.fColumns), dummy);
      bmp := TBitmap.Create;
      try
        bmp.Height := fMap.fRows;
        bmp.Width := fMap.fColumns;
        bmp.Canvas.Brush.Color := clBlack;
        bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
        for i := 0 to pred(fMap.fRows) do
          for j := 0 to pred(fMap.fColumns) do
            if fMap.CheckForBuilding(i, j) or (fMap.GetRoad(i, j) <> roadNone)
              then bmp.Canvas.Pixels[j, i] := clWhite;
        bmp.SaveToFile('C:\Temp\' + fMap.fClientView.GetWorldName + '.bmp');
      finally
        bmp.Free;
      end;
    end;

implementation

end.

