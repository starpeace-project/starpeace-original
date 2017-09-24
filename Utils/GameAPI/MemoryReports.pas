unit MemoryReports;

interface

  type
    TMemoryReportRecord =
      record
        cause     : string;
        allocsize : integer;
      end;

  procedure ReportMemoryAllocation(const cause : string; allocsize : integer);
  procedure GetMemoryReport(out report : array of TMemoryReportRecord; var reccount : integer);
  function  GetTotalAllocatedMemory : integer;

implementation

  type
    PMemoryReportRecords = ^TMemoryReportRecords;
    TMemoryReportRecords = array [0..0] of TMemoryReportRecord;

  var
    fMemoryReportRecords  : PMemoryReportRecords = nil;
    fMemoryReportRecCount : integer = 0;
    fMemoryReportRecAlloc : integer = 0;

  procedure ReportMemoryAllocation(const cause : string; allocsize : integer);
    const
      cAllocDelta = 5;
    var
      i     : integer;
      found : boolean;
    begin
      i := 0;
      found := false;
      while (i < fMemoryReportRecCount) and not found do
        begin
          found := cause = fMemoryReportRecords[i].cause;
          if not found
            then inc(i);
        end;
      if found
        then inc(fMemoryReportRecords[i].allocsize, allocsize)
        else
          begin
            if fMemoryReportRecCount = fMemoryReportRecAlloc
              then
                begin
                  inc(fMemoryReportRecAlloc, cAllocDelta);
                  reallocmem(fMemoryReportRecords, fMemoryReportRecAlloc*sizeof(fMemoryReportRecords[0]));
                  fillchar(fMemoryReportRecords[fMemoryReportRecCount], (fMemoryReportRecAlloc - fMemoryReportRecCount)*sizeof(fMemoryReportRecords[0]), 0);
                end;
            fMemoryReportRecords[fMemoryReportRecCount].cause := cause;
            fMemoryReportRecords[fMemoryReportRecCount].allocsize := allocsize;
            inc(fMemoryReportRecCount);
          end;
    end;

  procedure GetMemoryReport(out report : array of TMemoryReportRecord; var reccount : integer);
    var
      i : integer;
    begin
      if reccount > fMemoryReportRecCount
        then reccount := fMemoryReportRecCount;
      for i := 0 to pred(reccount) do
        report[i] := fMemoryReportRecords[i];
    end;

  function GetTotalAllocatedMemory : integer;
    var
      i          : integer;
      totalalloc : integer;
    begin
      totalalloc := 0;
      for i := 0 to pred(fMemoryReportRecCount) do
        inc(totalalloc, fMemoryReportRecords[i].allocsize);
      Result := totalalloc;
    end;

end.
