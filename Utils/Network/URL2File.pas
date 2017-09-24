unit URL2File;

interface

  uses
    Windows, WinInet;

  type
    TDownloadErrorCode = integer;

  const
    DWNL_NOERROR               = 0;
    DWNL_ERROR_Unknown         = 1;
    DWNL_ERROR_InvalidURL      = 2;
    DWNL_ERROR_CannotWriteFile = 3;
    DWNL_ERROR_NoInternetCnx   = 4;
    DWNL_ERROR_Aborted         = 5;
    DWNL_ERROR_DownloadFailed  = 6;

  type
    TDownloadStatus = (dlstConnecting, dlstDownloading);

  type
    TDownloadNotifyProc = procedure( Progress, ProgressMax : integer; Status : TDownloadStatus; StatusText: string; out Cancel : boolean ) of object;

  procedure InitDownloader( InternetSession : HINTERNET );
  procedure DoneDownloader;

  function DownloadURLToFile( const URL, Dest : string; NotifyProc : TDownloadNotifyProc ) : TDownloadErrorCode;

implementation

  uses
    Classes, SysUtils;

  var
    TheInternetSession  : HINTERNET = nil;
    OwnsInternetSession : boolean   = false;

  procedure InitDownloader( InternetSession : HINTERNET );
    begin
      OwnsInternetSession := InternetSession = nil;
      if OwnsInternetSession
        then TheInternetSession := InternetOpen( 'FIVEVoyager', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0 );
    end;

  procedure DoneDownloader;
    begin
      if OwnsInternetSession
        then InternetCloseHandle( TheInternetSession );
    end;

  function DownloadURLToFile( const URL, Dest : string; NotifyProc : TDownloadNotifyProc  ) : TDownloadErrorCode;

    function ReadURL( URLHandle : HINTERNET ) : TDownloadErrorCode;
      const
        BufferSize = 1024;
      var
        Buffer    : array[0..BufferSize] of byte;
        querybuff : string;
        reqsize   : cardinal;
        size      : integer;
        totalRead : dword;
        bytesRead : dword;
        succeeded : boolean;
        aborted   : boolean;
        useless   : cardinal;
        Stream    : TStream;
      begin
        totalRead := 0;
        useless   := 0;
        succeeded := false;
        aborted   := false;
        reqsize   := 32;
        setlength( querybuff, reqsize );
        fillchar( querybuff[1], reqsize, 0 );
        if assigned(NotifyProc)
          then NotifyProc( 0, 0, dlstConnecting, URL, aborted );
        if HttpQueryInfo( URLHandle, HTTP_QUERY_STATUS_CODE, @querybuff[1], reqsize, useless ) and (StrToInt(querybuff) = 200)
          then
            begin
              reqsize := 32;
              fillchar( querybuff[1], reqsize, 0 );
              if HttpQueryInfo( URLHandle, HTTP_QUERY_CONTENT_LENGTH, @querybuff[1], reqsize, useless )
                then
                  begin
                    size := StrToInt( querybuff );
                    Stream := TFileStream.Create( Dest, fmCreate );
                    try
                      repeat
                        if assigned(NotifyProc)
                          then NotifyProc( totalRead, size, dlstDownloading, URL, aborted );
                        if not aborted
                          then
                            begin
                              succeeded := InternetReadFile( URLHandle, @Buffer, BufferSize, bytesRead );
                              inc( totalRead, bytesRead );
                              if bytesRead > 0
                                then Stream.WriteBuffer( Buffer, bytesRead );
                            end;
                      until aborted or not succeeded or (bytesRead = 0);
                    finally
                      Stream.Free;
                    end;
                    if aborted
                      then result := DWNL_ERROR_Aborted
                      else
                        if not succeeded
                          then result := DWNL_ERROR_DownloadFailed
                          else result := DWNL_NOERROR
                  end
                else result := DWNL_ERROR_DownloadFailed
            end
          else result := DWNL_ERROR_InvalidURL
      end;

    var
      URLHandle : HINTERNET;
    begin
      try
        if TheInternetSession <> nil
          then
            begin
              URLHandle := InternetOpenUrl( TheInternetSession, pchar(URL), nil, 0, 0, 0 ); 
              if URLHandle <> nil
                then
                  try
                    result := ReadURL( URLHandle )
                  finally
                    InternetCloseHandle( URLHandle );
                  end
                else result := DWNL_ERROR_InvalidURL;
            end
          else result := DWNL_ERROR_NoInternetCnx;
      except
        on E : EStreamError do
          result := DWNL_ERROR_CannotWriteFile;
        on E : Exception do
          result := DWNL_ERROR_Unknown;
      end;
    end;

end.

