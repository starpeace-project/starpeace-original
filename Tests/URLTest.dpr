program URLTest;

{$APPTYPE CONSOLE}

uses
  URL2File in '..\Utils\Network\URL2File.pas';

{$R *.RES}

type
  TGauge =
    class
      public
        procedure NotifyProgress( Progress, ProgressMax : integer; Status : TDownloadStatus; StatusText: string; out Cancel : boolean );
    end;

  procedure TGauge.NotifyProgress( Progress, ProgressMax : integer; Status : TDownloadStatus; StatusText: string; out Cancel : boolean );
    begin
      write( #13 );
      case Status of
        dlstConnecting :
          write( 'Openning ', StatusText, '   ' );
        dlstDownloading :
          if ProgressMax > 0
            then write( 'Downloading ', StatusText, ' ', 100*Progress div ProgressMax, '%  ' );
      end;
    end;

var
  G : TGauge;
begin
  InitDownloader( nil );
  G := TGauge.Create;
  DownloadURLToFile( 'http://www.starpeace.net/five/client/bin.cab', 'c:\bin.cab', G.NotifyProgress );
  DoneDownloader;
  writeln( 'Done!' );
  readln;
end.
