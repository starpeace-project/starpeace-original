unit SeverUpdateForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Synchro, ExtCtrls;

type
  TForm1 = class(TForm)
    Source: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Dest: TEdit;
    Button1: TButton;
    OverallStatus: TLabel;
    TotalProgress: TProgressBar;
    TaskProgress: TProgressBar;
    Status: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  procedure TForm1.Button1Click(Sender: TObject);
    begin
      AsyncSynchronize( Source.Text, Dest.Text, Notify, 0 );
    end;

  procedure TForm1.Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    begin
      try
        OverallStatus.Caption := 'Overall Progress.   ';
        if SyncTask.MaxSize > 0
          then
            begin                                        
              OverallStatus.Caption :=
                OverallStatus.Caption +
                Format( '%.0n', [int(SyncTask.CurrSize/1024)] ) + ' KBytes (of ' + Format( '%.0n', [int(SyncTask.MaxSize/1024)] ) + ') ';
            end;
        {
        if (SyncTask.EstHours > 0) or (SyncTask.EstMins > 0)
          then
            begin
              OverallStatus.Caption := OverallStatus.Caption + 'Estimated Time: ';
              if SyncTask.EstHours > 0
                then
                  if SyncTask.EstHours > 1
                    then OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstHours) + ' hours. '
                    else OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstHours) + ' hour. ';
              if SyncTask.EstMins > 0
                then
                  if SyncTask.EstMins > 1
                    then OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstMins) + ' minutes. '
                    else OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstMins) + ' minute. ';
            end;
        }
        Status.Caption := TaskDesc;
        if EventId <> syncEvnFileDone
          then
            begin
              TotalProgress.Position := OverallProgress;
           end;
        {if Progress mod 10 = 0
          then }TaskProgress.Position := Progress;
        {
        if EventId = syncEvnDone                                                    
          then
            if (SyncTask = fNecessaryFiles)
              then
                begin
                  ErrorCode := fNecessaryFiles.ErrorCode;
                  if ErrorCode = SYNC_NOERROR
                    then
                      begin
                        fOptionalFiles  := AsyncSynchronize( fSource + 'client/cache/BuildingImages/', fPath + 'cache\BuildingImages\', Notify, 0 );
                        PlayNow.Visible := true;
                        OverallPages.PageIndex := 1;
                      end
                    else ReportError( ErrorCode, SyncTask.ErrorInfo );
                end
              else
                begin
                  ErrorCode := fOptionalFiles.ErrorCode;
                  if ErrorCode = SYNC_NOERROR
                    then FinishUpdate
                    else ReportError( ErrorCode, SyncTask.ErrorInfo );
                end;
        }
        Application.ProcessMessages;
        cancel := false;
      except
        cancel := true;
      end;
    end;


  procedure TForm1.FormCreate(Sender: TObject);
    begin
      InitSynchro( 1 );
    end;

end.
