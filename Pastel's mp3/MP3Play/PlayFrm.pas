unit PlayFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, MP3Player, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    fPlayer : TMP3Player;
    procedure PlayerState(Sender : TMP3Player; PrevState, NewState : TMP3PlayerState);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      fPlayer := TMP3Player.Create;
      fPlayer.OnStateChange := PlayerState;
      fPlayer.Loop := true;
      fPlayer.FileName := 'C:\Program Files\Winamp\demo.mp3';
    end;

  procedure TForm1.PlayerState(Sender : TMP3Player; PrevState, NewState : TMP3PlayerState);
    begin
      case NewState of
        mpsStopped : Caption := 'Stopped';
        mpsPlaying : Caption := 'Playing';
        mpsPaused  : Caption := 'Paused';
      end;
    end;

  procedure TForm1.FormDestroy(Sender: TObject);
    begin
      fPlayer.Free;
    end;

  procedure TForm1.Button2Click(Sender: TObject);
    begin
      fPlayer.Pause;
    end;

  procedure TForm1.Button3Click(Sender: TObject);
    begin
      fPlayer.Stop;
    end;

  procedure TForm1.Button1Click(Sender: TObject);
    begin
      if OpenDialog1.Execute
        then
          begin
            fPlayer.FileName := OpenDialog1.FileName;
            fPlayer.Play;
          end;

    end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fPlayer.Volume := 98;
end;


end.
