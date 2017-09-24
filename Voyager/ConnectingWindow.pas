unit ConnectingWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColoredGauge, BlockTicker, ExtCtrls, InternationalizerComponent;

type                         
  TConnectingWin = class(TForm)
    VUGauge: TColorGauge;
    BlockTicker: TBlockTicker;
    Timer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);                
    procedure FormHide(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private                                           
    fTimeout    : integer;
    fMaxTimeout : integer;
  private
    procedure SetMaxTimeout( value : integer );
  public
    property Timeout    : integer read fTimeout    write fTimeout;
    property MaxTimeout : integer read fMaxTimeout write SetMaxTimeout;
  public
    procedure Display( msg : string );
  end;
                                                          
var
  ConnectingWin: TConnectingWin;

implementation

{$R *.DFM}

  uses
    MathUtils;

  procedure TConnectingWin.TimerTimer(Sender: TObject);
    begin
      BlockTicker.Tick;
      if fMaxTimeout > 0
        then
          begin
            inc( fTimeout, Timer.Interval );
            VUGauge.Position := min( 100, 100*fTimeout div fMaxTimeout );
          end;
    end;

  procedure TConnectingWin.FormShow(Sender: TObject);
    begin
      Timer.Enabled := true;
    end;

  procedure TConnectingWin.SetMaxTimeout( value : integer );
    var
      str : string;
    begin
      fMaxTimeout := value;
      fTimeout    := 0;
      str := BlockTicker.Caption;
      BlockTicker.Caption := '';
      BlockTicker.Caption := str;
      VUGauge.Position := 0;
    end;

  procedure TConnectingWin.Display( msg : string );
    begin
      BlockTicker.Caption := msg;
      Show;
    end;

  procedure TConnectingWin.FormHide(Sender: TObject);
    begin
      Timer.Enabled := false;
    end;

procedure TConnectingWin.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    Action := caFree;
  end;

end.
