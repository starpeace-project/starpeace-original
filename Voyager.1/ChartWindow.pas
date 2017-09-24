unit ChartWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FramedButton, ExtCtrls, PlotterGrid, InternationalizerComponent;

type
  TChartWin = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btClose: TFramedButton;
    ChartPanel: TPanel;
    Title: TPanel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fChart : TPlotter;
  public
    property Chart : TPlotter read fChart;
  end;

var
  ChartWin: TChartWin;

implementation

{$R *.DFM}

  procedure TChartWin.btCloseClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TChartWin.FormCreate(Sender: TObject);
    begin
      fChart := TPlotter.Create( self );
      fChart.Parent := ChartPanel;
      fChart.Top  := 0;
      fChart.Left := 0;
    end;

end.
