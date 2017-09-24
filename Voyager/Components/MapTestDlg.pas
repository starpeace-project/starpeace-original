unit MapTestDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, VoyagerServerInterfaces, StdCtrls, InternationalizerComponent;

type
  TSurfaceViewer = class(TForm)
    Panel1: TPanel;
    Button: TButton;
    Memo: TMemo;
    xPos: TEdit;
    yPos: TEdit;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure ButtonClick(Sender: TObject);
  private
    fClientView : IClientView;
  public
    property ClientView : IClientView write fClientView;
  end;

var
  SurfaceViewer: TSurfaceViewer;

implementation

  {$R *.DFM}

  uses
    Matrix;

  procedure TSurfaceViewer.ButtonClick(Sender: TObject);

    function Padd( n : integer ) : string;
      begin
        result := IntToStr( n );
        while length(result) < 3 do
          result := ' ' + result;
      end;

    var
      i, j      : integer;
      matrix    : IMatrix;
      ErrorCode : TErrorCode;
      line      : string;
    begin
      matrix := fClientView.GetSurface( 'Beauty', StrToInt(xPos.Text), StrToInt(yPos.Text), 50, 100, ErrorCode );
      if ErrorCode = NOERROR
        then
          begin
            Memo.Lines.Clear;
            for i := 0 to pred(100) do
              begin
                line := '';
                for j := 0 to pred(50) do
                  line := line + Padd(round(matrix.getElement( i, j )));
                Memo.Lines.Add( line );
              end;
          end;
    end;


end.
