unit BlockView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Kernel;

type
  TBlockViewer = class(TForm)
    Memo: TMemo;
    BlockId: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    fFacility : TFacility;
  private
    procedure SetFacility( aFacility : TFacility );
  public
    property Facility : TFacility write SetFacility;
  public
    procedure RefreshContent;
  end;

var
  BlockViewer: TBlockViewer;

implementation

{$R *.DFM}

  procedure TBlockViewer.SetFacility( aFacility : TFacility );
    begin
      fFacility := aFacility;
      RefreshContent;
      if not Visible
        then Show;
    end;

  procedure TBlockViewer.RefreshContent;

    function RenderFluid( FluidData : PFluidData ) : string;
      begin
        if FluidData.Q < qIlimited
          then result := '('+ IntToStr(round(FluidData.Q)) + ', ' + IntToStr(FluidData.K) +')'
          else result := '( infinite, ' + IntToStr(FluidData.K) +')'
      end;

    var
      i : integer;
      fBlock : TBlock;
    begin
      Memo.Lines.Clear;
      if fFacility <> nil
        then
          try
            fBlock := fFacility.CurrBlock;
            BlockId.Caption := fBlock.MetaBlock.Id;
            Memo.Lines.BeginUpdate;
            Memo.Lines.Add( 'Inputs' );
            Memo.Lines.Add( '------' );
            for i := 0 to pred(fBlock.InputCount) do
              Memo.Lines.Add( '  ' + fBlock.Inputs[i].MetaInput.Name + ' = ' + RenderFluid(@fBlock.Inputs[i].LastValue));
            Memo.Lines.Add( '' );
            Memo.Lines.Add( 'Outputs' );
            Memo.Lines.Add( '-------' );
            for i := 0 to pred(fBlock.OutputCount) do
              Memo.Lines.Add( '  ' + fBlock.Outputs[i].MetaOutput.Name + ' = ' + RenderFluid(fBlock.Outputs[i].FluidData));
            Memo.Lines.EndUpdate;
          except
            fFacility := nil;
            Memo.Lines.EndUpdate;
          end
        else
          begin
            BlockId.Caption := 'Nothing';
            Memo.Lines.Add( 'No block is focused.' );
          end;
    end;

  procedure TBlockViewer.FormCreate(Sender: TObject);
    begin
      Left := Screen.Width - Width;
      Top  := 0;
    end;

end.

