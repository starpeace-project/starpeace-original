unit StudyWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Buttons, StdCtrls, BlockStudies, DepWindow, Spin, CloneBlockForm;

type
  TStudyWin = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Run: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Blocks: TListView;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Outputs: TListView;
    Inputs: TListView;
    Splitter2: TSplitter;
    Label3: TLabel;
    Budget: TLabel;
    Label1: TLabel;
    Profit: TLabel;
    Fluids: TListView;
    Image1: TImage;
    ImageList1: TImageList;
    OpenDialog: TOpenDialog;
    ImageList2: TImageList;
    Label5: TLabel;
    Recovery: TLabel;
    GenerateBtn: TSpeedButton;
    OpenUnitDialog: TOpenDialog;
    Dependencies: TSpeedButton;
    ImpPrice: TSpinEdit;
    ROITime: TSpinEdit;
    Operation: TLabel;
    Label4: TLabel;
    Clone: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure SpeedButton3Click(Sender: TObject);
    procedure RunClick(Sender: TObject);
    procedure BlocksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure BlocksDblClick(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure DependenciesClick(Sender: TObject);
    procedure CloneClick(Sender: TObject);
  private
    fStudy     : TStudy;
    fStudyPath : string;
    fRendering : boolean;
  private
    procedure RenderItems;
    procedure SelectBlock( BlockIdx : integer );
  end;

var
  StudyWin: TStudyWin;

implementation

  uses
    EditWindow;

  {$R *.DFM}

  procedure TStudyWin.SpeedButton3Click(Sender: TObject);
    begin
      if OpenDialog.Execute
        then
          begin
            fStudyPath := ExtractFilePath( OpenDialog.FileName );
            fStudy := TStudy.Create( fStudyPath );
            fRendering := true;
            RenderItems;
            fRendering := false;
            if fStudy.Blocks.Count > 0
              then SelectBlock( 0 );
            Run.Enabled := true;
            GenerateBtn.Enabled := true;
            Dependencies.Enabled := true;
            Clone.Enabled := true;
          end;
    end;

  procedure TStudyWin.RunClick(Sender: TObject);
    var
      CurrBlock : integer;
    begin
      fStudy.Free;
      fStudy := TStudy.Create( fStudyPath );
      BlockStudies.TradeCenterPrice := ImpPrice.Value / 100;
      BlockStudies.ROITime          := ROITime.Value*24*365;
      fStudy.Evaluate;
      if fStudy.Blocks.Count > 0
        then
          begin
            if Blocks.Selected <> nil
              then CurrBlock := Blocks.Selected.Index
              else CurrBlock := 0;
            fRendering := true;
            RenderItems;
            fRendering := false;
            SelectBlock( CurrBlock );
          end;
    end;

  procedure TStudyWin.RenderItems;
    var
      i : integer;
    begin
      Fluids.Items.BeginUpdate;
      Fluids.Items.Clear;
      for i := 0 to pred(fStudy.Fluids.Count) do
        with Fluids.Items.Add do
          begin
            Caption := fStudy.Fluids[i].Name;
            ImageIndex := 0;
            SubItems.Add( '$' + CurrToStr(fStudy.Fluids[i].Price) );
            Data := fStudy.Fluids[i];
          end;
      Fluids.Items.EndUpdate;
      Blocks.Items.BeginUpdate;
      Blocks.Items.Clear;
      for i := 0 to pred(fStudy.Blocks.Count) do
        with Blocks.Items.Add do
          begin
            Caption := fStudy.Blocks[i].Name;
            SubItems.Add( '$' + IntToStr(round(24*fStudy.Blocks[i].Profit)) );
            SubItems.Add( IntToStr(trunc(fStudy.Blocks[i].RecoveryTime/(365*24))) );
            if fStudy.Blocks[i].RecoveryTime > 2*365*24
              then ImageIndex := 0
              else ImageIndex := 1;
            Data := fStudy.Blocks[i];
          end;
      Blocks.Items.EndUpdate;
    end;

  procedure TStudyWin.SelectBlock( BlockIdx : integer );
    var
      Block : TBlockStudy;
      i     : integer;
    begin
      if not fRendering
        then
          begin
            Block := TBlockStudy(Blocks.Items[BlockIdx].Data);
            Profit.Caption := '$' + FloatToStr(Block.Profit) + '  (' + '$' + FloatToStr(24*Block.Profit) + '/day)';
            Budget.Caption := '$' + FloatToStr(Block.Budget) + '  (' + '$' + FloatToStr(24*Block.Budget) + '/day)';
            Recovery.Caption := IntToStr(trunc(Block.RecoveryTime/(365*24))) + ' years and ' + IntToStr(trunc(365*frac(Block.RecoveryTime/(365*24)))) + 'days';
            Operation.Caption := FloatToStr(round(Block.LowOperation));
            Outputs.Items.BeginUpdate;
            Outputs.Items.Clear;
            for i := 0 to pred(Block.OutputCount) do
              with Outputs.Items.Add do
                begin
                  Caption := Block.Outputs[i].Name;
                  SubItems.Add( FloatToStr(Block.Outputs[i].Max) );
                  if Block.Outputs[i].Fluid <> nil
                    then SubItems.Add( Block.Outputs[i].Fluid.Measure )
                    else SubItems.Add( 'Unknown' );
                  SubItems.Add( '$' + CurrToStr(Block.Outputs[i].Price) );
                  SubItems.Add( '$' + CurrToStr(Block.Outputs[i].Max*Block.Outputs[i].Price) );
                end;
            Outputs.Items.EndUpdate;
            Inputs.Items.BeginUpdate;
            Inputs.Items.Clear;
            for i := 0 to pred(Block.BasicInputCount) do
              with Inputs.Items.Add do
                begin
                  Caption := Block.BasicInputs[i].Name;
                  SubItems.Add( FloatToStr(Block.BasicInputs[i].Max) );
                  if Block.BasicInputs[i].Fluid <> nil
                    then SubItems.Add( Block.BasicInputs[i].Fluid.Measure )
                    else SubItems.Add( 'Unknown' );
                  SubItems.Add( '$' + CurrToStr(Block.BasicInputs[i].Price) );
                  SubItems.Add( '$' + CurrToStr(Block.BasicInputs[i].Max*Block.BasicInputs[i].Price) );
                  ImageIndex := 1;
                end;
            for i := 0 to pred(Block.AditionalInputCount) do
              with Inputs.Items.Add do
                begin
                  Caption := Block.AditionalInputs[i].Name;
                  SubItems.Add( FloatToStr(Block.AditionalInputs[i].Max) );
                  if Block.AditionalInputs[i].Fluid <> nil
                    then SubItems.Add( Block.AditionalInputs[i].Fluid.Measure )
                    else SubItems.Add( 'Unknown' );
                  SubItems.Add( '$' + CurrToStr(Block.AditionalInputs[i].Price) );
                  SubItems.Add( '$' + CurrToStr(Block.AditionalInputs[i].Max*Block.AditionalInputs[i].Price) );
                  ImageIndex := 2;
                end;
            Inputs.Items.EndUpdate;
            Blocks.Items[BlockIdx].MakeVisible(true);
          end;
    end;

  procedure TStudyWin.BlocksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      SelectBlock( Item.Index );
    end;

  procedure TStudyWin.BlocksDblClick(Sender: TObject);
    var
      index : integer;
    begin
      if Blocks.Selected <> nil
        then
          begin
            index := Blocks.Selected.Index;
            EditWin.FileName := TBlockStudy(Blocks.Selected.Data).FileName;
            if EditWin.ShowModal = mrOk
              then RunClick( self );
            Blocks.Items[index].Selected := true;
            Blocks.Items[index].MakeVisible(false);
          end;
    end;

  procedure TStudyWin.GenerateBtnClick(Sender: TObject);
    var
      Block : TBlockStudy;
    begin
      if OpenUnitDialog.Execute and (Blocks.Selected <> nil)
        then
          begin
            Block := TBlockStudy(Blocks.Selected.Data);
            fStudy.GenerateConsts(OpenUnitDialog.FileName, Block.Cluster);
          end;
    end;

  procedure TStudyWin.DependenciesClick(Sender: TObject);
    var
      Block  : TBlockStudy;
      DepWin : TDependencyWindow;
      List   : TStringList;
    begin
      if Blocks.Selected <> nil
        then
          begin
            Block := TBlockStudy(Blocks.Selected.Data);
            DepWin := TDependencyWindow.Create(self);
            DepWin.Caption := Block.Name;
            List := fStudy.GetDependencies(Block);
            DepWin.Lines := List;
            DepWin.ShowModal;
            List.Free;
          end;
    end;

  procedure TStudyWin.CloneClick(Sender: TObject);
    var
      i : integer;
      NewBlock : TBlockStudy;
    begin
      if (CloneForm.ShowModal = mrOk) and SaveDialog.Execute
        then
          for i := 0 to pred(fStudy.Blocks.Count) do
            begin
              NewBlock := fStudy.Blocks[i].Clone(
                StrToFloat(CloneForm.InputRatio.Text),
                StrToFloat(CloneForm.OutputRatio.Text),
                StrToFloat(CloneForm.GrowRatio.Text),
                CloneForm.Small.Checked);
              NewBlock.Cloned := false;
              NewBlock.Evaluate(CloneForm.ROI.Value*24*365);
              NewBlock.Cloned := true;
              NewBlock.Save(CloneForm.Cluster.Text, ExtractFilePath(SaveDialog.FileName), CloneForm.Small.Checked);
              NewBlock.Free;
            end;
    end;


end.



