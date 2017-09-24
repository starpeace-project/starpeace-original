unit CrimeTrainingDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MarqueeCtrl, FramedButton,
  VoyagerInterfaces, VoyagerServerInterfaces, PDTabControl, ComCtrls,
  InternationalizerComponent;

type
  TCrimeTrainingDlg = class(TForm)
    CloseBtn: TFramedButton;
    HintText: TMarquee;
    Panel1: TPanel;
    Tabs: TPDTabControl;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Notebook: TNotebook;
    trAvailable: TTreeView;
    trCompleted: TTreeView;
    Shape4: TShape;
    lbAvlProps: TLabel;
    btTrain: TFramedButton;
    lbCompletedProps: TLabel;
    Shape5: TShape;
    FramedButton1: TFramedButton;
    FramedButton2: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabsTabChange(Sender: TObject);
    procedure trTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FramedButton1Click(Sender: TObject);
    procedure btTrainClick(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
    fCriminal         : TStringList;
    fLeader           : TStringList;
    fTeam             : TStringList;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write fIllSystem;
    property Criminal         : TStringList       write fCriminal;
    property Leader           : TStringList       write fLeader;
    property Team             : TStringList       write fTeam;
  private
    procedure threadedGetTrainings( const parms : array of const );
    procedure syncGetTrainings( const parms : array of const );
  end;

var
  CrimeTrainingDlg: TCrimeTrainingDlg;

implementation

  {$R *.DFM}

  uses
    Threads, ClassStorage, MetaCrime, Protocol, MathUtils;

  procedure SupressToolTip(Tree : TTreeView);
    const
      TVS_NOTOOLTIPS = $80;
      TVM_SETBKCOLOR = $1100 + 29;
    var
      Msg : TMessage;
    begin
      with Tree do
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);
      Msg.Msg := TVM_SETBKCOLOR;
      Msg.WParam := 0;
      Msg.LParam := clWhite;
      Tree.Dispatch( Msg );
    end;

  procedure TCrimeTrainingDlg.btCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TCrimeTrainingDlg.threadedGetTrainings( const parms : array of const );
    var
      Info : TStringList;
    begin
      Info := TStringList.Create;
      try
        Info.Text := fIllSystem.RDOGetCriminalTrainingInfo( fCriminal.Values['Name'] );
        Join( syncGetTrainings, [Info] );
      except
        Info.Free;
      end;
    end;

  procedure TCrimeTrainingDlg.syncGetTrainings( const parms : array of const );

    function FindNode( Tree : TTreeView; NodeCaption : string ) : TTreeNode;

      function RecurseFindNode( Node : TTreeNode ) : TTreeNode;
        var
          i : integer;
        begin
          if Node.Text = NodeCaption
            then result := Node
            else
              if Node.Count > 0
                then
                  begin
                    i := 0;
                    repeat
                      result := RecurseFindNode( Node[i] );
                      inc( i );
                    until (i = Node.Count) or (result <> nil)
                  end
                else result := nil;
        end;

      var
        i : integer;
      begin
        if Tree.Items.Count > 0
          then
            begin
              i := 0;
              repeat
                result := RecurseFindNode( Tree.Items.Item[i] );
                inc( i );
              until (i = Tree.Items.Count) or (result <> nil)
            end
          else result := nil;
      end;

    var
      Info    : TStringList absolute parms[0].vPointer;
      count   : integer;
      i       : integer;
      AM      : TAttributeModifier;
      tree    : TTreeView;
      Node    : TTreeNode;
    begin
      try
        trAvailable.Items.BeginUpdate;
        try
          trCompleted.Items.BeginUpdate;
          try
            trAvailable.Items.Clear;
            trCompleted.Items.Clear;
            count := TheClassStorage.ClassCount['AttributeModifier'];
            for i := 0 to pred(count) do
              begin
                AM := TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', i]);
                if AM <> nil
                  then
                    begin
                      if Info.IndexOf( AM.Name ) = -1
                        then tree := trAvailable
                        else tree := trCompleted;
                      Node := FindNode( tree, AM.Attribute.Name );
                      if Node = nil
                        then Node := tree.Items.AddChildObject( nil, AM.Attribute.Name, pointer(-1) );
                      tree.Items.AddChildObject( Node, AM.Name, pointer(i) );
                    end;
              end;
          finally
            trCompleted.Items.EndUpdate;
          end;
        finally
          trAvailable.Items.EndUpdate;
        end;
      finally
        Info.Free;
      end;
    end;

  procedure TCrimeTrainingDlg.FormShow(Sender: TObject);
    begin
      SupressToolTip( trAvailable );
      SupressToolTip( trCompleted );
      Fork( threadedGetTrainings, priNormal, [0] );
    end;

  procedure TCrimeTrainingDlg.TabsTabChange(Sender: TObject);
    begin
      Notebook.PageIndex := Tabs.CurrentTab;
    end;

  procedure TCrimeTrainingDlg.trTreeChange(Sender: TObject; Node: TTreeNode);
    var
      AM    : TAttributeModifier;
      tree  : TTreeView;
      props : TLabel;                             
    begin
      tree := TTreeView(Sender);
      if tree = trAvailable
        then props := lbAvlProps
        else props := lbCompletedProps;
      if Node <> nil
        then
          if integer(Node.Data) <> -1
            then
              begin
                AM := TAttributeModifier(TheClassStorage.ClassByIdx['AttributeModifier', integer(Node.Data)]);
                if AM <> nil
                  then
                    begin
                      props.Caption := AM.Desc;
                      {props.Caption :=
                        props.Caption + LineBreak + LineBreak +
                        'Skill Increase: +' + IntToStr(AM.Value) + ' points' + LineBreak +
                        'Cost: ' + FormatMoney(AM.Cost) + LineBreak +
                        'Duration: ' + IntToStr(AM.Time) + ' hours' + LineBreak +
                        'Difficulty: ' + IntToStr(AM.Difficulty) + '%';}
                    end
                  else props.Caption := '';
              end
            else props.Caption := 'Click over the [+] sign to expand this branch of the tree. Click over the [-] sign to collapse it. '
        else props.Caption := '';
      btTrain.Enabled := (tree = trAvailable) and (Node <> nil) and (integer(Node.Data) <> -1);
    end;

  procedure TCrimeTrainingDlg.FramedButton1Click(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TCrimeTrainingDlg.btTrainClick(Sender: TObject);
    var
      ErrorCode : TErrorCode;
    begin
      if (trAvailable.Selected <> nil) and (integer(trAvailable.Selected.Data) <> -1)
        then
          try
            ErrorCode := fIllSystem.RDOCriminalTraining( fCriminal.Values['Name'], trAvailable.Selected.Text );
            ModalResult := mrOk;
          except
          end;
    end;

end.



