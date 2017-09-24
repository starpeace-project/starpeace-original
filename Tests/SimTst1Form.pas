unit SimTst1Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Kernel;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Adaptive: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Graph: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    TabSheet2: TTabSheet;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Shape7: TShape;
    Shape8: TShape;
    Panel2: TPanel;
    Label24: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AdaptiveClick(Sender: TObject);
  private
    i1, i2, i3, i4 : integer;
    o1, o2, o3, o4 : integer;
    e1, e2, e3, e4 : integer;
  end;

var
  Form1: TForm1;

implementation

 {$R *.DFM}

  uses
    SimTst1Blks;

  const
    dx   = 10;
    VMax = 5000;

  procedure TForm1.Timer1Timer(Sender: TObject);
    var
      ni1, ni2, ni3, ni4 : integer;
      no1, no2, no3, no4 : integer;
      ne1, ne2, ne3, ne4 : integer;
    begin

      // A1
      A1.CollectInputs;
      Label1.Caption := FloatToStr( A1.Inputs[0].FluidData.Q );
      ni1 := Graph.Height - round(Graph.Height*A1.Inputs[0].FluidData.Q/VMax);
      A1.Evaluate;
      Label5.Caption := FloatToStr( A1.Outputs[0].FluidData.Q );
      no1 := Graph.Height - round(Graph.Height*A1.Outputs[0].FluidData.Q/VMax);
      {$IFDEF PULL}
      ne1 := Graph.Height - round(Graph.Height*A1.fY.Extra.Q/VMax);
      Label9.Caption := FloatToStr( A1.fY.Extra.Q );
      {$ENDIF}
      {$IFDEF PUSH}
      ne1 := Graph.Height - round(Graph.Height*A1.fY.Extra.Q/VMax);
      Label9.Caption := FloatToStr( A1.fY.Extra.Q );
      {$ENDIF}
      A1.SpreadOutputs;
      A1.ResetInputs;

      // A2
      A2.CollectInputs;
      Label2.Caption := FloatToStr( A2.Inputs[0].FluidData.Q );
      ni2 := Graph.Height - round(Graph.Height*A2.Inputs[0].FluidData.Q/VMax);
      A2.Evaluate;
      Label10.Caption := FloatToStr( A2.fY.Extra.Q );
      ne2 := Graph.Height - round(Graph.Height*A2.fY.Extra.Q/VMax);
      A2.SpreadOutputs;
      Label6.Caption := FloatToStr( A2.Outputs[0].FluidData.Q );
      no2 := Graph.Height - round(Graph.Height*A2.Outputs[0].FluidData.Q/VMax);
      A2.ResetInputs;

      // A3
      A3.CollectInputs;
      Label3.Caption := FloatToStr( A3.Inputs[0].FluidData.Q );
      ni3 := Graph.Height - round(Graph.Height*A3.Inputs[0].FluidData.Q/VMax);
      A3.Evaluate;
      Label11.Caption := FloatToStr( A3.fY.Extra.Q );
      ne3 := Graph.Height - round(Graph.Height*A3.fY.Extra.Q/VMax);
      A3.SpreadOutputs;
      Label7.Caption := FloatToStr( A3.Outputs[0].FluidData.Q );
      no3 := Graph.Height - round(Graph.Height*A3.Outputs[0].FluidData.Q/VMax);
      A3.ResetInputs;

      // A4
      A4.CollectInputs;
      Label4.Caption := FloatToStr( A4.Inputs[0].FluidData.Q );
      ni4 := Graph.Height - round(Graph.Height*A4.Inputs[0].FluidData.Q/VMax);
      A4.Evaluate;
      Label12.Caption := FloatToStr( A4.fY.Extra.Q );
      ne4 := Graph.Height - round(Graph.Height*A4.fY.Extra.Q/VMax);
      A4.SpreadOutputs;
      Label8.Caption := FloatToStr( A4.Outputs[0].FluidData.Q );
      no4 := Graph.Height - round(Graph.Height*A4.Outputs[0].FluidData.Q/VMax);
      A4.ResetInputs;

      Label13.Caption := IntToStr( PInputData(A1.Inputs[0].FluidData).S );
      Label14.Caption := IntToStr( PInputData(A2.Inputs[0].FluidData).S );
      Label15.Caption := IntToStr( PInputData(A3.Inputs[0].FluidData).S );
      Label16.Caption := IntToStr( PInputData(A4.Inputs[0].FluidData).S );


      with Graph.Picture.Bitmap do
        begin
          Canvas.CopyRect( Rect(0, 0, Width - dx, Height), Canvas, Rect(dx, 0, Width, Height));
          Canvas.Pen.Color := clBlack;
          Canvas.Rectangle( Width - dx, 0, Width, Height );

          Canvas.Pen.Width := 2;
          Canvas.Pen.Color := clGreen;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, i1 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ni1 );
          Canvas.Pen.Color := clLime;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, o1 );
          Canvas.LineTo( Width - Canvas.Pen.Width, no1 );
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clGreen;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, e1 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ne1 );

          Canvas.Pen.Width := 2;
          Canvas.Pen.Color := clTeal;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, i2 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ni2 );
          Canvas.Pen.Color := clAqua;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, o2 );
          Canvas.LineTo( Width - Canvas.Pen.Width, no2 );
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clTeal;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, e2 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ne2 );

          Canvas.Pen.Width := 2;
          Canvas.Pen.Color := clPurple;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, i3 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ni3 );
          Canvas.Pen.Color := clFuchsia;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, o3 );
          Canvas.LineTo( Width - Canvas.Pen.Width, no3 );
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clPurple;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, e3 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ne3 );

          Canvas.Pen.Width := 2;
          Canvas.Pen.Color := clOlive;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, i4 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ni4 );
          Canvas.Pen.Color := clYellow;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, o4 );
          Canvas.LineTo( Width - Canvas.Pen.Width, no4 );
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clOlive;
          Canvas.MoveTo( Width - Canvas.Pen.Width - dx, e4 );
          Canvas.LineTo( Width - Canvas.Pen.Width, ne4 );
        end;

      i1 := ni1;
      i2 := ni2;            
      i3 := ni3;
      i4 := ni4;
      o1 := no1;
      o2 := no2;
      o3 := no3;
      o4 := no4;
      e1 := ne1;
      e2 := ne2;
      e3 := ne3;
      e4 := ne4;
    end;

  procedure TForm1.Button1Click(Sender: TObject);
    begin
      Timer1.Enabled := not Timer1.Enabled;
    end;

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      Graph.Picture.Bitmap := TBitmap.Create;
      Graph.Picture.Bitmap.Width  := Graph.Width;
      Graph.Picture.Bitmap.Height := Graph.Height;
      with Graph.Picture.Bitmap.Canvas do
        begin
          Brush.Color := clBlack;
          Brush.Style := bsSolid;
          Pen.Color   := clBlack;
          Pen.Style   := psSolid;
          Rectangle( 0, 0, Graph.Picture.Bitmap.Width, Graph.Picture.Bitmap.Height );
        end;
      i1 := Graph.Picture.Bitmap.Height;
      i2 := Graph.Picture.Bitmap.Height;
      i3 := Graph.Picture.Bitmap.Height;
      i4 := Graph.Picture.Bitmap.Height;
      o1 := Graph.Picture.Bitmap.Height;
      o2 := Graph.Picture.Bitmap.Height;
      o3 := Graph.Picture.Bitmap.Height;
      o4 := Graph.Picture.Bitmap.Height;
      e1 := Graph.Picture.Bitmap.Height;
      e2 := Graph.Picture.Bitmap.Height;
      e3 := Graph.Picture.Bitmap.Height;
      e4 := Graph.Picture.Bitmap.Height;
      {$IFDEF PULL}
      Adaptive.Enabled := false;
      Caption := 'Four blocks connected with pulling gates';
      {$ELSE}
      Caption := 'Four blocks connected with pushing gates';
      {$ENDIF}
    end;
  
  procedure TForm1.Button2Click(Sender: TObject);
    begin
      Noise := 4000;// + random( 2000 );
      Timer1Timer(Sender);
    end;

  procedure TForm1.AdaptiveClick(Sender: TObject);
    begin
      Adapt := Adaptive.Checked;
    end;




end.


