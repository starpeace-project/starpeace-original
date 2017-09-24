unit CircTstWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Circuits,
  ExtCtrls, StdCtrls, Collection;

type
  TTstWin = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    CircCount: TLabel;
    x1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    y1: TEdit;
    Label4: TLabel;
    x2: TEdit;
    Label5: TLabel;
    y2: TEdit;
    Go: TButton;
    ListBox: TListBox;
    Nodes: TLabel;
    Circs: TButton;
    OwnerId: TEdit;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GoClick(Sender: TObject);
    procedure CircsClick(Sender: TObject);
  private
    fMap : TCircuitMap;
    fDrawingSeg : TPoint;
    fInDrawingSeg : boolean;
  private
    procedure DrawMap;
  end;

var
  TstWin: TTstWin;

implementation

  const
    Scale = 10;

  {$R *.DFM}

  procedure TTstWin.FormCreate(Sender: TObject);
    begin
      fMap := TCircuitMap.Create( 1, TNode, TSegment, TCircuit );
      Image.Picture.Bitmap := TBitmap.Create;
      Image.Picture.Bitmap.Width := Image.Width;
      Image.Picture.Bitmap.Height := Image.Height;
    end;

  procedure TTstWin.FormShow(Sender: TObject);
    begin
      DrawMap;
    end;

  procedure TTstWin.DrawMap;

    procedure DrawNode( Node : TNode );
      const
        DotSize = 2;
      begin
        Image.Canvas.Pen.Color := clBlack;
        if Node.Segments[segNorth] <> nil
          then
            begin
              Image.Canvas.MoveTo( Scale*Node.x, Scale*Node.y );
              Image.Canvas.LineTo( Scale*Node.x, Scale*Node.y - Scale div 2 );
            end;
        if Node.Segments[segEast] <> nil
          then
            begin
              Image.Canvas.MoveTo( Scale*Node.x, Scale*Node.y );
              Image.Canvas.LineTo( Scale*Node.x + Scale div 2, Scale*Node.y );
            end;
        if Node.Segments[segSouth] <> nil
          then
            begin
              Image.Canvas.MoveTo( Scale*Node.x, Scale*Node.y );
              Image.Canvas.LineTo( Scale*Node.x, Scale*Node.y + Scale div 2 );
            end;
        if Node.Segments[segWest] <> nil
          then
            begin
              Image.Canvas.MoveTo( Scale*Node.x, Scale*Node.y );
              Image.Canvas.LineTo( Scale*Node.x - Scale div 2, Scale*Node.y );
            end;
        Image.Canvas.Ellipse( Scale*Node.x - DotSize, Scale*Node.y - DotSize, Scale*Node.x + DotSize, Scale*Node.y + DotSize + 1 );
      end;

    var
      i, j : integer;
    begin
      Image.Canvas.Pen.Color := clWhite;
      Image.Canvas.Rectangle( 0, 0, Width, Height );
      Image.Canvas.Pen.Color := $00EEEEEE;
      for i := 0 to Width div Scale do
        begin
          Image.Canvas.MoveTo( Scale*i - Scale div 2, 0 );
          Image.Canvas.LineTo( Scale*i - Scale div 2, Height );
        end;
      for i := 0 to Height div Scale do
        begin
          Image.Canvas.MoveTo( 0, Scale*i - Scale div 2 );
          Image.Canvas.LineTo( Width, Scale*i - Scale div 2 );
        end;
      for i := 0 to pred(fMap.Circuits.Count) do
        with TCircuit(fMap.Circuits[i]) do
          begin
            for j := 0 to pred(Segments.Count) do
              with TSegment(Segments[j]) do
                if (NodeA <> nil) and (NodeB <> nil)
                  then
                    begin
                      Image.Canvas.Pen.Color := clSilver;
                      Image.Canvas.MoveTo( Scale*NodeA.x, Scale*NodeA.y );
                      Image.Canvas.LineTo( Scale*NodeB.x, Scale*NodeB.y );
                      DrawNode( NodeA );
                      DrawNode( NodeB );
                    end;
          end;
      CircCount.Caption := IntToStr(fMap.Circuits.Count);
      if fMap.Circuits.Count > 0
        then
          begin
            Nodes.Caption := IntToStr(TCircuit(fMap.Circuits[0]).Nodes.Count) + ', ' + IntToStr(TCircuit(fMap.Circuits[0]).Segments.Count);
          end;
    end;

  procedure TTstWin.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      ErrorCode : TCircuitErrorCode;
    begin
      if Button = mbLeft
        then
          if not fInDrawingSeg
            then
              begin
                fDrawingSeg.x := x;
                fDrawingSeg.y := y;
                fInDrawingSeg := true;
                Image.Cursor  := crCross;
              end
            else
              begin
                if abs(fDrawingSeg.x - x) > abs(fDrawingSeg.y - y)
                  then y := fDrawingSeg.y
                  else x := fDrawingSeg.x;
                fMap.CreateSegment( round(fDrawingSeg.x/Scale), round(fDrawingSeg.y/Scale), round(x/Scale), round(y/Scale), StrToInt(OwnerId.Text), ErrorCode );
                if ErrorCode = CIRCUIT_NOERROR
                  then DrawMap;
                fInDrawingSeg := false;
                Image.Cursor  := crArrow;
              end
        else
          begin
            fMap.BreakSegmentInPoint( round(x/Scale), round(y/Scale), StrToInt(OwnerId.Text), ErrorCode );
            if ErrorCode = CIRCUIT_NOERROR
              then DrawMap;
          end;
    end;

  procedure TTstWin.GoClick(Sender: TObject);
    begin
      try
        ListBox.Items.Text := fMap.SegsInArea( StrToInt(x1.Text), StrToInt(y1.Text), StrToInt(x2.Text), StrToInt(y2.Text) );
      except
        ListBox.Items.Text := 'ERROR';
      end;
    end;

  procedure TTstWin.CircsClick(Sender: TObject);
    var
      Circs     : TCollection;
      Area      : TRect;
      ErrorCode : integer;
      i         : integer;
    begin
      Circs := TCollection.Create( 0, rkUse );
      try
        Area := Rect( StrToInt(x1.Text), StrToInt(y1.Text), StrToInt(x2.Text), StrToInt(y2.Text) );
        fMap.NearestCircuitsToArea( Area, 0, Circs, ErrorCode );
        ListBox.Items.Clear;
        for i := 0 to pred(Circs.Count) do
          ListBox.Items.Add( IntToStr(integer(Circs[i])) );
      finally
        Circs.Free;
      end;
    end;

end.


