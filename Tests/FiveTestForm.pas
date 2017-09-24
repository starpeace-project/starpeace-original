unit WorldTest1Form;

interface

uses
  Kernel, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Population, ToolWin, ExtCtrls, World, Menus, Buttons, StdCtrls;

type
  TWorldTestForm = class(TForm)
    MapImage: TImage;
    PopupMenu: TPopupMenu;
    Panel3: TPanel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    ShowTowns: TSpeedButton;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Timer: TTimer;
    Image1: TImage;
    Panel4: TPanel;
    Image2: TImage;
    Shape1: TShape;
    Money: TLabel;
    Date: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure MapImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ShowTownsClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MapImageDblClick(Sender: TObject);
  private
    fWorld : TInhabitedWorld;
    Company : TCompany;
    fx, fy : integer;
    fDelta : TPoint;
    fDrag  : TPoint;
    fDown  : boolean;
    Grnd : TBitmap;
    GridSize : integer;
    ItemDelete : TMenuItem;
  private
    procedure PaintMap( x, y, dx, dy : integer );
    procedure PaintFacilities;
    procedure PaintTowns;
  private
    procedure OnItemClick( Sender : TObject );
    procedure DeleteFacility( Sender : TObject );
  end;

var
  WorldTestForm: TWorldTestForm;

implementation

  {$R *.DFM}

  uses
    ClassStorage, Simtst1blks, PopulatedBlock, WorkCenterBlock, Trade,
    PublicFacility, BlockView, Construction;

  const
    TownCount = 25;

  var
    TownColors : array[0..TownCount - 1] of TColor;

  function FormatMoney( money : string ) : string;
    var
      i : integer; 
    begin
      result := money;
      for i := length(result) downto 1 do
        if (i mod 3 = 0) and (i < length(result))
          then insert( ',', result, length(result) - i + 1 )
    end;


  // TWorldTestForm

  procedure TWorldTestForm.FormCreate(Sender: TObject);
    var
      Town    : TTown;
      i, j, k : integer;
      x, y    : integer;
      Item    : TMenuItem;
    begin
      randomize;
      InitTheClassStorage;
      TMetaFluid.Create( 'Test', 'Test', 'Sample Fluid', 0, 100 ).Register( 'Fluids' );
      Population.RegisterMetaInstances;
      Construction.RegisterMetaInstances;

      with TMetaBlockUnderConstruction.Create( '1x1 construction', 40, 10, 200, 0, 0, TBlockUnderConstruction ) do
        begin
          xSize := 1;
          ySize := 1;
          Register( 'Blocks' );
        end;
      with TMetaBlockUnderConstruction.Create( '2x2 construction', 41, 20, 400, 0, 0, TBlockUnderConstruction ) do
        begin
          xSize := 2;
          ySize := 2;
          Register( 'Blocks' );
        end;
      with TMetaBlockUnderConstruction.Create( '3x3 construction', 42, 40, 900, 0, 0, TBlockUnderConstruction ) do
        begin
          xSize := 3;
          ySize := 3;
          Register( 'Blocks' );
        end;
      with TMetaBlockUnderConstruction.Create( '4x4 construction', 43, 130, 1600, 100, 1000, TBlockUnderConstruction ) do
        begin
          xSize := 4;
          ySize := 4;
          Register( 'Blocks' );
        end;

      with TMetaPopulatedBlock.Create( 'HighClassBuilding', 101, pkHigh, 50, TPopulatedBlock ) do
        begin
          xSize := 1;
          ySize := 1;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( '0A', 'Small Building (high class)', TFacility ) do
        begin
          XSize := 1;
          YSize := 1;
          Level := 120;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '1x1 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'HighClassBuilding'] as TMetaBlock ));
          Register( 'Facilities' );
        end;

      with TMetaPopulatedBlock.Create( 'MiddleClassBuilding', 102, pkMiddle, 200, TPopulatedBlock ) do
        begin
          xSize := 2;
          ySize := 2;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( '0B', 'Medium Building (middle class)', TFacility ) do
        begin
          XSize := 2;
          YSize := 2;
          Level := 110;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '2x2 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'MiddleClassBuilding'] as TMetaBlock ));
          Register( 'Facilities' );
        end;

      with TMetaPopulatedBlock.Create( 'LowClassBuilding', 103, pkLow, 400, TPopulatedBlock ) do
        begin
          xSize := 3;
          ySize := 3;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( '0C', 'Large Building (low class)', TFacility ) do
        begin
          XSize := 3;
          YSize := 3;
          Level := 140;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '3x3 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'LowClassBuilding'] as TMetaBlock ));
          Register( 'Facilities' );
        end;
      with TMetaWorkCenter.Create( 'Factory', 104, [40, 100, 500], TWorkCenter ) do
        begin
          xSize := 4;
          ySize := 4;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( '0D', 'Factory of Nothing', TFacility ) do
        begin
          XSize := 4;
          YSize := 4;
          Level := 14;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '4x4 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'Factory'] as TMetaBlock ));
          Register( 'Facilities' );
        end;

      // Public Facilities
      with TMetaPublicFacilityInfo.Create( 'Police' ) do
        begin
          Name := 'Police Dep.';
          Importance := 100;
          ModStrength := 10;
          Register( tidClassFamily_PublicFacilities )
        end;
      with TMetaPublicFacilityInfo.Create( 'Fire' ) do
        begin
          Name := 'Fire Dep.';
          Importance := 30;
          ModStrength := 10;
          Register( tidClassFamily_PublicFacilities )
        end;
      with TMetaPublicFacilityInfo.Create( 'Health' ) do
        begin
          Name := 'Health';
          Importance := 70;
          ModStrength := 10;
          Register( tidClassFamily_PublicFacilities )
        end;
      with TMetaPublicFacilityInfo.Create( 'School' ) do
        begin
          Name := 'School';
          Importance  := 80;
          ModFact     := 0.1;
          ModStrength := 10;
          Register( tidClassFamily_PublicFacilities )
        end;
      with TMetaPublicFacility.Create( 'PoliceStation', 110, TPublicFacility ) do
        begin
          Kind     := TheClassStorage.ClassById[tidClassFamily_PublicFacilities, 'Police'] as TMetaPublicFacilityInfo;
          Strength := 1000;
          XSize    := 1;
          YSize    := 1;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( 'PoliceStation', 'Police Station', TFacility ) do
        begin
          XSize := 1;
          YSize := 1;
          Level := 100;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '1x1 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'PoliceStation'] as TMetaBlock ));
          Register( 'Facilities' );
        end;
      with TMetaPublicFacility.Create( 'FireStation', 111, TPublicFacility ) do
        begin
          Kind     := TheClassStorage.ClassById[tidClassFamily_PublicFacilities, 'Fire'] as TMetaPublicFacilityInfo;
          Strength := 2000;
          XSize    := 2;
          YSize    := 2;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( 'FireStation', 'Fire Station', TFacility ) do
        begin
          XSize := 2;
          YSize := 2;
          Level := 100;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '2x2 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'FireStation'] as TMetaBlock ));
          Register( 'Facilities' );
        end;
      with TMetaPublicFacility.Create( 'Hospital', 112, TPublicFacility ) do
        begin
          Kind     := TheClassStorage.ClassById[tidClassFamily_PublicFacilities, 'Health'] as TMetaPublicFacilityInfo;
          Strength := 2000;
          XSize    := 2;
          YSize    := 2;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( 'Hospital', 'Hospital', TFacility ) do
        begin
          XSize := 2;
          YSize := 2;
          Level := 100;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '2x2 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'Hospital'] as TMetaBlock ));
          Register( 'Facilities' );
        end;
      with TMetaPublicFacility.Create( 'Clinic', 113, TPublicFacility ) do
        begin
          Kind     := TheClassStorage.ClassById[tidClassFamily_PublicFacilities, 'Health'] as TMetaPublicFacilityInfo;
          Strength := 100;
          XSize    := 1;
          YSize    := 1;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( 'Clinic', 'Clinic', TFacility ) do
        begin
          XSize := 1;
          YSize := 1;
          Level := 100;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '1x1 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'Clinic'] as TMetaBlock ));
          Register( 'Facilities' );
        end;
      with TMetaPublicFacility.Create( 'School', 114, TPublicFacility ) do
        begin
          Kind     := TheClassStorage.ClassById[tidClassFamily_PublicFacilities, 'School'] as TMetaPublicFacilityInfo;
          Strength := 100;
          XSize    := 2;
          YSize    := 2;
          Register( 'Blocks' );
        end;
      with TMetaFacility.Create( 'School', 'School', TFacility ) do
        begin
          XSize := 2;
          YSize := 2;
          Level := 100;
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', '2x2 construction'] as TMetaBlock ));
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'Suddenly achieved', TheClassStorage.ClassById['Blocks', 'School'] as TMetaBlock ));
          Register( 'Facilities' );
        end;

      RegisterTownHall( 'TownHall', 12, 3, 3 );
      RegisterTradeCenter( 'TradeCenter', 14, 2, 2 );

      GridSize := 25;
      fWorld := TInhabitedWorld.Create( 200, 200 );

      k := 0;
      for i := 0 to trunc(sqrt(TownCount)) - 1 do
        for j := 0 to trunc(sqrt(TownCount)) - 1 do
          begin
            TownColors[k] := random( $00FFFFFF );
            x := (fWorld.xSize div (trunc(sqrt(TownCount))))*succ(i) - (fWorld.xSize div (trunc(sqrt(TownCount)))) div 2 + (1 - 2*random(2))*random(10);
            y := (fWorld.ySize div (trunc(sqrt(TownCount))))*succ(j) - (fWorld.ySize div (trunc(sqrt(TownCount)))) div 2 + (1 - 2*random(2))*random(10);
            Town := TInhabitedTown.Create( 'TownHall', 'TradeCenter', x, y, fWorld );
            Town.Name := 'Guamuta ' + IntToStr(k + 1);
            fWorld.Towns.Insert( Town );
            inc( k );
          end;

      Company := TCompany.Create;
      Company.Budget := 50*1000*1000;
      Company.Name := 'Zonzom Corp.';
      fWorld.Companies.Insert( Company );
      MapImage.Picture.Bitmap := TBitmap.Create;
      MapImage.Picture.Bitmap.Width := MapImage.Width;
      MapImage.Picture.Bitmap.Height := MapImage.Height;
      for i := 0 to pred(TheClassStorage.ClassCount['Facilities']) do
        if (TheClassStorage.ClassByIdx['Facilities', i] as TMetaFacility).InTown
          then
            begin
              Item := TMenuItem.Create( PopupMenu );
              PopupMenu.Items.Add( Item );
              with Item, (TheClassStorage.ClassByIdx['Facilities', i] as TMetaFacility) do
                begin
                  Caption := 'Build ' + Name + ' $' + FormatMoney(IntToStr(round(Price/1000))) + 'K';
                  Tag     := i;
                  OnClick := OnItemClick;
                end;
            end;
      Item := TMenuItem.Create( PopupMenu );
      with Item do
        begin
          Caption := '-';
        end;
      PopupMenu.Items.Add( Item );
      ItemDelete := TMenuItem.Create( PopupMenu );
      with ItemDelete do
        begin
          Caption := 'Delete';
          OnClick := DeleteFacility;
        end;
      PopupMenu.Items.Add( ItemDelete );
      Grnd := TBitmap.Create;
      Grnd.LoadFromFile( ExtractFilePath(Application.ExeName) + 'ground.bmp' );
      for i := 0 to fWorld.xSize - 1 do
        for j := 0 to fWorld.ySize - 1 do
          fWorld.GroundMap[i, j] := fWorld.NearestTown( i, j ).Id;
      fDelta.x := fWorld.xSize div 2;
      fDelta.y := fWorld.ySize div 2;
      Left := 0;
      Top := 0;
      Width := Screen.Width;
      Height := Screen.Height;
    end;

  procedure TWorldTestForm.MapImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if Button = mbRight
        then
          begin
            fx := fDelta.x + x div GridSize;
            fy := fDelta.y + y div GridSize;
            PopupMenu.Popup( ClientOrigin.x + x, ClientOrigin.y + y );
          end
        else
          begin
            fDown := false;
            PaintFacilities;
            PaintTowns;
            {
            if (abs(x - fDrag.x) div GridSize > 0) or (abs(y - fDrag.y) div GridSize > 0)
              then
                begin
                  dec( fDelta.x, (x - fDrag.x) div GridSize );
                  dec( fDelta.y, (y - fDrag.y) div GridSize );
                  PaintMap;
                end;
            }
          end;
    end;

  procedure TWorldTestForm.PaintMap( x, y, dx, dy : integer );
    var
      xi, yi : integer;
    begin
      with MapImage.Picture.Bitmap.Canvas do
        begin
          for yi := y to y + dy - 1 do
            for xi := x to x + dx - 1 do
              begin
                if (xi < fWorld.xSize - 1) and (yi < fWorld.ySize - 1) and (xi > 0) and (yi > 0)
                  then
                    if fWorld.ObjectMap[xi, yi] = nil
                      then
                        begin
                          Brush.Color := Grnd.Canvas.Pixels[xi mod 100, yi mod 100];
                          Pen.Color := Brush.Color;
                        end
                      else
                        begin
                          Brush.Color := clBlack;
                          Pen.Color   := clSilver;
                        end
                  else
                    begin
                      Pen.Color := clBlack;
                      Brush.Color := clBlack;
                    end;
                Pen.Width := 1;
                Rectangle( GridSize*(xi - fDelta.x), GridSize*(yi - fDelta.y), GridSize*(xi - fDelta.x) + GridSize, GridSize*(yi - fDelta.y) + GridSize );
                if ShowTowns.Down
                  then
                    begin
                      Pen.Color := TownColors[fWorld.GroundMap[xi, yi] - 1];
                      //Pen.Width := 2;
                      MoveTo( GridSize*(xi - fDelta.x) + GridSize, GridSize*(yi - fDelta.y) );
                      LineTo( GridSize*(xi - fDelta.x), GridSize*(yi - fDelta.y) + GridSize );
                      MoveTo( GridSize*(xi - fDelta.x), GridSize*(yi - fDelta.y) );
                      LineTo( GridSize*(xi - fDelta.x) + GridSize, GridSize*(yi - fDelta.y) + GridSize );
                    end;
              end;
        end;
    end;

  procedure TWorldTestForm.PaintFacilities;

    function FindMetaBlock( id : TMetaBlockNumId ) : TMetaBlock;
      var
        i     : integer;
        count : integer;
      begin
        i := 0;
        count := TheClassStorage.ClassCount['Blocks'];
        while (i < count) and (TMetaBlock(TheClassStorage.ClassByIdx['Blocks', i]).NumId <> Id) do
          inc( i );
        if i < count
          then result := TMetaBlock(TheClassStorage.ClassByIdx['Blocks', i])
          else result := nil;
      end;

    var
      Report  : TObjectReport;
      BlockId : TMetaBlockNumId;
      MBlock  : TMetaBlock;
      x, y    : word;
      i       : integer;
    begin
      try
        Report := fWorld.GetObjectsInArea( fDelta.x, fDelta.y, MapImage.Width div GridSize - 1, MapImage.Height div GridSize - 1);
        for i := 0 to pred(length(Report) div 4) do
          begin
            BlockId := TMetaBlockNumId(Report[4*i + 1]);
            x       := TMetaBlockNumId(Report[4*i + 3]);
            y       := TMetaBlockNumId(Report[4*i + 4]);
            MBlock  := FindMetaBlock( BlockId );
            with MapImage.Picture.Bitmap.Canvas do
              begin
                Brush.Color := TownColors[fWorld.FacilityAt( x, y ).Town.Id - 1];
                if (BlockId >= 40) and (BlockId <= 50)
                  then
                    begin
                      Brush.Style := bsClear;
                      Pen.Color   := clRed;
                    end
                  else
                    begin
                      Brush.Style := bsSolid;
                      Pen.Color   := clBlack;
                    end;
                Rectangle( (x - fDelta.x)*GridSize, (y - fDelta.y)*GridSize, (x + MBlock.xSize - fDelta.x)*GridSize, (y + MBlock.ySize - fDelta.y)*GridSize );
              end;
          end;
        //Panel4.Caption := IntToStr(length(Report) div 4) + ' utilities on screen';
      except
      end;
    end;

  procedure TWorldTestForm.PaintTowns;
    var
      i : integer;
    begin
      with MapImage.Picture.Bitmap.Canvas do
        begin
          Brush.Color := clBlack;
          Font.Color   := clWhite;
        end;
      for i := 0 to pred(fWorld.Towns.Count) do
        with TTown(fWorld.Towns[i]) do
          if (xPos > fDelta.x) and (xPos < fDelta.x + MapImage.Width div GridSize) and
             (yPos > fDelta.y) and (yPos < fDelta.y + MapImage.Height div GridSize)
            then
              with MapImage.Picture.Bitmap.Canvas do
                begin
                  TextOut( (xPos - fDelta.x)*GridSize, (yPos - fDelta.y)*GridSize, Name );
                end;
    end;

  procedure TWorldTestForm.FormResize(Sender: TObject);
    begin
      MapImage.Picture.Bitmap.Width  := MapImage.Width;
      MapImage.Picture.Bitmap.Height := MapImage.Height;
      if fDelta.x < 0
        then fDelta.x := 0;
      if fDelta.y < 0
        then fDelta.y := 0;
      if fDelta.x + MapImage.Width div GridSize - 1 > fWorld.xSize
        then fDelta.x := fWorld.xSize - MapImage.Width div GridSize;
      if fDelta.y + MapImage.Height div GridSize - 1 > fWorld.xSize
        then fDelta.y := fWorld.ySize - MapImage.Height div GridSize;
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
      PaintFacilities;
      PaintTowns;
    end;

  procedure TWorldTestForm.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fDown := true;
      fDrag.x := x;
      fDrag.y := y;
    end;

  procedure TWorldTestForm.MapImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
      R1, R2   : TRect;
      oldDelta : TPoint;
    begin
      if fDown and ((abs(x - fDrag.x) div GridSize > 0) or (abs(y - fDrag.y) div GridSize > 0))
        then
          begin
            oldDelta := fDelta;
            dec( fDelta.x, (x - fDrag.x) div GridSize );
            dec( fDelta.y, (y - fDrag.y) div GridSize );
            if fDelta.x < 0
              then fDelta.x := 0;
            if fDelta.y < 0
              then fDelta.y := 0;
            if fDelta.x + MapImage.Width div GridSize - 1 > fWorld.xSize
              then fDelta.x := fWorld.xSize - MapImage.Width div GridSize;
            if fDelta.y + MapImage.Height div GridSize - 1 > fWorld.xSize
              then fDelta.y := fWorld.ySize - MapImage.Height div GridSize;

            x := GridSize*(oldDelta.x - fDelta.x) + fDrag.x;
            y := GridSize*(oldDelta.y - fDelta.y) + fDrag.y;

            if x > fDrag.x
              then
                begin
                  R1.Left  := 0;
                  R1.Right := MapImage.Width - ((x - fDrag.x) div GridSize)*GridSize;
                  R2.Left  := ((x - fDrag.x) div GridSize)*GridSize;
                  R2.Right := MapImage.Width;
                end
              else
                begin
                  R1.Left  := -((x - fDrag.x) div GridSize)*GridSize;
                  R1.Right := MapImage.Width;
                  R2.Left  := 0;
                  R2.Right := MapImage.Width + ((x - fDrag.x) div GridSize)*GridSize;
                end;

            if y > fDrag.y
              then
                begin
                  R1.Top    := 0;
                  R1.Bottom := MapImage.Height - ((y - fDrag.y) div GridSize)*GridSize;
                  R2.Top    := ((y - fDrag.y) div GridSize)*GridSize;
                  R2.Bottom := MapImage.Height;
                end
              else
                begin
                  R1.Top    := -((y - fDrag.y) div GridSize)*GridSize;
                  R1.Bottom := MapImage.Height;
                  R2.Top    := 0;
                  R2.Bottom := MapImage.Height + ((y - fDrag.y) div GridSize)*GridSize;
                end;

            MapImage.Picture.Bitmap.Canvas.CopyRect( R2, MapImage.Picture.Bitmap.Canvas, R1 );

            if x > fDrag.x
              then PaintMap( fDelta.x, fDelta.y, (x - fDrag.x) div GridSize, MapImage.Height div GridSize + 1 )
              else PaintMap( fDelta.x + MapImage.Width div GridSize - abs(x - fDrag.x) div GridSize, fDelta.y, abs(x - fDrag.x) div GridSize + 1, MapImage.Height div GridSize + 1 );

            if y > fDrag.y
              then PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, (y - fDrag.y) div GridSize )
              else PaintMap( fDelta.x, fDelta.y + MapImage.Height div GridSize - abs(y - fDrag.y) div GridSize, MapImage.Width div GridSize + 1, abs(y - fDrag.y) div GridSize + 1 );

            //PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );

            fDrag.x := x;
            fDrag.y := y;
          end;
    end;

  procedure TWorldTestForm.OnItemClick( Sender : TObject );
    begin
      fDown := false;
      with TheClassStorage.ClassByIdx['Facilities', (Sender as TComponent).Tag] as TMetaFacility do
        begin
          fWorld.NewFacility( Id, 1, fx, fy );
          PaintMap( fx, fy, xSize, ySize );
        end;
      PaintFacilities;
      PaintTowns;
    end;

  procedure TWorldTestForm.DeleteFacility( Sender : TObject );
    var
      x, y, dx, dy : integer;
    begin
      with fWorld.FacilityAt( fx, fy ), MetaFacility do
        begin
          x  := xPos;
          y  := yPos;
          dx := xSize;
          dy := ySize;
        end;
      fWorld.DelFacility( fx, fy );
      PaintMap( x, y, dx, dy );
      fDown := false;
    end;

  procedure TWorldTestForm.SpeedButton2Click(Sender: TObject);
    begin
      GridSize := (Sender as TComponent).Tag;
      if fDelta.x + MapImage.Width div GridSize - 1 > fWorld.xSize
        then fDelta.x := fWorld.xSize - MapImage.Width div GridSize;
      if fDelta.y + MapImage.Height div GridSize - 1 > fWorld.xSize
        then fDelta.y := fWorld.ySize - MapImage.Height div GridSize;
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
      PaintFacilities;
      PaintTowns;
    end;

  procedure TWorldTestForm.ShowTownsClick(Sender: TObject);
    begin
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
      PaintFacilities;
      PaintTowns;
    end;

  procedure TWorldTestForm.PopupMenuPopup(Sender: TObject);
    var
      F : TFacility;
    begin
      F := fWorld.FacilityAt( fx, fy );
      ItemDelete.Enabled := (F <> nil) and (F.Company <> nil);
    end;

  procedure TWorldTestForm.TimerTimer(Sender: TObject);
    begin
      fWorld.VirtualTimeTick( 1 );
      fWorld.Simulate;
      PaintFacilities;
      PaintTowns;
      if BlockViewer.Visible
        then BlockViewer.RefreshContent;
      Money.Caption := '$' + FormatMoney(IntToStr(round(Company.Budget)));
      Date.Caption := DateToStr( fWorld.VirtualTime );
    end;

  procedure TWorldTestForm.MapImageDblClick(Sender: TObject);
    var
      F : TFacility;
    begin
      F := fWorld.FacilityAt( fDelta.x + fDrag.x div GridSize, fDelta.y + fDrag.y div GridSize );
      if F <> nil
        then BlockViewer.Facility := F;
    end;









end.


