unit VoxelRendererWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    FiveControl;

  type
    TVoxelRendererForm =
      class(TForm)
          procedure FormCreate(Sender: TObject);
          procedure FormShow(Sender: TObject);
          procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        private
          { Private declarations }
          fMapView : TFiveControl;
        public
          { Public declarations }
      end;

  var
    VoxelRendererForm: TVoxelRendererForm;

implementation

  {$R *.DFM}

  uses
    LocalCacheManager;

  const
    CachePath = 'C:\Work\Five\Release\Cache';

  procedure TVoxelRendererForm.FormCreate(Sender: TObject);
    var
      Agents : TWorldAgents;
    begin
      fMapView := TFiveControl.Create(Self);
      with fMapView do
        begin
          Align     := alClient;
          ZoomLevel := 3; // ord(zr32x64);
        end;
      GetWorldAgents(Agents);
      Agents.Manager.Load(CachePath);
      Agents.Map.InitMap;
      fMapView.Document := Agents.Document;
    end;

  procedure TVoxelRendererForm.FormShow(Sender: TObject);
    begin
      fMapView.Parent := Self;
    end;

  procedure TVoxelRendererForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_ADD:
          fMapView.ZoomIn;
        VK_SUBTRACT:
          fMapView.ZoomOut;
      end;
    end;

end.
