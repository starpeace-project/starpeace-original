unit BackroundHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VisualControls, ExtCtrls;

type
  TBackgroundHandlerView = class(TVisualControl)
    Image: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BackgroundHandlerView: TBackgroundHandlerView;

implementation

{$R *.DFM}

end.
