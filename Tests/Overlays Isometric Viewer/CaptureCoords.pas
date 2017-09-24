unit CaptureCoords;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCaptureCoordinates = class(TForm)
    X: TEdit;
    Y: TEdit;
    Ok: TButton;
    Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CaptureCoordinates: TCaptureCoordinates;

implementation

{$R *.DFM}


end.
