unit DepWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TDependencyWindow = class(TForm)
    DepLines: TMemo;
    Panel1: TPanel;
    CloseBtn: TButton;
  private
    procedure SetLines(theLines : TStringList);
  public
    property Lines : TStringList write SetLines;
  end;

var
  DependencyWindow: TDependencyWindow;

implementation

{$R *.DFM}

  procedure TDependencyWindow.SetLines(theLines : TStringList);
    begin
      DepLines.Lines := theLines;
    end;

end.
