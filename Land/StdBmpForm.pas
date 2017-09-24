unit StdBmpForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TStdBmpViewer = class(TForm)
    ePath: TEdit;
    btnStart: TButton;
    Label1: TLabel;
    Label2: TLabel;
    eFilter: TEdit;
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StdBmpViewer: TStdBmpViewer;

implementation

{$R *.DFM}

  function OpenBitmap(path : string) : TBitmap;
    var
      Stream : TStream;
    begin
      result := nil;
      try
        Stream := TFileStream.Create(path, fmOpenRead);
        try
          result := TBitmap.Create;
          result.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
      except
        if result <> nil
          then result.Free;
        result := nil;
      end;
    end;

  procedure SaveBitmap(path : string; Bmp : TBitmap);
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create(path, fmCreate);
      try
        Bmp.SaveToStream(Stream);
      finally
        Stream.Free;
      end;
    end;

  procedure FilterBitmap(Template, Bmp : TBitmap);
    var
      x, y  : integer;
      tc    : TColor;
      c1    : TColor;
    begin
      tc := Bmp.Canvas.Pixels[0, 0];
      c1 := Template.Canvas.Pixels[0, 0];
      for x := 0 to pred(Bmp.Width) do
        for y := 0 to pred(Bmp.Height) do
          if Template.Canvas.Pixels[x, y] = c1
            then Bmp.Canvas.Pixels[x, y] := tc;
    end;

  procedure TStdBmpViewer.btnStartClick(Sender: TObject);
    var
      Template : TBitmap;
      Path     : string;
      Filter   : string;
      found    : integer;
      Search   : TSearchRec;
      TmpBmp   : TBitmap;
    begin
      Path     := ExtractFilePath(ParamStr(0));
      Template := OpenBitmap(Path + 'template.bmp');
      Filter   := UpperCase(eFilter.Text);
      if Template <> nil
        then
          begin
            Path  := ePath.Text;
            if Path[length(Path)] <> '\'
              then Path := Path + '\';
            CreateDir(Path + 'New');
            found := FindFirst(Path + '*.bmp', faArchive, Search);
            try
              btnStart.Enabled := false;
              while found = 0 do
                begin
                  if pos(Filter, UpperCase(Search.Name)) = 0
                    then
                      begin
                        TmpBmp := OpenBitmap(Path + Search.Name);
                        FilterBitmap(Template, TmpBmp);
                        SaveBitmap(Path + 'New\' + Search.Name, TmpBmp);
                        Application.ProcessMessages;
                      end;
                  found := FindNext(Search);
                end;
              btnStart.Enabled := true;
            finally
              FindClose(Search);
            end;
          end;
    end;

end.
