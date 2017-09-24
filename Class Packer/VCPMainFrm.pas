unit VCPMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IniClasses, VisualClassManager;

type
  TVisualClassPackerForm = class(TForm)
    Start: TButton;
    edPath: TEdit;
    btnBrowse: TButton;
    Label1: TLabel;
    OpenDialog: TOpenDialog;
    mClasses: TMemo;
    bLoad: TButton;
    Index: TEdit;
    Section: TEdit;
    vName: TEdit;
    Test: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
  private
    fClassMan : TClassManager;
  private
    procedure AddClass(path : string);
  end;

var
  VisualClassPackerForm: TVisualClassPackerForm;

implementation

{$R *.DFM}

  procedure TVisualClassPackerForm.btnBrowseClick(Sender: TObject);
    begin
      if OpenDialog.Execute
        then edPath.Text := OPenDialog.FileName;
    end;

  procedure TVisualClassPackerForm.AddClass(path : string);
    var
      IniClss : TIniClass;
    begin
      IniClss := TIniClass.Open(path);
      try
        IniClss.ReadAllSections;
        fClassMan.AddClass(IniClss);
      finally
        IniClss.Free;
      end;
    end;

  procedure TVisualClassPackerForm.StartClick(Sender: TObject);
    var
      Search : TSearchRec;
      Path   : string;
      count  : integer;
      Stream : TStream;
    begin
      count := 0;
      fClassMan.Free;
      fClassMan := TClassManager.Create;
      Path := ExtractFilePath(edPath.Text);
      if FindFirst(Path + '*.final.ini', faArchive, Search) = 0
        then
          repeat
            try
              AddClass(Path + Search.Name);
              inc(count);
            except
              ShowMessage('Class without Id found: ' + Search.Name);
            end;
          until FindNext(Search) <> 0;

      //for i := 0 to pred(fClassMan.Count) do
        //mClasses.Lines.Add(IntToStr(fClassMan.Classes[i].Id));

      ShowMessage(IntToStr(count) + ' classes read.');

      Stream := TFileStream.Create(Path + 'classes.bin', fmCreate);
      try
        fClassMan.Store(Stream);
      finally
        Stream.Free;
      end;
    end;

  procedure TVisualClassPackerForm.bLoadClick(Sender: TObject);
    var
      Stream : TStream;
      dt     : TDateTime;
    begin
      fClassMan.Free;
      Stream := TFileStream.Create(ExtractFilePath(edPath.Text) + 'classes.bin', fmOpenRead);
      try
        dt := Time;
        fClassMan := TClassManager.Load(Stream);
        dt := Time - dt;
        //for i := 0 to pred(fClassMan.Count) do
          //mClasses.Lines.Add(IntToStr(fClassMan.Classes[i].Id));
        ShowMessage(DateTimeToStr(dt));
      finally
        Stream.Free;
      end;
    end;

  procedure TVisualClassPackerForm.TestClick(Sender: TObject);
    begin
      ShowMessage(fClassMan.Classes[StrToInt(Index.Text)].ReadString(Section.Text, vName.Text, 'NIENTE!'));
    end;

end.
