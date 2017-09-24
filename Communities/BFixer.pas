unit BFixer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Path: TEdit;
    Label1: TLabel;
    Forums: TListBox;
    Label2: TLabel;
    Search: TButton;
    Fix: TButton;
    procedure SearchClick(Sender: TObject);
    procedure FixClick(Sender: TObject);
  private
    procedure FixForum( name: string );
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
  tidPrefix_Author           = 'Author';
  tidPrefix_Subject          = 'Subject';
  tidPrefix_Id               = 'Id';
  tidPrefix_UId              = 'UId';
  tidPrefix_TimeStamp        = 'TimeStamp';
  tidPrefix_Kind             = 'Kind';
  tidFileExt_Topic           = '.topic';
  tidFileExt_Post            = '.post';
  tidProperty_Modified       = 'Modified';
  tidProperty_PostCount      = 'PostCount';
  tidProperty_PollItemsCount = 'PollItemCount';
  tidProperty_PollItemDesc   = 'PollItemDesc';
  tidProperty_PollItemVotes  = 'PollItemVotes';
  tidFileName_TopicIdx       = 'topic.index';

procedure TForm1.SearchClick(Sender: TObject);
  var
    SearchRec : TSearchRec;
    found     : integer;
  begin
    if Path.Text = ''
      then MessageDlg( 'You have to specify a path !', mtError, [mbOk], 0 )
      else
        begin
          Forums.Items.Clear;
          found := FindFirst( Path.Text + '\*', faDirectory, SearchRec );
          try
            while found = 0 do
              begin
                if ((SearchRec.Attr and faDirectory) = faDirectory) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
                  then Forums.Items.Add( SearchRec.Name );
                found := FindNext( SearchRec );
              end;
          finally
            FindClose( SearchRec );
          end;
          Fix.Enabled := true;
        end;
  end;

procedure TForm1.FixClick(Sender: TObject);
var
  i : integer;
begin
  if Forums.SelCount = 0
    then MessageDlg( 'You have to select some forums from the list !', mtError, [mbOk], 0 )
    else
      for i := 0 to Forums.Items.Count - 1 do
        if Forums.Selected[i]
          then
            begin
              FixForum( Forums.Items[i] );
              Forums.Items[i] := Forums.Items[i] + '   --> FIXED!';
            end;
end;

procedure TForm1.FixForum( name: string );
  var
    folder    : string;
    SearchRec : TSearchRec;
    found     : integer;
    Topics    : TStringList;
    RepCount  : integer;
    TopicIdx  : TStringList;
    DateStamp : string;
    i         : integer;
    TopicPath : string;
  begin
    folder := Path.Text + '\' + name;
    Topics := TStringList.Create;
    try
      found := FindFirst( folder + '\*' + tidFileExt_Topic, faAnyFile, SearchRec );
      try
        while found = 0 do
          begin
            Topics.Add( SearchRec.Name );
            found := FindNext( SearchRec );
          end;
      finally
        FindClose( SearchRec );
      end;

      if Topics.Count > 0
        then
          for i := 0 to Topics.Count - 1 do
            begin
              TopicPath := folder + '\' + Topics[i];
              RepCount  := 0;
              found     := FindFirst( TopicPath + '\*' + tidFileExt_Post, faAnyFile, SearchRec );
              try
                while found = 0 do
                  begin
                    inc( RepCount );
                    found := FindNext( SearchRec );
                  end;
              finally
                FindClose( SearchRec );
              end;
              TopicIdx := TStringList.Create;
              try
                TopicIdx.LoadFromFile( TopicPath + '\' + tidFileName_TopicIdx );
                DateStamp := TopicIdx.Values[tidProperty_Modified];
              finally
                TopicIdx.Free;
              end;
              if DateStamp <> ''
                then
                  begin
                    TopicIdx := TStringList.Create;
                    try
                      try
                        TopicIdx.LoadFromFile( TopicPath + '\' + tidFileName_TopicIdx );
                      except
                      end;
                      TopicIdx.Values[tidProperty_Modified]  := DateStamp;
                      TopicIdx.Values[tidProperty_PostCount] := IntToStr( RepCount );
                      TopicIdx.SaveToFile( TopicPath + '\' + tidFileName_TopicIdx );
                    finally
                      TopicIdx.Free;
                    end;
                  end;
            end;
    finally
      Topics.Free;
    end;
  end;

end.
