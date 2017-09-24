unit SMC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TForm6 = 
    class(TForm)
        Button1: TButton;
        Button2: TButton;
        Button4: TButton;
        Button5: TButton;
        Button3: TButton;
        Button6: TButton;
        Button7: TButton;
        Button8: TButton;
        RichEdit1: TRichEdit;
        Splitter1: TSplitter;
    Button9: TButton;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button6Click(Sender: TObject);
        procedure Button7Click(Sender: TObject);
        procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
      public
        kk : pointer;
        ll : array of integer;
      private
        procedure CheckPointer(p: pointer);
    end;

var
  Form6: TForm6;

implementation
{$R *.DFM}

 uses
   SMCheck;

procedure TForm6.Button1Click(Sender: TObject);
  begin
    getmem(kk, 100028);
    CheckPointer(kk);
  end;

procedure TForm6.Button2Click(Sender: TObject);
  begin
    freemem(kk, 100028);
  end;

procedure TForm6.Button4Click(Sender: TObject);
  var
    i : integer;
  begin
    i := length(ll);
    setlength(ll, i+100);
  end;

procedure TForm6.Button5Click(Sender: TObject);
  begin
    setlength(ll, 0);
  end;

procedure TForm6.Button3Click(Sender: TObject);
  begin
    kk := nil;
    ReallocMem(kk, 0);
    CheckPointer(kk);
  end;

procedure TForm6.Button6Click(Sender: TObject);
  begin
    kk := nil;
    ReallocMem(kk, 100);
    CheckPointer(kk);
  end;

procedure TForm6.Button7Click(Sender: TObject);
  begin
    ReallocMem(kk, 0);
  end;

procedure TForm6.Button8Click(Sender: TObject);
  begin
    ReallocMem(kk, 4000);
    CheckPointer(kk);
  end;

procedure TForm6.CheckPointer(p: pointer);
  var
    Prev : pointer;
  begin
    with RichEdit1.lines do
      begin
        Prev := PrevBlock(p);
        if Prev=nil
          then add(format(' %x Not Prev',[integer(p)]))
          else 
            begin
              if IsObject(Prev)
                then add(format(' %x Prev = %x  Object = %s',[integer(p), integer(Prev), TObject(Prev).classname]))
                else add(format(' %x Prev = %x  Not Is Object',[integer(p), integer(Prev)]));
            end;
      end;
  end;

procedure TForm6.Button9Click(Sender: TObject);
  var
    t : TObject;
  begin
    t := TObject.create;
    CheckPointer(t); 
  end;

end.
