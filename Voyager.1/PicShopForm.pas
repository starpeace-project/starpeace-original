unit PicShopForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FramedButton, StdCtrls, OpaquePanel, MarqueeCtrl, ComCtrls,
  SocketComp, ExtDlgs, InternationalizerComponent;

const
  PictureTransferPort = 6010;
  BufferSize          = 1024;
  PhotoWidth          = 150;
  PhotoHeight         = 200;


type
  TPictureShopViewer = class(TForm)
    Panel3: TPanel;
    DestImage: TImage;
    btnLoad: TFramedButton;
    btnPreview: TFramedButton;
    btnSend: TFramedButton;
    btnCancel: TFramedButton;
    SourcePanel: TOpaquePanel;
    Panel1: TPanel;
    ZoomIn: TFramedButton;
    Panel4: TPanel;
    ZoomOut: TFramedButton;
    Marquee: TMarquee;
    Timer: TTimer;
    Shape1: TShape;
    SourceImage: TImage;
    OpenDialog: TOpenPictureDialog;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure SourcePanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SourcePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ZoomInClick(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    fUserImage  : TBitmap;
    fTempImage  : TBitmap;
    fRect       : TRect;
    fLastX      : integer;
    fLastY      : integer;
    fZoomLevel  : byte;
    fCanDrag    : boolean;
    fRepSource  : boolean;
    fProgress   : integer;
    fBusy       : boolean;
    fSocket     : TClientSocket;
    fStream     : TMemoryStream;
    fServerAddr : string;
    fWorldName  : string;
    fUserName   : string;
    fCacheDir   : string;
    fCanSndData : boolean;
  protected
    procedure Paint; override;
  public
    procedure LoadImage(path : string);
    function  CenterImage(var Image : TBitmap) : boolean;
    procedure SetImage;
    procedure DragImage(deltaX, deltaY : integer);
    procedure InvalidateImage;
    procedure NotifyPerc(Sender : TObject; perc : integer);
    procedure SendPicture;
    procedure SocketRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure SocketConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure SocketWrite(Sender : TObject; Socket : TCustomWinSocket);
    procedure FinishSend(success : boolean; msg : string);
    procedure HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
  private
    procedure Message_WM_NCHITTEST(var Message : TMessage); message WM_NCHITTEST;
  public
    property ServerAddr : string read fServerAddr write fServerAddr;
    property WorldName  : string read fWorldName  write fWorldName;
    property UserName   : string read fUserName   write fUserName;
    property CacheDir   : string read fCacheDir   write fCacheDir;
  end;

var
  PictureShopViewer: TPictureShopViewer;

implementation

  uses
    MathUtils, jpeg, Winsock, Literals;

{$R *.DFM}

  type
    TNotifyPerc = procedure(Sender : TObject; perc : integer) of object;

  type
    TRGBRec =
      packed record
        R : byte;
        G : byte;
        B : byte;
        I : byte;
      end;

    PColorArray = ^TColorArray;
    TColorArray = array[0..0] of TColor;

  procedure CopyBitmap(Source, Dest : TBitmap);
    var
      srcLine  : PColorArray;
      dstLine  : PColorArray;
      lidx     : integer;
      cidx     : integer;
      theWidth : integer;
    begin
      theWidth   := Source.Width;
      Dest.Width := theWidth;
      Dest.Height := Source.Height;
      for lidx := 0 to pred(Source.Height) do
        begin
          srcLine := PColorArray(Source.ScanLine[lidx]);
          dstLine := PColorArray(Dest.ScanLine[lidx]);
          for cidx := 0 to pred(theWidth) do
            srcLine[cidx] := dstLine[cidx];
          //move(srcLine^, dstLine^, theWith*sizeof(TColor));
        end;
    end;

  procedure ZoomBitmap(Source, Dest : TBitMap; level : byte; NotProc : TNotifyPerc);
    var
      R   : TRect;
      w   : integer;
      h   : integer;
      row : integer;
      col : integer;
      nr  : integer;
      nc  : integer;

    function AvrgPoint(pc, pr : integer; defColor : TColor) : TColor;
      var
        cnt : integer;
        i   : integer;
        j   : integer;
        sR  : integer;
        sG  : integer;
        sB  : integer;
      begin
        sR  := 0;
        sG  := 0;
        sB  := 0;
        cnt := 0;
        for i := pr to pr + level do
          for j := pc to pc + level do
            if PtInRect(R, Point(j, i))
              then
                with TRGBRec(Source.Canvas.Pixels[j, i]) do
                  begin
                    inc(sR, R);
                    inc(sG, G);
                    inc(sB, B);
                    inc(cnt);
                  end;
        result := defColor;
        if cnt > 0
          then
            with TRGBRec(Result) do
              begin
                R := round(sR/cnt);
                G := round(sG/cnt);
                B := round(sB/cnt);
              end;
      end;

    begin
      R := Rect(0, 0, Source.Width, Source.Height);
      w := Source.Width;
      h := Source.Height;
      Dest.Width  := w div level;
      Dest.Height := h div level;
      if level = 1
        then Dest.Canvas.CopyRect(R, Source.Canvas, R)
        else
          begin
            row := 0;
            nr  := 0;
            while row < h do
              begin
                col := 0;
                nc  := 0;
                while col < w do
                  begin
                    Dest.Canvas.Pixels[nc, nr] := AvrgPoint(col, row, clBlack);
                    inc(col, level);
                    inc(nc);
                  end;
                inc(row, level);
                inc(nr);
                if Assigned(NotProc)
                  then NotProc(nil, round(100*row/h));
              end;
          end;
    end;

  procedure MixImages(Dest, Source : TBitmap; NotProc : TNotifyPerc);
    var
      row   : integer;
      col   : integer;
      Color : TColor;
      h     : integer;
      R     : TRect;
    begin
      h := Dest.Height;
      for row := 0 to pred(h) do
        begin
          for col := 0 to pred(Dest.Width) do
            begin
              Color := Dest.Canvas.Pixels[col, row];
              with TRGBRec(Source.Canvas.Pixels[col, row]) do
                begin
                  TRGBRec(Color).R := round((R/255)*TRGBRec(Color).R);//(R + TRGBRec(Color).R) div 2;
                  TRGBRec(Color).G := round((G/255)*TRGBRec(Color).G);//(G + TRGBRec(Color).G) div 2;
                  TRGBRec(Color).B := round((B/255)*TRGBRec(Color).B);//(G + TRGBRec(Color).B) div 2;
                end;
              Dest.Canvas.Pixels[col, row] := Color;
            end;
          if Assigned(NotProc)
            then NotProc(nil, round(100*row/h));
        end;
      R := Rect(0, 0, Dest.Width, Dest.Height);
      InflateRect(R, -5, -5);
      Dest.Canvas.Brush.Style := bsClear;
      Dest.Canvas.Pen.Color   := $006A6A35;
      Dest.Canvas.Pen.Width   := 1;
      Dest.Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;

  procedure TPictureShopViewer.Paint;
    begin
      inherited;
      if fRepSource
        then
          begin
            SourceImage.Canvas.Brush.Style := bsSolid;
            SourceImage.Canvas.Brush.Color := $003C3C1E;
            SourceImage.Canvas.FillRect(Rect(0, 0, PhotoWidth, PhotoHeight));
            SourceImage.Canvas.CopyRect(Rect(0, 0, PhotoWidth, PhotoHeight), fTempImage.Canvas, fRect);
          end;
    end;

  procedure TPictureShopViewer.InvalidateImage;
    var
      R : TRect;
    begin
      fRepSource := true;
      SourceImage.Refresh;
      R := SourceImage.BoundsRect;
      InvalidateRect(Handle, @R, false);
    end;

  procedure TPictureShopViewer.Message_WM_NCHITTEST(var Message : TMessage);
    begin
      if hi(Message.LParam) > Marquee.Height
        then Message.Result := HTCAPTION
        else inherited;
    end;

  procedure TPictureShopViewer.DragImage(deltaX, deltaY : integer);
    var
      tmpWidth  : integer;
      tmpHeight : integer;
    begin
      tmpWidth  := fRect.Right - fRect.Left;
      if deltaX + fRect.Left < 0
        then
          begin
            fRect.Left  := 0;
            fRect.Right := tmpWidth;
          end
        else
          if deltaX + fRect.Right > fTempImage.Width
            then
              begin
                fRect.Right := fTempImage.Width;
                fRect.Left  := fRect.Right - tmpWidth;
              end
            else OffsetRect(fRect, deltaX, 0);

      tmpHeight := fRect.Bottom - fRect.Top;
      if deltaY + fRect.Top < 0
        then
          begin
            fRect.Top    := 0;
            fRect.Bottom := tmpHeight;
          end
        else
          if deltaY + fRect.Bottom > fTempImage.Height
            then
              begin
                fRect.Bottom := fTempImage.Height;
                fRect.Top    := fRect.Bottom - tmpHeight;
              end
            else OffsetRect(fRect, 0, deltaY);
    end;

  function TPictureShopViewer.CenterImage(var Image : TBitmap) : boolean;
    var
      w      : integer;
      h      : integer;
      S      : TRect;
      D      : TRect;
      tmpImg : TBitmap;
    begin
      w := max(PhotoWidth, fTempImage.Width);
      h := max(PhotoHeight, fTempImage.Height);
      if (fTempImage.Width < PhotoWidth) or (fTempImage.Height < PhotoHeight)
        then
          begin
            tmpImg := TBitmap.Create;
            tmpImg.Width  := w;
            tmpImg.Height := h;

            S := Rect(0, 0, Image.Width, Image.Height);
            D := Rect(0, 0, w, h);

            tmpImg.Canvas.Brush.Style := bsSolid;
            tmpImg.Canvas.Brush.Color := $003C3C1E;
            tmpImg.Canvas.FillRect(D);

            D := S;

            OffsetRect(D, max(0, (w - D.Right) div 2), max(0, (h - D.Bottom) div 2));
            tmpImg.Canvas.CopyRect(D, Image.Canvas, S);

            Image.Assign(tmpImg);

            tmpImg.Free;

            result := (fTempImage.Width > PhotoWidth) or (fTempImage.Height > PhotoHeight);
          end
        else result := true;
    end;

  procedure TPictureShopViewer.SetImage;
    begin
      fCanDrag := CenterImage(fTempImage);
      fRect := Rect(0, 0, PhotoWidth, PhotoHeight);
      OffsetRect(fRect, max(0, (fTempImage.Width - PhotoWidth) div 2), max(0, (fTempImage.Height - PhotoHeight) div 2));
      fUserImage.Free;
      fUserImage := TBitmap.Create;
      fUserImage.Assign(fTempImage);
      //DragImage(0, 0);
      InvalidateImage;
      SourcePanel.Cursor := crHandPoint;
      ZoomIn.Enabled := true;
      ZoomOut.Enabled := true;
    end;

  procedure TPictureShopViewer.LoadImage(path : string);
    var
      Stream : TStream;
      jpgImg : TJPEGImage;
    begin
      try
        Stream := TFileStream.Create(path, fmOpenRead);
        try
          if uppercase(ExtractFileExt(path)) = '.BMP'
            then fTempImage.LoadFromStream(Stream)
            else
              if uppercase(ExtractFileExt(path)) = '.JPG'
                then
                  begin
                    fTempImage.Free;
                    fTempImage := TBitmap.Create;
                    jpgImg := TJPEGImage.Create;
                    jpgImg.LoadFromStream(Stream);
                    fTempImage.Width  := jpgImg.Width;
                    fTempImage.Height := jpgImg.Height;
                    fTempImage.Canvas.Draw(0, 0, jpgImg);
                    jpgImg.Free;
                  end;
          SetImage;
          btnPreview.Enabled := true;
        finally
          Stream.Free;
        end;
      except
      end;
    end;

  procedure TPictureShopViewer.btnCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
      // close;
    end;

  procedure TPictureShopViewer.btnSendClick(Sender: TObject);
    var
      jpgImg : TJPEGImage;
    begin
      if btnSend.Text = GetLiteral('Literal49')
        then FinishSend(false, GetLiteral('Literal50'))
        else
          if not fBusy
            then
              begin
                fStream.Free;
                fBusy := true;
                jpgImg := TJPEGImage.Create;
                jpgImg.Assign(DestImage.Picture.Bitmap);
                fStream := TMemoryStream.Create;
                jpgImg.SaveToStream(fStream);
                jpgImg.Free;
                btnSend.Text := GetLiteral('Literal51');
                btnSend.Refresh;
                SendPicture;
                Marquee.Caption := GetLiteral('Literal52');
              end;
    end;

  procedure TPictureShopViewer.btnLoadClick(Sender: TObject);
    begin
      if not fBusy and OpenDialog.Execute
        then
          begin
            LoadImage(OpenDialog.FileName);
            fZoomLevel := 1;
          end;
    end;

  procedure TPictureShopViewer.FormCreate(Sender: TObject);
    begin
      fUserImage := TBitmap.Create;
      fTempImage := TBitmap.Create;
      fZoomLevel := 1;
      fCacheDir  := 'c:\work\five\release\cache\';
    end;

  procedure TPictureShopViewer.FormDestroy(Sender: TObject);
    begin
      fUserImage.Free;
      fTempImage.Free;
      if fSocket <> nil
        then fSocket.Close;
      fSocket.Free;
      fStream.Free;
    end;

  procedure TPictureShopViewer.btnPreviewClick(Sender: TObject);
    var
      Vignette : TBitmap;
      Stream   : TStream;
    begin
      try
        if not fBusy
          then
            begin
              fBusy := true;
              Vignette := TBitmap.Create;
              try
                Stream := TFileStream.Create(fCacheDir + 'OtherImages\photomask.bmp', fmOpenRead);
                try
                  try
                    Vignette.LoadFromStream(Stream);
                  finally
                    Stream.Free;
                  end;
                except
                  Vignette.Width  := PhotoWidth;
                  Vignette.Height := PhotoHeight;
                end;

                MixImages(Vignette, SourceImage.Picture.Bitmap, NotifyPerc);

                DestImage.Canvas.CopyRect(DestImage.ClientRect, Vignette.Canvas, DestImage.ClientRect);
                DestImage.Refresh;
                btnSend.Enabled := true;
                Vignette.Free;

                Marquee.Caption := GetLiteral('Literal53');
              except
              end;
              fBusy := false;
            end;
        except
        end;
    end;

  procedure TPictureShopViewer.SourcePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if fCanDrag
        then
          begin
            fLastX := X;
            fLastY := Y;
          end;
    end;

  procedure TPictureShopViewer.SourcePanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      if fCanDrag and (ssLeft in Shift)
        then
          begin
            DragImage(fLastX - X, fLastY - Y);
            InvalidateImage;
            fLastX := X;
            fLastY := Y;
          end;
    end;

  procedure TPictureShopViewer.ZoomInClick(Sender: TObject);
    begin
      if not fBusy
        then
          begin
            if fZoomLevel > 1
              then
                begin
                  fBusy := true;
                  dec(fZoomLevel);
                  ZoomBitmap(fUserImage, fTempImage, fZoomLevel, NotifyPerc);
                  fCanDrag := CenterImage(fTempImage);
                  //DestImage.Picture.Bitmap.Assign(fTempImage);
                  DragImage(0, 0);
                  InvalidateImage;
                  fBusy := false;
                end;
          end;
    end;

  procedure TPictureShopViewer.ZoomOutClick(Sender: TObject);
    begin
      if not fBusy
        then
          begin
            if fZoomLevel < 5
              then
                begin
                  fBusy := true;
                  inc(fZoomLevel);
                  ZoomBitmap(fUserImage, fTempImage, fZoomLevel, NotifyPerc);
                  fCanDrag := CenterImage(fTempImage);
                  //DestImage.Picture.Bitmap.Assign(fTempImage);
                  DragImage(0, 0);
                  InvalidateImage;
                  fBusy := false;
                end;
          end;
    end;

  procedure TPictureShopViewer.TimerTimer(Sender: TObject);
    begin
      Marquee.Tick;
    end;

  procedure TPictureShopViewer.NotifyPerc(Sender : TObject; perc : integer);
    begin
      if fProgress <> perc
        then
          begin
            fProgress := perc;
            Marquee.Caption := GetLiteral('Literal54') + IntToStr(perc) + '%';
            Application.ProcessMessages;
          end;
    end;

  procedure TPictureShopViewer.SocketConnect(Sender : TObject; Socket : TCustomWinSocket);
    var
      logText : string;
    begin
      logText := 'User=' + fUserName + ^M^J + 'World=' + fWorldName + ^M^J + 'Size=' + IntToStr(fStream.Size);
      Socket.SendText(logText);
    end;

  procedure TPictureShopViewer.SocketRead(Sender : TObject; Socket : TCustomWinSocket);
    var
      text : string;
    begin
      text := Socket.ReceiveText;
      if text = 'SEND'
        then
          begin
            fCanSndData := true;
            SocketWrite(Sender, Socket)
          end
        else
          if text = 'OK'
            then FinishSend(true, '')
            else
              if text = 'ERROR'
                then FinishSend(false, GetLiteral('Literal55'))
                else FinishSend(true, ''); // >> This should never happen...
    end;

  procedure TPictureShopViewer.SocketWrite(Sender : TObject; Socket : TCustomWinSocket);
    var
      buffer : array[0..BufferSize-1] of byte;
      count  : integer;
    begin
      if fCanSndData
        then
          begin
            fStream.Seek(0, 0);
            repeat
              count := fStream.Read(buffer, BufferSize);
              Socket.SendBuf(buffer, count);
            until count < BufferSize;
            Marquee.Caption := GetLiteral('Literal56');
          end;
    end;

  procedure TPictureShopViewer.HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
    begin
      case ErrorEvent of
        eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept :
          begin
            ErrorCode := 0;
            FinishSend(false, GetLiteral('Literal57'));
          end;
      end
    end;

  procedure TPictureShopViewer.FinishSend(success : boolean; msg : string);
    begin
      fCanSndData := false;
      if msg <> ''
        then Marquee.Caption := msg;
      if success
        then
          begin
            fSocket.Close;
            fBusy := false;
            btnSend.Text := GetLiteral('Literal58');
            ModalResult := mrOk;
          end
        else
          begin
            fSocket.Close;
            fBusy := false;
            btnSend.Text := GetLiteral('Literal59');
          end;
    end;

  procedure TPictureShopViewer.SendPicture;
    begin
      fCanSndData := false;
      fSocket.Free;
      fSocket := TClientSocket.Create(self);
      fSocket.OnConnect := SocketConnect;
      fSocket.OnRead    := SocketRead;
      fSocket.OnWrite   := SocketWrite;
      fSocket.Port      := PictureTransferPort;
      fSocket.OnError   := HandleError;
      if inet_addr(pchar(fServerAddr)) = u_long(INADDR_NONE)
        then fSocket.Host := fServerAddr
        else fSocket.Address := fServerAddr;
      fSocket.Open;
    end;

end.
