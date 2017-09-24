unit ChatRenderer;

interface

  uses
    Classes, SysUtils, Windows, Graphics, ExtCtrls, CanvasBMP, SpeedBmp, SpriteImages, GameControl;

  type
    TChatRenderer = class;

    TDecodeCodeMSGChat = function (const MSGChat: string): string of object;  

    TChatRendererImageLocator = function ( ImgId : string ) : TGraphic of object;
    TChatRendererNotifyUpdate = procedure ( Sender : TChatRenderer ) of object;

    TChatRenderer =
      class
        public
          constructor Create( aMaxLines, aAvgLineHeight, aLineTimeOut : integer; aKeyColor : TColor; aImgLocator : TChatRendererImageLocator; aNotifyUpdate : TChatRendererNotifyUpdate );
          destructor  Destroy; override;
        private
          fBuffer        : TSpeedBitmap;
          fFrameImage    : TFrameImage;
          fKeyColor      : TColor;
          fLines         : TStringList;
          fMaxLines      : integer;
          fAvgLineHeight : integer;
          fLineTimeOut   : integer;
          fLineOfs       : integer;
          fShowLast      : boolean;
          fImgLocator    : TChatRendererImageLocator;
          fNotifyUpdate  : TChatRendererNotifyUpdate;
          fBottomVisible : boolean;
          fDefaultConfig : string;
          fNoBorder      : boolean;
          fOpaque        : boolean;
          fBackColor     : TColor;
          fTimer         : TTimer;
          fTextWidth     : integer;
          fTextHeight    : integer;
        public
          property Buffer        : TSpeedBitmap read fBuffer;
          property FrameImage    : TFrameImage  read fFrameImage;
          property DefaultConfig : string          read fDefaultConfig write fDefaultConfig;
          property NoBorder      : boolean         read fNoBorder      write fNoBorder;
          property Opaque        : boolean         read fOpaque        write fOpaque;
          property BackColor     : TColor          read fBackColor     write fBackColor;
          property TextWidth     : integer         read fTextWidth;
          property TextHeight    : integer         read fTextHeight;
          property KeyColor      : TColor          read fKeyColor;
        published
          property ImgLocator    : TChatRendererImageLocator write fImgLocator;
          property NotifyUpdate  : TChatRendererNotifyUpdate write fNotifyUpdate;
        public
          procedure SetSize( aWidth, aHeight : integer );
          procedure AddLine( Line : string );
          function  Scroll( lineDelta : integer ) : boolean;
          procedure ShowLast;
        protected
          procedure Render;
        public
          procedure RenderToCanvas( Canvas : TCanvas; ClipRect : TRect );
          procedure RenderToBMPCanvas( Canvas : TBufferCanvas; ClipRect : TRect; paint : boolean );
        private
          procedure TimerTick( Sender : TObject );
          function  TimeInScreenFor( text : string ) : integer;
      end;

implementation

  uses
    mr_StrUtils, MathUtils, Palettes, Forms, ColorTableMgr, Gdi, BitBlt;

  procedure RenderTextToCanvas( x, y : integer; ClipRect : TRect; TheCanvas : TCanvas; TheText : string; Width : integer; var LineHeight : integer; BorderColor : TColor; DrawBorder : boolean; var PixelWidth, PixelHeight : integer; ImageLocator : TChatRendererImageLocator; Paint : boolean ); forward;
  procedure RenderTextToBMPCanvas( x, y : integer; ClipRect : TRect; TheCanvas : TBufferCanvas; TheText : string; Width : integer; var LineHeight : integer; BorderColor : TColor; DrawBorder : boolean; var PixelWidth, PixelHeight : integer; ImageLocator : TChatRendererImageLocator; Paint : boolean ); forward;

  constructor TChatRenderer.Create( aMaxLines, aAvgLineHeight, aLineTimeOut : integer; aKeyColor : TColor; aImgLocator : TChatRendererImageLocator; aNotifyUpdate : TChatRendererNotifyUpdate );
    begin
      inherited Create;
      fMaxLines      := aMaxLines;
      fAvgLineHeight := aAvgLineHeight;
      fLineTimeOut   := aLineTimeOut;
      fKeyColor      := aKeyColor;
      fImgLocator    := aImgLocator;
      fNotifyUpdate  := aNotifyUpdate;
      fLines         := TStringList.Create;
      fLineOfs       := 0;
      fShowLast      := true;
      if assigned(fNotifyUpdate)
        then
          begin
            if fLineTimeOut = 0
              then fLineTimeOut := 4000;
            fTimer := TTimer.Create( nil );
            fTimer.Enabled := false;
            fTimer.OnTimer := TimerTick;
          end;
    end;

  destructor TChatRenderer.Destroy;
    begin
      fTimer.Free;
      fBuffer.Free;
      fLines.Free;
      inherited;
    end;

  procedure TChatRenderer.SetSize( aWidth, aHeight : integer );
    begin
      if not (((fBuffer<>nil) and (fBuffer.Width=aWidth) and (fBuffer.Height=aHeight)))
        then
          begin
            fBuffer.Free;
            fBuffer := TSpeedBitmap.CreateSized( aWidth, aHeight, cCanvasBitCount );
            Render;
          end;
    end;

  procedure TChatRenderer.AddLine( Line : string );
    begin
      while fLines.Count > fMaxLines do
        begin
          fLines.Delete( 0 );
          if (fLineOfs>0)
            then
              if ((buffer<>nil) and (TextHeight<buffer.ClientRect.Bottom - 20)) or (buffer=nil)
                  then dec(fLineOfs);
        end;
      fLines.Add( DefaultConfig + Line );
      if Buffer<>nil
        then
          begin
            repeat
              RenderToBMPCanvas( Buffer.Canvas, buffer.ClientRect, false);
              if (TextHeight>Buffer.ClientRect.Bottom-20)
                then inc(fLineOfs);
            until TextHeight<=Buffer.ClientRect.Bottom;
            render;
          end;
      if (fTimer <> nil) and not fTimer.Enabled
        then
          begin
            fTimer.Interval := TimeInScreenFor( fLines[fLineOfs] );
            fTimer.Enabled := true;
          end;
    end;

  procedure TChatRenderer.Render;
    begin
      if fBuffer <> nil
        then
          begin
            fBuffer.Canvas.Brush.Color := fKeyColor;
            fBuffer.Canvas.FillRect(fBuffer.ClientRect);
            RenderToBMPCanvas(fBuffer.Canvas, fBuffer.ClientRect, true);
          end;
    end;

  procedure TChatRenderer.RenderToCanvas( Canvas : TCanvas; ClipRect : TRect );

    procedure CreateTextBuffer( x, y : integer; shadow, paint : boolean );
      var
        lineofs    : integer;
        ofsY       : integer;
        clipR      : TRect;
        i          : integer;
        lineHeight : integer;
        pixHeight  : integer;
      begin
        lineofs := fLineOfs;
        ofsY    := ClipRect.Top;
        clipR   := ClipRect;
        fTextHeight := 0;
        fTextWidth  := 0;
        pixHeight   := 0;
        Canvas.Brush.Style := bsClear;
        i := lineofs;
        while (i<=pred(fLines.Count)) and (ofsY<ClipRect.Bottom) do
        //for i := lineofs to pred(fLines.Count) do
          begin
            lineHeight := 0;
            RenderTextToCanvas( x, y + ofsY, clipR, Canvas, fLines[i], ClipRect.Right - ClipRect.Left + x, lineHeight, clBlack, shadow, fTextWidth, pixHeight, fImgLocator, paint );
            inc( ofsY, pixHeight );
            inc( fTextHeight, pixHeight );
            inc(i);
          end;
        fBottomVisible := ofsY + pixHeight < ClipRect.Bottom - ClipRect.Top;
        if fShowLast and (ofsY + pixHeight div 2 > ClipRect.Bottom - ClipRect.Top) and (fLineOfs < pred(fLines.Count))
          then
            begin
              inc( fLineOfs );
              Canvas.Brush.Style := bsSolid;
              if fOpaque
                then
                  begin
                    Canvas.Brush.Color := fBackColor;
                    Canvas.Pen.Color   := fBackColor;
                  end
                else
                  begin
                    Canvas.Brush.Color := fKeyColor;
                    Canvas.Pen.Color   := fKeyColor;
                  end;
              if fOpaque and paint // (fBuffer <> nil) or 
                then Canvas.Rectangle( ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom );
              CreateTextBuffer( x, y, shadow, paint );
            end;
        inc( fTextHeight, pixHeight );
      end;

    begin
      Canvas.Brush.Style := bsSolid;
      if fOpaque
        then
          begin
            Canvas.Brush.Color := fBackColor;
            Canvas.Pen.Color   := fBackColor;
          end
        else
          begin
            Canvas.Brush.Color := fKeyColor;
            Canvas.Pen.Color   := fKeyColor;
          end;
      if (fOpaque) // (fBuffer <> nil) or 
        then Canvas.Rectangle( ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom );
      CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 1, false, false );
      if not fNoBorder
        then
          begin
            CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 0, true, true );
            CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 2, true, true );
            CreateTextBuffer( ClipRect.Left + 0, ClipRect.Top + 1, true, true );
            CreateTextBuffer( ClipRect.Left + 2, ClipRect.Top + 1, true, true );
          end;
      CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 1, false, true );
    end;

  procedure TChatRenderer.RenderToBMPCanvas( Canvas : TBufferCanvas; ClipRect : TRect; paint : boolean );

    procedure CreateTextBuffer( x, y : integer; shadow, paint : boolean );
      var
        lineofs    : integer;
        ofsY       : integer;
        clipR      : TRect;
        i          : integer;
        lineHeight : integer;
        pixHeight  : integer;
      begin
        lineofs := fLineOfs;
        ofsY    := ClipRect.Top;
        clipR   := ClipRect;
        fTextHeight := 0;
        fTextWidth  := 0;
        pixHeight   := 0;
        Canvas.Brush.Style := bsClear;
        for i := lineofs to pred(fLines.Count) do
          begin
            lineHeight := 0;
            RenderTextToBMPCanvas( x, y + ofsY, clipR, Canvas, fLines[i], ClipRect.Right - ClipRect.Left + x, lineHeight, clBlack, shadow, fTextWidth, pixHeight, fImgLocator, paint );
            inc( ofsY, pixHeight );
            inc( fTextHeight, pixHeight );
          end;
        fBottomVisible := ofsY + pixHeight < ClipRect.Bottom - ClipRect.Top;
        if fShowLast and (ofsY + pixHeight div 2 > ClipRect.Bottom - ClipRect.Top) and (fLineOfs < pred(fLines.Count))
          then
            begin
               inc( fLineOfs );
              Canvas.Brush.Style := bsSolid;
              if fOpaque
                then
                  begin
                    Canvas.Brush.Color := fBackColor;
                    Canvas.Pen.Color   := fBackColor;
                  end
                else
                  begin
                    Canvas.Brush.Color := fKeyColor;
                    Canvas.Pen.Color   := fKeyColor;
                  end;
              if (fOpaque) and paint // ((fBuffer <> nil) or 
                then Canvas.Rectangle( ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom );
              CreateTextBuffer( x, y, shadow, paint );
            end;
        inc( fTextHeight, pixHeight );
      end;

    begin
      Canvas.Brush.Style := bsSolid;
      if fOpaque
        then
          begin
            Canvas.Brush.Color := fBackColor;
            Canvas.Pen.Color   := fBackColor;
          end
        else
          begin
            Canvas.Brush.Color := fKeyColor;
            Canvas.Pen.Color   := fKeyColor;
          end;
      if (fOpaque) // (fBuffer <> nil) or 
        then Canvas.Rectangle( ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom );
      CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 1, false, false );
      if paint
        then
          begin
            if not fNoBorder
              then
                begin
                  CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 0, true, true );
                  CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 2, true, true );
                  CreateTextBuffer( ClipRect.Left + 0, ClipRect.Top + 1, true, true );
                  CreateTextBuffer( ClipRect.Left + 2, ClipRect.Top + 1, true, true );
                end;
            CreateTextBuffer( ClipRect.Left + 1, ClipRect.Top + 1, false, true );
          end;
    end;

  function TChatRenderer.Scroll( lineDelta : integer ) : boolean;
    var
      initialOfs : integer;
    begin
      initialOfs := fLineOfs;
      if (linedelta > 0) and fBottomVisible
        then linedelta := 0;
      if (fLineOfs + lineDelta < 0) or (fLineOfs + lineDelta >= fLines.Count)
        then
          if linedelta < 0
            then linedelta := -fLineOfs
            else linedelta := fLines.Count - fLineOfs;
      inc( fLineOfs, lineDelta );
      fShowLast := false;
      Render;
      result := initialOfs <> fLineOfs;
    end;

  procedure TChatRenderer.ShowLast;
    begin
      fShowLast := true;
    end;

  procedure TChatRenderer.TimerTick( Sender : TObject );
    begin
      if fLines.Count > 0
        then fLines.Delete( 0 );
      if (fLineOfs > 0) and (fLineOfs>=fLines.Count)
        then fLineOfs := pred(fLines.Count);
      if (fLines.Count > 0)
        then fTimer.Interval := TimeInScreenFor(fLines[fLineOfs])
        else fTimer.Enabled  := false;
      Render;
      if assigned(fNotifyUpdate)
        then fNotifyUpdate( self );
    end;

  function TChatRenderer.TimeInScreenFor( text : string ) : integer;
    begin
      result := fLineTimeOut + 40*length(text);
    end;

  // util functions
  procedure RenderTextToCanvas( x, y : integer; ClipRect : TRect; TheCanvas : TCanvas; TheText : string; Width : integer; var LineHeight : integer; BorderColor : TColor; DrawBorder : boolean; var PixelWidth, PixelHeight : integer; ImageLocator : TChatRendererImageLocator; Paint : boolean );
    const
      OutText      : string = '';
      cSeparators   = [' ', '-', #10, #13];
      cCommandOpen  = '<';
      cCommandClose = '>';
      cStopChars    = cSeparators + [cCommandOpen];
    const
      cCmdColor = 'c';
      cCmdImage = 'i';
      cCmdStyle = 's';
      cCmdSize  = 'z';
      cCmdFace  = 'f';
      cCmdBR    = 'k';
    var
      text      : string;
      nextword  : string;
      separator : char;
      command   : char;
      cmdLine   : string;
      ofs       : TPoint;
      size      : TSize;
      maxheight : integer;
      p, sp     : integer;
      Img       : TGraphic;
      nextstyle : string;
      newLineHeight : integer;
      aTemp     : integer;
    begin
      ofs.x     := x;
      ofs.y     := y;
      maxheight := LineHeight;
      newLineHeight := 0;
      PixelHeight   := 0;
      TheCanvas.Font.Color := BorderColor;
      text := TheText;
      while text <> '' do
        begin
          p := 1;
          nextword := GetNextString( text, p, cStopChars );
          separator := text[p];
          case separator of
            ' ', '-' :
              begin
                nextword := nextword + separator;
              end;
          end;
          size := TheCanvas.TextExtent( nextword );
          if ofs.x + size.cx > Width
            then
              begin
                ofs.x := x;
                inc( ofs.y, maxheight );
                inc( PixelHeight, maxheight );
                maxheight := max( maxheight, size.cy );
              end
            else
              if size.cy > maxheight
                then maxheight := size.cy;
          newLineHeight := max( newLineHeight, maxheight );
          if (LineHeight > 0) and (nextword <> '') and paint
            then ExtTextOut( TheCanvas.Handle, ofs.x, ofs.y + LineHeight - size.cy, 0, @ClipRect, pchar(nextword), length(nextword), nil );
            //TheCanvas.TextRect( ClipRect, ofs.x, ofs.y + LineHeight - size.cy, nextword );
            //TheCanvas.TextOut( ofs.x, ofs.y + LineHeight - size.cy, nextword );
          //TheCanvas.TextOut( ofs.x, ofs.y, nextword );
          inc( ofs.x, size.cx );
          PixelWidth := max( PixelWidth, ofs.x );
          case separator of
            cCommandOpen :
              begin
                inc( p );
                command := text[p];
                inc( p );
                cmdLine := Trim( GetNextString( text, p, [cCommandClose] ));
                case command of
                  cCmdColor :
                    if not DrawBorder
                      then
                        begin
                          aTemp := BorderColor;
                          if cmdLine <> 'def'
                            then
                              try
                                aTemp := TColor(StrToInt(cmdLine));
                              except
                                if not IdentToColor('cl'+cmdLine, integer(aTemp))
                                  then aTemp := BorderColor;
                              end;
                          TheCanvas.Font.Color := aTemp;
                        end;
                  cCmdSize :
                    try
                      aTemp := TColor(StrToInt(cmdLine));
                      if aTemp<18
                        then TheCanvas.Font.Size := aTemp;
                    except
                    end;
                  cCmdFace :
                    try
                      TheCanvas.Font.Name := cmdLine;
                    except
                    end;
                  cCmdBR :
                    begin
                      ofs.x := x;
                      inc( ofs.y, LineHeight );
                      inc( PixelHeight, LineHeight );
                      maxheight := 0;
                    end;
                  cCmdStyle :
                    try
                      sp := 1;
                      repeat
                        nextstyle := GetNextString( cmdLine, sp, [' ', cCommandClose] );
                        inc( sp );
                        if nextstyle = 'bold'
                          then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsBold]
                          else
                            if nextstyle = '/bold'
                              then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsBold]
                              else
                                if nextstyle = 'ital'
                                  then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsItalic]
                                  else
                                    if nextstyle = '/ital'
                                      then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsItalic]
                                      else
                                        if nextstyle = 'und'
                                          then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsUnderline]
                                          else
                                            if nextstyle = '/und'
                                              then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsUnderline];
                      until nextstyle = '';
                    except
                    end;
                  cCmdImage :
                    try
                      Img := ImageLocator( cmdLine );
                      if Img <> nil
                        then
                          begin
                            if ofs.x + Img.width > Width
                              then
                                begin
                                  ofs.x := x;
                                  inc( ofs.y, maxheight );
                                  inc( PixelHeight, maxheight );
                                end;
                            if (LineHeight > 0) and not DrawBorder and paint
                              then TheCanvas.Draw( ofs.x, ofs.y + LineHeight - Img.Height, Img );
                            //TheCanvas.Draw( ofs.x, ofs.y, Img );
                            maxheight := max( maxheight, Img.height );
                            inc( ofs.x, Img.width );
                          end;
                    except
                    end;
                end;
              end;
          end;
          // here
          text := copy( text, p + 1, length(text) - p );
        end;
      inc( PixelHeight, maxheight );
      if (LineHeight = 0) and (newLineHeight > 0)
        then RenderTextToCanvas( x, y, ClipRect, TheCanvas, TheText, Width, newLineHeight, BorderColor, DrawBorder, PixelWidth, PixelHeight, ImageLocator, Paint );
    end;

 procedure RenderTextToBMPCanvas( x, y : integer; ClipRect : TRect; TheCanvas : TBufferCanvas; TheText : string; Width : integer; var LineHeight : integer; BorderColor : TColor; DrawBorder : boolean; var PixelWidth, PixelHeight : integer; ImageLocator : TChatRendererImageLocator; Paint : boolean );
    const
      OutText      : string = '';
      cSeparators   = [' ', '-', #10, #13];
      cCommandOpen  = '<';
      cCommandClose = '>';
      cStopChars    = cSeparators + [cCommandOpen];
    const
      cCmdColor = 'c';
      cCmdImage = 'i';
      cCmdStyle = 's';
      cCmdSize  = 'z';
      cCmdFace  = 'f';
      cCmdBR    = 'k';
    var
      text      : string;
      nextword  : string;
      separator : char;
      command   : char;
      cmdLine   : string;
      ofs       : TPoint;
      size      : TSize;
      maxheight : integer;
      p, sp     : integer;
      Img       : TGraphic;
      nextstyle : string;
      newLineHeight : integer;
      aTemp     : integer;
    begin
      ofs.x     := x;
      ofs.y     := y;
      maxheight := LineHeight;
      newLineHeight := 0;
      PixelHeight   := 0;
      TheCanvas.Font.Color := BorderColor;
//      SetBkMode (TheCanvas.Handle, TRANSPARENT);
      text := TheText;
      while text <> '' do
        begin
          p := 1;
          nextword := GetNextString( text, p, cStopChars );
          separator := text[p];
          case separator of
            ' ', '-' :
              begin
                nextword := nextword + separator;
              end;
          end;
          size := TheCanvas.TextExtent( nextword );
          if ofs.x + size.cx > Width
            then
              begin
                ofs.x := x;
                inc( ofs.y, maxheight );
                inc( PixelHeight, maxheight );
                maxheight := max( maxheight, size.cy );
              end
            else
              if size.cy > maxheight
                then maxheight := size.cy;
          newLineHeight := max( newLineHeight, maxheight );
          if (LineHeight > 0) and (nextword <> '') and paint
            then ExtTextOut( TheCanvas.Handle, ofs.x, ofs.y + LineHeight - size.cy, 0, @ClipRect, pchar(nextword), length(nextword), nil );
            //TheCanvas.TextRect( ClipRect, ofs.x, ofs.y + LineHeight - size.cy, nextword );
            //TheCanvas.TextOut( ofs.x, ofs.y + LineHeight - size.cy, nextword );
          //TheCanvas.TextOut( ofs.x, ofs.y, nextword );
          inc( ofs.x, size.cx );
          PixelWidth := max( PixelWidth, ofs.x );
          case separator of
            cCommandOpen :
              begin
                inc( p );
                command := text[p];
                inc( p );
                cmdLine := Trim( GetNextString( text, p, [cCommandClose] ));
                case command of
                  cCmdColor :
                    if not DrawBorder
                      then
                        begin
                          aTemp := BorderColor;
                          if cmdLine <> 'def'
                            then
                              try
                                aTemp := TColor(StrToInt(cmdLine));
                              except
                                if not IdentToColor('cl'+cmdLine, integer(aTemp))
                                  then aTemp := BorderColor;
                              end;
                          TheCanvas.Font.Color := aTemp;
                        end;
                  cCmdSize :
                    try
                      aTemp := TColor(StrToInt(cmdLine));
                      if aTemp<18
                        then TheCanvas.Font.Size := aTemp;
                    except
                    end;
                  cCmdFace :
                    try
                      TheCanvas.Font.Name := cmdLine;
                    except
                    end;
                  cCmdBR :
                    begin
                      ofs.x := x;
                      inc( ofs.y, LineHeight );
                      inc( PixelHeight, LineHeight );
                      maxheight := 0;
                    end;
                  cCmdStyle :
                    try
                      sp := 1;
                      repeat
                        nextstyle := GetNextString( cmdLine, sp, [' ', cCommandClose] );
                        inc( sp );
                        if nextstyle = 'bold'
                          then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsBold]
                          else
                            if nextstyle = '/bold'
                              then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsBold]
                              else
                                if nextstyle = 'ital'
                                  then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsItalic]
                                  else
                                    if nextstyle = '/ital'
                                      then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsItalic]
                                      else
                                        if nextstyle = 'und'
                                          then TheCanvas.Font.Style := TheCanvas.Font.Style + [fsUnderline]
                                          else
                                            if nextstyle = '/und'
                                              then TheCanvas.Font.Style := TheCanvas.Font.Style - [fsUnderline];
                      until nextstyle = '';
                    except
                    end;
                  cCmdImage :
                    try
                      Img := ImageLocator( cmdLine );
                      if Img <> nil
                        then
                          begin
                            if ofs.x + Img.width > Width
                              then
                                begin
                                  ofs.x := x;
                                  inc( ofs.y, maxheight );
                                  inc( PixelHeight, maxheight );
                                end;
                            if (LineHeight > 0) and not DrawBorder and paint
                              then TheCanvas.Draw( ofs.x, ofs.y + LineHeight - Img.Height, Img );
                            //TheCanvas.Draw( ofs.x, ofs.y, Img );
                            maxheight := max( maxheight, Img.height );
                            inc( ofs.x, Img.width );
                            PixelWidth := max( PixelWidth, ofs.x );
                          end;
                    except
                    end;
                end;
              end;
          end;
          // here
          text := copy( text, p + 1, length(text) - p );
        end;
      inc( PixelHeight, maxheight );
      if (LineHeight = 0) and (newLineHeight > 0)
        then RenderTextToBMPCanvas( x, y, ClipRect, TheCanvas, TheText, Width, newLineHeight, BorderColor, DrawBorder, PixelWidth, PixelHeight, ImageLocator, Paint );
    end;

end.
