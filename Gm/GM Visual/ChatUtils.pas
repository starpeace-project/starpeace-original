unit ChatUtils;

interface

  uses
    Windows, Graphics, ComCtrls;

  procedure AddChatString( RichEdit : TRichEdit; Header, Body : string; HeaderColor : TColor; var CharCount : integer );
  procedure PosScrollBar( RichEdit : TRichEdit );

implementation

  uses
    Messages, Forms;

  procedure AddChatString( RichEdit : TRichEdit; Header, Body : string; HeaderColor : TColor; var CharCount : integer );
    const
      Carriage = #13#10;
    var
      s : string;
    begin
      s := Header + ' ' + Body;

      RichEdit.Lines.Add( s );

      RichEdit.SelStart            := CharCount;
      RichEdit.SelLength           := length(Header);
      RichEdit.SelAttributes.Color := HeaderColor;
      RichEdit.SelAttributes.Style := [fsBold];
      RichEdit.SelLength           := 0;

      inc( CharCount, length(s) + length(Carriage) );

      PosScrollBar( RichEdit );
    end;

  procedure PosScrollBar( RichEdit : TRichEdit );
    var
      ScrollInfo : TScrollInfo;
      P          : TPoint;
    begin
      P.x := 0;
      P.y := RichEdit.Lines.Count;
      SendMessage( RichEdit.Handle, EM_SCROLLCARET, 0, 0 );

      {
      ScrollInfo.cbSize := sizeof(ScrollInfo);
      ScrollInfo.fMask  := SIF_RANGE;

      GetScrollInfo( ScrollWnd, SB_VERT, ScrollInfo );

      SetScrollPos( ScrollWnd, SB_VERT, ScrollInfo.nMax, true );
      Application.ProcessMessages;
      }
    end;

end.
