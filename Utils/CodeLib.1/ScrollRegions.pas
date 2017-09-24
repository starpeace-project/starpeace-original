unit ScrollRegions;

interface

uses
  Windows;

function GetScrollUpdateRegion(const target : HWND; dx, dy : integer) : HRGN;


implementation

function GetOverlappedRegion(const which : HWND; dx, dy : integer) : HRGN;   forward;

function GetScrollUpdateRegion(const target : HWND; dx, dy : integer) : HRGN;
  var
    ClientRect : TRect;
    aux        : HRGN;
  begin
    if (target <> 0) and IsWindowVisible(target) and ((dx <> 0) or (dy <> 0))
      then
        begin
          GetClientRect(target, ClientRect);
          if dx <> 0
            then
              if dx < 0
                then aux := CreateRectRgn(ClientRect.Right + dx, 0, ClientRect.Right, ClientRect.Bottom)
                else aux := CreateRectRgn(ClientRect.Left, ClientRect.Top, ClientRect.Left + dx, ClientRect.Bottom)
            else aux := CreateRectRgn(0, 0, 0, 0);
          if dy <> 0
            then
              begin
                if dy < 0
                  then Result := CreateRectRgn(0, ClientRect.Bottom + dy, ClientRect.Right, ClientRect.Bottom)
                  else Result := CreateRectRgn(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + dy);
                CombineRgn(Result, aux, Result, RGN_OR);
                DeleteObject(aux);
              end
            else Result := aux;
          aux := GetOverlappedRegion(target, dx, dy);
          if aux <> 0
            then
              begin
                CombineRgn(Result, Result, aux, RGN_OR);
                DeleteObject(aux);
              end;
        end
      else Result := 0;
  end;

// utils implementations

function GetOverlappedRegion(const which : HWND; dx, dy : integer) : HRGN;
  var
    BaseRect : TRect;

  procedure GetRegion(const current : HWND);
    var
      prev    : HWND;
      CurRect : TRect;
      r1, r2  : HRGN;
    begin
      prev := current;
      repeat
        prev := GetWindow(prev, GW_HWNDPREV);
        if (prev <> 0) and IsWindowVisible(prev)
          then
            begin
              GetWindowRect(prev, CurRect);
              ScreenToClient(which, CurRect.TopLeft);
              ScreenToClient(which, CurRect.BottomRight);
              r1 := CreateRectRgnIndirect(CurRect);
              if dx < 0
                then inc(CurRect.Left, dx)
                else inc(CurRect.Right, dx);
              if dy < 0
                then inc(CurRect.Top, dy)
                else inc(CurRect.Bottom, dy);
              r2 := CreateRectRgnIndirect(CurRect);
              CombineRgn(r1, r2, r1, RGN_DIFF);
              DeleteObject(r2);
              r2 := CreateRectRgnIndirect(BaseRect);
              CombineRgn(r1, r2, r1, RGN_AND);
              DeleteObject(r2);
              CombineRgn(Result, Result, r1, RGN_OR);
              DeleteObject(r1);
            end;
      until prev = 0;
      prev := GetParent(current);
      if prev <> 0
        then GetRegion(prev);
    end;

  begin
    GetClientRect(which, BaseRect);
    Result := CreateRectRgn(0, 0, 0, 0);
    GetRegion(which);
  end;

end.
