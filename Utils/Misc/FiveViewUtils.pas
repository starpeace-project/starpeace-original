unit FiveViewUtils;

interface

  uses
    Classes, Controls;

  const
    xfPrefix    = 'xfer_';
    tidNonAvail = 'n/a';

  function  SetViewProp(View : TWinControl; Properties : TStringList) : boolean;
  procedure GetViewPropNames(View : TWinControl; Names : TStringList);

implementation

  uses
    Forms;
    
  procedure GetViewPropNames(View : TWinControl; Names : TStringList);
    var
      i        : integer;
      Control  : TControl;
      PropName : string;
    begin
      try
        for i := 0 to pred(View.ControlCount) do
          begin
            Application.ProcessMessages;
            Control := View.Controls[i];
            if pos(xfPrefix, Control.Name) = 1
              then
                begin
                  PropName := copy(Control.Name, length(xfPrefix) + 1, length(Control.Name));
                  Names.Add(PropName);
                end;
            if Control is TWinControl
              then GetViewPropNames(Control as TWinControl, Names);
          end;
      except
      end;
    end;

  function SetViewProp(View : TWinControl; Properties : TStringList) : boolean;
    var
      i        : integer;
      Control  : TControl;
      PropName : string;
      PropVal  : string;
    begin
      try
        for i := 0 to pred(View.ControlCount) do
          begin
            Application.ProcessMessages;
            Control := View.Controls[i];
            if pos(xfPrefix, Control.Name) = 1
              then
                begin
                  PropName := copy(Control.Name, length(xfPrefix) + 1, length(Control.Name));
                  try
                    PropVal  := Properties.Values[PropName];
                    if PropVal = ''
                      then PropVal := tidNonAvail;
                    Control.SetTextBuf(pchar(PropVal));
                  except
                  end;
                end;
            if Control is TWinControl
              then SetViewProp(Control as TWinControl, Properties);
          end;
        result := true;
      except
        result := false;
      end;
    end;

end.
