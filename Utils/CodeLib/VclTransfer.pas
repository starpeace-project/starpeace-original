unit VclTransfer;

interface

uses
  Controls, Forms, CoreTypes, Transfer;

type
  TVclPropertyStorage =
    class(TInterfacedObject, IPropertyStorage)
      public
        constructor Create(ctrl : TWinControl; lineal : boolean);
      private // IPropertyStorage
        function GetClass : string;
        function GetName : string;
        function SetName(const which : string) : boolean;
        function GetProperty(const name : string) : string;
        function IPropertyStorage.SetProperty = CreateProperty;
        function CreateProperty(const name, value : string) : boolean;
        function EnumProperties : IEnumNames;
        function OpenStorage(const name : string) : IPropertyStorage;
        function CreateStorage(const theClass, name : string) : IPropertyStorage;
        function EnumChildren : IEnumNames;
      private
        fControl : TWinControl;
        fLineal  : boolean;
        function FindProperty(const name : string) : TControl;
        function FindStorage(const name : string) : TWinControl;
    end;


implementation


uses
  SysUtils, Classes;


type
  TLinealCtrlProperties =
    class(TInterfacedObject, IEnumNames)
      public
        constructor Create(ctrl : TWinControl);
      private
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fControl : TControl;
        fCurrent : integer;
    end;

type
  TCtrlProperties =
    class(TInterfacedObject, IEnumNames)
      public
        constructor Create(ctrl : TWinControl);
      private
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fControl : TWinControl;
        fCurrent : integer;
    end;

type
  TCtrlStorages =
    class(TInterfacedObject, IEnumNames)
      public
        constructor Create(ctrl : TWinControl);
      private
        function  Next(out which : array of string) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      private
        fControl : TWinControl;
        fCurrent : integer;
    end;


// Utils

function FindLinearProperty(Container : TWinControl; const name : string) : TControl;
  var
    i   : integer;
    aux : TComponent;
  begin
    Result := nil;
    i := Container.ComponentCount - 1;
    if i >= 0
      then
        repeat
          aux := Container.Components[i];
          if aux is TControl
            then
              if CompareText(aux.Name, name) = 0
                then Result := TControl(aux)
                else
                  if aux is TWinControl
                    then Result := FindLinearProperty(TWinControl(aux), name);
          dec(i);
        until (i < 0) or (Result <> nil);
  end;

function FindLocalProperty(Container : TWinControl; const name : string) : TControl;
  var
    i   : integer;
    aux : TControl;
  begin
    Result := nil;
    i := Container.ControlCount - 1;
    if i >= 0
      then
        repeat
          aux := Container.Controls[i];
          if CompareText(aux.Name, name) = 0
            then Result := TControl(aux);
          dec(i);
        until (i < 0) or (Result <> nil);
  end;

function GetProperStorage(var which : TControl) : boolean;
  begin
    while (which.Name = '') and (which is TWinControl) and (TWinControl(which).ControlCount = 1) do
      which := TWinControl(which).Controls[0];
    Result := (which.Name <> '') and (which is TWinControl) and (TWinControl(which).ControlCount > 0);
  end;


// TVclPropertyStorage

constructor TVclPropertyStorage.Create(ctrl : TWinControl; lineal : boolean);
  begin
    inherited Create;
    fControl := ctrl;
    fLineal  := lineal;
  end;

function TVclPropertyStorage.GetClass : string;
  begin
    Result := '';
  end;

function TVclPropertyStorage.GetName : string;
  begin
    Result := fControl.Name;
  end;

function TVclPropertyStorage.SetName(const which : string) : boolean;
  begin
    fControl.Name := which;
    Result := true;
  end;

function TVclPropertyStorage.GetProperty(const name : string) : string;
  var
    ctrl : TControl;
    len  : integer;
  begin
    ctrl := FindProperty(name);
    if ctrl <> nil
      then
        begin
          len := ctrl.GetTextLen;
          if len > 0
            then
              begin
                SetLength(Result, len);
                ctrl.GetTextBuf(pchar(Result), len + 1);
              end
            else Result := '';
        end
      else Result := '';
  end;

function TVclPropertyStorage.CreateProperty(const name, value : string) : boolean;
  var
    ctrl : TControl;
  begin
    ctrl := FindProperty(name);
    Result := (ctrl <> nil);
    if Result
      then ctrl.SetTextBuf(pchar(value));
  end;

function TVclPropertyStorage.EnumProperties : IEnumStrings;
  begin
    if fLineal
      then Result := TLinealCtrlProperties.Create(fControl)
      else Result := TCtrlProperties.Create(fControl);
  end;

function TVclPropertyStorage.OpenStorage(const name : string) : IPropertyStorage;
  var
    ctrl : TWinControl;
  begin
    if fLineal
      then Result := Self
      else
        begin
          ctrl := FindStorage(name);
          if ctrl <> nil
            then Result := TVclPropertyStorage.Create(TWinControl(ctrl), false)
            else Result := nil;
        end;
  end;

function TVclPropertyStorage.CreateStorage(const theClass, name : string) : IPropertyStorage;
  begin
    Result := OpenStorage(name);
  end;

function TVclPropertyStorage.EnumChildren : IEnumStrings;
  begin
    if fLineal
      then Result := nil
      else Result := TCtrlStorages.Create(fControl)
  end;

function TVclPropertyStorage.FindProperty(const name : string) : TControl;
  begin
    if fLineal
      then Result := FindLinearProperty(fControl, name)
      else Result := FindLocalProperty(fControl, name);
  end;

function TVclPropertyStorage.FindStorage(const name : string) : TWinControl;
  var
    aux : TControl;
    i   : integer;
  begin
    Result := nil;
    i := fControl.ControlCount - 1;
    if i >= 0
      then
        repeat
          aux := fControl.Controls[i];
          if GetProperStorage(aux) and (CompareText(aux.Name, name) = 0)
            then Result := TWinControl(aux);
          dec(i);
        until (i < 0) or (Result <> nil);
  end;


// TLinealCtrlProperties

constructor TLinealCtrlProperties.Create(ctrl : TWinControl);
  begin
    inherited Create;
    fControl := ctrl;
  end;

function TLinealCtrlProperties.Next(out which : array of string) : integer;
  var
    cc  : integer;
    aux : TComponent;
  begin
    cc := fControl.ComponentCount;
    Result := 0;
    while (fCurrent < cc) and (Result <= high(which)) do
      begin
        aux := fControl.Components[fCurrent];
        if (aux.Name <> '') and (aux is TControl)
          then
            begin
              which[Result] := aux.Name;
              inc(Result);
            end;
        inc(fCurrent);
      end;
  end;

function TLinealCtrlProperties.Skip(count : integer) : integer;
  var
    cc  : integer;
    aux : TComponent;
  begin
    cc := fControl.ComponentCount;
    Result := 0;
    while (fCurrent < cc) and (Result < count) do
      begin
        aux := fControl.Components[fCurrent];
        if (aux.Name <> '') and (aux is TControl)
          then inc(Result);
        inc(fCurrent);
      end;
  end;

procedure TLinealCtrlProperties.Reset;
  begin
    fCurrent := 0;
  end;


// TCtrlProperties

constructor TCtrlProperties.Create(ctrl : TWinControl);
  begin
    inherited Create;
    fControl := ctrl;
  end;

function TCtrlProperties.Next(out which : array of string) : integer;
  var
    cc  : integer;
    aux : string;
  begin
    cc := fControl.ControlCount;
    Result := 0;
    while (fCurrent < cc) and (Result <= high(which)) do
      begin
        aux := fControl.Controls[fCurrent].Name;
        if aux <> ''
          then
            begin
              which[Result] := aux;
              inc(Result);
            end;
        inc(fCurrent);
      end;
  end;

function TCtrlProperties.Skip(count : integer) : integer;
  var
    cc : integer;
  begin
    cc := fControl.ControlCount;
    Result := 0;
    while (fCurrent < cc) and (Result < count) do
      begin
        if fControl.Controls[fCurrent].Name <> ''
          then inc(Result);
        inc(fCurrent);
      end;
  end;

procedure TCtrlProperties.Reset;
  begin
    fCurrent := 0;
  end;


// TCtrlStorages

constructor TCtrlStorages.Create(ctrl : TWinControl);
  begin
    inherited Create;
    fControl := ctrl;
  end;

function TCtrlStorages.Next(out which : array of string) : integer;
  var
    cc  : integer;
    aux : TControl;
  begin
    cc := fControl.ControlCount;
    Result := 0;
    while (fCurrent < cc) and (Result <= high(which)) do
      begin
        aux := fControl.Controls[fCurrent];
        if GetProperStorage(aux)
          then
            begin
              which[Result] := aux.Name;
              inc(Result);
            end;
        inc(fCurrent);
      end;
  end;

function TCtrlStorages.Skip(count : integer) : integer;
  var
    cc  : integer;
    aux : TControl;
  begin
    cc := fControl.ControlCount;
    Result := 0;
    while (fCurrent < cc) and (Result < count) do
      begin
        aux := fControl.Controls[fCurrent];
        if GetProperStorage(aux)
          then inc(Result);
        inc(fCurrent);
      end;
  end;

procedure TCtrlStorages.Reset;
  begin
    fCurrent := 0;
  end;

end.

