
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1999 Inprise Corporation          }
{                                                       }
{*******************************************************}

unit Mtsobj;

{$H+,X+}

interface

uses Windows, ComObj, Mtx;

type
  { TMtsAutoObject }

  TMtsAutoObject = class(TAutoObject, IObjectControl)
  private
    FObjectContext: IObjectContext;
  protected
    { IObjectControl }
    procedure Activate; safecall;
    procedure Deactivate; stdcall;
    function CanBePooled: Bool; stdcall;

    procedure OnActivate; virtual;
    procedure OnDeactivate; virtual;
    property ObjectContext: IObjectContext read FObjectContext;
  public
    procedure SetComplete;
    procedure SetAbort;
    procedure EnableCommit;
    procedure DisableCommit;
    function IsInTransaction: Bool;
    function IsSecurityEnabled: Bool;
    function IsCallerInRole(const Role: WideString): Bool;
  end;

implementation

procedure TMtsAutoObject.Activate;
begin
  FObjectContext := GetObjectContext;
  OnActivate;
end;

procedure TMtsAutoObject.OnActivate;
begin
end;

procedure TMtsAutoObject.Deactivate;
begin
  OnDeactivate;
  FObjectContext := nil;
end;

procedure TMtsAutoObject.OnDeactivate;
begin
end;

function TMtsAutoObject.CanBePooled: Bool;
begin
  Result := False;
end;

procedure TMtsAutoObject.SetComplete;
begin
  if Assigned(FObjectContext) then FObjectContext.SetComplete;
end;

procedure TMtsAutoObject.SetAbort;
begin
  if Assigned(FObjectContext) then FObjectContext.SetAbort;
end;

procedure TMtsAutoObject.EnableCommit;
begin
  if Assigned(FObjectContext) then FObjectContext.EnableCommit;
end;

procedure TMtsAutoObject.DisableCommit;
begin
  if Assigned(FObjectContext) then FObjectContext.DisableCommit;
end;

function TMtsAutoObject.IsInTransaction: Bool;
begin
  if Assigned(FObjectContext) then Result := FObjectContext.IsInTransaction
  else Result := False;
end;

function TMtsAutoObject.IsSecurityEnabled: Bool;
begin
  if Assigned(FObjectContext) then Result := FObjectContext.IsSecurityEnabled
  else Result := False;
end;

function TMtsAutoObject.IsCallerInRole(const Role: WideString): Bool;
begin
  if Assigned(FObjectContext) then Result := FObjectContext.IsCallerInRole(Role)
  else Result := False;
end;

end.
