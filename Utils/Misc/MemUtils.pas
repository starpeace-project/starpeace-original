unit MemUtils; // Memory Management

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  // Old-plain pointers
  procedure FreePtr( var Ptr );
  function  AllocatedSize( const Ptr ) : integer;

  // Delphi Object Model
  procedure FreeObject( var Obj );
  function  ObjectOk( Obj : TObject ) : boolean;

  // Component Object Model (COM)
  procedure ReleaseComObject( var Obj );

implementation

  uses
    ComObj;

  type
    pdword = ^integer;

  function AllocatedSize( const Ptr ) : integer;
    var
      Instance : pointer absolute Ptr;
    begin
      if Assigned( Instance )
        then Result := pdword(  pchar( Instance ) - 4 )^
        else Result := 0;
    end;

  procedure FreePtr( var Ptr );
    var
      Instance : pointer absolute Ptr;
    begin
      ReallocMem( Instance, 0 );
    end;

  procedure FreeObject( var Obj );
    var
      Instance : TObject absolute Obj;
    begin
      Instance.Free;
      Instance := nil;
    end;

  function ObjectOk( Obj : TObject ) : boolean;
    begin
      try
        Result := Obj.ClassName <> '';
      except
        Result := false;
      end;
    end;

  procedure ReleaseComObject( var Obj );
    var
      Instance : TInterfacedObject absolute Obj;
    begin
      Instance.Free;
      Instance := nil;
    end;

end.
