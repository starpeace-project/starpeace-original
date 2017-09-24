unit NetTypes;

interface

  uses
    Windows;

  const
    MaxResources = 50;
    AllEntries   = $FFFFFFFF;

  type
    PNetResource = ^TNetResource;
    TNetResource =
      record
        dwScope       : DWORD;
        dwType        : DWORD;
        dwDisplayType : DWORD;
        dwUsage       : DWORD;
        lpLocalName   : pchar;
        lpRemoteName  : pchar;
        lpComment     : pchar;
        lpProvider    : pchar;
      end;

    PResourcesArray = ^TResourcesArray;
    TResourcesArray = array[0..MaxResources] of TNetResource;

implementation

end.

