unit IsoProfile;

interface

  const
    prfKind_Main               = 0;
    prfId_Rendering            = 1;
    prfId_Blitting             = 2;
    prfId_RenderingInit        = 3;
    prfId_RegionFill           = 4;
    prfId_LandRendering        = 5;
    prfId_TextRendering        = 6;
    prfId_ConnectionsRendering = 7;
    prfId_SurfaceRendering     = 8;
    prfId_AirplaneRendering    = 9;
    prfId_ChatTextRendering    = 10;
    prfId_ImageDrawing         = 11;

  procedure InitIsoViewerProfiles;
  procedure LogIsoViewerProfiles;

implementation

  uses
    Profiler;

  procedure InitIsoViewerProfiles;
    begin
      {$IFDEF PROFILES}
      RequestProfile(prfKind_Main, prfId_Rendering, 'Rendering');
      RequestProfile(prfKind_Main, prfId_Blitting, 'Blitting');
      {
      RequestProfile(prfKind_Main, prfId_RenderingInit, 'Rendering Initialization');
      RequestProfile(prfKind_Main, prfId_RegionFill, 'Region Fill');
      RequestProfile(prfKind_Main, prfId_LandRendering, 'Land Rendering');
      RequestProfile(prfKind_Main, prfId_TextRendering, 'Text Rendering');
      RequestProfile(prfKind_Main, prfId_ConnectionsRendering, 'Connections Rendering');
      RequestProfile(prfKind_Main, prfId_SurfaceRendering, 'Surface Rendering');
      RequestProfile(prfKind_Main, prfId_AirplaneRendering, 'Airplane Rendering');
      RequestProfile(prfKind_Main, prfId_ChatTextRendering, 'Chat Text Rendering');
      }
      RequestProfile(prfKind_Main, prfId_ImageDrawing, 'Image Drawing');
      {$ENDIF}
    end;

  procedure LogIsoViewerProfiles;
    begin
      {$IFDEF PROFILES}
      LogResults(prfKind_Main, 'MainProfile');
      {$ENDIF}
    end;

end.
