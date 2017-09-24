unit VisualClasses;

interface


  function GetVisualRootPath : string;
  procedure SetCacheRootPath(const aPath : string);


implementation

  var
    VisualRoot : string = '';

  function GetVisualRootPath : string;
    begin
      if VisualRoot <> ''
        then SetVisualRootPath(CacheRegistryData.ReadCacheValue('', 'RootPath'));
    end;

  procedure SetVisualRootPath(const aPath : string);
    begin
      if aPath[length(aPath)] = '\'
        then VisualRoot := aPath
        else VisualRoot := aPath + '\';
    end;

end.
