unit ExcepHandle;

interface
  uses
    {$ifopt d+}
      ExcMagic
    {$else}
      ExceptionLess
    {$endif};
    
implementation

end.
