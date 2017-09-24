unit EditorEvents;

interface

  uses
    classes;

  const
    evGetTemplates = 1;

  type
    TGetTemplateData =
      record
        Name     : string;
        Template : TStrings;
      end;

implementation

end.
 