unit NewsServerInterfaces;

interface

  type
    INewsServer =
      interface
        procedure CreateNewsCenter  ( World : string );
        procedure CreateNewspaper   ( World, Name, Style, Town : string );
        procedure GenerateNewspapers( World : widestring; Date : TDateTime );
      end;

implementation

end.
