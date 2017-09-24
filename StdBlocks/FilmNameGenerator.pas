unit FilmNameGenerator;

interface

  uses
    Classes, SysUtils;

  function GenerateName(lng : integer; titleType : integer) : string;

implementation

  var
    Languages : TStringList = nil;

  function SubListByName(List : TStringList; name : string) : TStringList;
    var
      idx : integer;
    begin
      idx := List.IndexOf(name);
      if idx <> -1
        then result := TStringList(List.Objects[idx])
        else result := nil;
    end;

  function GenerateName(lng : integer; titleType : integer) : string;
    var
      Lang : TStringList;
      lstA : TStringList;
      lstB : TStringList;
      str1 : string;
      str2 : string;
      idx1 : integer;
      idx2 : integer;
    begin
      if lng >= Languages.Count
        then lng := 0;
      if titleType = -1
        then titleType := random(5);
      Lang := TStringList(Languages.Objects[lng]);
      case titleType of
        0:
          begin
            lstA := SubListByName(Lang, 'Classics');
            if lstA <> nil
              then result := lstA[random(lstA.Count)]
              else result := 'Living la vida loca';
          end;
        1:
          begin
            lstA := SubListByName(Lang, 'Adjectives');
            lstB := SubListByName(Lang, 'Names');
            if (lstA <> nil) and (lstB <> nil)
              then
                begin
                  str1 := lstA[random(lstA.Count)];
                  str2 := lstB[random(lstB.Count)];
                  case lng of
                    1, 4:
                      result := str1 + ' ' + str2;
                    else
                      result := str2 + ' ' + str1;
                  end;
                end
              else result := GenerateName(lng, 0);
          end;
        2:
          begin
            lstA := SubListByName(Lang, 'Subjectives');
            lstB := SubListByName(Lang, 'Verbs');
            if (lstA <> nil) and (lstB <> nil)
              then
                begin
                  str1 := lstA[random(lstA.Count)];
                  str2 := lstB[random(lstB.Count)];
                  result := str1 + ' ' + str2;
                end
              else result := GenerateName(lng, 0);
          end;
        3:
          begin
            lstA := SubListByName(Lang, 'Relatives');
            lstB := SubListByName(Lang, 'Characters');
            if (lstA <> nil) and (lstB <> nil)
              then
                begin
                  str1 := lstA[random(lstA.Count)];
                  str2 := lstB[random(lstB.Count)];
                  result := str1 + ' ' + str2;
                end
              else result := GenerateName(lng, 0);
          end;
        4:
          begin
            lstA := SubListByName(Lang, 'Subjectives');
            if (lstA <> nil) and (lstA.Count > 1)
              then
                begin
                  idx1 := random(lstA.Count);
                  repeat
                    idx2 := random(lstA.Count);
                  until idx1 <> idx2;
                  result := str1 + ' ' + str2;
                end
              else result := GenerateName(lng, 0);
          end;
      end;
    end;

  procedure Init0;
    var
      Eng         : TStringList;
      Adjectives  : TStringList;
      Classics    : TStringList;
      Characters  : TStringList;
      Names       : TStringList;
      Relatives   : TStringList;
      Subjectives : TStringList;
      Verbs       : TStringList;
    begin
      // Eng
      Eng := TStringList.Create;
      Languages.AddObject('0', Eng);

      // Adjectives
      Adjectives := TStringList.Create;
      Eng.AddObject('Adjectives', Adjectives);

      with Adjectives do
        begin
          Add('Domestic');
          Add('Dangerous');
          Add('Symmetric');
          Add('Final');
          Add('Fatal');
          Add('Blind');
          Add('Mortal');
          Add('Double');
          Add('Hidden');
          Add('Idiotic');
          Add('Impossible');
          Add('Absurd');
          Add('Morbid');
          Add('Risky');
          Add('Lethal');
          Add('Satanic');
          Add('Animal');
          Add('Lost');
          Add('Secret');
          Add('Desperate');
          Add('Histrionic');
          Add('Basic');
        end;

      // Characters
      Characters := TStringList.Create;
      Eng.AddObject('Characters', Characters);
      with Characters do
        begin
          Add('of the Mummy');
          Add('of the Zombie');
          Add('of Frankenstein');
          Add('of Dracula');
          Add('of the Thing from Outer Space');
          Add('of Godzilla');
          Add('of the Bionic Woman');
          Add('of the 6 Million Dollar Man');
          Add('of Spider Man');
          Add('of Pac Man');
          Add('of the Swamp Thing');
          Add('of Superman');
          Add('of Alien');
          Add('of Chucky');
          Add('of Cyclops');
          Add('of the Giant Squid');
          Add('of Cujo');
          Add('of Freddy Krueger');
          Add('of King Kong');
          Add('of the Leprechaun');
          Add('of Pumpinkhead');
          Add('of the Loch Ness Monster');
          Add('of Sandman');
          Add('of the Blob');
          Add('of Dr. Jekill');
          Add('of Mr. Hide');
          Add('of Jack the Ripper');
          Add('of Jabba the Hutt');
          Add('of Tarzan');
          Add('of Batman');
          Add('of Supergirl');
          Add('of Robin');
          Add('of Rambo');
          Add('of the Invisible Man');
          Add('of Rocky');
          Add('of Hercules');
          Add('of the Pink Panthers');
          Add('of Billy the Kid');
        end;

      // Classics
      Classics := TStringList.Create;
      Eng.AddObject('Classics', Classics);
      with Classics do
        begin
          Add('The Godfather');
          Add('The Shawshank Redemption');
          Add('Schindler''s List');
          Add('Citizen Kane');
          Add('Casablanca');
          Add('Star Wars');
          Add('Memento');
          Add('Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb');
          Add('One Flew Over the Cuckoo''s Nest');
          Add('Rear Window');
          Add('Raiders of the Lost Ark');
          Add('American Beauty');
          Add('The Usual Suspects');
          Add('Psycho');
          Add('Pulp Fiction');
          Add('The Silence of the Lambs');
          Add('North by Northwest');
          Add('It''s a Wonderful Life');
          Add('Goodfellas');
          Add('Lawrence of Arabia');
          Add('Saving Private Ryan');
          Add('Vertigo');
          Add('Taxi Driver');
          Add('To Kill a Mockingbird');
          Add('L.A. Confidential');
          Add('Fight Club');
          Add('The Matrix');
          Add('The Third Man');
          Add('Apocalypse Now');
          Add('Paths of Glory');
          Add('Sunset Blvd.');
          Add('Some Like It Hot');
          Add('The Maltese Falcon');
          Add('The Wizard of Oz');
          Add('The Sixth Sense');
          Add('Chinatown');
          Add('M');
          Add('Raging Bull');
          Add('Toy Story 2');
          Add('Requiem for a Dream');
          Add('The Bridge on the River Kwai');
          Add('Singin'' in the Rain');
          Add('All About Eve');
          Add('2001: A Space Odyssey');
          Add('Monty Python and the Holy Grail');
          Add('American History X');
          Add('A Clockwork Orange');
          Add('Alien');
          Add('The Manchurian Candidate');
          Add('Touch of Evil');
          Add('Braveheart');
          Add('Reservoir Dogs');
          Add('Fargo');
          Add('Blade Runner');
          Add('Jaws');
          Add('The Treasure of the Sierra Madre');
          Add('Double Indemnity');
          Add('Double Jeopardy');
          Add('Léon');
          Add('The Sting');
          Add('Amadeus');
        end;

     // Names
     Names := TStringList.Create;
     Eng.AddObject('Names', Names);

    with Names do
      begin
        Add('Disturbance');
        Add('Liaison');
        Add('Sensibility');
        Add('Hallucination');
        Add('Intelligence');
        Add('Analysis');
        Add('Affair');
        Add('Business');
        Add('Attraction');
        Add('Obsession');
        Add('Mission');
        Add('Apparition');
        Add('Innocence');
        Add('Jeopardy');
        Add('Transfer');
        Add('Beauty');
        Add('Exposure');
        Add('Adventure');
        Add('Crossing');
        Add('Highways');
        Add('Instinct');
      end;

    // Relatives
    Relatives := TStringList.Create;
    Eng.AddObject('Relatives', Relatives);
    with Relatives do
      begin
        Add('The Son');
        Add('The Daughter');
        Add('The Father');
        Add('The Mother');
        Add('The Brother');
        Add('The Sister');
        Add('The Cousin');
        Add('The Niece');
        Add('The Nephew');
        Add('The Grandson');
        Add('The Granddaughter');
        Add('The Grandfather');
        Add('The Grandmother');
        Add('The Father-in-Law');
        Add('The Mother-in-Law');
        Add('The Son-in-Law');
        Add('The Daughter-in-Law');
        Add('The Aunt');
        Add('The Uncle');
        Add('The Bride');
        Add('The Groom');
        Add('The Husband');
        Add('The Wife');
        Add('The Fiancée');
        Add('The Dog');
        Add('The Cat');
        Add('The Widow');
        Add('The Twin');
        Add('The Revenge');
        Add('The Resurrection');
        Add('The Education');
        Add('The Night');
      end;

      // Subjectives
      Subjectives := TStringList.Create;
      Eng.AddObject('Subjectives', Subjectives);
      with Subjectives do
        begin
          Add('The Mummy');
          Add('The Zombie');
          Add('Frankenstein');
          Add('Dracula');
          Add('The Thing from Outer Space');
          Add('Godzilla');
          Add('The Bionic Woman');
          Add('The 6 Million Dollar Man');
          Add('Spider Man');
          Add('Pac Man');
          Add('The Swamp Thing');
          Add('Superman');
          Add('Alien');
          Add('Chucky');
          Add('Cyclops');
          Add('The Giant Squid');
          Add('Cujo');
          Add('Freddy Krueger');
          Add('King Kong');
          Add('The Leprechaun');
          Add('Pumpkinhead');
          Add('The Loch Ness Monster');
          Add('Sandman');
          Add('The Blob');
          Add('Dr. Jekill');
          Add('Mr. Hide');
          Add('Jack The Ripper');
          Add('Jabba The Hutt');
          Add('Tarzan');
          Add('Batman');
          Add('Supergirl');
          Add('Robin');
          Add('Rambo');
          Add('Rocky');
          Add('The Invisible Man');
          Add('Hercules');
          Add('The Pink Panther');
          Add('Billy the Kid');
        end;

      // Verbs
      Verbs := TStringList.Create;
      Eng.AddObject('Verbs', Verbs);
      with Verbs do
        begin
          Add('Strikes Back');
          Add('Goes to the Distance');
          Add('Fights an Evil Twin');
          Add('is Back with a Vengeance');
          Add('Goes Ballistic');
          Add('Always Rings Twice');
          Add('Strikes Again');
        end;
    end;

  procedure Init;
    begin
      Languages := TStringList.Create;
      Init0;
    end;

initialization

  if Langauges = nil
    then Init;

end.
