unit MediaNameGenerator;

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
                      result := str2 + ' ' + str1;
                    else
                      result := str1 + ' ' + str2;
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
                  case lng of
                    0:
                      begin
                        if Copy(lstA[idx2], 0, 4) = 'The '
                          then str1 := 'the' + Copy(lstA[idx2], 4, length(lstA[idx2]))
                          else str1 := lstA[idx2];
                        result := lstA[idx1] + ' meets ' + lstA[idx2];
                      end;
                    4:
                      begin
                        str2 := lstA[idx2];
                        if (Copy(str2, 0, 3) = 'Lo ') or (Copy(str2, 0, 3) = 'La ') or (Copy(str2, 0, 2) = 'L''')
                          then str2 := 'l' + Copy(str2, 2, length(str2))
                          else
                            if (Copy(str2, 0, 3) = 'Il ')
                              then str2 := 'i' + Copy(str2, 2, length(str2));
                        result := lstA[idx1] + ' contro ' + str2;
                      end;
                    else result := lstA[idx1] + ' vs ' + lstA[idx2];
                  end;
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

procedure Init1;
  var
    Spa         : TStringList;
    //Adjectives  : TStringList;
    Classics    : TStringList;
    //Characters  : TStringList;
    //Names       : TStringList;
    //Relatives   : TStringList;
    //Subjectives : TStringList;
    //Verbs       : TStringList;
  begin
    // Spa
    Spa := TStringList.Create;
    Languages.AddObject('1', Spa);

    // Adjectives
    {Adjectives := TStringList.Create;
    Spa.AddObject('Adjectives', Adjectives);
    with Adjectives do
      begin
        //Add('');
      end;}

    // Characters
    {Characters := TStringList.Create;
    Spa.AddObject('Characters', Characters);
    with Characters do
      begin
        //Add('');
      end;}

    // Classics
    Classics := TStringList.Create;
    Spa.AddObject('Classics', Classics);
    with Classics do
      begin
        //Add('');
        Add('Soy Cuba/Ya Kuba');
        Add('Memorias del subdesarrollo');
        Add('Lista de espera');
        Add('Lucía');
        Add('Azúcar amarga');
        Add('Buena Vista Social Club');
        Add('Fresa y chocolate');
        Add('La Vida es silbar');
        Add('Guantanamera');
        Add('Az én XX. századom');
        Add('Esperando la carroza');
        Add('Plácido');
        Add('El Verdugo');
        Add('Km. 0');
        Add('Ratas, ratones, rateros');
        Add('Nueve reinas');
        Add('La Ley de Herodes');
        Add('Bienvenido Mister Marshall');
        Add('El Sur');
        Add('Los Olvidados');
        Add('No se lo digas a nadie');
        Add('Esperando al mesías');
        Add('Cenizas del paraíso');
        Add('El Faro');
        Add('Moebius');
        Add('Sol de otoño');
        Add('El Ángel exterminador');
        Add('Una Noche con Sabrina Love');
        Add('Viridiana');
        Add('Sólo con tu pareja');
        Add('Sobreviviré');
        Add('Amores perros');
        Add('Todo el poder');
        Add('Todo sobre mi madre');
        Add('Cría cuervos');
        Add('Lucía y el sexo');
        Add('Un Lugar en el mundo');
        Add('Los Santos inocentes');
        Add('Memorias del subdesarrollo');
        Add('Arrebato');
        Add('El Discreto encanto de la burguesía');
        Add('Abre los ojos');
        Add('El Chacotero sentimental: La película');
        Add('Solas');
        Add('El Bola');
        Add('El Espíritu de la colmena');
        Add('Marcelino pan y vino');
        Add('Caballos salvajes');
        Add('La Comunidad');
        Add('Pantaleón y las visitadoras');
        Add('Hombre mirando al sudeste');
        Add('Ensayo de un crimen');
        Add('Garage Olimpo');
        Add('La Historia oficial');
        Add('La Vendedora de rosas');
        Add('Nazarín');
        Add('La Ciénaga');
        Add('Simón del desierto');
        Add('Kika');
        Add('Boca a boca');
        Add('Se infiel y no mires con quien');
        Add('Malena es un nombre de tango');
        Add('Clandestino');
        Add('Adorables mentiras');
        Add('Los pajaros tirandole a la escopeta');
        Add('Plaf!');
        Add('Amor vertical');
        Add('Tropicana, un paraiso bajo las estrellas');
        Add('America y El mango');
        Add('Bienvenido welcome');
        Add('El viejo y el mar');
        Add('El año del conejo');
        Add('Kleines Tropicana');
        Add('El retrato de Teresa');
        Add('Amores perros');
        Add('La infiel');
        Add('La puta calle');
        Add('Alicia en el pueblo de las maravillas');
        Add('Perro jibaro');
        Add('Papeles secundarios');
        Add('La bella de la Alambra');
        Add('Jamón jamón');
      end;

    // Names
    {Names := TStringList.Create;
    Spa.AddObject('Names', Names);
    with Names do
      begin
        //Add('');
      end;}

    // Relatives
    {Relatives := TStringList.Create;
    Spa.AddObject('Relatives', Relatives);
    with Relatives do
      begin
        //Add('');
      end;}

    // Subjectives
    {Subjectives := TStringList.Create;
    Spa.AddObject('Subjectives', Subjectives);
    with Subjectives do
      begin
        //Add('');
      end;}

    // Verbs
    {Verbs := TStringList.Create;
    Spa.AddObject('Verbs', Verbs);
    with Verbs do
      begin
        //Add('');
      end;}
  end;

procedure Init2;
  var
    Fre         : TStringList;
    //Adjectives  : TStringList;
    Classics    : TStringList;
    //Characters  : TStringList;
    //Names       : TStringList;
    //Relatives   : TStringList;
    //Subjectives : TStringList;
    //Verbs       : TStringList;
  begin
    // Fre
    Fre := TStringList.Create;
    Languages.AddObject('1', Fre);

    // Adjectives
    {Adjectives := TStringList.Create;
    Fre.AddObject('Adjectives', Adjectives);
    with Adjectives do
      begin
        //Add('');
      end;}

    // Characters
    {Characters := TStringList.Create;
    Fre.AddObject('Characters', Characters);
    with Characters do
      begin
        //Add('');
      end;}

    // Classics
    Classics := TStringList.Create;
    Fre.AddObject('Classics', Classics);
    with Classics do
      begin
        //Add('');
        Add('Nuit et brouillard');
        Add('Le Fabuleux destin d''Amélie Poulain');
        Add('Un condamné à mort s''est échappé ou Le vent souffle où il veut');
        Add('La Rivière du hibou');
        Add('La Maman et la putain');
        Add('Au hasard Balthazar');
        Add('Sans soleil');
        Add('La Grande illusion');
        Add('Les Quatre cents coups');
        Add('Le Salaire de la peur');
        Add('La Règle du jeu');
        Add('La Belle et la bête');
        Add('Les Enfants du paradis');
        Add('Jeux interdits');
        Add('Du rififi chez les hommes');
        Add('Orphée');
        Add('Les Diaboliques');
        Add('Vivre sa vie: Film en douze tableaux');
        Add('La Jetée');
        Add('0  Innocence');
        Add('Masculin, féminin');
        Add('Baisers volés');
        Add('Le Vieux fusil');
        Add('Le Ballon rouge');
        Add('Bande à part');
        Add('Jean de Florette');
        Add('L''Atalante');
        Add('Le Charme discret de la bourgeoisie');
        Add('Ma nuit chez Maud');
        Add('Les Tontons flingueurs');
        Add('Les Demoiselles de Rochefort');
        Add('L''Âge d''or');
        Add('Les Parapluies de Cherbourg');
        Add('Manon des sources');
        Add('Au revoir les enfants');
        Add('Le Fantôme de la liberté');
        Add('Ascenseur pour l''échafaud');
        Add('Olivier, Olivier');
        Add('Mouchette');
        Add('À bout de souffle');
        Add('Le Violon rouge');
        Add('La Nuit américaine');
        Add('Les Enfants du marais');
        Add('Les Yeux sans visage');
        Add('Le Cercle rouge');
        Add('Le Confessionnal');
        Add('La Cité des enfants perdus');
        Add('L''Argent de poche');
        Add('Les Deux anglaises et le continent');
        Add('Jules et Jim');
        Add('La Vie rêvée des anges');
        Add('Le Samouraï');
        Add('Delicatessen');
        Add('Mon oncle');
        Add('Ressources humaines');
        Add('La Cérémonie');
        Add('Sous le sable');
        Add('Conte d''été');
        Add('Le Mari de la coiffeuse');
        Add('La Haine');
        Add('La femme Nikita');
      end;

    // Names
    {Names := TStringList.Create;
    Fre.AddObject('Names', Names);
    with Names do
      begin
        //Add('');
      end;}

    // Relatives
    {Relatives := TStringList.Create;
    Fre.AddObject('Relatives', Relatives);
    with Relatives do
      begin
        //Add('');
      end;}

    // Subjectives
    {Subjectives := TStringList.Create;
    Fre.AddObject('Subjectives', Subjectives);
    with Subjectives do
      begin
        //Add('');
      end;}

    // Verbs
    {Verbs := TStringList.Create;
    Fre.AddObject('Verbs', Verbs);
    with Verbs do
      begin
        //Add('');
      end;}
  end;

procedure Init3;
  var
    Ger         : TStringList;
    //Adjectives  : TStringList;
    Classics    : TStringList;
    //Characters  : TStringList;
    //Names       : TStringList;
    //Relatives   : TStringList;
    //Subjectives : TStringList;
    //Verbs       : TStringList;
  begin
    // Ger
    Ger := TStringList.Create;
    Languages.AddObject('1', Ger);

    // Adjectives
    {Adjectives := TStringList.Create;
    Ger.AddObject('Adjectives', Adjectives);
    with Adjectives do
      begin
        //Add('');
      end;}

    // Characters
    {Characters := TStringList.Create;
    Ger.AddObject('Characters', Characters);
    with Characters do
      begin
        //Add('');
      end;}

    // Classics
    Classics := TStringList.Create;
    Ger.AddObject('Classics', Classics);
    with Classics do
      begin
        //Add('');
        Add('M - Eine Stadt sucht einen Mörder');
        Add('Es geschah am hellichten Tag');
        Add('Das Boot');
        Add('Festen');
        Add('Stalag 17');
        Add('Lola rennt');
        Add('Die Brücke');
        Add('Das Experiment');
        Add('Jeder für sich und Gott gegen alle');
        Add('Angst essen Seele auf');
        Add('Aguirre, der Zorn Gottes');
        Add('Die Siebtelbauern');
        Add('Der Himmel über Berlin');
        Add('Der Totmacher');
        Add('Der Blaue Engel');
        Add('Die Blechtrommel');
        Add('Indien');
        Add('Die Feuerzangenbowle');
        Add('Bang Boom Bang - Ein todsicheres Ding');
        Add('Karakter');
        Add('Zugvögel... einmal nach Inari');
        Add('Fitzcarraldo');
        Add('Jenseits der Stille');
        Add('Vampyr - Der Traum des Allan Grey');
        Add('Das Testament des Dr. Mabuse');
        Add('Winterschläfer');
        Add('Aimée & Jaguar');
        Add('Der Krieger und die Kaiserin');
        Add('Die Ehe der Maria Braun');
        Add('Das Leben ist eine Baustelle.');
        Add('Code inconnu: Récit incomplet de divers voyages');
        Add('Metropolis');
        Add('Nosferatu, eine Symphonie des Grauens');
        Add('Das Kabinett des Doktor Caligari');
        Add('Die Büchse der Pandora');
      end;

    // Names
    {Names := TStringList.Create;
    Ger.AddObject('Names', Names);
    with Names do
      begin
        //Add('');
      end;}

    // Relatives
    {Relatives := TStringList.Create;
    Ger.AddObject('Relatives', Relatives);
    with Relatives do
      begin
        //Add('');
      end;}

    // Subjectives
    {Subjectives := TStringList.Create;
    Ger.AddObject('Subjectives', Subjectives);
    with Subjectives do
      begin
        //Add('');
      end;}

    // Verbs
    {Verbs := TStringList.Create;
    Ger.AddObject('Verbs', Verbs);
    with Verbs do
      begin
        //Add('');
      end;}
  end;

procedure Init4;
  var
    Ita         : TStringList;
    Adjectives  : TStringList;
    Classics    : TStringList;
    Characters  : TStringList;
    Names       : TStringList;
    Relatives   : TStringList;
    Subjectives : TStringList;
    Verbs       : TStringList;
  begin
    // Ita
    Ita := TStringList.Create;
    Languages.AddObject('1', Ita);

    // Adjectives
    Adjectives := TStringList.Create;
    Ita.AddObject('Adjectives', Adjectives);
    with Adjectives do
      begin
        //Add('');
        Add('Domestica');
        Add('Pericolosa');
        Add('Simmetrica');
        Add('Finale');
        Add('Fatale');
        Add('Cieca');
        Add('Mortale');
        Add('Doppia');
        Add('Nascosta');
        Add('Idiota');
        Add('Impossibile');
        Add('Assurda');
        Add('Morbosa');
        Add('Rischiosa');
        Add('Letale');
        Add('Satanica');
        Add('Animale');
        Add('Perduta');
        Add('Segreta');
        Add('Disperata');
        Add('Istrionica');
      end;

    // Characters
    Characters := TStringList.Create;
    Ita.AddObject('Characters', Characters);
    with Characters do
      begin
        //Add('');
        Add('della Mummia');
        Add('dello Zombie');
        Add('di Frankenstein');
        Add('di Dracula');
        Add('della Cosa dallo Spazio Profondo');
        Add('di Godzilla');
        Add('della Donna Bionica');
        Add('dell''Uomo da 6 Milioni di Dollari');
        Add('dell''Uomo Ragno');
        Add('di Pac Man');
        Add('del Mostro della Laguna Nera');
        Add('di Superman');
        Add('di Alien');
        Add('di Chucki');
        Add('di Cyclope');
        Add('del Calamaro Gigante');
        Add('di Cujo');
        Add('di Freddy Krueger');
        Add('di King Kong');
        Add('del Leprechaun');
        Add('di Pumpinkhead');
        Add('del Mostro di Loch Ness');
        Add('di Sandman');
        Add('di Blob');
        Add('di Dr. Jekill');
        Add('di Mr. Hyde');
        Add('di Jack la Squartatore');
        Add('di Jabba the Hutt');
        Add('di Tarzan');
        Add('di Batman');
        Add('di Supergirl');
        Add('di Robin');
        Add('di Rambo');
        Add('di Rocky');
        Add('di Sandokan');
        Add('del Corsaro Nero');
        Add('dell''Uomo Invisibile');
        Add('di Maciste');
        Add('della Pantera Rosa');
      end;

    // Classics
    Classics := TStringList.Create;
    Ita.AddObject('Classics', Classics);
    with Classics do
      begin
        //Add('');
        Add('La Ballata del boia');
        Add('Umberto D');
        Add('Bianca');
        Add('Ladri di biciclette');
        Add('Il Buono, il brutto, il cattivo');
        Add('La Grande guerra');
        Add('C''era una volta il West');
        Add('La Vita è bella');
        Add('La Battaglia di Algeri');
        Add('Il Ladro di bambini');
        Add('C''eravamo tanto amati');
        Add('Una Giornata particolare');
        Add('L''Armata Brancaleone');
        Add('Vite vendute');
        Add('Le Notti di Cabiria');
        Add('Il Sorpasso');
        Add('Nuovo cinema Paradiso');
        Add('La Strada');
        Add('La Ciociara');
        Add('Non ci resta che piangere');
        Add('Roma, città aperta');
        Add('Il Vangelo secondo Matteo');
        Add('La Notte di San Lorenzo');
        Add('La Notte');
        Add('Pasqualino Settebellezze');
        Add('Amarcord');
        Add('Rocco e i suoi fratelli');
        Add('La Traviata');
        Add('Brutti sporchi e cattivi');
        Add('8 1/2');
        Add('I Soliti ignoti');
        Add('La Dolce vita');
        Add('Il Conformista');
        Add('C''era una volta in America');
        Add('Amici miei');
        Add('Il Violino rosso');
        Add('Effetto notte');
        Add('Divorzio all''italiana');
        Add('L''Albero degli zoccoli');
        Add('Fuori dal mondo');
        Add('Il Gattopardo');
        Add('Pane e cioccolata');
        Add('Mamma Roma');
        Add('Fantozzi');
        Add('Kaos');
        Add('Ossessione');
        Add('Il Giardino dei Finzi-Contini');
        Add('Paisà');
        Add('Per qualche dollaro in più');
        Add('Miracolo a Milano');
        Add('L''Avventura');
        Add('L''Ultimo imperatore');
        Add('La Messa è finita');
        Add('Profondo rosso');
        Add('Profumo di donna');
        Add('Il Grande silenzio');
        Add('1900');
        Add('Il Postino');
        Add('Palombella rossa');
        Add('Per un pugno di dollari');
      end;

    // Names
    Names := TStringList.Create;
    Ita.AddObject('Names', Names);

    with Names do
      begin
        //Add('');
        Add('Molestia');
        Add('Sensibilità');
        Add('Allucinazione');
        Add('Intelligenza');
        Add('Analisi');
        Add('Attrazione');
        Add('Ossessione');
        Add('Relazione');
        Add('Ammirazione');
        Add('Salivazione');
        Add('Missione');
        Add('Apparizione');
        Add('Innocenza');
        Add('Bellezza');
        Add('Avventura');
        Add('Aspettazione');
        Add('Ispirazione');
        Add('Opacità');
        Add('Preoccupazione');
        Add('Ostilità');
        Add('Istintività');
      end;

    // Relatives
    Relatives := TStringList.Create;
    Ita.AddObject('Relatives', Relatives);
    with Relatives do
      begin
        //Add('');
        Add('La Figlia');
        Add('Il Figlio');
        Add('Il Padre');
        Add('La Madre');
        Add('Il Fratello');
        Add('La Sorella');
        Add('Il Cugino');
        Add('La Cugina');
        Add('Il Nipote');
        Add('La Nipote');
        Add('Il Suocero');
        Add('La Suocera');
        Add('Il Nuoro');
        Add('La Nuora');
        Add('Lo Zio');
        Add('La Zia');
        Add('Il Nonno');
        Add('La Nonna');
        Add('La Sposa');
        Add('Lo Sposo');
        Add('La Moglie');
        Add('Il Marito');
        Add('Il Fidanzato');
        Add('La Fidanzata');
        Add('Il Cane');
        Add('Il Gatto');
        Add('La vedova');
        Add('Il vedovo');
        Add('Il Fratello Gemello');
        Add('La Sorella Gemella');
        Add('La Vendetta');
        Add('La Resurrezione');
        Add('L''Educazione');
      end;

    // Subjectives
    Subjectives := TStringList.Create;
    Ita.AddObject('Subjectives', Subjectives);
    with Subjectives do
      begin
        //Add('');
        Add('La Mummia');
        Add('Lo Zombie');
        Add('Frankenstein');
        Add('Dracula');
        Add('La Cosa dallo Spazio Profondo');
        Add('Godzilla');
        Add('La Donna Bionica');
        Add('L''Uomo da 6 Milioni di Dollari');
        Add('L''Uomo Ragno');
        Add('Pac Man');
        Add('Il Mostro della Laguna Nera');
        Add('Superman');
        Add('Alien');
        Add('Chucki');
        Add('Cyclope');
        Add('Il Calamaro Gigante');
        Add('Cujo');
        Add('Freddy Krueger');
        Add('King Kong');
        Add('Il Leprechaun');
        Add('Pumpinkhead');
        Add('Il Mostro di Loch Ness');
        Add('Sandman');
        Add('Blob');
        Add('Dr. Jekill');
        Add('Mr. Hide');
        Add('Jack lo Squartatore');
        Add('Jabba the Hutt');
        Add('Tarzan');
        Add('Batman');
        Add('Supergirl');
        Add('Robin');
        Add('Rambo');
        Add('Rocky');
        Add('Sandokan');
        Add('Il Corsaro Nero');
        Add('L''Uomo Invisibile');
        Add('Maciste');
        Add('La Pantera Rosa');
        Add('Johny stecchino');
        Add('Il Mostro');
      end;

    // Verbs
    Verbs := TStringList.Create;
    Ita.AddObject('Verbs', Verbs);
    with Verbs do
      begin
        //Add('');
        Add('Colpisce Ancora');
        Add('Si Sposa');
        Add('Non Si Risparmia');
        Add('Contro il Suo Malefico Gemello');
        Add('Si Vendica');
        Add('Va Veloce');
        Add('Suona Sempre Due Volte');
        Add('Contrattacca');
      end;
  end;

  procedure Init;
    begin
      Languages := TStringList.Create;
      Init0;
      Init1;
      Init2;
      Init3;
      Init4;
    end;

initialization

  if Languages = nil
    then Init;

end.
