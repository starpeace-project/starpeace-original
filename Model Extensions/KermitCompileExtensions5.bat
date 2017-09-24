@echo off

c:

echo "GENERAL"

cd "\Five\Source\Model Extensions\General\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "GeneralPack1.dpr"

echo "MOAB"

cd "\Five\Source\Model Extensions\Moab\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "MoabPack1.dpr"

echo "DISSIDENTS"

cd "\Five\Source\Model Extensions\Dissidents\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "DissidentPack1.dpr"

echo "UNIVERSAL WAREHOUSES"

cd "\Five\Source\Model Extensions\UW\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "UWPack1.dpr"

echo "PGI"

cd "\Five\Source\Model Extensions\PGI\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "PGIPack1.dpr"

echo "MARIKO"

cd "\Five\Source\Model Extensions\Mariko\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "MarikoPack1.dpr"

echo "TRAINS"

cd "\Five\Source\Model Extensions\Trains\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Land\;..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;c:\Program Files\Borland\Delphi 3\Lib" "Trains.dpr"

echo "MODEL SERVER"

cd "\Five\Source\Model Server\"

"c:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\dcu" -e"..\..\release\Servers" -u"..\Tasks;..\Actors;..\Model extensions;..\Kernel;..\StdBlocks;..\Class Storage;..\Cache;..\RDO\Common;..\RDO\Server;..\Utils\Network;..\RDO\Client\;..\Persistence;..\Utils\Misc;..\Utils\Vcl;..\Surfaces;..\Mail;c:\Program Files\Borland\Delphi 3\Lib" "FIVEModelServer.dpr"

