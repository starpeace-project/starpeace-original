@echo off

e:

echo "GENERAL"

cd "\Work\Five\Source\Model Extensions\General\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "GeneralPack1.dpr"

echo "MOAB"

cd "\Work\Five\Source\Model Extensions\Moab\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "MoabPack1.dpr"

echo "DISSIDENTS"

cd "\Work\Five\Source\Model Extensions\Dissidents\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "DissidentPack1.dpr"

echo "UNIVERSAL WAREHOUSES"

cd "\Work\Five\Source\Model Extensions\UW\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "UWPack1.dpr"

echo "PGI"

cd "\Work\Five\Source\Model Extensions\PGI\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "PGIPack1.dpr"

echo "MARIKO"

cd "\Work\Five\Source\Model Extensions\Mariko\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "MarikoPack1.dpr"

echo "TRAINS"

cd "\Work\Five\Source\Model Extensions\Trains\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\..\dcu" -e"..\..\..\release\Servers" -u"..\..\Land\;..\..\Tasks;..\..\Actors;..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits;d:\Program Files\Borland\Delphi 3\Lib" "Trains.dpr"

echo "MODEL SERVER"

cd "\Work\Five\Source\Model Server\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -n"..\..\dcu" -e"..\..\release\Servers" -u"..\Tasks;..\Actors;..\Model extensions;..\Kernel;..\StdBlocks;..\Class Storage;..\Cache;..\RDO\Common;..\RDO\Server;..\Utils\Network;..\RDO\Client\;..\Persistence;..\Utils\Misc;..\Utils\Vcl;..\Surfaces;..\Mail;d:\Program Files\Borland\Delphi 3\Lib" "FIVEModelServer.dpr"

