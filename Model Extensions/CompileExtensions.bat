@echo off

d:

cd "\Work\Five\Source\Model Extensions\General\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "GeneralPack1.dpr"

cd "\Work\Five\Source\Model Extensions\Moab\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "MoabPack1.dpr"

cd "\Work\Five\Source\Model Extensions\Dissidents\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "DissidentPack1.dpr"

cd "\Work\Five\Source\Model Extensions\UW\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "UWPack1.dpr"

cd "\Work\Five\Source\Model Extensions\PGI\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "PGIPack1.dpr"

cd "\Work\Five\Source\Model Extensions\Mariko\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\..\dcu" -e"..\..\..\release" -u"..\..\Kernel;..\..\StdBlocks;..\..\Class Storage;..\..\Cache;..\..\RDO\Common;..\..\RDO\Server;..\..\Utils\Network;..\..\RDO\Client\;..\..\Persistence;..\..\Utils\Misc;..\..\Utils\Vcl;..\..\Surfaces;..\..\Mail;..\..\Protocol;..\..\Circuits" "MarikoPack1.dpr"

cd "\Work\Five\Source\Model Server\"

"d:\Program Files\Borland\Delphi 3\Bin\dcc32.exe" -b -n"..\..\dcu" -e"..\..\release" -u"..\Model extensions;..\Kernel;..\StdBlocks;..\Class Storage;..\Cache;..\RDO\Common;..\RDO\Server;..\Utils\Network;..\RDO\Client\;..\Persistence;..\Utils\Misc;..\Utils\Vcl;..\Surfaces;..\Mail" "FIVEModelServer.dpr"

