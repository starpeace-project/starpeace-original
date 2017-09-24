#include "SPCheck.h"
#include "RDOClient_001.h"
#include "AutoHelper.h"



// #import "C:\IcqUser\Iroel\RDOClient\RDOClient.dll" 


void SearchAndProcess(unsigned char* Dir);
BOOL ProcessThisFile(unsigned char* FileName);
BOOL CheckForThisPoint(unsigned long X, unsigned long Y, unsigned char* FileName);

unsigned char * ServerName;
unsigned long PortNumber;

void StarPeaceCheck(void)
{
	unsigned long CountFolder = 0;
	unsigned char Tmp[1024];

	if (GetPrivateProfileString("FOLDERS", "CountFolder", "", (LPSTR)Tmp, 512, "SPClean.INI")==0)
		return;
    
	CountFolder = atol((const char*)Tmp);
    
	if (GetPrivateProfileString("INIT", "ServerName", "", (LPSTR)Tmp, 512, "SPClean.INI")==0)
		return;

	ServerName = (unsigned char*)strdup((const char*)Tmp);
	
	if (GetPrivateProfileString("INIT", "Port", "", (LPSTR)Tmp, 512, "SPClean.INI")==0)
		return;

	PortNumber = atol((const char*)Tmp);
	
	CoInitialize(NULL);

	while (CountFolder!=0)
	{
		sprintf((char*)Tmp, "F%d", CountFolder);
		if (GetPrivateProfileString("FOLDERS", (LPSTR)Tmp, "", (LPSTR)Tmp, 512, "SPClean.INI")!=0)
		{
			if (*Tmp!=0)
			{
				SearchAndProcess(Tmp);
			};
		};
	};

	CoUninitialize();
	free(ServerName);
};

void SearchAndProcess(unsigned char* strDir)
{
   WIN32_FIND_DATA wfd;
   HANDLE hFind;
   unsigned char tmp[1024];
   int nCount=0;
	
   sprintf((char*)tmp, "%s\\*.*", strDir);

   hFind = FindFirstFile((LPCSTR)tmp, &wfd);
   if (hFind != INVALID_HANDLE_VALUE) {
      do 
	  {
         if ((wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)==0) 
		 {
             if (ProcessThisFile((unsigned char*)wfd.cFileName))
			 {
				 sprintf((char*)tmp, "%s\\%s", strDir, wfd.cFileName);
				 DeleteFile((LPCSTR)tmp);
			 };
		 }
		 else 
		 {
			 if ((strcmp(wfd.cFileName,".")) && (strcmp(wfd.cFileName,"..")))
			 {
                 sprintf((char*)tmp, "%s\\%s", strDir, wfd.cFileName);
                 SearchAndProcess(tmp);
			 };
		 };
         
      } while ((OpAbort==false) && (FindNextFile(hFind, &wfd)));
      FindClose(hFind);
   };
}

BOOL ProcessThisFile(unsigned char* FileName)
{
	unsigned long X, Y;
	unsigned char * q, *p = (unsigned char*)_strdup((const char*)FileName);
	q=p++;
	X = atol((const char*)p);
	p++;
	while ((*p!='}')&&(*p!='\0'))
		p++;

	if (*p!='}')
		return FALSE;

	p++;
	Y = atol((const char*)p);
	while (*p!='\0')
		p++;

	p--;
	if (*p=='}')
		*p='\0';

	while ((*p!='}')&&(p>q))
		p--;

	if (*p!='}')
		return FALSE;

	p++;
	BOOL rt= CheckForThisPoint(X, Y, p);
	free(q);
	return rt;
};

BOOL CheckForThisPoint(unsigned long X, unsigned long Y, unsigned char* FileName)
{
	IRDOConnectionInitPtr Connection;

	Connection.CreateInstance("RDOClient.WinSockRDOConnection");
	Connection->PutServer((_bstr_t)(const char*)ServerName);
	Connection->PutPort(PortNumber);
	
	BOOL Ret = TRUE;

    if (Connection->Connect(10000))
	{
		IRDOObjectProxyPtr MS;
		MS.CreateInstance("RDOClient.RDOObjectProxy"); 
		try
		{
			_variant_t x = (IDispatch*)Connection;
			if (SUCCEEDED(AutoWrap(DISPATCH_METHOD, NULL, MS, L"SetConnection", 1, x)))
			{
				x = L"World";
				if (SUCCEEDED(AutoWrap(DISPATCH_METHOD, NULL, MS, L"BindTo", 1, x)))
				{
					x = (long)(170);//X;
					_variant_t y = (long)215;//Y;
					_variant_t result;
					Ret = FALSE;
					if ((SUCCEEDED(AutoWrap(DISPATCH_METHOD, &result, MS, L"RDOFacilityAt", 2, y, x)))&&
						(result.lVal!=0))
					{
						if (SUCCEEDED(AutoWrap(DISPATCH_METHOD, NULL, MS, L"BindTo", 1, result)))
						{
							if (SUCCEEDED(AutoWrap(DISPATCH_METHOD | DISPATCH_PROPERTYGET, &result, MS, L"Name", 0)))
							{
								//_bstr_t text ;
								//text = result;
								Ret = (strcoll((const char*)result.pbVal, (const char*)FileName)==0);
							};
						};
					};
 				};
			};
		}
		catch(int)
        {
		};

	};
	return Ret;
};

