
                             CABinet Resource Kit
                           Updated October 17, 1996
                           ------------------------

    This package provides developers with the components need to utilize
    Microsoft's CAB technology within other applications.  Microsoft is
    committed to making CAB an open technology.

    The code in this release supports 32-bit applications on Intel (R)
    i386 (tm) platforms.  Libraries for 16-bit applications on Intel (R)
    x86 platforms and other 32-bit Microsoft (R) Windows (R) NT platforms
    are available.  These files will be updated periodically; check
    http://www.microsoft.com/workshop/java/cab-f.htm for the latest.

    A detailed description of the CAB file format will be provided in the
    next release.  Comments and questions may be addressed via email to
    javacab@microsoft.com.

        README.TXT     This file

        FCIZ.LIB       File Compression Interface library
        FDIZ.LIB       File Decompression Interface library
        FCI_INT.H      Header file for FCI
        FDI_INT.H      Header file for FDI
        TYPES.H        Types used in FCI_INT.H, FDI_INT.H

        FCI-FDI.DOC    Microsoft (R) Word document describing use of
                       FCI and FDI from your application

        TESTFCI\       Sample file compression application
        TESTFDI\       Sample file decompression application


    The TESTFCI sample creates a CAB file (set) from a collection of
    files named on the command line.  TESTFCI limits each CAB file to
    300K compressed, to demonstrate files spanning across multiple CABs
    or multiple diskettes.

    The TESTFDI sample extracts the files from a CAB, including files
    that span multiple CABs.  It provides details on the status message
    callbacks from FDI.


