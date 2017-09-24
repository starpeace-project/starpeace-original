/*
 * testfci.h
 *
 * Header file for testfci application.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#include <dos.h>
#include <sys/stat.h>

#include "..\types.h"
#include "..\fdi_int.h"


/*
 * Prototypes in fdiutil.c
 */

char* return_fdi_error_string(FDIERROR err);
