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

#include "..\types.h"
#include "..\fci_int.h"


/*
 * Prototypes in fciutil.c
 */
void	strip_path(char *filename, char *stripped_name);
int		get_percentage(unsigned long a, unsigned long b);
char	*return_fci_error_string(FCIERROR err);
