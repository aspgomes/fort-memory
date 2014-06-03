/*
 *
 *
 * Copyright (c) 2008 Andre Severo Pereira Gomes <gomes@few.vu.nl>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *
 * $Id: tracker_util.c 12168 2010-12-13 16:28:43Z gomes $
 *
 * tracker_util.c : functions to retrieve information from memory usage (addresses, amount of free memory)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "trace.h"
#include "fortran_interface.h"

/* using linux's interface to memory */
#if defined (__linux)
#include <sys/sysinfo.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif 


#ifdef ARCH32BIT
void get_memory_address_(void *variable, fortran_int4_t *address);
#else
void get_memory_address_(void *variable, fortran_int8_t *address);
#endif

void get_memory_info_(char *fstring, int *flength);
void get_free_mwords_(size_t *freemw, size_t *verbose);
void get_maximum_mwords_(size_t *maxmw, size_t *verbose);

/* variable: is the real 
   address_string: is a character string with the memory address of variable
*/


#ifdef ARCH32BIT
void get_memory_address_(void *variable, fortran_int4_t *address)
#else
void get_memory_address_(void *variable, fortran_int8_t *address)
#endif
{
   char bufvar[BUFFERSIZE]; 

   errno = 0;

   snprintf (bufvar,(BUFFERSIZE-1), "%p" ,variable);

#ifdef ARCH32BIT
      *address = FORTRAN_STRx_TO_INT4(bufvar);
#else
      *address = FORTRAN_STRx_TO_INT8(bufvar);
#endif

// if ( errno != 0 ) 
//    *(int *)0 = 0;

}

/* reads memory information. note, sysinfo is linux-specific */

void get_memory_info_(char *fstring, int *flength)
{
   unsigned long total_bytes = 1, total_kbytes = 1;
   size_t i, len;
   int status;

#if defined (__linux)
   struct sysinfo info;
   
   status = sysinfo(&info);
   if (status == 0)
   {
      total_bytes  = info.freeram * info.mem_unit;
      total_kbytes = total_bytes / 1024;
   }
#endif

   snprintf(fstring,(size_t)(*flength),"%li Kb",total_kbytes);
   len = strlen(fstring);
   
   if ( *flength > TYPICAL_FCHAR_LENGTH )
      printf ("warning: possible problem with fortran character array size\n"
              "         value passed to get_memory_info() is %d\n"
              "         typical value in the code is however %d\n",
              *flength,TYPICAL_FCHAR_LENGTH);

   if ( len < *flength )
   {
      for ( i = len; i < *flength; i++)
         fstring[i] = ' ';
      fstring[i] = '\0';
   }
}


void get_free_mwords_(size_t *freemw, size_t *verbose)
{
   unsigned long total_bytes = 1, total_kbytes = 0;
   int status;

#if defined (__linux)
   struct sysinfo info;

   status = sysinfo(&info);
   if (status == 0)
   {
      total_bytes  = info.freeram * info.mem_unit;
      total_kbytes = total_bytes / 1024;
   }
   *freemw = (size_t)(total_kbytes/(1024*8)); 

   if (*verbose != 0)
      printf ("Info: getFreeMwords() found %ld (%ld,%ld) mWords avaiable\n",*freemw,*freemw,total_kbytes);
#else
   *freemw = (size_t)(total_kbytes);
#endif /* __linux */
}

void get_maximum_mwords_(size_t *maxmw, size_t *verbose)
{
   unsigned long total_bytes = 1, total_kbytes = 0;
   int status;

#if defined (__linux)
   struct sysinfo info;

   status = sysinfo(&info);
   if (status == 0)
   {
      total_bytes  = info.totalram * info.mem_unit;
      total_kbytes = total_bytes / 1024;
   }
   *maxmw = (size_t)(total_kbytes/(1024*8));

   if (*verbose != 0)
      printf ("Info: get_maximum_mwords() found %ld (%ld,%ld) mWords avaiable\n",*maxmw,*maxmw,total_kbytes);
#else
   *maxmw = (size_t)(total_kbytes);
#endif /* __linux */
}

void stop_as_failure_(void)
{
   exit(EXIT_FAILURE);
}

void stop_as_success_(void)
{
   exit(EXIT_SUCCESS);
}
