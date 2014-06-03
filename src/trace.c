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
 * $Id: tracing.c 12434 2011-01-11 17:01:37Z gomes $
 *
 * tracing.c : functions to retrieve information from the execution stack and process them into fortran-friendly data
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "trace.h"

#if defined (__GLIBC__) || (__APPLE__ & __MACH__)
#include <execinfo.h>
#elif defined (__BSD_EXECINFO__)
#include "execinfo_bsd.h"
#endif


// function(s) to be called from fortran
void get_traceback_info_(enum allowedActions*, char *fstring, int *fsize);

// auxiliary functions 
void retrieve_symbol(enum retrieveSymbolKinds, char *result, char *backtrace_string);
void print_trace_to_stdout(enum allowedActions, size_t size, char **strings);
void print_trace_to_string(char *symbol_in, char *symbol_out, int length);
void parse_trace(size_t size, char **strings);
size_t findnext_internal_symbols(size_t size, char **strings);


//
// get_traceback_info: will perform the traceback and get whatever information we want and copy it to the result string or prints to stdout
//
// variables:
//    action  : specifies what to retieve and what to do with it
//    result  : when passing information back to the fortran program, *result is the fortran array holding the data
//    fsize   : when passing information back to the fortran program, *fsize holds the size of the "result" array in fortran
//
void get_traceback_info_(enum allowedActions *action, char *fstring, int *fsize)
{
  void *array[MAXDEPTH_TRACE];
  size_t size = 0;
  char **strings = NULL;
  size_t i = 3, j = 1;
  char safe[] = "unknown";

  char symbol_name[MAXSIZE_SYMBOL];

/* we support for the moment tracebacks in the style of glibc, that is, the original gnu one,
   the one available for mac osx, and for bsd systems, that only support to linux for now. for other systems the traceback is yet to be implemented
   mac osx 10.5 should have the same interface */
#if defined (__GLIBC__) || (__APPLE__ & __MACH__) || defined (__BSD_EXECINFO__)
   size    = backtrace (array, MAXDEPTH_TRACE);
   strings = backtrace_symbols (array, size);
#endif 

   if ( strings != NULL ) 
   {
// now we branch off for the different pieces of the trace we want 
// to use 

//  if we are on a mac, reformat the traceback strings
#if (__APPLE__ & __MACH__)
      parse_trace(size,strings);
#endif

      if ( *action == printAllToStdout ) 
         retrieve_symbol(symbolRetAddr,symbol_name,strings[i]);
      else if ( *action == printPrettyTrace || *action == prinRawTrace )
         print_trace_to_stdout(*action,size,strings);
      else if ( *action == printAllocCaller )
      {

// size <= i if, for, instance, part of the traceback information is gone
// due to compiler optimizations. that being the case, we just return a 
// default string

         if ( size > i )
         {

// we will verify whether or not the cleaned-up symbol does start with alloc or
// dealloc; if it does, it is still an internal routine, so we go for the next symbol
// down the stack until that is not the case, so the resulting symbol is that of
// the caller to alloc/dealloc. this was necessary to make it find the correct
// caller, even if the compiler (does/does not) inline code (something that
// would mess up the caller's position on the call stack, since it was taken to always be
// i=3 before) 
//
////        print_trace_to_stdout(prinRawTrace,size,strings);
            j = findnext_internal_symbols(size,strings);

////        printf("now the pretty trace...\n");
////        print_trace_to_stdout(printPrettyTrace,size,strings);

            retrieve_symbol(symbolName,symbol_name,strings[j]);
            print_trace_to_string(symbol_name,fstring,*fsize);
         }
         else
            print_trace_to_string(safe,fstring,*fsize);

      }
      free (strings);
   }
}

//
// auxiliary routines
//

size_t findnext_internal_symbols(size_t size, char **strings)
{
   size_t i, j, next = 1;
   char symbol_name[MAXSIZE_SYMBOL];

//
// j = next = 1 here to skip reference to get_traceback_info_ itself, which is at position 0
// (top of the stack trace)
//
   for (j = next; j < size; j++)
   {
      retrieve_symbol(symbolName,symbol_name,strings[j]);
      if ( (strncmp("alloc",symbol_name,5) == 0 || strncmp("dealloc",symbol_name,7) == 0) )
         next = j+1;
   }

   return next;
}

//   print_trace_to_stdout: prints  information from the trace to stdout, either as it was
//                          returned from the libraries ('raw'), or reformatted to my (bad?) taste ('pretty')
//  
//   action  : whether or not to print 'raw' or 'pretty'
//   size    : depth of the trace
//   strings : information from the trace 
//
void print_trace_to_stdout(enum allowedActions action, size_t size, char **strings)
{
   size_t i, j, entry_point;
   char symbol_name[MAXSIZE_SYMBOL];
 
   if ( action == prinRawTrace )
   { 
      printf ("\nTraceback information:\n\n\tObtained %zd stack frames.\n", size);

      for (i = 0; i < size; i++)
         printf ("\t\t%100s\n", strings[i]);

       printf ("\n");
   }
   else if ( action == printPrettyTrace )
   {
      // minus five below is 2 from the glibc (the lowest 2 on the stack), one from this function,
      // one from get_traceback_info and one from the fortran side (the caller of get_traceback_info) 

      entry_point = 2;
      j = (size - entry_point - 2);

      retrieve_symbol(symbolName,symbol_name,strings[entry_point]);
      printf ("\n Traceback from this point (%s): %zd frames.\n\n",symbol_name,j);
      j--;

      // then start trace from below the caller of get_backtrace_info down the stack 

      for (i = entry_point; i < (size-2); i++)
      {
         retrieve_symbol(symbolName,symbol_name,strings[i]);

         if (strncmp(symbol_name,"",1))
            printf("%3lu   function name:   %30s\n",(unsigned long)j,symbol_name);
         else
            printf("%3lu   function name:   Unknown\n",(unsigned long)j);

         j--;
      }
      printf("\n");
   }
}

//
// parse_trace: process traceback strings coming from the apple implementation
//              TODO: perhaps use it to simplify retrieve_symbol
//
//   size       : depth of the trace
//   strings_in : information from the trace, will return reformatted in a way that
//                retrieve_symbol recognizes 
//
void parse_trace(size_t size, char **strings_in)
{
   size_t i, len, k, junk; 
   int a;
   unsigned int b;
   char string[MAXSIZE_SYMBOL];
   char strings_out[MAXSIZE_SYMBOL];

   for (i = 0; i < size; i++)
   {
      sscanf(strings_in[i], "%d %s %x %s", &a,string,&b,strings_out);
      len = strlen(strings_in[i]); 

      junk = 0;
      for (k = 0; k < len; k++)
         if (strings_out[k] == '_' ) 
            junk++;
         else
            break;

      if ( junk != 0 )
         for (k = 0; k < (len-junk); k++)
            strings_out[k] = strings_out[k+junk];
 
      snprintf(strings_in[i], (len-1), "%d <%s> [%x]\n",a,strings_out,b);
   }
 
}
//
// print_trace_to_string:
//
// symbol_in  :  C string with the already processed (no parenthesis or brackets) trace symbol (function name+entry address or return adress)
// symbol_out :  
// length     : integer that 
//
void print_trace_to_string(char *symbol_in, char *symbol_out, int length)
{
   size_t i,j=0,n;
   
   n = strlen(symbol_in);

   for (i = 0; i < n; i++)
   {
      if (( symbol_in[i] != ' ' )&&( i < length ))
      {
         symbol_out[j] = symbol_in[i];
         j++;
      }
   }
   for (i = j ; i < length; i++)
      symbol_out[i] = ' ';

   symbol_out[length-1] = '\0';
}
//
// retrieve_symbol: gets the function name or address from the traceback string  
// 
// kind             : specifies whether it's the name of the function, or its return address
// result           : a string with a cleaned-up name or return address
// backtrace_string : the original traceback information 
//
void retrieve_symbol(enum retrieveSymbolKinds kind, char *result, char *backtrace_string)
{
  size_t j, k;
  int symbol_name_start, symbol_name_end;
  char element;

  symbol_name_start = -1;
  symbol_name_end   = -1;
  k = 0;

  for ( j = 0 ; j < MAXSIZE_SYMBOL ; j++)
     {
        element = *(backtrace_string+j);

        if ( element == '#'  )
        {
           result[k] = '\0';
           break;
        }

        if ( element == '\0' )
        {
           result[k] = element;
           break;
        }

        if ( ((element == '(')&&(kind == symbolName))    ||
             ((element == '<')&&(kind == symbolName))    ||
             ((element == '[')&&(kind == symbolRetAddr))  )
        {
           symbol_name_start = j+1;
        }
        else if ( ((element == ')')&&(kind == symbolName))   ||
                  ((element == '>')&&(kind == symbolName))   ||
                  ((element == ']')&&(kind == symbolRetAddr)) )
        {
           symbol_name_end   = j-1;
           symbol_name_start = -1;
        }

        if ( (symbol_name_end == -1)&&( j >= symbol_name_start ) )
        {
           result[k] = element;
           k++;
           if ( k == (TYPICAL_FCHAR_LENGTH - 1 ) )
           {
              result[k] = '\0';
              break;
           }
        }
     }
}

