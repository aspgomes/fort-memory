!*
!*
!* Copyright (c) 2008-2011 Andre Severo Pereira Gomes <andre.gomes@univ-lille1.fr>
!* All rights reserved.
!*
!* Redistribution and use in source and binary forms, with or without
!* modification, are permitted provided that the following conditions
!* are met:
!* 1. Redistributions of source code must retain the above copyright
!*    notice, this list of conditions and the following disclaimer.
!* 2. Redistributions in binary form must reproduce the above copyright
!*    notice, this list of conditions and the following disclaimer in the
!*    documentation and/or other materials provided with the distribution.
!*
!* THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
!* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
!* ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
!* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
!* OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
!* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
!* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
!* SUCH DAMAGE.
!*
!*
!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module allocator_parameters

         public

         integer, parameter :: kchar    = 1
#ifdef INT_STAR8
         integer, parameter :: kint     = 8
#else
         integer, parameter :: kint     = 4
#endif
         integer, parameter :: kinteger = kint 

#ifdef ARCH32BIT
         integer, parameter :: klongint = 4
#else
         integer, parameter :: klongint = 8
#endif
         integer, parameter :: kreal    = 8
         integer, parameter :: kcomplex = 8

! error codes, variable kinds

         integer, parameter :: charArrayLength = 132

! cases for the error handled
         integer, parameter :: alreadyAllocated   = 1
         integer, parameter :: alreadyDeallocated = 2
         integer, parameter :: allocateFailure    = 3 
         integer, parameter :: deallocateFailure  = 4 
         integer, parameter :: malformedAllocCall = 5
         integer, parameter :: reqMemoryOverLimit = 6
         integer, parameter :: negativeSizeCall   = 7
! 
! error code
         integer, parameter :: myfunnyerrorcode   = -1212121212

! actions for getting backtrace information
! have to find a better way to keep this in sync with the C part...
                               
         integer, parameter :: printAllToStdout  = 1 
         integer, parameter :: printAllocCaller  = 2
         integer, parameter :: printPrettyTrace  = 3
         integer, parameter :: prinRawTrace      = 4
         
! coversion factor (we keep track of bytes used in each allocation (that would be the size times kind)
! but in printing we give kbyes or mbytes

         real(kind=kreal), parameter  :: b2kbfac = 1024
         real(kind=kreal), parameter  :: b2mbfac = 1048576
 
      end module allocator_parameters
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
