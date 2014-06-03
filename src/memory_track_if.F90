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
! $Id: memory_allocator.F90 9830 2009-10-30 04:37:36Z gomes $
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! memory allocator module
!
! allocates memory and (optionally) keeps track of memory usage 
!
      module allocator_track_if
      
         use allocator_parameters
         use allocator_cfg
         use memory_tracker
                  
         implicit none
         
         private
 
         public allocator_get_available_mem
         public allocator_get_maxsize
         public allocator_get_maxbuff
         public allocator_get_arraysize
         public allocator_get_meminuse
         public allocator_get_words_inuse
         public allocator_init
         public allocator_report
         public allocator_cleanup
         public allocator_set_group
         public allocator_track
         public allocator_withinRange

         interface allocator_track
            module procedure allocator_registerAllocation
            module procedure allocator_registerDeallocation
         end interface allocator_track

         interface allocator_get_maxsize
            module procedure allocator_get_max_arraysize_i8
            module procedure allocator_get_max_arraysize_i4
         end interface allocator_get_maxsize 

         interface allocator_get_maxbuff
            module procedure allocator_get_max_buffsize_i8
            module procedure allocator_get_max_buffsize_i4
         end interface allocator_get_maxbuff

         interface allocator_get_words_inuse
            module procedure allocator_get_words_inuse_i4
            module procedure allocator_get_words_inuse_i8
         end interface allocator_get_words_inuse

         interface allocator_get_arraysize
            module procedure allocator_get_arrayindex_size_i4
            module procedure allocator_get_arrayindex_size_i8
         end interface allocator_get_arraysize
         contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! interface to the tracker
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine allocator_init
          character(len=charArrayLength) :: label 

          write (label,'(a)') 'main program' 

          call track_initTracker
          call track_SetCurrentGroup(label)
      end subroutine allocator_init


      subroutine allocator_set_group(label)
         character(len=charArrayLength), intent(in) :: label
         
         call track_SetCurrentGroup(label)
      end  subroutine allocator_set_group


      logical function allocator_withinRange(size,kind)
         integer :: size, kind 
         real(kind=kreal) :: requestedMem, inuseMem, totalMem, maxMem

         requestedMem = real(size)*real(kind)
         call track_getMemoryInuse(inuseMem)
         call allocator_get_max(maxMem)

         totalMem = requestedMem + inuseMem

         if ( totalMem .le. maxMem ) then
            allocator_withinRange = .true.
         else
            allocator_withinRange = .false.
         endif

      end function allocator_withinRange


      subroutine allocator_report(mytid,verbose)
         integer :: mytid, verbose
         call track_PrintStats(mytid, verbose)
      end subroutine allocator_report
      
      
      subroutine allocator_registerAllocation(size, kind,varAddr,varLabel)
         integer                        :: size
         integer                        :: kind
         integer                        :: traceStackDepth = -1
         integer(kind=klongint)         :: varAddr 
         character(len=charArrayLength) :: trace_result
         character(len=charArrayLength) :: varLabel

#ifndef GFORTRAN_ALLOCATOR_WORKAROUND
         trace_result = ' '
         call get_traceback_info (2,trace_result,charArrayLength,traceStackDepth)
         call track_RegisterAllocation(size,kind,varAddr,varLabel,&
     &                                 trace_result,traceStackDepth) 
#endif

      end subroutine allocator_registerAllocation
      

      subroutine allocator_registerDeallocation(varAddr,varLabel)
         integer                        :: size
         integer                        :: kind
         integer                        :: traceStackDepth = -1
         integer(kind=klongint)         :: varAddr 
         character(len=charArrayLength) :: trace_result
         character(len=charArrayLength) :: varLabel

#ifndef GFORTRAN_ALLOCATOR_WORKAROUND
         trace_result = ' '
         call get_traceback_info (2,trace_result,charArrayLength,traceStackDepth)
         call track_RegisterDeallocation(varAddr,varLabel,&
     &                                   trace_result,traceStackDepth) 
#endif

      end subroutine allocator_registerDeallocation
!     
!
!
      subroutine allocator_cleanup
         call track_DestroyTracker
      end subroutine allocator_cleanup
!
!
!
!
      subroutine allocator_get_arrayindex_size_i4(size,memory,kind)
         integer, intent(in)          :: kind
         integer(kind=4), intent(out) :: size 
         real(kind=kreal), intent(in) :: memory

         size = nint(memory/real(kind))

      end subroutine allocator_get_arrayindex_size_i4

      subroutine allocator_get_arrayindex_size_i8(size,memory,kind)
         integer, intent(in)          :: kind
         integer(kind=8), intent(out) :: size 
         real(kind=kreal), intent(in) :: memory

         size = nint(memory/real(kind))
      end subroutine allocator_get_arrayindex_size_i8
!
! will give the size of
!
      subroutine allocator_get_max_arraysize_i8(maxindex,kind_to_use)
         integer, intent(in)    :: kind_to_use
         integer(kind=8)        :: maxindex, hugemax
         integer(kind=klongint) :: maxindex_long
         real(kind=kreal)       :: possible_mem

         call allocator_get_available_mem(possible_mem) 

         maxindex_long = nint(possible_mem/real(kind_to_use))
         hugemax       = huge(maxindex) 

         if ( maxindex_long < hugemax ) then 
            maxindex = maxindex_long
         else
            maxindex = -1
         endif

       end subroutine allocator_get_max_arraysize_i8
!
!
      subroutine allocator_get_max_arraysize_i4(maxindex,kind_to_use)
         integer, intent(in)    :: kind_to_use
         integer(kind=4)        :: maxindex, hugemax
         integer(kind=klongint) :: maxindex_long
         real(kind=kreal)       :: possible_mem

         call allocator_get_available_mem(possible_mem) 

         maxindex_long = nint(possible_mem/real(kind_to_use))
         hugemax       = huge(maxindex) 

         if ( maxindex_long < hugemax ) then 
            maxindex = maxindex_long
         else
            maxindex = -1
         endif

       end subroutine allocator_get_max_arraysize_i4
!
!
!
      subroutine allocator_get_max_buffsize_i8(buffsize,kind_to_use)
         integer, intent(in)    :: kind_to_use
         integer(kind=8)        :: buffsize, hugebuf
         real(kind=kreal)       :: possible_mem, maxindex_real, buffsize_real

         call allocator_get_available_mem(possible_mem)

         hugebuf       = huge(buffsize)
         maxindex_real = possible_mem/real(kind_to_use,kind(possible_mem))
         buffsize_real = real(hugebuf,kind(buffsize_real))

         if ( maxindex_real < buffsize_real ) then
            buffsize = nint(maxindex_real)
         else
            buffsize = hugebuf
         endif

         buffsize = (buffsize / 10) * 9

      end subroutine allocator_get_max_buffsize_i8
!
!
      subroutine allocator_get_max_buffsize_i4(buffsize,kind_to_use)
         integer, intent(in)    :: kind_to_use
         integer(kind=4)        :: buffsize, hugebuf
         real(kind=kreal)       :: possible_mem, maxindex_real, buffsize_real

         call allocator_get_available_mem(possible_mem)

         hugebuf       = huge(buffsize)
         maxindex_real = possible_mem/real(kind_to_use,kind(possible_mem))
         buffsize_real = real(hugebuf,kind(buffsize_real))

         if ( maxindex_real < buffsize_real ) then
            buffsize = nint(maxindex_real)
         else
            buffsize = hugebuf
         endif

         buffsize = (buffsize / 10) * 9

      end subroutine allocator_get_max_buffsize_i4
!
!
      subroutine allocator_get_meminuse(mem)
         real(kind=kreal), intent(out) :: mem

         call track_getMemoryInuse(mem)
      end subroutine allocator_get_meminuse
!
!
      subroutine allocator_get_available_mem(available)
         real(kind=kreal), intent(inout) :: available
         real(kind=kreal)       :: ceil_mem, used_mem

         call track_getMemoryInuse(used_mem)
         call allocator_get_max(ceil_mem)

         available = ceil_mem - used_mem
         if ( available .lt. 1 ) available = 0

      end subroutine allocator_get_available_mem
!
!
      subroutine allocator_get_words_inuse_i8(mem_in_words)
         real(kind=kreal)                    :: mem
         integer(kind=8), intent(out)        :: mem_in_words

         call track_getMemoryInuse(mem)
         mem_in_words = nint(mem/real(kind(mem)),kind(8))
      end subroutine allocator_get_words_inuse_i8
!
!
      subroutine allocator_get_words_inuse_i4(mem_in_words)
         real(kind=kreal)                    :: mem
         integer(kind=4), intent(out)        :: mem_in_words

         call track_getMemoryInuse(mem)
         mem_in_words = nint(mem/real(kind(mem)),kind(4))
      end subroutine allocator_get_words_inuse_i4
!
      end module allocator_track_if
