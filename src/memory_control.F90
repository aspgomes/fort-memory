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

      module allocator_cfg

         use allocator_parameters

         private

         real(kind=kreal), save :: max_mem = -1
         real(kind=kreal), save :: max_mem_soft = -1
         real(kind=kreal), save :: max_mem_hard = -1

         logical, save :: verbose = .false.
         integer(kind=kint), save :: verbosity_level = 1

         public allocator_set_max
         public allocator_get_max

         public allocator_set_quiet
         public allocator_set_verbose
         public allocator_get_verbose
         public allocator_set_verbose_level
!        public allocator_get_verbose_level

         interface allocator_set_max
            module procedure allocator_setmax_i4
            module procedure allocator_setmax_i8
         end interface allocator_set_max

         contains

            subroutine allocator_get_verbose(isVerbose)
               logical, intent(out) :: isVerbose

               isVerbose = verbose 
            end subroutine allocator_get_verbose

            subroutine allocator_set_verbose
               verbose = .true.
            end subroutine allocator_set_verbose

            subroutine allocator_set_quiet
               verbose = .false.
            end subroutine allocator_set_quiet

            subroutine allocator_set_verbose_level(level)
               integer(kind=kint) :: level
               verbosity_level = level
            end subroutine allocator_set_verbose_level

!radovan: weird construct, should it be a function?
!           subroutine allocator_get_verbose_level
!              return verbosity_level
!           end subroutine allocator_get_verbose_level


            subroutine allocator_setmax_i4(size)
               integer(kind=4), intent(in) :: size 
    
               if (size.le.0) then ! there are no limits so we use a large integer
                  max_mem = huge(max_mem) 
               else 
                  max_mem = real(size)*real(kreal)
               endif
            end subroutine allocator_setmax_i4

            subroutine allocator_setmax_i8(size)
               integer(kind=8), intent(in) :: size 
    
               if (size.le.0) then ! there are no limits so we use a large integer
                  max_mem = huge(max_mem) 
               else 
                  max_mem = real(size)*real(kreal)
               endif
            end subroutine allocator_setmax_i8

            subroutine allocator_get_max(max)
               real(kind=kreal), intent(out) :: max

               max = max_mem 
            end subroutine allocator_get_max
  

      end module allocator_cfg
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
