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
! memory allocator module
!
! allocates memory and (optionally) keeps track of memory usage 
!
      module memory_allocator
      
         use allocator_parameters
         use allocator_cfg
         use allocator_track_if
         use allocator_eh

         use allocator_internal_A_1d_C  
         use allocator_internal_A_2d_C  
         use allocator_internal_A_3d_C 
         use allocator_internal_A_4d_C 

         use allocator_internal_A_1d_R  
         use allocator_internal_A_2d_R  
         use allocator_internal_A_3d_R 
         use allocator_internal_A_4d_R 

         use allocator_internal_A_1d_I  
         use allocator_internal_A_2d_I  
         use allocator_internal_A_3d_I 
         use allocator_internal_A_4d_I 

         use allocator_internal_P_1d_C  
         use allocator_internal_P_2d_C  
         use allocator_internal_P_3d_C 
         use allocator_internal_P_4d_C 

         use allocator_internal_P_1d_R  
         use allocator_internal_P_2d_R  
         use allocator_internal_P_3d_R 
         use allocator_internal_P_4d_R 

         use allocator_internal_P_1d_I  
         use allocator_internal_P_2d_I  
         use allocator_internal_P_3d_I 
         use allocator_internal_P_4d_I 
                  
         use allocator_legacy_dirac

         implicit none
         
         private
 
         public alloc
         public dealloc

         public allocp
         public deallocp

         public allocator_set_max
         public allocator_get_max

         public legacy_lwork_get
         public legacy_lwork_set

! making some stuff public
         public allocator_get_available_mem
         public allocator_get_maxsize
         public allocator_get_maxbuff
         public allocator_get_meminuse
         public allocator_get_words_inuse
         public allocator_init
         public allocator_report
         public allocator_cleanup
         public allocator_set_group
         public allocator_get_arraysize

         interface alloc
            module procedure alloc_A_normal_1d_C
            module procedure alloc_A_normal_2d_C
            module procedure alloc_A_normal_3d_C
            module procedure alloc_A_normal_4d_C
            module procedure alloc_A_normal_1d_R
            module procedure alloc_A_normal_2d_R
            module procedure alloc_A_normal_3d_R
            module procedure alloc_A_normal_4d_R
            module procedure alloc_A_normal_1d_I
            module procedure alloc_A_normal_2d_I
            module procedure alloc_A_normal_3d_I
            module procedure alloc_A_normal_4d_I
            module procedure alloc_A_sliced_1d_C
            module procedure alloc_A_sliced_2d_C
            module procedure alloc_A_sliced_3d_C
            module procedure alloc_A_sliced_4d_C
            module procedure alloc_A_sliced_1d_R
            module procedure alloc_A_sliced_2d_R
            module procedure alloc_A_sliced_3d_R
            module procedure alloc_A_sliced_4d_R
            module procedure alloc_A_sliced_1d_I
            module procedure alloc_A_sliced_2d_I
            module procedure alloc_A_sliced_3d_I
            module procedure alloc_A_sliced_4d_I
         end interface alloc

         interface allocp
            module procedure alloc_P_normal_1d_C
            module procedure alloc_P_normal_2d_C
            module procedure alloc_P_normal_3d_C
            module procedure alloc_P_normal_4d_C
            module procedure alloc_P_normal_1d_R
            module procedure alloc_P_normal_2d_R
            module procedure alloc_P_normal_3d_R
            module procedure alloc_P_normal_4d_R
            module procedure alloc_P_normal_1d_I
            module procedure alloc_P_normal_2d_I
            module procedure alloc_P_normal_3d_I
            module procedure alloc_P_normal_4d_I
            module procedure alloc_P_sliced_1d_C
            module procedure alloc_P_sliced_2d_C
            module procedure alloc_P_sliced_3d_C
            module procedure alloc_P_sliced_4d_C
            module procedure alloc_P_sliced_1d_R
            module procedure alloc_P_sliced_2d_R
            module procedure alloc_P_sliced_3d_R
            module procedure alloc_P_sliced_4d_R
            module procedure alloc_P_sliced_1d_I
            module procedure alloc_P_sliced_2d_I
            module procedure alloc_P_sliced_3d_I
            module procedure alloc_P_sliced_4d_I
         end interface allocp


         interface dealloc
            module procedure dealloc_A_1d_C
            module procedure dealloc_A_2d_C
            module procedure dealloc_A_3d_C
            module procedure dealloc_A_4d_C
            module procedure dealloc_A_1d_R
            module procedure dealloc_A_2d_R
            module procedure dealloc_A_3d_R
            module procedure dealloc_A_4d_R
            module procedure dealloc_A_1d_I
            module procedure dealloc_A_2d_I
            module procedure dealloc_A_3d_I
            module procedure dealloc_A_4d_I
         end interface dealloc

         interface deallocp
            module procedure dealloc_P_1d_C
            module procedure dealloc_P_2d_C
            module procedure dealloc_P_3d_C
            module procedure dealloc_P_4d_C
            module procedure dealloc_P_1d_R
            module procedure dealloc_P_2d_R
            module procedure dealloc_P_3d_R
            module procedure dealloc_P_4d_R
            module procedure dealloc_P_1d_I
            module procedure dealloc_P_2d_I
            module procedure dealloc_P_3d_I
            module procedure dealloc_P_4d_I
         end interface deallocp


      end module memory_allocator
