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
      module allocator_eh
      
         use allocator_parameters
         use allocator_cfg
         use allocator_track_if

         implicit none
         
         private 

         public allocator_errorHandler

         public varIsAliveP
         public varIsAliveA

         interface varIsAliveP
            module procedure cPointerIsAlive_1d
            module procedure rPointerIsAlive_1d
            module procedure iPointerIsAlive_1d
            module procedure cPointerIsAlive_2d
            module procedure rPointerIsAlive_2d
            module procedure iPointerIsAlive_2d
            module procedure cPointerIsAlive_3d
            module procedure rPointerIsAlive_3d
            module procedure iPointerIsAlive_3d
            module procedure cPointerIsAlive_4d
            module procedure rPointerIsAlive_4d
            module procedure iPointerIsAlive_4d
         end interface varIsAliveP

         interface varIsAliveA
            module procedure cAllocatableIsAlive_1d
            module procedure rAllocatableIsAlive_1d
            module procedure iAllocatableIsAlive_1d
            module procedure cAllocatableIsAlive_2d
            module procedure rAllocatableIsAlive_2d
            module procedure iAllocatableIsAlive_2d
            module procedure cAllocatableIsAlive_3d
            module procedure rAllocatableIsAlive_3d
            module procedure iAllocatableIsAlive_3d
            module procedure cAllocatableIsAlive_4d
            module procedure rAllocatableIsAlive_4d
            module procedure iAllocatableIsAlive_4d
         end interface varIsAliveA

         contains


            subroutine allocator_errorHandler(status,size,data_kind,var_id,ignoreError)
               integer                        :: status
! optional arguments
               logical, optional              :: ignoreError
               integer, optional              :: size
               integer, optional              :: data_kind
               character(len=*), optional     :: var_id 
! local variables
               logical                        :: ignoreErrorLocal, isFatal, isVerbose
               integer                        :: kbytesRequested, sizeLocal, kindLocal
               character(len=charArrayLength) :: ErrorMessage
               character(len=charArrayLength) :: InfoMessage(10)
               character(len=charArrayLength) :: trace_result
               character(len=charArrayLength) :: kbytesAvailable
               character(len=1)               :: dummy
               real(kind=kreal)               :: mbytesInUse, mbytesRequested, mbytesMax
               integer                        :: nlines_info

               isFatal = .True.
               call allocator_get_verbose(isVerbose)

               if (present(ignoreError)) then 
                  ignoreErrorLocal = ignoreError
               else
                  ignoreErrorLocal = .False.
               endif

               if (present(size)) then 
                  sizeLocal = size
               else
                  sizeLocal = 0
               endif

               if (present(data_kind)) then
                  kindLocal = data_kind
               else
                  kindLocal = 4
               endif

               if (present(var_id).and.isVerbose) then
                  if (var_id.ne.'None') then
                     write (*,*) 'Warning: at allocation for var. id '//var_id(1:42)
                  endif
               endif
 
               write (InfoMessage,  '(a)') 'Unknown error!'
               write (ErrorMessage, '(a)') 'Unknown error!'
               write (trace_result, '(a)') 'unknown (stack trace info unavailable)'

               call get_traceback_info (printAllocCaller,trace_result,charArrayLength)
               
               mbytesRequested = real(sizeLocal)*real(kindLocal)/b2mbfac
               call allocator_get_meminuse(mbytesInUse)  
               mbytesInUse = mbytesInUse/b2mbfac

               call allocator_get_max(mbytesMax)
               mbytesMax   = mbytesMax/b2mbfac

               nlines_info = 1

               select case (status)
                  case (alreadyAllocated)      
                     write (InfoMessage, '(a)') 'Trying to allocate variable already allocated.'
                     write (ErrorMessage,'(a)') 'Fatal error on memory allocation at '//trace_result(1:42)
                     isFatal = .True.

                  case (alreadyDeallocated)     
                     write (InfoMessage, '(a)') 'Trying to deallocate variable already deallocated.'
                     write (ErrorMessage,'(a)') 'Non-fatal error on memory deallocation at '//trace_result(1:42)
                     isFatal = .False.
                  
                  case (allocateFailure)
                     write (InfoMessage(1),'(a,i7,a)')                                                &
     &                     'Not enough memory available when attempting to allocate ',                &
     &                      nint(mbytesRequested),                                                    &
     &                     ' Mb in subroutine '//trace_result(1:42)
                     write (InfoMessage(2),'(a)') ' '                                                  
                     write (InfoMessage(3),'(a)') 'Please verify whether:'                                                  
                     write (InfoMessage(4),'(a)')                                                     &
     &                     ' - the amount of memory per cpu you wish to use is compatible with that'//   &
     &                     ' installed in the system'
                     write (InfoMessage(5),'(a)')                                                     &
     &                     ' - the shell/queueing system/OS place any constraints on the memory available.'
                     nlines_info = 5

                     write (ErrorMessage,'(a)') 'Fatal memory allocation error at '//trace_result(1:42)
                     isFatal = .True.

                  case (deallocateFailure)
                     write (InfoMessage, '(a)') 'Memory deallocation failed at '//trace_result(1:42)
                     write (ErrorMessage,'(a)') 'Non-fatal memory deallocation error at '//trace_result(1:42)
                     isFatal = .False.

                  case (malformedAllocCall)
                     write (InfoMessage, '(a)') 'Malformed call to alloc(). Please submit a bug report.'
                     write (ErrorMessage,'(a)') 'Fatal memory allocation error at '//trace_result(1:42)
                     isFatal = .True.

                  case (reqMemoryOverLimit)
                     write (InfoMessage(1),'(a)')                                                     &
     &                  'Error when allocating memory at subroutine:'//trace_result(1:42)

                     write (InfoMessage(2),'(a)') ' '                                                  
                     write (InfoMessage(3),'(a,i7,a)')                                                &
     &                  'adding the      Memory requested in current allocation          :',          &
     &                  nint(mbytesRequested),                                                        &
     &                  ' Mb'

                     write (InfoMessage(4),'(a,i7,a)')                                                &
     &                  'to the          Current dynamical memory usage                  :',          &
     &                  nint(mbytesInUse),                                                            &
     &                  ' Mb'

                     write (InfoMessage(5),'(a,i7,a)')                                                &
     &                  'will exceed the User-defined dynamically allocated memory limit :',          &
     &                  nint(mbytesMax),                                                              &
     &                  ' Mb' 

                     write (InfoMessage(6),'(a)') ' '                                                  
                     write (InfoMessage(7),'(a)')                                                     &
     &                  'Please consult the user manual for information on how to solve this issue.'

                     nlines_info = 7

                     write (ErrorMessage,'(a)') 'Fatal memory allocation error at '//trace_result(1:42)
                     isFatal = .True.

                  case (negativeSizeCall)
                     write (InfoMessage, '(a)')                                    & 
     &                     'Negative array dimension (e.g. a value larger than '// &
     &                     'what the variable can hold). Allocation is not possible.'
                     write (ErrorMessage,'(a)') 'Fatal memory allocation error at '//trace_result(1:42)
                     isFatal = .True.

               end select

               if (isFatal) then
                  if (ignoreErrorLocal) then
                     if (isVerbose) &
     &               write (*,*) 'Non-fatal error at '//trace_result(1:42)//'. The program will continue.'
                  else
                     call quit(InfoMessage,ErrorMessage,nlines_info)
                  endif
               else
                  if (isVerbose) then
                     write (*,*) 'Non-fatal error at '//trace_result(1:42)
                     write (*,*) InfoMessage
                     write (*,*) 'The program will continue.'
                  endif
               endif
               
            end subroutine allocator_errorHandler


            subroutine quit(info,error,nlines)
               character(len=charArrayLength) :: info(10)
               character(len=charArrayLength) :: error
               integer :: i,nlines
   
               do i = 1, nlines
                  write (*,*) info(i)
               enddo 
               write (*,*) ' '
               write (*,*) error
               call stop_as_failure

            end subroutine quit

!
! other error-handling related routines
!

!
! 1d-objects
!
            logical function cPointerIsAlive_1d(data)
               complex(kind=kcomplex), pointer :: data(:)

               cPointerIsAlive_1d = .false.
               if ( associated(data) ) cPointerIsAlive_1d = .true.
            end function cPointerIsAlive_1d

            logical function rPointerIsAlive_1d(data)
               real(kind=kreal), pointer :: data(:)

               rPointerIsAlive_1d = .false.
               if ( associated(data) ) rPointerIsAlive_1d = .true.
            end function rPointerIsAlive_1d

            logical function iPointerIsAlive_1d(data)
               integer(kind=kint), pointer :: data(:)

               iPointerIsAlive_1d = .false.
               if ( associated(data) ) iPointerIsAlive_1d = .true.
            end function iPointerIsAlive_1d

            logical function cAllocatableIsAlive_1d(data)
               complex(kind=kcomplex), allocatable :: data(:)

               cAllocatableIsAlive_1d = .false.
               if ( allocated(data) ) cAllocatableIsAlive_1d = .true.
            end function cAllocatableIsAlive_1d

            logical function rAllocatableIsAlive_1d(data)
               real(kind=kreal), allocatable :: data(:)

               rAllocatableIsAlive_1d = .false.
               if ( allocated(data) ) rAllocatableIsAlive_1d = .true.
            end function rAllocatableIsAlive_1d

            logical function iAllocatableIsAlive_1d(data)
               integer(kind=kint), allocatable :: data(:)

               iAllocatableIsAlive_1d = .false.
               if ( allocated(data) ) iAllocatableIsAlive_1d = .true.
            end function iAllocatableIsAlive_1d
!
! 2d-objects
!
            logical function cPointerIsAlive_2d(data)
               complex(kind=kcomplex), pointer :: data(:,:)

               cPointerIsAlive_2d = .false.
               if ( associated(data) ) cPointerIsAlive_2d = .true.
            end function cPointerIsAlive_2d

            logical function rPointerIsAlive_2d(data)
               real(kind=kreal), pointer :: data(:,:)

               rPointerIsAlive_2d = .false.
               if ( associated(data) ) rPointerIsAlive_2d = .true.
            end function rPointerIsAlive_2d

            logical function iPointerIsAlive_2d(data)
               integer(kind=kint), pointer :: data(:,:)

               iPointerIsAlive_2d = .false.
               if ( associated(data) ) iPointerIsAlive_2d = .true.
            end function iPointerIsAlive_2d

            logical function cAllocatableIsAlive_2d(data)
               complex(kind=kcomplex), allocatable :: data(:,:)

               cAllocatableIsAlive_2d = .false.
               if ( allocated(data) ) cAllocatableIsAlive_2d = .true.
            end function cAllocatableIsAlive_2d

            logical function rAllocatableIsAlive_2d(data)
               real(kind=kreal), allocatable :: data(:,:)

               rAllocatableIsAlive_2d = .false.
               if ( allocated(data) ) rAllocatableIsAlive_2d = .true.
            end function rAllocatableIsAlive_2d

            logical function iAllocatableIsAlive_2d(data)
               integer(kind=kint), allocatable :: data(:,:)

               iAllocatableIsAlive_2d = .false.
               if ( allocated(data) ) iAllocatableIsAlive_2d = .true.
            end function iAllocatableIsAlive_2d

!
! 3d-objects
!
            logical function cPointerIsAlive_3d(data)
               complex(kind=kcomplex), pointer :: data(:,:,:)

               cPointerIsAlive_3d = .false.
               if ( associated(data) ) cPointerIsAlive_3d = .true.
            end function cPointerIsAlive_3d

            logical function rPointerIsAlive_3d(data)
               real(kind=kreal), pointer :: data(:,:,:)

               rPointerIsAlive_3d = .false.
               if ( associated(data) ) rPointerIsAlive_3d = .true.
            end function rPointerIsAlive_3d

            logical function iPointerIsAlive_3d(data)
               integer(kind=kint), pointer :: data(:,:,:)

               iPointerIsAlive_3d = .false.
               if ( associated(data) ) iPointerIsAlive_3d = .true.
            end function iPointerIsAlive_3d

            logical function cAllocatableIsAlive_3d(data)
               complex(kind=kcomplex), allocatable :: data(:,:,:)

               cAllocatableIsAlive_3d = .false.
               if ( allocated(data) ) cAllocatableIsAlive_3d = .true.
            end function cAllocatableIsAlive_3d

            logical function rAllocatableIsAlive_3d(data)
               real(kind=kreal), allocatable :: data(:,:,:)

               rAllocatableIsAlive_3d = .false.
               if ( allocated(data) ) rAllocatableIsAlive_3d = .true.
            end function rAllocatableIsAlive_3d

            logical function iAllocatableIsAlive_3d(data)
               integer(kind=kint), allocatable :: data(:,:,:)

               iAllocatableIsAlive_3d = .false.
               if ( allocated(data) ) iAllocatableIsAlive_3d = .true.
            end function iAllocatableIsAlive_3d

!
! 4d-objects
!
            logical function cPointerIsAlive_4d(data)
               complex(kind=kcomplex), pointer :: data(:,:,:,:)

               cPointerIsAlive_4d = .false.
               if ( associated(data) ) cPointerIsAlive_4d = .true.
            end function cPointerIsAlive_4d

            logical function rPointerIsAlive_4d(data)
               real(kind=kreal), pointer :: data(:,:,:,:)

               rPointerIsAlive_4d = .false.
               if ( associated(data) ) rPointerIsAlive_4d = .true.
            end function rPointerIsAlive_4d

            logical function iPointerIsAlive_4d(data)
               integer(kind=kint), pointer :: data(:,:,:,:)

               iPointerIsAlive_4d = .false.
               if ( associated(data) ) iPointerIsAlive_4d = .true.
            end function iPointerIsAlive_4d

            logical function cAllocatableIsAlive_4d(data)
               complex(kind=kcomplex), allocatable :: data(:,:,:,:)

               cAllocatableIsAlive_4d = .false.
               if ( allocated(data) ) cAllocatableIsAlive_4d = .true.
            end function cAllocatableIsAlive_4d

            logical function rAllocatableIsAlive_4d(data)
               real(kind=kreal), allocatable :: data(:,:,:,:)

               rAllocatableIsAlive_4d = .false.
               if ( allocated(data) ) rAllocatableIsAlive_4d = .true.
            end function rAllocatableIsAlive_4d

            logical function iAllocatableIsAlive_4d(data)
               integer(kind=kint), allocatable :: data(:,:,:,:)

               iAllocatableIsAlive_4d = .false.
               if ( allocated(data) ) iAllocatableIsAlive_4d = .true.
            end function iAllocatableIsAlive_4d

      end module allocator_eh
