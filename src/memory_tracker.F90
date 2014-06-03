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
! memory tracker module
!
! keeps track of memory usage, used as part of the memory allocator
! system 
!
      module memory_tracker

         use allocator_parameters

         implicit none

         private

         integer, parameter :: topListSize = 10

         type         :: memChunk
            integer                        :: n_elements        ! size of the memory, in units
            integer                        :: type              ! type of variable
            integer(kind=klongint)         :: varID             ! type of variable
            character(len=charArrayLength) :: varLabel          ! type of variable
            character(len=charArrayLength) :: allocatedAt       ! type of variable
            type (memChunk), pointer       :: next, prev        ! for the linked list
         end type memChunk

         type         :: memList
            type (memChunk), pointer       :: head
            type (memChunk), pointer       :: tail 
            integer                        :: elements
            real(kind=kreal)               :: nbytes 
         end type memList

         type         :: memInfo
            real(kind=kreal)               :: bytes
            character(len=charArrayLength) :: label 
            character(len=charArrayLength) :: at
         end type memInfo 

         type         :: memTracker
            integer                        :: n_created, n_destroyed
            type (memList)                 :: aL
            type (memList)                 :: dL
            type (memInfo)                 :: peakMem
            type (memInfo)                 :: largeMem(topListSize)
            type (memInfo)                 :: smallMem(topListSize) 
            real(kind=kreal)               :: CurrentMemoryUse
         end type memTracker

         type(memTracker), save               :: memTracking
         character(len=charArrayLength), save :: current_group

         public track_initTracker, track_DestroyTracker
         public track_RegisterAllocation, track_RegisterDeallocation
         public track_PrintStats
         public track_SetCurrentGroup, track_GetCurrentGroup
         public track_getMemoryInuse

         contains
!
!
            subroutine track_RegisterAllocation(size,type,varID,varLabel,trackCaller,trackLevel)
               integer                  :: size, type, trackLevel
               real(kind=kreal)         :: bytes
               integer(kind=klongint)   :: varID
               character(len=*)         :: varLabel, trackCaller
               type (memChunk), pointer :: newChunk

               call track_createNewChunk(newChunk)

               newChunk%n_elements  = size
               newChunk%type        = type
               newChunk%varID       = varID
               newChunk%varLabel    = varLabel
               bytes = real(size)*real(type)
               write (newChunk%allocatedAt,'(a42)') trackCaller

               call track_AddToList(memTracking%aL,newChunk)

               memTracking%n_created        = memTracking%n_created + 1
               memTracking%CurrentMemoryUse = memTracking%CurrentMemoryUse + bytes 

               call track_updateTopLists(memTracking,newChunk)

            end subroutine track_RegisterAllocation
!
! registerDeallocation will ad the information gathered within dealloc
!
            subroutine track_RegisterDeallocation(varID,varLabel,trackCaller,trackLevel)
               integer(kind=klongint)   :: varID
               character(len=*)         :: varLabel
               character(len=*)         :: trackCaller
               integer                  :: trackLevel
               real(kind=kreal)         :: bytes
               type (memChunk), pointer :: tmp

               call track_findEntryFromEnd(varID,tmp,memTracking%aL)

               if ( associated(tmp) ) then
                  bytes = real(tmp%n_elements) * real(tmp%type)
                  memTracking%CurrentMemoryUse = memTracking%CurrentMemoryUse - bytes
                  memTracking%n_destroyed = memTracking%n_destroyed + 1
                  call track_moveEntry(tmp,memTracking%aL,memTracking%dL)
               else
                  write (*,*) '*** dealloc() was not used on a variable allocated with alloc()'
               endif

            end subroutine track_RegisterDeallocation

!
! track_updateTopLists updates the list of maximum and minimum memory usage
!
            subroutine track_updateTopLists(activeTracker,activeChunk) 
               integer                     :: i, j
               real(kind=kreal)            :: bytes
               type (memChunk), intent(in) :: activeChunk
               type(memTracker), intent(inout) :: activeTracker

               bytes = real(activeChunk%n_elements) * real(activeChunk%type)
               if ( bytes.lt.0 ) write (*,*) '*** negative memory usage?'

               if ( activeTracker%peakMem%bytes .le. activeTracker%CurrentMemoryUse ) then 
                  activeTracker%peakMem%bytes = activeTracker%CurrentMemoryUse
                  activeTracker%peakMem%at    = activeChunk%allocatedAt
                  activeTracker%peakMem%label = activeChunk%varLabel
               endif

               do i = 1, topListsize 
                  if ( bytes .le. activeTracker%smallMem(i)%bytes ) then
                     do j = i, topListsize-1
                         activeTracker%smallMem(j+1)%bytes = activeTracker%smallMem(j)%bytes
                         activeTracker%smallMem(j+1)%at    = activeTracker%smallMem(j)%at
                         activeTracker%smallMem(j+1)%label = activeTracker%smallMem(j)%label
                     enddo

                     activeTracker%smallMem(i)%bytes = bytes
                     activeTracker%smallMem(i)%at    = activeChunk%allocatedAt
                     activeTracker%smallMem(i)%label = activeChunk%varLabel
                     goto 111 
                  endif
               enddo

 111           continue

               do i = 1, topListsize
                  if ( bytes .ge. activeTracker%largeMem(i)%bytes ) then
                     do j = topListsize-1, i, -1
                         activeTracker%largeMem(j+1)%bytes = activeTracker%largeMem(j)%bytes
                         activeTracker%largeMem(j+1)%at    = activeTracker%largeMem(j)%at
                         activeTracker%largeMem(j+1)%label = activeTracker%largeMem(j)%label
                     enddo

                     activeTracker%largeMem(i)%bytes = bytes
                     activeTracker%largeMem(i)%at    = activeChunk%allocatedAt
                     activeTracker%largeMem(i)%label = activeChunk%varLabel
                     return
                  endif
               enddo
            end subroutine track_updateTopLists

!
! routines to initialise the tracker, and clean thing up when a tracker isn't 
! needed anymore
!
            subroutine track_initTracker
                integer :: i

                memTracking%n_created    = 0
                memTracking%n_destroyed  = 0

                memTracking%aL%head      => NULL()
                memTracking%aL%tail      => NULL()
                memTracking%aL%nbytes    = 0
                memTracking%aL%elements  = 0

                memTracking%dL%head      => NULL()
                memTracking%dL%tail      => NULL()
                memTracking%dL%nbytes    = 0
                memTracking%dL%elements  = 0

                memTracking%CurrentMemoryUse   = 0

                memTracking%peakMem%bytes  = 0
                memTracking%peakMem%at     = ' '
                memTracking%peakMem%label  = 'unnamed variable'

                do i = 1, topListSize
                   memTracking%smallMem(i)%bytes  = huge(memTracking%peakMem%bytes)
                   memTracking%smallMem(i)%at     = ' '
                   memTracking%smallMem(i)%label  = 'unnamed variable'

                   memTracking%largeMem(i)%bytes  = -1
                   memTracking%largeMem(i)%at     = ' '
                   memTracking%largeMem(i)%label  = 'unnamed variable'
                end do

            end subroutine track_initTracker


            subroutine     track_DestroyTracker
               call track_destroyList(memTracking%aL)
               call track_destroyList(memTracking%dL)
            end subroutine track_DestroyTracker
!
! handling of the so-called "groups", namely a way of setting labels so that
! a set of the (de)allocations are identified for later analysis
!
            subroutine track_SetCurrentGroup(label)
               character(len=charArrayLength), intent(in) :: label
               
               current_group = label
            end subroutine track_SetCurrentGroup

            subroutine track_GetCurrentGroup(label)
               character(len=charArrayLength), intent(out) :: label
               
               label = current_group
               
            end subroutine track_GetCurrentGroup
            
!
! routines to handle displaying the information we've gathered
!

            subroutine track_getMemoryInuse(mem)
               real(kind=kreal), intent(out) :: mem

               mem = memTracking%CurrentMemoryUse 
            end subroutine track_getMemoryInuse

            subroutine track_ShowMemoryUsage(list,title)
               type (memChunk), pointer :: tmp
               type (memList)           :: list
               integer                  :: counter 
               character(len=*)         :: title
               
               counter = 0
               write (*,*) ' '
               write (*,*) ' <<< Tracked variables, ',title,' >>>'
               write (*,*) '     >>> ',list%elements,' variables so far'
           
               tmp => list%head
               
               do
                  if (.not. associated(tmp)) then
                      write (*,*) '     >>> Done after',counter,' el.'
                     exit
                  endif
                  counter = counter + 1
                  call track_ShowContent(tmp)
                  tmp => tmp%next
               end do
            end subroutine track_ShowMemoryUsage
!
! showContent displays the members of the structure used to hold the relevant
! information for the "chunk" of memory we allocated
!
            subroutine track_ShowContent(chunk)
               type (memChunk), pointer :: chunk

               write (*,*) '     >>> (de)allocated data details <<<'
               write (*,*) '         var. name                   : ',chunk%varLabel
               write (*,*) '              kind                   : ',chunk%type
               write (*,*) '              tot.dim. (<1 if array) : ',chunk%n_elements
               write (*,*) '              was allocated at       : ',chunk%allocatedAt
               write (*,*) '              mem.addr. (as integer) : ',chunk%varID
               
            end subroutine track_ShowContent  
!
! printStats display different statistic related to the use of memory
!
            subroutine track_PrintStats(mytid, verbose)
               integer :: mytid, verbose, i, j, actualTopListSize
               real (kind=kreal) :: totalActive
               real (kind=kreal) :: totalFreed 
               real (kind=kreal) :: totalMemory 
               real (kind=kreal) :: meanMemory 
               real (kind=kreal) :: Peak, small, large

               totalActive = nint(memTracking%aL%nbytes/(1024*1024))
               totalFreed  = nint(memTracking%dL%nbytes/(1024*1024))
               totalMemory = totalActive + totalFreed

               meanMemory  = totalMemory/(memTracking%n_created) 

               Peak = nint(memTracking%peakMem%bytes/(1024*1024))

               if (verbose.gt.0) then
                  call track_ShowMemoryUsage(memTracking%aL,'alloc') 
                  call track_ShowMemoryUsage(memTracking%dL,'dealloc') 
               endif

               actualTopListSize = min(topListsize,memTracking%n_created)

               write (*,*) ' '
!              if (mytid.eq.1) then
!               write (*,*) ' Dynamical Memory Usage Summary for Slave(s)'
!              elseif (mytid.eq.0) then 
!               write (*,*) ' Dynamical Memory Usage Summary for Master'
!              endif

               if (mytid.eq.0) then
                write (*,*) ' Dynamical Memory Usage Summary'
                write (*,*) ' '
                write (*,'(a,f12.2)') '  Mean allocation size (Mb) : ',meanMemory
                if (.false.) then
                write (*,*) ' '
                write (*,*) ' Smallest',actualTopListsize,' allocations'
                write (*,*) ' '
                do i = 1, actualTopListsize
                small = memTracking%smallMem(i)%bytes/b2mbfac
                if (small .gt. 1e+8 ) then
                write (*,*) '            n/a'
                else
                write (*,'(f12.2,a,a)') small,                          &
     &          ' Mb at subroutine '//memTracking%smallMem(i)%at(1:42), &
     &          ' for '//memTracking%smallMem(i)%label(1:42) 
                endif
                enddo
                endif ! ends the block set to false, if someone wants to see the smallest allocations it gets enabled again
                write (*,*) ' '
                write (*,*) ' Largest',actualTopListsize,' allocations'
                write (*,*) ' '
                do i = 1, actualTopListSize 
                large = memTracking%largeMem(i)%bytes/b2mbfac
                write (*,'(f12.2,a,a)') large,                          &
     &          ' Mb at subroutine '//memTracking%largeMem(i)%at(1:42), &
     &          ' for '//memTracking%largeMem(i)%label(1:42) 
                 enddo
                write (*,*) ' '
                write (*,'(a,f12.2)') '  Peak memory usage    (Mb) : ',Peak
                write (*,*) '      reached at subroutine : '//memTracking%peakMem%at(1:42)
                write (*,*) '             for variable   : '//memTracking%peakMem%label(1:42)
                write (*,*) ' '

                if ( memTracking%aL%elements .ne. 0 ) then
                  write (*,*) ' Memory possibly left allocated! '
                  write (*,*) '    dumping list now... '
                  call track_ShowMemoryUsage(memTracking%aL,'alloc') 
                  write (*,*) ' '
                  write (*,*) ' There were ',memTracking%n_created,              &
     &                        ' allocations/  ',memTracking%n_destroyed,' deallocations'
                  write (*,*) '     Current memory usage (in Mb) is ',totalActive
                endif

               endif

            end subroutine track_PrintStats
!
! routines for manipulating the data structures (we will use here a doubly linked list) that
! will hold the data over collected over the allocations. one list will be kept for the 
! allocations (from alloc), and onther for the deallocations (from dealloc) 
!
! createNewChunck creates and intitialises a memChunk variable
!
            subroutine     track_createNewChunk(c)
               type (memChunk), pointer :: c

               allocate(c)

               c%n_elements  = 0
               c%type        = 0
               c%varID       = 0
               c%varLabel    = 'None'
               c%allocatedAt = 'None'
               c%next        => NULL()
               c%prev        => NULL()
            end subroutine track_createNewChunk
!
! destroyList empties a given list, deallocation its members
!
            subroutine track_destroyList(list)
               type (memChunk), pointer :: tmp
               type (memList)           :: list

               if (.not.associated(list%tail)) then
                  return
               else if ( associated(list%tail%prev)) then
                  do
                     tmp => list%tail%prev
                     deallocate(list%tail)
                     if (.not. associated(tmp)) then
                        exit
                     endif
                     list%tail => tmp
                  end do
               else if (associated(list%tail)) then
                  deallocate(list%tail)
               endif
            end subroutine track_DestroyList
!
! addToList adds a memChunk variable to a given list, updating the head, tail and so on
!
            subroutine     track_AddToList(list,new)
               type (memChunk), pointer   :: new
               type (memChunk), pointer   :: tmp 
               type (memList)             :: list 
               real(kind=kreal)           :: bytes

               tmp => list%head

               if ( .not. associated(tmp)) then
                  list%head => new
                  list%tail => new
                  list%elements = 1
                  bytes = real(new%n_elements) * real(new%type)
                  list%nbytes = list%nbytes + bytes
               else
                  do 
                    if (associated(tmp%next)) then
                       tmp => tmp%next
                    else
                       tmp%next => new
                       new%prev => tmp
                       list%tail => new
                       list%elements = list%elements + 1
                       bytes = real(new%n_elements) * real(new%type)
                       list%nbytes = list%nbytes + bytes
                       exit 
                    endif 
                  enddo
               endif
            end subroutine track_addToList
!
! findEntryFromEnd will walk on the list, starting from the end, since it's more likely
! that something allocated last will be deallocated first. 
! 
            subroutine     track_findEntryFromEnd(key,entry,list)
               type (memChunk), pointer   :: entry
               type (memChunk), pointer   :: tmp 
               type (memList)             :: list 
               integer(kind=klongint)     :: key

               tmp => list%tail

               do
                  if (key .eq. tmp%varID) then
                     entry => tmp
                     exit
                  endif

                  if (associated(tmp%prev)) then 
                     tmp => tmp%prev 
                  else
                     write (*,*) '*** label',key,' not found!'
                     entry => NULL()
                     return
                  endif
               end do

            end subroutine track_findEntryFromEnd 
!
! clone will copy the information from one memChunk variable to another, allocating the latter
!
            subroutine     track_clone(src,dst)
               type (memChunk), pointer   :: src, dst

               call track_createNewChunk(dst)
               
               dst%n_elements  = src%n_elements
               dst%type        = src%type 
               dst%varID       = src%varID 
               dst%varLabel    = src%varLabel 
               dst%allocatedAt = src%allocatedAt 

            end subroutine track_clone
!
! deleteFromList removes a given entry from a list, updating the references of the 
! elements immediately linked to it 
! 
            subroutine     track_deleteFromList(list,entry)
               type (memChunk), pointer   :: entry, prev, next, head, tail
               type (memList)             :: list
               real(kind=kreal)           :: bytes

               head => list%head
               tail => list%tail

               if ( list%elements .eq. 1 ) then
                  list%head => NULL()
                  list%tail => NULL()
                  list%elements = 0
                  list%nbytes   = 0
                  deallocate(entry)
                  return

               else if ( list%head%varID .eq. entry%varID ) then
                  head      => entry%next
                  head%prev => NULL()
               else if ( list%tail%varID .eq. entry%varID ) then
                  tail      => entry%prev
                  tail%next => NULL()
               else
                  prev => entry%prev 
                  next => entry%next 
                  entry%prev%next => next
                  entry%next%prev => prev
               endif 

               list%head => head 
               list%tail => tail

               list%elements = list%elements - 1
               bytes = real(entry%n_elements) * real(entry%type)
               list%nbytes   = list%nbytes - bytes 

               deallocate(entry)

            end subroutine track_deleteFromList
!
! moveEntry will move a given element from one list to another
!
            subroutine     track_moveEntry(entry,listSrc,listDst)
               type (memChunk), pointer   :: entry
               type (memChunk), pointer   :: tmp_src, tmp_dst 
               type (memList)             :: listSrc,listDst 

               tmp_src => listSrc%tail

               do 
                  if (tmp_src%varID .eq. entry%varID) then
                     call track_clone(entry,tmp_dst)
                     call track_AddToList(listDst,tmp_dst)
                     call track_deleteFromList(listSrc,tmp_src)
                     exit
                  endif

                  if (associated(tmp_src%prev)) then
                     tmp_src => tmp_src%prev
                  else
                     write (*,*) '*** no moving to do!'
                  endif
               end do

            end subroutine track_moveEntry 

     end module memory_tracker
