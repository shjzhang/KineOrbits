*
*  subroutine read_pos
*      
      subroutine read_pos()
c
c=======================================================================
c     ****f* SmartPPP/read_pos
c
c   FUNCTION   
c   
c     read input receiver's inital position, which are used to 
c     accelerate the solution and reduce the iteration time
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: read_pos.f,v 1.0 2009/08/10 $
c=======================================================================
*         
      implicit none
c
      include       '../../include/igs.h'
      include       '../../include/atx.h'
      include       '../../include/rinex.h'
      include       '../../include/rinex.conf.h'
      include       '../../include/SmartPPP.h'
      include       '../../include/SmartPPP.conf.h'
c
c     local
c
      character*200 line
      real*8        time,x,y,z,dt
c
c     loop
c   
      integer       iepo 
      integer       i,j,k
c
c     common
c
      real*8        POS(MAX_EPO,4)
      integer       nrec_pos
c
      common /ini/  pos, NREC_POS
c
      do iepo=1, MAX_EPO
         POS(iepo,1) = 0.0d0
         POS(iepo,2) = 0.0d0
         POS(iepo,3) = 0.0d0
         POS(iepo,4) = 0.0d0
      enddo
c
      iepo = 0
c
100   continue
c
c     read a line from ipos file
      read(105,'(A200)', end=444) line
c
c     accumulate record number
c
      iepo = iepo + 1
c
c     read time and position
      read(line,*) time,x,y,z,dt 
c
      POS(iepo,1) = time
      POS(iepo,2) = x
      POS(iepo,3) = y
      POS(iepo,4) = z
c
c     EPOCH number exceeds MAX_EPO?
c
      if(iepo.gt.MAX_EPO)then
         write(*,*) ' read_att  '
         write(*,*) ' please enlarge the MAX_REC_ATT'
         stop
      endif
c
c     read next line
c
      goto 100
c
c     total record number
c
444   continue
c 
      NREC_POS = iepo 
c
      return
c
      end
