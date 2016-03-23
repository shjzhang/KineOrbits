*
*  subroutine read_dop
*      
      subroutine read_dop()
c
c=======================================================================
c     ****f* SmartPPP/read_dop
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
C     $Id: read_dop.f,v 1.0 2009/08/24 $
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
      real*8        time,sig,sig_x,sig_y,sig_z,sig_dt,GDOP
c
      character*200 line
c
c     loop
c   
      integer       irec
      integer       i,j,k
c
c     common
c
      real*8        dop(MAX_EPO,2)
      integer       ndop
c
      common /dop/  dop, ndop
c
      irec = 0
c
100   continue
c
c     read a line from ipos file
      read(107,'(A200)', end=444) line
c
c     accumulate record number
c
      irec = irec + 1
c
c     read time and position
      read(line,*) 
     &     time,sig,sig_x,sig_y,sig_z,sig_dt,GDOP 
c
      dop(irec,1) = time
      dop(irec,2) = gdop
c
c     write(*,*) (dop(irec,k),k=1,6)
c
c     EPOCH number exceeds MAX_EPO?
c
      if(irec.gt.MAX_EPO)then
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
      ndop = irec 
c
      return
c
      end
