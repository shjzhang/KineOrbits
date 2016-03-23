*
*  subroutine read_svr
*      
      subroutine read_svr()
c
c=======================================================================
c     ****f* SmartPPP/read_svr
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
C     $Id: read_svr.f,v 1.0 2009/08/24 $
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
      real*8        time0,time1,xv,yv,zv,dtv
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
      real*8        svr(MAX_EPO,6)
      integer       nrec_svr
c
      common /svr/  svr, nrec_svr
c
      irec = 0
c
100   continue
c
c     read a line from ipos file
      read(106,'(A200)', end=444) line
c
c     accumulate record number
c
      irec = irec + 1
c
c     read time and position
      read(line,*) time0,time1,xv,yv,zv,dtv 
c
      svr(irec,1) = time0
      svr(irec,2) = time1
      svr(irec,3) = xv
      svr(irec,4) = yv
      svr(irec,5) = zv
      svr(irec,6) = dtv
c
c     write(*,*) (svr(irec,k),k=1,6)
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
      NREC_svr = irec 
c
      return
c
      end
