
c
c   subroutine write_pos
c
      subroutine write_pos()
c
c=======================================================================
c     ****f* fmttrs/write_pos
c
c   FUNCTION   
c   
c
C   ARGUMENTS
C   
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   History
c
c
c     ***
c
C     $Id: write_pos.f,v 1.0 2010/11/22 $
c=======================================================================
c
      implicit none

      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/igs.h'
c
      integer       i,j,k
      integer       iPRN
c
c     Declaration_of_the_Local_Variables
c
      integer       NREC_SP3(MAX_PRN) 
      REAL*8        EPH(MAX_PRN, MAX_REC_SP3, 4)
c
      common /SP3/  NREC_SP3, EPH
c
c     Warning, only output the first PRN's ephemeris data

      iPRN = 15
c
      write(*,*) iPRN, NREC_SP3(iPRN)
c
      do i=1,NREC_SP3(iPRN)
         write(201,'(F20.8, 4F14.3)'), 
     &   EPH(iPRN,i,1), 
     &   EPH(iPRN,i,2)*1000.0d0,
     &   EPH(iPRN,i,3)*1000.0d0,
     &   EPH(iPRN,i,4)*1000.0d0,
     &   0.0
c        write(*,'(5F14.3)'), 
c    &   EPH(iPRN,i,1) , EPH(iPRN,i,2), EPH(iPRN,i,3), EPH(iPRN,i,4)
      enddo

c
      return
c
      end
