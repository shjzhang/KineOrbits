*
*  subroutine read_att
*      
      subroutine read_att()
c
c=======================================================================
c     ****f* SmartPPP/read_clk
c
c   FUNCTION   
c   
c     read IGS clock data from file.
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
C     $Id: read_clk.f,v 1.0 2009/07/29 $
c=======================================================================
*         
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
      include      '../../include/igs.h'
*         
c
      character*001 rcv_id
*
*     local variable
*     
      integer       sca_id, qualflag   
      integer       i, j, k, ios
      integer       irec
c
      character*100 line
      character*030 flag
c
c     common
c
      integer       NREC_ATT
      real*8        ATT(MAX_REC_ATT, 6)
c
      common /att/  att,NREC_ATT 
c
c     initialization
c
      irec = 1
c
c     attittude file header
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
100   continue
c
      read(104, '(A80)', end=444)  line
c
c     header file flags
c      
      flag = line(1:30)
c
c     end of header file
c
      if(trim(flag).eq.'END OF HEADER')then
c
        goto 200
c
      endif
c
      goto 100
c
c     read the ATT data
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
200   continue
c
      read(104, *, end=444)
     +     ATT(irec,1), rcv_id,      sca_id, 
     +     ATT(irec,2), ATT(irec,3), ATT(irec,4),  
     +     ATT(irec,5), ATT(irec,6), qualflag  
c
c     accumulate record number
c
      irec = irec + 1
c
c     record number exceeds Max_nrec_sac?
c
      if(irec.gt.MAX_REC_ATT)then
         write(*,*) ' read_att  '
         write(*,*) ' please enlarge the MAX_REC_ATT'
         stop
      endif
c
c     read next line
c
      goto 200
c
c     total record number
c
444   continue
c 
      NREC_ATT = irec - 1
c
      return
c
      end
