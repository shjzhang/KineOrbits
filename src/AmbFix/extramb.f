c
c   subroutine extramb
c
      subroutine extramb(isat,NREC_iSAT, 
     &                   raw_epo,raw_flag,raw_Nw,raw_N3,
     &                   EPOCH,flag,Nw,N3)
c
c=======================================================================
c     ****f* ambfix/extramb
c
c   FUNCTION   
c   
c     Extract obsevation data for cetern PRN 
c
c
c   INPUTS
c
c     isat       (i)          number of satellites in HPRNX
c     NREC_iSAT  (i)          record number for isat 
c     iPRN       (i)          iPRN for satellite, 
c                             if sat.system = GPS,     iPRN = PRN
c                             if sat.system = Galieo,  iPRN = PRN+100
c                             if sat.system = Glonass, iPRN = PRN+200
c                             if sat.system = SBAS,    iPRN = PRN+300
c     rnxvsn     (R)          rinex version 
c     nobs_typ   (I)          observation types number for existed 
c     cobs_typ   (C)          observation types for existed 
c     raw_lli    (I)          lli
c     raw_snr    (I)          signal to noise 
c     raw_epo    (R)          time in seconds past J2000.0
c     raw_obs    (R)          observations
c
c   OUTPUT
c
c     L1 L2 LA C1 P1 P2 S1 S2 SA LLI EPOCH
c
c   COPYRIGHT
c
c     Copyright(c) 2006-      Shoujian Zhang,
c                             School of Geodesy and Geomatics,
c                             Wuhan University.
c     ***
c
C     $Id: extramb.f,v 1.0 2009/07/10 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output
c
c     input
      integer       isat
      integer       NREC_iSAT(MAX_SAT_NUM)
c
      integer       iPRN
      real*8        rnxvsn
c
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      real*8        raw_epo (MAX_OBS_REC)
      real*8        raw_Nw  (MAX_OBS_REC)
      real*8        raw_N3  (MAX_OBS_REC)
      integer       raw_flag(MAX_OBS_REC)
c
c     output
c
      real*8        Nw   (MAX_OBS_REC)
      real*8        N3   (MAX_OBS_REC)
c
      integer       flag (MAX_OBS_REC)
      real*8        EPOCH(MAX_OBS_REC)
c
      integer       i, j, k
c     first record for isat
      integer       arec
c     last  record for isat
      integer       zrec
c
      do i=1, MAX_OBS_REC
c
        Nw   (i) = 0.0d0
        N3   (i) = 0.0d0
        flag (i) = 0
        EPOCH(i) = 0.0d0
c
      enddo
c
c     extract observations from hprnx level2 file
c     ===========================================
c
      
c     read observables for PRN according to 
c     a) isat number 
c     b) isat record number stroed in NREC_iSAT(MAX_SAT_NUM)
c
c     accumulating record numbers from 1 to isat-1
c   
c     arec: first record for isat
c
      arec = 0
      zrec = 0
c
      do i=1, isat-1
c
      arec = arec + NREC_iSAT(i)
      zrec = zrec + NREC_iSAT(i)
c
      enddo
c
      arec = arec + 1
      zrec = zrec + NREC_iSAT(isat)
c
c     read observables according the record for PRN(isat)
c   
      k = 0
c
      do i=arec, zrec
c
         k        = k + 1
c
         Nw(k)    = raw_Nw(i)
         N3(k)    = raw_N3(i)
c
         flag (k) = raw_flag(i)
         EPOCH(k) = raw_epo(i)
c
      enddo
c
      if(k.NE.NREC_iSAT(isat))then
c
         write(*,*) 'ambfix/extramb'
         write(*,*) '  record number not equal'
         write(*,*)  k, '.vs.', NREC_iSAT(isat)
         stop
c
      endif
c
      return
c
      end
