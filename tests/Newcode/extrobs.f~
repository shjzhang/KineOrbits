c
c   subroutine extrobs
c
      subroutine extrobs(isat,nrec_isat, 
     &                   iPRN,rnxvsn,nobs_typ,cobs_typ,
     &                   raw_epo,raw_obs,raw_res,raw_sat,
     &                   EPOCH,L1,L2,LA,C1,P1,P2,S1,S2,SA,res,lsat)
c
c=======================================================================
c     ****f* qualicontr/extrobs
c
c   FUNCTION   
c   
c     Extract obsevation data for cetern PRN 
c
c
c   INPUTS
c
c     isat       (i)          number of satellites in HPRNX
c     nrec_isat  (i)          record number for isat 
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
C     $Id: extrobs.f,v 1.0 2009/07/10 $
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
      integer       nrec_isat(MAX_SAT_NUM)
c
      integer       iPRN
      real*8        rnxvsn
c
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      real*8        raw_epo(MAX_OBS_REC)
      real*8        raw_obs(MAX_OBS_REC, MAX_OBS_TYP)
      real*8        raw_res(MAX_OBS_REC)
      integer       raw_snr(MAX_OBS_REC, MAX_OBS_TYP)
      integer       raw_lli(MAX_OBS_REC, MAX_OBS_TYP)
      integer       raw_sat(MAX_OBS_REC)
c
c     output
c
      real*8        L1   (MAX_OBS_REC)
      real*8        L2   (MAX_OBS_REC)
      real*8        LA   (MAX_OBS_REC)
      real*8        C1   (MAX_OBS_REC)
      real*8        P1   (MAX_OBS_REC)
      real*8        P2   (MAX_OBS_REC)
c
      real*8        S1   (MAX_OBS_REC)
      real*8        S2   (MAX_OBS_REC)
      real*8        SA   (MAX_OBS_REC)
      real*8        res  (MAX_OBS_REC)
      integer       lsat (MAX_OBS_REC)
c
      integer       LLI  (MAX_OBS_REC)
      real*8        EPOCH(MAX_OBS_REC)
c
c     local variables
c
      integer       idx_L1, idx_L2, idx_LA 
      integer       idx_C1, idx_P1, idx_P2
      integer       idx_S1, idx_S2, idx_SA
c
      integer       i, j, k
c     first record for isat
      integer       arec
c     last  record for isat
      integer       zrec
c
      do i=1, MAX_OBS_REC
c
        L1   (i) = 0.0d0
        L2   (i) = 0.0d0
        LA   (i) = 0.0d0
        C1   (i) = 0.0d0
        P1   (i) = 0.0d0
        P2   (i) = 0.0d0
        S1   (i) = 0.0d0
        S2   (i) = 0.0d0
        SA   (i) = 0.0d0
        res  (i) = 0.0
        EPOCH(i) = 0.0d0
c
      enddo
c
c     extract observations from hprnx level2 file
c     ===========================================
c
      
c     read observables for PRN according to 
c     a) isat number 
c     b) isat record number stroed in nrec_isat(MAX_SAT_NUM)
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
      arec = arec + nrec_isat(i)
      zrec = zrec + nrec_isat(i)
c
      enddo
c
      arec = arec + 1
      zrec = zrec + nrec_isat(isat)
c
c     get idx for observable types from obs_type
c
      idx_L1 = 0
      idx_L2 = 0
      idx_C1 = 0
      idx_P1 = 0
      idx_P2 = 0
      idx_S1 = 0
      idx_S2 = 0
c
      if(rnxvsn.lt.3.00)then
c
         do i=1, nobs_typ(1)
           if(    trim(cobs_typ(1,i)).eq.'L1')then
             idx_L1 = i
           elseif(trim(cobs_typ(1,i)).eq.'L2')then
             idx_L2 = i
           elseif(trim(cobs_typ(1,i)).eq.'LA')then
             idx_LA = i
           elseif(trim(cobs_typ(1,i)).eq.'C1')then
             idx_C1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P1')then
             idx_P1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P2')then
             idx_P2 = i
           elseif(trim(cobs_typ(1,i)).eq.'S1')then
             idx_S1 = i
           elseif(trim(cobs_typ(1,i)).eq.'S2')then
             idx_S2 = i
           elseif(trim(cobs_typ(1,i)).eq.'SA')then
             idx_SA = i
           endif
         enddo
c
c        double-frequency receiver ??
         if(idx_L1.EQ.0.or.idx_L2.EQ.0)then
           write(*,*) 'qualicontr/extrobs'
           write(*,*) '  not double-frequency GPS receiver'
           stop
         endif
c
      else
         write(*,*) "qualicontr/extrobs"
         write(*,*) '  Please modifiy to process rinex 3.00' 
         stop
      endif
c
c     read observables according the record for PRN(isat)
c   
      k = 0
c
      do i=arec, zrec
c
         k        = k + 1
c
         L1(k)    = raw_obs(i, idx_L1)
         L2(k)    = raw_obs(i, idx_L2)
         LA(k)    = raw_obs(i, idx_LA)
c
         C1(k)    = raw_obs(i, idx_C1)
c
         if(    idx_P1.NE.0)then
         P1(k)    = raw_obs(i, idx_P1)
         elseif(idx_C1.NE.0)then
         P1(k)    = raw_obs(i, idx_C1)
         endif
c
         P2(k)    = raw_obs(i, idx_P2)
c
         if(idx_S1.NE.0)then
         S1(k)    = raw_obs(i, idx_S1)
         S2(k)    = raw_obs(i, idx_S2)
         SA(k)    = raw_obs(i, idx_SA)
         else
         S1(k)    = 100
         S2(k)    = 100
         SA(k)    = 100
         endif
c
         res  (k) = raw_res(i)
         EPOCH(k) = raw_epo(i)
c
      enddo
c
      if(k.NE.nrec_isat(isat))then
c
         write(*,*) 'qualicontr/extrobs'
         write(*,*) '  record number not equal'
         write(*,*)  k, '.vs.', nrec_isat(isat)
         stop
c
      endif
c
      return
c
      end
