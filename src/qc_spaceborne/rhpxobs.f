c
c  subroutine rhpxobs
c
      subroutine rhpxobs(rnxvsn, 
     &                   nsat_sys, csat_sys, nobs_typ, cobs_typ, nobs, 
     &                   lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,
     &                   lvl2_LLI, lvl2_SNR)             
c
c=======================================================================
c     ****f* qualicontr/rhpxobs
c
c   FUNCTION   
c   
c     read lvl2 format observation data into memory to process later.
c
c   INPUTS
c
c     rnxvsn     real*8         rinex version
c     csat_sys   characeter     returned satellite systems in rinex 
c                               file
c     nobs_typ   integer        observation types number for existed 
c                               satellite systems.
c     cobs_typ   character      observation types for existed 
c                               satellite systems.
c
c   OUTPUT
c
c     lvl2_PNTR  integer        record pointer in lvl2. 
c     lvl2_Time  real*8         time in seconds past J2000.0
c     lvl2_iPRN  integer        iPRN for satellite, 
c                               if sat.system = GPS,     iPRN = PRN
c                               if sat.system = Galieo,  iPRN = PRN+100
c                               if sat.system = Glonass, iPRN = PRN+200
c                               if sat.system = SBAS,    iPRN = PRN+300
c   
c     lvl2_OBS                  observations
c     lvl2_LLI                  lli
c     lvl2_SNR                  signal to noise 
c
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: rhpxobs.f,v 1.0 2009/07/10 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     output
c
      integer       nobs
      integer       lvl2_PNTR(MAX_OBS_REC)
      real*8        lvl2_Time(MAX_OBS_REC)
      integer       lvl2_iPRN(MAX_OBS_REC)
c
      real*8        lvl2_OBS(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_LLI(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_SNR(MAX_OBS_REC, MAX_OBS_TYP)
c
c     local
c
      character*200 line
      integer       k, irec, nrec
c
c     rewind
c
      rewind(211)
c
c     read lvl2 observation data
c     ==========================
c
      irec = 0
c
      if(rnxvsn.lt.3.00)then
c
 100     continue
c
         irec = irec + 1
c
         read(211, fmt=1000, end=444) 
     &        lvl2_PNTR(irec),  lvl2_Time(irec),  lvl2_iPRN(irec),
     &       (lvl2_OBS( irec,k),lvl2_LLI( irec,k),lvl2_SNR( irec,k),
     &        k=1, nobs_typ(1))
c
 1000    format(I6, F18.7, (X,I3), 10(F14.3,I1,I1))
c
         goto 100
c
      endif
c
 444  continue
c
      nobs = irec - 1
c
      return
c
      end
