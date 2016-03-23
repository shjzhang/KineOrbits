c
c  subroutine rhpxres
c
      subroutine rhpxres(lvl2_nsat,lvl2_rres,nres)             
c
c=======================================================================
c     ****f* qualicontr/rhpxres
c
c   FUNCTION   
c   
c     read lvl2 format residual data into memory to process later.
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     res           (r)         residual
c     nres          (i)         number of residual
c     
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: rhpxres.f,v 1.0 2009/08/08 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
      integer       nres
      integer       lvl2_PNTR(MAX_OBS_REC)
      real*8        lvl2_Time(MAX_OBS_REC)
      integer       lvl2_iPRN(MAX_OBS_REC)
      integer       lvl2_nsat(MAX_OBS_REC)
c
      real*8        lvl2_rres(MAX_OBS_REC)
c
c     local
c
      character*200 line
      integer       k, irec, nrec
c
c     rewind
c
      rewind(221)
c
c     read lvl2 observation data
c     ==========================
c
      irec = 0
c
 100  continue
c
      irec = irec + 1
c
      read(221, fmt=1000, end=444) 
     &     lvl2_PNTR(irec), lvl2_nsat(irec), lvl2_Time(irec),  
     &     lvl2_iPRN(irec), lvl2_rres(irec)
c
 1000 format(2I6, F18.7, (X,I3), F8.3)
c
      goto 100
c
 444  continue
c
      nres = irec - 1
c
      return
c
      end
