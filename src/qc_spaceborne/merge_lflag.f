c
c   subroutine merge_lflag
c
      subroutine merge_lflag(nrec,iPRN,EPOCH,lotlr,wflag,mflag,lflag)
c
c=======================================================================
c     ****f* qualicontr/merge_lflag
c
c   FUNCTION   
c
c     write widelane flag, lflag, and lotlr, into iflag
c
c   INPUTS
c
c     nrec        (I)           record number
c     iPRN        (I)           PRN number
c     EPOCH       (R)           observation epoch in seconds past J2000.0
c     lotlr       (R)           lotlr 
c     lflag       (R)           cycleslip flags for Nw
c
c   OUTPUT
c
c     iflag       (R)           cycle slip and outlier flags
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: merge_lflag.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/qualicontr.h'
      include      '../../include/qualicontr.conf.h'
c
c     input/output
c
c     input
      integer       nrec
      integer       iPRN
c
      real*8        EPOCH (MAX_OBS_REC)
c
      logical       lotlr(MAX_OBS_REC)
      logical       wflag(MAX_OBS_REC)
      logical       mflag(MAX_OBS_REC)
c
c     output
      logical       lflag(MAX_OBS_REC)
c
c     local
c
      integer       i, j, k
      integer       irec
c
      integer       outlier(MAX_PRN)
      integer       amb(MAX_PRN)
c
      common /statistic/ outlier, amb
c
      do irec=1,MAX_OBS_REC
         lflag(irec) = .false.
      enddo
c
      do irec=1, nrec
         if(wflag(irec).or.mflag(irec))then
            lflag(irec) = .true.
            if(lotlr(irec))then
               lotlr(irec) = .false.
               outlier(iPRN) = outlier(iPRN) - 1
            endif
         endif
      enddo
c
      return
c
      end
