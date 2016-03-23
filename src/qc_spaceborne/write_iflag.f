c
c   subroutine write_iflag
c
      subroutine write_iflag(nrec,iPRN,EPOCH,lotlr,lflag,iflag)
c
c=======================================================================
c     ****f* qualicontr/write_iflag
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
C     $Id: write_iflag.f,v 1.0 2009/07/11 $
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
      logical       lflag(MAX_OBS_REC)
c     output
      integer       iflag (MAX_OBS_REC)
c
c     local
c
      integer       i, j, k
      integer       irec
c
      do irec=1, nrec
         if(    lotlr(irec))then
            iflag(irec) = 9
         elseif(lflag(irec))then
            iflag(irec) = 1
         endif
      enddo
c
      return
c
      end
