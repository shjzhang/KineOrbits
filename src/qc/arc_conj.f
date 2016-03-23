c
c   subroutine arc_conj
c
      subroutine arc_conj(nrec,iPRN,EPOCH,narc,xpts,xmean,xerr,
     &                    lotlr,lflag,xbias)
c
c=======================================================================
c     ****f* qualicontr/arc_conj
c
c   FUNCTION   
c
c     find arcs to be connected, connect arcs
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     lflag       (L)           diff1N2 flag
c     narc        (I)           total arc number
c     xpts        (I)           arc point number for every diff1N2 arc  
c
c   OUTPUT
c
c     lotlr       (L)           bad observation flag: logical outlier
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: arc_conj.f,v 1.0 2009/07/11 $
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
c     input/output variables
c
c     input
c
      integer       nrec
      integer       iPRN
c  
      real*8        EPOCH(MAX_OBS_REC)
c
      integer       narc
      integer       xpts(MAX_ARC)
c
      real*8        xmean(MAX_ARC)
      real*8        xerr(MAX_ARC)
c
      logical       lotlr(MAX_OBS_REC)
      logical       lflag(MAX_OBS_REC)
c
      real*8        xbias(MAX_OBS_REC)
c
c     local
c
      real*8        xmin, xint(MAX_ARC)
      real*8        xfra, xsig, xdif
      logical       connected(MAX_ARC), badarc(MAX_ARC)
c
      real*8        arclen
      integer       iarc,jarc,irec
      integer       npts
      integer       ifst, ilst
c
c     local variables
c
      if(narc.eq.0) then
         return
      endif
c
c     fix (n1-n2) offsets if sufficiently confident
c
      xmin = 1.0d+30
c
      do iarc=1,narc
         if(xerr(iarc).lt.xmin)then
            jarc = iarc
            xmin = xerr(iarc)
         endif
      enddo
c
      connected(jarc) = .true.
c
      xint(jarc) = 0.0d0
      do iarc = 1, narc
         if(iarc.ne.jarc)then
            xfra       = xmean(iarc) - xmean(jarc)
            xint(iarc) = dnint( xfra )
            xfra       = xfra - xint(iarc) 
            xsig       = dsqrt( xerr(iarc)**2 + xerr(jarc)**2 )
            if(  xpts(iarc).ge.4.and.xsig.lt.0.15d0  .and.
     &         ((abs(xfra).lt.0.35d0 .and.xint(iarc).eq.0.0d0) .or. 
     &           abs(xfra).lt.0.30d0 ) )then
                 connected(iarc) = .true.
            else
                 connected(iarc) = .false.
            endif
         endif
      enddo
c
c     correct offsets of all arcs
c     delete unconnectable arcs < arcminlen minutes long 
c                          or.  < arcminpts points
c
      iarc = 0
      npts = 0      
c
      do irec=1,nrec
         if(.not.lotlr(irec))then 
            if(lflag(irec))then
               if(iarc.gt.0)then
                  badarc(iarc) = .false.
                  arclen = ( EPOCH(ilst) - EPOCH(ifst) ) /60.0d0
                  if(.not.connected(iarc))then
                     badarc(iarc) = ( arclen .lt. 5 .or. npts .lt. 5*6 )
                  endif
               endif
               iarc = iarc + 1
               ifst = irec
               npts = 0
            endif
            ilst = irec
            if(irec.le.nrec)then
               xbias(irec) = -dnint(xmean(jarc)) - xint(iarc)
               npts = npts + 1
            endif
         endif
      enddo
c
      return
c
      end
