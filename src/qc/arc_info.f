c
c   subroutine arc_info
c
      subroutine arc_info(nrec,iPRN,EPOCH,x,lotlr,lflag,
     +                    narc,xpts,xmean,xerr)
c
c=======================================================================
c     ****f* qualicontr/arc_info
c
c   FUNCTION   
c
c     delete short arc, if the arc point number is less than
c     min_xpts.
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
c     lotlr       (L)           bad observation flag
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: arc_info.f,v 1.0 2009/07/11 $
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
      real*8        EPOCH(MAX_OBS_REC), x(MAX_OBS_REC)
c
      logical       lotlr(MAX_OBS_REC)
      logical       lflag(MAX_OBS_REC)
c
      integer       narc
      integer       xpts(MAX_ARC)
c
      real*8        xmean(MAX_ARC)
      real*8        xrms(MAX_ARC)
      real*8        xbias(MAX_ARC)
      real*8        xerr(MAX_ARC)
c
c     local variables
c
      real*8        v (MAX_OBS_REC)
      real*8        vv(MAX_OBS_REC)
      real*8        xarc(MAX_ARC, MAX_ARC_PNT)
      logical       short_arc(MAX_ARC)
c
      integer       irec, iarc
      integer       npts
      integer       year, mon, day, hour, min, sec
      real*8        frac
c
c     common
c
      integer       outlier(MAX_PRN)
      integer       amb(MAX_PRN)
c
      common /statistic/ outlier, amb
c
c     initial
c
      do iarc=1, MAX_ARC
         short_arc(iarc) = .false.
      enddo
c
      do iarc=1, MAX_ARC
         xpts(iarc) = 0
      enddo
c
c     calculate arc number and arc point number for every arc
c
      narc = 0
      do irec=1, nrec
         if(.not.lotlr(irec))then
            if(lflag(irec))then
               narc = narc + 1
               if(narc.gt.MAX_ARC)then
                 write(*,*) "qualicontr/arc_info"
                 write(*,*) "    widelane arc number is:", narc
                 write(*,*) "    please modify the MAX_ARC in",
     +                      "    rinex.params!"
                 stop
               endif
               xpts(narc)   = 1
            else
               xpts(narc)   = xpts(narc) + 1
               if(xpts(narc).gt.MAX_ARC_PNT)then
                 write(*,*) "qualicontr/arc_info"
                 write(*,*) "    widelane arc point number is:",
     +                           xpts(narc)
                 write(*,*) "    please modify the MAX_ARC_PNT in",
     +                      "    rinex.params!"
                 stop
               endif
            endif
         endif
      enddo
c
c     search the minimum arc 
c
      do iarc=1, narc
         if(xpts(iarc).lt.MIN_ARC_PNT)then
            short_arc(iarc) = .true.
            outlier(iPRN)=outlier(iPRN) + xpts(iarc)
         endif
      enddo
c
c     write(*,*) "total  arc number is:", narc
c
      iarc = 0
      do irec=1, nrec
         if(.not.lotlr(irec))then
c
c           accumulate arc number
            if(lflag(irec))then
               iarc = iarc + 1
            endif  
c
c           mark the record in short arc as outlier
            lotlr(irec) = short_arc(iarc)
c
c           output the outliers owing to xpts less than min_xpts
            if(lotlr(irec))then
c
               lflag(irec) = .false.
c
c              time conversion
               call sec2cal(EPOCH(irec),year,mon,day,hour,min,sec,frac)
c
c              output information
               write(65, fmt=1000)
     +              "short_arc      ", EPOCH(irec), iPRN, 'iarc',iarc
c
1000          format((2x,A15),(x,F14.3),(x,I3),(X,A,X,I3))
c
            endif
         endif
      enddo
c
2000  format(A1,x,A15,x,I14,x,I3)
c
      write(65, fmt=2000)
     +     '*',"summation      ", outlier(iPRN), iPRN
c
c     new arc number after deleting short arc
c
      do iarc=1, MAX_ARC
         xmean(iarc) = 0.0d0
         xerr(iarc) = 0.0d0
      enddo
c
      do iarc=1, MAX_ARC
         do irec=1, MAX_ARC_PNT
            xarc(iarc, irec) = 0.0d0
         enddo
      enddo
c
      do iarc=1, MAX_ARC
         xpts(iarc) = 0
      enddo
c
c     calculate arc average, sigma
c
      iarc = 0
      do irec=1, nrec
         if(.not.lotlr(irec))then
            if(lflag(irec))then
               iarc              = iarc + 1
               xpts(iarc)        = 1
               xbias(iarc)       = dint(x(irec))
               xmean(iarc)       = x(irec) - xbias(iarc)
               xrms(iarc)        =(x(irec) - xbias(iarc)) **2 
            else
               xpts(iarc)        = xpts(iarc) + 1
               xmean(iarc)       = xmean(iarc) + x(irec)-xbias(iarc)
               xrms(iarc)        = xrms(iarc) + (x(irec)-xbias(iarc))**2
            endif  
         endif
      enddo
c
      narc = iarc
c
      do iarc=1,narc
         xmean(iarc) = xmean(iarc)/dfloat(xpts(iarc))
         xerr(iarc)  = dsqrt((xrms(iarc)- xpts(iarc)*xmean(iarc)**2)/
     &                       (dfloat(xpts(iarc))-1))
         xmean(iarc) = xmean(iarc) + xbias(iarc)
      enddo
c
      return
c
      end
