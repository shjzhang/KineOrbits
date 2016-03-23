c
c   subroutine arcsta
c
      subroutine arcsta(nrec,iPRN,EPOCH,Ts,lotlr,lflag,
     +                  narc,points,TsMean,TsMeanSig,Ts1,TsSig1)
c
c=======================================================================
c     ****f* qualicontr/arcsta
c
c   FUNCTION   
c
c     delete short arc, if the arc point number is less than
c     min_points.
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     lflag       (L)           diff1N2 flag
c     narc        (I)           total arc number
c     points      (I)           arc point number for every diff1N2 arc  
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
C     $Id: arcsta.f,v 1.0 2009/07/11 $
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
      real*8        EPOCH(MAX_OBS_REC), Ts(MAX_OBS_REC)
c
      logical       lotlr(MAX_OBS_REC)
      logical       lflag(MAX_OBS_REC)
c
      integer       narc
      integer       points(MAX_ARC)
c
      real*8        TsMean(MAX_ARC)
      real*8        TsMeanSig(MAX_ARC)
c
      real*8        Ts1(MAX_ARC)
      real*8        TsSig1(MAX_ARC)
c
c     local variables
c
      real*8        v (MAX_OBS_REC)
      real*8        vv(MAX_OBS_REC)
      real*8        TsArc(MAX_ARC, MAX_ARC_PNT)
      logical       short_arc(MAX_ARC)
c
      integer       irec, iarc
      integer       npoint
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
         points(iarc) = 0
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
                 write(*,*) "qualicontr/arcsta"
                 write(*,*) "    widelane arc number is:", narc
                 write(*,*) "    please modify the MAX_ARC in",
     +                      "    rinex.params!"
                 stop
               endif
               points(narc)   = 1
            else
               points(narc)   = points(narc) + 1
               if(points(narc).gt.MAX_ARC_PNT)then
                 write(*,*) "qualicontr/arcsta"
                 write(*,*) "    widelane arc point number is:",
     +                           points(narc)
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
         if(points(iarc).lt.MIN_ARC_PNT)then
            short_arc(iarc) = .true.
            outlier(iPRN)=outlier(iPRN) + points(iarc)
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
c           output the outliers owing to points less than min_points
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
         TsMean(iarc) = 0.0d0
         TsMeanSig(iarc) = 0.0d0
      enddo
c
      do iarc=1, MAX_ARC
         do irec=1, MAX_ARC_PNT
            TsArc(iarc, irec) = 0.0d0
         enddo
      enddo
c
      do irec=1,MAX_OBS_REC
         Ts1(irec) = 0.0d0
         TsSig1(irec) = 0.0d0
      enddo
c
      do iarc=1, MAX_ARC
         points(iarc) = 0
      enddo
c
c     calculate arc average, sigma
c
      narc = 0
      do irec=1, nrec
         if(.not.lotlr(irec))then
c
c           accumulate arc number
            if(lflag(irec))then
               narc                = narc + 1
               points(narc)        = 1
               TsArc (narc,1)      = Ts(irec)
               TsMean(narc)        = Ts(irec)
            else
               points(narc)        = points(narc) + 1
               npoint              = points(narc)
               TsArc (narc,npoint) = Ts(irec)
               TsMean(narc)        = TsMean(narc) + Ts(irec)
            endif  
         endif
      enddo
c
c     calculate the average and standard error(rms) for every arc
c
      do iarc=1, narc
         vv(iarc) = 0.0d0
         TsMean(iarc) = TsMean(iarc)/dfloat(points(iarc))
c
         do irec=1, points(iarc)
            v(irec)   = TsMean(iarc) - TsArc(iarc, irec)
            vv(iarc)  = vv(iarc) + v(irec)**2
         enddo
c
         if(points(iarc).gt.2)then
            TsMeanSig(iarc) = dsqrt(vv(iarc)/dfloat(points(iarc)-1))
         else
            TsMeanSig(iarc) = dsqrt(vv(iarc))
         endif
      enddo
c
c     substitue the input Ts with the mean Ts.
c
      iarc = 0
      do irec=1, nrec
c
         if(.not.lotlr(irec))then
c
c           accumulate arc number
            if(lflag(irec))then
               iarc = iarc + 1
            endif  
c
c           substitute
            Ts1(irec) = TsMean(iarc)
            TsSig1(irec) = TsMeanSig(iarc)
c
         endif
c
      enddo
c
      return
c
      end
