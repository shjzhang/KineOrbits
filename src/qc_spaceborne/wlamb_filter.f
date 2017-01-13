c
c   subroutine wlamb_filter
c
      subroutine wlamb_filter(nrec, iPRN, EPOCH, Nw, lotlr, wflag)
c
c=======================================================================
c     ****f* qualicontr/wlamb_filter
c
c   FUNCTION   
c
c     Edit widelane ambiguity with recursive formulas. Widelane Cycle slip
c     and outliers will be detected and deleted EPOCH by EPOCH.
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     Nw          (R)           widelane ambiguity 
c     lotlr       (L)           bad observation flag
c
c   OUTPUT
c
c     wflag       (L)           widelane flag
c
c   NOTES
c
c     The Nw filter algorithm is as follows:
c
c     1) mean of Nw are calculated by the following recursive
c        formula:       
c        <Nw>(i) = <Nw>(i-1) + (Nw(i)-<Nw>(i-1))/i;                          
c
c     2) RMS of <Nw>(i) are calculted from the following recursive
c        formula:  
c        sigma(i)^2 = (i-2)*sigma(i-1)^2/(i-1) + (Nw(i)-<Nw>(i-1))^2/i;
c
c     3) cycle slip and outliers are recognized by the formula:              
c        |Nw(i)-<Nw>(i-1)|>=3*(0.2484^2+sigma(i-1)^2)^(1/2); 
c
c   Reference
c
c     Blewitt G., 1990, An automatic editing algorithm for GPS data,
c                       Geophysical Research Letters.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: wlamb_filter.f,v 1.0 2009/07/11 $
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
      real*8        EPOCH(MAX_OBS_REC), Nw(MAX_OBS_REC)
c
c     output
c
      logical       lotlr(MAX_OBS_REC)
      logical       wflag(MAX_OBS_REC)
c
c     local variables
c
      real*8        mean_Nw, sigma_Nw, sigma2_Nw
      real*8        dNw, Nw_fore
      real*8        bias, limit, arcgap
c  
      integer       irec, ifirst, ifore, ilast, arc_rec
c  
      logical       first
      logical       wflag_new
c
      integer       year, mon, day, hour, min, sec
      real*8        frac
c
c     common
c
      integer       outlier(MAX_PRN)
c
      common /statistic/ outlier
c
c     filter initial
c     ==============
c
      ilast      = 1
      ifore      = 1
      ifirst     = 1
      first      = .true.
c
      dNw        = 0
      Nw_fore    = 0.0d0
      mean_Nw    = 0
      sigma2_Nw  = 0
      wflag_new = .false.
c
c     widelane filter
c     ===============
c
c
      do irec = 1, nrec
c
         wflag(irec) = .false.
c
c        if not outlier, then filter
         if(.not.lotlr(irec)) then
c
            bias    = Nw(irec)-mean_Nw
            limit   = 3*dsqrt(0.2484**2+sigma2_Nw)
            arcgap  = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
c
c           cycleslip or outliers ??
            if(     first   
     &         .or. wflag_new
     &         .or.(dabs(bias).gt.limit)
     +         .or.(arcgap    .gt.MAX_ARC_GAP))then 
c
               dNw = dabs(Nw(irec)-Nw_fore)
c
               if(.not.wflag_new)then
                  ifirst     = irec
                  wflag_new = .true.
                  Nw_fore    = Nw(irec)
                  call sec2cal(EPOCH(irec),
     &                         year,mon,day,hour,min,sec,frac)
               elseif(dNw.gt.1.05d0) then
                  lotlr(ifirst)   = .true.
                  outlier(iPRN) = outlier(iPRN) + 1
c
                  write(65, fmt=1000) 
     &                 'widelane       ', EPOCH(irec), iPRN
c
1000              format((2x,A15),(x,F14.3),(x,I3))
c
c
                  sigma_Nw     = dsqrt(sigma2_Nw)
                  arcgap       = dabs(EPOCH(irec)-EPOCH(ilast))/10.0d0
c
                  if(dabs(bias)  .lt.limit.and.
     +               dabs(arcgap).lt.MAX_ARC_GAP)then
c
                     wflag_new= .false. 
                     arc_rec   = arc_rec+1
                     ilast     = irec
                     Nw_fore   = Nw(irec)
                     mean_Nw   = mean_Nw+bias/dfloat(arc_rec)
                     sigma2_Nw = (arc_rec-2)*sigma2_Nw/dfloat(arc_rec-1)
     +                         + (bias**2)/dfloat(arc_rec)
c
                  else
c
                     ifirst     = irec
                     Nw_fore    = Nw(irec)
                     call sec2cal(EPOCH(irec),
     &                            year,mon,day,hour,min,sec,frac)
                  endif
c
               else if(arcgap.gt.MAX_ARC_GAP)then
c
                  lotlr(ifirst)   = .true. 
                  outlier(iPRN) = outlier(iPRN) + 1
                  ifirst           = irec
                  wflag_new        = .true.
                  Nw_fore          = Nw(irec)
c
               else
c
c                 if 2 outliers "agree" to within 1 cycle
c                 take this as the new reference arc
c
                  wflag(ifirst) = .true.
c
c                 first arc
c
                  if(first) then            
                     write(66,fmt=2000) 
     +                    " first    Nw  arc at rec    :",ifirst,
     +                    " time :", year, mon, day, hour, min, sec,
     +                    " PRN :", iPRN
c
2000                 format(a,I4,a,I4,4I3,I5,a,I3)
c
                     first = .false.
c
                  else
c
                     sigma_Nw = dsqrt(sigma2_Nw)
                     write(66,fmt=3000)
     +                    " new      Nw  arc at rec    :",ifirst, 
     +                    " time :", year, mon, day, hour, min, sec,
     +                    " PRN :", iPRN 
c
3000                 format(2(a,I4),4I3,I5,a,I3)
c
                  end if
                  mean_Nw    = (Nw_fore+Nw(irec))*0.5d+00
                  sigma2_Nw  = 2*0.2482**2
                  arc_rec    = 2
                  ilast      = irec
                  wflag_new  = .false.
               end if
            else
               wflag_new     = .false.
               arc_rec       = arc_rec+1
               ilast         = irec
               mean_Nw       = mean_Nw+bias/dfloat(arc_rec)
               sigma2_Nw     = (arc_rec-2)*sigma2_Nw/dfloat(arc_rec-1)
     +                        +(bias**2)/dfloat(arc_rec)
            end if
c
            ifore = irec
c
         end if
c
      end do
c
      if((ifore.ne.1).and.(ifirst.eq.ifore))then
c
        lotlr(ifore) = .true.
        outlier(iPRN) = outlier(iPRN) + 1
        write(66,fmt=4000) 
     +       " arc number is 1, at rec    :",
     +         ifirst, 
     +       " time :", year, mon, day, hour, min, sec,
     +       " PRN :", iPRN
c
4000    format(A,I4,A,I4,4I3,I5,A,I3)
c
      endif
c
      if(arc_rec.gt.0)then
c
         sigma_Nw = dsqrt(sigma2_Nw)
c
         call sec2cal(EPOCH(ilast),year,mon,day,hour,min,sec,frac)
c
         write(66,fmt=5000)
     +        " end      Nw  arc at rec    :",ilast, 
     +        " time :", year, mon, day, hour, min, sec,
     +        " PRN :", iPRN 
c
5000     format(a,I4,a,I4,4I3,I5,a,I3)
c
      endif
c
      return
c
      end
