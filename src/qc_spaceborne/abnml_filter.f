c
c   subroutine abnml_filter
c
      subroutine abnml_filter(irec_fst,irec_lst,iPRN,EPOCH,abnml,
     &                                               lotlr,cflag)
c
c=======================================================================
c     ****f* qualicontr/abnml_filter
c
c   FUNCTION   
c
c     Edit IF abnml Times series (which are from difference eqaution) 
c     with recursive formulas. Cycle slip and outliers will be detected 
c     EPOCH by EPOCH.
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     abnml       (R)           IF residual time series in [m]
c     lotlr       (L)           bad observation flag
c
c   OUTPUT
c
c     cflag       (L)           widelane flag
c
c   NOTES
c
c     The abnml filter algorithm is as follows:
c
c     1) mean of abnml are calculated by the following recursive
c        formula:       
c        <abnml>(i) = <abnml>(i-1) + (abnml(i)-<abnml>(i-1))/i;                          
c
c     2) RMS of <abnml>(i) are calculted from the following recursive
c        formula:  
c        sigma(i)^2 = (i-2)*sigma(i-1)^2/(i-1) + (abnml(i)-<abnml>(i-1))^2/i;
c
c     3) cycle slip and outliers are recognized by the formula:              
c        |abnml(i)-<abnml>(i-1)|>=3*(0.2484^2+sigma(i-1)^2)^(1/2); 
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
C     $Id: abnml_filter.f,v 1.0 2009/07/21 $
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
      integer       irec_fst, irec_lst
      integer       iPRN
c  
      real*8        EPOCH(MAX_OBS_REC), abnml(MAX_OBS_REC)
c
c     output
c
      logical       lotlr(MAX_OBS_REC)
      logical       cflag(MAX_OBS_REC)
c
c     local variables
c
      real*8        mean_abnml, sigma_abnml, sigma2_abnml
      real*8        dabnml, abnml_fore
      real*8        bias, limit, arcgap
c  
      integer       irec, ifirst, ifore, ilast, arc_rec
c  
      logical       first
      logical       cflag_new
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
      ilast   = irec_fst
      ifore   = 0
      ifirst  = irec_fst
      first   = .true.
      arc_rec = 0
c
      dabnml       = 0
      abnml_fore   = 0.0d0
      mean_abnml   = 0
      sigma2_abnml = 0
      cflag_new    = .false.
c
c     widelane filter
c     ===============
c
      do irec = irec_fst, irec_lst
c
         cflag(irec) = .false.
c
c        if not outlier, then filter
         if(.not.lotlr(irec)) then
c
            bias    = abnml(irec)-mean_abnml
c           limit   = 3*dsqrt(0.01d0**2+sigma2_abnml)
            limit   = 0.06
            arcgap  = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
c
c           write(*,*) irec,bias,limit,arc_rec,mean_abnml,sigma2_abnml
c
c           cycleslip or outliers ??
            if(     cflag_new
     &         .or.(dabs(bias).gt.limit))then 
c
               dabnml = dabs(abnml(irec)-abnml_fore)
c
               if(.not.cflag_new)then
c
                  ifirst     = irec
                  cflag_new  = .true.
                  abnml_fore = abnml(irec)
                  call sec2cal(EPOCH(irec),
     &                         year,mon,day,hour,min,sec,frac)
c
               elseif(dabnml.gt.0.03d0) then
c
                  lotlr(ifirst) = .true.
                  outlier(iPRN) = outlier(iPRN) + 1
c
                  write(65, fmt=1000) 
     &                 'IF outlier     ', EPOCH(irec), iPRN
c
1000              format((2x,A15),(x,F14.3),(x,I3))
c
c
                  sigma_abnml    = dsqrt(sigma2_abnml)
                  arcgap         = dabs(EPOCH(irec)-EPOCH(ilast))/10.0d0
c
                  if(dabs(bias)  .lt.limit.and.
     +               dabs(arcgap).lt.MAX_ARC_GAP)then
c
                     cflag_new    = .false. 
                     arc_rec      = arc_rec+1
                     ilast        = irec
                     abnml_fore   = abnml(irec)
                     mean_abnml   = mean_abnml+bias/dfloat(arc_rec)
                     sigma2_abnml =
     &              (arc_rec-2)*sigma2_abnml/dfloat(arc_rec-1)
     +            + (bias**2)/dfloat(arc_rec)
c
                  else
c
                     ifirst       = irec
                     abnml_fore   = abnml(irec)
                     call sec2cal(EPOCH(irec),
     &                            year,mon,day,hour,min,sec,frac)
                  endif
c
               else
c
c                 if 2 outliers "agree" to within 1 cycle
c                 take this as the new reference arc
c
                  cflag(ifirst)= .true.
c
c                 first arc
c
                  sigma_abnml  = dsqrt(sigma2_abnml)
                  write(66, fmt=3000)
     +                 " new   abnml  arc at rec    :",ifirst, 
     +                 " time :", year, mon, day, hour, min, sec,
     +                 " PRN :", iPRN 
c
3000              format(2(a,I4),4I3,I5,a,I3)
c
                  mean_abnml   = (abnml_fore+abnml(irec))*0.5d+00
                  sigma2_abnml = 2*0.006**2
                  arc_rec      = 2
                  ilast        = irec
                  cflag_new    = .false.
c
               end if
c
            else
c
               cflag_new    = .false.
               arc_rec      = arc_rec+1
               ilast        = irec
               mean_abnml   = mean_abnml+bias/dfloat(arc_rec)
c
               if(arc_rec.gt.1)then
c
               sigma2_abnml = (arc_rec-2)*sigma2_abnml/dfloat(arc_rec-1)
     +                      + (bias**2)/dfloat(arc_rec)
c
               endif
c
            end if
c
            ifore = irec
c
         end if
c
      end do
c
      if((ifore.ne.0).and.(ifirst.eq.ifore))then
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
         sigma_abnml = dsqrt(sigma2_abnml)
c
         call sec2cal(EPOCH(ilast),year,mon,day,hour,min,sec,frac)
c
         write(66,fmt=5000)
     +        " end   abnml  arc at rec    :",ilast, 
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
