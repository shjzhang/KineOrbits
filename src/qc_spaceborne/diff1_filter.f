c
c   subroutine diff1_filter
c
      subroutine diff1_filter(nrec,iPRN,EPOCH,N2,lotlr,dflag)
c
c=======================================================================
c     ****f* qualicontr/diff1_filter
c
c   FUNCTION   
c
c     Edit 1st order difference Time series with recursive formulas. 
c     Cycle slip will be detected EPOCH by EPOCH.
c
c   NOTES
c
c     The outliers will not be returned. because the outliers in 1st 
c     order difference is the ionspheric flicker. !!!!
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     Ts          (R)           1st order difference Time series
c     lotlr       (L)           bad observation flag
c
c   OUTPUT
c
c     dflag       (L)           1st order difference flag
c
c   NOTES
c
c     The Ts filter algorithm is as follows:
c
c     1) mean of Ts are calculated by the following recursive
c        formula:       
c        <Ts>(i) = <Ts>(i-1) + (Ts(i)-<Ts>(i-1))/i;                          
c
c     2) RMS of <Ts>(i) are calculted from the following recursive
c        formula:  
c        sigma(i)^2 = (i-2)*sigma(i-1)^2/(i-1) + (Ts(i)-<Ts>(i-1))^2/i;
c
c     3) cycle slip and outliers are recognized by the formula:              
c        |Ts(i)-<Ts>(i-1)|>=3*(0.2484^2+sigma(i-1)^2)^(1/2); 
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
C     $Id: diff1_filter.f,v 1.0 2009/07/11 $
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
      real*8        N2(MAX_OBS_REC)
c
c     output
c
      logical       lotlr(MAX_OBS_REC)
      logical       dflag(MAX_OBS_REC)
c
c     local variables
c
      real*8        Ts(MAX_OBS_REC)
c
      real*8        N2_Arc(MAX_OBS_REC)
      integer       N2_Arc_pntr(MAX_OBS_REC)
      real*8        df2N2(MAX_OBS_REC)
      real*8        df2N2_Arc(MAX_OBS_REC)
c
      integer       ifst_arc_N2(MAX_ARC)
      integer       ilst_arc_N2(MAX_ARC)
      integer       ifst, ilst
      integer       iarc, narc, npnt
c
      real*8        dTs, Ts_fore
      real*8        bias, bias_fore, limit, arcgap
      real*8        mean_Ts, sigma_Ts, sigma2_Ts
c  
      integer       irec, ifirst, ifore, ilast, arc_rec
c  
      logical       first
      logical       dflag_new
c
      logical       motlr(MAX_OBS_REC)
c
      integer       i, j, k
      integer       year, mon, day, hour, min, sec
      real*8        frac
c
      integer       outlier(MAX_PRN)
c
      do iarc=1, MAX_ARC
         ifst_arc_N2(iarc) = 0
         ilst_arc_N2(iarc) = 0
      enddo
c
      do irec=1, MAX_OBS_REC
         N2_Arc(irec) = 0.0d0
         df2N2(irec) = 0.0d0
         df2N2_Arc(irec) = 0.0d0
         N2_Arc_pntr(irec) = 0
      enddo
c
c     N2 from geometry free (L1-L2) filter
c     ====================================
c
c     calculate 2rd difference for N2
c
c++   separate the raw Time series data sets if arcgap > 30
c
      iarc = 0
      ifore = 1
      first = .true.
c
      do irec=1,nrec
c
         arcgap = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
c
         if(first.or.arcgap.gt.30)then
            iarc = iarc + 1
            ifst_arc_N2(iarc)  = irec
            first = .false.
            ilst_arc_N2(iarc)  = ifst_arc_N2(iarc)
         else
            ilst_arc_N2(iarc)  = irec
         endif
c
         ifore = irec
c
      enddo
c
      narc = iarc
c
c     determine arc, the points in arc, according to ifst_arc, ilast_arc
c
      do iarc=1, narc
c
         ifst = ifst_arc_N2(iarc)
         ilst = ilst_arc_N2(iarc)
c
         npnt = 0
         do irec=ifst,ilst
c
            npnt = npnt + 1
c
            N2_Arc(npnt) = N2(irec)
            N2_Arc_pntr(irec) = iarc
c
         enddo
c
c++      calculate 5th order difference for every Ts arc
c
         call diff(N2_Arc,MAX_OBS_REC,2,df2N2_Arc)
c
c++      df2N2
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            df2N2(irec) = df2N2_Arc(npnt)
         enddo
c        
      enddo
c
c     do i=1,nrec
c        write(*,*) EPOCH(i),N2(i),N2_Arc_pntr(i),i,df2N2(i)
c     enddo
c     stop
c
c     recover the 1st order difference from 2nd order difference
c     ==========================================================
c
      call diff1_rcv(df2N2,MAX_OBS_REC,Ts)
c
c     filter initial
c     ==============
c
      do irec=1,nrec
         motlr(irec) = .false.
      enddo
c
      do i=1,MAX_PRN
         outlier(i) = 0
      enddo
c
      ilast      = 1
      ifore      = 1
      ifirst     = 1
      first      = .true.
c
      dTs        = 0
      Ts_fore    = 0.0d0
      mean_Ts    = 0
      sigma2_Ts  = 0
      dflag_new  = .false.
c
c     1st order difference filter
c     ===========================
c
      irec = 1
c
      do while(irec.le.nrec)
c
c        if not outlier, then filter
c
         if(.not.lotlr(irec)) then
c
            arcgap = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
c
            if(first.or.arcgap.gt.30)then
c
c              if the new arc begins, ??
c
               irec      = irec + 2
               arc_rec   = 0
               mean_Ts   = 0
               sigma2_Ts = 0
               first     = .false.
c
            endif
c
            bias      = Ts(irec) - mean_Ts
            bias_fore = Ts_fore  - mean_Ts
c           the input Ts is difference of N2, here, we only detect
c           cycleslip with 2 and above
            limit     = 5
c
c           cycleslip or outliers ??
            if(dflag_new.or.(dabs(bias).gt.limit))then 
c
               dTs = dabs(Ts(irec)-Ts_fore)
c
               if(.not.dflag_new)then
c
                  ifirst     = irec
                  dflag_new  = .true.
                  Ts_fore    = Ts(irec)
                  call sec2cal(EPOCH(irec),
     &                         year,mon,day,hour,min,sec,frac)
c
               elseif((dabs(bias).gt.limit).and.dabs(dTs).gt.limit)then
c              elseif((dabs(bias).gt.limit).and.dabs(dTs).gt.1.0  )then
c
                  motlr(ifirst)   = .true.
                  outlier(iPRN)   = outlier(iPRN) + 1
c
                  write(65, fmt=1000) 
     &             '1st_order_diff ', EPOCH(irec), iPRN
c
1000              format((2x,A15),(x,F14.3),(x,I3),(X,A,X,F8.2))
c
c
                  sigma_Ts     = dsqrt(sigma2_Ts)
c
                  if(dabs(bias+bias_fore).lt.1.0)then
c
                     dflag_new = .false.
                     Ts_fore   = Ts(irec)
c
                  else
c
                     ifirst    = irec
                     Ts_fore   = Ts(irec)
                     call sec2cal(EPOCH(irec),
     &                            year,mon,day,hour,min,sec,frac)
                  endif
c
               else
c
c                 cycleslip happans
                  dflag(ifirst) = .true.
c
                  sigma_Ts = dsqrt(sigma2_Ts)
                  write(66,fmt=3000)
     +                 " new      df1 arc at rec    :",ifirst, 
     +                 " time :", year, mon, day, hour, min, sec,
     +                 " PRN :", iPRN 
c
3000              format(2(a,I4),4I3,I5,a,I3)
c
                  dflag_new = .false.
                  arc_rec   = 1
                  ilast     = irec
                  mean_Ts   = Ts(irec)
                  sigma2_Ts = 0.0
               end if
            else
               dflag_new    = .false.
               arc_rec      = arc_rec+1
               ilast        = irec
               mean_Ts      = mean_Ts+bias/dfloat(arc_rec)
               if(arc_rec.EQ.1)then
               sigma2_Ts    = 0.0d0
               else
               sigma2_Ts    =(arc_rec-2)*sigma2_Ts/dfloat(arc_rec-1)
     &                      +(bias**2)/dfloat(arc_rec)
               endif
            end if
c
            ifore = irec
c
         end if
c
         irec = irec+1
c
      end do
c
      if((ifore.ne.0).and.(ifirst.eq.ifore))then
c
        motlr(ifore) = .true.
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
      return
c
      end
