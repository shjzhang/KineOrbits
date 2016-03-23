c
c   subroutine diff5_filter
c
      subroutine diff5_filter(nrec,iPRN,EPOCH,Ts,lotlr,dflag)
c
c=======================================================================
c     ****f* qualicontr/diff5_filter
c
c   FUNCTION   
c
c     Edit 5-order difference Time series with recursive formulas. 
c     Cycle slip will be detected EPOCH by EPOCH.
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     df5Ts       (R)           5th order difference Time series
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
C     $Id: diff5_filter.f,v 1.0 2009/07/11 $
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
c     output
c
      logical       lotlr(MAX_OBS_REC)
      logical       dflag(MAX_OBS_REC)
c
c     local variables
c
      real*8        Ts_Arc(MAX_OBS_REC)
      real*8        EPOCH_Arc(MAX_OBS_REC)
      integer       Ts_Arc_pntr(MAX_OBS_REC)
c
      real*8        df5Ts(MAX_OBS_REC)
      real*8        df5Ts_Arc(MAX_OBS_REC)
      integer       df5Ts_Arc_pntr(MAX_OBS_REC)
c
      integer       ifst_arc_Ts(MAX_ARC)
      integer       ilst_arc_Ts(MAX_ARC)
      integer       iarc, narc
      integer       ifst, ilst
      logical       first
      integer       irec, ifore
c
c     recover raw abnomal value
c
      real*8        df5Ts1(MAX_OBS_REC)
      real*8        abnml(MAX_OBS_REC)
      real*8        abnml_arc(MAX_OBS_REC)
      integer       abnml_arc_pntr(MAX_OBS_REC)
      integer       abnml_arc_pnts(MAX_ARC)
      real*8        df5abnml(MAX_OBS_REC)
      real*8        df5abnml_Arc(MAX_OBS_REC)
c
      logical       first1
      integer       iarc_abnml, narc_abnml
      integer       ifst_arc_abnml(MAX_ARC)
      integer       ilst_arc_abnml(MAX_ARC)
      integer       ifst1, ilst1, irec1
      integer       ifst2
      integer       npnt
c
c     Abnormal recursive filter
c
      real*8        dTs, Ts_fore
      real*8        bias, bias_fore, limit, arcgap
      real*8        mean_Ts, sigma_Ts, sigma2_Ts
c  
      logical       dflag_new
c
      integer       i
      integer       year, mon, day, hour, min, sec
      real*8        frac
c
      integer       outlier(MAX_PRN)
c
      do irec=1, MAX_OBS_REC
         abnml_arc(irec) = 0.0d0
      enddo
c
      do iarc=1, MAX_ARC
         ifst_arc_abnml(iarc) = 0
         ilst_arc_abnml(iarc) = 0
         ifst_arc_Ts(iarc) = 0
         ilst_arc_Ts(iarc) = 0
      enddo
c
      do iarc=1, MAX_ARC
         abnml_arc_pnts(iarc) = 0
      enddo
c
      do irec=1, MAX_OBS_REC
         abnml_arc_pntr(irec) = 0
      enddo
c
      do irec=1, MAX_OBS_REC
         Ts_Arc(irec) = 0.0
      enddo
c
      do irec=1, MAX_OBS_REC
         df5Ts(irec) = 0.0
         df5Ts1(irec) = 0.0
         df5Ts_Arc(irec) = 0.0
      enddo
c
      do irec=1, MAX_OBS_REC
         df5abnml(irec) = 0.0
         df5abnml_Arc(irec) = 0.0
      enddo
c
      do irec=1, MAX_OBS_REC
         abnml(irec) = 0.0d0
      enddo
c
c     5th order difference       
c     ====================
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
            ifst_arc_Ts(iarc)  = irec
            first = .false.
            ilst_arc_Ts(iarc)  = ifst_arc_Ts(iarc)
         else
            ilst_arc_Ts(iarc)  = irec
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
         ifst = ifst_arc_Ts(iarc)
         ilst = ilst_arc_Ts(iarc)
c
c        write(*,*) iarc, ifst_arc_Ts(iarc), ilst_arc_Ts(iarc)
c
         npnt = 0
         do irec=ifst,ilst
c
            npnt = npnt + 1
CZc
CZc         cycleslip test: 
CZ          if(iarc.EQ.1.and.irec.ge.21.and.irec.le.30)then
CZ          Ts_Arc(npnt) = Ts(irec) + 0.1096 
CZ          else
CZ          Ts_Arc(npnt) = Ts(irec)
CZ          end if
CZc         outlier test
CZ          if(iarc.EQ.1.and.irec.eq.21)then
CZ          Ts_Arc(npnt) = Ts(irec) + 0.5
CZ          endif
CZ          if(iarc.EQ.1.and.irec.eq.22)then
CZ          Ts_Arc(npnt) = Ts(irec) - 0.3
CZ          endif
c
            Ts_Arc(npnt) = Ts(irec)
            Ts_Arc_pntr(irec) = iarc
c
         enddo
c
c++      calculate 5th order difference for every Ts arc
c
         call diff(Ts_Arc,MAX_OBS_REC,5,df5Ts_Arc)
c
c++      df5Ts
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            df5Ts(irec) = df5Ts_Arc(npnt)
         enddo
c        
      enddo
c
c     recover the raw time series from dif5Ts
c     =======================================
c
      limit = 0.08
c
      do iarc=1, narc
c
         ifst  = ifst_arc_Ts(iarc)
         ilst  = ilst_arc_Ts(iarc)
c
         ifore = ifst
         iarc_abnml = 0
         first = .true.
c
c        determine abnormal arc: fisrt and last record
c
         do irec=ifst,ilst
c
            bias = dabs(df5Ts(irec))
c
            if(bias.gt.limit)then
c
               arcgap = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
c
               if(first.or.(arcgap.gt.2))then
                  iarc_abnml = iarc_abnml + 1
                  ifst_arc_abnml(iarc_abnml)=irec
                  first = .false.
                  ilst_arc_abnml(iarc_abnml)=ifst_arc_abnml(iarc_abnml)
               else
                  ilst_arc_abnml(iarc_abnml)=irec
               endif
               ifore =  irec
            endif
         enddo
c
         narc_abnml = iarc_abnml
c
         first1 = .true.
         do iarc_abnml=1,narc_abnml
c
            ifst1 = ifst_arc_abnml(iarc_abnml)
            ilst1 = ilst_arc_abnml(iarc_abnml)
c
c++         solve the 5th order difference equation
            npnt = 0
            do irec1=ifst1,ilst1
               npnt = npnt + 1
               abnml_arc_pntr(irec1) = iarc_abnml
            enddo
c
c           solve the 5th difference equation
c
            if(      npnt.ge.5
     &         .and.(abs(ifst1-ifst).gt.10)
     &         .and.(abs(ilst1-ilst).gt.10))then
c
               call diff5_solve(ifst1,ilst1,df5Ts,abnml)
c
CZ             write(*,*) 'abnml '
CZc
CZ             do i=ifst1-5,ilst1
CZ                write(*,*) i, df5Ts(i),abnml(i),lotlr(i),dflag(i)
CZ             enddo
c
c              filtering with recovered IF residual(abnomal values)
c
               call abnml_filter(ifst1-5,ilst1,iPRN,EPOCH,abnml,
     &                                              lotlr,dflag)
c
CZ             call diff(abnml,MAX_OBS_REC,5,df5abnml)
CZc
CZ             write(*,*) 'abnml resolved'
CZc
CZ             do i=ifst1-5,ilst1
CZ                write(*,*) i, df5Ts(i),abnml(i),df5abnml(i),
CZ   &                       lotlr(i),dflag(i)
CZ             enddo
c
            endif
c
         enddo
c
      enddo
c
      return
c
      end
