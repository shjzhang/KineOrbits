c
c   subroutine ionos_filter
c
      subroutine ionos_filter(nrec,iPRN,EPOCH,wflag,lotlr,NwCL,
     &                        N12fit,N12flag)
c
c=======================================================================
c     ****f* qualicontr/ionos_filter
c
c   FUNCTION   
c
c     look at ionosphere combination to fix remaining ambiguity.First
c     correct ionosphere combination for widelane ambiguity. 
c
c   INPUTS
c
c     nrec        (I)           total record number for iPRN
c     iPRN        (I)           satellite PRN number
c     EPOCH       (R)           EPOCH time in GPS time
c     wflag       (L)           widelane flags
c     lotlr       (L)           bad observation flag
c     NwCL        (R)           cycles in widelane
c     N12fit      (R)           L4-polyfit(P4)
c
c   OUTPUT
c
c     N12flag       (L)           widelane flag
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
C     $Id: ionos_filter.f,v 1.0 2009/11/11 $
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
      real*8        EPOCH(MAX_OBS_REC), NwCL(MAX_OBS_REC)
      real*8        N12fit(MAX_OBS_REC)
c
      logical       lotlr(MAX_OBS_REC)
      logical       wflag(MAX_OBS_REC)
c
c     output
      logical       N12flag(MAX_OBS_REC)
c
c     local variables
c
      real*8        NwCL_last ! widelane cycle last
      real*8        N12last
c
      integer       irec, ifirst, ifore, ilast, arc_rec
      integer       i,j,k
c  
      logical       first, first2, Tflag
c
      integer       year, mon, day, hour, min, sec
      real*8        frac
      real*8        N12_Err, NwCL_Err
      real*8        tol_after, tol_ion
c
      real*8        dN2       
      real*8        N2_fore   
      real*8        N2_last ! the last one of the previous arc
      real*8        mean_N2   
      real*8        sigma2_N2 
      real*8        arcgap
      real*8        bias
      real*8        sigma_N2
      logical       N2flag_new
c
c     N2
c
      real*8        N2(MAX_OBS_REC)
      logical       N2flag(MAX_OBS_REC)
c
c     common
c
      integer       outlier(MAX_PRN)
c
      common /statistic/ outlier
c
c     setting N12flag and correcnt widelane ambiguity
c
      do irec=1,nrec
         if(.not.lotlr(irec))then
            N12flag(irec) =  wflag(irec)
         endif
      enddo
c
      do irec=1,nrec
         if(.not.lotlr(irec))then
            N12fit(irec)=(N12fit(irec) 
     &                  + NwCL(irec)*lam_L1_GPS)/(lam_L2_GPS-lam_L1_GPS)
         endif
      enddo
c
c     filter initial
c     ==============
c
c     !!! effective for N12 filter
c
C$      first  = .true.
C$      ilast  = 0
C$      N12last = 1.0d+20
C$      tol_ion = 6
C$      Tflag = .false.
C$c
C$      do irec=1,nrec
C$         if(.not.lotlr(irec))then
C$c
C$            N12_Err = dabs(N12fit(irec) - N12last)
C$c
C$            if(     first
C$     &         .or. Tflag
C$     &         .or. N12_Err.gt.tol_ion)then
C$c
C$               if(.not.Tflag)then
C$                  Tflag = .true.
C$               elseif(N12_Err.gt.tol_ion)then
C$                  lotlr(ilast) = .true.
C$                  if(N12flag(ilast)) N12flag(irec) = .true.
C$                  Tflag = .true.
C$               elseif(N12_Err.gt.1.0.and.N12flag(ilast))then
C$                  lotlr(ilast) = .true.
C$                  N12flag(irec) = .true.
C$                  Tflag = .true.
C$               elseif(N12_Err.lt.1.0)then
C$                  if(first)then
C$                     first = .false.
C$                  endif
C$                  N12flag(ilast) = .true.
C$                  Tflag = .false.
C$               endif
C$            endif
C$            ilast = irec
C$            N12last = N12fit(irec)
C$         endif
C$      enddo
C$c
C$      if(ilast.ne.0)then
C$         lotlr(ilast) = Tflag
C$      endif
c
c$    do i=ilast-5,ilast
c$    write(*,*) i,N12fit(i),wflag(i),N12flag(i),lotlr(i)
c$    enddo
c
      do irec=1,nrec
         N2(irec) = N12fit(irec)
         N2flag(irec) = N12flag(irec)
      enddo
c
      ilast      = 1
      ifore      = 0
      ifirst     = 1
      first      = .true.
c
      dN2        = 0
      N2_fore    = 0.0d0
      N2_last    = 0.0d0
      mean_N2    = 0
      sigma2_N2  = 0
      N2flag_new = .false.
c
c     N2 filter
c     =========
c
      do irec = 1, nrec
c
c        if not outlier, then filter
         if(.not.lotlr(irec)) then
c
            bias    = N2(irec)-N2_last
            tol_ion = 3 ! unit[cycles]
            arcgap  = dabs(EPOCH(irec)-EPOCH(ifore))/60.0d0
c
c           cycleslip or outliers ??
            if(     first   
     &         .or. N2flag_new
     &         .or.(dabs(bias).gt.tol_ion)
     +         .or.(arcgap    .gt.MAX_ARC_GAP))then 
c
               dN2 = dabs(N2(irec)-N2_fore)
c
               if(.not.N2flag_new)then
c
                  ifirst       = irec
                  N2flag_new   = .true.
                  N2_fore      = N2(irec)
c
               elseif(dN2.gt.1.00d0) then
c
                  lotlr(ifirst)= .true.
c 
c!                pass flags to current record
                  if(wflag(ifirst)) wflag(irec) = .true.
                  if(N2flag(ifirst)) N2flag(irec) = .true.
                  outlier(iPRN)= outlier(iPRN) + 1
c
                  ifirst       = irec
                  N2_fore      = N2(irec)
                  N2flag_new   = .true.
c
               elseif(arcgap.gt.MAX_ARC_GAP)then
c
                  lotlr(ifirst)= .true. 
c
c!                pass flags to current record
                  if(wflag(ifirst)) wflag(irec) = .true.
                  if(N2flag(ifirst)) N2flag(irec) = .true.
                  outlier(iPRN)= outlier(iPRN) + 1
c
                  ifirst       = irec
                  N2flag_new   = .true.
                  N2_fore      = N2(irec)
c
               else
c
c                 if 2 outliers "agree" to within 1 cycle
c                 take this as the new reference arc
                  N2flag(ifirst) = .true.
c
c                 first arc
                  if(first) then            
                     first = .false.
                  else
                     sigma_N2 = dsqrt(sigma2_N2)
                  end if
                  N2flag_new = .false.
                  N2_last    = N2(irec)
c
               endif
c
            else
c
               N2flag_new    = .false.
               N2_last       = N2(irec)
c
            endif
c
            ifore = irec
c
         endif
c
      enddo
c
      if((ifore.ne.0).and.(ifirst.eq.ifore))then
c
        lotlr(ifore) = .true.
        outlier(iPRN) = outlier(iPRN) + 1
c
      endif
c
      if(arc_rec.gt.0)then
         sigma_N2 = dsqrt(sigma2_N2)
      endif
c
c     store flags on N2 into N12 !!!
c     ******************************
c
      do irec=1,nrec
         N12fit(irec)  =  N2(irec)     
         N12flag(irec) =  N2flag(irec) 
      enddo
c
      return
c
      end
