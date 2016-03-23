c
c subroutine N12fit
c
      subroutine ionos_N12fit(nrec,iPRN,EPOCH,wflag,lotlr,L4,P4,S1,S2,
     &                        N12fit)
c
c=======================================================================
c     ****f* qualicontr/ionos_N12fit
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
c     L4          (R)           L1-L2
c     P4          (R)           P1-P2
c
c   OUTPUT
c
c     N12fit      (R)           L4-polyfit(P4)
c
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: ionos_N12fit.f,v 1.0 2009/07/11 $
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
      integer       MAX_ORDER
      parameter(    MAX_ORDER=21)
c
c     input/output variables
c
c     input
c
      integer       nrec
      integer       iPRN
      logical       lotlr(MAX_OBS_REC)
      logical       wflag(MAX_OBS_REC)
c  
      real*8        EPOCH(MAX_OBS_REC), L4(MAX_OBS_REC), P4(MAX_OBS_REC)
      real*8        S1(MAX_OBS_REC)
      real*8        S2(MAX_OBS_REC)
c
c     output
c
      real*8        N12fit(MAX_OBS_REC)
c
c     local
c
      real*8        x(MAX_OBS_REC), y(MAX_OBS_REC), yfit(MAX_OBS_REC)
      real*8        z(MAX_OBS_REC)
      real*8        coeff(MAX_ORDER)
      real*8        arcgap, arclen
c
      integer       irec, arec, zrec, jrec, mpts
      integer       npts, ndeg, nmax, narc 
      integer       ifst, ilst
c
      logical       find
c
      integer       i,j,k
      integer       ik,order
c
      do i=1,MAX_OBS_REC
         x(i) = 0.0d0
         y(i) = 0.0d0
         yfit(i) = 0.0d0
      enddo
c
      irec = 0
      narc = 0
      npts = 0
      do while(irec.le.nrec)
         irec = irec + 1
c        processing last widelane arc
c
         find = (narc.gt.0.and.wflag(irec)).or.
     &          (irec.eq.nrec.and.npts.gt.0)
c
         if(find)then
c
            order = 3
c
            call polyfit(npts,x,y,order,coeff)
c
            call polyval(npts,x,order,coeff,yfit)
c
            ik = 0
            do jrec = ifst,ilst
               if(.not.lotlr(jrec))then
                  ik = ik + 1
                  if(x(ik).NE.EPOCH(jrec))then
                     write(*,*) 'qualicontr/ionos_N12fit.f'
                     write(*,*) 'x(ik).ne.EPOCH(jrec)'
                     stop
                  endif
                  N12fit(jrec) = L4(jrec) - yfit(ik)
               endif
            enddo
c
            if(ik.NE.npts)then
               write(*,*) 'qualicontr/ionos_N12fit.f'
               write(*,*) 'y, not correspond with yfit'
            endif

            npts = 0
            find = .false.
c
         endif
c
c        arc information
c
         if(.not.lotlr(irec))then
c
            if(wflag(irec))then
               narc = narc + 1 
               npts = npts + 1
c              x,y
               x(npts) = EPOCH(irec)
               y(npts) = P4(irec)
               z(npts) = L4(irec)
c              start record
               ifst = irec
            else
               npts = npts + 1
c              x,y
               x(npts) = EPOCH(irec)
               y(npts) = P4(irec)
               z(npts) = L4(irec)
c              end record
               ilst = irec
            endif
c
         endif
      enddo
c
      return
c
      end
c
