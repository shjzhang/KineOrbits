c
c   subroutine newcodelib
c
      subroutine newcodelib(nrec,iPRN,EPOCH,L3,P3,res,lsat,true_err)
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
c     P3          (R)           P3 
c
c   OUTPUT
c
c     true_err    (R)           true err or P3
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
      include      '../../include/newcode.h'
c
c     input/output variables
c
c     input
c
      integer       nrec
      integer       iPRN
c  
      real*8        EPOCH(MAX_OBS_REC)
      real*8        L3   (MAX_OBS_REC)
      real*8        P3   (MAX_OBS_REC)
      real*8        res  (MAX_OBS_REC)
      integer       lsat (MAX_OBS_REC)
c
c     output
c
      real*8        true_err(MAX_OBS_REC)
c
c     local variables
c
      real*8        EPOCH_Arc(MAX_OBS_REC)
      real*8        P3_Arc(MAX_OBS_REC)
      integer       P3_Arc_pntr(MAX_OBS_REC)
      real*8        Te_Arc(MAX_OBS_REC)
      integer       Te_Arc_pntr(MAX_OBS_REC)
c
      real*8        df5P3(MAX_OBS_REC)
      real*8        df5Te(MAX_OBS_REC)
      real*8        df5P3_Arc(MAX_OBS_REC)
      real*8        df5Te_Arc(MAX_OBS_REC)
      integer       df5P3_Arc_pntr(MAX_OBS_REC)
      integer       df5Te_Arc_pntr(MAX_OBS_REC)
c
      integer       ifst_arc_P3(MAX_ARC)
      integer       ilst_arc_P3(MAX_ARC)
      integer       iarc, narc
      integer       ifst, ilst
      integer       irec, ifore
      integer       npnt
c
      real*8        arcgap
      real*8        arclen
c
      logical       first
c
      do iarc=1, MAX_ARC
         ifst_arc_P3(iarc) = 0
         ilst_arc_P3(iarc) = 0
      enddo
c
      do irec=1, MAX_OBS_REC
         P3_Arc(irec) = 0.0
      enddo
c
      do irec=1, MAX_OBS_REC
         true_err(irec) = 0.0
      enddo
c
      do irec=1, MAX_OBS_REC
         df5P3(irec) = 0.0
         df5P3_Arc(irec) = 0.0
      enddo
c
c     5th order difference       
c     ====================
c
c++   separate the raw Time series data sets if arcgap > 30
c
      iarc  = 0
      ifst  = 1
      ifore = 1
      first = .true.
c
      do irec=1,nrec
c
         arcgap = dabs(EPOCH(irec)-EPOCH(ifore))/10.0d0
         arclen = dabs(EPOCH(irec)-EPOCH(ifst ))/10.0d0
c
         if(first.or.arcgap.gt.30)then
            iarc               = iarc + 1
            ifst               = irec 
            ifst_arc_P3(iarc)  = irec
            first              = .false.
            ilst_arc_P3(iarc)  = irec
         else
            ilst_arc_P3(iarc)  = irec
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
         ifst = ifst_arc_P3(iarc)
         ilst = ilst_arc_P3(iarc)
c
c        write(*,*) iarc, ifst_arc_P3(iarc), ilst_arc_P3(iarc)
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            P3_Arc     (npnt) = P3(irec)
            P3_Arc_pntr(irec) = iarc
         enddo
c
c++      calculate 5th order difference for every P3 arc
c
         call diff(P3_Arc,MAX_OBS_REC,5,df5P3_Arc)
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            df5P3(irec) = df5P3_Arc(npnt)
         enddo
c
         call diff5_solve1(ifst+5,ilst,df5P3,res,lsat,true_err)
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            Te_Arc     (npnt) = true_err(irec)
            Te_Arc_pntr(irec) = iarc
         enddo
c
         call diff(Te_Arc,MAX_OBS_REC,5,df5Te_Arc)
c
         npnt = 0
         do irec=ifst,ilst
            npnt = npnt + 1
            df5Te(irec) = df5Te_Arc(npnt)
         enddo
c
         do irec=ifst,ilst
            write(*,'(5I6,5F14.3)') 
     &            iPRN,iarc,ifst,ilst,irec,
     &            EPOCH(irec),res(irec),true_err(irec),
     &            df5P3(irec),df5Te(irec)
         enddo
c
      enddo
c
      return
c
      end
