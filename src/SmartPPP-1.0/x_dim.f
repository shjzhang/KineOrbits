c
c  subroutine x_dim
c
      subroutine x_dim(ndim,nobs)
c
c=======================================================================
c     ****f* SmartPPP/x_dim
c
c   FUNCTION   
c   
c     determine the parameter dimension for Precise Point Positioning
c     with least square batch method
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     ndim       integer        observable number
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: x_dim.f,v 1.0 2009/08/10 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output
c
      integer       ndim, ntrp
      integer       nobs
c
c     local
c
      integer       i,j,k
c
c     common
c     ******
c
      integer       NSAT, NEPO
      integer       NAMB, NREC
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
c
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN
c
c     spacebore: not affected by tropospheric
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c        3 pos + 1 clock   
c
         ndim = 4*NEPO + namb   
c
         if(    observ_model.EQ.1)then
c
         nobs = 2*NREC
c
         elseif(observ_model.EQ.2)then
c
         nobs = 1*NREC + namb
c
         elseif(observ_model.EQ.3)then
c
         nobs = 1*NREC + namb + 4*(NEPO-1)
c
         endif
c
c     other types,such as geodetic,should consider tropospheric zpd
c
      elseif(MARKER_TYPE.EQ.'GEODETIC'  )then
c
c        3 pos + 1 clock + 1 tropospheric zpd
c   
         ntrp = (TIME_SPAN(2) - TIME_SPAN(1))/600.0d0
c
         if(    trim(trop_estmt_func).EQ.'piece_constant')then
c
         ndim = 4*NEPO + ntrp*1 + namb
c
         elseif(trim(trop_estmt_func).EQ.'piece_linear')then
c
         ndim = 4*NEPO + ntrp*2 + namb
c
         endif
c
         if(    observ_model.EQ.1)then
         nobs = 2*NREC
         elseif(observ_model.EQ.2)then
         nobs = 1*NREC + namb
         elseif(observ_model.EQ.3)then
         nobs = 1*NREC + namb + 4*(NEPO-1)
         endif
c
      else
c
         write(*,*) '<SmartPPP/x_dim>'
         write(*,*) ' please modify for this Marker Type'
         stop
c
      endif
c
      return
c
      end
