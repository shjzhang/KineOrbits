*
*  subroutine xv_weight
*      
      subroutine xv_weight(iobs,elv,P)
c
c=======================================================================
c     ****f* SmartPPP/xv_weight.f
c
c   FUNCTION   
c   
*     xv_weight of the observation equation
c
c   INPUTS
c
c     iobs        integer          number of observable
c
c   OUTPUT
c
c     P           real*8           weight matrix of the observ equation
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/21                   build
c
c     ***
c
C     $Id: xv_weight.f.f,v 1.0 2009/08/21 $
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
      integer       iobs
c
      real*8        elv
c
c     local
c
      real*8        P(MAX_SAT_NUM,MAX_SAT_NUM)
c
c     loop
c
      integer       i,j,k
c
c     weighting 
c
      P(iobs,iobs) = (dsin(elv))**2
c
      return
c
      end
