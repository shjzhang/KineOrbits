*
*  subroutine weight1
*      
      subroutine weight1(iobs,elv,P)
c
c=======================================================================
c     ****f* SmartPPP/weight1.f
c
c   FUNCTION   
c   
*     weight1 of the observation equation
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
c     2009/08/05                   build
c
c     ***
c
C     $Id: weight1.f.f,v 1.0 2009/07/28 $
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
c     local
c
      real*8        elv
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
