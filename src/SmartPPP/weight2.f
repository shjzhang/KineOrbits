*
*  subroutine weight2
*      
      subroutine weight2(irec,elv,P)
c
c=======================================================================
c     ****f* SmartPPP/weight2.f
c
c   FUNCTION   
c   
*     weight2 of the observation equation
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
C     $Id: weight2.f.f,v 1.0 2009/07/28 $
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
      integer       irec
c
c     local
c
      real*8        elv
      real*8        P(MAX_SAT_NUM,MAX_SAT_NUM)
c
c     loop
c
      integer       i,j,k
      integer       iobs
      logical       debug
c
      debug     = .false.
c
c     weighting 
c
c     SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
         if(    observ_model.EQ.1)then
c
c++         weight for P3
c
            iobs         = (irec-1)*2 + 1
c
            P(iobs,iobs) = (dsin(elv))**2*sig_L3**2/sig_P3**2
c
c++         weight for P3
c
            iobs         = (irec-1)*2 + 2
c
            P(iobs,iobs) = (dsin(elv))**2
c
        elseif(observ_model.EQ.2)then
c
c++         weight for L3
c
            iobs         =  irec
c
            P(iobs,iobs) = (dsin(elv))**2
c
        endif
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         if(    observ_model.EQ.1)then
c
c++         weight for P3
c
            iobs         = (irec-1)*2 + 1
            if(debug)then
               write(*,*)'elv, sig_L3, sig_P3'
               write(*,*) elv, sig_L3, sig_P3
            endif
c
            P(iobs,iobs) = (dsin(elv))**2*sig_L3**2/sig_P3**2
c
            if(debug)then
               write(*,*) 'P'
               write(*,*)  P(iobs,iobs)
            endif
c
c++         weight for P3
c
            iobs         = (irec-1)*2 + 2
c
            P(iobs,iobs) = (dsin(elv))**2
c
            if(debug)then
               write(*,*) 'P'
               write(*,*)  P(iobs,iobs)
            endif
c
        elseif(observ_model.EQ.2)then
c
c++         weight for L3
c
            iobs         =  irec
c
            P(iobs,iobs) = (dsin(elv))**2
c
        endif
c
      else
c
         write(*,*) 'SmartPPP/weight2'
         write(*,*) 'Please make modification for this type'
         stop
c
      endif
c
      return
c
      end
