*
*  subroutine apriori_OMC
*      
      subroutine apriori_statvar_OMC(iobs,svr0,svr1,
     &                               NNZL,NRL,RL,CL,L)
c
c=======================================================================
c     ****f* SmartPPP/apriori_OMC.f
c
c   FUNCTION   
c   
*     apriori_OMC of the observation equation for Preciese Point Positioning
*     with Least Square Batch method
c
c   INPUTS
c
c   OUTPUT
c
c     NNZL        integer          number of non-zero of L
c     RL          integer          row index of L
c     CL          integer          column index of L
c     L           real*8           Observed Minus Computed value,CSR
c                                  format
c   REVISION
c
c     2009/08/10                   build
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: apriori_OMC.f,v 1.0 2009/08/10 $
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
      include      '../../include/igs.h'
      include      '../../include/atx.h'
c
      real*8        pi
      parameter (   pi = 3.1415926535897932D0 )
c
c     input/output
c
      integer       iobs
c
      real*8        svr0(3)
      real*8        svr1(3)
c
      integer       NRL
      integer       NCL
      integer       NNZL
      integer       CL(MAX_NNZL), RL(MAX_NRL+1)
c
      real*8        L (MAX_NNZL)
c
c     local
c
c     loop
c
      integer       i,j,k
c
c**   SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c&&      Model3: traditional L3,P3 + state_variation
         if(    observ_model.EQ.3)then
c
c**         residual for state variation: vx
c
            NRL       = iobs + 1
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(1) - svr1(1)
            CL(NNZL)  = 1  
c
c**         residual for state variation: vy
c
            NRL       = iobs + 2
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(2) - svr1(2)
            CL(NNZL)  = 1  
c
c**         residual for state variation: vz
c
            NRL       = iobs + 3
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(3) - svr1(3)
            CL(NNZL)  = 1  
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RL(NRL+1) = NNZL+1
c
            if(NRL+1.GT.MAX_NRL)then
               write(*,*) '<apriori_OMC> error'
               write(*,*) ' enlarge MAX_NRL in SmartPPP.h'
               stop
            endif
c
c**      Model4:receiver clock-decoupled L3 + state_variation
         elseif(observ_model.EQ.4)then
c
c**         residual for state variation: vx
c
            NRL       = iobs + 1
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(1) - svr1(1)
            CL(NNZL)  = 1  
c
c**         residual for state variation: vy
c
            NRL       = iobs + 2
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(2) - svr1(2)
            CL(NNZL)  = 1  
c
c**         residual for state variation: vz
c
            NRL       = iobs + 3
            RL(NRL)   = NNZL + 1
c*
            NNZL      = NNZL + 1
c
             L(NNZL)  = svr0(3) - svr1(3)
            CL(NNZL)  = 1  
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RL(NRL+1) = NNZL+1
c
            if(NRL+1.GT.MAX_NRL)then
               write(*,*) '<apriori_OMC> error'
               write(*,*) ' enlarge MAX_NRL in SmartPPP.h'
               stop
            endif
c
         endif
c
c**   GEODETIC
c
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         write(*,*) 'SmartPPP/apriori_OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
         stop
c
      else
c
         write(*,*) 'SmartPPP/apriori_OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
c
      endif
c
      return
c
      end
