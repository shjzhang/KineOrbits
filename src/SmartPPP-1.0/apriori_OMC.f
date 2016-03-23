*
*  subroutine apriori_OMC
*      
      subroutine apriori_OMC(   iepo,iobs,idx_iamb,N3,aprN3,
     &                          NNZL,NRL,RL,CL,L)
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
c     iepo        integer          observation number index
c     iobs        integer          observation number index
c     isat        integer          satellite number
c     iamb_isat   integer          ambiguity number for iobs
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
      integer       iepo
      integer       iobs
      integer       iPRN
      integer       idx_iamb
c
      real*8        N3
      real*8        aprN3
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
      real*8        dis_windup
c
c     loop
c
      integer       i,j,k
c
c     common
c
      real*8        rcv_pco  (MAX_FREQ,3)
      real*8        dpco     (3)
      real*8        xZEN     (MAX_REC_ZEN)
      real*8        yant_pcv1(MAX_FREQ,MAX_REC_ZEN)
      real*8        yant_pcv2(MAX_FREQ,MAX_REC_AZI,MAX_REC_ZEN)
      integer       nrec_zen
c
      real*8        trs_pco  (MAX_PRN,3)
c*
      common /atx/  rcv_pco,dpco,xzen,yant_pcv1,yant_pcv2,nrec_zen,
     &              trs_pco
c
c**   SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c&&      Model1: traditional L3,P3
         if(    observ_model.EQ.1)then
c
c           row no. of the residual matrix
            NRL       = iobs
            RL(NRL)   = NNZL + 1
c
c**         residual for L3 
c
            NNZL      = NNZL + 1
            if(NNZL.GE.MAX_NNZL)then
               write(*,*) '<apriori_OMC> error'
               write(*,*) ' enlarge MAX_NNZL in SmartPPP.h'
               stop
            endif
c
             L(NNZL)  = aprN3 - N3
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
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2)then
c
c           row no. of the residual matrix
            NRL       = iobs
            RL(NRL)   = NNZL + 1
c
c**         residual for L3 
c
            NNZL      = NNZL + 1
            if(NNZL.GE.MAX_NNZL)then
               write(*,*) '<apriori_OMC> error'
               write(*,*) ' enlarge MAX_NNZL in SmartPPP.h'
               stop
            endif
c
             L(NNZL)  = aprN3 - N3
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
c&&      Model3:Model2 + state variation constraints
         elseif(observ_model.EQ.3)then
c
c           row no. of the residual matrix
            NRL       = iobs
            RL(NRL)   = NNZL + 1
c
c**         residual for L3 
c
            NNZL      = NNZL + 1
            if(NNZL.GE.MAX_NNZL)then
               write(*,*) '<apriori_OMC> error'
               write(*,*) ' enlarge MAX_NNZL in SmartPPP.h'
               stop
            endif
c
             L(NNZL)  = aprN3 - N3
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
c&&      Model4:Model2 + clock variation constraints
         elseif(observ_model.EQ.4)then
c
            write(*,*) 'SmartPPP/coeff'
            write(*,*) 'Please modify for this observation model'
            stop
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
