*
*  subroutine apriori_weight
*      
      subroutine apriori_weight(iepo,iobs,idx_iamb,
     &                          NNZP,NRP,RP,CP,P)
c
c=======================================================================
c     ****f* SmartPPP/apriori_weight.f
c
c   FUNCTION   
c   
*     apriori_weight of the observation equation
c
c   INPUTS
c
c     iobs        integer          number of observable
c     elv         real*8           elevation
c
c   OUTPUT
c
c     P           real*8           apriori_weight matrix in CSR format
c     RP          integer          row
c     CP          integer          column
c     NNZP        integer          number of non-zero of P
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/20                   build
c
c     ***
c
C     $Id: apriori_weight.f,v 1.0 2009/08/20 $
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
      integer       weighting_function
      parameter(    weighting_function = 2)
c
c     input/output
c
      integer       iepo
      integer       iobs
      integer       idx_iamb
c
      integer       NRP
      integer       NNZP
      integer       CP(MAX_NNZP), RP(MAX_NRP+1)
c
      real*8        P(MAX_NNZP)
c
c     loop
c
      integer       i,j,k
c
c     apriori_weighting 
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c&&      Model1: traditional L3,P3
         if(    observ_model.EQ.1)then
c
c           row no. of the apriori_weight matrix
            NRP      = iobs
            RP(NRP)  = NNZP + 1
c
c**         apriori_weight for P3 
c
            NNZP     =  NNZP + 1
c
             P(NNZP) = 1.0d0/100.0d0**2*sig_L3**2
            CP(NNZP) = NRP  
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1)= NNZP+1
c
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2)then
c
c           row no. of the apriori_weight matrix
            NRP      = iobs
            RP(NRP)  = NNZP + 1
c
c**         apriori_weight for L3 
c
            NNZP     = NNZP + 1
c
             P(NNZP) = 1.0d0/100.0d0**2*sig_L3**2
            CP(NNZP) = NRP  
c
c           write(*,*) NRP,NNZP,P(NNZP),CP(NNZP)
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1) = NNZP+1
c
            if(NRP+1.GT.MAX_NRP)then
               write(*,*) '<apriori_weight> error'
               write(*,*) ' enlarge MAX_NRP in SmartPPP.h'
               stop
            endif
c
            if(NNZP.GT.MAX_NNZP)then
               write(*,*) '<apriori_weight> error'
               write(*,*) ' enlarge MAX_NNZP in SmartPPP.h'
               stop
            endif
c
c&&      Model3:Model2 + state variation constraints
         elseif(observ_model.EQ.3)then
c
c           row no. of the apriori_weight matrix
            NRP      = iobs
            RP(NRP)  = NNZP + 1
c
c**         apriori_weight for L3 
c
            NNZP     = NNZP + 1
c
             P(NNZP) = 1.0d0/100.0d0**2*sig_L3**2
            CP(NNZP) = NRP  
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1) = NNZP+1
c
            if(NRP+1.GT.MAX_NRP)then
               write(*,*) '<apriori_weight> error'
               write(*,*) ' enlarge MAX_NRP in SmartPPP.h'
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
         write(*,*) 'SmartPPP/OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
         stop
c
      else
c
         write(*,*) 'SmartPPP/OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
c
      endif
c
c
      return
c
      end
