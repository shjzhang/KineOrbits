*
*  subroutine weight
*      
      subroutine weight(iobs,iepo,elv,NNZP,NRP,RP,CP,P)
c
c=======================================================================
c     ****f* SmartPPP/weight.f
c
c   FUNCTION   
c   
*     weight of the observation equation
c
c   INPUTS
c
c     iobs        integer          number of observable
c     elv         real*8           elevation
c
c   OUTPUT
c
c     P           real*8           weight matrix in CSR format
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
c     2009/08/10                   build
c
c     ***
c
C     $Id: weight.f,v 1.0 2009/08/10 $
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
      integer       iobs
      integer       iepo
      integer       NRP
      integer       NNZP
      integer       CP(MAX_NNZP), RP(MAX_NRP+1)
c
      real*8        P(MAX_NNZP)
      real*8        elv
c
c     loop
c
      integer       i,j,k
c
c     weighting 
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c&&      Model1: traditional L3,P3
         if(    observ_model.EQ.1 .or. observ_model.EQ.3 )then
c
c           row no. of the weight matrix
            NRP      =(iobs-1)*2+1
            RP(NRP)  = NNZP + 1
c
c**         weight for P3 
c
            NNZP     =  NNZP + 1
c
            if(    weighting_function.eq.1)then
             P(NNZP) = 1.0d0/sig_P3**2*sig_L3**2
            CP(NNZP) = NRP  
            elseif(weighting_function.eq.2)then
             P(NNZP) = dsin(elv)**2*1.0d0/sig_P3**2*sig_L3**2
            CP(NNZP) = NRP  
            endif
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1)= NNZP+1
c
c           row no. of the weight matrix
            NRP      =(iobs-1)*2+2
            RP(NRP)  = NNZP + 1
c
c**         weight for L3 
c
            NNZP     =  NNZP + 1
c
            if(    weighting_function.eq.1)then
             P(NNZP) = 1.0d0 
            CP(NNZP) = NRP  
            elseif(weighting_function.eq.2)then
             P(NNZP) =(dsin(elv))**2
            CP(NNZP) = NRP  
            endif
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1) = NNZP+1
c
            if(NRP+1.GT.MAX_NRP)then
               write(*,*) '<weight> error'
               write(*,*) ' enlarge MAX_NRP in SmartPPP.h'
               stop
            endif
c
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2 .or. observ_model.EQ.4)then
c
c           row no. of the weight matrix
            NRP      = iobs
            RP(NRP)  = NNZP + 1
c
c**         weight for L3 
c
            NNZP     = NNZP + 1
c
            if(    weighting_function.eq.1)then
             P(NNZP) = 1.0d0 
            CP(NNZP) = NRP  
            elseif(weighting_function.eq.2)then
             P(NNZP) =(dsin(elv))**2 
            CP(NNZP) = NRP  
            endif
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RP(NRP+1) = NNZP+1
c
            if(NRP+1.GT.MAX_NRP)then
               write(*,*) '<weight> error'
               write(*,*) ' enlarge MAX_NRP in SmartPPP.h'
               stop
            endif
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
