c
c  subroutine x_wrt
c
      subroutine x_wrt(rnxvsn,
     &                 nsat_sys,csat_sys,
     &                 nobs_typ,cobs_typ,ndim,x0,sig,Qxx,
     &                 NRA,NCA,RA,CA,A,NRP,NCP,RP,CP,P,
     &                 NRL,NCL,RL,CL,L)
c
c=======================================================================
c     ****f* SmartPPP/x_wrt
c
c   FUNCTION   
c   
c     write related information 
c
c   INPUTS
c
c     rnxvsn     real*8         rinex version
c     csat_sys   characeter     returned satellite systems in rinex 
c                               file
c     nobs_typ   integer        observation types number for existed 
c                               satellite systems.
c     cobs_typ   character      observation types for existed 
c                               satellite systems.
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: x_wrt.f,v 1.0 2009/08/10 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
      real*8        pi
      parameter (   pi = 3.1415926535897932D0 )
c
c     input
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      integer       ndim
      real*8        sig, Qxx(MAX_PMS)
c
      integer       NRA,  NRP,  NRL
      integer       NCA,  NCP,  NCL
      integer       NNZA, NNZP, NNZL
c
      integer       CA(MAX_NNZA), RA(MAX_NRA+1)
      integer       CP(MAX_NNZP), RP(MAX_NRP+1)
      integer       CL(MAX_NNZL), RL(MAX_NRL+1)
c
      real*8        A (MAX_NNZA)
      real*8        P (MAX_NNZP)
      real*8        L (MAX_NNZL)
c
c     local
c
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       EPOCH_flag
      character*3   cPRN
      integer       EPOCH_iPRN
      character*3   EPOCH_cPRN
      real*8        EPOCH_OBS(MAX_OBS_TYP)       
      integer       EPOCH_LLI(MAX_OBS_TYP)
      integer       EPOCH_SNR(MAX_OBS_TYP)
c
      integer       idx_L1, idx_L2, idx_LA 
      integer       idx_C1, idx_P1, idx_P2
      integer       idx_S1, idx_S2, idx_SA
c
c     read data block
c
      integer       tmp_nsat
      integer       tmp_flag(MAX_SAT_NUM)
      integer       tmp_iPRN(MAX_SAT_NUM)
c
      real*8        tmp_time
      real*8        tmp_time_fore
      real*8        tmp_L1, tmp_L2, tmp_P1, tmp_P2
      real*8        tmp_L3(MAX_SAT_NUM)
      real*8        tmp_P3(MAX_SAT_NUM)
c
c     x_wrt
c
      integer       iter
      logical       convgd
      logical       more
c
      integer       iobs, irec, iepo, iamb
      integer       iPRN, isat
      integer       flag
      integer       iAMB_iPRN(MAX_PRN), iAMB_iSAT(MAX_PRN)
      integer       Namb_base
      integer       idx_iamb
c
      real*8        x0(MAX_PMS)
      real*8        xrcv(3),crcv,xtrs(3),vtrs(3),ctrs
      real*8        sig_x,sig_y,sig_z,sig_dt,sig_N3
      real*8        Qxxi,Qyyi,Qzzi,Qtti
      real*8        GDOP
      real*8        rP3,rL3
      real*8        L3, P3
      real*8        time
      real*8        elv
      real*8        N3
c
c     loop
      integer       i, k 
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
      real*8        aEPO(MAX_EPO)
c
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN,
     &              aEPO
c
c     Initialization
c
      do i=1,MAX_SAT_NUM
         tmp_L3(i) = 0.0d0
         tmp_P3(i) = 0.0d0
      enddo
c
      idx_L1 = 0
      idx_L2 = 0
c
      if(rnxvsn.lt.3.00)then
c
         do i=1, nobs_typ(1)
           if(    trim(cobs_typ(1,i)).eq.'L1')then
             idx_L1 = i
           elseif(trim(cobs_typ(1,i)).eq.'L2')then
             idx_L2 = i
           elseif(trim(cobs_typ(1,i)).eq.'P1')then
             idx_P1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P2')then
             idx_P2 = i
           endif
         enddo
c
c        double-frequency receiver ??
         if(idx_L1.EQ.0.or.idx_L2.EQ.0)then
            write(*,*) 'SmartPPP/x_wrt'
            write(*,*) 'not double-frequency GPS receiver'
            stop
         endif
c
      else
c
         write(*,*) "SmartPPP/x_wrt"
         write(*,*) 'Please modifiy to process rinex 3.00' 
         stop
c
      endif
c
c     ambiguity
c
      do i=1,MAX_PRN
         iAMB_iPRN(i) = 0
         iAMB_iSAT(i) = 0
      enddo
c
c     read observation data block
c     ***************************
c
      iobs = 0
      iepo = 0
      iamb = 0
c
      more = .true.
      tmp_nsat = 0
      tmp_time = 0.0d0
      tmp_time_fore = 0.0d0
c
 100  continue
c
      read(111, fmt=2000, end=300)
     +      irec, 
     +      EPOCH,       EPOCH_iPRN, (EPOCH_OBS(k),
     +      EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 2000 format(I6, F18.7, (X,I3), 12(F14.3,I1,I1))
c
      tmp_time = EPOCH
c
c     continue reading observables at the same epoch
c
 110  continue
c
c     finish this epoch reading ???
c
      if(tmp_time.gt.tmp_time_fore.and.tmp_nsat.gt.0)then
c
         goto 200
c
      endif
c
c     pass the outliers
c
      if(EPOCH_LLI(idx_L1).EQ.9)then
c
         goto 100
c
      endif
c
      tmp_nsat           = tmp_nsat + 1
      tmp_flag(tmp_nsat) = EPOCH_LLI(idx_L1)
      tmp_iPRN(tmp_nsat) = EPOCH_iPRN
c
c     store last epoch time
c
      tmp_time_fore      = tmp_time
c
      goto 100
c
c     END of reading data block
c     *************************
c
 200  continue
c
c     compose observation equation: NOTES tmp_time_fore !!!
c     =====================================================
c
      if(tmp_nsat.GE.5)then
c
         iepo = iepo + 1
c        time
         Time = tmp_time_fore
c
         if(Time.NE.aEPO(iepo))then
         write(*,*) 'SmartPPP/x_wrt'
         write(*,*) 'Time and iepo not match'
         stop
         endif
c
c        receiver position write
c        ***********************
c
         xrcv(1) = x0 ( (iepo-1)*4+1 )
         xrcv(2) = x0 ( (iepo-1)*4+2 )
         xrcv(3) = x0 ( (iepo-1)*4+3 )
         crcv    = x0 ( (iepo-1)*4+4 )
c
         Qxxi    = Qxx( (iepo-1)*4+1 )
         Qyyi    = Qxx( (iepo-1)*4+2 )
         Qzzi    = Qxx( (iepo-1)*4+3 )
         Qtti    = Qxx( (iepo-1)*4+4 )
c
         sig_x   = sig*dsqrt( Qxxi )
         sig_y   = sig*dsqrt( Qyyi )
         sig_z   = sig*dsqrt( Qzzi )
         sig_dt  = sig*dsqrt( Qtti )
c
         GDOP    = dsqrt( Qxxi+Qyyi+Qzzi+Qtti )
c
         write(201,'(F14.3,4F14.3)') time,(xrcv(k),k=1,3),crcv
c        
c        information
c        ***********
c
         write(301,'(F14.3,I6    )') time,tmp_nsat
c
c        statis
c        ******
c   
         write(303,'(F14.3,6F8.3 )') time,sig,
     &                                    sig_x,sig_y,sig_z,sig_dt,GDOP
c
c**      compose obseq one by one at the same epoch
c
         do i=1,tmp_nsat
c
c           flag, iPRN
c
            iPRN = tmp_iPRN(i)
c
            iobs = iobs + 1
            isat = iSAT_iPRN(iPRN)
c
c**         ambiguity for isat increase
c
            flag = tmp_flag(i)
c
            if(flag.EQ.1)then
            iAMB_iSAT(isat) = iAMB_iSAT(isat) + 1
            endif
c
            Namb_base = 0
            do k=1,(isat-1)
            Namb_base= Namb_base + NAMB_iSAT(k)
            enddo
c
            idx_iamb = 4*NEPO + Namb_base + iAMB_iSAT(isat) 
c
c**         ambiguity
c
            N3       = x0(idx_iamb)
            sig_N3   = sig*dsqrt( Qxx(idx_iamb) )
c
c           GPS position and velocity, clock error
c
            call corr_trs(Time,iPRN,xrcv,xtrs,vtrs,ctrs)
c
c**         elvation
c
            call x_elv(xrcv,xtrs,elv)
c
c**         residual
c
            if(    observ_model.EQ.1 .or. observ_model.EQ.3)then
c
               rP3      = L( RL( (iobs-1)*2+1) )
               rL3      = L( RL( (iobs-1)*2+2) )
c
               write(301,'(I3,5F8.3)') 
     &               iPRN,rP3,rL3,N3,sig_N3,elv*180.0d0/pi
c
            elseif(observ_model.EQ.2 .or. observ_model.EQ.4)then
c
               rP3      = 0.0d0
               rL3      = L( RL( iobs ) )
c
               write(301,'(I3,5F8.3)') 
     &               iPRN,rP3,rL3,N3,sig_N3,elv*180.0d0/pi
c
            else
               write(*,*) 'SmartPPP/x_wrt'
               write(*,*) 'please extend for this model'
               stop
            endif
c
         enddo
c
      endif
c
      tmp_nsat = 0
c
c     END of DATA
c
      if(.not.more)then
c
         goto 400
c
      endif
c
      goto 110
c
  300 continue
c
      more = .false.
c
c     read the last EPOCH observation data block
      goto 200
c
  400 continue
c
      return
c
      end
