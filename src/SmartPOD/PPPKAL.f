c
c  subroutine pppkal
c
      subroutine pppkal(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* SmartPPP/pppkal
c
c   FUNCTION   
c   
c     kalman filter for PPP (Precise Point Positioning)
c
c     It applies to the linear system:  Y= A x, that is to say:
c
c     [  ]   [                                                     ] [dx ]
c     [  ]   [                                                     ] [dy ]
c     [  ]   [                                                     ] [dz ]
c     [  ] = [                                                     ] [dt ]
c     [yi]   [(x0-xs)/ro (y0-ys)/ro (z0-zs)/ro 1  0  ...  1  ...  0] [b1 ]
c     [  ]   [                                            ^        ] [...]
c     [  ]   [                                            |        ] [bk ]
c     [  ]   [                                            |        ] [...]
c     [  ]   [                                            |        ] [b32]
c                     coeff. for the "bk"                 |
c                     if observation [yi] corresponds_____|
c                     to the satellite with PRN=k
c
c
c
c       where:
c              [b1 ... bk ... b32] are the bias of the phase arcs
c                                  for the satellites PRN01,...,PRN32  
c
c
c       being:
c                    [1/(sigma_y1)^2                       ]
c                    [     ...                             ]
c                 W= [        1/(sigma_yi)^2               ]
c                    [                    ...              ]
c                    [                      1/(sigma_yn)^2 ]
c
c
c
c          [Pxx                    ]       [Qxx                    ] 
c          [   Pyy                 ]       [   Qyy                 ] 
c          [      Pzz              ]       [      Qzz              ] 
c      P0= [         Ptt           ]    Q= [         Qtt           ] 
c          [            Pb1        ]       [            Qb1        ]  
c          [               ...     ]       [               ...     ] 
c          [                  Pb32 ]       [                  Qb32 ]           
c
c
c
c
c          [fi_x                          ]    - If a cycle-slip is produced
c          [    fi_y                      ]      in the sat. PRN=k:
c          [        fi_z                  ]       fi_bk=0, Qbk= 1e10 m2
c      fi= [            fi_t              ]
c          [                fi_b1         ]    - If no cycle-slip is produced
c          [                     ...      ]       fi_bk=1, Qbk= 0
c          [                        fi_b32]   
c                                              * "flag" allows us to identify
c                                                 cycle-slips. 
c                                                 If "flag" equal with 1=>cycle-slip
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
C     $Id: pppkal.f,v 1.0 2009/08/25 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
      integer       nmax
      parameter    (nmax=MAX_SAT_NUM+5)
c
c     input
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     local
c
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       EPOCH_flag
      integer       iPRN
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
      integer       tmp_nsat
c
      real*8        tmp_time
      real*8        tmp_time_fore
      integer       tmp_flag(MAX_SAT_NUM)
      integer       tmp_aPRN(MAX_SAT_NUM)
c
      real*8        tmp_L1, tmp_L2, tmp_P1, tmp_P2
      real*8        tmp_L3(MAX_SAT_NUM)
      real*8        tmp_P3(MAX_SAT_NUM)
c
      logical       more
c
c     variables for phasedif
c
      real*8        time0
c
      integer       iobs0, iepo0, iamb0, nobs0
      integer       nsat0
      integer       flag0(MAX_SAT_NUM)
      integer       aPRN0(MAX_SAT_NUM)
c
      real*8        L30(MAX_SAT_NUM)
      real*8        P30(MAX_SAT_NUM)
c!
      real*8        time1
c
      integer       iobs1, iepo1, iamb1,nobs1
      integer       nsat1
      integer       flag1(MAX_SAT_NUM)
      integer       aPRN1(MAX_SAT_NUM)
c
      real*8        L31(MAX_SAT_NUM)
      real*8        P31(MAX_SAT_NUM)
c
      real*8        xv0(6)
      real*8        sig_xv0(6)
c
c     pppkal
c
      integer       nvar
      integer       nobs 
      integer       flag
c
      real*8        A  (MAX_SAT_NUM,nmax)
      real*8        AT (nmax       ,MAX_SAT_NUM)
      real*8        W  (MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        y  (MAX_SAT_NUM,1)
      real*8        APA(MAX_SAT_NUM,MAX_SAT_NUM  )
c
c     Kalman filter, similar to least square 
c
      real*8        AWy(nmax       ,1)
      real*8        AWA(nmax       ,nmax)
      real*8        PX (nmax       ,1)
      real*8        yy (nmax       ,1)
c
      real*8        Phi(nmax       ,nmax)
      real*8        Q  (nmax       ,nmax)
      real*8        P  (nmax       ,nmax)
c
c     Extended kalman filter
c
      real*8        Ki (nmax       ,MAX_SAT_NUM  )        
      real*8        KT (MAX_SAT_NUM,nmax)        
      real*8        Ky (nmax       ,1)
      real*8        KA (nmax       ,nmax)
      real*8        KAT(nmax       ,nmax)
      real*8        KWK(nmax       ,nmax)
      real*8        KPK(nmax       ,nmax)
c
      real*8        x0 (nmax)
      real*8        sig_x0(nmax)
      real*8        x1 (nmax,1)
c
c     loop
      integer       i, k, irec
c
c     INITIAL FOR Kalman Filter
c
      do i=1,MAX_SAT_NUM
      do k=1,nmax
         A(i,k)  = 0.0d0
      enddo
      enddo
c
      do i=1,nmax
      do k=1,MAX_SAT_NUM
         AT(i,k)  = 0.0d0
      enddo
      enddo
c
c
      do i=1,MAX_SAT_NUM
      do k=1,MAX_SAT_NUM
         W  (i,k) = 0.0d0
         APA(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,MAX_SAT_NUM
         y(i,1) = 0.0d0
      enddo
c
c     kalman filter similar to least square
c
      do i=1,nmax
      do k=1,nmax
         P  (i,k) = 0.0d0
         Phi(i,k) = 0.0d0
         Q  (i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,nmax
         AWy(i,1) = 0.0d0
         PX (i,1) = 0.0d0
         yy (i,1) = 0.0d0
      enddo
c
      do i=1,nmax
      do k=1,nmax
         AWA(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,nmax
      do k=1,MAX_SAT_NUM
         Ki(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,nmax
         x0(i)   = 0.0d0
         x1(i,1) = 0.0d0
      enddo
c
c     INITIAL FOR READ DATA
c
      do i=1,MAX_SAT_NUM
         tmp_L3(i) = 0.0d0
         tmp_P3(i) = 0.0d0
      enddo
c
      do i=1,6
         xv0(i) = 0.0d0
         sig_xv0(i) = 0.0d0
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
           elseif(trim(cobs_typ(1,i)).eq.'LA')then
             idx_LA = i
           elseif(trim(cobs_typ(1,i)).eq.'C1')then
             idx_C1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P1')then
             idx_P1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P2')then
             idx_P2 = i
           elseif(trim(cobs_typ(1,i)).eq.'S1')then
             idx_S1 = i
           elseif(trim(cobs_typ(1,i)).eq.'S2')then
             idx_S2 = i
           elseif(trim(cobs_typ(1,i)).eq.'SA')then
             idx_SA = i
           endif
         enddo
c
c        double-frequency receiver ??
         if(idx_L1.EQ.0.or.idx_L2.EQ.0)then
            write(*,*) 'SmartPPP/extrobs'
            write(*,*) '  not double-frequency GPS receiver'
            stop
         endif
c
      else
c
         write(*,*) "SmartPPP/extrobs"
         write(*,*) '  Please modifiy to process rinex 3.00' 
         stop
c
      endif
c
c     Precise Point Positioning with Kalman Filter
c     ********************************************
c
c
c++   inital for kalman filtering loop
c
      iobs1 = 0
      iepo1 = 0
      iamb1 = 0
c
      iobs0 = 0
      iepo0 = 0
      iamb0 = 0
c
      time0 = 0.0d0
      time1 = 0.0d0     
c
c     kalman filter declaration matrix
c
      nvar = nmax
      nobs = MAX_SAT_NUM
c      
c&&   state transition matrix: phi_xx,yy,zz,tt,trop ..
c
      x0(1)    =  6378137.0d0
      x0(2)    =  6378137.0d0
      x0(3)    =  6378137.0d0
      x0(4)    =  0.0d0
c
      Phi(1,1) = 1.0     !x
      Phi(2,2) = 1.0     !y
      Phi(3,3) = 1.0     !z
      Phi(4,4) = 1.0     !t
c
      do i=5,nvar
      Phi(i,i) = 1.0d0   ! ambiguity bias
      enddo
c
c&&   A priori covariance values (in meters)...
c
      P  (1,1) = 1.0d+10 !xx
      P  (2,2) = 1.0d+10 !yy
      P  (3,3) = 1.0d+10 !zz
      P  (4,4) = 1.0d+10 !tt
c
      do i=5,nvar
      P  (i,i) = 1.0d-10 !ambiguity bias
      enddo
c
c&&   Process noise matrix (in meters).......
c
      Q  (1,1) =1.0d+10 ! zz
      Q  (2,2) =1.0d+10 ! yy
      Q  (3,3) =1.0d+10 ! zz
      Q  (4,4) =1.0d+10 ! tt 
c
c++   inital for data block reading loop
c
      more = .true.
      tmp_nsat = 0
      tmp_time = 0.0d0
      tmp_time_fore = 0.0d0
c
c++   BEGINNING OF READ DATA BLOCK loop
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
      tmp_aPRN(tmp_nsat) = EPOCH_iPRN
c
      tmp_L1             = EPOCH_OBS(idx_L1)*lam_L1_GPS
      tmp_L2             = EPOCH_OBS(idx_L2)*lam_L2_GPS
      tmp_P1             = EPOCH_OBS(idx_P1)
      tmp_P2             = EPOCH_OBS(idx_P2)
c
c     LC combination
c
      tmp_L3(tmp_nsat)   = 
     &      (f1_GPS**2*tmp_L1-f2_GPS**2*tmp_L2)/(f1_GPS**2-f2_GPS**2)
      tmp_P3(tmp_nsat)   =
     &      (f1_GPS**2*tmp_P1-f2_GPS**2*tmp_P2)/(f1_GPS**2-f2_GPS**2)
c
c     store the last epoch time
c
      tmp_time_fore = tmp_time
c
      goto 100
c
c++   END read EPOCH data block
c
 200  continue
c
c++   kalman filtering
c
      if(tmp_nsat.GE.5)then
c
         iepo1 = iepo1 + 1
c
         nsat1 = tmp_nsat
         time1 = tmp_time_fore
c
         do i=1, nsat1
c           observables           
            L31(i)   = tmp_L3(i)
            P31(i)   = tmp_P3(i)
c           aPRN
            aPRN1(i) = tmp_aPRN(i)
            flag1(i) = tmp_flag(i)
         enddo
c
c++      Completing Phi and Q matrix accoring to flag
c
         do i=1, nsat1
c
            flag = flag1(i)
            iPRN = aPRN1(i)
c
            if(observ_model.EQ.1)then
               if(flag.EQ.1)then
                  Q  (iPRN+4,iPRN+4) = 1.0d+10
                  Phi(iPRN+4,iPRN+4) = 0.0d0
               endif
            elseif(observ_model.EQ.2)then
               if(flag.EQ.1)then
                  Q  (iPRN+4,iPRN+4) = 1.0d+4
c????? are these right? neeed to be checked and corrected.2009.10.24
                  Phi(iPRN+4,iPRN+4) = L31(i) - P31(i) 
               endif
            endif
c
         enddo
c
c        coordinate and clock all white noise
c
         call ppossub(time1,nsat1,aPRN1,flag1,L31,P31,
     &                x0,sig_x0)
c
         P(1,1) = 1.0d+10 !xx
         P(2,2) = 1.0d+10 !yy
         P(3,3) = 1.0d+10 !zz
         P(4,4) = 1.0d+10 !tt
c
c++      BEGIN fordware propagation:
c
         do i=1,nvar
            x0(i) = Phi(i,i)*x0(i)
         enddo
c
c        ---------------------------------
c        P:= P_(n)=phi(n)*P(n-1)*phi(n)'+Q(n) ................
c
         do i=1,nvar
            P(i,i)=Phi(i,i)*P(i,i)*Phi(i,i)+Q(i,i)
         enddo
c
         write(*,*)time1,(xv0(k),k=1,4)
         write(*,*)time1,(x0 (k),k=1,4)
c
c        END of fordware propagation.
c        =================================
c
c++      BEGIN design matrix,weight and OMC, A,W,y:
c
         call compsObsEq2(nsat1,aPRN1,L31,P31,flag1,time1,nvar,x0,A,W,y)
c        
c        END of design matrix,weight and OMC, A,W,y:
c        =================================
c
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c++      BEGIN of Measurement update
C$$c
C$$c     inv(W) ....
C$$c
C$$      write(*,*) nobs
C$$c
C$$      do i=1,nobs
C$$         write(*,'(20F8.3)') (W(i,k),k=1,nobs)
C$$      enddo
C$$c
C$$      call chlinv(W,nobs)
C$$c
C$$      call mtxinv(W,nobs)
C$$c
C$$      call mtxtrs(A,AT,nobs,nvar)
C$$c
C$$      call maxbxc(A,P,AT,APA,nobs,nvar,nvar,nobs)
C$$c
C$$c     APA = inv( inv(W) + APA )
C$$      do i=1,nobs
C$$      do k=1,nobs
C$$         APA(i,k) = W(i,k)+APA(i,k)
C$$      enddo
C$$      enddo
C$$      call mtxinv(APA,nobs)
C$$c
C$$c++   kalman gain : Ki
C$$      call maxbxc(P,AT,APA,Ki,nvar,nvar,nobs,nobs)
C$$c
C$$c     Ki*yi
C$$      call mtxmul(Ki,y,Ky,nvar,nobs,1)        
C$$c
C$$c++   x(n) = x_(n) + Ky
C$$      do i=1,nvar
C$$         x0(i) = x0(i) + Ky(i,1)
C$$      enddo
C$$c
C$$c++   KA
C$$      call mtxmul(Ki,A,KA,nvar,nobs,nvar)
C$$c
C$$c     KA = I - KA
C$$      do i=1,nvar
C$$      do k=1,nvar
C$$         KA(i,k) = 1.0d0 - KA(i,k)
C$$      enddo
C$$      enddo
C$$c     (I-KA)^(T)
C$$      call mtxtrs(KA,KAT,nvar,nvar)
C$$c
C$$c     P(n) = (I-KA)*P_(n)*(I-KA)^T + KWK
C$$c          =  KPK + KWK
C$$      call mtxtrs(Ki,KT, nvar, nobs)
C$$      call maxbxc(KA,P,KAT,KPK,nvar,nvar,nvar,nvar)
C$$      call maxbxc(K, W,KT, KWK,nvar,nobs,nobs,nvar)
C$$c
C$$      do i=1,nvar
C$$      do k=1,nvar
C$$         P(i,k) = KPK(i,k) + KWK(i,k)
C$$      enddo
C$$      enddo
C++      END of Measurement update of Extended kalman filter
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
         call mtxtrs(A,AT,nobs,nvar)
c
         call maxbxc(AT,W ,y,AWy,nvar,nobs,nobs,1)
         call maxbxc(AT,W ,A,AWA,nvar,nobs,nobs,nvar)
c
c        write(*,*) 'P'
c        do i=1,nvar
c           write(*,*) P(i,i)
c        enddo
c
c        P = inv(P)
c        call mtxinv(P,nvar)
         call chlinv(P,nvar)
c
c
c        PX = inv(P) * x^_(n)
         do i=1,nvar
            x1(i,1) = x0(i)
         enddo
c
         call mtxmul(P,X1,PX,nvar,nvar,1)
c
c        P(n)=inv[inv(P_(n))+A'(n)*W(n)*A(n)]==>  P:=inv[P + AWA]
         do i=1,nvar
         do k=1,nvar
            P(i,k) = P(i,k) + AWA(i,k)           
         enddo
         enddo
c
c        write(*,*) 'P'
c        do i=1,nvar
c           write(*,*) P(i,i)
c        enddo
c
c        call mtxinv(P,nvar)
         call chlinv(P,nvar)
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&       x^(n)=P(n)*[inv(P_(n))*x^_(n)+A'(n)*W(n)*Y(n)] => x:=P(n)*[PIx + AWy]
c$       do i=1,nvar
c$          yy(i,1) = PX(i,1) + AWy(i,1)
c$       enddo
c$       call mtxmul(P, yy,x1,nvar,nvar,1)
c$       do i=1,nvar
c$          x0(i) = x1(i,1)
c$       enddo
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c        x^(n) = x^(n-1) + dx0 = x^(n-1) + {inv[P + AWA]}*{A'(n)*W(n)*Y(n)}
         call mtxmul(P,AWy,x1,nvar,nvar,1)
c
         do i=1,nvar
            x0(i) = x0(i) + x1(i,1)
         enddo
c
c++      END of Measurement update
c        =================================
c
c++      Reinitializing variables for the next iteration ...   
c   
         do i=5,nvar
            Phi(i,i) = 1.0d0   ! ambiguity bias
            Q  (i,i) = 0.0d0
         enddo
c
         do i=1,nobs
         do k=1,nobs
            W  (i,k) = 0.0d0
            APA(i,k) = 0.0d0
         enddo
         enddo
c
         do i=1,6
            xv0(i) = 0.0d0
         enddo
c
c++      write parameters
c           
         write(201,'(5F14.3)') time1,(x0(k),k=1,4)
         write(  *,'(5F14.3)') time1,(x0(k),k=1,4)
c
c++      store current information into temporary arry
c
         iepo0 = iepo1
         nsat0 = nsat1 
         time0 = time1
c
         do i=1, nsat0
c        observables
         L30(i)   = L31(i)
         P30(i)   = P31(i)
c        aPRN 
         aPRN0(i) = aPRN1(i)
         flag0(i) = flag1(i)
         enddo
c
      endif
c
      tmp_nsat = 0
c
c     END of DATA FILE 
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
