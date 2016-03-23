C --- Subroutine generated on Wed Jul  1 15:55:09 CEST 2009

        subroutine fcnnut090701(mjd,X,Y,dX,dY)
        
C-------------------------------------------------------------------------C
C                                                                         C
C       Empirical model for FCN                                           C
C                                                                         C
C-------------------------------------------------------------------------C
C                                                                         C
C       Input: mjd, modified julian date                                  C
C                                                                         C
C       Output: X, Y, CIP offsets (microas)                               C
C               dX, dY, uncertainties on dX, dY (microas)                 C
C                                                                         C
C-------------------------------------------------------------------------C
C                                                                         C
C       Subroutine interface: none                                        C
C                                                                         C
C-------------------------------------------------------------------------C
C                                                                         C
C       Author: S. Lambert (sebastien.lambert@obspm.fr)                   C
C                                                                         C
C-------------------------------------------------------------------------C

        implicit none
        
C       Input variables

        double precision mjd

C       Output variables
        
        double precision X,Y    ! FCN contributions
        double precision dX,dY  ! uncertainties on X, Y
        
C       Internal variables

        integer i,j,N
        parameter(N=26)
        double precision pi,per,phi,mpe
        double precision axc,axs,ayc,ays
        double precision daxc,daxs,dayc,days,dt,t
        double precision TABLE(4,N)
        double precision DATE(N),XC(N),XS(N),YC(N),YS(N),SX(N),SY(N)

        pi=3.14159265358979323846D0
        
C       Mean prediction error

        mpe=0.1325D0                       ! microas per day

C       FCN parameters

        per=-430.21D0                      ! period in days
        phi=(2.D0*pi/per)*(mjd-51544.5D0)  ! phase in rad
                
C       Block data of amplitudes for X (microas)
        
      data ((TABLE(i,j),i=1,4),j=1,N) /
     . 45700.D0,     4.55D0,   -36.58D0,    19.72D0, ! 1984.0
     . 46066.D0,  -141.82D0,  -105.35D0,    11.12D0, ! 1985.0
     . 46431.D0,  -246.56D0,  -170.21D0,     9.47D0, ! 1986.0
     . 46796.D0,  -281.89D0,  -159.24D0,     8.65D0, ! 1987.0
     . 47161.D0,  -255.05D0,   -43.58D0,     8.11D0, ! 1988.0
     . 47527.D0,  -210.46D0,   -88.56D0,     7.31D0, ! 1989.0
     . 47892.D0,  -187.79D0,   -57.35D0,     6.41D0, ! 1990.0
     . 48257.D0,  -163.01D0,    26.26D0,     5.52D0, ! 1991.0
     . 48622.D0,  -141.24D0,    44.59D0,     4.77D0, ! 1992.0
     . 48988.D0,  -128.69D0,    28.62D0,     4.63D0, ! 1993.0
     . 49353.D0,  -108.94D0,    19.47D0,     3.91D0, ! 1994.0
     . 49718.D0,   -96.67D0,    19.66D0,     3.13D0, ! 1995.0
     . 50083.D0,  -104.03D0,    11.90D0,     2.95D0, ! 1996.0
     . 50449.D0,  -126.81D0,    30.42D0,     2.76D0, ! 1997.0
     . 50814.D0,   -81.85D0,    25.04D0,     2.65D0, ! 1998.0
     . 51179.D0,   -19.72D0,   -20.13D0,     2.63D0, ! 1999.0
     . 51544.D0,    10.80D0,   -76.81D0,     2.66D0, ! 2000.0
     . 51910.D0,    65.57D0,  -137.36D0,     2.53D0, ! 2001.0
     . 52275.D0,    78.16D0,  -127.14D0,     2.33D0, ! 2002.0
     . 52640.D0,   108.73D0,   -42.31D0,     2.15D0, ! 2003.0
     . 53005.D0,   117.56D0,    -1.42D0,     2.23D0, ! 2004.0
     . 53371.D0,   115.66D0,     5.73D0,     2.93D0, ! 2005.0
     . 53736.D0,   159.66D0,    24.16D0,     4.19D0, ! 2006.0
     . 54101.D0,   154.66D0,    61.16D0,     4.46D0, ! 2007.0
     . 54466.D0,   161.06D0,    98.41D0,     4.32D0, ! 2008.0
     . 54832.D0,   162.61D0,   159.21D0,     5.02D0/ ! 2009.0
          
C       Amplitudes extracted from the table

        do i=1,N
           DATE(i)=TABLE(1,i)
           XC(i)=TABLE(2,i)
           XS(i)=TABLE(3,i)
           SX(i)=TABLE(4,i)
           YC(i)=XS(i)
           YS(i)=-XC(i)
           SY(i)=SX(i)
        end do

C       Prediction of the amplitude at the input date

        if (mjd.le.DATE(1)) then
           axc=XC(1)
           axs=XS(1)
           ayc=YC(1)
           ays=YS(1)
        else if (mjd.ge.DATE(N)) then
           axc=XC(N)
           axs=XS(N)
           ayc=YC(N)
           ays=YS(N)
        else
           do i=1,N-1
              if (mjd.ge.DATE(i).and.mjd.lt.DATE(i+1)) then
                 t=mjd-DATE(i)
                 dt=DATE(i+1)-DATE(i)
                 daxc=XC(i+1)-XC(i)
                 daxs=XS(i+1)-XS(i)
                 dayc=YC(i+1)-YC(i)
                 days=YS(i+1)-YS(i)
                 axc=XC(i)+(daxc/dt)*t
                 axs=XS(i)+(daxs/dt)*t
                 ayc=YC(i)+(dayc/dt)*t
                 ays=YS(i)+(days/dt)*t
              end if
           end do
        end if
        
C       Computation of X and Y

        X=axc*dcos(phi)-axs*dsin(phi)  ! microas
        Y=ayc*dcos(phi)-ays*dsin(phi)  ! microas
        
C       Prediction of the uncertainty at the input date

        if (mjd.le.DATE(1)) then
           axc=SX(1)+mpe*(DATE(1)-mjd)
           axs=SX(1)+mpe*(DATE(1)-mjd)
           ayc=SY(1)+mpe*(DATE(1)-mjd)
           ays=SY(1)+mpe*(DATE(1)-mjd)
        else if (mjd.ge.DATE(N)) then
           axc=SX(N)+mpe*(mjd-DATE(N))
           axs=SX(N)+mpe*(mjd-DATE(N))
           ayc=SY(N)+mpe*(mjd-DATE(N))
           ays=SY(N)+mpe*(mjd-DATE(N))
        else
           do i=1,N-1
              if (mjd.ge.DATE(i).and.mjd.lt.DATE(i+1)) then
                 t=mjd-DATE(i)
                 dt=DATE(i+1)-DATE(i)
                 daxc=SX(i+1)-SX(i)
                 daxs=SX(i+1)-SX(i)
                 dayc=SY(i+1)-SY(i)
                 days=SY(i+1)-SY(i)
                 axc=dabs(SX(i)+(daxc/dt)*t)
                 axs=dabs(SX(i)+(daxs/dt)*t)
                 ayc=dabs(SY(i)+(dayc/dt)*t)
                 ays=dabs(SY(i)+(days/dt)*t)
              end if
           end do
        end if
        
C       Computation of the uncertainties

        dX=axc+axs  ! microas
        dY=ayc+ays  ! microas
        
        end
