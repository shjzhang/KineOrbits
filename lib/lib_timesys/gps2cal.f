*
*  Procedure CAL2GPS
*  
      subroutine GPS2CAL(GPST,YEAR,MONTH,DAY,HOUR,MINUTE,SECOND)
*********1*********2*********3*********4*********5*********6*********7**
*
*  FUNCTION:            Conversion of GPS time to  Calender time
*
*  PARAMETERS:
*
*   NAME    I/O  type   DESCRIPTION 
*   ----    ---  ----   ------------------------------------------------
*   GPST    I    REAL   GPS time
*   YEAR    I    I      YEAR
*   MONTH   I    I      MONTH
*   DAY     I    I      DAY
*   HOUR    I    I      HOUR
*   MINUTE  I    I      MINUTE
*   SECOND  I    F      SECOND
*      
*  Notes:
*
*  Example:
*   example 1:
*       GPS time in SECONDs:
*           630763200.0d0
*       Calender time in format
*       YEAR/MONTH/DAY/HOUR/MINUTE/SECOND:   
*           2000/01/01/12/00/00     
*   example 2:
*       Calender time:
*           2004/05/01/10/05/15  
*       GPS time in GPS Week and Seconds in Week
*           GPS Week:           1268
*           Seconds in Week:    554715.0d0
*           
*
*  History:
*
*  Author: S.J.Zhang. 2007/06/10, build this subroutine
*
*********1*********2*********3*********4*********5*********6*********7**
c
c  Declarations_of_Variable_Type
c
      implicit real*8(a-h, o-z)
c
c  Declarations_of_Input_and_Output_Arguments
c
      real*8     GPST
      integer*4  GPS_Week,YEAR,MONTH,DAY,Hour,Minute
c
c  Declarations_of_Local_Variables
c
      integer*4  jan61980, jan11901
      integer*4  guess, more, yDAY, MJD, DAYs_fr_jan11901
      integer*4  leap_MONTH_DAY, delta_yrs, num_four_yrs
      integer*4  regu_MONTH_DAY, DAYs_left, YEARs_so_far
***
      real*8     Sec_of_Week, Second
      real*8     Sec_per_DAY
      real*8     fMJD
      
      dimension  regu_MONTH_DAY(13)
      dimension  leap_MONTH_DAY(13)
c
c  Initialization
c
      data regu_MONTH_DAY / 0,   31,  59,  90,  120, 151, 181, 212,
     &                      243, 273, 304, 334, 365 /
      data leap_MONTH_DAY / 0,   31,  60,  91,  121, 152, 182, 213,
     &                      244, 274, 305, 335, 366 /
c
      jan61980      = 44244
      jan11901      = 15385
      Sec_per_DAY   = 86400.0d0
c
      GPS_Week    = GPST/604800
      Sec_of_Week = GPST - GPS_Week*604800
c
      MJD   = GPS_Week*7 + Sec_of_Week/Sec_per_DAY + jan61980
      fMJD  = dmod(Sec_of_Week, Sec_per_DAY)/Sec_per_DAY
c      
      DAYs_fr_jan11901  = MJD - jan11901
      num_four_yrs      = DAYs_fr_jan11901/1461
      YEARs_so_far      = 1901 + 4*num_four_yrs
      DAYs_left         = DAYs_fr_jan11901 - 1461*num_four_yrs
      delta_yrs         = DAYs_left/365 - DAYs_left/1460
c      
      YEAR              = YEARs_so_far + delta_yrs
      yDAY              = DAYs_left - 365*delta_yrs + 1
      Hour              = fMJD*24.0d0
      Minute            = fMJD*1440.0d0 - Hour*60.0d0
      Second            = fMJD*86400.0d0 - Hour*3600.0d0 - Minute*60.0d0
c
      if(Second.ge.60.0d0)then
        Second = Second - 60.0d0
        Minute = Minute + 1.0d0
      endif
      if(Minute.ge.60.0d0)then
        Minute = Minute - 60.0d0
        Hour   = Hour   + 1.0d0
      endif
      if(Hour.ge.24.0d0)then
        Hour   = Hour   - 24.0d0
        yday   = yday   + 1.0d0
      endif
c      
      guess = yDAY*0.0320d0 + 1
      more  = 0
      if( mod(YEAR,4) .eq. 0 ) then
        if( (yDAY - leap_MONTH_DAY(guess+1)) .gt. 0 ) more = 1
        MONTH   = guess + more
        DAY    = yDAY - leap_MONTH_DAY(guess+more)
      else
        if( (yDAY - regu_MONTH_DAY(guess+1)) .gt. 0 ) more = 1
        MONTH   = guess + more
        DAY    = yDAY - regu_MONTH_DAY(guess+more)
      endif
c     
      return
c
      end

