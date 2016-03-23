*
*  Procedure CAL2GPS
*  
      subroutine CAL2GPS(YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,GPST)
*********1*********2*********3*********4*********5*********6*********7**
*
*  FUNCTION:            Conversion of Calender time(given in the format:
*                       YEAR, MONTH, DAY, HOUR:MINUTE:SECOND) to GPS time in 
*                       SECONDs.GPS reference time is 1981.01.06 00:00:00      
*
*  PARAMETERS:
*
*   NAME    I/O  type   DESCRIPTION 
*   ----    ---  ----   ------------------------------------------------
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
*       Calender time in format YEAR/MONTH/DAY/HOUR/MINUTE/SECOND:   
*           2000/01/01/12/00/00     
*       GPS time in SECONDs:
*           630763200.0d0
*   example 2:
*       Calender time:
*           2004/05/01/10/05/15  
*       GPS time in GPS week and seconds in week
*           GPS week:           1268
*           Seconds in week:    554715.0d0
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
      INTEGER   YEAR
      INTEGER   MONTH
      INTEGER   DAY
      INTEGER   HOUR
      INTEGER   MINUTE
***
      REAL*8    SECOND
      REAL*8    GPST
c
c  Declarations_of_LoCAL_Variables      
c
      integer   y,m  
      integer   DAY_of_WEEK
      integer   DAY_of_YEAR
      integer   TOTAL_DAY 
      integer   DAY_in_MONTH(12)
      integer   GPS_WEEK              
      real*8    SEC_of_WEEK
c
c  Initialization
c      
      data DAY_in_MONTH /31,28,31,30,31,30,31,31,30,31,30,31/
c
c  Judge whether the time range are overflowed. And 
c
c     
      if(YEAR.lt.50) then
        YEAR = YEAR + 2000
      endif
***
      if((YEAR.gt.50).and.(YEAR.lt.100)) then
        YEAR = YEAR + 1900
      endif
***     
      if(SECOND.eq.60.0d0) then
        SECOND = 0.0d0
        MINUTE = MINUTE + 1
      endif
***
      if(MINUTE.eq.60) then
        MINUTE = 0
        HOUR   = HOUR + 1 
      endif
***    
      if(HOUR.eq.24) then
        HOUR = 0
        DAY  = DAY + 1
      endif
c
c  Judge the CALender time range
c      
      if( (YEAR  .lt.1981).or.
     +    (MONTH .lt.1   ).or.
     +    (MONTH .gt.12  ).or.
     +    (DAY   .lt.1   ).or.
     +    (DAY   .gt.31  )    )then
        write(*,*) 'Fatal error in subroutine CAL2GPS.f'
        write(*,*) '      wrong CALender time!!!!'
        write(*,*) 'Please check the Calender time'
        stop
        GPS_WEEK   = 0
      endif 
c    
c  Accumulate DAYs in the present YEAR 
c
      if(MONTH.eq.1)then
        DAY_of_YEAR = DAY
      else
        DAY_of_YEAR = 0
        do m = 1, (MONTH-1)
          DAY_of_YEAR = DAY_of_YEAR + DAY_in_MONTH(m)
          if(m.eq.2) then
            if(mod(YEAR,4).eq.0.and.mod(YEAR,100).ne.0.or.
     &                              mod(YEAR,400).eq.0     )then
              DAY_of_YEAR = DAY_of_YEAR+1
            endif
          endif
        end do
        DAY_of_YEAR = DAY_of_YEAR+DAY
      endif
c
c  Accumulate all the DAYs from the YEAR 1981 to the present YEAR 
c  Notes:
c    There are 365 DAYs in the ordinary YEAR!!!
c    Thear are 366 DAYs in the leap     YEAR!!! 
c
      TOTAL_DAY = 360
      do y= 1981,(YEAR-1)
          TOTAL_DAY = TOTAL_DAY+365
          if(mod(y,4).eq.0.and.mod(y,100).ne.0.or.
     +                         mod(y,400).eq.0    ) then
             TOTAL_DAY  =TOTAL_DAY+1
          endif
      end do
c 
c  Calculate the GPS Week and Seconds in the week according to the 
c  TOTAL days refering to 1981.1.6 and the Hour:Minute:Second in the
c  day. 
c
      TOTAL_DAY   = TOTAL_DAY+DAY_of_YEAR
      GPS_WEEK    = TOTAL_DAY/7
      DAY_of_WEEK = TOTAL_DAY-7*GPS_WEEK  
      SEC_of_WEEK = DAY_of_WEEK*86400 + HOUR*3600 + MINUTE*60 +SECOND  
c
c  Result
c  
      GPST        = GPS_WEEK*604800+SEC_of_WEEK
c
      return
c
      end
