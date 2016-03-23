   
C Procedure           CAL2SEC
C
      DOUBLE PRECISION FUNCTION     CAL2SEC                              
     *        ( YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC )
C
C
C                            Purpose
C
C  This double precision function (CALendar date to SEConds) takes the
C  components of a calendar date and time
C
C            Year / Month / Day , Hour : Minute : Second.Frac
C
C  and returns the corresponding seconds past the reference date (JDREF)
C  this library.
C
C
C                            Input_Arguments
C
C  YEAR     is the year.
C  MONTH    is the month number.
C  DAY      is the day.
C  HOUR     is the hour.
C  MINUTE   is the minute.
C  SECOND   is the second.
C  FRAC     is the fractional seconds.
C
C
C                            Output_Arguments
C
C                            Declarations_of_External_Functions
C
      INTEGER            DATE2J
      DOUBLE PRECISION   JD2SEC
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      INTEGER            YEAR
      INTEGER            MONTH
      INTEGER            DAY
      INTEGER            HOUR
      INTEGER            MINUTE
      INTEGER            SECOND
      DOUBLE PRECISION   FRAC
C
C
C                            Declarations_of_Local_Variables
C
      DOUBLE PRECISION   JD
C
C
C                     Method
C-&
C***********************************************************************
      if(YEAR.lt.50)then
        YEAR = YEAR + 2000
      endif
      if(YEAR.ge.50.and.YEAR.lt.100)then
        YEAR = YEAR + 1900
      endif
C
C1  Call DATE2J to compute the double precision Julian date at the start
C1  the current day.
      JD = DATE2J( YEAR, MONTH, DAY ) - 0.5D0
C
C1  Call JD2SEC to compute the seconds past the reference date at the st
C1  of the current day and then add in the seconds in the remaining frac
C1  day.
      CAL2SEC =   JD2SEC( JD )                                           
     *         + HOUR   * 3600.0D0                                      
     *         + MINUTE *   60.0D0                                      
     *         + SECOND                                                 
     *         + FRAC
C
      RETURN
      END   
