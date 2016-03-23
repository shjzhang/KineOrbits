   
C Procedure           CAL2JD
C
      DOUBLE PRECISION FUNCTION     CAL2JD                              
     *        ( YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC )
C
C
C                            Purpose
C
C  This double precision function (CALendar date to Julian Date) takes t
C  components of a calendar date and time
C
C            Year / Month / Day , Hour : Minute : Second.Frac
C
C  and returns the corresponding Julian date.
C
C  The values for MONTH, DAY, HOUR, MINUTE, SECOND, and FRACtional secon
C  not have to lie within the usual range, i.e., MONTH=13 and DAY=212 ar
C  legitimate.
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
C                     Method
C-&
C***********************************************************************
C
C1    Call DATE2J to convert the date (year, month, day) to the integer
C1    Julian date at noon of that day.
      CAL2JD = DATE2J( YEAR, MONTH, DAY ) - 0.5D0 +                     
     *         (3600.D0*HOUR + 60.D0*MINUTE + SECOND + FRAC) / 86400.D0
C
      RETURN
      END    