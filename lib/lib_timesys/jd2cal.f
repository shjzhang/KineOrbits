       
C Procedure                  JD2CAL
C
      SUBROUTINE     JD2CAL( JD, YEAR, MONTH,  DAY,                     
     *                           HOUR, MINUTE, SECOND, FRAC )
C
C
C                            Purpose
C
C  This subroutine (Julian Date 2 CALendar date) takes an input Julian d
C  and returns the various components of the corresponding calendar date
C  The components of the calendar date are all returned as numbers to al
C  for use in computation. For instance, the month is returned as the
C  integer month number rather than as a character string.
C
C
C                            Input_Arguments
C
C  JD   is the Julian date.
C
C
C                            Output_Arguments
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
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION   JD
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
      DOUBLE PRECISION   SECDAY
      PARAMETER        ( SECDAY = 86400.0D0 )
C
      DOUBLE PRECISION   JDPLUS
      INTEGER            JDINT
      DOUBLE PRECISION   DSEC
      INTEGER            ISEC
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Compute JDINT, the integer Julian date at noon of the current day
C1    compute DSEC, the number of seconds elapsed since the start of the
C1    current day.
      JDPLUS = JD + 0.5D0
      JDINT  = INT( JDPLUS )
      DSEC   = SECDAY * MOD( JDPLUS, 1.0D0 )
      IF( DSEC .GE. SECDAY )THEN
         JDINT = JDINT + 1
         DSEC  = DSEC  - SECDAY
      END IF
      ISEC = INT( DSEC )
C
C1    Call J2DATE with JDINT to compute the year, month, and day of the
C1    calendar date.
      CALL J2DATE( JDINT, YEAR, MONTH, DAY )
C
C1    Compute HOUR.
      HOUR   = ISEC/3600
      ISEC   = ISEC - 3600*HOUR
C
C1    Compute MINUTE.
      MINUTE = ISEC/60
      ISEC   = ISEC - 60*MINUTE
C
C1    Compute SECOND.
      SECOND = ISEC
C
C1    Compute FRAC.
      FRAC = MOD( DSEC, 1.0D0 )
C
      RETURN
      END 