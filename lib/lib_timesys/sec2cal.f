     
C Procedure                  SEC2CAL
C
      SUBROUTINE     SEC2CAL( SEC, YEAR, MONTH,  DAY,                   
     *                             HOUR, MINUTE, SECOND, FRAC )
C
C
C                            Purpose
C
C  This subroutine (SEConds to CALendar date) takes an input seconds pas
C  the Julian reference date (JDREF) for this library and returns the va
C  components of the corresponding calendar date. The components of the
C  calendar date are all returned as numbers to allow for use in computa
C  For instance, the month is returned as the integer month number rathe
C  as a character string.
C
C
C                            Input_Arguments
C
C  SEC     is the seconds past the reference date for this library.
C
C
C                            Output_Arguments
C
C  YEAR    is the year.
C  MONTH   is the month number.
C  DAY     is the day.
C  HOUR    is the hour.
C  MINUTE  is the minute.
C  SECOND  is the second.
C  FRAC    is the fractional seconds.
C
C
C                            Declarations_of_External_Functions
C
      DOUBLE PRECISION   SEC2JD
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION   SEC
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
      DOUBLE PRECISION   TEMP
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Extract the fraction seconds (FRAC) from SEC. Note that care must
C1    taken if SEC<0 since FRAC must be a non-negative number < 1. The o
C1    fraction seconds of the calendar representation is simply the frac
C1    part of the input seconds and can be computed immediately. The rem
C1    integral seconds is converted to the rest of the calendar date. Th
C1    done to avoid round off error that could be introduced by the
C1    intermediate conversion to Julian date.
      FRAC = MOD( SEC, 1.0D0 )
      IF ( FRAC .LT. 0.0D0 )   FRAC = 1.0D0 + FRAC
C
C1    Call SEC2JD to convert the integral seconds to the Julian date.
      JD = SEC2JD( SEC - FRAC + 0.5D0 )
C
C1    Call JD2CAL to convert the Julian date to calandar date.
      CALL JD2CAL( JD, YEAR, MONTH,  DAY,                               
     *                 HOUR, MINUTE, SECOND, TEMP )
C
      RETURN
      END        
