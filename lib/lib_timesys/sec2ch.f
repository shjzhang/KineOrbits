     
C Procedure                  SEC2CH
C
      CHARACTER*(*) FUNCTION     SEC2CH( SEC )
C
C
C                            Purpose
C
C  This subroutine (SEConds 2 CHaracter string) takes the input seconds
C  the Julian reference date (JDREF) for this library and returns as its
C  functional value the equivalent time in the character*25 format
C  'DD-MMM-YYYY HH:MM:SS.FFFF'.
C
C  If the user wishes to display only some initial segment of the calend
C  date, he may do so with his declaration of SEC2CH. For example, if he
C  wants to display 'DD-MMM-YYYY HH:MM:SS' then he may declare the funct
C  SEC2CH to be character*20.
C
C  Note that this function will round rather than truncate to the neares
C  fractional second. The output accuracy is computed using the length o
C  declared in the calling routine. For example, if the calling routine
C  SEC2CH to be character*24, then this routine will round to the neares
C  millisecond. If the calling routine declares SEC2CH to be character*2
C  this routine will round to the nearest second.
C
C
C                            Input_Arguments
C
C  SEC  is the time in seconds past the reference date for this library.
C
C
C                            Declarations_of_External_Functions
C
      CHARACTER*31       CAL2CH
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION   SEC
C
C
C                            Declarations_of_Local_Variables
C
      INTEGER            YEAR
      INTEGER            MONTH
      INTEGER            DAY
      INTEGER            HOUR
      INTEGER            MINUTE
      INTEGER            SECOND
      DOUBLE PRECISION   FRAC
C
      INTEGER            NDIGIT
      DOUBLE PRECISION   EPS
      DOUBLE PRECISION   SECTMP
C
C  NDIGIT is the number of fractional digits to be output.
C  EPS is the fraction needed to be added to SEC to round to NDIGIT digi
C  SECTMP is the 'rounded' number of seconds.
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Check to see if the date is out of bounds.
      IF( SEC .LT. -1.0D12 )THEN
         SEC2CH = 'DISTANT-PAST'
         RETURN
      ELSEIF( SEC .GT.  1.0D12 )THEN
         SEC2CH = 'DISTANT-FUTURE'
         RETURN
      END IF
C
C1    Round the number of seconds based on the output character string l
      NDIGIT = MIN(  MAX( LEN(SEC2CH)-21, 0 ),  20  )
      EPS    = 0.5D0 / ( 10.D0**NDIGIT )
      SECTMP = SEC + EPS
C 
C1    Call SEC2CAL to convert seconds to calendar date.
      CALL SEC2CAL(SECTMP, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC)
C
C1    Call CAL2CH to convert calendar date to a character string.
      SEC2CH = CAL2CH( YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC )
C
      RETURN
      END 
