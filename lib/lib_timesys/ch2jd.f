    
C Procedure           CH2JD
C
      DOUBLE PRECISION FUNCTION     CH2JD( STRING, MSG, ERROR )
C
C
C                            Purpose
C
C  This double precision function (CHaracter string 2 Julian Date) will
C  an input character string containing the date and time in the format
C  'DD-MMM-YYYY HH:MM:SS.FFFF' and return as its functional value the
C  corresponding Julian date.
C
C  The user may input only an initial segment of the date/time string, b
C  the input string must include at least the day, month, and year.
C
C
C                            Input_Arguments
C
C  STRING   is the input data and time in the format
C           'DD-MMM-YYYY HH:MM:SS.FFFF'. The fractional seconds, seconds
C           minutes, and/or hours may be omitted if their intended value
C           zero.
C  MSG      controls the response to an input error.
C           If MSG=true and the input STRING contains a string which can
C              be parsed, then an error message is written to the standa
C              system output file and the program is terminated with a
C              walkback.
C           If MSG=false and the input STRING contains a string which ca
C              be parsed, no message is written but ERROR=true is return
C
C
C                            Output_Arguments
C
C  ERROR    = true if and only if MSG=false and the input string cannot
C           parsed; otherwise ERROR=false is returned.
C
C
C                            Declarations_of_External_Functions
C
      DOUBLE PRECISION   CAL2JD
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      CHARACTER*(*)      STRING
      LOGICAL            MSG
      LOGICAL            ERROR
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
C
C                     Method
C-&
C***********************************************************************
C
C1    Call CH2CAL to convert the character string to a calendar date.
      CALL CH2CAL( STRING, MSG,                                         
     *             YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC,        
     *             ERROR )
      IF (ERROR)  THEN
         ch2jd = 0
	  RETURN
      end IF
C
C1    Call CAL2JD to convert the calendar date to the Julian date.
      CH2JD = CAL2JD( YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC )
C
      RETURN
      END   
