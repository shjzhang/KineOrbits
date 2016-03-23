   
C Procedure                  CH2CAL
C
      SUBROUTINE  CH2CAL( STRING, MSG,                                  
     *                    YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRAC, 
     *                    ERROR )
C
C
C                            Purpose
C
C  This subroutine (CHaracter string 2 CALendar date) will parse an inpu
C  character string containing the date and time in the format
C  'DD-MMM-YYYY HH:MM:SS.FFFF' and return the seven components of this d
C  and time converted to numbers.
C
C  The user may input only an initial segment of the date/time string, b
C  the input string must include at least the day, month, and year.
C
C
C                            Input_Arguments
C
C  STRING   is the input date and time in the format
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
C  ERROR    = true if the input string cannot be parsed; otherwise,
C           ERROR=false is returned.
C
C
C                            Declarations_of_External_Functions
C
      INTEGER              CH2INT
      DOUBLE PRECISION     CH2DP
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      CHARACTER*(*)      STRING
      LOGICAL            MSG
      INTEGER            YEAR
      INTEGER            MONTH
      INTEGER            DAY
      INTEGER            HOUR
      INTEGER            MINUTE
      INTEGER            SECOND
      DOUBLE PRECISION   FRAC
      LOGICAL            ERROR
C
C
C                            Declarations_of_Local_Variables
C
      CHARACTER*80       BUFFER
C
      INTEGER            S
      INTEGER            E
      INTEGER            NDIGIT
C
      INTEGER            INT
C
      LOGICAL            FIND
C
      INTEGER            IERROR
      CHARACTER*40       ERRMSG(5)
C
      CHARACTER*3        MONTHS(12)
C
C
      SAVE               MONTHS
      SAVE               ERRMSG
C
C                            Data_Statements
C
      DATA               MONTHS( 1)   /'JAN'/
      DATA               MONTHS( 2)   /'FEB'/
      DATA               MONTHS( 3)   /'MAR'/
      DATA               MONTHS( 4)   /'APR'/
      DATA               MONTHS( 5)   /'MAY'/
      DATA               MONTHS( 6)   /'JUN'/
      DATA               MONTHS( 7)   /'JUL'/
      DATA               MONTHS( 8)   /'AUG'/
      DATA               MONTHS( 9)   /'SEP'/
      DATA               MONTHS(10)   /'OCT'/
      DATA               MONTHS(11)   /'NOV'/
      DATA               MONTHS(12)   /'DEC'/
C
      DATA     ERRMSG(1)  /'Name of month not found'/
      DATA     ERRMSG(2)  /'Year not found'/
      DATA     ERRMSG(3)  /'Too many digits in fractional seconds'/
      DATA     ERRMSG(4)  /'No decimal before fractional seconds'/
      DATA     ERRMSG(5)  /'Too many digits in integer'/
C
C
C                            Method
C-&
C***********************************************************************
C1  Main Routine
C***********************************************************************
C
      ERROR = .FALSE.
C
      CALL CASCHG( STRING, BUFFER )
C
      GO TO 30001
C
20002 GO TO 30002
C
20003 GO TO 30003
C
20004 GO TO 30004
C
20005 GO TO 30005
C
20006 GO TO 30006
C
20007 GO TO 30007
C
20008 RETURN
C
C***********************************************************************
C1  Procedures
C***********************************************************************
C
C     PROCEDURE (GET DAY NUMBER)
C
30001    S = 1
      ASSIGN 20009 TO NPR008
      GO TO 30008
20009    DAY = INT
C
      GO TO 20002
C
C***********************************************************************
C
C     PROCEDURE (GET MONTH NUMBER)
C
30002    S = E + 1
C
      DO 20011 I = 1,12
               N = INDEX( BUFFER(S:), MONTHS(I) )
      IF( N .GT. 0 )THEN
                  MONTH = I
                  E = S + N
      GO TO 20010
      END IF
20011 CONTINUE
            IERROR = 1
      ASSIGN 20016 TO NPR009
      GO TO 30009
C
20016 CONTINUE
20010 GO TO 20003
C
C***********************************************************************
C
C     PROCEDURE (GET YEAR)
C
30003    S = E + 1
      ASSIGN 20017 TO NPR008
      GO TO 30008
20017    YEAR = INT
      IF (.NOT.( YEAR .LE. 0 )) GO TO 20019
            IERROR = 2
      ASSIGN 20020 TO NPR009
      GO TO 30009
C
20020 CONTINUE
20019 GO TO 20004
C
C***********************************************************************
C
C     PROCEDURE (GET HOURS)
C
30004    S = E + 1
      ASSIGN 20021 TO NPR008
      GO TO 30008
20021    HOUR = INT
C
      GO TO 20005
C
C***********************************************************************
C
C     PROCEDURE (GET MINUTES)
C
30005    S = E + 1
      ASSIGN 20022 TO NPR008
      GO TO 30008
20022    MINUTE = INT
C
      GO TO 20006
C
C***********************************************************************
C
C     PROCEDURE (GET INTEGRAL SECONDS)
C
30006    S = E + 1
      ASSIGN 20023 TO NPR008
      GO TO 30008
20023    SECOND = INT
C
      GO TO 20007
C
C***********************************************************************
C
C     PROCEDURE (GET FRACTIONAL SECONDS)
C
30007    S = E + 1
C
      ASSIGN 20024 TO NPR010
      GO TO 30010
C
20024    NDIGIT = E - S + 1
      IF (.NOT.( .NOT. FIND )) GO TO 20026
            FRAC = 0.0
      GO TO 20025
20026 IF (.NOT.( NDIGIT .GT. 10 )) GO TO 20027
            IERROR = 3
      ASSIGN 20028 TO NPR009
      GO TO 30009
20028 GO TO 20025
20027 IF (.NOT.( BUFFER(S-1:S-1) .NE. '.' )) GO TO 20029
            IERROR = 4
      ASSIGN 20030 TO NPR009
      GO TO 30009
20030 GO TO 20025
20029       FRAC = CH2DP( BUFFER(S-1:E) )
C
20025 GO TO 20008
C
C***********************************************************************
C
C     PROCEDURE (GET NEXT INTEGER INT)
C
30008 ASSIGN 20031 TO NPR010
      GO TO 30010
C
20031    NDIGIT = E - S + 1
      IF (.NOT.( .NOT. FIND )) GO TO 20033
            INT = 0
      GO TO 20032
20033 IF (.NOT.( NDIGIT .GT. 10 )) GO TO 20034
            IERROR = 5
      ASSIGN 20035 TO NPR009
      GO TO 30009
20035 GO TO 20032
20034       INT  = CH2INT( BUFFER(S:E) )
C
20032 GO TO NPR008,(20009,20017,20021,20022,20023)
C
C***********************************************************************
C
C     PROCEDURE (FIND START S AND END E OF NEXT DIGIT STRING)
C
30010       N = S
      DO 20037 S = N,LEN(STRING)
      IF ( INDEX( '1234567890', BUFFER(S:S) ) .GT. 0 ) GO TO 20036
20037 CONTINUE
            E = S
            FIND = .FALSE.
      GO TO 31010
C
20036 DO 20040 E = S+1,LEN(STRING)
      IF ( INDEX( '1234567890', BUFFER(E:E) ) .LE. 0 ) GO TO 20041
20040 CONTINUE
20041    E = E - 1
         FIND = .TRUE.
C
31010 GO TO NPR010,(20024,20031)
C
C***********************************************************************
C
C     PROCEDURE (PROCESS INPUT ERROR)
C
30009 IF(MSG)THEN
            WRITE(*,110)   STRING,   ERRMSG( IERROR )
110        FORMAT(' ','Error - ''',A,'''',                             
     *            /' ','         is not a legitimate date/time string', 
     *            /' ','         ',A)
C            CALL HALT
      ELSE
            ERROR = .TRUE.
            RETURN
      END IF
C
      GO TO NPR009,(20016,20020,20028,20030,20035)
C
C***********************************************************************
C
      END   