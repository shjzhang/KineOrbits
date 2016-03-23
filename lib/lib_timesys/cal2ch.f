C*******************************************************************************
C
C       Copyright (C) 1993, California Institute of Technology.  U.S.
C       Government Sponsorhip under NASA Contract NAS7-918 is
C       acknowledged.
C
C*******************************************************************************
C Procedure                  CAL2CH
C
      CHARACTER*(*) FUNCTION     CAL2CH( YEAR, MONTH,  DAY,             
     *                                   HOUR, MINUTE, SECOND, FRAC )
C
C
C                            Purpose
C
C  This subroutine (CALendar date 2 CHaracter string) takes the various
C  components of an input calendar date and returns as its functional va
C  the equivalent time in the character*25 format 'DD-MMM-YYYY HH:MM:SS.
C
C  If the user wishes to display only some initial segment of the calend
C  date, he may do so with his declaration of CAL2CH. For example, if he
C  wants to display 'DD-MMM-YYYY HH:MM:SS' then he may declare the funct
C  CAL2CH to be character*20.
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
C                            Declarations_of_External_Functions
C
      CHARACTER*5          INT2CH
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      INTEGER              YEAR
      INTEGER              MONTH
      INTEGER              DAY
      INTEGER              HOUR
      INTEGER              MINUTE
      INTEGER              SECOND
      DOUBLE PRECISION     FRAC
C
C
C                            Declarations_of_Local_Variables
C
      CHARACTER*3          MONTHS(12)
C
      CHARACTER*3          STR
      CHARACTER*31         NEWTIM
      DOUBLE PRECISION     TEMP
      INTEGER              NDIGIT
      INTEGER              IDIGIT
C
C
      SAVE                 MONTHS
C
C                            Data_Statements
C
      DATA                 MONTHS( 1)   /'JAN'/
      DATA                 MONTHS( 2)   /'FEB'/
      DATA                 MONTHS( 3)   /'MAR'/
      DATA                 MONTHS( 4)   /'APR'/
      DATA                 MONTHS( 5)   /'MAY'/
      DATA                 MONTHS( 6)   /'JUN'/
      DATA                 MONTHS( 7)   /'JUL'/
      DATA                 MONTHS( 8)   /'AUG'/
      DATA                 MONTHS( 9)   /'SEP'/
      DATA                 MONTHS(10)   /'OCT'/
      DATA                 MONTHS(11)   /'NOV'/
      DATA                 MONTHS(12)   /'DEC'/
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Check to see if the input calendar date is out of bounds.
      IF( YEAR .LT.    0 )THEN
         CAL2CH = 'DISTANT-PAST'
         RETURN
      ELSEIF( YEAR .GT. 9999 )THEN
         CAL2CH = 'DISTANT-FUTURE'
         RETURN
      END IF
C
C------------------------------
C
C1    Convert the time from integers to characters.
C
C2    Fractional seconds
      NDIGIT = MIN( LEN(CAL2CH), LEN(NEWTIM) ) - 21
      TEMP   = FRAC
      DO 20005 I = 1,NDIGIT
         TEMP   = 10.0D0 * MOD( TEMP, 1.0D0 )
         IDIGIT = INT( TEMP )
         NEWTIM(21+I:21+I) = INT2CH( IDIGIT )
20005 CONTINUE
      NEWTIM(21:21) = '.'
C
C2    Second
      NEWTIM(18:20) = INT2CH( SECOND + 100 )
      NEWTIM(18:18) = ':'
C
C2    Minute
      NEWTIM(15:17) = INT2CH( MINUTE + 100 )
      NEWTIM(15:15) = ':'
C 
C2    Hour
      NEWTIM(12:14) = INT2CH( HOUR + 100 )
      NEWTIM(12:12) = ' '
C
C2    Year
      NEWTIM(8:11)  = INT2CH( YEAR )
      NEWTIM(7:7)   = '-'
C
C2    Month
      NEWTIM(4:6)   = MONTHS( MONTH )
      NEWTIM(3:3)   = '-'
C
C2    Day
      STR           = INT2CH( DAY + 100 )
      IF ( STR(2:2) .EQ. '0' )  STR(2:2) = ' '
      NEWTIM(1:2)   = STR(2:3)
C
C
C1    Return the converted time string.
      CAL2CH = NEWTIM
C
      RETURN
      END     