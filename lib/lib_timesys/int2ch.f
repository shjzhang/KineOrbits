     
C Procedure           INT2CH
C
      CHARACTER*(*)   FUNCTION     INT2CH( INT )
C
C
C                     Purpose
C
C  This character*(*) function (INTeger 2 CHaracter) takes the input int
C  INT and returns the equivalent character string representation left
C  justified and padded with blanks on the the right. For example, if IN
C  is declared as CHARACTER*5 in the calling routine then INT2CH(23) wil
C  return the five character string '23   '.
C
C
C                     Input_Arguments
C
C  INT    is an integer.
C
C
C                     Declarations_of_Input_and_Output_Arguments
C
      INTEGER              INT
C
C
C                     Declarations_of_Local_Variables
C
      PARAMETER          ( MAXLEN = 15 )
C
      LOGICAL              MINUS
      INTEGER              ITEMP
      CHARACTER*(MAXLEN)   STRING
C
      CHARACTER*1          DIGIT(0:9)
C
C
C                     Data_Statements
C
      DATA                 DIGIT(0)     / '0' /
      DATA                 DIGIT(1)     / '1' /
      DATA                 DIGIT(2)     / '2' /
      DATA                 DIGIT(3)     / '3' /
      DATA                 DIGIT(4)     / '4' /
      DATA                 DIGIT(5)     / '5' /
      DATA                 DIGIT(6)     / '6' /
      DATA                 DIGIT(7)     / '7' /
      DATA                 DIGIT(8)     / '8' /
      DATA                 DIGIT(9)     / '9' /
C
C
C                     Method
C-&
C***********************************************************************
C
      IF( INT .LT. 0 )THEN
         MINUS = .TRUE.
      ELSE
         MINUS = .FALSE.
      END IF
C
      ITEMP  = ABS( INT )
      ICHR   = MAXLEN + 1
      STRING = ' '
C
      GO TO 20006
20004 IF ( ITEMP .EQ. 0 ) GO TO 20005
20006    JTEMP  = ITEMP/10
         N      = ITEMP - 10*JTEMP
         ICHR   = ICHR - 1
         STRING(ICHR:ICHR) = DIGIT(N)
         ITEMP  = JTEMP
      GO TO 20004
C
20005 IF(MINUS)THEN
         ICHR   = ICHR - 1
         STRING(ICHR:ICHR) = '-'
      END IF
C
      INT2CH  = STRING( ICHR:MAXLEN )
C
      RETURN
      END   