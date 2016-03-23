       
C Procedure           CH2INT
C
      INTEGER FUNCTION     CH2INT( STRING )
C
C
C                     Purpose
C
C  This integer function takes the input character string STRING and ret
C  integer equivalent. The string STRING must contain a legitimate integ
C  Any blanks in the input STRING are ignored.
C
C
C                     Input_Arguments
C
C  STRING   is a character string.
C
C
C                     Declarations_of_Input_and_Output_Arguments
C
      CHARACTER*(*)        STRING
C
C
C                     Declarations_of_Local_Variables
C
      INTEGER              ICHR
      INTEGER              LENGTH
C
      CHARACTER*1          CHR
C
      INTEGER              INT
C
      LOGICAL              MINUS
C
C
C                     Data_Statements
C
C                     Method
C-&
C***********************************************************************
C1  Main Routine
C***********************************************************************
C
      GO TO 30001
C
20002 ASSIGN 20003 TO NPR002
      GO TO 30002
20003 IF (.NOT.( CHR .EQ. '+' )) GO TO 20005
      ASSIGN 20006 TO NPR002
      GO TO 30002
20006 GO TO 20004
20005 IF (.NOT.( CHR .EQ. '-' )) GO TO 20007
         MINUS  = .TRUE.
      ASSIGN 20008 TO NPR002
      GO TO 30002
20008 CONTINUE
20007 CONTINUE
C
20004 IF (.NOT.( CHR .EQ. ' ' )) GO TO 20009
      ASSIGN 20009 TO NPR003
      GO TO 30003
C
20009 CONTINUE
20010 IF (.NOT.( CHR .NE. ' ' )) GO TO 20011
         N = INDEX( '0123456789', CHR ) - 1
      IF (.NOT.( N .LT. 0 )) GO TO 20012
      ASSIGN 20012 TO NPR003
      GO TO 30003
20012    INT = 10*INT + N
      ASSIGN 20013 TO NPR002
      GO TO 30002
20013 GO TO 20010
C
20011 IF(MINUS)THEN
         CH2INT = -INT
      ELSE
         CH2INT =  INT
      END IF
C
      RETURN
C
C***********************************************************************
C1  Procedures
C***********************************************************************
C
C     PROCEDURE (INITIALIZE)
C
30001    LENGTH = LEN( STRING )
         ICHR   = 0
         MINUS  = .FALSE.
         INT    = 0
C
      GO TO 20002
C
C***********************************************************************
C
C     PROCEDURE (GET CHR: THE NEXT NONBLANK CHARACTER IN STRING)
C
30002 CONTINUE
20016       ICHR = ICHR + 1
      IF( ICHR .GT. LENGTH )THEN
               CHR = ' '
      GO TO 20017
      ELSE
               CHR = STRING(ICHR:ICHR)
      IF ( CHR .NE. ' ' ) GO TO 20017
      END IF
      GO TO 20016
C
20017 GO TO NPR002,(20003,20006,20008,20013)
C
C***********************************************************************
C
C     PROCEDURE (WRITE ERROR MESSAGE)
C
30003    WRITE(*,*)  'ERROR - Unable to convert string to integer: ',   
     *                        'String = ''', STRING, ''''
C        CALL HALT
C
      GO TO NPR003,(20009,20012)
C
C***********************************************************************
C
      END    