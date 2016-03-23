     
C Procedure           CASCHG
C
      SUBROUTINE      CASCHG( INSTR, OUTSTR )
C
C
C                            Purpose
C
C  This subroutine (CASe CHanGe) will take an input character string and
C  all occurances of lowercase letters with the corresponding uppercase
C
C
C                            Input_Arguments
C
C  INSTR    is the input character string.
C
C
C                            Output_Arguments
C
C  OUTSTR   is the output character string. This string may the same as
C           input string.
C
C
C                            Declarations_of_External_Functions
C
C                            Declarations_of_Input_and_Output_Arguments
C
      CHARACTER*(*)      INSTR
      CHARACTER*(*)      OUTSTR
C
C
C                            Declarations_of_Local_Variables
C
      CHARACTER*26       LOWER
      CHARACTER*26       UPPER
C
C
C                            Data_Statements
C
      DATA   LOWER     /'abcdefghijklmnopqrstuvwxyz'/
      DATA   UPPER     /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C
C                     Method
C-&
C***********************************************************************
C
      OUTSTR = INSTR
C
      DO 20002 I = 1,MIN( LEN(INSTR), LEN(OUTSTR) )
         N = INDEX( LOWER, OUTSTR(I:I) )
         IF ( N .GT. 0 )   OUTSTR(I:I) = UPPER(N:N)
20002 CONTINUE
C
      RETURN
      END     