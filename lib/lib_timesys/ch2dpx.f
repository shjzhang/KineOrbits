C Procedure           CH2DPX
C
      SUBROUTINE      CH2DPX( CH, MSG, DP, ISTAT )
C
C
C                     Purpose
C
C  This subroutine (CHaracter 2 Double Precision X) takes the input char
C  string CH and returns the double precision numeric equivalent DP. The
C  CH must contain a legitimate number, either floating point or integer
C  Leading and trailing blanks are ignored but any other character in CH
C  is not a digit or an exponent delimiter ('D' or 'E') will cause a fat
C  error. In particular, embedded blanks are not allowed.
C
C
C                            Input_Arguments
C
C  CH      is the character string to be converted to a double precision
C  MSG     is a logical flag which controls the response to an error con
C          If MSG=.TRUE. and an error is encountered, then an error mess
C             written to the standard system output file and the program
C             terminated with a walkback.
C          If MSG=.FALSE. and an error is encountered, then no error mes
C             written but ISTAT<0 is returned.
C
C
C                            Output_Arguments
C
C  DP      is the double precision equivalent of the number in the chara
C          string CH.
C  ISTAT   is the error status flag:
C             ISTAT=0  if the request was successful. The other output
C                      arguments are defined.
C             ISTAT>0  if the request was successful but some warning co
C                      prevails that the user may wish to be aware of. T
C                      other output arguments are defined.
C             ISTAT<0  if the request was unsuccessful. The other output
C                      arguments are undefined and should not be used.
C
C
C                            Declarations_of_External_Functions
C
      DOUBLE PRECISION     D1MACH
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION     DP
      LOGICAL              MSG
      CHARACTER*(*)        CH
      INTEGER              ISTAT
C
C
C                     Declarations_of_Local_Variables
C
      PARAMETER          ( MAXLEN = 30 )
C
      INTEGER              N1
      INTEGER              N2
      INTEGER              LENGTH
C 
      CHARACTER*(MAXLEN)   CTEMP1
      CHARACTER*(MAXLEN)   CTEMP2
C
      INTEGER              MSGNUM
C
C
      SAVE
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
C2  Trim leading and trailing blanks.
      DO 20002 N1 = 1,LEN(CH)
      IF ( CH(N1:N1) .NE. ' ' ) GO TO 20003
20002 CONTINUE
20003 DO 20005 N2 = LEN(CH),1,-1
      IF ( CH(N2:N2) .NE. ' ' ) GO TO 20006
20005 CONTINUE
C
C2  Verify that the trimmed string is not empty.
20006 IF (.NOT.( N1 .GT. N2 )) GO TO 20009
         MSGNUM = 1
      ASSIGN 20010 TO NPR001
      GO TO 30001
C
20010 CONTINUE
20009 LENGTH = N2 - N1 + 1
      IF (.NOT.( LENGTH .GT. MAXLEN )) GO TO 20012
         MSGNUM = 2
      ASSIGN 20013 TO NPR001
      GO TO 30001
C
C2  If the input number is an integer, add a decimal point.
20013 CONTINUE
20012 CTEMP1 = CH( N1:N2 )
      IF( INDEX( CTEMP1, '.' ) .EQ. 0 )THEN
         CTEMP1  = CTEMP1(:LENGTH)  //  '.'
         LENGTH = LENGTH + 1
      END IF
C
C2  Convert the input string to a double precision using a FORTRAN inter
C2  file.
      CTEMP2                     = ' '
      CTEMP2( MAXLEN-LENGTH+1: ) = CTEMP1
      READ ( UNIT=CTEMP2, FMT='(D30.22)', IOSTAT=IOS )    DP
      IF (.NOT.( IOS .GT. 0 )) GO TO 20017
         MSGNUM = 3
      ASSIGN 20018 TO NPR001
      GO TO 30001
C
20018 CONTINUE
20017 RETURN
C
C***********************************************************************
C1  Procedures
C***********************************************************************
C
C     PROCEDURE (WRITE ERROR MESSAGE NUMBER MSGNUM)
C
30001    IF ( .NOT. MSG)  RETURN
C
         WRITE(*,*) 'ERROR - Unable to convert string to ',             
     *                      'double precision number'
         WRITE(*,*)        '        Input string = ', CH
      GO TO (20021,20022,20023),  MSGNUM
      GO TO 20020
20021          WRITE(*,*)  '        String is empty'
      GO TO 20020
20022          WRITE(*,*)  '        String is too long'
      GO TO 20020
20023          READ ( UNIT=CTEMP2, FMT='(D30.22)' )    DP
C
C20020    CALL HALT
20020    CONTINUE
C
      GO TO NPR001,(20010,20013,20018)
C
C***********************************************************************
C
      END 