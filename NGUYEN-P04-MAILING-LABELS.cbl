      ******************************************************************
      *PROGRAM : PROJECT 4   Mailing Labels                            *
      *AUTHOR  : David Nguyen                                          *
      *DATE    : 03/09/2022                                            *
      *ABSTRACT: Use of UNSTRING and STRING                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NGUYEN-P04-MAILING-LABELS.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-LIST  ASSIGN TO 'p04-cust-list.csv'
                             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LABEL-FILE ASSIGN TO 'p04-labels.txt'
                             ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-LIST.
       01  CUST-REC                    PIC X(91).
       FD  LABEL-FILE
           LABEL RECORD ARE OMITTED.                  
       01  LABEL-REC                   PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03  I-FNAME                 PIC X(15).
           03  I-LNAME                 PIC X(15).
           03  I-ADDRESS               PIC X(25).
           03  I-CITY                  PIC X(20).
           03  I-STATE                 PIC X(2).
           03  I-ZIP                   PIC 9(9).
           03  I-ZIP-SPLIT REDEFINES I-ZIP.
               05  I-ZIP-SPLIT-5       PIC 9(5).
               05  I-ZIP-SPLIT-4       PIC 9(4).
       01  OUTPUT-DATA.
           03  O-NAME-LINE             PIC X(31).
           03  O-ADDRESS-LINE          PIC X(25).
           03  O-LOCATION-LINE         PIC X(36).
       01  MISC.
           03  WS-CTR                  PIC 9(6)    VALUE ZERO.
           03  WS-FLAGS.
               05  WS-EOF-FLAG         PIC X       VALUE 'N'.
                   88  EOF                         VALUE 'Y'.
           03  WS-UPPERCASE PIC X(26) VALUE 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           03  WS-LOWERCASE PIC X(26) VALUE 
               'abcdefghijklmnopqrstuvwxyz'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT  CUST-LIST.
           OPEN OUTPUT LABEL-FILE.
           PERFORM UNTIL EOF
               READ CUST-LIST
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-CTR
                       PERFORM 200-UNSTRING-RECORD
                       PERFORM 300-STRING-RECORD
                       PERFORM 400-PRINT-RECORD
           END-PERFORM.
           DISPLAY SPACES.
           DISPLAY 'LABELS PRINTED: ', WS-CTR.
           CLOSE CUST-LIST
                 LABEL-FILE.
           STOP RUN.
      *-----------------------------------------------------------------
       200-UNSTRING-RECORD.
           INSPECT CUST-REC
               CONVERTING WS-LOWERCASE TO WS-UPPERCASE.
           UNSTRING CUST-REC DELIMITED BY ',' OR '*'
                   INTO I-LNAME, I-FNAME, I-ADDRESS, 
                       I-CITY, I-STATE, I-ZIP
           END-UNSTRING.
      *-----------------------------------------------------------------
       300-STRING-RECORD.
           STRING
               I-FNAME DELIMITED BY SPACE
               ' ' DELIMITED BY SIZE
               I-LNAME DELIMITED BY SPACE
               INTO O-NAME-LINE
           END-STRING.
           STRING
               I-ADDRESS DELIMITED BY SIZE
               INTO O-ADDRESS-LINE
           END-STRING.
           STRING
               I-CITY DELIMITED BY SPACE
               ', ' DELIMITED BY SIZE
               I-STATE DELIMITED BY SPACE
               '  ' DELIMITED BY SIZE
               I-ZIP-SPLIT-5 DELIMITED BY SPACE
               '-' DELIMITED BY SIZE
               I-ZIP-SPLIT-4 DELIMITED BY SPACE
               INTO O-LOCATION-LINE
           END-STRING.
      *-----------------------------------------------------------------
       400-PRINT-RECORD.
           WRITE LABEL-REC FROM O-NAME-LINE.
           WRITE LABEL-REC FROM O-ADDRESS-LINE.
           WRITE LABEL-REC FROM O-LOCATION-LINE.
           WRITE LABEL-REC FROM SPACES.
           WRITE LABEL-REC FROM SPACES.
           MOVE SPACES TO O-NAME-LINE.
           MOVE SPACES TO O-ADDRESS-LINE.
           MOVE SPACES TO O-LOCATION-LINE.
      *----------------------------------------------------------------- 
       END PROGRAM NGUYEN-P04-MAILING-LABELS.
