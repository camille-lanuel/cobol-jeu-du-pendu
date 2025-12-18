       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEU-DU-PENDU.
       AUTHOR. CAMILLE.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORDS-FILE ASSIGN TO "words.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION. 
       FD  WORDS-FILE.
       01 WORD-READ                PIC A(15).
       WORKING-STORAGE SECTION.
       01 TABLE-WORDS.
          05 WORD                  PIC A(15) OCCURS 100 TIMES.
       01 COUNTER                  PIC 9(3)  VALUE 0.
       01 WS-CURRENT-DATE-FIELDS.
          05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR    PIC  9(4).
             10 WS-CURRENT-MONTH   PIC  9(2).
             10 WS-CURRENT-DAY     PIC  9(2).
          05 WS-CURRENT-TIME.
             10 WS-CURRENT-HOUR    PIC  9(2).
             10 WS-CURRENT-MINUTE  PIC  9(2).
             10 WS-CURRENT-SECOND  PIC  9(2).
             10 WS-CURRENT-MS      PIC  9(2).
          05 WS-DIFF-FROM-GMT      PIC S9(4).
       01 SEED                     PIC 9(4)  VALUE 0.
       01 RDM-ID                   PIC 9(2).
       01 WORD-TO-GUESS            PIC A(15).
       01 HIDDEN-WORD              PIC A(15).
       01 LIVES                    PIC 9     VALUE 6.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM GENERATE-WORD-TO-GUESS.
           DISPLAY "Mot à deviner : " HIDDEN-WORD.
           STOP RUN.
       GENERATE-WORD-TO-GUESS.
           OPEN INPUT WORDS-FILE.
           PERFORM UNTIL COUNTER = 100
                   READ WORDS-FILE INTO WORD(COUNTER + 1)
                   AT END
                      EXIT PERFORM
                   NOT AT END
                       ADD 1 TO COUNTER
                   END-READ
           END-PERFORM.
           CLOSE WORDS-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           COMPUTE SEED = WS-CURRENT-SECOND * 100 + WS-CURRENT-MS.
           COMPUTE RDM-ID = FUNCTION RANDOM(SEED) * COUNTER + 1.
           MOVE WORD(RDM-ID) TO WORD-TO-GUESS.
           MOVE WORD-TO-GUESS TO HIDDEN-WORD.
           INSPECT HIDDEN-WORD
              REPLACING CHARACTERS BY "*" BEFORE " ".
       END PROGRAM JEU-DU-PENDU.