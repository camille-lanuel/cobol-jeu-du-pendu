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
       01 INPUT-CHAR               PIC A.
       01 COUNT-CHAR               PIC 9(2)  VALUE 0.
       01 I                        PIC 9(2)  VALUE 1.
       01 IS-GUESSED               PIC 1     VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM GENERATE-WORD-TO-GUESS.
           DISPLAY WORD-TO-GUESS.
           PERFORM UNTIL LIVES = 0 OR IS-GUESSED = 1
                   DISPLAY "Mot à deviner : " HIDDEN-WORD
                   DISPLAY "Entre la lettre à tester :"
                   ACCEPT INPUT-CHAR
                   PERFORM CHECK-INPUT
                   PERFORM CHECK-GUESSED
                   DISPLAY "Tu as " LIVES " vies."
                   DISPLAY "--------------------"
           END-PERFORM.
           IF IS-GUESSED = 0
              DISPLAY "Dommage, le mot à deviner était " WORD-TO-GUESS
           END-IF.
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
           MOVE WORD(RDM-ID) TO HIDDEN-WORD.
           MOVE HIDDEN-WORD TO WORD-TO-GUESS.
           INSPECT HIDDEN-WORD
              REPLACING CHARACTERS BY "*" BEFORE SPACE.
       CHECK-INPUT.
           MOVE FUNCTION UPPER-CASE(INPUT-CHAR) TO INPUT-CHAR.
           MOVE 0 TO COUNT-CHAR.
           INSPECT WORD-TO-GUESS TALLYING COUNT-CHAR FOR ALL INPUT-CHAR.
           IF COUNT-CHAR = 0
              DISPLAY INPUT-CHAR " n'est pas dans le mot."
              SUBTRACT 1 FROM LIVES
           ELSE
              PERFORM REPLACE-CHAR
           END-IF.
       REPLACE-CHAR.
           MOVE 1 TO I.
           PERFORM 15 TIMES
                   IF WORD-TO-GUESS(I:1) = INPUT-CHAR 
                      MOVE INPUT-CHAR TO HIDDEN-WORD(I:1)
                   END-IF
                   ADD 1 TO I
           END-PERFORM.
       CHECK-GUESSED.
           MOVE 0 TO COUNT-CHAR.
           INSPECT HIDDEN-WORD TALLYING COUNT-CHAR FOR ALL "*".
           IF COUNT-CHAR = 0
              MOVE 1 TO IS-GUESSED
              DISPLAY "Bravo ! Tu as deviné le mot " HIDDEN-WORD
           END-IF.
       END PROGRAM JEU-DU-PENDU.
      * TODO : empêcher l'utilisateur de tester une lettre déjà testée