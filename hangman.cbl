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
       01 FD-WORD-READ                PIC A(15).
       WORKING-STORAGE SECTION.
       01 WS-WORD-GENERATION.
          05 WS-DICTIONARY.
             10 WS-DICT-WORD          PIC A(15) OCCURS 100 TIMES.
          05 WS-DICT-SIZE             PIC 9(3)  VALUE 0.
          05 WS-CURRENT-DATE-FIELDS.
             10 WS-CURRENT-DATE.
                15 WS-CURRENT-YEAR    PIC  9(4).
                15 WS-CURRENT-MONTH   PIC  9(2).
                15 WS-CURRENT-DAY     PIC  9(2).
             10 WS-CURRENT-TIME.
                15 WS-CURRENT-HOUR    PIC  9(2).
                15 WS-CURRENT-MINUTE  PIC  9(2).
                15 WS-CURRENT-SECOND  PIC  9(2).
                15 WS-CURRENT-MS      PIC  9(2).
             10 WS-DIFF-FROM-GMT      PIC S9(4).
          05 WS-SEED                  PIC 9(4)  VALUE 0.
          05 WS-RAND-ID               PIC 9(2).
       01 WS-MAIN.
          05 I                        PIC 9(2)  VALUE 1.
          05 WS-TARGET-WORD           PIC A(15).
          05 WS-MARKED-WORD           PIC A(15).
          05 WS-LIVES                 PIC 9     VALUE 6.
          05 WS-USER-INPUT            PIC A.
          05 COUNT-CHAR               PIC 9(2)  VALUE 0.
       01 WS-GAME-STATUS              PIC A     VALUE 'P'.
          88 PLAYING                            VALUE 'P'.
          88 LOST                               VALUE 'L'.
          88 WON                                VALUE 'W'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM GENERATE-WORD-TO-GUESS.
           PERFORM UNTIL NOT PLAYING 
                   DISPLAY "Mot à deviner : " WS-MARKED-WORD
                   DISPLAY "Entre la lettre à tester :"
                   ACCEPT WS-USER-INPUT
                   PERFORM CHECK-INPUT
                   PERFORM CHECK-STATUS
                   DISPLAY "Tu as " WS-LIVES " vies."
                   DISPLAY "--------------------"
           END-PERFORM.
           STOP RUN.
       GENERATE-WORD-TO-GUESS.
           OPEN INPUT WORDS-FILE.
           PERFORM UNTIL WS-DICT-SIZE = 100
                   READ WORDS-FILE INTO WS-DICT-WORD(WS-DICT-SIZE + 1)
                   AT END
                      EXIT PERFORM
                   NOT AT END
                       ADD 1 TO WS-DICT-SIZE
                   END-READ
           END-PERFORM.
           CLOSE WORDS-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           COMPUTE WS-SEED = WS-CURRENT-SECOND * 100 + WS-CURRENT-MS.
           COMPUTE WS-RAND-ID =
              FUNCTION RANDOM(WS-SEED) * WS-DICT-SIZE + 1.
           MOVE WS-DICT-WORD(WS-RAND-ID) TO WS-MARKED-WORD.
           MOVE WS-MARKED-WORD TO WS-TARGET-WORD.
           INSPECT WS-MARKED-WORD
              REPLACING CHARACTERS BY "*" BEFORE SPACE.
       CHECK-INPUT.
           MOVE FUNCTION UPPER-CASE(WS-USER-INPUT) TO WS-USER-INPUT.
           MOVE 0 TO COUNT-CHAR.
           INSPECT WS-TARGET-WORD TALLYING COUNT-CHAR
              FOR ALL WS-USER-INPUT.
           IF COUNT-CHAR = 0
              DISPLAY WS-USER-INPUT " n'est pas dans le mot."
              SUBTRACT 1 FROM WS-LIVES
           ELSE
              PERFORM REPLACE-CHAR
           END-IF.
       REPLACE-CHAR.
           MOVE 1 TO I.
           PERFORM 15 TIMES
                   IF WS-TARGET-WORD(I:1) = WS-USER-INPUT 
                      MOVE WS-USER-INPUT TO WS-MARKED-WORD(I:1)
                   END-IF
                   ADD 1 TO I
           END-PERFORM.
       CHECK-STATUS.
           MOVE 0 TO COUNT-CHAR.
           INSPECT WS-MARKED-WORD TALLYING COUNT-CHAR FOR ALL "*".
           IF COUNT-CHAR = 0
              SET WON TO TRUE 
              DISPLAY "Bravo ! Tu as deviné le mot " WS-MARKED-WORD
           ELSE
              IF WS-LIVES = 0
                 SET LOST TO TRUE 
              END-IF.
       END PROGRAM JEU-DU-PENDU.
      * TODO : empêcher l'utilisateur de tester une lettre déjà testée