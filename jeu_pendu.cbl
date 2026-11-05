      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEU-DU-PENDU.
       AUTHOR. CAMILLE.
      
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LISTE-MOTS ASSIGN TO "words.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION. 
       FD  LISTE-MOTS.
       01 F-MOT-LU                 PIC A(15).
      
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------- 
       
       77 C-NBR-MAX-MOTS           PIC 9(3)  VALUE 100.
       77 C-NBR-MAX-LETTRES        PIC 9(2)  VALUE 15.
       01 W-IDC-MOT                PIC 9(3)  VALUE 1.
       01 W-TABLE-MOTS.
          05 W-MOT                 PIC A(15) OCCURS 100 TIMES.
       
       01 W-TIMESTAMP.
          05 FILLER                PIC 9(8).
          05 W-TEMPS.
             10 FILLER             PIC 9(4).
             10 W-SECONDE          PIC 9(2).
             10 W-MILLISECONDE     PIC 9(2).
          05 FILLER                PIC S9(4).
       
       01 W-GRAINE                 PIC 9(4)  VALUE 0.
       01 W-IDX-MOT-CHOISI         PIC 9(2).
       
       01 W-IDX-LETTRE             PIC 9(2)  VALUE 1.
       77 C-ALPHABET               PIC A(26) VALUE
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01 W-MOT-CIBLE              PIC A(15).
       01 W-MOT-MASQUE             PIC A(15).
       01 W-NBR-VIES               PIC 9     VALUE 6.
       01 W-LETTRE-ENTREE          PIC A.
       01 W-LETTRES-TESTEES        PIC A(26) VALUE SPACES.
       01 W-COUNT                  PIC 9(2)  VALUE 0.
       
       01 W-STATUT-ENTREE          PIC A     VALUE 'V'.
          88 W-ENTREE-VALIDE                 VALUE 'V'.
          88 W-ENTREE-DEJA-TESTEE            VALUE 'T'.
          88 W-ENTREE-INVALIDE               VALUE 'L'.
       
       01 W-STATUT-PARTIE          PIC A     VALUE 'P'.
          88 W-PARTIE-EN-COURS               VALUE 'P'.
          88 W-PARTIE-PERDUE                 VALUE 'L'.
          88 W-PARTIE-GAGNEE                 VALUE 'W'.
       
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       
      *----------------------------------------------------------------- 
       MAIN-PROCEDURE.  

           PERFORM GENERER-MOT-CIBLE
           
           PERFORM UNTIL NOT W-PARTIE-EN-COURS 
                   DISPLAY "--------------------"
                   DISPLAY "Mot à deviner : " W-MOT-MASQUE
                   DISPLAY "Tu as " W-NBR-VIES " vies."
                   DISPLAY "Lettres déjà testées : "
                           W-LETTRES-TESTEES
                   PERFORM RECUPERER-ENTREE
                   PERFORM TRAITER-ENTREE
                   PERFORM MAJ-STATUT-PARTIE
           END-PERFORM
           
           STOP RUN
           .
       
      *----------------------------------------------------------------- 
       GENERER-MOT-CIBLE.

           OPEN INPUT LISTE-MOTS
           
           PERFORM VARYING W-IDC-MOT FROM 1 BY 1
              UNTIL W-IDC-MOT > C-NBR-MAX-MOTS 
                   READ LISTE-MOTS INTO W-MOT(W-IDC-MOT)
                   AT END
                      EXIT PERFORM
                   END-READ

           END-PERFORM
           
           CLOSE LISTE-MOTS

      *    Calcul graine aléatiore à partir du timestamp
           MOVE FUNCTION CURRENT-DATE TO W-TIMESTAMP
           COMPUTE W-GRAINE = W-SECONDE * 100 + W-MILLISECONDE
      
      *    Calcul indice du mot à deviner dans la table
           COMPUTE W-IDX-MOT-CHOISI =
              FUNCTION RANDOM(W-GRAINE) *(W-IDC-MOT - 1) + 1
           
      *    Préparation du mot à deviner et du mot masqué
           MOVE W-MOT(W-IDX-MOT-CHOISI) TO W-MOT-MASQUE
           MOVE W-MOT-MASQUE TO W-MOT-CIBLE
           INSPECT W-MOT-MASQUE
              REPLACING CHARACTERS BY "*" BEFORE SPACE
           .
      
      *----------------------------------------------------------------- 
       RECUPERER-ENTREE.
       
           DISPLAY "Entre la lettre à tester (a-z) :"
           ACCEPT W-LETTRE-ENTREE
           
           PERFORM MAJ-STATUT-ENTREE
           
           PERFORM UNTIL W-ENTREE-VALIDE
                   IF W-ENTREE-INVALIDE 
                      DISPLAY "ERREUR : "
                              W-LETTRE-ENTREE
                              " n'est pas dans l'alphabet (a-z)."
                   END-IF
                   
                   IF W-ENTREE-DEJA-TESTEE 
                      DISPLAY "ERREUR : "
                              W-LETTRE-ENTREE
                              " a déjà été testée."
                   END-IF 
                   
                   MOVE SPACE TO W-LETTRE-ENTREE
                   DISPLAY "Recommence :"
                   ACCEPT W-LETTRE-ENTREE
                   
                   PERFORM MAJ-STATUT-ENTREE
           END-PERFORM
           .

      *----------------------------------------------------------------- 
       MAJ-STATUT-ENTREE.
       
           SET W-ENTREE-VALIDE TO TRUE
           MOVE FUNCTION UPPER-CASE(W-LETTRE-ENTREE) TO W-LETTRE-ENTREE
           
      *    Vérifie si c'est une lettre de l'alphabet
           MOVE 0 TO W-COUNT
           INSPECT C-ALPHABET TALLYING W-COUNT FOR ALL W-LETTRE-ENTREE
           IF W-COUNT = 0
              SET W-ENTREE-INVALIDE TO TRUE
           END-IF
           
      *    Vérifie si la lettre a déjà été testée
           MOVE 0 TO W-COUNT
           INSPECT W-LETTRES-TESTEES TALLYING W-COUNT
              FOR ALL W-LETTRE-ENTREE BEFORE SPACE
           
           IF W-COUNT > 0
              SET W-ENTREE-DEJA-TESTEE TO TRUE
           END-IF
           .     
       
      *----------------------------------------------------------------- 
       TRAITER-ENTREE.
       
      *    Ajout de la lettre à l'ensemble des lettres déjà testées
           INSPECT W-LETTRES-TESTEES
              REPLACING FIRST SPACE BY W-LETTRE-ENTREE
           
      *    Vérifie si la lettre est dans le mot à deviner
           MOVE 0 TO W-COUNT
           INSPECT W-MOT-CIBLE TALLYING W-COUNT
              FOR ALL W-LETTRE-ENTREE
           
           IF W-COUNT = 0
      *       On retire une vie si la lettre n'y est pas
              DISPLAY W-LETTRE-ENTREE " n'est pas dans le mot."
              SUBTRACT 1 FROM W-NBR-VIES
           ELSE
      *       On découvre les lettres conrrespondantes si elle y est
              PERFORM DECOUVRIR-LETTRES
           END-IF
           .
           
      *----------------------------------------------------------------- 
       DECOUVRIR-LETTRES.
       
           PERFORM VARYING W-IDX-LETTRE FROM 1 BY 1
              UNTIL W-IDX-LETTRE > C-NBR-MAX-LETTRES 
                   IF W-MOT-CIBLE(W-IDX-LETTRE:1) = W-LETTRE-ENTREE 
                      MOVE W-LETTRE-ENTREE
                         TO W-MOT-MASQUE(W-IDX-LETTRE:1)
                   END-IF
           END-PERFORM
           .
       
      *----------------------------------------------------------------- 
       MAJ-STATUT-PARTIE.
       
           MOVE 0 TO W-COUNT
           INSPECT W-MOT-MASQUE TALLYING W-COUNT FOR ALL "*"
           
      *    Si plus aucune lettre masquée, la partie est gagnée
           IF W-COUNT = 0
              SET W-PARTIE-GAGNEE TO TRUE
              DISPLAY "--------------------"
              DISPLAY "Bravo ! Tu as deviné le mot " W-MOT-MASQUE
           END-IF
           
      *    Si plus aucune vie, la partie est perdue
           IF W-NBR-VIES = 0
              SET W-PARTIE-PERDUE TO TRUE
              DISPLAY "--------------------"
              DISPLAY "Dommage... Le mot était " W-MOT-CIBLE 
           END-IF
           .
       
      *----------------------------------------------------------------- 
       END PROGRAM JEU-DU-PENDU.
