# Description

This is a hangman game implemented in COBOL for self-education purposes.

The game and all the words are in French. The dictionary of words that the game can select is in the file `words.txt`.

# What I learned

- Read content from an input file

- Use level 88

- Work with strings

- Use INSPECT

# How to use

1. Install GnuCOBOL if not already installed
   
   With a Debian-based distro:
   
   ```bash
   apt install gnucobol
   ```

2. Compile the game
   
   ```bash
   cobc -x hangman.cbl
   ```

3. Run the game
   
   ```bash
   ./hangman
   ```


