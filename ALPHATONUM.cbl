       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALPHATONUM.

       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.

       DATA DIVISION.
          FILE SECTION.

          WORKING-STORAGE SECTION.
          01 A         PIC X(10).
          01 B         PIC 9(10).
          LOCAL-STORAGE SECTION.

          LINKAGE SECTION.

       PROCEDURE DIVISION.
          DISPLAY 'Executing COBOL program'.
          ACCEPT A FROM CONSOLE.
          COMPUTE B = FUNCTION NUMVAL-c(A).
          display A
          display A.
       STOP RUN.
