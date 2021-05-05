      ******************************************************************
      * Author: Lawrence Scroggs
      * Date: 04/09/21
      * Purpose: The purpose of this program is to calculate the GPA
      * for the passing grade of entered student name.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GPA-CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  USER-NAME       PIC A(20).
       77  INPUT-READ      PIC A(10).
       77  GRADE-HOLD      PIC 9(3).
       77  COUNT-HOLD      PIC 9(2).
       77  USER-GPA        PIC 9(3).
       77  GPA-DISPLAY     PIC 9V99.
       77  DATA-READ       PIC A(10).
       01  A-DATA.
           03 USER-READ    PIC A(6).
           88 INSTRUCT-1 VALUE "CALC".
           88 INSTRUCT-2 VALUE "STOP".
           03 GRADE-READ   PIC A(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 030-PROGRAM-RUN.
           STOP RUN.

       010-INPUT-NAME.
           DISPLAY "Enter Student Name: " ACCEPT USER-NAME.
       020-INPUT-DATA.
           DISPLAY "Enter Class and Grade".
           DISPLAY "(Enter CALC for GPA or STOP to quit): ".
           ACCEPT A-DATA.
           IF USER-READ EQUAL NOT INSTRUCT-1 OR NOT INSTRUCT-2 THEN
               PERFORM 040-ADD-GRADE
           END-IF.

       030-PROGRAM-RUN.
           PERFORM 010-INPUT-NAME.
           PERFORM 020-INPUT-DATA UNTIL USER-READ EQUAL "CALC" OR "STOP"
           IF USER-READ EQUAL "CALC" THEN
               PERFORM 050-CALCULATE-GPA
               PERFORM 030-PROGRAM-RUN
           ELSE
               DISPLAY "Ending Program...."
           END-IF.


       040-ADD-GRADE.
           EVALUATE GRADE-READ
               WHEN 'A'  ADD 400 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'A-' ADD 367 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'B+' ADD 333 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'B'  ADD 300 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'B-' ADD 267 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'C+' ADD 233 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'C'  ADD 200 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN 'C-' ADD 167 TO GRADE-HOLD ADD 1 TO COUNT-HOLD
               WHEN OTHER ADD  0 TO GRADE-HOLD
           END-EVALUATE.

       050-CALCULATE-GPA.
           DIVIDE GRADE-HOLD BY COUNT-HOLD GIVING USER-GPA.
           MULTIPLY USER-GPA BY .01 GIVING GPA-DISPLAY.

           DISPLAY USER-NAME "  GPA: " GPA-DISPLAY.
           MOVE "NA" TO A-DATA.
           MOVE 0 TO GRADE-HOLD.
           MOVE 0 TO USER-GPA.
           MOVE 0 TO COUNT-HOLD.

       END PROGRAM GPA-CALCULATOR.
