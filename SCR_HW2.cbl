      ******************************************************************
      * Author: Lawrence Scroggs
      * Date: 04/17/21
      * Purpose: Scan through the givin text file and grab the info on
      * teacher, course # and credit hours.  Then display the amount of
      * revenue generated.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES ASSIGN TO "C:\Users\lawre\Courses.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT C-SORT ASSIGN TO "C:\Users\lawre\Courses-sorted.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-FILE ASSIGN TO "WORK.tmp".
       DATA DIVISION.
       FILE SECTION.
           FD COURSES.
           01 UNSORTED-RECORD          PIC X(206).
           FD C-SORT.
           01 CLASS-RECORD.
               03  FILLER              PIC X(6).
               03  COURSE-PREFIX       PIC 9.
               03  FILLER              PIC X(98).
               03  EMAIL               PIC X(16).
               03  FILLER              PIC X(16).
               03  SCH                 PIC X(4).
               03  FILLER              PIC X(50).
  .        SD SORTED-FILE.
           01 SORT-RECORD.
               03  FILLER              PIC X(105).
               03  E-KEY               PIC X(16).
               03  FILLER              PIC X(85).
       WORKING-STORAGE SECTION.
           77  TEACH-EMAIL             PIC X(16).
           77  COURSE-TUITION          PIC 9(5)V99 VALUE IS ZERO.
           77  TUITION-TOTAL           PIC 9(9)V99 VALUE IS ZERO.
           77  SCH-NUM                 PIC 9(4).
           77  SCH-TOTAL               PIC 9(6)    VALUE IS ZERO.
           77  SCH-OVERALL             PIC 9(8)    VALUE IS ZERO.
           77  COST-OVERALL            PIC 9(9)V99 VALUE IS ZERO.
           77  LAST-E                  PIC X(16).
           77  FILE-STATUS             PIC 99      VALUE IS 0.
                88 E-O-F                           VALUE IS 1.
           77  CNTR                    PIC 99      VALUE IS 0.
           77  COST-FORMAT             PIC $$$,$$$,$$9.99.
           01  OUTPUT-LINE.
               03  TEACHER             PIC X(16).
               03  FILLER              PIC X(7).
               03  SCH_AMOUNT          PIC 9(6).
               03  FILLER              PIC X(5).
               03  MONEYS              PIC $$$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       010-MAIN.
           PERFORM 020-INITIALIZE.
           PERFORM 025-DISPLAY-FIRST.
           PERFORM 030-PROCESS-FILE.
           PERFORM 041-CLOSE-STUFF.
           PERFORM 026-DISPLAY-LAST.

           STOP RUN.

       020-INITIALIZE.
           SORT SORTED-FILE ON ASCENDING KEY E-KEY
               USING COURSES
               GIVING C-SORT.
           OPEN INPUT C-SORT.
       025-DISPLAY-FIRST.
           DISPLAY '=================================================='.
           DISPLAY ' '.
           DISPLAY 'INSTRUCTOR         TOTAL_SCH       TOTAL_TUITION'.
           DISPLAY '----------         ---------       -------------'.

       026-DISPLAY-LAST.
           MOVE COST-OVERALL TO COST-FORMAT.
           DISPLAY ' '
           DISPLAY 'College of Engineering:'SCH-OVERALL'   'COST-FORMAT.
           DISPLAY '=================================================='.
       030-PROCESS-FILE.
           READ C-SORT AT END MOVE 1 TO FILE-STATUS.
           MOVE EMAIL TO LAST-E.
           PERFORM 035-CALCULATE-RECORDS UNTIL E-O-F.

       035-CALCULATE-RECORDS.

           MOVE EMAIL TO TEACHER.

           IF TEACHER IS NOT EQUAL TO LAST-E
               IF CNTR IS EQUAL TO 0
                   ADD 1 TO CNTR
               ELSE
                   PERFORM 036-SWITCH-TEACH
                   PERFORM 040-DISPLAY-DEETS


           ELSE
               MOVE SCH TO SCH-NUM.
               ADD SCH-NUM TO SCH-TOTAL.
               ADD SCH-NUM TO SCH-OVERALL.
               IF COURSE-PREFIX IS LESS THAN 5
                   MULTIPLY SCH-NUM BY 238.85 GIVING COURSE-TUITION
               ELSE
                   MULTIPLY SCH-NUM BY 496.50 GIVING COURSE-TUITION.
           ADD COURSE-TUITION TO TUITION-TOTAL.
           ADD COURSE-TUITION TO COST-OVERALL.




           READ C-SORT AT END MOVE 1 TO FILE-STATUS.


       036-SWITCH-TEACH.
           MOVE TEACHER TO LAST-E.




       040-DISPLAY-DEETS.
           MOVE SCH TO SCH-NUM.
           MOVE TUITION-TOTAL TO MONEYS.
           MOVE SCH-TOTAL TO SCH_AMOUNT.

           DISPLAY OUTPUT-LINE.

           SET SCH_AMOUNT TO 0.
           SET COURSE-TUITION TO 0.
           SET SCH-TOTAL TO 0.
           SET TUITION-TOTAL TO 0.


       041-CLOSE-STUFF.
           CLOSE C-SORT.

       END PROGRAM HOMEWORK-2.
