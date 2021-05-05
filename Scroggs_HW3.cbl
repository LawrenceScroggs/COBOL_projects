      ******************************************************************
      * Author: Lawrence Scroggs
      * Date: 04/27/21
      * Purpose: Scan through the givin text file and grab the info on
      * teacher, College they work for and credit hours.
      * Then display the total credit hours per teacher, then department
      * then for the whole colleg.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES-FILE
               ASSIGN TO "C:\Users\lawre\COURSES-ALL.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "REPORT.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD COURSES-FILE.
       01 COURSES-RECORD.
           03 FILLER       PIC X(15).
           03 COLLEGE      PIC X(24).
           03 FILLER       PIC X(14).
           03 DEPT         PIC X(25).
           03 FILLER       PIC X(76).
           03 SCH          PIC X(3).
           03 FILLER       PIC X(7).
           03 TEACHER      PIC X(23).
       FD REPORT-FILE
           REPORT IS COURSES-REPORT.
       WORKING-STORAGE SECTION.
           77 SUM-SCH           PIC ZZZZ9  VALUE 0.
           77 FILE-STATUS       PIC 99     VALUE 0.
               88 EndOfFile     VALUE 1.

       REPORT SECTION.

       RD COURSES-REPORT
           CONTROLS ARE COLLEGE, DEPT, TEACHER
           PAGE LIMIT IS 55 LINES
           FIRST DETAIL 1
           LAST DETAIL 54.

       01 TYPE IS REPORT HEADING.

       01 COLLEGE-HEADING TYPE IS CONTROL HEADING COLLEGE.
               02 FIRST-LINE LINE PLUS 1.
               05 COLUMN 1 PIC X(46)
               VALUE "==============================================".
               05 COLUMN 47 PIC X(46)
               VALUE "==============================================".
           02 FIRST-LINE LINE PLUS 1.
               05 COLUMN 15 PIC X(3)
                   VALUE "***".
               05 COLUMN 18 PIC X(24)
                   SOURCE COLLEGE.
               05 COLUMN 42 PIC X(3)
                   VALUE "***".
           02 SECOND-LINE LINE PLUS 1.
               05 COLUMN 15 PIC X(37)
                   VALUE '------------------------------------'.
       01 TEACHER-SUMMARY TYPE IS CONTROL HEADING TEACHER.
               05 COLUMN 55 PIC ZZZZ9   SOURCE SCH.
       01 DEPART-SUMMARY TYPE IS CONTROL HEADING DEPT.
       01 REPORT-LINE TYPE IS DETAIL.
               05 COLUMN 10 PIC X(1) VALUE NULL.

       01 TEACH-SUMMARY TYPE IS CONTROL FOOTING TEACHER
               NEXT GROUP PLUS 1.

           02 LINE PLUS 1.
               05 COLUMN 5     PIC X(23) SOURCE TEACHER.
               05 COLUMN 55    PIC ZZ,ZZ9 SUM SCH.

       01 DEPT-SUMMARY TYPE IS CONTROL FOOTING DEPT
               NEXT GROUP PLUS 1.

           02 LINE PLUS 1.
               05 COLUMN 10    PIC X(13) VALUE "*DEPARTMENT: ".
               05 COLUMN 30    PIC X(26)  SOURCE DEPT.
               05 COLUMN 65    PIC ZZ,ZZ9  SUM SCH.



       01 REPORT-SUMMARY TYPE IS CONTROL FOOTING COLLEGE

               NEXT GROUP PLUS 1.

           02 LINE PLUS 1.

               05 COLUMN 30    PIC X(11)   VALUE "**COLLEGE:".
               05 COLUMN 45    PIC X(24)  SOURCE COLLEGE.
               05 COLUMN 75    PIC ZZZ,ZZ9 SUM SCH.

           02 LINE PLUS 1.
               05 COLUMN 1     PIC X(46)
               VALUE "==============================================".
               05 COLUMN 47    PIC X(46)
               VALUE "==============================================".
           02 LINE PLUS 3.
               05 COLUMN 1     PIC X(1) VALUE ' '.




       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

            OPEN INPUT COURSES-FILE.

            OPEN OUTPUT REPORT-FILE.

            INITIATE COURSES-REPORT.

            READ COURSES-FILE AT END MOVE 1 TO FILE-STATUS.

            PERFORM GENERATE-REPORT UNTIL EndOfFile.

            TERMINATE COURSES-REPORT.

            CLOSE COURSES-FILE.

            CLOSE REPORT-FILE.

            STOP RUN.



       GENERATE-REPORT.
           GENERATE REPORT-LINE.
           READ COURSES-FILE AT END MOVE 1 TO FILE-STATUS.


       END PROGRAM HOMEWORK-3.
