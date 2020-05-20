      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *****   **   **  **	  **   *****  ********   ***   ********  *
      *  **   **  **   **  **  **   **   **    **     ** **	    **     *
      *  **       **   **  ** **    **         **    **   **    **     *
      *  **       *******  ****      *****     **    *******    **     *
      *  **       **   **  ** **	     **    **    **   **    **     *
      *  **   **  **   **  **  **   **   **    **    **   **    **     *
      *   *****   **   **  **   **   *****     **    **   **    **     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C H E C K	I - O   S T A T U S   P R O G R A M            *
      *                                                                *
      *     VERSION 9.00.00 - October 2012                             *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRINTUTL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   November 2000.

	   This program is used to check the status after an I-O operation.

	   The program performs the following functions:

	       Check type of error.
	       Interpret the error.
	       Display the error message.
	       Return a code to the calling program.

	   The calling program/s pass the status code, file name and the
	   record key to this program.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2000 - 2012
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.

000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000260 DATA DIVISION.
000270 FILE SECTION.
      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001570 77  WS-S1	   PIC	9(04)	 COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4	   PIC	9(04)	 COMP-5.
       77  WS-IO	   PIC	9(01).
       77  WS-ERR-TYPE	   PIC	X(22).
       77  WS-RESULT	   PIC	X(28).
       77  WS-OPTION	   PIC	X(01).
       77  WS-DUPLICATE    PIC	X(15) VALUE "Will change key".
       77  WS-FATAL	   PIC	X(26) VALUE
			  "Processing will be aborted".
       77  WS-POSREC	   PIC	X(27) VALUE
			  "May continue or end process".
       77  WS-RECOVER	   PIC	X(22) VALUE "Will attemp to recover".

002230 COPY WS.WS.

       01  W05-FILE.
	   03  W05-FILE-CHAR PIC  X(01) OCCURS 12.

       01  W10-STATUS.
	   03  W10-STAT1     PIC  X(01).
	   03  W10-HYPH      PIC  X(01).
	   03  W10-STAT2     PIC  9(03).

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

       LINKAGE SECTION.
       77  LS-ERROR	       PIC  9(02) COMP-X.

000010 01  LS-FILE-STATUS.
000020	   03  LS-STATUS       PIC  X(02).

000180 01  LS-FILE.
	   05  LS-FILE-NME     PIC  X(64).

000190 01  LS-KEY.
	   03  LS-RAND-KEY     PIC  X(06).

       01  LS-KEYX.
	   03  LS-IDX-KEY      PIC  X(24).

       01  LS-PROGRAM.
	   03  LS-PROG-NME     PIC  X(08).

004580 SCREEN SECTION.

      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLEAR-L25.
	   05  LINE 25 BLANK LINE BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 1.

       01  S01.
	   03  LINE  9 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4
		   VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "¿".
	   03  LINE 10 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "File Name  :                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 11 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "Status Code:                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 12 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "Error Type :                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 13 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "Error      :                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 14 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "Record Key :                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 15 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "Result     :                             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 16 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "Ã".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "´".
	   03  LINE 17 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "        Press any key to continue        ".
	   03	       COLUMN 61 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03  LINE 18 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE "À".
	   03	       COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4
		   VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02.
	   03  LINE 10 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(12) FROM W05-FILE.
	   03  LINE 11 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(05) FROM W10-STATUS.
	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(22) FROM WS-ERR-TYPE.
	   03  LINE 13 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(28) FROM WS-STAT-MESSAGE.
	   03  LINE 14 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(24) FROM WS-KEYX.
	   03  LINE 15 COLUMN 33 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC  X(28) FROM WS-RESULT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

001230 PROCEDURE DIVISION
	     USING LS-FILE-STATUS LS-FILE LS-KEYX LS-ERROR.
       AA000	       SECTION.
       AA00.
	     MOVE LS-ERROR	 TO WS-IO.
	     MOVE ZERO		 TO LS-ERROR.
	     MOVE LS-FILE-STATUS TO WS-FILE-STATUS
				    W10-STATUS.
	     PERFORM SAVE-SCREEN.
	     PERFORM AA200.
	     EVALUATE WS-STAT1
      * 		  S U C C E S S F U L L
	       WHEN "0"
		       GO TO AA05
      * 		  E N D   O F	F I L E
	       WHEN "1"
		       MOVE 3	 TO LS-ERROR
		       GO TO AA05
      * 		  I N V A L I D   K E Y
	       WHEN "2"
		       PERFORM BA000
      * 		  P E R M A N E N T   E R R O R
	       WHEN "3"
		       PERFORM CA000
      * 		  L O G I C A L   E R R O R
	       WHEN "4"
		       PERFORM DA000
      * 		  R U N   T I M E   S Y S T E M   E R R O R
	       WHEN "9"
		       PERFORM EA000
      * 		  U N I D E N T I F I E D   E R R O R
	       WHEN OTHER
		       PERFORM FA000
	     END-EVALUATE.

       AA05.
	     PERFORM RESTORE-SCREEN.

       AA99.
	     EXIT PROGRAM.
      *
      *    ****   D I S P L A Y   M E S S A G E
      *
       AA100	       SECTION.
       AA105.
	     DISPLAY S01.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 21		 TO SHADE-COL.
	     MOVE 41		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S02.
	     PERFORM SCREEN-SHADOW.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA199.
	     EXIT.

       COPY FUNCTION.CRT.

      *
      *    ****   E X T R A C T   S H O R T   F I L E	N A M E
      *
       AA200	       SECTION 50.
       AA205.
	     MOVE LS-FILE	 TO WS-FILE.
	   IF WS-FILE = SPACES
	       MOVE "NO NAME"	 TO WS-FILE
	       GO TO AA299.
	     MOVE 64		 TO WS-S4.

       AA210.
	   IF WS-FILE-CHAR(WS-S4) = SPACE
	       SUBTRACT 1	 FROM WS-S4
	       GO TO AA210.
	     SUBTRACT 1 FROM WS-S4
				 GIVING WS-S3.

       AA215.
	   IF NOT(WS-FILE-CHAR(WS-S3) = "\") AND
		 (WS-S3 > 1)
	       SUBTRACT 1	 FROM WS-S3
	       GO TO AA215.
	   IF WS-FILE-CHAR(WS-S3) = "\"
	       ADD 1		 TO WS-S3.
	     MOVE SPACES	 TO W05-FILE.
	     MOVE 1		 TO WS-S2.

       AA220.
	     MOVE WS-FILE-CHAR(WS-S3)
				 TO W05-FILE-CHAR(WS-S2).
	   IF WS-S3 < WS-S4
	       ADD 1		 TO WS-S3 WS-S2
	       GO TO AA220.

       AA299.
	     EXIT.

       AA999.
	     EXIT.
      *
      *    ****    I N V A L I D   K E Y
      *
       BA000	       SECTION 60.
       BA00.
	     MOVE "Access Key Error"
				 TO WS-ERR-TYPE.
	     EVALUATE WS-STAT2
	       WHEN "1"
		   MOVE "Sequence error"
				 TO WS-STAT-MESSAGE
	       WHEN "2"
		   MOVE "Duplicate key"
				 TO WS-STAT-MESSAGE
		   MOVE WS-DUPLICATE
				 TO WS-RESULT
		   MOVE 2	 TO LS-ERROR
		   GO TO BA05
	       WHEN "3"
		   MOVE "No record found"
				 TO WS-STAT-MESSAGE
		   MOVE 3	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO BA05
	       WHEN "4"
		   MOVE "Boundry violation"
				 TO WS-STAT-MESSAGE
	     END-EVALUATE.
	     MOVE 99		 TO LS-ERROR.
	     MOVE WS-FATAL	 TO WS-RESULT.


       BA05.
	     PERFORM AA100.

       BA999.
	     EXIT.
      *
      *    ****    P E R M A N E N T   E R R O R
      *
       CA000	       SECTION 61.
       CA00.
	     MOVE "Permanent Error"
				 TO WS-ERR-TYPE.
	     EVALUATE WS-STAT2
	       WHEN "0"
		   MOVE "DISK error"
				 TO WS-STAT-MESSAGE
	       WHEN "4"
		   MOVE "Boundry violation"
				 TO WS-STAT-MESSAGE
	       WHEN "5"
		   MOVE "File not found"
				 TO WS-STAT-MESSAGE
	       WHEN "7"
		   MOVE "Open mode not supported"
				 TO WS-STAT-MESSAGE
	       WHEN "8"
		   MOVE "File locked"
				 TO WS-STAT-MESSAGE
		   MOVE 12	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO CA05
	       WHEN "9"
		   MOVE "File attributes error"
				 TO WS-STAT-MESSAGE
	     END-EVALUATE.
	     MOVE 99		 TO LS-ERROR.
	     MOVE WS-FATAL	 TO WS-RESULT.

       CA05.
	     PERFORM AA100.

       CA999.
	     EXIT.
      *
      *    ****    L O G I C A L   E R R O R
      *
       DA000	       SECTION 62.
       DA00.
	     MOVE "Logical Error"
				 TO WS-ERR-TYPE.
	     EVALUATE WS-STAT2
	       WHEN "1"
		   MOVE "File already open"
				 TO WS-STAT-MESSAGE
		   MOVE 4	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "2"
		   MOVE "File already closed"
				 TO WS-STAT-MESSAGE
		   MOVE 5	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "3"
		   MOVE "Previous Read unsuccessfull"
				 TO WS-STAT-MESSAGE
		   MOVE 3	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "4"
		   MOVE "Boundry violation"
				 TO WS-STAT-MESSAGE
	       WHEN "5"
		   MOVE "Record size error"
				 TO WS-STAT-MESSAGE
	       WHEN "6"
		   MOVE "No next record available"
				 TO WS-STAT-MESSAGE
		   MOVE 3	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "7"
		   MOVE "File not open for input"
				 TO WS-STAT-MESSAGE
		   MOVE 10	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "8"
		   MOVE "File not open for output"
				 TO WS-STAT-MESSAGE
		   MOVE 11	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	       WHEN "9"
		   MOVE "File not open as IO"
				 TO WS-STAT-MESSAGE
		   MOVE 14	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO DA10
	     END-EVALUATE.
	     MOVE 99		 TO LS-ERROR.
	     MOVE WS-FATAL	 TO WS-RESULT.

       DA10.
	     PERFORM AA100.

       DA999.
	     EXIT.
      *
      *    ****    R U N - T I M E   S Y S T E M   E R R O R
      *
       EA000	       SECTION 63.
       EA00.
	     MOVE "Run-Time System Error"
				 TO WS-ERR-TYPE.
	     MOVE "-"		 TO W10-HYPH.
	     MOVE WS-STAT2H	 TO W10-STAT2.
	     EVALUATE WS-STAT2H
	       WHEN 2
		   MOVE "File not open"
				 TO WS-STAT-MESSAGE
		   MOVE 13	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 6
		   MOVE "File input only"
				 TO WS-STAT-MESSAGE
		   MOVE 11	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 7
		   MOVE "No disk space available"
				 TO WS-STAT-MESSAGE
	       WHEN 8
		   MOVE "File output only"
				 TO WS-STAT-MESSAGE
		   MOVE 10	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 12
		   MOVE "File already open"
				 TO WS-STAT-MESSAGE
		   MOVE 4	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 13
		   MOVE "File not found"
				 TO WS-STAT-MESSAGE
	       WHEN 14
		   MOVE "File limit exceeded"
				 TO WS-STAT-MESSAGE
		   MOVE 9	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 24
		   MOVE "Disk error"
				 TO WS-STAT-MESSAGE
	       WHEN 41
	       WHEN 43
		   MOVE "Index corrupted"
				 TO WS-STAT-MESSAGE
		   MOVE 6	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 65
		   MOVE "File locked"
				 TO WS-STAT-MESSAGE
		   MOVE 12	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 68
		   MOVE "Record locked"
				 TO WS-STAT-MESSAGE
		   MOVE 1	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 208
		   MOVE "Network error"
				 TO WS-STAT-MESSAGE
		   MOVE 7	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 209
		   MOVE "Communication error"
				 TO WS-STAT-MESSAGE
		   MOVE 7	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	       WHEN 213
		   MOVE "Record locked limit exceeded"
				 TO WS-STAT-MESSAGE
		   MOVE 8	 TO LS-ERROR
		   MOVE WS-RECOVER
				 TO WS-RESULT
		   GO TO EA10
	     END-EVALUATE.
	     MOVE 99		 TO LS-ERROR.
	     MOVE WS-FATAL	 TO WS-RESULT.

       EA10.
	     PERFORM AA100.

       EA999.
	     EXIT.
      *
      *    ****    U N I D E N T I F I E D   E R R O R
      *
       FA000	       SECTION 64.
       FA00.
	     MOVE "Unidentified Error"
				 TO WS-ERR-TYPE.
	     MOVE "-"		 TO W10-HYPH.
	     MOVE WS-STAT2H	 TO W10-STAT2.
	     MOVE "Notify Dealer with Details"
				 TO WS-STAT-MESSAGE.
	     MOVE 99		 TO LS-ERROR.
	     MOVE WS-FATAL	 TO WS-RESULT.
	     PERFORM AA100.

       FA999.
	     EXIT.
