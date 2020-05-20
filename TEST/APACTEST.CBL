      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ****    *******    ******   ********   ******   ********    *
      *   **  **   **	 **  **    **	  **	 **    **     **       *
      *  **    **  **	 **  ** 	  **	 **	      **       *
      *  ********  *******   ** 	  **	  ******      **       *
      *  **    **  **	     ** 	  **	       **     **       *
      *  **    **  **	     **    **	  **	 **    **     **       *
      *  **    **  **	      ******	  **	  ******      **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   A P A C   T E S T   S Y S T E M   M E N U   P R O G R A M    *
      *                                                                *
      *     VERSION 8.14.00 - June 2009				       *
      * 							       *
      ******************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      TESTING THE APAC SYSTEM		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё The problems with printing to a USB printer has become an Ё
      *    Ё urgent shortfall in the APAC system. The only method to   Ё
      *    Ё solve the problem, with the current compilers, will be to Ё
      *    Ё use the windows API's when using one of these printers.   Ё
      *    Ё The quick solution is to use NET EXPRESS to do the print- Ё
      *    Ё ing, while the neccessary code is written for APAC. This  Ё
      *    Ё will be possible as most programs call PRNTUTIL to do the Ё
      *    Ё printing. With an additional indicator the system could   Ё
      *    Ё call a different program for USB printing. Before this    Ё
      *    Ё happens, it must be possible to call a NET EXPRESS pro-   Ё
      *    Ё gram from the current programs. This program will be used Ё
      *    Ё to test and implement the necessary code to enable this.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACTEST.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JUNE 2009.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 -
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
		 PARTICULAR PURPOSE. See the GNU General Public License
		 for more details.

		 You should have received a copy of the GNU General
		 Public License along with this program; if not, write
		 to the Free Software Foundation, Inc., 59 Temple Place
		 - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.

000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

      *COPY APACFIDS.SL.

000140*COPY PARAM.SL.

000120*COPY SYSTEM.SL.

000330 DATA DIVISION.
000340 FILE SECTION.

      *COPY APACFIDS.FDE.

000220*COPY PARAM.IFD.

000200*COPY SYSTEM.FD.

000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000890 77  WS-ERROR	       PIC  9(01) VALUE ZERO.
       77  WS-OPTION	   PIC	X(01).
       77  PRG-NAME	   PIC	X(28)
			   VALUE "APACTEST - MENU PROGRAM 2009".
       77  WS-NUM          PIC  Z9.
       77  WS-CONV	   PIC	9(01).
       77  APAC-SYSTEM	   PIC	X(12).
       77  APAC-VERSION    PIC	X(04).

001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "зддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "APC".
	   03  WS-PROG	       PIC  X(03) VALUE "TST".

001860 COPY CHAIN.WS.

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY APACFILE.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

       01  BLOCK-DETAIL1.
	   03  ORIGINAL-VID1.
	       05  ORIGINAL-CHAR1 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID1.
	       05  REVERSE-CHAR1  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
	   03  SHAD1-ROW       PIC  9(02) COMP-X.
	   03  SHAD1-COL       PIC  9(02) COMP-X.
	   03  SHAD1-LINES     PIC  9(02) COMP-X.
	   03  SHAD1-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD1-CHAR      PIC  X(01).

       01  CRT-DETAIL1.
	   03  TOP-ROW1       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW1     PIC  9(02) COMP-X.
	   03  STRING-LENGTH1  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION1.
	       05  SCREEN-LIN1 PIC  9(02) COMP-X.
	       05  SCREEN-COL1 PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
	   03  ORIGINAL-VID2.
	       05  ORIGINAL-CHAR2 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID2.
	       05  REVERSE-CHAR2  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
	   03  SHAD2-ROW       PIC  9(02) COMP-X.
	   03  SHAD2-COL       PIC  9(02) COMP-X.
	   03  SHAD2-LINES     PIC  9(02) COMP-X.
	   03  SHAD2-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD2-CHAR      PIC  X(01).

       01  CRT-DETAIL2.
	   03  TOP-ROW2       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW2     PIC  9(02) COMP-X.
	   03  STRING-LENGTH2  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION2.
	       05  SCREEN-LIN2 PIC  9(02) COMP-X.
	       05  SCREEN-COL2 PIC  9(02) COMP-X.

	   03  W02-CHECK-SYS.
	       05  W02-CHK-ID  PIC  X(03) VALUE "DEM".
	       05  FILLER      PIC  X(16) VALUE "_DATA\SYSTEM.DAT".
	       05  FILLER      PIC  X(01) VALUE X"00".

001160 01  W05-COMMAND.
001170	   03  W05-CHAR        PIC  X(128).

       COPY W12.WS.

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  W25-DAYS    PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

      /
000750 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 34 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
000870				 VALUE "APAC TEST MENU".

       01  S-MENU.
	   03  LINE  3 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "здддддддддддддддддддд".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "©".
	   03  LINE  4 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "Ё  ".
	   03	       COLUMN 33 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PROGRAM  CONTROL".
	   03	       COLUMN 49 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "  Ё".
	   03  LINE  5 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 33 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE " Screen Test  	  ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "Ё".
	   03  LINE  6 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 33 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE " Printer Test     ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "Ё".
001470	   03  LINE  7 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 33 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE " Graphics Test 	  ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "Ё".
001570	   03  LINE  8 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "Ё ".
	   03	       COLUMN 32 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 33 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE " End System       ".
	   03	       COLUMN 51 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "Ё".
	   03  LINE  9 COLUMN 30 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 5 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 31 BACKGROUND-COLOR 5 FOREGROUND-COLOR 0
				 VALUE "дддддддддддддддддддды".

002480 01  S98.
	   03  LINE 11 COLUMN 14 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT VALUE
	       "зддддддддддддддддддддддддддддддддддддддддддддддддддд".
	   03	       COLUMN 66 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "©".
002490	   03  LINE 12 COLUMN 14 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 15 BACKGROUND-COLOR 4
002500                           FOREGROUND-COLOR 15 VALUE 
002510		"End of test for screen, printing and graphics MF5.1".
	   03	       COLUMN 66 BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
				 VALUE "Ё".
	   03  LINE 13 COLUMN 14 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 4 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 15 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 0 VALUE
	       "ддддддддддддддддддддддддддддддддддддддддддддддддддды".
	   03  LINE 24 COLUMN 79 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 3 VALUE "д".

       COPY ERROR.CRT.

      /
010570 PROCEDURE DIVISION.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     DISPLAY S-PROG.
	     PERFORM BA000.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
007630	     DISPLAY CLR-SCREEN.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 36		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
007820	     DISPLAY S98.
	     PERFORM SCREEN-SHADOW.
	     STOP RUN.

       AA100	       SECTION.
       AA101.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     PERFORM SAVE-SCREEN-3.

       AA102.
	     MOVE SPACES	 TO WS-NEXT-PRG.
	     CALL PRG-NAME
		 USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
		 ON EXCEPTION
		     GO TO AA110
	     END-CALL.
	     CANCEL PRG-NAME.

       AA105.
	     PERFORM RESTORE-SCREEN-3.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     GO TO AA199.

       AA110.
	     MOVE SPACE 	 TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO AA105.

       AA199.
	     EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.SCR.

      /    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	  D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      ERASE SCREEN FROM SPECIFIED LOCATION	       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CPOS (CLIN,CCOL) as starting position and increases  Ё
      *    Ё CLIN by 1 until CLIN reaches the line allocated to exit.  Ё
      *    Ё The screen is cleared with Column 1 and 80 containing "Ё" Ё
      *    Ё and columns 2 to 79 containing spaces.		       Ё
      *    Ё  eg.						       Ё
      *    Ё	  MOVE 0801	 TO CPOS.			       Ё
      *    Ё	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

011433 COPY CLEAR.CRT.

      /    *************************************************************
      *    ****    READ FILES ROUTINES
      *    *************************************************************
008680 AC000              SECTION.

011750*COPY PARAM.RD.

       COPY APAC.HLP.

       BA000	       SECTION 50.
       BA00.
      /    *************************************************************
      *    ****      D I S P L A Y   T H E   T E S T   M E N U	    ****
      *    *************************************************************
      *
      *			     здддддддддддддддддддд©
      * 		     Ё	PROGRAM  CONTROL  Ё╟╟
      * 		     Ё 1 Screen Test	  Ё╟╟
      * 		     Ё 2 Printer Test	  Ё╟╟
001470* 		     Ё 3 Graphics Test	  Ё╟╟
001570* 		     Ё 0 End System	  Ё╟╟
      *			     юдддддддддддддддддддды╟╟
      *			       ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
	     MOVE 4		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA02.
	     MOVE 4		 TO TOP-ROW.
	     MOVE 7		 TO BOTTOM-ROW.
	     MOVE 3		 TO SCREEN-LIN.
	     MOVE 32		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < 0 OR > 3)
		   CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	     EVALUATE SCREEN-LIN
	       WHEN 4
		   MOVE "1"	 TO WS-OPTION
	       WHEN 5
		   MOVE "2"	 TO WS-OPTION
	       WHEN 6
		   MOVE "3"	 TO WS-OPTION
	       WHEN 7
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO BA05
	     END-EVALUATE.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 4	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 5	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 6	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	     PERFORM SAVE-SCREEN.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL1.
	     MOVE SHADOW-DETAIL  TO SHADOW-DETAIL1.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL1.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   GO TO BA999
	       WHEN "1"
		   PERFORM CA000
	       WHEN "2"
		   PERFORM DA000
	       WHEN "3"
		   PERFORM EA000
	     END-EVALUATE.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.

       BA999.
	     EXIT.

       CA000	       SECTION 51.
       CA00.
	     PERFORM SAVE-SCREEN-2.
007630	     DISPLAY CLR-SCREEN.
      *
      *       здддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *       Ё	Test the SCREEN GUI using OBJ COBOL and Express 5.1. Ё╟╟
      *       Ё      Select OBJECT or Express COBOL.		     Ё╟╟
      *       юдддддддддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      * 	╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
	     DISPLAY
	     "здддддддддддддддддддддддддддддддддддддддддддддддддддддд"
	      AT 1213 WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
	     "©" WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 0.
	     DISPLAY "Ё" AT 1313 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				      BACKGROUND-COLOR 4.
	     DISPLAY
	     " Test the SCREEN GUI using OBJ COBOL and Express 5.1. "
	      AT 1314 WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "Ё" AT 1368 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 4.
	     DISPLAY "Ё" AT 1413 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				      BACKGROUND-COLOR 4.
	     DISPLAY
	     "     Select "
	      AT 1414 WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 4
	     "O" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 4
	     "BJECT or " WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 4
	     "E" WITH FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 4
	     "xpress COBOL.		     "
	      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 4.
	     DISPLAY "Ё" AT 1468 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 4.
	     DISPLAY "ю" AT 1513 WITH FOREGROUND-COLOR 4 HIGHLIGHT
				      BACKGROUND-COLOR 4.
	     DISPLAY
	     "дддддддддддддддддддддддддддддддддддддддддддддддддддддды"
	      AT 1514 WITH FOREGROUND-COLOR 0
			   BACKGROUND-COLOR 4.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 15		 TO SHADE-COL.
	     MOVE 54		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

       CA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CA05
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		   PERFORM RESTORE-SCREEN-2
		   GO TO CA999
		 WHEN OTHER	 CALL X"E5" CALL X"E5" CALL X"E5"
	       END-EVALUATE
	       GO TO CA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF NOT(WS-OPTION = "O" OR "E")
		   CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO CA05
	       END-IF
	   ELSE
	       CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO CA05.
	   EVALUATE WS-OPTION
             WHEN "O"
               MOVE "TESTCALL"   TO PRG-NAME
	     WHEN "E"
               MOVE "TESTWIN"    TO PRG-NAME
	     WHEN OTHER
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO CA05
	   END-EVALUATE.
	     PERFORM AA100.
	     GO TO CA05.

       CA999.
	     EXIT.

       DA000	       SECTION 52.
       DA00.
	     PERFORM SAVE-SCREEN-3.

	     PERFORM RESTORE-SCREEN-3.

       DA999.
	     EXIT.

       EA000	       SECTION 53.
       EA00.
	     PERFORM SAVE-SCREEN-3.

	     PERFORM RESTORE-SCREEN-3.

       EA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
      *      MOVE W02-CHECK-SYS	 TO W02-SYSTEM.

      *	     MOVE "DEM" 	 TO W02-SYSPTH W02-CHK-ID.
	     MOVE ZERO		 TO WS-ERROR.
      *	     OPEN I-O APACFILE.
      *	   IF NOT (WS-STATUS = "00")
      *	       OPEN I-O SECUR.
      *	   IF NOT (WS-STATUS = "00")
      *	       MOVE 9		 TO WS-ERROR
      *	       GO TO APACFILES-EXIT
      *	   ELSE
      *	       CLOSE SECUR
      *	   END-IF
	     MOVE "Version 9.00B"
				 TO APAC-VERSION.
	     MOVE "APAC Testing" TO APAC-SYSTEM.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  DATE RETURNS A SIX DIGIT RESULT AS YYMMDD. W25-FACT IS   Ё
      *    Ё  DEFINED AS YYMMDD. THESE DETAILS ARE THEN MOVED TO       Ё
      *    Ё  W12-TODAY (DDMMCCYY) AND THE CENTURY (20) IS INSERTED.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
      *
      * 	     Insert the century
      *
	     MOVE 20		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
      *
      * 	     Switch date to CCYYMMDD
      *
003200	     MOVE W12-DAY	 TO W12-DD.
003210	     MOVE W12-MONTH	 TO W12-MM.
	     MOVE W12-CENT	 TO W12-CC.
003220	     MOVE W12-YEAR	 TO W12-YY.
	     MOVE TODAY-DDMMYY	 TO W12-TODAY.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D o   n o t   d i s p l a y   E R R O R   m e s s a g e s Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO USER-ACTION.
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   S e t   u p	 t h e	 F U N C T I O N   k e y s     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       ESC, F1 to F10 keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    PAGE-UP and PAGE-DOWN keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   A C T I V A T E   M O U S E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	   ELSE
	       GO TO ZA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       ZA00.
	     DISPLAY CLR-SCREEN.
      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
040090 ZA60.
040650	     MOVE WS-COMPANY	 TO WS-TOP-COMP.
	     MOVE "Supervisorд"	 TO WS-WRKHD.

040750 ZA999.
040760       EXIT.
