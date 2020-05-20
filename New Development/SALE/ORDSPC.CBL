      $SET LINKCOUNT"586"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******    ******   *******    ******    *
      *   **    **  **	  **  **    **  **    **  **	  **  **	**   *
      *   **    **  **	  **  **    **  **	  **	  **  **	     *
      *   **    **  *******   **    **   ******   *******   **	     *
      *   **    **  **	 **   **    **        **  **	      **	     *
      *   **    **  **	  **  **    **  **    **  **	      **	**   *
      *    ******   **	  **  ******     ******   **	       ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    O R D E R S	 -   S P E C I A L   R O U T I N E S           *
      *                                                                *
      *     VERSION 8.14.03 - May 2010                                 *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     ORDSPC.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 2002.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2010
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY CONTROL.SL.

000140 COPY PARAM.SL.

000250 COPY SORDER.SL.

000310 COPY STOCK.SL.

000140 COPY SYSUSE.SL.

000250 COPY WORDER.SL.

000140 COPY WYSUSE.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000220 COPY PARAM.FDE.

000540 COPY SORDER.FDE.

000540 COPY STOCK.FDE.

000310 COPY SYSUSE.FDE.

000540 COPY WORDER.FDE.

000310 COPY WYSUSE.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000950 77  WS-S1	   PIC	9(04)	 COMP-5.
000960 77  WS-S2	   PIC	9(04)	 COMP-5.
000970 77  WS-S3	   PIC	9(04)	 COMP-5.
000980 77  WS-S4	   PIC	9(04)	 COMP-5.
000990 77  WS-S5	   PIC	9(04)	 COMP-5.
001000 77  WS-S6	   PIC	9(04)	 COMP-5.
001000 77  WS-S7	   PIC	9(04)	 COMP-5.
001000 77  WS-S8	   PIC	9(04)	 COMP-5.
001010 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-PRNKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
       77  WS-ADV-RATE	   PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI	   PIC	X(04).
003100 77  WS-6LPI	   PIC	X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001100 77  WS-OPTION	   PIC	X(01).
001100 77  WS-VAT-GST	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001140 77  WS-CHAR1	   PIC	X(01).
001150 77  WS-CHAR2	   PIC	X(01).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-TYPE	   PIC	X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-REF	   PIC	X(08) VALUE SPACES.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440	       05  WS-TCHR PIC X(80) VALUE "фммммммммммммммямммммммммммм
      -	       "ммммммммммммяммммммммммммммммммммммяммммммямммммммм╣".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC X(80) VALUE "зддддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддд©".
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR PIC X(80) VALUE "Ё              Ё
      -        "            Ё                      Ё      Ё        Ё".
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "Ё".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC X(80) VALUE "фммммммммммммммомммммммммммм
      -	       "ммммммммммммоммммммммммммммммммммммоммммммомммммммм╣".
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC X(80) VALUE "юддддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддды".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "ORD".
	   03  WS-PROG	       PIC  X(03) VALUE "SPC".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  WS-LOCAL-PATH.
	   03  FILLER	       PIC  X(12) VALUE "C:\APACSHR".

       01  WS-LOCAL-ID.
	   03  WS-LOCAL-DRIVE  PIC  X(01) VALUE "C".
	   03  FILLER	       PIC  X(14) VALUE ":\APACSHR\SORD".
	   03  WS-USER-ID      PIC  X(03).
	   03  FILLER	       PIC  X(04) VALUE ".ORD".

       01  WS-LOCAL-ID2.
	   03  WS-LOCAL-DRIVE2 PIC  X(01) VALUE "C".
	   03  FILLER	       PIC  X(19) VALUE ":\APACSHR\USERS.DAT".

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

003670 COPY SORDER.ID.

002220 COPY STOCK.ID.

       COPY SYSUSE.ID.

003670 COPY WORDER.ID.

       COPY WYSUSE.ID.

001660 01  W10-INPUT.
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

       COPY W20.WS.

001890 01  W25-CALCS.
001900     03  W25-RESULT     PIC 9(07)V99.
001910     03  W25-RESULT1 REDEFINES W25-RESULT.
001920         05  FILLER      PIC 9(01).
001930         05  W25-WHOLE  PIC 9(06).
001940         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(06).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
001950     03  W25-PASS.
001960         05  W25-PASS1  PIC 9(03).
001970         05  W25-PASS2  PIC 9(03).
001980     03  W25-TIME       PIC 9(08).
001990     03  W25-TRED REDEFINES W25-TIME.
002000         05  FILLER      PIC 9(02).
002010         05  W25-FACT   PIC 9(06).
002020     03  W25-TOTAL      PIC S9(07)V99  COMP-3.
002030     03  W25-VALUE      PIC S9(07)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       01  W60-FILES.
	   03  W60-FILE-NAME	 PIC  X(22) VALUE SPACES.
	   03  W60-FILE-DETAILS.
	       05  W60-FILE-SIZE PIC X(08) COMP-X.
	       05  W60-FILE-DATE.
		   07  W60-DAY	 PIC X(01) COMP-X.
		   07  W60-MNTH  PIC X(01) COMP-X.
		   07  W60-YEAR  PIC X(02) COMP-X.
	       05  W60-FILE-TIME.
		   07  W60-HRS	 PIC X(01) COMP-X.
		   07  W60-MINS  PIC X(01) COMP-X.
		   07  W60-SECS  PIC X(01) COMP-X.
		   07  W60-DEC	 PIC X(01) COMP-X.

      /
      *        **	******	**     **  **	**
      *        **	  **	***    **  **  **
      *        **	  **	** *   **  ** **
      *        **	  **	**  *  **  ***
      *        **	  **	**   * **  ** **
      *        **	  **	**    ***  **  **
      *        *******	******	**     **  **	**
      *
       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **    **  **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
003150 SCREEN SECTION.

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
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

       01  S37.
	   03  LINE  8 COLUMN 59 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(01)
				 USING DEB-ORD-SYS AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
      *        *******    *******     ******     ******
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ******
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006220 AA000-MAIN	       SECTION.
006230 AA000-INIT.
003240	   IF LS0-SALEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM BA000.

       AA49.
             EXIT PROGRAM.

      *
      *    ****   E R A S E   S C R E E N   F R O M   S P E C I F I E D
      *           L O C A T I O N
      *
000010 ERASE-SCREEN       SECTION.

000030 ERASE-SCREEN-LOOP.
002130	     DISPLAY "Ё" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BGRND78 WITH FOREGROUND-COLOR 3
		     "Ё" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
000050	     ADD 1		 TO CLIN.

000070 ERASE-SCREEN-EXIT.
000080       EXIT.

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY APAC.HLP.

       AA200              SECTION.
       AA201.
002130	     DISPLAY "Ё" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BGRND78 WITH FOREGROUND-COLOR 3
		     "Ё" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1		 TO CLIN.
       AA205.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

011270 COPY SORDER.RD.

011830 COPY STOCK.RD.

       COPY SYSUSE.RD.

011270 COPY WORDER.RD.

       COPY WYSUSE.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

011270 COPY SORDER.WR.

       COPY STOCK.WR.

       COPY SYSUSE.WR.

011270 COPY WORDER.WR.

       COPY WYSUSE.WR.

      *    *************************************************************
      * 	 O R D E R S   -   S P E C I A L   R O U T I N E S
      * 		    S E L E C T I O N	M E N U
      *    *************************************************************
       BA000	       SECTION 5.
       BA00.
018580	     DISPLAY CLR-SCREEN.
	     DISPLAY "SALES - SPECIAL ROUTINES" AT 0229
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддд" AT 0626
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "©"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "Ё  " AT 0726
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "SALES UPDATE OPTIONS"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     " Ё" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "Ё " AT 0826
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Update Server Files Ё"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "Ё " AT 0926
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Update Local Files  Ё"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "Ё " AT 1026
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Exit                Ё"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY "ю" AT 1126
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 3 HIGHLIGHT
		     "ддддддддддддддддддддддды"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 0.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 9		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 28		 TO SCREEN-COL.
	     MOVE 21		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
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
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "2"
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF WS-OPTION = "0"
	       GO TO BA999.
	   IF WS-OPTION = "1"
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM EA000.
	   IF WS-ERROR = 9
	       GO TO BA999.
	     GO TO BA00.

       BA999.
	     EXIT.
      *    *************************************************************
      * 			O P E N   F I L E S
      *    *************************************************************
       CA000		       SECTION.
       CA00.
038970	     OPEN I-O SORDER.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000		   GO TO CA200
039010         ELSE
039020         IF FLE-LIMIT
039030		   GO TO CA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060             MOVE 20       TO WS-F-ERROR
039070		   GO TO CA50.
039080     IF WS-STATUS NOT = "00"
039090         MOVE 20           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO CA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
054300	     OPEN I-O STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330		   GO TO CA200
054340         ELSE
054350         IF FLE-LIMIT
054360		   GO TO CA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400		   GO TO CA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO CA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.
038970	     OPEN I-O WORDER.
	   IF WS-STATUS = "05"
	       CLOSE WORDER
	       OPEN OUTPUT WORDER
	       CLOSE WORDER
	       OPEN I-O WORDER.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000		   GO TO CA200
039010         ELSE
039020         IF FLE-LIMIT
039030		   GO TO CA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 42	 TO WS-F-ERROR
039070		   GO TO CA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 42		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO CA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
038970	     OPEN I-O SYSUSER.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000		   GO TO CA200
039010         ELSE
039020         IF FLE-LIMIT
039030		   GO TO CA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 64	 TO WS-F-ERROR
039070		   GO TO CA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 64		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO CA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
038970	     OPEN I-O WYSUSER.
	   IF WS-STATUS = "05"
	       CLOSE WYSUSER
	       OPEN OUTPUT WYSUSER
	       CLOSE WYSUSER
	       OPEN I-O WYSUSER.
038980	   IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000		   GO TO CA200
039010         ELSE
039020         IF FLE-LIMIT
039030		   GO TO CA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 65	 TO WS-F-ERROR
039070		   GO TO CA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 65		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO CA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.
	     MOVE ZERO		 TO WS-ERROR.
	     GO TO CA499.

039780 CA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
039830       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
039840	     GO TO CA205.

       CA50.
039900	   IF WS-F-ERROR = 20
039910	       MOVE "SALES ORDER (Serv)"
				 TO WS-FILE
041840	   ELSE
039900	   IF WS-F-ERROR = 22
039910	       MOVE "STOCK FILE" TO WS-FILE
041840	   ELSE
039900	   IF WS-F-ERROR = 42
039910	       MOVE "SALES ORDER (Local)"
				 TO WS-FILE
041840	   ELSE
041850	   IF WS-F-ERROR = 64
041860	       MOVE "USER (Serv) file"
                                 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 65
041860	       MOVE "USER (Local) file"
				 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
	     ACCEPT WS-OPTION AT 1038
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
039840	     GO TO CA205.

040660 CA200.
040670       DISPLAY "Files locked - Try later" AT 2312
040680                WITH FOREGROUND-COLOR 14
040690               " " WS-STATUS WITH FOREGROUND-COLOR 15.
040700       DISPLAY "Press any key" AT 2512
040710		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
040720	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       CA205.
	     MOVE 9		 TO WS-ERROR.

       CA499.
	     EXIT.
      *    *************************************************************
      * 			C L O S E   F I L E S
      *    *************************************************************
       CA500		       SECTION.
       CA501.
	     CLOSE SORDER
		   STOCK
		   SYSUSER
		   WORDER
		   WYSUSER.

       CA999.
	     EXIT.
      /    *************************************************************
      * 	       U P D A T E   S E R V E R   F I L E S
      *    *************************************************************
       DA000		       SECTION 50.
       DA00.
018580	     DISPLAY CLR-SCREEN.
	     DISPLAY "SALES - UPDATE SERVER FILES" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "This routine will read the orders on the local har
      -		     "d drive and update the" AT 0405
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "Sales order file on the network file server. The o
      - 	     "rder numbers will also" AT 0505
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "be updated on the Network file server.  After the
      - 	     "update, the orders  on" AT 0605
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "the local hard drive will be deleted.
      -		     "			    " AT 0705
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "Continue Y/N?" AT 0934
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-OPTION.

       DA05.
	     ACCEPT WS-OPTION AT 0948
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       CALL X"E5" CALL X"E5"
	       GO TO DA05.
	   IF WS-OPTION = "N"
	       GO TO DA999.
	     MOVE 0401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 9.
	     PERFORM CA000.
	   IF WS-ERROR = 9
	       GO TO DA998.
      *
      *    ****    C H E C K   U P D A T E   S E Q U E N C E
      *
	     MOVE WS-USER-ID	 TO USE-NO
				    WSE-NO.
	     PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       MOVE "Invalid USER number (Server)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA998.
	   IF USE-SEQ = 1
	       MOVE "May not UPDATE server files"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA998.
	     PERFORM READ-WYSUSER THRU READ-WYSUSER-EXIT.
	   IF WS-F-ERROR = 65
	       MOVE "Invalid USER number (Local)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA998.
	   IF WSE-SEQ = 0
	       MOVE USE-RECORD	 TO WSE-RECORD
	       MOVE 1		 TO WSE-SEQ
	       PERFORM REWRITE-WYSUSER THRU WRITE-WYSUSER-EXIT
	       GO TO DA998.
	     DISPLAY "Download Orders" AT 0412
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY "Reading:" AT 0512.
	     DISPLAY "Writing:" AT 0612.
000740	     MOVE SPACES	 TO WOR-REF
				    WS-REF.
	     MOVE ZERO		 TO WOR-SEQ.
000750	     PERFORM START-AT-WORDER-NO THRU READ-WORDER-EXIT.
	   IF WS-F-ERROR = 42
	       MOVE "Nothing to update"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA15.

       DA10.
	     PERFORM READ-WORDER-NEXT THRU READ-WORDER-EXIT.
	   IF WS-F-ERROR = 42
	       CLOSE WORDER
	       OPEN OUTPUT WORDER
	       CLOSE WORDER
	       OPEN I-O WORDER
	       GO TO DA15.
	   IF WOR-TYPE = ZERO
	       IF WOR-DOC = "O"
		   MOVE WOR-REF	 TO WS-REF
	       ELSE
		   MOVE SPACES	 TO WS-REF.
	     DISPLAY WOR-KEY AT 0521
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE WOR-REC1	 TO SOR-REC1.
	     PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
	     DISPLAY SOR-KEY AT 0621
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF WOR-TYPE = 1
	       IF WOR-REF = WS-REF
		   PERFORM DA20.
	     PERFORM DELETE-WORDER-REC THRU WRITE-WORDER-EXIT.
	     GO TO DA10.

       DA15.
	     DISPLAY "Update User Details - User No:" AT 0812
		      USE-NO AT 0843
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE WSE-RECORD	 TO USE-RECORD.
	     PERFORM REWRITE-SYSUSER THRU WRITE-SYSUSER-EXIT.
	     MOVE ZERO		 TO WSE-SEQ.
	     PERFORM REWRITE-WYSUSER THRU WRITE-WYSUSER-EXIT.
	     DISPLAY "Sales order processing transferred to Server"
		      AT 1011 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY "Update of Server files complete - Press any key to
      -		     " Continue" AT 1111
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.

	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO DA998.
      *    *************************************************************
      *       U P D A T E   P I C K I N G   S L I P   Q U A N T I T Y
      *    *************************************************************
       DA20.
	     MOVE WOR-EXT-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	     SUBTRACT WOR-QNT	 FROM STK-QUANT.
	     ADD WOR-QNT	 TO STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       DA25.
	     EXIT.

       DA998.
	     PERFORM CA500.

       DA999.
	     EXIT.
      /    *************************************************************
      * 	       U P D A T E   L O C A L	 F I L E S
      *    *************************************************************
       EA000		       SECTION 51.
       EA00.
018580	     DISPLAY CLR-SCREEN.
	     DISPLAY "SALES - UPDATE LOCAL FILES" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "This routine will update the User Record on the Lo
      -		     "cal drive." AT 0405
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "Continue Y/N?" AT 0634
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE "N"		 TO WS-OPTION.

       EA05.
	     ACCEPT WS-OPTION AT 0648
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       CALL X"E5" CALL X"E5"
	       GO TO EA05.
	   IF WS-OPTION = "N"
	       GO TO EA999.
	     MOVE 0401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 9.
	     PERFORM CA000.
	   IF WS-ERROR = 9
	       GO TO EA998.
      *
      *    ****    C H E C K   U P D A T E   S E Q U E N C E
      *
	     MOVE WS-USER-ID	 TO USE-NO
				    WSE-NO.
	     PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       MOVE "Invalid USER number (Server)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA998.
	     PERFORM READ-WYSUSER THRU READ-WYSUSER-EXIT.
	   IF WS-F-ERROR = 65
	       GO TO EA10.
	   IF WSE-SEQ = 1
	       MOVE "May not UPDATE local files"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO EA998.

       EA10.
	     DISPLAY "Update User Details - User No:" AT 0412
		      USE-NO AT 0443
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE USE-RECORD	 TO WSE-RECORD.
	     MOVE 1		 TO WSE-SEQ.
	   IF WS-F-ERROR = 65
	       PERFORM WRITE-WYSUSER THRU WRITE-WYSUSER-EXIT
	   ELSE
	       PERFORM REWRITE-WYSUSER THRU WRITE-WYSUSER-EXIT.
	     MOVE ZERO		 TO USE-SEQ.
	     PERFORM REWRITE-SYSUSER THRU WRITE-SYSUSER-EXIT.
	     DISPLAY "Sales order processing transferred to Local"
		      AT 0611 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY "Update of Local files complete - Press any key to
      -		     "Continue" AT 0711
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       EA998.
	     PERFORM CA500.

       EA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
015920 ZA000-INIT	       SECTION 80.
015930 ZA000-OPEN.
	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
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
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "SYSUSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSUSER.
      *
      *    ****    F I L E S   O N   L O C A L	 D R I V E
      *
	     MOVE LS0-NO	 TO WS-USER-ID.
	     MOVE WS-LOCAL-ID	 TO W02-WORDER.
	     MOVE WS-LOCAL-ID2	 TO W02-WYSUSER.
      *
      *    ****    C H E C K   I F   F I L E S	 A R E	 O N   T H E
      * 	   L O C A L   D R I V E
      *
	     MOVE WS-LOCAL-ID2	 TO W60-FILE-NAME.
	     CALL "CBL_CHECK_FILE_EXIST" USING W60-FILE-NAME
					       W60-FILE-DETAILS
				     RETURNING WS-STATUS.
	   IF (WS-STAT1H = ZERO)
		GO TO ZA27.
	     CALL "CBL_CREATE_DIR" USING WS-LOCAL-PATH
				   RETURNING WS-STATUS.

       ZA27.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

041240 ZA60.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 81.

041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.

041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.

041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.

041560 DISPLAY-FILE-NAME.
051840	   IF WS-F-ERROR = 2
051850	       MOVE W02-NETWORK  TO WS-FILE
051860	       MOVE WS-NETKEY	 TO WS-KEY
052190     ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
052190	   ELSE
052200     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
	       MOVE SOR-KEY	 TO WS-KEYX
041310     ELSE
041320     IF WS-F-ERROR = 22
041330         MOVE W02-STOCKF   TO WS-FILE
041340         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
052190	   ELSE
052200	   IF WS-F-ERROR = 42
052210	       MOVE W02-WORDER	 TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
	       MOVE WOR-KEY	 TO WS-KEYX
052190	   ELSE
052200	   IF WS-F-ERROR = 64
052210	       MOVE W02-SYSUSER  TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
	       MOVE USE-NO	 TO WS-KEYX
052190	   ELSE
052200	   IF WS-F-ERROR = 65
052210	       MOVE W02-WYSUSER  TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
	       MOVE WSE-NO	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94"
045410         DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
