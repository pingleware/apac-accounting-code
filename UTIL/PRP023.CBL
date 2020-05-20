      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******    ******    *
      *   **	**  **	  **  **    **	 **  **   **	**  **	  **   *
      *   **	**  **	  **  **    **	**    **       **	  **   *
      *   *******   ******    *******	**    **     **        ****    *
      *   **	    **	 **   **	**    **   **		  **   *
      *   **	    **	  **  **	 **  **   **	    **	  **   *
      *   **	    **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   C A S H	 S L I P   H E A D I N G S     *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP023.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2010
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

000120 COPY CONTROL.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000220 COPY PARAM.FDE.

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
000950 77  WS-S1	       PIC 9(04)     COMP-5.
000960 77  WS-S2               PIC 9(04)     COMP-5.
000970 77  WS-S3               PIC 9(04)     COMP-5.
000980 77  WS-S4               PIC 9(04)     COMP-5.
000990 77  WS-S5               PIC 9(04)     COMP-5.
001000 77  WS-S6               PIC 9(04)     COMP-5.
001000 77  WS-S7	       PIC 9(04)     COMP-5.
001000 77  WS-S8	       PIC 9(04)     COMP-5.
001010 77  WS-PARKEY           PIC 9(04)     COMP-5.
001010 77  WS-PRNKEY           PIC 9(04)     COMP-5.
001010 77  WS-DEPKEY           PIC 9(04)     COMP-5.
001010 77  WS-NETKEY           PIC 9(04)     COMP-5.
001020 77  WS-SECKEY           PIC 9(04)     COMP-5.
001070 77  WS-NUM              PIC Z9.
001070 77  WS-SNO              PIC 9(02).
       77  WS-ADV-RATE         PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE             PIC 9(04)     COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI       PIC  X(04).
003100 77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001100 77  WS-OPTION           PIC X(01).
001120 77  WS-ERROR            PIC 9(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-CSH-ADD	   PIC	X(01).
       77  WS-CSH-TEL	   PIC	X(01).
       77  WS-CSH-VAT	   PIC	X(01).
       77  WS-CSH-TIME	   PIC	X(01).
       77  WS-CSH-ASSIST   PIC	X(01).
       77  WS-CSH-DETAIL   PIC	X(01).
       77  WS-CSH-VAT-DET  PIC	X(01).
       77  WS-CSH-ITM	   PIC	X(01).
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

001260 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

001660 01  W10-INPUT.
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

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

004800 01  S21.
004810     03  LINE  3 COLUMN 32 FOREGROUND-COLOR 15
004820                           VALUE "CASH SLIP HEADINGS".
	   03  LINE  4 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
			"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "¿".
004830	   03  LINE  5 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Name".
	   03	       COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                                    ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
004850	   03  LINE  6 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Address".
	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                                 ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE  7 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
			   "       S L I P   D E T A I L S          ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE  8 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print Address".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                           ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE  9 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print Telephone".
	   03	       COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                         ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print VAT Number".
	   03	       COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                        ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 11 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print Time".
	   03	       COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                              ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 12 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print Assisted by".
	   03	       COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                       ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 13 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print 1 or 2 lines per item".
	   03	       COLUMN 48 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "             ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 14 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "³".
	   03	       COLUMN 21 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "Print VAT Details".
	   03	       COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
				 "                       ".
	   03	       COLUMN 61 FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
				 VALUE "³".
	   03  LINE 15 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "À".
	   03	       COLUMN 21 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 4 VALUE
		       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

004890 01  S22.
004900	   03  LINE  5 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004910                           PIC X(32) USING PAR-CSHEAD AUTO.
004920	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004930                           PIC X(32) USING PAR-CSHADD AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

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
003240	   IF LS0-PALEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.
       AA49.
             EXIT PROGRAM.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       AA200		  SECTION.
       AA201.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1 TO CLIN.
       AA205.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
006630 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
006940 AD000-WRITE	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

      /
      *    *****    CASH SLIP HEADINGS	  *****
      *
008640 YA000		       SECTION 5.
013050 YA325-HEAD.
013060       DISPLAY CLR-SCREEN.
013070	     DISPLAY S21.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 22		 TO SHADE-COL.
	     MOVE 40		 TO SHADE-WIDTH.
	     MOVE 11		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
013080	     MOVE 1		 TO WS-PARKEY.
013090       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

013130 YA328.
013140	     ACCEPT S22.

       YA330.
	     ACCEPT WS-CSH-ADD AT 0835
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-ADD
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-ADD = "Y" OR "N")
	       GO TO YA330.

       YA335.
	     ACCEPT WS-CSH-TEL AT 0937
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-TEL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-TEL = "Y" OR "N")
	       GO TO YA335.

       YA340.
	     ACCEPT WS-CSH-VAT AT 1038
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-VAT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-VAT = "Y" OR "N")
	       GO TO YA340.

       YA345.
	     ACCEPT WS-CSH-TIME AT 1132
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-TIME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-TIME = "Y" OR "N")
	       GO TO YA345.

       YA350.
	     ACCEPT WS-CSH-ASSIST AT 1239
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-ASSIST
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-ASSIST = "Y" OR "N")
	       GO TO YA350.

       YA355.
	     ACCEPT WS-CSH-DETAIL AT 1349
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-DETAIL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-DETAIL = "1" OR "2")
	       GO TO YA355.
	   IF WS-CSH-DETAIL = "1"
	       PERFORM YA400.

       YA360.
	     ACCEPT WS-CSH-VAT-DET AT 1439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-CSH-VAT-DET
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-CSH-VAT-DET = "Y" OR "N")
	       GO TO YA360.
	     MOVE 18		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
013220	   IF WS-OPTION = "N"
013230         GO TO YA328.
013240       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.            
	     MOVE 7		 TO WS-PARKEY.
021500	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     MOVE WS-CSH-ADD	 TO PAR-CSH-ADD.
	     MOVE WS-CSH-TEL	 TO PAR-CSH-TEL.
	     MOVE WS-CSH-VAT	 TO PAR-CSH-VAT.
	     MOVE WS-CSH-TIME	 TO PAR-CSH-TIME.
	     MOVE WS-CSH-ASSIST  TO PAR-CSH-ASSIST.
	     MOVE WS-CSH-DETAIL  TO PAR-CSH-DETAIL.
	     MOVE WS-CSH-VAT-DET TO PAR-CSH-VAT-DET.
	     MOVE WS-CSH-ITM	 TO PAR-CSH-ITM.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     UNLOCK PARAM.
	     GO TO YA999.

       YA400.
	     PERFORM SAVE-SCREEN-3.
	     MOVE WS-CSH-ITM	 TO WS-OPTION.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "'I'tem or 'D'escription  [" DELIMITED SIZE
		      WS-CSH-ITM DELIMITED SIZE
		      "]" DELIMITED SIZE
		      INTO WS-ERR-MES
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-CSH-ITM.
011900	   IF NOT (WS-CSH-ITM = "D" OR "I")
011910	       GO TO YA400.

       YA999.
	     EXIT.
      /
015920 ZA000-INIT	       SECTION 8.
015930 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
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
	       GO TO ZA27.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
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

       ZA27.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
	     MOVE 7		 TO WS-PARKEY.
021500	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	   IF NOT (PAR-CSH-ADD = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-ADD
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-TEL = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-TEL
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-VAT = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-VAT
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-TIME = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-TIME
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-ASSIST = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-ASSIST
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-DETAIL = "1" OR "2")
	       MOVE "2" 	 TO PAR-CSH-DETAIL
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-VAT-DET = "Y" OR "N")
	       MOVE "Y" 	 TO PAR-CSH-VAT-DET
	       MOVE 3		 TO WS-ERROR.
	   IF NOT (PAR-CSH-ITM = "I" OR "D")
	       MOVE "D" 	 TO PAR-CSH-ITM
	       MOVE 3		 TO WS-ERROR.
	     MOVE PAR-CSH-ADD	 TO WS-CSH-ADD.
	     MOVE PAR-CSH-TEL	 TO WS-CSH-TEL.
	     MOVE PAR-CSH-VAT	 TO WS-CSH-VAT.
	     MOVE PAR-CSH-TIME	 TO WS-CSH-TIME.
	     MOVE PAR-CSH-ASSIST TO WS-CSH-ASSIST.
	     MOVE PAR-CSH-DETAIL TO WS-CSH-DETAIL.
	     MOVE PAR-CSH-VAT-DET
				 TO WS-CSH-VAT-DET.
	     MOVE PAR-CSH-ITM	 TO WS-CSH-ITM.
	   IF WS-ERROR = 3
	       MOVE ZERO	 TO WS-ERROR
	       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     UNLOCK PARAM.
044780	     GO TO ZA999-EXIT.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 8.
      
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
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
