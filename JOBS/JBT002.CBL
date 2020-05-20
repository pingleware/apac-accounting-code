      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   ********  *******   ********	   ****	     ****     ******   *
      *      **     **	  **	 **	  **  **    **	**   **    **  *
      *      **     **	  **	 **	 **    **  **	 **	  **   *
      *      **     *******	 **	 **    **  **	 **	**     *
      *      **     **	  **	 **	 **    **  **	 **   **       *
      *   ** **     **	  **	 **	  **  **    **	**   **        *
      *    ***	    *******	 **	   ****	     ****    ********  *
      *                                                                *
      *     ENGLISH						       *
      *                                                                *
      *     J O B   C O S T I N G   -   J O B - T Y P E   M A I N T    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JBT002.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   NOVEMBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000270 COPY JOBRMK.SL.

000210 COPY PARAM.SL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000500 COPY JOBRMK.FDE.

000440 COPY PARAM.FDE.

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-S1	   PIC	9(04)	 COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
000990 77  WS-LINES        PIC  9(02) VALUE 0.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
001060 77  WS-OPTION       PIC  X(01).
001120 77  WS-ERROR        PIC  9(01).
001130 77  WS-SELECT       PIC  X(13) VALUE "Select Option".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
001350 01  WS-DB-LINE.
001360     03  WS-TOP-LINE.
001370         05  WS-TCHAR PIC X(01) OCCURS 80.
001380     03  WS-TOP-LNE2.
001390         05  WS-TCR  PIC X(01) OCCURS 80.
001400     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001440	   03  WS-MID-LINE.
001450         05  WS-MCHAR PIC X(01) OCCURS 80.
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
001480	   03  WS-BOT-LINE.
001490         05  WS-BCHAR PIC X(01) OCCURS 80.
001500     03  WS-BOT-LNE2.
001510         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
	       05  FILLER  PIC	X(01) VALUE "³".
               05  FILLER  PIC  X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
001520     03  WS-TOP-LNE.
001530         05  WS-TCHR PIC X(01) OCCURS 80.
001540     03  WS-T-LINE REDEFINES WS-TOP-LNE.
001550         05  FILLER  PIC X(01).
001560         05  WS-H-LINE
001570                     PIC X(78).
001580         05  FILLER  PIC X(01).
001590     03  WS-MID-LNE.
001600         05  WS-MCHR PIC X(01) OCCURS 80.
001610     03  WS-BOT-LNE.
001620         05  WS-BCHR PIC X(01) OCCURS 80.
001630     03  WS-TP-LNE.
001640         05  WS-TCH  PIC X(01) OCCURS 80.
001650     03  WS-MD-LNE.
001660         05  WS-MCH  PIC X(01) OCCURS 80.
001670     03  WS-BT-LNE.
001680         05  WS-BCH  PIC X(01) OCCURS 80.
001690     03  WS-TP-LINE.
001700         05  WS-TCA  PIC X(01) OCCURS 80.
001710     03  WS-MD-LINE.
001720         05  WS-MCA  PIC X(01) OCCURS 80.
001730     03  WS-BT-LINE.
001740         05  WS-BCA  PIC X(01) OCCURS 80.
001710	   03  WS-MD-LIN.
001720	       05  WS-MCB   PIC  X(80) VALUE "³  ³        ³            ³
      -        "                                ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JBT".
	   03  WS-PROG	       PIC  X(03) VALUE "002".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

002180 COPY JOBRMK.ID.

       COPY PARAM.ID.

       01  W10-JOB-TYPE.
           03  W10-KEY.
	       05  W10-JREM   PIC  X(06).
           03  W10-REMK1      PIC  X(30).
           03  W10-REMK2      PIC  X(30).
           03  W10-REMK3      PIC  X(30).
           03  W10-REMK4      PIC  X(30).

003280 01  W12-DATE.
003290     03  W12-TODAY       PIC 9(06).
003300     03  W12-DATE-DMY REDEFINES W12-TODAY.
003310         05  W12-DAY     PIC 9(02).
003320         05  W12-MONTH   PIC 9(02).
003330         05  W12-YEAR    PIC 9(02).
003340     03  W12-T-YMD       PIC 9(06).
003350     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003360         05  W12-YY      PIC 9(02).
003370         05  W12-MM      PIC 9(02).
003380         05  W12-DD      PIC 9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

004760 01  W75-KEYS.
004770     03  W75-S          PIC 9(02) COMP-5.
004780     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
004810	   03  W75-JREM       PIC X(06) OCCURS 18.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006010 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S03.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
                                 VALUE "JOB TYPE - DELETE".
       01  S04.
	   03  LINE  4 COLUMN 12 VALUE "Type Code".
	   03  LINE  5 COLUMN 12 VALUE "Remarks 1".
	   03  LINE  6 COLUMN 12 VALUE "Remarks 2".
	   03  LINE  7 COLUMN 12 VALUE "Remarks 3".
	   03  LINE  8 COLUMN 12 VALUE "Remarks 4".

       01  S05.
           02  S05A.
	   03  LINE  4 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W10-JREM AUTO.
           02  S05B.
	   03  LINE  5 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W10-REMK1 AUTO.
	   03  LINE  6 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W10-REMK2 AUTO.
	   03  LINE  7 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W10-REMK3 AUTO.
	   03  LINE  8 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W10-REMK4 AUTO.

006620 01  S06.
006630	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

       COPY JRMLUP.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010580 AA000-MAIN	       SECTION.
010590 AA000-INIT.
003240	   IF LS0-JBLEV < 3
	       DISPLAY CLEAR-L25
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010600	     PERFORM ZA000-INIT.
	     PERFORM CB000.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM JOBRMK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.SCR.

       COPY OPTION.CRT.

       COPY ERROR.SCR.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000           SECTION.

011790 COPY JOBRMK.RD.

011750 COPY PARAM.RD.

      /
011850 AD000-WRITE     SECTION.

011930 COPY JOBRMK.WR.

011890 COPY PARAM.WR.

       COPY JOBRMK.LUP.

       COPY APAC.HLP.

      /
      *    ****    D E L E T E   J O B   T Y P E
      *
       CB000	       SECTION 5.
       CB00.
             DISPLAY CLR-SCREEN.
             DISPLAY S03.
	     DISPLAY S04.
	     MOVE SPACES	 TO W10-JREM.
       CB05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Job Code Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       CB10.
             ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO CB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB10
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB10
	       END-EVALUATE
	       DISPLAY S05A
	       IF W10-JREM = SPACES
		   GO TO CB10.
	   IF W10-JREM = SPACES
	       GO TO CB999.
	     MOVE W10-JREM	 TO JRM-CODE.
	     PERFORM READ-JOBRMK THRU READ-JOBRMK-EXIT.
           IF WS-F-ERROR = 41
016670	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB10.
	     DISPLAY CLEAR-L25.
	     MOVE JRM-REMK1	 TO W10-REMK1.
	     MOVE JRM-REMK2	 TO W10-REMK2.
	     MOVE JRM-REMK3	 TO W10-REMK3.
	     MOVE JRM-REMK4	 TO W10-REMK4.
             DISPLAY S05B.
       CB15.
	     MOVE "Delete (Y/N)  [ ]"
				 TO WS-ERR-MES.
             MOVE SPACE          TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB00
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CB15.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CB15.
           IF WS-OPTION = "N"
               GO TO CB00.
	     PERFORM READ-JOBRMK-LOCK THRU READ-JOBRMK-EXIT.
	     PERFORM DELETE-JOBRMK-REC-UNLOCK THRU WRITE-JOBRMK-EXIT.
             GO TO CB00.
       CB999.
             EXIT.

038140 ZA000-INIT	       SECTION 9.
038150 ZA000-OPEN.
038160       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
038170	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
038180       DISPLAY "JOB TYPE - MAINTENANCE PROGRAM" AT 1229
038190                WITH FOREGROUND-COLOR 15.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "JOBRMK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBRMK.
039440	     MOVE 1		 TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039770       GO TO ZA999.
039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
039830       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
039840       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040040 ZA55.
040050       MOVE 1 TO WS-S1.
040060       MOVE SPACES         TO WS-MID-LINE WS-MID-LNE 
040070				    WS-MD-LINE	WS-MD-LNE.
040090 ZA60.
040100       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
040110       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
040120       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
040130       MOVE WS-G1          TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
040140       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
040150     IF WS-S1 < 80
040160         ADD 1 TO WS-S1
040170         GO TO ZA60.
040180       MOVE WS-G9          TO WS-TCR   (1).
040190       MOVE WS-G10         TO WS-TCR   (80).
040200       MOVE WS-G11         TO WS-BCR   (1).
040210       MOVE WS-G12         TO WS-BCR   (80).
040220       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
040230                              WS-TCHR  (1)  WS-BCHR  (1)
040240                              WS-TCA   (1)  WS-BCA   (1)
040250                              WS-TCH   (1)  WS-BCH   (1).
040260       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
040270                              WS-TCHR  (80) WS-BCHR  (80)
040280                              WS-TCA   (80) WS-BCA   (80)
040290                              WS-TCH   (80) WS-BCH   (80).
040300       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
040310                              WS-TCHAR (50) WS-TCHAR (59) 
040320                              WS-TCHAR (69)
040330                              WS-TCHR  (18) WS-TCHR  (49)
040340                              WS-TCHR  (60) WS-TCHR  (70)
040350                              WS-TCA   (10) WS-TCA   (17)
040360                              WS-TCA   (50) WS-TCA   (59)
040370                              WS-TCA   (68) 
040380                              WS-TCH   (8)  WS-TCH   (49)
040390                              WS-TCH   (61) WS-TCH   (72).
040400       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
040410                              WS-MCHAR (50) WS-MCHAR (59) 
040420                              WS-MCHAR (69) WS-MCHAR (80)
040430                              WS-MCHAR (1)
040440                              WS-MCH   (8)  WS-MCH   (49)
040450                              WS-MCH   (61) WS-MCH   (72)
040460                              WS-MCA   (1)  WS-MCA   (80)
040470                              WS-MCA   (10) WS-MCA   (17)
040480                              WS-MCA   (50) WS-MCA   (59)
040490                              WS-MCA   (68) 
040510                              WS-MCHR  (18) WS-MCHR  (49)
040520                              WS-MCHR  (60) WS-MCHR  (70)
040530                              WS-MCHR  (1)  WS-MCHR  (80) 
040540                              WS-MCH   (1)  WS-MCH   (80).
040550       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
040560                              WS-BCHAR (50) WS-BCHAR (59) 
040570                              WS-BCHAR (69)
040580                              WS-BCHR  (18) WS-BCHR  (49)
040590                              WS-BCHR  (60) WS-BCHR  (70)
040600                              WS-BCA   (10) WS-BCA   (17)
040610                              WS-BCA   (50) WS-BCA   (59)
040620                              WS-BCA   (68) 
040630                              WS-BCH   (8)  WS-BCH   (49)
040640                              WS-BCH   (61) WS-BCH   (72).
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
040660 ZA200.
040670       DISPLAY "Files locked - Try later" AT 2312
040680                WITH FOREGROUND-COLOR 14
040690               " " WS-STATUS WITH FOREGROUND-COLOR 15.
040700       DISPLAY "Press any key" AT 2512 
040710		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
040720	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
040730 ZA205.
040740       EXIT PROGRAM.
040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION	9.
040790 OPEN-ERROR.
040800       DISPLAY CLR-SCREEN.
040810       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
040820       PERFORM DISPLAY-FILE-NAME.
040830
040840 READ-ERROR.
040850       DISPLAY CLR-SCREEN.
040860       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
040870       PERFORM DISPLAY-FILE-NAME.
040880
040890 WRITE-ERROR.
040900       DISPLAY CLR-SCREEN.
040910       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
040920       PERFORM DISPLAY-FILE-NAME.
040930

040950 DISPLAY-FILE-NAME.
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041240     IF WS-F-ERROR =  41
041250	       MOVE W02-JOBRMK	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JRM-KEY	 TO WS-KEYX.
041350     IF WS-STATUS = "10"
041360         MOVE "End of FILE" TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450         MOVE "DISK full" TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510         MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530       DISPLAY "Status " AT 1212 
041540                WS-STATUS WITH FOREGROUND-COLOR 11
041550               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560     IF WS-STATUS NOT = "94" 
041570         DISPLAY "Key    " AT 1412 
041580                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
041590         DISPLAY "Reverse backup or contact program Support"
041600                  AT 1612.
041610         DISPLAY "Please make a note of these details" AT 1812.
041620       STOP RUN.

