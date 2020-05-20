      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******   ********   *
      *   **	**     **     **    **	 **  **   **	**  **	       *
      *   **	**     **     **    **	**    **  **	    **	       *
      *   **	**     **     *******	********  **	    *****      *
      *   **	**     **     **	**    **  **   ***  **	       *
      *   **	**     **     **	**    **  **	**  **	       *
      *   *******      **     **	**    **   ******   ********   *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *    D E B T O R	 P A T C H   A G E   D A T E S	 P A T C H     *
      *                                                                *
      *     VERSION 9.00.00 - August 2012			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPAGE.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 2006.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2006 - 2012
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

000120 COPY PARAM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

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
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001770 77  WS-NTP	   PIC	X(01).
001820 77  WS-OPTION	   PIC	X(01).
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
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
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       01  W10-DTE.
002910	   03  W10-DATE.
	       05  W10-EDTE	 PIC  99/99/9999.

       COPY W12.WS.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W30-AGE-DATES.
	   03  W30-120	       PIC  9(08) COMP-3.
	   03  W30-90	       PIC  9(08) COMP-3.
	   03  W30-60	       PIC  9(08) COMP-3.
	   03  W30-30	       PIC  9(08) COMP-3.
	   03  W30-CUR	       PIC  9(08) COMP-3.

       01  W35-AGE-DATES.
	   03  W35-120	       PIC  9(08) COMP-3.
	   03  W35-90	       PIC  9(08) COMP-3.
	   03  W35-60	       PIC  9(08) COMP-3.
	   03  W35-30	       PIC  9(08) COMP-3.
	   03  W35-CUR	       PIC  9(08) COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

004580 SCREEN SECTION.

       01  S01.
	   03  LINE  2 COLUMN	 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "AMEND DEBTOR MONTH-END DATES".

       01  S02.
	   03  LINE  4 COLUMN 31 VALUE "Current :".
	   03  LINE  5 COLUMN 31 VALUE " 30 Days:".
	   03  LINE  6 COLUMN 31 VALUE " 60 Days:".
	   03  LINE  7 COLUMN 31 VALUE " 90 Days:".
	   03  LINE  8 COLUMN 31 VALUE "120 Days:".

       01  S03.
	   03 S03-CUR.
	   05  LINE  4 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9/99/9999 USING W35-CUR AUTO.
	   03 S03-30D.
	   05  LINE  5 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9/99/9999 USING W35-30 AUTO.
	   03 S03-60D.
	   05  LINE  6 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9/99/9999 USING W35-60 AUTO.
	   03 S03-90D.
	   05  LINE  7 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9/99/9999 USING W35-90 AUTO.
	   03 S03-120.
	   05  LINE  8 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9/99/9999 USING W35-120 AUTO.


      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-DBLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CI000.

013140 AA49.
013150       EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.				       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      *    *************************************************************
      *    ****        D A T E	 C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY 	       ³
      *    ³							       ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       CI000	       SECTION 5.
       CI00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Switch dates from YYYYMMDD format to DDMMYYYY.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W30-CUR	 TO W20-DTE.
	     PERFORM CI100.
	     MOVE W22-DTE3	 TO W35-CUR.
	     MOVE W30-30	 TO W20-DTE.
	     PERFORM CI100.
	     MOVE W22-DTE3	 TO W35-30.
	     MOVE W30-60	 TO W20-DTE.
	     PERFORM CI100.
	     MOVE W22-DTE3	 TO W35-60.
	     MOVE W30-90	 TO W20-DTE.
	     PERFORM CI100.
	     MOVE W22-DTE3	 TO W35-90.
	     MOVE W30-120	 TO W20-DTE.
	     PERFORM CI100.
	     MOVE W22-DTE3	 TO W35-120.
	     DISPLAY S02.
      *    *************************************************************
      *    **** 	      C U R R E N T   D A T E		    ****
      *    *************************************************************
       CI10.
	     ACCEPT S03-CUR.
	     MOVE W35-CUR	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Current date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI10.
      *    *************************************************************
      *    **** 	      3 0   D A Y S   D A T E		    ****
      *    *************************************************************
       CI15.
	     ACCEPT S03-30D.
	     MOVE W35-30	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "30 Days date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI15.
      *    *************************************************************
      *    **** 	      6 0   D A Y S   D A T E		    ****
      *    *************************************************************
       CI20.
	     ACCEPT S03-60D.
	     MOVE W35-60	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "60 Days date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI20.
      *    *************************************************************
      *    **** 	      9 0   D A Y S   D A T E		    ****
      *    *************************************************************
       CI25.
	     ACCEPT S03-90D.
	     MOVE W35-90	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "90 Days date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI25.
      *    *************************************************************
      *    **** 	     1 2 0   D A Y S   D A T E		    ****
      *    *************************************************************
       CI30.
	     ACCEPT S03-120.
	     MOVE W35-120	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "120 Days date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CI30.

       CI35.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO CI10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Switch dates from DDMMYYYY format to YYYYMMDD.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE W35-CUR	 TO W22-DTE3.
	     PERFORM CI110.
	     MOVE W20-DTE	 TO W30-CUR.
	     MOVE W35-30	 TO W22-DTE3.
	     PERFORM CI110.
	     MOVE W20-DTE	 TO W30-30.
	     MOVE W35-60	 TO W22-DTE3.
	     PERFORM CI110.
	     MOVE W20-DTE	 TO W30-60.
	     MOVE W35-90	 TO W22-DTE3.
	     PERFORM CI110.
	     MOVE W20-DTE	 TO W30-90.
	     MOVE W35-120	 TO W22-DTE3.
	     PERFORM CI110.
	     MOVE W20-DTE	 TO W30-120.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Update the Control record with modified dates.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
	     MOVE W30-CUR	 TO DEB-CUR-DTE.
	     MOVE W30-30	 TO DEB-30D-DTE.
	     MOVE W30-60	 TO DEB-60D-DTE.
	     MOVE W30-90	 TO DEB-90D-DTE.
	     MOVE W30-120	 TO DEB-120-DTE.
	     PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
	     MOVE "Request processed - press ANY key to continue"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     GO TO CI999.

       CI100.
	     MOVE W20-CENT	 TO W22-CC3.
	     MOVE W20-YEAR	 TO W22-YY3.
	     MOVE W20-MONTH	 TO W22-MM3.
	     MOVE W20-DAY	 TO W22-DD3.

       CI110.
	     MOVE W22-CC3	 TO W20-CENT.
	     MOVE W22-YY3	 TO W20-YEAR.
	     MOVE W22-MM3	 TO W20-MONTH.
	     MOVE W22-DD3	 TO W20-DAY.

       CI999.
	     EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
	     DISPLAY S01.
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
	       GO TO ZA00A.
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
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *
      *    ****    G E T   D E B T O R	 M O N T H - E N D   D A Y
      *
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE DEB-CUR-DTE	 TO W30-CUR.
	     MOVE DEB-30D-DTE	 TO W30-30.
	     MOVE DEB-60D-DTE	 TO W30-60.
	     MOVE DEB-90D-DTE	 TO W30-90.
	     MOVE DEB-120-DTE	 TO W30-120.
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
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 8.

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
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
041880	   IF WS-STATUS = "10"
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
