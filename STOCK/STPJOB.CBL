      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	********   ******   *******    *
      *   **	**     **     **    **	   **	  **	**  **	  **   *
      *   **	       **     **    **	   **	  **	**  **	  **   *
      *    ******      **     *******	   **	  **	**  ******     *
      * 	**     **     **	   **	  **	**  **	  **   *
      *   **	**     **     **	** **	  **	**  **	  **   *
      *    ******      **     **	 ***	   ******   *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     J O B   D E T A I L S   P R O G R A M   C A L L E D   B Y  *
      *     S T O C K  E N Q U I R I E S			       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPJOB.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   SEPTEMBER 2005.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2008
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

000120 COPY PARAM.SL.

       COPY JOBCST.SL.

000010	   SELECT JOBDET  ASSIGN DISK
000020                    LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000050                    ACCESS RANDOM
000060                    ORGANIZATION RELATIVE
000070			  RELATIVE WS-JEXKEY.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY PARAM.FDE.

       COPY JOBCST.FDE.

      /
000010 FD  JOBDET    IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-JOBDET.
000220 01  JEX-REC1.
	   03  JEX-KEY.
000050	     05  JEX-REF       PIC  X(08).
	     05  JEX-SEQN.
		 07  JEX-SEQ   PIC  9(03).
		 07  JEX-SUB   PIC  9(02).
      *
      *    ****    TYPE : 1  -	ITEM DETAILS
      *                 : 2  -  DISCOUNT DETAILS
      *                 : 3  -  REMARKS
      *                 : 4  -  INTERNAL NOTES FOR PICKING SLIP 
      * 		: 7  -	REQUISITIONS
      *                 : 8  -  JOB REQUIRED
      *
	   03  JEX-TYPE        PIC  9(01).
000140	   03  JEX-DETAIL.
	     05  JEX-EXT-ITEM.
000190		 07  JEX-ITEM  PIC  X(14).
		 07  JEX-ITM   PIC  X(03).
		 07  FILLER    PIC  X(01).
000160	     05  JEX-QNT       PIC S9(06)V999 COMP-3.
	     05  JEX-COST      PIC S9(06)V999 COMP-3.
	     05  JEX-SELL      PIC S9(08)V999 COMP-3.
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
       77  WS-CHECK	   PIC	X(18)	 VALUE
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
001860 77  WS-JEXKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-JEXLAST	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-JOB-STAT	   PIC	X(01) VALUE "Y".
	   88 CLOSE-JOB    VALUE "Y".
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-ETYPE	   PIC	X(01).
001820 77  WS-SEQ	   PIC	X(01) VALUE "D".
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "Ё                Ё
      -        "                      Ё          Ё         Ё         Ё".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "POR".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

000300 COPY JOBCST.ID.

000010	   03  W02-JOBDET.
000030	       05  FILLER      PIC  X(04) VALUE "JOBX".
	       05  FILLER      PIC  X(01) VALUE "_".
000040	       05  W02-USE13T  PIC  X(03).
000050	       05  FILLER      PIC  X(04) VALUE ".APC".

002550 01  W10-STOCK.
0	   03  W10-EXT-ITEM.
02560	       05  W10-ITEM   PIC  X(14).
	       05  W10-ITM    PIC  X(03).
	       05  FILLER     PIC  X(01).
002910	   03  W10-DATE.
	       05  W10-EDTE   PIC  99/99/9999.
	   03  W10-COST       PIC S9(07)V99 COMP-3.

       COPY W12.WS.

       COPY W15.WS.

       COPY W20.WS.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003530 COPY W50.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-S2	      PIC 9(02) COMP-5.
005860	   03  W75-CNO	      PIC X(06) OCCURS 18.
	   03  W75-COST       PIC S9(07)V99 COMP-3 OCCURS 18.

004390 01  W95-STM.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.

004490 01  W100-EDIT.
	   03  W100-Q1.
	       05  W100-QNT	PIC Z(05)9.999.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       01  W16-CALL.
	   03  W16-EXT-ITEM.
	       05  W16-ITEM  PIC  X(14).
	       05  W16-ITM   PIC  X(03).
	       05  FILLER    PIC  X(01).
	   03  W16-CRED      PIC  X(06).
	   03  W16-COST      PIC S9(07)V99 COMP-3.
	   03  W16-CANCEL    PIC  X(01).

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS
		       LS0-SECURITY W16-CALL.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
	     MOVE "Y"		 TO W16-CANCEL.
	     MOVE W16-EXT-ITEM	 TO W10-EXT-ITEM.
	     MOVE W16-COST	 TO W10-COST.
010270	     PERFORM ZA000-INIT.
	     PERFORM JB-VIEW-QUANT.
	   IF CLOSE-JOB
	       CLOSE JOBCST.
	     CLOSE JOBDET.

       AA49.
013150       EXIT PROGRAM.

       AA99.
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
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
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

       COPY FUNCTION.CRT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  OPT-MESSAGE			       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'. 			       Ё
      *    Ё							       Ё
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё							       Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.				       Ё
      *    Ё							       Ё
      *    Ё This would be displayed as:			       Ё
      *    Ё	здддддддддддддддддддддддддддддддддддддддддддддддд©     Ё
      *    Ё	Ё	   Print transactions (Y/N) [ ] 	 Ё╟╟   Ё
      *    Ё	юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟   Ё
      *    Ё	  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟   Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 13) move the line number to be used to SLIN and   Ё
      *    Ё then PEFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

       COPY DATE.CHK.

       COPY DATE.CNV.

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       R E A D	 F I L E S   R O U T I N E S	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
011650 AC000		  SECTION.

       COPY PARAM.RD.

       COPY JOBCST.RD.

      /
      *    ****    V I E W   J O B   C A R D   Q U A N T I T I E S
      *
      *			       зддддддддбдддддддддд©	5
      *			       Ё Job RefЁ Quantity Ё╟╟	6
      *			       цддддддддедддддддддд╢╟╟	7
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟	8
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟	9
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 10
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 11
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 12
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 13
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 14
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 15
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 16
      *			       ЁJJJJJJJJЁZZZZZ9.999Ё╟╟ 17
      *			       юддддддддадддддддддды╟╟ 18
      *				 ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
      *
      *
       JB-VIEW-QUANT   SECTION 50.
       JB-VIEW.
	     PERFORM SAVE-SCREEN.
	     DISPLAY CLEAR-L25.
	     DISPLAY "зддддддддбдддддддддд©" AT 0554
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё Job RefЁ Quantity Ё" AT 0654
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "цддддддддедддддддддд╢" AT 0754
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "юддддддддадддддддддды" AT 1854
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE 0854		 TO CPOS.
	     PERFORM JB-VIEW-CRT-EDGE UNTIL CLIN = 18.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 56		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 13		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE W16-EXT-ITEM	 TO JOB-EXT-ITEM.
	     MOVE 1		 TO JOB-TYPE.
	     MOVE SPACES	 TO JOB-REF.

       JB-VIEW-START.
	     PERFORM START-AT-JOB-DETAIL THRU READ-JOBCST-EXIT.
	   IF WS-F-ERROR = 13
	       GO TO JB-VIEW-RESTORE.
	     MOVE ZERO		 TO WS-JEXKEY.

       JB-EXTRACT-DET.
	     PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
	   IF WS-F-ERROR = 13
	       IF WS-JEXKEY > ZERO
		   GO TO JB-STORE-END
	       ELSE
		   GO TO JB-NO-DETAIL.
	   IF NOT (JOB-TYPE = 1)
	       IF WS-JEXKEY > ZERO
		   GO TO JB-STORE-END
	       ELSE
		   GO TO JB-NO-DETAIL.
	   IF NOT (W16-EXT-ITEM = JOB-EXT-ITEM)
	       IF WS-JEXKEY > ZERO
		   GO TO JB-STORE-END
	       ELSE
		   GO TO JB-NO-DETAIL.
000050	      MOVE JOB-REF	 TO JEX-REF.
	      MOVE JOB-SEQN	 TO JEX-SEQN.
	      MOVE JOB-TYPE	 TO JEX-TYPE.
	      MOVE JOB-EXT-ITEM  TO JEX-EXT-ITEM.
000160	      MOVE JOB-QNT	 TO JEX-QNT.
	      MOVE JOB-COST	 TO JEX-COST.
	      MOVE JOB-SELL	 TO JEX-SELL.
	      ADD 1		 TO WS-JEXKEY.
	      WRITE JEX-REC1.
	      GO TO JB-EXTRACT-DET.

       JB-STORE-END.
	     MOVE WS-JEXKEY	 TO WS-JEXLAST.
	     MOVE 1		 TO WS-JEXKEY.

       JB-VIEW-TOP.
000180	     MOVE 08		 TO CLIN.

       JB-VIEW-READ-NEXT.
	     READ JOBDET.
008710	   IF NOT (WS-STATUS = "00")
	       GO TO JB-VIEW-USER-OPT.
	     MOVE JEX-QNT	 TO W100-QNT.
	     DISPLAY (CLIN, 55) JEX-REF
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY ( , 64) W100-Q1
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF CLIN < 17
	       IF WS-JEXKEY < WS-JEXLAST
		   ADD 1	 TO CLIN WS-JEXKEY
		   GO TO JB-VIEW-READ-NEXT.

       JB-VIEW-USER-OPT.
001020	     DISPLAY "PgUp" AT 2527
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgDn" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     " - " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001170		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to quit" WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1.

001310 JB-VIEW-GET-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO JB-VIEW-RESTORE
		 WHEN PAGE-DOWN
001340		     GO TO JB-VIEW-CHK-END-CRT
		 WHEN PAGE-UP
001340		     GO TO JB-VIEW-READ-PREV
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO JB-VIEW-GET-OPT.
	     CALL X"E5".
001280	     GO TO JB-VIEW-GET-OPT.

       JB-VIEW-CHK-END-CRT.
	   IF CLIN < 17
	       GO TO JB-VIEW-GET-OPT.

       JB-VIEW-CLEAR-CRT.
	     MOVE 0854		 TO CPOS.
	     PERFORM JB-VIEW-CRT-EDGE UNTIL CLIN = 18.
001630	     GO TO JB-VIEW-TOP.

       JB-VIEW-READ-PREV.
	   IF WS-JEXKEY > 19
	       SUBTRACT 20	 FROM WS-JEXKEY
	   ELSE
	       MOVE 1		 TO WS-JEXKEY.
	     GO TO JB-VIEW-TOP.

       JB-NO-DETAIL.
	     MOVE "No outstanding jobs"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE
	     GO TO JB-VIEW-RESTORE.

       JB-VIEW-CRT-EDGE.
	     DISPLAY "Ё        Ё          Ё" AT CPOS
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     ADD 1		 TO CLIN.

       JB-VIEW-RESTORE.
011320       PERFORM RESTORE-SCREEN.

       JB-VIEW-EXIT.
	     EXIT.

      /    *************************************************************
      *    ****  I N I T I A L I Z E   P R O G R A M
      *    *************************************************************
041980 ZA000-INIT	       SECTION 90.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE LS-USER	 TO W02-USE13T.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
053830	     OPEN I-O JOBCST.
	   IF WS-STATUS = "41"
	       MOVE "N" 	 TO WS-JOB-STAT
	       GO TO ZA05.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT OR
		 (WS-STATUS = "39")
		   MOVE 13	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 13		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.

       ZA05.
	     OPEN OUTPUT JOBDET.
053840	   IF RUNTIME-ERROR
053850	       IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
	       ELSE
053910		   MOVE 32	 TO WS-F-ERROR
053920	       PERFORM OPEN-ERROR.
	     CLOSE JOBDET.
	     OPEN I-O JOBDET.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 13
041860	       MOVE "JOB COSTING file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
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
021970	     MOVE WS-G4 	 TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 91.
      
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
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 13
041860	       MOVE W02-JOBCST	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE JOB-KEY	 TO WS-KEYX
	   ELSE
041850	   IF WS-F-ERROR = 32
041860	       MOVE W02-JOBDET	 TO WS-FILE
041870	       MOVE WS-JEXKEY	 TO WS-KEY
	       MOVE JEX-EXT-ITEM TO WS-KEYX.
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
