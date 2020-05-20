      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **    **  **   **	  **   *
      *   **	    **	  **  **    **	**    **  **	**  **	       *
      *   **	    ******    *******	**    **  **	**  *******    *
      *   **	    **	 **   **	**    **  **	**  **	  **   *
      *   **	**  **	  **  **	 **  **    **  **   **	  **   *
      *    ******   **	  **  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *     A G E   A C C O U N T S				       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP006.
000030 AUTHOR.		       J.W.LEMMON (CSS).
000040 DATE-WRITTEN.	       JUNE, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY CRDMEM.SL.

000160 COPY CREDIT.SL.

000200 COPY CRTRAN.SL.

000180	   SELECT CRTOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210			  ORGANIZATION SEQUENTIAL.

000140 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000140 COPY CRDMEM.FDE.

000310 COPY CREDIT.FDE.

000330 COPY CRTRAN.FDE.

       COPY CRTOLD.FD.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000410 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000430 77  WS-CTOKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000430 77  WS-CTRKEY       PIC  9(04)    COMP-5 VALUE 1.
000440 77  WS-CREKEY       PIC  9(04)    COMP-5 VALUE 1.
000450 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
000470 77  WS-RECKEY       PIC  9(04)    COMP-5.
000480 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000490 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
000500 77  WS-ERROR        PIC  9(01) VALUE 0.
000530 77  WS-FC           PIC  9(01) VALUE 0.
       77  WS-SUB          PIC  9(04)    COMP-5.
000550 77  WS-S            PIC  9(04)    COMP-5.
000560 77  WS-S1           PIC  9(04)    COMP-5.
000570 77  WS-S2           PIC  9(04)    COMP-5.
000580 77  WS-S3           PIC  9(04)    COMP-5.
000590 77  WS-S4           PIC  9(04)    COMP-5.
000600 77  WS-S5           PIC  9(04)    COMP-5.
000610 77  WS-S6           PIC  9(04)    COMP-5.
000620 77  WS-S7           PIC  9(04)    COMP-5.
000620 77  WS-S8	   PIC	9(04)	 COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
000630 77  WS-ENT          PIC  X(08).
000640 77  WS-ENT1         PIC  X(08).
000650 77  WS-KEYSTR       PIC  9(04)    COMP-5.
000660 77  WS-OPTION       PIC  X(01).
001820 77  WS-REST	   PIC	X(01).
       77  WS-NUM          PIC  Z9.
000690 77  WS-KEY1         PIC  9(04)    COMP-5.
000700 77  WS-CONREC       PIC  9(01) VALUE ZERO.
000710 77  WS-INDS         PIC  9(01) VALUE 0.
000720 77  WS-INDC         PIC  9(01) VALUE 0.
000750 77  WS-SKIP	   PIC	X(01) VALUE " ".
000750 77  WS-CHAR1        PIC  X(01).
000760 77  WS-LIN          PIC  9(02) VALUE ZERO.
000770 77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
000780 77  WS-PAGE         PIC  9(04)    COMP-5.
000790 77  WS-SEASON       PIC  X(01).
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
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
002430	   03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
	   03  WS-TPCR-LNE REDEFINES WS-TP-LNE.
	       05  WS-TCR1     PIC  X(01).
	       05  WS-TCR78    PIC  X(78).
	       05  WS-TCR80    PIC  X(01).
002450	   03  WS-MD-LNE.
002460         05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
	   03  WS-BTCR-LNE REDEFINES WS-BT-LNE.
	       05  WS-BCR1     PIC  X(01).
	       05  WS-BCR78    PIC  X(78).
	       05  WS-BCR80    PIC  X(01).
	   03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL X"CD".

000980 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "006".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000140 COPY CRDMEM.ID.

       COPY CREDIT.ID.

       COPY CRTRAN.ID.

       COPY CRTOLD.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY W05.WS.

001310 01  W10-MONTHS.
001320     03  W10-MONTH-NAMES
                             PIC  X(36) VALUE
001330                        "JanFebMarAprMayJunJulAugSepOctNovDec".
001340     03  W10-MONTH     REDEFINES W10-MONTH-NAMES
001350                       PIC  X(03) OCCURS 12.

       COPY W15.CRP.

001850 01  W20-CALCS.
001860     03  W20-RESULT    PIC  9(06)V99.
001870     03  W20-RESULT-1  REDEFINES W20-RESULT.
001880         05  W20-WHOLE PIC  9(06).
001890         05  W20-DECIMAL
                             PIC  9(02).
001900     03  W20-ORD       PIC  9(07)    COMP-3.
	   03  W20-DATE.
	       05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
			       PIC 9(08).

002180 01  W40-NADD.
002190     03  W40-ADD.
002200         05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS PIC  X(60).
               05  FILLER    PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

002240 COPY W50.WS.

005680 COPY W70CRED.WS.

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
	   03  W75-CNO	     PIC  X(06) OCCURS 18.

002440 01  W95-COMPANY.
002450     03  W95-COMP      PIC  X(40).

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
002460 SCREEN SECTION.

       COPY BLANK.CRT.

007080 01  S12.
007090	   03  LINE  2 COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
007100                           VALUE "BUSY WITH MONTH-END AGE".

007860 01  S39.
007870	   03  LINE  4 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
007880				 " This routine does not include a recov
007890- 			 "ery procedure. Security ".
007900	   03  LINE  5 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
007910				 " copies  of the data files should be m
007920- 			 "ade as a power  failure ".
007930	   03  LINE  6 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
007940				 " or any other problem during this proc
007950- 			 "edure would require the ".
007960	   03  LINE  7 COLUMN 10 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE
007970				 " DATA FILES from the previous backup t
007980- 			 "o be restored !!!       ".
007990	   03  LINE  9 COLUMN 10 VALUE
				 " Do you wish to continue (Y/N)? ".

       01  S-CRE-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.

       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY CRDLUP.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
003240	   IF LS0-CRLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
004940	     PERFORM HA000.
005260	     CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM CREDITOR-LOOKUP
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

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
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³	       other keys are to be tested on exit)	       ³
      *    ³			     OR 			       ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)					       ³
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
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY APAC.HLP.

      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

000140 COPY CRDMEM.RD.

       COPY CREDIT.RD.

       COPY CRTRAN.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
007140 AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY CRTRAN.WR.

       COPY PARAM.WR.

       COPY CREDIT.LUP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
      *        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007940 AY000	       SECTION.
007950 AY01.
007960	     MOVE 01		 TO REC-FILE.
007970	     MOVE WS-PARKEY	 TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
007980	     MOVE PAR-RECORD1	 TO REC-PARAM.
007990       GO TO AY50.
008000 AY07.
008010       MOVE 07 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
008030	     MOVE P-REC 	 TO REC-CREDIT.
008040       GO TO AY50.
008050 AY08.
008060	     MOVE 08		 TO REC-FILE.
008070	     MOVE WS-CTRKEY	 TO REC-KEY.
008080	     MOVE CT-REC	 TO REC-CRTRAN.
008090       GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
008200 AY40.
008210	     MOVE 99		 TO REC-FILE.
008220	     MOVE ZERO		 TO REC-KEY.
008230	     MOVE SPACES	 TO REC-DETAIL.
008240       PERFORM AY50.
008250	     ADD 1		 TO WS-TRANS.
008260     IF WS-RECOVER > 95
008270         CLOSE RECOVER
008280         OPEN OUTPUT RECOVER
008290         CLOSE RECOVER
008300         OPEN I-O RECOVER
008310	       MOVE ZERO	 TO WS-RECOVER.
008320       GO TO AY59.
008330 AY50.
008340	     ADD 1		 TO WS-RECOVER.
008350	     MOVE WS-RECOVER	 TO WS-RECKEY.
008360	     MOVE WS-TRANS	 TO REC-TRAN.
008370       WRITE REC-RECORD.
008380     IF WS-STATUS NOT = "00"
008390         DISPLAY "Write error Recovery file - Status " AT 2212
008400		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
008410	       DISPLAY WS-STATUS AT 2247
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
008420         STOP RUN.
008450 AY59.
008460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 4 HIGHLIGHT.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "CR"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT(PAR-PROG(WS-SUB) = SPACES OR
		  PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT(PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT(PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT(PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT(PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
013090	     COMMIT.

011860 AY999.
011870       EXIT.
      /
011010 CA155-GET-CREDITOR      SECTION.
011020 CA155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150	       MOVE 1		 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180	       MOVE 2		 TO WS-ERROR
011190     ELSE
011200	       MOVE 0		 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.
      /
036530 HA000	     SECTION 5.
036540 HA00.
036550       DISPLAY CLR-SCREEN.
	     ADD 1 W05-MM	 GIVING WS-S8.
           IF WS-S8 > 12
               MOVE 1            TO WS-S8.
036560	     DISPLAY S12.
	     PERFORM SAVE-SCREEN-3.
036570       DISPLAY S39.

036580 HA05.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
036590	       ACCEPT WS-OPTION AT 0942
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
036670	   IF WS-OPTION = "N"
036680         GO TO HA999.
034160	     DISPLAY "Have BACKUP copies of files been made (Y/N)"
		      AT 1112.

       HA10.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
034180	       ACCEPT WS-OPTION AT 1156
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
034240	   IF WS-OPTION = "N"
034250	       GO TO HA999.
034260	     DISPLAY "Have Remittance Advices been printed (Y/N)"
		      AT 1312.

       HA15.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
034270	       ACCEPT WS-OPTION AT 1355
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
034330	   IF WS-OPTION = "N"
034340	       GO TO HA999.
034350	     DISPLAY "Are you sure (Y/N)" AT 1512.

       HA20.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
034360	       ACCEPT WS-OPTION AT 1531
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
034420	   IF WS-OPTION = "N"
034430	       GO TO HA999.
	     DISPLAY "Is this a restart (Y/N)" AT 1712.
	     MOVE "N"		 TO WS-REST.

       HA22.
	     PERFORM TEST AFTER
		     UNTIL WS-REST = "N" OR "Y"
034460	       ACCEPT WS-REST AT 1736
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-REST
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF WS-REST = "Y"
	       PERFORM HA90 THRU HA100.
	     MOVE ZERO		 TO W70-DUE-NOW W70-DUE-30
                                    W70-DUE-150 W70-DUE-60 
                                    W70-DUE-180 W70-DUE-90 
                                    W70-DUE-120 W70-OPEN  
                                    W70-CRED    W70-CLOSE 
                                    W70-DEBT  
036690	     PERFORM AY60 THRU AY999.
036700	     MOVE "  !" 	 TO P-NUMBER.
036710	     DISPLAY "Account :" AT 1920.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
	       GO TO HA40.

036720 HA25.
036730       PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
	       GO TO HA40.
036770	     DISPLAY P-NUMBER AT 1930
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " " P-NAME
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	   IF WS-REST = "Y"
	       IF P-NUMBER < W15-ACNO
		   ADD P-BAL	 TO W70-OPEN
				    W70-CLOSE
		   ADD P-30D	 TO W70-DUE-30
		   ADD P-60D	 TO W70-DUE-60
		   ADD P-90D	 TO W70-DUE-90
		   ADD P-120D	 TO W70-DUE-120
		   ADD P-150D	 TO W70-DUE-150
		   ADD P-OUT	 TO W70-DUE-NOW
		   GO TO HA35
	       ELSE
		   MOVE "N"	 TO WS-REST.
      *    IF OPEN-ITEM
      *        GO TO HA70.
036820	     ADD P-30D		 TO P-OUT.
036830       MOVE P-60D          TO P-30D.
036840       MOVE P-90D          TO P-60D.
036850       MOVE P-120D         TO P-90D.
036860       MOVE P-150D         TO P-120D.
036870       MOVE P-180D         TO P-150D.
036880	     MOVE ZERO		 TO P-180D P-DT P-CR.
036890       MOVE P-BAL          TO P-PBAL.
	     COMPUTE P-OUT = P-BAL - (P-30D  + P-60D  + P-90D +
				      P-120D + P-150D + P-180D).
	   IF P-OUT < ZERO
	       ADD P-OUT	 TO P-30D
	       MOVE ZERO	 TO P-OUT.
	   IF P-30D < ZERO
	       ADD P-30D	 TO P-60D
	       MOVE ZERO	 TO P-30D.
	   IF P-60D < ZERO
	       ADD P-60D	 TO P-90D
	       MOVE ZERO	 TO P-60D.
	   IF P-90D < ZERO
	       ADD P-90D	 TO P-120D
	       MOVE ZERO	 TO P-90D.
	   IF P-120D < ZERO
	       ADD P-120D	 TO P-150D
	       MOVE ZERO	 TO P-120D.
	   IF P-150D < ZERO
	       ADD P-150D	 TO P-180D
	       MOVE ZERO	 TO P-150D.
	   IF P-180D < ZERO
	       ADD P-180D	 TO P-OUT
	       MOVE ZERO	 TO P-180D.
	     ADD P-BAL		 TO W70-OPEN
                                    W70-CLOSE.
             ADD P-30D           TO W70-DUE-30.
             ADD P-60D           TO W70-DUE-60.
             ADD P-90D           TO W70-DUE-90.
             ADD P-120D          TO W70-DUE-120.
             ADD P-150D          TO W70-DUE-150.
             ADD P-OUT           TO W70-DUE-NOW.
	     MOVE P-NUMBER	 TO CT-CRED.
	     MOVE ZERO		 TO CT-CRED
				    CT-SDTE
				    CT-CODE
				    CT-SEQ.
	     MOVE SPACES	 TO CT-SINV.
	     PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
	   IF WS-F-ERROR = 4
	       GO TO HA35.

       HA30.
018220	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
	   IF (WS-F-ERROR = 4) OR
	      (CT-CRED NOT = P-NUMBER)
	       GO TO HA35.
	     PERFORM DELETE-CRTRAN-REC THRU WRITE-CRTRAN-EXIT.
	     GO TO HA30.

       HA35.
036930       PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
	     GO TO HA25.

036940 HA40.
	     PERFORM RESTORE-SCREEN-3.
      *       MOVE 1201 	  TO CPOS.
036990*       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
035330	     DISPLAY "Transaction file will now be rebuilt" AT 0412.
	     DISPLAY "NB" AT 0612
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     " - ALL other workstations must EXIT from ".
	     DISPLAY "CREDITORS before continuing " AT 0812
		     "C" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "ontinue [ ]".

       HA45.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "C"
	       ACCEPT WS-OPTION AT 0850
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
037050	     CLOSE CTRANS.
	     DISPLAY "Reading :" AT 1012.
             DISPLAY "Writing :" AT 1212.
      *
      *    ****    R E N A M E	 C R T R A N   F I L E
      *
004990	     CALL "_MSRENAME" USING BY REFERENCE W02-CRTRAN
005000				    BY CONTENT LENGTH OF W02-CRTRAN
004990				    BY REFERENCE W02-CRTOLD
005000				    BY CONTENT LENGTH OF W02-CRTOLD
005030                              BY REFERENCE WS-STATUS.

       HA50.
037060	     OPEN INPUT CRTOLD.
	   IF WS-STAT1 = "9"
	       IF FLE-LOCKED
		   DISPLAY "Waiting" AT 2562
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT BLINK
		   PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
		   GO TO HA50.
	   IF NOT(WS-STATUS = "00")
	       MOVE 4		 TO WS-F-ERROR
	       PERFORM OPEN-ERROR.
	     DISPLAY CLEAR-L25.

       HA55.
037080	     OPEN OUTPUT CTRANS.
	   IF WS-STAT1 = "9"
	       IF FLE-LOCKED
		   DISPLAY "Waiting" AT 2562
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT BLINK
		   PERFORM LOCK-REC-LOOP THRU LOCK-REC-EXIT
		   GO TO HA50.
	   IF NOT(WS-STATUS = "00")
	       MOVE 4		 TO WS-F-ERROR
	       PERFORM OPEN-ERROR.
	     DISPLAY CLEAR-L25.

       HA60.
	     READ CRTOLD
		  AT END
		  GO TO HA65.
	     DISPLAY CTO-CRED AT 1022
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
		     CTO-SDTE
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
		     CTO-CODE
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT " "
		     CTO-SINV
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
011870	     MOVE CTO-REC	 TO CT-REC.
	     DISPLAY CT-CRED AT 1222
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
		     CT-SDTE
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
		     CT-CODE
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
		     CT-SINV
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
002310	     PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
           IF WS-STAT1 NOT = "0"
	       MOVE 4		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.
	     GO TO HA60.

       HA65.
	     CLOSE CTRANS
		   CRTOLD.
      *
      *    ****    D E L E T E	 O L D	 C R T R A N   F I L E
      *
004990	     CALL "_MSREMOVE" USING BY REFERENCE W02-CRTOLD
005000				    BY CONTENT LENGTH OF W02-CRTOLD
005030                              BY REFERENCE WS-STATUS.
035380	     OPEN I-O CTRANS.
      *
      *    ****    End Recovery Logging.
      *
037140	     PERFORM AY70 THRU AY999.
	     GO TO HA999.

      *HA70.
      *      DISPLAY "Transaction :" AT 1912.
      *	     MOVE ZERO		 TO P-OUT
      *				    P-30D
      *				    P-60D
      *				    P-90D
      *				    P-120D
      *				    P-150D
      *				    P-180D.
      *	     MOVE P-NUMBER	 TO CT-CRED.
      *	     MOVE ZERO		 TO CT-CRED
      *				    CT-SDTE
      *				    CT-CODE
      *				    CT-SEQ.
      *	     MOVE SPACES	 TO CT-SINV.
      *	     PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
      *	   IF WS-F-ERROR = 4
      *	       GO TO HA********

      *HA75.
      *	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
      *	   IF (WS-F-ERROR = 4) OR
      *	      (CT-CRED NOT = P-NUMBER)
      *	       GO TO HA*******
      *	     DISPLAY CT-CRED AT 1926
      *		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *		     CT-SDTE
      *		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *		     CT-CODE
      *		     WITH FOREGROUND-COLOR 6 HIGHLIGHT " "
      *		     CT-SINV
      *		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      *	   IF CT-COST

       HA90.
	     DISPLAY "Restart at A/C No." AT 2012.
	     MOVE SPACES	 TO W15-ACNO.

       HA95.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT W15-ACNO AT 2031
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO HA100
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50-LOOKUP
		 WHEN OTHER
		     CALL X"E5"
		     GO TO HA95
	       END-EVALUATE
016570	       DISPLAY W15-ACNO AT 2031
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
016580	       IF W15-ACNO = SPACES
016590		   GO TO HA95.
	   IF W15-ACNO = SPACES
	       GO TO HA100.
	     DISPLAY CLEAR-L25.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
013040       PERFORM CA155-GET-CREDITOR.
013050     IF WS-ERROR NOT = ZERO
	       GO TO HA95.
014380	     DISPLAY P-NAME AT 2038
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       HA96.
032920	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "C" OR "R".
	   IF WS-OPTION = "R"
	       GO TO HA95.

       HA100.
	     EXIT.

037170 HA999.
037180       EXIT.
      /
019640 ZA000-INIT    SECTION 8.
019650 ZA000-START.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019660	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTOLD.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
019960       MOVE 1              TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980       MOVE PAR-COMPANY    TO W95-COMP.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
	     MOVE 2		 TO WS-NETKEY.
020030       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
020100       GO TO ZA999-EXIT.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCR(WS-S1) WS-BCR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1)
				    WS-TCH(WS-S1) WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
021930	     MOVE WS-G2 	 TO WS-TCR(8) WS-TCR(50)
021940				    WS-TCR(66).
021950	     MOVE WS-G3 	 TO WS-MCR(8)  WS-MCR(50)
021960				    WS-MCR(66) WS-MCR(1)
				    WS-MCR(80).
021970	     MOVE WS-G4 	 TO WS-BCR(8) WS-BCR(50)
021980				    WS-BCR(66).
051190	     MOVE WS-G9 	 TO WS-TCH(1).
051200	     MOVE WS-G10	 TO WS-TCH(80).
051210	     MOVE WS-G11	 TO WS-BCH(1).
051220	     MOVE WS-G12	 TO WS-BCH(80).
051230	     MOVE WS-G14	 TO WS-TCR(1)  WS-BCR(1).
051260	     MOVE WS-G13	 TO WS-TCR(80) WS-BCR(80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
020110 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

020190 ZA999-EXIT.
020200       EXIT.
      /
020210 ZB000-I-O-ERROR SECTION 9.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660	       MOVE W02-CREDIT	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680	   ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE WS-CTRKEY	 TO WS-KEY
           ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
