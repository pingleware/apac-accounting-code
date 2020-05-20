      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **    **  **   **	  **   *
      *   **	    **	  **  **    **	**    **  **	**	 **    *
      *   **	    ******    *******	**    **  **	**     **      *
      *   **	    **	 **   **	**    **  **	**   **        *
      *   **	**  **	  **  **	 **  **    **  **   **	       *
      *    ******   **	  **  **	  ****	    ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   C R E D I T O R   A M E N D	A C C O U N T	D E T A I L S  *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP002.
000030 AUTHOR.		       J.W.LEMMON (CSS).
000040 DATE-WRITTEN.	       JUNE, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.co.za.

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

000160 COPY CREDIT.SL.

000140 COPY CRDMEM.SL.

000140 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000140 COPY CRDMEM.FDE.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000410 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000450 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
000470 77  WS-RECKEY       PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000480 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
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
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
000630 77  WS-ENT          PIC  X(08).
000640 77  WS-ENT1         PIC  X(08).
000650 77  WS-KEYSTR       PIC  9(04)    COMP-5.
000660 77  WS-OPTION       PIC  X(01).
       77  WS-NUM          PIC  Z9.
000690 77  WS-KEY1         PIC  9(04)    COMP-5.
000700 77  WS-CONREC       PIC  9(01) VALUE ZERO.
000710 77  WS-INDS         PIC  9(01) VALUE 0.
000720 77  WS-INDC         PIC  9(01) VALUE 0.
000750 77  WS-SKIP         PIC  X(01) VALUE " ".
000750 77  WS-CHAR1        PIC  X(01).
000760 77  WS-LIN          PIC  9(02) VALUE ZERO.
000770 77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
000780 77  WS-PAGE         PIC  9(04)    COMP-5.
000790 77  WS-SEASON       PIC  X(01).
000840 77  WS-ER1          PIC  X(23) VALUE
000850                     "Address > 57 characters".
000930 77  WS-ER8          PIC  X(09) VALUE "No record".
000940 77  WS-ER11         PIC  X(15) VALUE "Data incomplete".
000950 77  WS-H1           PIC  X(05) VALUE "DATE:".
000960 77  WS-H3           PIC  X(05) VALUE "PAGE:".
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
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

000980 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "002".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

000140 COPY CRDMEM.ID.

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

002840 01  S05.
	   03  LINE  5 COLUMN 13 VALUE "Account number ".
002850	   03  LINE  6 COLUMN 13 VALUE "Creditor name  ".
002860	   03  LINE  7 COLUMN 13 VALUE "Address line 1 ".
002870	   03  LINE  8 COLUMN 13 VALUE "        line 2 ".
002880	   03  LINE  9 COLUMN 13 VALUE "        line 3 ".
002890	   03  LINE 10 COLUMN 13 VALUE "        line 4 ".
002900	   03  LINE 11 COLUMN 13 VALUE "Postal code    ".
	   03  LINE 12 COLUMN 13 VALUE "Contact        ".
	   03  LINE 13 COLUMN 13 VALUE "Telephone      ".
	   03  LINE 14 COLUMN 13 VALUE "Fax number     ".
002910	   03  LINE 15 COLUMN 13 VALUE "Terms       30 ".
002930	   03  LINE 16 COLUMN 13 VALUE "            60 ".
002940	   03  LINE 17 COLUMN 13 VALUE "            90 ".
002950	   03  LINE 18 COLUMN 13 VALUE "           120 ".
002960	   03  LINE 19 COLUMN 13 VALUE "           150 ".
002970	   03  LINE 20 COLUMN 13 VALUE "           180 ".
002910	   03  LINE 21 COLUMN 13 VALUE "Payment        ".
	   03	       COLUMN 42 VALUE "30D Disc".
	   03	       COLUMN 58 VALUE "60D Disc".
	   03  LINE 22 COLUMN 13 VALUE "VAT code       ".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "I".
           03          COLUMN 32 VALUE "n/".
	   03	       COLUMN 34 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "E".
           03          COLUMN 35 VALUE "xclusive/".
	   03	       COLUMN 44 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "N".
           03          COLUMN 45 VALUE "o VAT".
      *	   03  LINE 23 COLUMN 13 VALUE "Account Type   "
      *		       COLUMN 31 HIGHLIGHT VALUE "O".
      *	   03	       COLUMN 32 VALUE "pen item/".
      *	   03	       COLUMN 41 HIGHLIGHT VALUE "B".
      *	   03	       COLUMN 42 VALUE "al forward".

002980 01  S06.
002990     02  S06A.
	   03  LINE  5 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W15-ACNO AUTO.
003020     02  S06B.
003000	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003010                           PIC X(40) USING W15-NAME AUTO.
003030	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003040                           PIC X(30) USING W15-ADDR1 AUTO.
003050	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003060                           PIC X(30) USING W15-ADDR2 AUTO.
003070	   03  LINE  9 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003080                           PIC X(30) USING W15-ADDR3 AUTO.
003090	   03  LINE 10 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003100                           PIC X(30) USING W15-ADDR4 AUTO.
003110	   03  LINE 11 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003120				 PIC 9(04) USING W15-PCODE AUTO.
	   03  LINE 12 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-CONTACT AUTO.
	   03  LINE 13 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING W15-TEL AUTO.
	   03  LINE 14 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING W15-FAX AUTO.
003130	   03  LINE 15 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140				 PIC 9(01) USING W15-30 AUTO.
003150	   03  LINE 16 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003160                           PIC 9(01) USING W15-60 AUTO.
003170	   03  LINE 17 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003180                           PIC 9(01) USING W15-90 AUTO.
003190	   03  LINE 18 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003200                           PIC 9(01) USING W15-120 AUTO.
003210	   03  LINE 19 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003220                           PIC 9(01) USING W15-150 AUTO.
003230	   03  LINE 20 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003240                           PIC 9(01) USING W15-180 AUTO.
003130	   03  LINE 21 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140                           PIC X(12) USING W15-PMNT AUTO.
	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140				 PIC Z9.999 USING W15-DISC1 AUTO.
	   03	       COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140				 PIC Z9.999 USING W15-DISC2 AUTO.
	   03  LINE 22 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(01) USING W15-TAX AUTO.
      *	   03  LINE 23 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
      *				 BACKGROUND-COLOR 5
      *				 PIC X(01) USING W15-STM AUTO.

003250 01  S07.
003260	   03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
003270                           VALUE "AMEND CREDITOR RECORD".

       COPY CRDLUP.CRT.

004870 COPY S99.CRT.

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

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
003240	   IF LS0-CRLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
004940	     PERFORM CC000.
005260	     CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
011050	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM CREDITOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

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

       COPY LOCKED.REC.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      ERASE SCREEN FROM SPECIFIED LOCATION	       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
      *    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
      *    ³ The screen is cleared with Column 1 and 80 containing "³" ³
      *    ³ and columns 2 to 79 containing spaces.		       ³
      *    ³  eg.						       ³
      *    ³	  MOVE 0801	 TO CPOS.			       ³
      *    ³	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY CLEAR.CRT.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

000140 COPY CRDMEM.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007140 AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY PARAM.WR.

       COPY CREDIT.LUP.

       COPY APAC.HLP.

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
008000 AY07.
008010       MOVE 07 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
008030	     MOVE P-REC 	 TO REC-CREDIT.
008040       GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE
	     MOVE NET-RECORD	 TO REC-NETWORK.
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
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.
      /
      *    ****    A M E N D   C R E D I T O R   D E T A I L S
      *
012940 CC000	     SECTION 5.
012950 CC00.
012960       DISPLAY CLR-SCREEN.
012970       DISPLAY S07.
013080       DISPLAY S05.
013000       MOVE SPACES         TO W15-ACNO.

012990 CC02.
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
013010       ACCEPT S06A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO CC999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CC02
	       END-EVALUATE
	       DISPLAY S06A
	       IF W15-ACNO = SPACES
015990		   GO TO CC02.
	     DISPLAY CLEAR-L25.
013020     IF W15-ACNO = SPACES
013030         GO TO CC999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
013040	     PERFORM CA155-GET-CREDITOR.
013050     IF WS-ERROR NOT = ZERO
013060	       MOVE WS-ER8	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013070         GO TO CC02.

013090 CC005.
013110       MOVE P-NAME         TO W15-NAME.
013120       MOVE P-ADDR         TO W40-ADD.
             MOVE P-CONTACT      TO W15-CONTACT.
             MOVE P-TEL          TO W15-TEL.
             MOVE P-FAX          TO W15-FAX.
013130       MOVE P-30           TO W15-30.
013140       MOVE P-60           TO W15-60.
013150       MOVE P-90           TO W15-90.
013160       MOVE P-120          TO W15-120.
013170       MOVE P-150          TO W15-150.
013180       MOVE P-180          TO W15-180.
013190       MOVE P-PC           TO W15-PCODE.
             MOVE P-TAX          TO W15-TAX.
             MOVE P-PMNT         TO W15-PMNT.
	     MOVE P-30-DISC	 TO W15-DISC1.
	     MOVE P-60-DISC	 TO W15-DISC2.
	     MOVE P-STM 	 TO W15-STM.
015150	     UNSTRING W40-ADD DELIMITED BY "*" INTO
015160         W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4
015170	       OVERFLOW MOVE 1	 TO WS-ERROR.

013230 CC010.
013240       ACCEPT S06B.
	     PERFORM CHECK-CORRECT.
013320	   IF WS-OPTION = "N"
013330         GO TO CC010.
013340	     DISPLAY CLEAR-L25.
013350     IF W15-NAME = SPACES
013360	       MOVE WS-ER11	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013370         GO TO CC010.
013380     IF W15-ADDRESS = SPACES 
013390	       MOVE WS-ER11	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013400         GO TO CC010.
013410       PERFORM CE000.
013420     IF WS-ERROR > ZERO
013430	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013440         GO TO CC010.
	     CALL "CBL_TOUPPER" USING W15-TAX
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT(W15-TAX = "E" OR "I" OR "N")
	       MOVE "VAT code must be E, I or N"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CC010.
      *	   IF NOT(W15-STM = "O" OR "B")
      *	       MOVE "Account type must be O, or B"
      *				 TO WS-ERR-STRING
      *	       GO TO CC010.
013450	     PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
013460       PERFORM AY07 THRU AY59.
012580       MOVE W15-NAME       TO P-NAME.
             MOVE W15-TEL        TO P-TEL.
             MOVE W15-FAX        TO P-FAX.
             MOVE W15-CONTACT    TO P-CONTACT.
012600     IF W15-30 > 0
012610	       MOVE 1		 TO W15-30.
012620     IF W15-60 > 0
012630	       MOVE 1		 TO W15-60.
012640     IF W15-90 > 0
012650	       MOVE 1		 TO W15-90.
012660     IF W15-120 > 0
012670	       MOVE 1		 TO W15-120.
012680     IF W15-150 > 0
012690	       MOVE 1		 TO W15-150.
012700     IF W15-180 > 0
012710	       MOVE 1		 TO W15-180.
	     MOVE W15-30	 TO P-30.
	     MOVE W15-60	 TO P-60.
	     MOVE W15-90	 TO P-90.
	     MOVE W15-120	 TO P-120.
	     MOVE W15-150	 TO P-150.
	     MOVE W15-180	 TO P-180.
012720       MOVE W40-ADD        TO P-ADDR.
012730       MOVE W15-PCODE      TO P-PC.
             MOVE W15-TAX        TO P-TAX.
             MOVE W15-PMNT       TO P-PMNT.
	     MOVE W15-STM	 TO P-STM.
	     MOVE W15-DISC1	 TO P-30-DISC.
	     MOVE W15-DISC2	 TO P-60-DISC.
013780	     PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
013790       PERFORM AY70 THRU AY999.
013800       GO TO CC00.

013810 CC999.
013820       EXIT.
      /
      *    ****    P A C K   A D D R E S S
      *
014670 CE000	     SECTION 5.
014680 CE00.
014690       MOVE ZERO TO WS-ERROR.
014700       MOVE 1 TO WS-S1 WS-S2.
014710       MOVE 30 TO WS-S3.
014720       MOVE SPACES TO W40-ADD.

014730 CE05.
014740	   IF W15-ACHAR(WS-S3) NOT = SPACE
014750         GO TO CE10.
014760     IF WS-S3 > WS-S2
014770         SUBTRACT 1 FROM WS-S3
014780         GO TO CE05.

014790 CE10.
014800	     MOVE W15-ACHAR(WS-S2) TO W40-ACHAR(WS-S1).
014810     IF WS-S2 = WS-S3
014820         GO TO CE15.
014830       ADD 1 TO WS-S1.
014840       ADD 1 TO WS-S2.
014850     IF WS-S1 > 70
014860         MOVE 1 TO WS-ERROR
014870     ELSE
014880         GO TO CE10.

014890 CE15.
014900     IF WS-ERROR = 1
014910         GO TO CE20.
014920     IF WS-S1 < 70
014930         ADD 1 TO WS-S1
014940	       MOVE "*" TO W40-ACHAR(WS-S1).

014950 CE20.
014960       ADD 1 TO WS-S1.

014970 CE25.
014980       MOVE 31 TO WS-S2.
014990       MOVE 60 TO WS-S3.
015000       PERFORM CE05 THRU CE20.
015010     IF WS-ERROR = 1
015020         GO TO CE999.
015030       MOVE 61 TO WS-S2.
015040       MOVE 90 TO WS-S3.
015050       PERFORM CE05 THRU CE20.
015060     IF WS-ERROR = 1
015070         GO TO CE999.
015080       MOVE 91 TO WS-S2.
015090       MOVE 120 TO WS-S3.
015100       PERFORM CE05 THRU CE20.

015110 CE999.
015120       EXIT.

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
020020       MOVE 2              TO WS-NETKEY.
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
021930	     MOVE WS-G2 	 TO WS-TCR(8)  WS-TCR(50)
021940				    WS-TCR(66).
021950	     MOVE WS-G3 	 TO WS-MCR(8)  WS-MCR(50)
021960				    WS-MCR(66) WS-MCR(1)
				    WS-MCR(80).
021970	     MOVE WS-G4 	 TO WS-BCR(8)  WS-BCR(50)
021980				    WS-BCR(66).
051190	     MOVE WS-G9 	 TO WS-TCH(1).
051200	     MOVE WS-G10	 TO WS-TCH(80).
051210	     MOVE WS-G11	 TO WS-BCH(1).
051220	     MOVE WS-G12	 TO WS-BCH(80).
051230	     MOVE WS-G14	 TO WS-TCR(1)  WS-BCR(1).
051260	     MOVE WS-G13	 TO WS-TCR(80) WS-BCR(80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

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
041840	   ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
