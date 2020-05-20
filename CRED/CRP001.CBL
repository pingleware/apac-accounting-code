      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****       **      *
      *   **    **  **    **  **    **   **  **    **  **     ***      *
      *   **        **    **  **    **  **    **  **    **     **      *
      *   **        ******    *******   **    **  **    **     **      *
      *   **        **   **   **        **    **  **    **     **      *
      *   **    **  **    **  **         **  **    **  **      **      *
      *    ******   **    **  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.                     CRP001.
000030 AUTHOR.                         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.                   JUNE, 1983.

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
000480 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000490 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
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
000840 77  WS-ER1	   PIC	X(23) VALUE
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
	   03  WS-PROG	       PIC  X(03) VALUE "001".

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

       COPY W15.CRP.

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

002810 01  S04.
002820	   03  LINE  3 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
002830                           VALUE "CREATE CREDITOR RECORD".

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
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "I".
           03          COLUMN 32 VALUE "n/".
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "E".
           03          COLUMN 35 VALUE "xclusive/".
	   03	       COLUMN 44 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "N".
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
003240	   IF LS0-CRLEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
004940	     PERFORM CB000.
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

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

000140 COPY CRDMEM.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
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
008010	     MOVE 07		 TO REC-FILE.
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
      *    ****    C R E A T E   N E W   C R E D I T O R
      *
011990 CB000	     SECTION 50.
012000 CB00.
012010       DISPLAY CLR-SCREEN.
012050       DISPLAY S04.
012070 CB005.
012080       DISPLAY S05.
012090       MOVE SPACES         TO W15-ADDRESS
                                    W15-NAME W15-ADDR4  
                                    W15-TEL  W15-CONTACT
                                    W15-FAX  W15-ACNO
                                    W15-PMNT.
012100	     MOVE ZERO		 TO W15-SETT
				    W15-DISC1 W15-DISC2.
012110       MOVE ZERO           TO W15-PCODE.
             MOVE "E"            TO W15-TAX.
	     MOVE "B"		 TO W15-STM.
      *	     MOVE "O"		 TO W15-STM.
	     DISPLAY S06.

012120 CB010.
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
012130	     ACCEPT S06A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO CB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB010
	       END-EVALUATE
	       DISPLAY S06A
015990	       GO TO CB010.
	     DISPLAY CLEAR-L25.
012140     IF W15-ACNO = SPACES
012150         GO TO CB999.
	     DISPLAY CLEAR-L25.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W15-ACNO	 TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 0
	       MOVE "Existing A/C number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CB035.
012160	     ACCEPT S06B.

012170 CB020.
	     PERFORM CHECK-CORRECT.
012250	   IF WS-OPTION = "N"
012260	       GO TO CB010.
012270	   IF W15-NAME = SPACES
012280	       MOVE WS-ER11	 TO WS-ERR-STRING
012290	       GO TO CB035.
012270     IF W15-ADDRESS = SPACES
012280	       MOVE WS-ER11	 TO WS-ERR-STRING
012290	       GO TO CB035.
012300       PERFORM CE000.
012310     IF WS-ERROR > ZERO
012320	       MOVE WS-ER1	 TO WS-ERR-STRING
012330	       GO TO CB035.
012340     IF W15-PCODE = ZERO
012350	       MOVE WS-ER11	 TO WS-ERR-STRING
012360	       GO TO CB035.
	     CALL "CBL_TOUPPER" USING W15-TAX
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT(W15-TAX = "E" OR "I" OR "N")
	       MOVE "VAT code must be E, I or N"
				 TO WS-ERR-STRING
	       GO TO CB035.
      *	   IF NOT(W15-STM = "O" OR "B")
      *	       MOVE "Account type must be O, or B"
      *				 TO WS-ERR-STRING
      *	       GO TO CB035.
010830	     PERFORM AY60 THRU AY999.
             INITIALIZE P-REC.

012560 CB025.
012570       MOVE W15-ACNO       TO P-NUMBER.
012580       MOVE W15-NAME       TO P-NAME.
             MOVE W15-TEL        TO P-TEL.
             MOVE W15-FAX        TO P-FAX.
             MOVE W15-CONTACT    TO P-CONTACT.
012600     IF W15-30 > 0
012610         MOVE 1            TO P-30.
012620     IF W15-60 > 0
012630         MOVE 1            TO P-60.
012640     IF W15-90 > 0
012650         MOVE 1            TO P-90.
012660     IF W15-120 > 0
012670         MOVE 1            TO P-120.
012680     IF W15-150 > 0
012690         MOVE 1            TO P-150.
012700     IF W15-180 > 0
012710         MOVE 1            TO P-180.
012720       MOVE W40-ADD        TO P-ADDR.
012730       MOVE W15-PCODE      TO P-PC.
             MOVE W15-TAX        TO P-TAX.
             MOVE W15-PMNT       TO P-PMNT.
	     MOVE W15-STM	 TO P-STM.
	     MOVE W15-DISC1	 TO P-30-DISC.
	     MOVE W15-DISC2	 TO P-60-DISC.
012750       PERFORM WRITE-CREDIT THRU WRITE-CREDIT-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY07 THRU AY59.
             ADD 1               TO P-ACCOUNTS.
      *
      *    ****    Write links back and unlock PARAM record 17.
      *
012790 CB030.
012830	     PERFORM AY70 THRU AY999.
	     MOVE "ENTER to continue"
				 TO WS-ERR-MES.
012900	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
012910	     GO TO CB00.

       CB035.
003260	     PERFORM ERROR-MESSAGE.
012910	     GO TO CB010.

012920 CB999.
012930       EXIT.
      /
      *    ****    P A C K   A D D R E S S
      *
014670 CE000	     SECTION 50.
014680 CE00.
014690	     MOVE ZERO		 TO WS-ERROR.
014700	     MOVE 1		 TO WS-S1 WS-S2.
014710	     MOVE 30		 TO WS-S3.
014720	     MOVE SPACES	 TO W40-ADD.

014730 CE05.
014740	   IF NOT(W15-ACHAR(WS-S3) = SPACE)
014750         GO TO CE10.
014760     IF WS-S3 > WS-S2
014770	       SUBTRACT 1	 FROM WS-S3
014780         GO TO CE05.

014790 CE10.
014800	     MOVE W15-ACHAR(WS-S2)
				 TO W40-ACHAR(WS-S1).
014810     IF WS-S2 = WS-S3
014820         GO TO CE15.
014830	     ADD 1		 TO WS-S1.
014840	     ADD 1		 TO WS-S2.
014850     IF WS-S1 > 70
014860	       MOVE 1		 TO WS-ERROR
014870     ELSE
014880         GO TO CE10.

014890 CE15.
014900     IF WS-ERROR = 1
014910         GO TO CE20.
014920     IF WS-S1 < 70
014930	       ADD 1		 TO WS-S1
014940	       MOVE "*" 	 TO W40-ACHAR(WS-S1).

014950 CE20.
014960	     ADD 1		 TO WS-S1.

014970 CE25.
014980	     MOVE 31		 TO WS-S2.
014990	     MOVE 60		 TO WS-S3.
015000       PERFORM CE05 THRU CE20.
015010     IF WS-ERROR = 1
015020         GO TO CE999.
015030	     MOVE 61		 TO WS-S2.
015040	     MOVE 90		 TO WS-S3.
015050       PERFORM CE05 THRU CE20.
015060     IF WS-ERROR = 1
015070         GO TO CE999.
015080	     MOVE 91		 TO WS-S2.
015090	     MOVE 120		 TO WS-S3.
015100       PERFORM CE05 THRU CE20.

015110 CE999.
015120       EXIT.

      /
019640 ZA000-INIT    SECTION 80.
019650 ZA000-START.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019660	     DISPLAY CLR-SCREEN.
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
      *    *************************************************************
      *    **** 	C R E D I T O R	  M E M O   F I L E	    ****
      *    *************************************************************
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
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
041250	     MOVE WS-G1 	 TO WS-TCR(WS-S1)  WS-BCR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1)
				    WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260	   IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280	       GO TO ZA60.
021970	     MOVE WS-G4 	 TO WS-BCR(8)  WS-BCR(50)
021980				    WS-BCR(66).
021950	     MOVE WS-G3 	 TO WS-MCR(8)  WS-MCR(50)
021960				    WS-MCR(66) WS-MCR(1)
				    WS-MCR(80).
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
020210 ZB000-I-O-ERROR SECTION 90.
      
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
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
