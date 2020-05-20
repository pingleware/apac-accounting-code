      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******    ******    *
      *   **	**  **	  **  **    **	 **  **   **	**  **	  **   *
      *   **	    **	  **  **    **	**    **  **	**	 **    *
      *   **	    ******    *******	**    **   *******     **      *
      *   **	    **	 **   **	**    **	**   **        *
      *   **	**  **	  **  **	 **  **   **	**  **	       *
      *    ******   **	  **  **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   P A T C H   A G I N G   P R O G R A M    *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP092.
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

000160 COPY CREDIT.SL.

000180 COPY CRTRAN.SL.

000140 COPY CRDMEM.SL.

       COPY PARAM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000180 COPY CRTRAN.FDE.

000140 COPY CRDMEM.FDE.

000290 COPY PARAM.FDE.

000410 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000450 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
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
000650 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
000660 77  WS-OPTION       PIC  X(01).
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
000930 77  WS-ER8	   PIC	X(09) VALUE "No record".
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
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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
	   03  WS-PROG	       PIC  X(03) VALUE "092".

002250 01  W00-TRAN-CODES.
           03  W00-C-ABR   PIC  X(03).
001410     03  W00-C-DESC  PIC  X(15).
001430     03  W00-C-ACT   PIC  9(01).
001440     03  W00-C-CODE  PIC  9(02)    COMP-5.
001450     03  W00-C-GL    PIC  9(06)    COMP-3.
001460     03  W00-C-DAY   PIC  9(06)    COMP-5.
001470     03  W00-C-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-C-MTD   PIC  9(06)    COMP-5.
001490     03  W00-C-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-C-YTD   PIC  9(06)    COMP-5.
001510     03  W00-C-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

000180 COPY CRTRAN.ID.

000140 COPY CRDMEM.ID.

       COPY PARAM.ID.

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

001910 01  W25-VALUES.
005610	   03  W25-CFLOW     PIC S9(09)V99 COMP-3 OCCURS 6.
	   03  W25-OUT	     PIC S9(09)V99 COMP-3.
	   03  W25-30D	     PIC S9(09)V99 COMP-3.
	   03  W25-60D	     PIC S9(09)V99 COMP-3.
	   03  W25-90D	     PIC S9(09)V99 COMP-3.
	   03  W25-120D      PIC S9(09)V99 COMP-3.
	   03  W25-150D	     PIC S9(09)V99 COMP-3.
	   03  W25-180D	     PIC S9(09)V99 COMP-3.
	   03  W25-BAL	     PIC S9(09)V99 COMP-3.
001930	   03  W25-V1	     PIC S9(09)V99 COMP-3.
001940	   03  W25-V2	     PIC S9(09)V99 COMP-3.

005090 01  W25-VALUES-2.
005100     03  W25-CFLW2 OCCURS 6 PIC S9(09)V99  COMP-3.
005110     03  W25-V1-2        PIC S9(09)V99 COMP-3.
005120     03  W25-V2-2        PIC S9(09)V99 COMP-3.

001950 01  W25-EDIT.
001960     03  W25-5.
001970         05  W25-1     PIC  ZZZZ9.
001980     03  W25-3V2.
001990         05  W25-2     PIC  ZZ9.99.
002000     03  W25-7V2.
002010         05  W25-3     PIC  Z(06)9.99.
005320	   03  W25-S7V2.
005330         05  W25-VAL2    PIC Z(06)9.99DB.
002040	   03  W25-3N.
002050         05  W25-4     PIC  ZZ9.
002060     03  W25-S5V2.
002070         05  W25-CVAL  PIC  Z(06)9.99-.
002080     03  W25-DTE.
002090	       05  W25-DATE  PIC  Z9/99/9999.
002100     03  W25-DEL.
002110         05  W25-2N    PIC  9(02).
002120	   03  W25-8.
002130         05  W25-NO    PIC  Z(07)9.

005360 01  W30-AGE.
005370     03  W30-CUR         PIC S9(09)V99 COMP-3.
005380     03  W30-30          PIC S9(09)V99 COMP-3.
005390     03  W30-60          PIC S9(09)V99 COMP-3.
005400     03  W30-90          PIC S9(09)V99 COMP-3.
005410     03  W30-120         PIC S9(09)V99 COMP-3.
005420     03  W30-150         PIC S9(09)V99 COMP-3.
005430     03  W30-180         PIC S9(09)V99 COMP-3.

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

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.

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

007450 01  S19.
007460	   03  FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 5 PIC X(06) USING W15-ACNO AUTO.

003640 01  S54.
003650	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
003660				 VALUE "CREDITOR ENQUIRY".
003670     03  LINE  3 COLUMN  2 VALUE "Ac-No".
003680	   03	       COLUMN 15 VALUE "Name".
	   03  LINE  4 COLUMN 15 VALUE "Contact".
           03          COLUMN 55 VALUE "Telephone".
003690	   03  LINE  5 COLUMN  2 VALUE "Addr".
003700	   03	       COLUMN 39 VALUE "�".
003710     03          COLUMN 41 VALUE "Op-Bal".
003720	   03	       COLUMN 62 VALUE "�".
003730     03          COLUMN 64 VALUE "30:".
003740	   03  LINE  6 COLUMN 39 VALUE "�".
003750     03          COLUMN 41 VALUE "Debits".
003760	   03	       COLUMN 62 VALUE "�".
003770     03          COLUMN 64 VALUE "60:".
003780	   03  LINE  7 COLUMN 39 VALUE "�".
003790     03          COLUMN 41 VALUE "Credits".
003800	   03	       COLUMN 62 VALUE "�".
003810     03          COLUMN 64 VALUE "90:".
003820	   03  LINE  8 COLUMN 39 VALUE "�".
003830     03          COLUMN 41 VALUE "Cl-Bal".
003840	   03	       COLUMN 62 VALUE "�".
003850     03          COLUMN 63 VALUE "120:".
003860     03  LINE  9 COLUMN  2 VALUE "P/C".
003870	   03	       COLUMN 39 VALUE "�".
003890	   03	       COLUMN 62 VALUE "�".
003900     03          COLUMN 63 VALUE "150:".
003910	   03  LINE 10 COLUMN 39 VALUE "�".
003880     03          COLUMN 45 VALUE "Due".
003920	   03	       COLUMN 62 VALUE "�".
003930     03          COLUMN 63 VALUE "180:".
003940     03  LINE 11 COLUMN  2 VALUE 
003950                 "Terms - :".
003960     03  LINE 12 COLUMN  2 VALUE "Transactions:". 
003970     03  LINE 13 COLUMN  4 VALUE "Date".
003980	   03	       COLUMN 14 VALUE "Ref".
003990	   03	       COLUMN 20 VALUE
004000		       "Typ   Inv No   Date     Cost/Value      Selling
004010- 	       " Age 30-180".

       01  S54A.
	   03  LINE  3 COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(40) USING P-NAME.
	   03  LINE  4 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING P-CONTACT.
	   03	       COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING P-TEL.
	   03  LINE  5 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR1.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-PBAL.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-30D.
	   03  LINE  6 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR2.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-DT.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-60D.
	   03  LINE  7 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR3.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-CR.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-90D.
	   03  LINE  8 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-ADDR4.
	   03	       COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-BAL.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-120D.
	   03  LINE  9 COLUMN  8 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 9(04) USING P-PC.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-150D.
	   03  LINE 10 COLUMN 48 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-OUT.
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(07)9.99DB USING P-180D.
	   03  LINE 11 COLUMN 14 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(12) USING W15-PMNT.

       COPY CRDLUP.CRT.

       COPY S99.CRT.

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

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
003240	   IF LS0-CRLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
004940	     PERFORM KA000.
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

000180 COPY CRTRAN.RD.

000140 COPY CRDMEM.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
007140 AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

000180 COPY CRTRAN.WR.

       COPY PARAM.WR.

       COPY CREDIT.LUP.

       COPY APAC.HLP.

      *
      *    G E T   A   C R E D I T O R   T R A N S A C T I O N
      *    R E C O R D
      *
      *    PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE
      *
      *    ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-
      *    WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE
      *    WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE
      *    TRANSACTION CODE SUBSCRIPT IN THE RECORD.
      *
       GET-CREDITOR-TRAN-RECORD SECTION.

       GET-CREDITOR-TRAN.
             ADD 381 W90-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-TRNKEY
                                 REMAINDER W90-TS.
             ADD 1               TO W90-TS.

       GET-CREDITOR-TRAN-CODE.
             MOVE W90-TRNKEY     TO WS-PARKEY.

       STORE-CREDITOR-TRAN-DESC.
	     MOVE PAR-C-ABR(W90-TS)
                                 TO W00-C-ABR.
	     MOVE PAR-C-DESC(W90-TS)
                                 TO W00-C-DESC.
	     MOVE PAR-C-ACT(W90-TS)
                                 TO W00-C-ACT.
	     MOVE PAR-C-CODE(W90-TS)
                                 TO W00-C-CODE.
	     MOVE PAR-C-GL(W90-TS)
                                 TO W00-C-GL.

       STORE-CREDITOR-TRAN-STATS.
	     MOVE PAR-C-DAY(W90-TS)
                                 TO W00-C-DAY.
	     MOVE PAR-C-VAL(W90-TS)
                                 TO W00-C-VAL.
	     MOVE PAR-C-MTD(W90-TS)
                                 TO W00-C-MTD.
	     MOVE PAR-C-MTDV(W90-TS)
                                 TO W00-C-MTDV.
	     MOVE PAR-C-YTD(W90-TS)
                                 TO W00-C-YTD.
	     MOVE PAR-C-YTDV(W90-TS)
                                 TO W00-C-YTDV.

       GET-CREDITOR-TRAN-EXIT.
             EXIT.

       GET-CREDITOR-TRAN-DESC SECTION.

       GET-CRED-TRAN-DESC.
             PERFORM GET-CREDITOR-TRAN.
             MOVE W90-TRNKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-CREDITOR-TRAN-DESC.

       GET-CRED-TRAN-DESC-EXIT.
             EXIT.
      
      /
011010 CA155-GET-CREDITOR      SECTION.
011020 CA155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130	     PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
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
      *    ****    R E A G E   T R A N S A C T I O N
      *
017650 KA000	     SECTION 5.
       KA0.
	     DISPLAY "RE-AGE TRANSACTION" AT 0232
                      WITH FOREGROUND-COLOR 15.
032000	     MOVE 5		 TO WS-PARKEY.
032010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
032020       MOVE PAR-PW         TO W85-PASS. 
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "旼컴컴컴컴컴컴컴컴컴컴컴" AT 1127
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "� " AT 1227 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "�" AT 1327 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "컴컴컴컴컴컴컴컴컴컴컴켸"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1244
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1		 TO WS-LENGTH.
018370     IF NOT (W40-CSHPASS = W85-SUPER (1) OR 
				 W85-SUPER (3) OR
				 "CRTAGE")
	       GO TO KA999.

017660 KA00.
017670       DISPLAY CLR-SCREEN.
017680       DISPLAY S54.
             DISPLAY "RE-AGE TRANSACTION" AT 0232 
                      WITH FOREGROUND-COLOR 15.
             MOVE SPACES         TO W15-ACNO.
017690 KA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
017700	     ACCEPT S19 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO KA06
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO KA05
	       END-EVALUATE
018820	       DISPLAY S19 AT 0308
018830	       IF W15-ACNO = SPACES
018840		   GO TO KA05.

       KA06.
	     DISPLAY CLEAR-L25.
017710     IF W15-ACNO = SPACES
017720         GO TO KA999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
017740	     PERFORM CA155-GET-CREDITOR.
017750     IF WS-ERROR NOT = ZERO
017760	       MOVE WS-ER8	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017770         GO TO KA05.
	     PERFORM KA95.
017790       MOVE P-ADDR         TO W40-ADD.
015150       UNSTRING W40-ADD DELIMITED BY "*" INTO
015160         W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4
015170	       OVERFLOW MOVE 1	 TO WS-ERROR.
             DISPLAY S54A.
           IF P-TAX = "I"
	       DISPLAY "(Inclusive)" AT 1162 WITH FOREGROUND-COLOR 11
           ELSE
           IF P-TAX = "E"
	       DISPLAY "(Exclusive)" AT 1162 WITH FOREGROUND-COLOR 11
           ELSE
	       DISPLAY "(No VAT)" AT 1162 WITH FOREGROUND-COLOR 11.

	     MOVE P-NUMBER	 TO CT-CRED.
	     MOVE ZERO		 TO CT-SDTE
				    CT-CODE
				    CT-SEQ.
	     MOVE SPACES	 TO CT-SINV.
	     PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
	   IF WS-F-ERROR = 4
	       GO TO KA90.
	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
	   IF (WS-F-ERROR = 4) OR
	      (CT-CRED NOT = P-NUMBER)
	       GO TO KA90.

018180 KA10.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
018200       MOVE 14             TO LIN.

018210 KA15.
018230	     MOVE CT-DTE	 TO W20-DTE.
	     MOVE W20-CC	 TO W15-CC.
	     MOVE W20-YY	 TO W15-YY.
	     MOVE W20-MM	 TO W15-MM.
	     MOVE W20-DD	 TO W15-DD.
018240	     DISPLAY W15-DD AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     W15-MM WITH FOREGROUND-COLOR 7
				 BACKGROUND-COLOR 5
		     W15-CCYY WITH FOREGROUND-COLOR 3 HIGHLIGHT
				   BACKGROUND-COLOR 5.
018250	   IF CT-REF NUMERIC
018260	       MOVE CT-REFNO	 TO W25-NO
	   ELSE
	       MOVE CT-REF	 TO W25-8.
018270	     DISPLAY W25-8 AT LINE LIN COLUMN 11
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-DESC.
	     DISPLAY W00-C-ABR AT LINE LIN COLUMN 20
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018470	   IF CT-SINV NUMERIC
018480	       MOVE CT-SINN	 TO W25-NO
	   ELSE
	       MOVE CT-SINV	 TO W25-8.
018490	     DISPLAY W25-8 AT LINE LIN COLUMN 24
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018500     IF CT-SDTE NOT = ZERO
018510	       MOVE CT-SDTE	 TO W20-DTE
	       MOVE W20-CC	 TO W15-CC
	       MOVE W20-YY	 TO W15-YY
	       MOVE W20-MM	 TO W15-MM
	       MOVE W20-DD	 TO W15-DD
018520	       DISPLAY W15-DD AT LINE LIN COLUMN 33
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
		       W15-MM WITH FOREGROUND-COLOR 7
				   BACKGROUND-COLOR 5
		       W15-CCYY WITH FOREGROUND-COLOR 3 HIGHLIGHT
				     BACKGROUND-COLOR 5.
018530       MOVE CT-COST        TO W25-VAL2.
018540	     DISPLAY W25-S7V2 AT LINE LIN COLUMN 43
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018550     IF CT-SELL NOT = ZERO
018560         MOVE CT-SELL      TO W25-VAL2
018570	       DISPLAY W25-S7V2 AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
018580     IF CT-AGE = ZERO
018590         GO TO KA18.
018600       MOVE ZERO           TO W15-SETT.
018610       MOVE CT-AGE         TO WS-S4.
018620     IF WS-S4 > 31
018630         MOVE 1            TO W15-180
018640         SUBTRACT 32       FROM WS-S4.
018650     IF WS-S4 > 15
018660         MOVE 1            TO W15-150
018670         SUBTRACT 16       FROM WS-S4.
018680     IF WS-S4 > 7
018690         MOVE 1            TO W15-120
018700         SUBTRACT 8        FROM WS-S4.
018710     IF WS-S4 > 3
018720         MOVE 1            TO W15-90
018730         SUBTRACT 4        FROM WS-S4.
018740     IF WS-S4 > 1
018750         MOVE 1            TO W15-60
018760         SUBTRACT 2        FROM WS-S4.
018770     IF WS-S4 > 0
018780         MOVE 1            TO W15-30.

       KA18.
018790	     DISPLAY W15-30 AT LINE LIN COLUMN 69
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018800	     DISPLAY W15-60 AT LINE LIN COLUMN 71
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018810	     DISPLAY W15-90 AT LINE LIN COLUMN 73
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018820	     DISPLAY W15-120 AT LINE LIN COLUMN 75
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018830	     DISPLAY W15-150 AT LINE LIN COLUMN 77
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018840	     DISPLAY W15-180 AT LINE LIN COLUMN 79
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       KA20.
	     DISPLAY "A" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "mend / "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "N"            TO WS-OPTION.

       KA25.
             ACCEPT WS-OPTION AT 2526 
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO KA00
		 WHEN OTHER
		     CALL X"E5"
		     GO TO KA25
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "N")
               GO TO KA25.
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "A"
               GO TO KA30.
	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
	   IF (WS-F-ERROR = 4) OR
	      (CT-CRED NOT = P-NUMBER)
018970	       DISPLAY "**** End **** - ENTER" AT 2512
018980			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018990	       ACCEPT WS-OPTION AT 2534
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
019000         GO TO KA00.
	     DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
             GO TO KA15.

       KA30.
             MOVE CT-COST        TO W20-RESULT.
017890       MOVE 0              TO WS-S.
017900       MOVE 1              TO WS-S2.

017910 KA45.
017920       MOVE ZERO           TO W25-CFLOW (WS-S2).
017930     IF W15-MONTH (WS-S2) > 0
017940         ADD 1 TO WS-S.
017950     IF WS-S2 < 6
017960         ADD 1             TO WS-S2
017970         GO TO KA45.
017980       MOVE ZERO           TO W25-V1 W25-V2.
017990       MOVE 1              TO WS-S3.
018020     IF WS-S = 0 
018030         MOVE W20-RESULT   TO W25-V2
               GO TO KA55.
018020     IF WS-S = 1
018030         MOVE W20-RESULT   TO W25-V2
018040         GO TO KA50.
018050       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
018060       SUBTRACT 1          FROM WS-S.
018070       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
018080       ADD 1               TO WS-S.

018090 KA50.
018100     IF W15-MONTH (WS-S3) = ZERO
018110         ADD 1             TO WS-S3
018120         GO TO KA50.
018130     IF WS-S > 1
018140         MOVE W25-V1       TO W25-CFLOW (WS-S3)
018150         SUBTRACT 1        FROM WS-S
018160         ADD 1             TO WS-S3
018170         GO TO KA50.
018180       MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V2.

018200 KA55.
             EXIT.

       KA58.
             MOVE W25-VALUES     TO W25-VALUES-2.
           IF CT-COST < ZERO
               GO TO KA65.
             MOVE 1              TO WS-S2.

       KA60.
             MULTIPLY -1         BY W25-CFLW2 (WS-S2).
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO KA60.
             MULTIPLY -1         BY W25-V2-2.

       KA65.
018790	     ACCEPT W15-30 AT 1469
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
018800	     ACCEPT W15-60 AT 1471
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
018810	     ACCEPT W15-90 AT 1473
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
018820       ACCEPT W15-120 AT 1475 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
018830       ACCEPT W15-150 AT 1477 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
018840       ACCEPT W15-180 AT 1479 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
012180	     PERFORM CHECK-CORRECT.
020850	   IF WS-OPTION = "N"
020860         GO TO KA65.
	     PERFORM KA30 THRU KA55.
           IF CT-COST > ZERO
               GO TO KA75.
             MOVE 1              TO WS-S2.

       KA70.
             MULTIPLY -1         BY W25-CFLOW (WS-S2).
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO KA70.
             MULTIPLY -1         BY W25-V2.

       KA75.
             PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE 1              TO WS-S2.
021810       ADD W25-CFLOW (6)   TO P-180D.
021810       ADD W25-CFLW2 (6)   TO P-180D.
021850       ADD W25-CFLOW (5)   TO P-150D.
021850       ADD W25-CFLW2 (5)   TO P-150D.
021890       ADD W25-CFLOW (4)   TO P-120D.
021890       ADD W25-CFLW2 (4)   TO P-120D.
021930       ADD W25-CFLOW (3)   TO P-90D.
021930       ADD W25-CFLW2 (3)   TO P-90D.
021970       ADD W25-CFLOW (2)   TO P-60D.
021970       ADD W25-CFLW2 (2)   TO P-60D.
022010       ADD W25-CFLOW (1)   TO P-30D.
022010       ADD W25-CFLW2 (1)   TO P-30D.
022050       ADD W25-V2          TO P-OUT.
022050       ADD W25-V2-2        TO P-OUT.
021630       MOVE 1              TO WS-S WS-S2.
             MOVE ZERO           TO CT-AGE.
021640     IF W25-V2 NOT = ZERO
021650         GO TO KA85.

021660 KA80.
021670     IF W15-MONTH (WS-S) NOT = ZERO
021680         ADD WS-S2         TO CT-AGE.
021690     IF WS-S < 6
021700         ADD 1             TO WS-S
021710         MULTIPLY 2        BY WS-S2
021720         GO TO KA80.

       KA85.
             PERFORM REWRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             DISPLAY S54A.
	     PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
	     PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
             GO TO KA15.

       KA90.
	     MOVE "No transactions on file"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
018160	     GO TO KA00.

       KA95.
             COMPUTE W30-CUR = P-BAL - (P-30D + P-120D + 
                                        P-60D + P-150D +
                                        P-90D + P-180D).
           IF (P-OUT NOT = W30-CUR) OR
              (P-OUT < ZERO)
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT
               MOVE W30-CUR      TO P-OUT
               IF P-OUT < ZERO 
                   ADD P-OUT     TO P-30D
                   MOVE ZERO     TO P-OUT
               END-IF
               IF P-30D < ZERO 
                   ADD P-30D     TO P-60D
                   MOVE ZERO     TO P-30D
               END-IF
               IF P-60D < ZERO 
                   ADD P-60D     TO P-90D
                   MOVE ZERO     TO P-60D
               END-IF
               IF P-90D < ZERO 
                   ADD P-90D     TO P-120D
                   MOVE ZERO     TO P-90D
               END-IF
               IF P-120D < ZERO
                   ADD P-120D    TO P-150D
                   MOVE ZERO     TO P-120D
               END-IF
               IF P-150D < ZERO
                   ADD P-150D    TO P-180D
                   MOVE ZERO     TO P-150D
               END-IF
               IF P-180D < ZERO
                   ADD P-180D    TO P-OUT
                   MOVE ZERO     TO P-180D
               END-IF
               PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.

       KA999.
             EXIT.
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
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
019960	     MOVE 1		 TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980       MOVE PAR-COMPANY    TO W95-COMP.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
	     MOVE 2		 TO WS-NETKEY.
020030       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
020100       GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCR  (8)  WS-TCR (50)
021940                              WS-TCR  (66).
021950       MOVE WS-G3          TO WS-MCR  (8)  WS-MCR (50)
021960                              WS-MCR  (66) WS-MCR (1)
				    WS-MCR  (80).
021970       MOVE WS-G4          TO WS-BCR  (8)  WS-BCR (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR (1).
051260       MOVE WS-G13         TO WS-TCR (80)  WS-BCR (80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2312
020130                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
020190 ZA999-EXIT.
020200       EXIT.
      /
020210 ZB000-I-O-ERROR SECTION 9.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680     ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY
	   ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
