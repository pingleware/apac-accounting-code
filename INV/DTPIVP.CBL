      $SET LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	********  **	**  *******    *
      *   **	**     **     **    **	   **	  **	**  **	  **   *
      *   **	**     **     **    **	   **	  **	**  **	  **   *
      *   **	**     **     *******	   **	  **	**  *******    *
      *   **	**     **     **	   **	   **  **   **	       *
      *   **	**     **     **	   **	    ****    **	       *
      *   *******      **     **	********     **     **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   P R O F E S S I O N A L   I N V O I C I N G	P R O G R A M  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPIVP.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2008
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
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000190 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000210 COPY DEPART.SL.

000270 COPY PARAM.SL.

000310 COPY SALE.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000480 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000500 COPY DEPART.FDE.

000560 COPY PARAM.FDE.

000600 COPY SALE.FDE.

      /
002130 WORKING-STORAGE SECTION.
002140 77  WS-DISC         PIC  99V99    COMP-3.
002150 77  WS-TDISC        PIC  99V99    COMP-3.
002160 77  WS-CASH-AC      PIC  X(06).
002170 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
002180 77  WS-P1	   PIC	9(04)	 COMP-5.
002180 77  WS-S1           PIC  9(04)    COMP-5.
002190 77  WS-S2           PIC  9(04)    COMP-5.
002200 77  WS-S3           PIC  9(04)    COMP-5.
002210 77  WS-S4           PIC  9(04)    COMP-5.
002220 77  WS-S5           PIC  9(04)    COMP-5.
002230 77  WS-S6           PIC  9(04)    COMP-5.
002240 77  WS-S7           PIC  9(04)    COMP-5.
002250 77  WS-S8           PIC  9(04)    COMP-5.
002260 77  WS-S9           PIC  9(04)    COMP-5.
002270 77  WS-IXS          PIC  9(04)    COMP-5.
002280 77  WS-IXS1         PIC  9(04)    COMP-5.
002290 77  WS-IXS2         PIC  9(04)    COMP-5.
002300 77  WS-IXS3         PIC  9(04)    COMP-5.
002310 77  WS-IXS4         PIC  9(04)    COMP-5.
002320 77  WS-PARKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
002350 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002380 77  WS-SALSTR       PIC  9(04)    COMP-5.
002390 77  WS-SALKEY       PIC  9(04)    COMP-5.
002400 77  WS-NETKEY       PIC  9(04)    COMP-5.
002450 77  WS-GAR          PIC  9(01) VALUE 0.  
       77  WS-NUM          PIC  Z9.
002480 77  WS-START        PIC  9(08).
002490 77  WS-END          PIC  9(08).
002550 77  WS-PAR          PIC  9(01) VALUE 0.
002560 77  WS-INO          PIC  9(02) VALUE 0.
002570 77  WS-LINES        PIC  9(02) VALUE 0.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002600 77  WS-LASTINV      PIC  9(04)    COMP-5.
002630 77  WS-L1           PIC  9(04)    COMP-5.
002750 77  WS-OPTION	   PIC	X(01).
002770 77  WS-SKIP         PIC  X(01) VALUE " ".
002780 77  WS-LOOK         PIC  X(01) VALUE "S".
002790 77  WS-ETYPE        PIC  X(01) VALUE "I".
002820 77  WS-ERROR        PIC  9(01).
002840 77  WS-TAX	   PIC	X(01).
002860 77  WS-COMP         PIC  9(01).
002890 77  WS-HEAD         PIC  X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE          PIC  X(01).
002920 77  WS-PRC          PIC  X(01).
002920 77  WS-COPY         PIC  X(01) VALUE "N".
002920 77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
002950 77  WS-BATCH        PIC  9(05).
002980 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002990 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
003000 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
003010 77  WS-ERR2         PIC  X(09) VALUE "No Record".
003020 77  WS-PER          PIC  Z(02)9.99.
003030 77  WS-CR           PIC  X(01).
003040 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
003040 77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
       01  PRG-NAME.
	   03  FILLER	   PIC	X(08) VALUE "INV\INPF".
	   03  PRG-NUMBER  PIC	9(02) VALUE 01.

003050 01  WS-DB-LINE.
003060     03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
003080     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³   ³       ³
      -        "                                       ³             ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
003200     03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍµ".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍµ".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµ".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍµ".
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

003480 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "INP".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003700 COPY CONTROL.ID.

003740 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

003760 COPY DEPART.ID.

       COPY PARAM.ID.

003840 COPY SALE.ID.

       COPY W05.VAT.

       COPY W10.PRO.

       COPY W12.WS.

004610 01  W15-DISCOUNT.
004620     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W15-EXTRA.
	   03  W15-LEDG        PIC  X(04).

004720 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004770     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004780     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004790     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005610 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).
005690     03  W60-ITM.
005700         05  W60-ICHAR OCCURS 14 PIC X(01).

005830 01  W75-KEYS.
005840     03  W75-S          PIC 9(02) COMP-5.
005850     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860	   03  W75-DNO	      PIC  X(06) OCCURS 18.
	   03  W75-SLEDG      PIC  X(04) OCCURS 18.

005880 01  W75-TITLES.
005890     03  W75-DESCRIP.
005900         05  FILLER      PIC X(28)      VALUE
005910                     "Mr     Mnr    Mrs    Mev    ".
005920         05  FILLER      PIC X(28)      VALUE
005930                     "Miss   Mej    Ms     Ms     ".
005940         05  FILLER      PIC X(28)      VALUE
005950                     "Dr     Dr     Rev    Ds     ".
005960         05  FILLER      PIC X(28)      VALUE
005970                     "Prof   Prof   The HonSy Edel".
005980         05  FILLER      PIC X(28)      VALUE
005990                     "                            ".
006000     03  W75-CODES REDEFINES W75-DESCRIP.
006010         05  W75-TITLE  OCCURS 10.
006020             07  W75-ETITLE PIC X(07).
006030             07  W75-ATITLE PIC X(07).
006040 01  W80-ACCOUNT.
006050     03  W80-ACNO        PIC X(06).
006060     03  W80-CRED REDEFINES W80-ACNO.
006070         05  W80-CNO     PIC 9(05).
006080         05  W80-CAC REDEFINES W80-CNO.
006090             07  W80-CTYPE PIC 9(01).
006100             07  W80-SER   PIC 9(04).
006110         05  FILLER        PIC 9(01).
006120     03  W80-TYPE        PIC 9(01).
006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.
006310 01  W85-BATCHES.
006320         05  W85-BATCH   PIC 9(05).

       COPY W90.DBT.

007010 01  W100-EDIT.
007020     03  W100-LBAL       PIC Z(06)9.99-.
007030	   03  W100-LARR       PIC Z(06)9.99.
007040     03  W100-BAL        PIC Z(08)9.99-.
007050     03  W100-CUR        PIC Z(08)9.99-.
007060     03  W100-30         PIC Z(08)9.99-.
007070     03  W100-60         PIC Z(08)9.99-.
007080     03  W100-90         PIC Z(08)9.99-.
007090     03  W100-120        PIC Z(08)9.99-.
007100     03  W100-INT        PIC Z(08)9.99-.
007110     03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
007020	   03  W100-PRC        PIC Z(06)9.99-.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120     03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
	   03  W100-COST       PIC Z(06)9.999.
	   03  W100-AVRG       PIC Z(06)9.999.
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
007190 SCREEN SECTION.

       COPY BLANK.CRT.

007800 01  S05.
007810	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

007820 01  S06.
007830	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 15 BACKGROUND-COLOR 5
		       PIC X(06) USING W40-CSHPASS AUTO SECURE.

       COPY DEBLUP.CRT.

008990 01  S25.
	   03  LINE  7 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
009000	   03  LINE  8 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "³Addresses:                 ".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Balance  ".
009010	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-BAL.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009030	   03  LINE  9 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 1 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  MTD      ".
009040	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING W100-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009060	   03  LINE 10 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Current  ".
009070	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009090	   03  LINE 11 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   30 Days ".
009100	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-30.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009120	   03  LINE 12 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   60 Days ".
009130	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-60.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009150	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   90 Days ".
009160	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-90.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009180	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 2 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  120 Days ".
009190	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-120.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009210	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Interest ".
009220	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-INT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009240	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Cr Limit ".
009250	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9 USING DEB-CRL.
009090	   03	       COLUMN 69 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "    ³".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Last Pmt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z9/99/9999 USING DEB-PMT.
009090	   03	       COLUMN 70 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   ³".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "       Amt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-LAST-PMT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

009270 01  S26.
009280     03  LINE  2 COLUMN 37 FOREGROUND-COLOR 15 VALUE "INVOICE".
009290     03  LINE  3 COLUMN  2 VALUE "AC No".
009300	   03	       COLUMN 15 VALUE "Name".
	   03	       COLUMN 20 BACKGROUND-COLOR 0
				 PIC  X(40) FROM WS-SPACE.
009310	   03	       COLUMN 61 VALUE "Ord No°".
	   03	       COLUMN 68 BACKGROUND-COLOR 0
				 PIC  X(12) FROM WS-SPACE.
009320	   03  LINE  4 COLUMN  2 VALUE "Assisted".
	   03	       COLUMN 11 BACKGROUND-COLOR 0
				 PIC  X(02) FROM WS-SPACE.
	   03	       COLUMN 61 VALUE "Tel.".
	   03	       COLUMN 66 BACKGROUND-COLOR 0
				 PIC  X(14) FROM WS-SPACE.
009330	   03  LINE  5 COLUMN  2 VALUE "Inv. No".
	   03	       COLUMN 10 BACKGROUND-COLOR 0
				 PIC  X(08) FROM WS-SPACE.
009340	   03	       COLUMN 21 VALUE "Contact".
	   03	       COLUMN 29 BACKGROUND-COLOR 0
				 PIC  X(14) FROM WS-SPACE.
009350	   03	       COLUMN 47 VALUE "Ref No".
	   03	       COLUMN 54 BACKGROUND-COLOR 0
				 PIC  X(09) FROM WS-SPACE.
009360	   03	       COLUMN 65 VALUE "Date".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 PIC  X(10) FROM WS-SPACE.
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO
				 VALUE "No  Qte/Ref Description/Remarks
009380-        "                                         Value".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÑÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÏÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009450	   03  LINE 20 COLUMN  3 VALUE "Sub Total ".
009460*	   03  LINE 21 COLUMN  3 VALUE "Discount  ".
009490     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
009480     03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".
009490 01  S26B.
009500     03  LINE 20 COLUMN 35 VALUE "Payment Received -".
009510	   03	       COLUMN 54 VALUE "Cash       ".
009520     03          COLUMN 66 VALUE ":".
009530	   03  LINE 21 COLUMN 54 VALUE "Cheque     ".
009540     03          COLUMN 66 VALUE ":".
009550	   03  LINE 22 COLUMN 54 VALUE "Credit Card".
009560     03          COLUMN 66 VALUE ":".
009570     03  LINE 23 COLUMN 34 VALUE "-Balance".
009580	   03	       COLUMN 54 VALUE "CHANGE     ".
009590     03          COLUMN 66 VALUE ":".

009630 01  S26-PRC.
009640	   03	       COLUMN 68 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009650				 PIC Z(07)9.99- USING W10-SELL AUTO.

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009690 01  S26-LEDG.
009700	   03  LINE 20 COLUMN 59 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710				 PIC X(04) USING W10-LEDG AUTO.

009870 01  S28.
009880	   03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009890                           PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ú".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "¿".
007450	   03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 VALUE "CLIENT DETAILS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Name    >".
	   03	       COLUMN 65 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Address >".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 PIC  X(36) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "À".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ù".

       01  S31.
	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NAME AUTO.
	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-ADD1 AUTO.
	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-ADD2 AUTO.
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-ADD3 AUTO.
	   03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-ADD4 AUTO.
	   03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC1 AUTO.

009930 01  S36.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC X(06) USING W80-ACNO AUTO.

       COPY DEPART.CRT.

       01  S-DEB-MEM.
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
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
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

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
003240	   IF (LS0-SALEV < 3) AND (LS0-DBLEV < 3)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AZ100.
010250	     PERFORM ZA000-INIT.
016330	     PERFORM BC000.
	     CLOSE SALE.
013280 AZ100.
013290       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010480 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
010750	     PERFORM DEBTOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010900 AA99.
010910       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

010930 COPY LOCKED.RC2.

010950 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011150 COPY CONTROL.RD.

011190 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011210 COPY DEPART.RD.

011270 COPY PARAM.RD.

011290 COPY SALEP.RD.

      /
011350 AD000-WRITE             SECTION.

011390 COPY CONTROL.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

011510 COPY PARAM.WR.

011530 COPY SALEP.WR.

011630 COPY DEBTOR.ILU.

      /
      *    ****    V I E W   D E P A R T M E N T S
      *
000010 DEPART-VIEW	     SECTION.

       DEPT-VIEW-SCREEN.
	     DISPLAY S-DPT.
	     MOVE "   !"	 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
011320	       PERFORM RESTORE-SCREEN
	       MOVE "No departments created"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DEPART-VIEW-EXIT.

       DEPT-VIEW-CLR-SCREEN.
	     MOVE 1041		 TO CPOS.
	     PERFORM DEPT-CRT-EDGE UNTIL CLIN = 20.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 43		 TO SHADE-COL.
	     MOVE 35		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE 1042		 TO CPOS.
             MOVE 1              TO W75-S W75-S1.

000110 DEPT-VIEW-NUMB.
000170	     DISPLAY CLEAR-L25.
000180	     MOVE 10		 TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       DEPT-VIEW-TAB-LOOP.
             MOVE ZERO           TO W75-SLEDG (W75-S1).
           IF W75-S1 < 10
	       ADD 1		 TO W75-S1
	       GO TO DEPT-VIEW-TAB-LOOP.
             MOVE ZERO           TO W75-S1.        

       DEPT-READ.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO DEPART-SKIP-READ.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
000940	       SUBTRACT 1	 FROM W75-S
	       GO TO DEPT-FULL-WINDOW.
	   IF ((DPT-CODE = "XXXX" OR "YYYY") OR
	       (DPT-SCDE = "VAT"))
	       GO TO DEPT-READ.

       DEPART-SKIP-READ.
	     DISPLAY (CLIN, 42) DPT-CODE
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 1.
	     DISPLAY ( , 47) DPT-DESC
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 1.
           IF CLIN < 19
	       ADD 1		 TO CLIN W75-S
           ELSE
               GO TO DEPT-FULL-WINDOW.
	     GO TO DEPT-READ.

       DEPT-FULL-WINDOW.             
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

001310 DEPT-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DEPART-VIEW-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO DEPT-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO DEPT-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DEPT-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO DEPT-GET-USER-OPT.

002120 DEPT-CRT-EDGE.
002130	     DISPLAY "³    ³                              ³" AT CPOS
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 0.
002140	     ADD 1		 TO CLIN.

       DEPT-GET-NEXT-PAGE.
	   IF W75-S < 10
001560	       GO TO DEPT-GET-USER-OPT.

       DEPT-NEW-PAGE.
001570	     MOVE 1041		 TO CPOS.
	     PERFORM DEPT-CRT-EDGE UNTIL CLIN = 20.
001590	     MOVE 10		 TO CLIN.
001610	     MOVE 1		 TO W75-S W75-S1.
001620	     PERFORM DEPT-VIEW-TAB-LOOP.
001630	     GO TO DEPT-READ.

       DEPT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       DEPT-GET-PREV-LOOP.
	     PERFORM READ-DEPART-PREV THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE SPACE	 TO WS-OPTION
	       GO TO DEPT-NEW-PAGE.
	   IF ((DPT-CODE = "XXXX" OR "YYYY") OR
	       (DPT-SCDE = "VAT"))
	       GO TO DEPT-GET-PREV-LOOP.
	   IF WS-IXS1 < 20
	       ADD 1	     TO WS-IXS1
	       GO TO DEPT-GET-PREV-LOOP.
	     GO TO DEPT-NEW-PAGE.

       DEPART-VIEW-EXIT.
	     EXIT.

       COPY APAC.HLP.

      *
      *    ****    Read debtor record - using account number.
      *
013750 CA155-GET-DEBTOR  SECTION 2.
013760 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
013870     IF DEB-ACNO NOT = W80-ACNO
013880         MOVE 2            TO WS-ERROR
013890     ELSE
013900         MOVE 0            TO WS-ERROR
013910         MOVE DEB-TYPE     TO W80-TYPE.
013920 CA160-EXIT.
013930       EXIT.
      *
013940 CA165-NAME        SECTION 2.
013950 CA165.
013960       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013970                           INTO W40-SURNAME 
013980                                W40-FNAME 
013990                                W40-SNAME
014000       OVERFLOW
014010           MOVE 1          TO WS-ERROR.
014020 CA170-EXIT.
014030       EXIT.
      *
014040 CA175-ADDRESS     SECTION 2.
014050 CA175.
014060       UNSTRING W60-ADDRESS DELIMITED BY "*"
014070                           INTO W40-ADD1 
014080                                W40-ADD2 
014090                                W40-ADD3 
014100                                W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.
014130 CA175-EXIT.
014140       EXIT.
      *
014150 CA185-ADDRESS     SECTION 2.
014160 CA185.
014170       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014180                           INTO W40-PADD1 
014190                                W40-PADD2 
014200                                W40-PADD3 
014210                                W40-PADD4
014220       OVERFLOW
014230           MOVE 1          TO WS-ERROR.
014240 CA185-EXIT.
014250       EXIT.

      /
       BB000		   SECTION 50.
       BB00.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
       BB05.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BB05.
           IF WS-OPTION = "N"
               GO TO BB20.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-NAME W40-ADD1 W40-ADD2
                                    W40-ADD3 W40-ADD4 W40-PC1.
             MOVE 0915           TO CPOS.
       BB10.
             DISPLAY 
                 "                                                    "
                 AT CPOS.
           IF CLIN < 17
               ADD 1             TO CLIN
               GO TO BB10.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
             DISPLAY S30. 
	     PERFORM SCREEN-SHADOW.
       BB15.
             ACCEPT S31.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
	       GO TO BB15.
       BB20.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
      *    ****    I N V O I C E   A N D   C A S H   S A L E
      *
018500 BC000	     SECTION 50.
018510 BC00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
018520       MOVE 5              TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 
018570 BC05.
      *
      *    ****    I N V O I C E   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S26.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
018660       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
018670				    W90-BAL W90-SALES
018680				    W90-CUR W90-EXCTOT.
018690       MOVE 1              TO WS-FIRST WS-LAST.
	     MOVE 1		 TO WS-VAT-SUB.
018700       MOVE SPACES         TO W90-TAX.
018710       MOVE "I"            TO WS-CR.
	     MOVE 1		 TO WS-S9.

       BC05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC05A.

018720 BC10.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
018770       MOVE SPACES         TO W80-ACNO.

018780 BC11.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     " to do Debtor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
018810       ACCEPT S36 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BC11A
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC11
	       END-EVALUATE
018840	       DISPLAY S36 AT 0308
018850         IF W80-ACNO = SPACES
018860             GO TO BC11.
018870	     DISPLAY CLEAR-L25.
       BC11A.
018880	   IF W80-ACNO = SPACES
018900         GO TO BC599.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

018910 BC12.
018920	     DISPLAY CLEAR-L25.
018930       PERFORM CA155-GET-DEBTOR.
018940     IF WS-ERROR = 1
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
018960	       GO TO BC11.
018970     IF WS-ERROR = 2
032810	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019000	       GO TO BC11.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
019010 BC15.
	     MOVE DEB-ACNO	 TO W40-ACNO.
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
019070       COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
019080                          DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     PERFORM BC189 THRU BC215.
	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *
             PERFORM BC18-MEMO THRU BC18E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
      *
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *
           IF DEB-CAT = 80
	       MOVE "This account has been suspended"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC05.

       BC16.
032920	     MOVE "'C'ontinue, 'R'ekey, 'E'xit, 'F9' Memo/Remarks"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

019110 BC18.
	     MOVE 21		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F9-KEY GO TO BC19
	       END-EVALUATE
	   ELSE
	       GO TO BC19.

       BC18-MEMO.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC18E.
             INITIALIZE W200-MEM-FLDS.
       BC18A.
             MOVE DME-SEQ        TO WS-S1.
             MOVE DME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO BC18A.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     DISPLAY S-DEB-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN.

       BC18E.
             EXIT.

       BC18-E.
             GO TO BC16.

       BC19.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
019230	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
019240         GO TO BC18.
	     PERFORM RESTORE-SCREEN-2.

019270 BC20.
019280	     DISPLAY CLEAR-L25.
019290     IF WS-OPTION = "R"
019300         GO TO BC05.
019310     IF WS-OPTION = "E"
019330         GO TO BC599.
019340       MOVE "Y"            TO WS-OPTION.
      *
      *    ****    C H E C K   A R R E A R S   &   C R E D I T   L I M I T
      *
019350     IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
019360	       MOVE "Arrears > 30 days - Override Y/N"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
019450	       IF NOT (WS-OPTION = "Y")
019490		   MOVE "N"	 TO WS-OPTION.
019510     IF WS-OPTION NOT = "Y"
019520         GO TO BC05.
019530     IF DEB-CRL > ZERO
019540         IF DEB-BAL > DEB-CRL
019550		   MOVE "Credit limit exceded - Override Y/N"
				 TO WS-ERR-MES
		   MOVE SPACE	 TO WS-OPTION
		   PERFORM OPT-MESSAGE
019640	       IF NOT (WS-OPTION = "Y")
019680		   MOVE "N"	 TO WS-OPTION.
019700     IF WS-OPTION NOT = "Y"
019710         GO TO BC05.   
	     MOVE DEB-BAL	 TO WS-BAL.
019920 BC25.
019930       MOVE SPACES         TO W90-ORD W90-REG.
019940 BC30.
A19950	     DISPLAY CLEAR-L25.
019960	     ACCEPT W90-ORD AT 0368
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
019990	     MOVE DEB-SMAN	 TO W10-SMAN.
020020 BC35.
020030	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC05
	       END-EVALUATE.
020050     IF W10-SMAN > 50
020060         GO TO BC35.
             MOVE SPACES         TO W40-ASSIST.
020070     IF W10-SMAN = ZERO
020080	       GO TO BC36.
020090       ADD 100 W10-SMAN    GIVING WS-PARKEY.
020100       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020110     IF PAR-SALESMAN = SPACES
033530	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020140	       GO TO BC35.
020160	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
       BC36.
	   IF DEB-B-TEL = SPACES
	       MOVE DEB-H-TEL	 TO W40-TEL
	   ELSE
	       MOVE DEB-B-TEL	 TO W40-TEL.
	    DISPLAY W40-TEL AT 0466
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 0.
020170 BC40.
	   IF DEB-CAT = 79
	       MOVE "C" 	 TO W90-EXP
	       MOVE W05-VATNO	 TO W90-TAX
	       GO TO BC42A.
020300	   IF DEB-INV-TYPE = "E"
020310         MOVE "Y"      TO WS-OPTION
	   ELSE
	   IF DEB-INV-TYPE = "I"
020330	       MOVE "N"      TO WS-OPTION
	   ELSE
	       MOVE "X"      TO WS-OPTION.
       BC42.
	     MOVE SPACES     TO WS-ERR-MES.
033840	     STRING "TAX Exclusive (Y/N) or eXport?  ["
		    DELIMITED SIZE
		    WS-OPTION DELIMITED SIZE
		    "]" DELIMITED SIZE
			     INTO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC35
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N" OR "X")
               GO TO BC42.
020580       MOVE WS-OPTION      TO W90-EXP.
	   IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
020600     IF WS-OPTION = "N"
020610         MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
020630     ELSE
020640	       MOVE W05-VATNO	 TO W90-TAX.

       BC42A.
	     MOVE DEB-CONTACT	 TO W90-CONTACT.
020660	     ACCEPT W90-CONTACT AT 0529
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.

       BC43.
	     DISPLAY CLEAR-L25.

020750 BC45.
020760	     ACCEPT W90-REG AT 0554
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC43
	       END-EVALUATE.
020780	     DISPLAY CLEAR-L25.
020890	     MOVE ZERO		 TO W90-DB W90-CR.

020900 BC50.
020910       MOVE 8              TO LIN.

020920 BC55.
020930	     MOVE SPACES	 TO W10-RMKS W10-QTE WS-TAX.
020940	     MOVE ZERO		 TO W10-SELL WS-SUPER.
      *
      *    ****    G E T   D E S C R I P T I O N / R E M A R K S
      *
020950 BC60.
021200	     ACCEPT (LIN, 6) W10-QTE
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
021200	     ACCEPT (LIN, 14) W10-RMKS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
023730	   IF W10-RMKS = SPACES
023740         GO TO BC112.
021110	     MOVE WS-LAST	 TO WS-SALKEY.
021120	     INITIALIZE SAP-RECORD1.
021130	     MOVE WS-LAST	 TO SAP-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   I N V O I C E
      *
021140	     DISPLAY (LIN, 2) WS-LAST
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
021160	     DISPLAY CLEAR-L25.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
021350	     MOVE W10-RMKS	 TO SAP-DESC.
023480	     DISPLAY "V" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023490		     "alue or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023500		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023510		     " other key to continue"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE SPACE 	 TO WS-OPTION.
	     ACCEPT WS-OPTION AT 2547
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
021330	     DISPLAY CLEAR-L25.
	   IF NOT (WS-OPTION = "V")
	       MOVE ZERO	 TO W10-SELL
	       MOVE SPACE	 TO WS-TAX W10-LEDG
017340	       GO TO BC105.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
       BC62.
021780	     ACCEPT S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC60
	       END-EVALUATE.
	   IF W10-SELL = ZERO
	       GO TO BC105.
023480	     DISPLAY "Is this value subject to VAT " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023500		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023510		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023500		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023510		     "?" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BC65.
	     MOVE "Y"		 TO WS-OPTION.
	     ACCEPT WS-OPTION AT 2546
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
017340	       GO TO BC65.
021330	     DISPLAY CLEAR-L25.
	     MOVE WS-OPTION	 TO WS-TAX.
022320	     MOVE W10-SELL	 TO SAP-SELL W25-VALUE.
023480	     DISPLAY "Allocate to Department:" AT 2035
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BC70.
023500	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023510		     " to view Departments"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BC75.
	     ACCEPT S26-LEDG.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC60
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
		     PERFORM RESTORE-SCREEN
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO BC75.
	     DISPLAY CLEAR-L25.
	   IF (W10-LEDG = SPACES) OR
	      (W10-LEDG > "VAT0" AND < "VAT7")
	       GO TO BC80.
	     MOVE W10-LEDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF DPT-DESC = SPACES
	       GO TO BC80.
	     DISPLAY DPT-DESC AT 2145
		     WITH BACKGROUND-COLOR 0
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 2035
		      WITH FOREGROUND-COLOR 3.
	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 2145
		      WITH FOREGROUND-COLOR 3.
018520	   IF WS-OPTION = "N"
018530	       GO TO BC60.
	     MOVE W10-LEDG	 TO SAP-LEDG.
	     GO TO BC85.

       BC80.
	     MOVE "Invalid Department Code"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BC70.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
022290 BC85.
023060	     MOVE W05-VAT (WS-VAT-SUB)
				 TO W05-RATE.
	     MOVE ZERO		 TO SAP-VAT.
	     MOVE W10-SELL	 TO W90-VALUE.
	   IF WS-TAX = "Y"
	       IF W90-EXP = "I"
		   COMPUTE SAP-VAT ROUNDED = (W10-SELL /
			  (100.0000 + W05-RATE)) * W05-RATE
		   SUBTRACT SAP-VAT FROM W10-SELL
				 GIVING W90-VALUE.
022840	   IF WS-TAX = "N"
022850         ADD W90-VALUE     TO W90-NONTAX
022860	       MOVE W90-VALUE	 TO SAP-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
022870     ELSE
022880	       MOVE W90-VALUE	 TO SAP-TAXVAL
022890	       ADD W90-VALUE	 TO W90-TAXVAL(WS-VAT-SUB)
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB
022910	       IF W90-TAX NOT = SPACES
022920		   COMPUTE SAP-VAT ROUNDED = SAP-TAXVAL * W05-RTE.
	     MOVE W90-VALUE	 TO SAP-SELL.
	     ADD SAP-SELL SAP-VAT
				 GIVING SAP-VAL.
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
022940       ADD W25-VALUE       TO W90-DB.

022950 BC100.
022960       MOVE W90-DB         TO W90-TOTAL.
022980	     MOVE W90-DB	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
023040	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
023050	       GO TO BC102.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC101.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC101.
023100	     MOVE W25-TOTAL	 TO W100-VAL.
023120	     ADD W25-TOTAL	 TO W90-TOTAL.
023110	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

023130 BC102.
023140	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

023160 BC105.
036100	     MOVE "'A'ccept or 'C'ancel  [A]"
				 TO WS-ERR-MES.
023210	     MOVE "A"		 TO WS-OPTION.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
023220 BC110.
	     PERFORM OPT-MESSAGE.
023300	   IF NOT (WS-OPTION = "A" OR "C")
023310	       GO TO BC105.
023330     IF WS-OPTION = "C"
023340	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
023350         PERFORM BC114
023360         PERFORM BC100 THRU BC102
023370         GO TO BC55.               

023380 BC111.
023390       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
023400       ADD 1               TO WS-LAST.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
       BC111E.
023410	   IF LIN < 18
023420         ADD 1             TO LIN
023430         GO TO BC112.
023440       MOVE 8              TO LIN.
023450       PERFORM BC115 UNTIL LIN > 18.
023460       GO TO BC50.

023470 BC112.
023480	     MOVE "'R'emarks or 'E'nd  [R]"
				 TO WS-ERR-MES.
023540	     MOVE "R"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023620	   IF NOT (WS-OPTION = "R" OR "E")
023630	       GO TO BC112.
023670     IF WS-OPTION = "E"
023680         SUBTRACT 1        FROM WS-LAST
023690         GO TO BC120.
	     GO TO BC55.

023810 BC114.
023820       SUBTRACT W25-VALUE  FROM W90-DB.
023830       SUBTRACT W90-CDISC  FROM W90-LDISC.
023840	     SUBTRACT SAP-NONTAX FROM W90-NONTAX.
	   IF SAP-VAT-SUB > ZERO
036780	       SUBTRACT SAP-TAXVAL FROM W90-TAXVAL(SAP-VAT-SUB)
	       SUBTRACT SAP-VAT	 FROM W90-VAT(SAP-VAT-SUB).

023880 BC115.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
023900       ADD 1               TO LIN.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
023910 BC120.
023920	   IF WS-LAST = ZERO
023930         GO TO BC05.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
023940 BC125.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
024040 BC130.
	     PERFORM OPT-MESSAGE.
024110	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
024120         GO TO BC130.
024140     IF WS-OPTION = "M"
024150         ADD 1             TO WS-LAST
024160         GO TO BC112.
024170     IF WS-OPTION = "A"
024180         PERFORM BC270 THRU BC335
024190         GO TO BC125.
024200     IF WS-OPTION = "C"
024210         GO TO BC599.
024220       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
024230     IF W90-LDISC NOT = ZERO
024240         MOVE W90-LDISC    TO W90-TDISC
024250	   ELSE
024260	       MOVE W90-DB	 TO W90-DVAL.
024300       SUBTRACT W90-TDISC FROM W90-DB 
024310                           GIVING W90-TSELL W90-TOTAL.
024320       MULTIPLY -1 BY W90-TDISC 
024330				 GIVING W100-VAL.
024340	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
024350       MOVE ZERO           TO W90-TTAX.
	     MOVE 1		 TO WS-P1.
037540	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
024430	       GO TO BC195.
      *
      *    ****    C A L C U L A T E   T O T A L   V A T
      *
       BC141.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BC142.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
021490	     COMPUTE W90-TTAX ROUNDED = (W90-TTAX +
		    (W90-TAXVAL(WS-P1) * W05-RTE)).
      *
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *
       BC142.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BC141.
024470	     MOVE W90-TTAX	 TO W100-VAL.
024480	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
024490       ADD W90-TTAX        TO W90-TOTAL.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *
025130 BC185.
	     MOVE "Print Invoice Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.

025310 BC186.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PRNINV.
	   IF NOT (WS-PRNINV = "Y" OR "N")
	       GO TO BC185.
	     MOVE WS-PRNINV	 TO W40-PRNINV.
	     MOVE WS-LAST	 TO W40-LAST.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
	   IF DEB-TERMS = "3"
	       PERFORM BB000.
025430	     DISPLAY "Processing Invoice" AT 2502 WITH
025440		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
029610	     CLOSE SALE.
	     OPEN I-O SALE.
	     MOVE W10-SMAN	 TO W40-SMAN.
	     MOVE WS-PRC	 TO W40-PRC.
	     MOVE "N"		 TO W40-REPRINT.
	     MOVE WS-CR 	 TO W40-CR.
	     CALL PRG-NAME USING WS-PARID
				 LS-USER-ID
				 LS0-PROGRAMS
				 W40-COMPANY
				 W90-TRAN
		  ON EXCEPTION
		      GO TO BC187
	     END-CALL.
	     CANCEL PRG-NAME.
	     GO TO BC188.

       BC187.
	     MOVE SPACES     TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.

025410 BC188.
029690	     DISPLAY CLEAR-L25.
029610	     CLOSE SALE.
029600	     OPEN OUTPUT SALE.
029610	     CLOSE SALE.
	     OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
029800	   IF WS-OPTION = "E"
029820         GO TO BC599.
029830       GO TO BC05.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
       BC189.
025570	   IF W80-TYPE NOT = 1
025580         GO TO BC210.
025590       MOVE DEB-NAME       TO W60-NAME.
025600       PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
025610     IF DEB-LANG = "A"
025620         MOVE W75-ATITLE (DEB-TITLE) 
025630                           TO W60-NAME
025640     ELSE
025650         MOVE W75-ETITLE (DEB-TITLE) 
025660                           TO W60-NAME.
025670       MOVE 7              TO WS-S6.

025680 BC190.
025690     IF W60-NCHAR (WS-S6) NOT = SPACE
025700         GO TO BC195.
025710       SUBTRACT 1          FROM WS-S6.
025720     IF WS-S6 > 0
025730         GO TO BC190.
      *
      *    ****    F I R S T   I N I T I A L
      *
025740 BC195.
025750	     MOVE 1		 TO WS-S5.
025760       ADD 2               TO WS-S6.
025770       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
025780       ADD 1               TO WS-S6.
025790       MOVE "."            TO W60-NCHAR (WS-S6).
025800     IF W40-NCHAR (49) = SPACE
025810         GO TO BC200.
      *
      *    ****    S E C O N D   I N I T I A L
      *
025820       ADD 1               TO WS-S6.
025830       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
025840       ADD 1               TO WS-S6.
025850       MOVE "."            TO W60-NCHAR (WS-S6).

025860 BC200.
025870       ADD 2               TO WS-S6.
      *
      *    ****    S U R N A M E
      *
025880 BC205.
025890	     MOVE W40-NCHAR (WS-S5)
025900                           TO W60-NCHAR (WS-S6).
025910     IF WS-S5 < 24
025920         ADD 1             TO WS-S5
025930         ADD 1             TO WS-S6
025940         GO TO BC205.
025950       MOVE W60-NAME       TO W40-NAME.

025960 BC210.
025980       MOVE SPACES         TO W40-PC1 W40-PC2.
025990       MOVE DEB-ADDRESS    TO W60-ADDRESS.
026000       PERFORM CA175-ADDRESS.
026010     IF NOT (DEB-POST = ZERO OR 9999)
026020         MOVE DEB-POST     TO W40-PC1.
026030     IF W40-ADD4 = SPACES
026040         MOVE W40-PC1      TO W40-ADD4
026050         MOVE SPACES       TO W40-PC1.
026060     IF W40-ADD3 = SPACES
026070         MOVE W40-ADD4     TO W40-ADD3
026080         MOVE SPACES       TO W40-ADD4.
026090     IF W40-ADD2 = SPACES
026100         MOVE W40-ADD3     TO W40-ADD2
026110         MOVE SPACES       TO W40-ADD3.
026120     IF W40-ADD1 = SPACES
026130         MOVE W40-ADD2     TO W40-ADD1
026140         MOVE SPACES       TO W40-ADD2.
026150       MOVE DEB-PADDR      TO W60-ADDRESS1.
026160       PERFORM CA185-ADDRESS.
026170     IF NOT (DEB-PPOST = ZERO OR 9999)
026180         MOVE DEB-PPOST    TO W40-PC2.
026190     IF W40-PADD4 = SPACES
026200         MOVE W40-PC2      TO W40-PADD4
026210         MOVE SPACES       TO W40-PC2.
026220     IF W40-PADD3 = SPACES
026230         MOVE W40-PADD4    TO W40-PADD3
026240         MOVE SPACES       TO W40-PADD4.
026250     IF W40-PADD2 = SPACES
026260         MOVE W40-PADD3    TO W40-PADD2
026270         MOVE SPACES       TO W40-PADD3.
026280     IF W40-PADD1 = SPACES
026290         MOVE W40-PADD2    TO W40-PADD1
026300         MOVE SPACES       TO W40-PADD2.

       BC215.
	       EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
029980 BC270.
	     MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE "S"		 TO WS-OPTION.

       BC271.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "H" OR "S")
	       GO TO BC270.
           IF WS-OPTION = "S"
               GO TO BC272
           ELSE
           IF WS-OPTION = "E"
               GO TO BC335.

       BC271A.
	     MOVE "Amend - 'O'rd No, 'R'ef No, 'S'alesman or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

       BC271B.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "O" OR "R" OR "S")
	       GO TO BC271A.
           IF WS-OPTION = "E"
               GO TO BC335.
           IF WS-OPTION = "O"
017660         ACCEPT W90-ORD AT 0368 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0 UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "R"
               ACCEPT W90-REG AT 0554 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0 UPDATE AUTO
               GO TO BC271A.

       BC271C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BC271C.
             MOVE SPACES         TO W40-ASSIST.
	     MOVE W10-SMAN	 TO W40-SMAN.
017740     IF W10-SMAN = ZERO
               DISPLAY "                              "  AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BC271A.
017820	     DISPLAY CLEAR-L25.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810	       GO TO BC271C.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
             GO TO BC271A.

       BC272.
029990       MOVE 8              TO LIN.
030000       PERFORM BC115 UNTIL LIN > 18.
030010       MOVE 8              TO LIN.
030020       MOVE 1              TO WS-LINES WS-FIRST.

030030 BC275.
030040       MOVE WS-FIRST TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
030050 BC280.
030060	   IF WS-SALKEY > WS-LAST
030070         GO TO BC285.
030080       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030090	     DISPLAY (LIN, 2) SAP-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030140	     DISPLAY ( , 6) SAP-QTE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030140	     DISPLAY ( , 14) SAP-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	   IF NOT (SAP-SELL = ZERO)
030170	       MOVE SAP-SELL	 TO W10-SELL
030180	       DISPLAY S26-PRC AT LINE LIN.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
030210 BC282.
030220     IF LIN < 18
030230         ADD 1             TO LIN WS-SALKEY WS-LINES
030240         GO TO BC280.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
030250 BC285.
029400	   IF WS-LINES < 11
029410         IF WS-FIRST = 1
029420		   MOVE "'A'mend/'D'elete an Item - 'E'xit"
				 TO WS-ERR-MES
029440	       ELSE
029450		   MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES
029480		   MOVE 2	 TO WS-LINES
029490     ELSE
029500     IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029450		   MOVE "'A'mend/'D'elete an Item,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029550		   MOVE 1	 TO WS-LINES
029560         ELSE
029450		   MOVE "'A'mend/'D'elete,'P'revious,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029600		   MOVE 3	 TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029450	       MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES.

030520 BC290.
	     PERFORM OPT-MESSAGE.
030590	   IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
030600	       GO TO BC285.
030610	   IF WS-OPTION = "E"
030620         GO TO BC335.
030630     IF WS-OPTION = "A" OR "D"
030640         GO TO BC300.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
030650     IF WS-LINES = 2 OR 3
030660         IF WS-OPTION = "P"
030670             PERFORM BC295
030690             SUBTRACT 11   FROM WS-FIRST
030700             GO TO BC275.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
030710     IF WS-LINES = 1 OR 3
030720         IF WS-OPTION = "N"
030730             PERFORM BC295
030750             ADD 11        TO WS-FIRST
030760             GO TO BC275.
030770	     GO TO BC285.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
030780 BC295.
030790	     MOVE 8		 TO LIN.
030800       PERFORM BC115 UNTIL LIN > 18.
030810       MOVE 8              TO LIN.
030820       MOVE 1              TO WS-LINES.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
030830 BC300.
030840	   IF WS-OPTION = "A"
030850         GO TO BC310.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
       BC305.
043780	     MOVE "Are you sure Y/N?"
				 TO WS-ERR-MES.
030880	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF NOT (WS-OPTION = "N" OR "Y")
030980         GO TO BC305.              
031000     IF WS-OPTION = "N"
031010	       GO TO BC285.
031020       MOVE "D"            TO WS-OPTION.

031030 BC310.
031040	     DISPLAY CLEAR-L25.
031050       DISPLAY "Enter item No to Amend/Delete >  <" AT 2501
031060		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
031070       MOVE ZERO           TO WS-INO.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
031080 BC315.
031090	     ACCEPT WS-INO AT 2532
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
031100                                  UPDATE AUTO.
031110     IF WS-INO = ZERO
031120	       DISPLAY CLEAR-L25
031130	       GO TO BC285.
031140     IF WS-INO > WS-SALKEY
031150         GO TO BC315.
031160     IF WS-INO < WS-FIRST
031170         GO TO BC315.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
031180     IF WS-INO < 12
031190         ADD 7 WS-INO      GIVING LIN
031200     ELSE
031210         COMPUTE LIN = WS-INO - WS-FIRST + 8.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
031220       MOVE WS-INO         TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
	   IF NOT (SAP-SELL = ZERO)
	       MOVE SAP-VAT-SUB	 TO WS-VAT-SUB
031300	       MOVE SAP-SELL	 TO W10-SELL W25-VALUE
      *
      *    ****    R E D U C E   T O T A L S 
      *
031340	       SUBTRACT W25-VALUE
				 FROM W90-DB
031360	       SUBTRACT SAP-NONTAX
				 FROM W90-NONTAX
	       IF WS-VAT-SUB > ZERO
031370		   SUBTRACT SAL-TAXVAL
				 FROM W90-TAXVAL(WS-VAT-SUB)
		   SUBTRACT SAL-VAT
				 FROM W90-VAT(SAL-VAT-SUB)
	       END-IF
	   ELSE
	       MOVE ZERO	 TO W10-SELL W25-VALUE.
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
031400 BC316.
031410     IF WS-OPTION = "A"
031420         GO TO BC325.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
031440	     PERFORM BC100 THRU BC102.
031450 BC320.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
031460       MOVE WS-SALKEY      TO WS-SALSTR.
031470       ADD 1               TO WS-SALKEY.
031480     IF WS-SALKEY > WS-LAST
031490         SUBTRACT 1        FROM WS-LAST
031500         GO TO BC272.
031510       PERFORM READ-SALE THRU READ-SALE-EXIT.
031520	     SUBTRACT 1 	 FROM SAP-CODE.
031530       MOVE WS-SALSTR      TO WS-SALKEY.
031540       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031550       ADD 1               TO WS-SALKEY.
031560       GO TO BC320.
031570 BC325.
031590	     ACCEPT (LIN, 6) SAP-QTE
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
031590	     ACCEPT (LIN, 14) SAP-SDSC
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF SAP-SELL = ZERO
031610	       GO TO BC332.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
021780	     ACCEPT S26-PRC AT LINE LIN.
	     MOVE W10-SELL	 TO W25-VALUE.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
031840	     MOVE W25-VALUE	 TO SAP-SELL.
031890 BC330.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
031900	     PERFORM BC85 THRU BC102.
031910 BC332.
031920       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031930       GO TO BC270.
031940 BC335.
031950       EXIT.
031960 BC599.
031970       EXIT.
      /
052700 ZA000-INIT	       SECTION 90.
052710 ZA000-OPEN.
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
052730	     DISPLAY CLR-SCREEN.
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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-INV = 1)
	       MOVE 7		 TO PRG-NUMBER
	   ELSE
	       MOVE PAR-INV	 TO PRG-NUMBER.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
054270	     OPEN OUTPUT SALE.
054280	     CLOSE SALE.
054290	     OPEN I-O SALE.
054760       MOVE 1              TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
054710       MOVE 5              TO WS-PARKEY.
054720       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054750	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
055260	     GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
055330       GO TO ZA205.
055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	91.
056100 OPEN-ERROR.
056110       DISPLAY CLR-SCREEN.
056120       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
056130       PERFORM DISPLAY-FILE-NAME.
056140
056150 READ-ERROR.
056160       DISPLAY CLR-SCREEN.
056170       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
056180       PERFORM DISPLAY-FILE-NAME.
056190
056200 WRITE-ERROR.
056210       DISPLAY CLR-SCREEN.
056220       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
056230       PERFORM DISPLAY-FILE-NAME.
056240

056260 DISPLAY-FILE-NAME.
056310	   IF WS-F-ERROR = 2
056320	       MOVE W02-NETWORK  TO WS-FILE
056330	       MOVE WS-NETKEY	 TO WS-KEY
056380     ELSE
056390     IF WS-F-ERROR = 6
056400	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420     ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
056500	   ELSE
056510     IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
056580	   ELSE
056590     IF WS-F-ERROR = 19
056600	       MOVE W02-SALE	 TO WS-FILE
056610	       MOVE WS-SALKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX.
056740     IF WS-STATUS = "10"
056750         MOVE "End of FILE" TO WS-STAT-MESSAGE
056760     ELSE
056770     IF WS-STATUS = "22"
056780         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
056790     ELSE
056800     IF WS-STATUS = "23"
056810         MOVE "Invalid record number" TO WS-STAT-MESSAGE
056820     ELSE
056830     IF WS-STATUS = "24"
056840         MOVE "DISK full" TO WS-STAT-MESSAGE
056850     ELSE
056860     IF WS-STATUS = "30"
056870         MOVE "DISK error" TO WS-STAT-MESSAGE
056880     ELSE
056890     IF WS-STATUS = "94"
056900         MOVE "FILE locked" TO WS-STAT-MESSAGE.
056910       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
056920       DISPLAY "Status " AT 1212 
056930                WS-STATUS WITH FOREGROUND-COLOR 11
056940               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
056950     IF WS-STATUS NOT = "94" 
056960         DISPLAY "Key    " AT 1412 
056970                  WS-KEY WITH FOREGROUND-COLOR 11
056980         DISPLAY "Reverse backup or contact program Support"
056990                  AT 1612.
057000         DISPLAY "Please make a note of these details" AT 1812.
057010       STOP RUN.
