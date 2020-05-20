      $SET LINKCOUNT"448"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	 ******   *******   **	   **  *
      *   **	**     **     **    **	**    **  **	**  ***	   **  *
      *   **	**     **     **    **	**	  **	**  ** *   **  *
      *   **	**     **     *******	**	  *******   **	*  **  *
      *   **	**     **     **	**	  **   **   **	 * **  *
      *   **	**     **     **	**    **  **	**  **	  ***  *
      *   *******      **     **	 ******   **	**  **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T   N O T E   P R O G R A M		       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPCRN.
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

000250 COPY GARTEE.SL.

000270 COPY PARAM.SL.

000310 COPY SALE.SL.

000330 COPY STOCK.SL.

000140 COPY STKMEM.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000480 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000500 COPY DEPART.FDE.

000250 COPY GARTEE.FDE.

000560 COPY PARAM.FDE.

000600 COPY SALE.FDE.

000620 COPY STOCK.FDE.

       COPY STKMEM.FDE.

      /
002130 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002140 77  WS-DISC	   PIC	99V99	 COMP-3.
002150 77  WS-TDISC        PIC  99V99    COMP-3.
002170 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
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
002450 77  WS-PRN1         PIC  9(01) VALUE 0.  
002460 77  WS-PRN2         PIC  9(01) VALUE 0.  
       77  WS-NUM          PIC  Z9.
002470*77  WS-ADV-RATE	   PIC	9(03)V99 COMP-3.
      *77  WS-ADVALOREM    PIC	X(01).
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
002840 77  WS-CHAR1        PIC  X(01).
002850 77  WS-CHAR2        PIC  X(01).
002860 77  WS-COMP         PIC  9(01).
002890 77  WS-HEAD         PIC  X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE          PIC  X(01).
002920 77  WS-PRC          PIC  X(01).
002920 77  WS-INPUT        PIC  X(01) VALUE "N".
002920 77  WS-COPY         PIC  X(01) VALUE "N".
002920 77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
002950 77  WS-BATCH        PIC  9(05).
002960 77  WS-CHK          PIC  X(33) VALUE
002970                          "ENTER if correct - N if incorrect".
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
	   03  FILLER	   PIC	X(08) VALUE "INV\CRNF".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.

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
001450	       05  WS-MCHAR PIC  X(80) VALUE "³   ³              ³
      -        "                       ³        ³         ³          ³".
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
	   03  WS-PROG	       PIC  X(03) VALUE "CRN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003700 COPY CONTROL.ID.

003740 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY PARAM.ID.

003840 COPY SALE.ID.

003860 COPY STOCK.ID.

000140 COPY STKMEM.ID.

       COPY W05.VAT.

       COPY W10.SAL.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

004610 01  W15-DISCOUNT.
004620     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

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
005860     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

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
007830     03  LINE 12 COLUMN 28 FOREGROUND-COLOR 15 PIC  X(06)
007840                           USING W40-CSHPASS AUTO SECURE.

       COPY STKLUP.CRT.

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

       COPY INVOICE.CRT.

009600 01  S26-QNT.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009620                           PIC Z(04)9.99 USING W10-QUANT AUTO.

009630 01  S26-PRC.
009640	   03	       COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009650                           PIC Z(05)9.99 USING W10-SELL AUTO.

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009720 01  S26-AVGE.
009730	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009740				 PIC Z(04)9.999 USING STK-AVGE AUTO.

       01  S26-DATE.
	   03  LINE  4 COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  99/99/9999 USING W21-DATE AUTO.

009840 01  S027-CNOTE.
009850     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 
009860                           VALUE "CREDIT NOTE".
	   03  LINE  4 COLUMN 60 VALUE "Inv. Date".

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

       COPY S99.CRT.

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

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AZ100.
010250	     PERFORM ZA000-INIT.
016330	     PERFORM BD000.
	     CLOSE SALE.
013280 AZ100.
013290       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010460 AA50D.
010470       MOVE "D" TO WS-LOOK.
010480 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
010740	   IF WS-LOOK = "D"
010750         PERFORM DEBTOR-LOOKUP
010760     ELSE
010770         PERFORM STOCK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010890	     MOVE "S" TO WS-LOOK.
010900 AA99.
010910       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

010930 COPY LOCKED.RC2.

010950 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300              SECTION.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Item code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Alt code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Description/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " X/Ref Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011150 COPY CONTROL.RD.

011190 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011270 COPY PARAM.RD.

011290 COPY SALE.RD.

000140 COPY STKMEM.RD.

011310 COPY STOCK.RD.

      /
011350 AD000-WRITE             SECTION.

011390 COPY CONTROL.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011510 COPY PARAM.WR.

011530 COPY SALE.WR.

011550 COPY STOCK.WR.

011610 COPY STOCK.LUP.

011630 COPY DEBTOR.ILU.

       COPY APAC.HLP.

       COPY DATE.CHK.

       COPY CA000.STK.

       COPY CA100.STK.

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

       COPY CA200.STK.

       COPY CA500.STK.

      /
       BB000               SECTION.
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
	     PERFORM SAVE-SCREEN-2.
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
020250	   IF WS-OPTION = "N"
               GO TO BB15.
       BB20.
011320	     PERFORM RESTORE-SCREEN-2.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
       BB030           SECTION.
       BB30.
             MOVE ZERO           TO WS-ERROR.
	     MOVE 1		 TO WS-S5.
	     MOVE W10-QUANT	 TO WS-S6.
             MOVE SPACES         TO W43-SERIALS.
	     PERFORM SAVE-SCREEN-3.
       BB40.
             DISPLAY (LIN, 51) "Serial No:" WITH FOREGROUND-COLOR 14
                                                 BACKGROUND-COLOR 7
                        "            (   )" WITH FOREGROUND-COLOR 15
                                                 BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
             DISPLAY (LIN, 74) W100-SER WITH FOREGROUND-COLOR 15
                                             BACKGROUND-COLOR 7.
       BB42.
	     ACCEPT (LIN, 62) W43-SERNO (WS-S5)
                              WITH FOREGROUND-COLOR 0 
                                   BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W43-SERIALS
		     GO TO BB45
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB42.
	   IF W43-SERNO (WS-S5) = SPACES
               GO TO BB42.
             MOVE STK-CODE       TO GUA-CODE.
	     MOVE W43-SERNO (WS-S5)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
	   IF WS-F-ERROR = 1
	       MOVE "Serial number not on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BB42.
       BB44.
	   IF WS-S5 < WS-S6
	       ADD 1		 TO WS-S5
               GO TO BB41.
       BB45.
	     PERFORM RESTORE-SCREEN-3.
       BB49.
             EXIT.
      /
017970 BB100               SECTION.
017980 BB100-MARG.
017990       MOVE ZERO TO WS-ERROR.
018010     IF W90-TAX = SPACES
018020         IF STK-TAX NOT = ZERO
018030		   COMPUTE W10-SPAPRC ROUNDED = (W10-SPAPRC /
018040                (100.0000 + W05-RATE)) * 100. 
           IF (WS-PRC = "R") AND (STK-SELL NOT > W10-SPAPRC)
015720         GO TO BB110
           ELSE
           IF (WS-PRC = "C") AND (STK-CASH NOT > W10-SPAPRC)
015720         GO TO BB110
           ELSE
018050     IF STK-WSALE NOT > W10-SPAPRC
018060         GO TO BB110.
018070       COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
018080                          * 100.00.
018090     IF W10-MARG NOT < W85-MARG
018100         GO TO BB110.
018110	     DISPLAY "Margin less than allowed - Override " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		      "Y" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		      "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		      "N" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT.
018130 BB105.
018140	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018200	   IF NOT (WS-OPTION = "N" OR "Y")
018210         GO TO BB105.
018220     IF WS-OPTION = "N"
018230         MOVE 1 TO WS-ERROR
018240         GO TO BB110.
018250 BB106.
018260	     DISPLAY "Enter password" AT 2551
018270		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
018280	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
018290			 AUTO NO-ECHO.
018300       MOVE 6 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1 TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /
      *    ****    C R E D I T   N O T E
      *
031980 BD000	       SECTION 5.
031990 BD0-PASS.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
032000       MOVE 5              TO WS-PARKEY.
032010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
032020       MOVE PAR-PW         TO W85-PASS. 
032380 BD00.
032390       DISPLAY CLR-SCREEN.
032400       DISPLAY S26.
032440	     DISPLAY S027-CNOTE.
032450       MOVE 8              TO LIN.
032460       PERFORM BD80 UNTIL LIN > 18.
032470	     MOVE W12-TODAY	 TO W90-DTE W21-DATE.
018650	     DISPLAY W90-DTE AT 0572
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
032490	     MOVE ZERO		 TO W90-BCR W90-NONTAX W90-CRED
032500                              W90-BAL W90-TAXVAL W90-LDISC 
032510				    W90-CUR W90-EXCTOT.
032520       MOVE 1              TO WS-FIRST WS-LAST.
	     MOVE 1		 TO WS-VAT-SUB.
032530       MOVE SPACES         TO W90-TAX.
032540       MOVE "C"            TO WS-CR.
             MOVE "N"            TO WS-INPUT.
032550 BD05.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
032600	     MOVE SPACES	 TO W80-ACNO.
032610 BD06.
032620	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032630		     " to do Debtor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
032640       ACCEPT S36 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BD07
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD06
	       END-EVALUATE
032670	       DISPLAY S36 AT 0308
032680         IF W80-ACNO = SPACES
032690             GO TO BD06.
032700	     DISPLAY CLEAR-L25.
       BD07.
032710	   IF W80-ACNO = SPACES
032730         GO TO BD999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
032740 BD08.
032750	     DISPLAY CLEAR-L25.
032760       PERFORM CA155-GET-DEBTOR.
032770     IF WS-ERROR = 1
032780	       DISPLAY "No record" AT 2540
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032790         GO TO BD06.
032800     IF WS-ERROR = 2
032810         DISPLAY "Invalid account" AT 2540 
032820			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032830         GO TO BD06.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
	     MOVE DEB-ACNO	 TO W40-ACNO.
032840       MOVE DEB-NAME TO W40-NAME.
032850     IF W80-TYPE = 1
032860         INSPECT W40-NAME REPLACING ALL "*" BY " "
032870         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
032890	     COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
032900                          DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     PERFORM BD120A THRU BD145.
      *
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *
	     PERFORM BD09-MEMO THRU BD09E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
       BD08A.
032920	     DISPLAY "C" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032930		     "ontinue "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
032940		     "R" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
032950		     "ekey or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
032960		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019150		     "xit > < - Press "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " for Memo/Remarks"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE "C"		 TO WS-OPTION.
032980 BD09.
032990       ACCEPT WS-OPTION AT 2536 
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F9-KEY GO TO BD09O
	       END-EVALUATE
	   ELSE
	       GO TO BD09O.
       BD09-MEMO.
019250	     DISPLAY CLEAR-L25.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
	       GO TO BD09E.
             INITIALIZE W200-MEM-FLDS.
       BD09A.
             MOVE DME-SEQ        TO WS-S1.
             MOVE DME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO BD09A.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN.
       BD09E.
	     EXIT.
       BD09-E.
             GO TO BD08A.
       BD09O.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
033050	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
033060         GO TO BD09.
	     PERFORM RESTORE-SCREEN-2.
019690	     DISPLAY CLEAR-L25.
033090     IF WS-OPTION = "R"
033100         GO TO BD00.
033110     IF WS-OPTION = "E"
033130         GO TO BD999.
           IF NOT VALID-PRICE
               MOVE "R"          TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
033250 BD09P.
033340	     MOVE SPACES	 TO W90-ORD W90-REG.
033350 BD10.
033360	     DISPLAY CLEAR-L25.
019960	     ACCEPT W90-ORD AT 0368
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
033400	     MOVE DEB-SMAN	 TO W10-SMAN.
033430 BD15.
020030	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD00
	       END-EVALUATE.
033460	   IF W10-SMAN > 50
033470         GO TO BD15.
             MOVE SPACES         TO W40-ASSIST.
033480     IF W10-SMAN = ZERO
033490	       GO TO BD15A.
033500       ADD 100 W10-SMAN    GIVING WS-PARKEY.
033510       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
033520     IF PAR-SALESMAN = SPACES
033530         DISPLAY "Invalid Salesman Number" AT 2518
033540			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
033550         GO TO BD15.
033560	     DISPLAY CLEAR-L25.
             MOVE PAR-SALESMAN   TO W40-ASSIST.

       BD15A.
033570	     DISPLAY W40-ASSIST AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

       BD16.
	     ACCEPT S26-DATE.
	     MOVE W21-DATE	 TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = ZERO
008570	       GO TO BD19.
      
008580 BD17.
008590     IF W20-DAY > 29
008600	       GO TO BD18.
008610       DIVIDE W20-YEAR BY 4 GIVING W25-RESULT.
008620     IF W25-DECIMAL = 0
008630	       GO TO BD19.
      
008640 BD18.
008650	     DISPLAY "Invalid date" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
	     GO TO BD16.

008660 BD19.
	   IF W22-DATE < WS-VAT-DATE
	       ADD 6		 TO WS-VAT-SUB.

033580 BD20.
033590	     DISPLAY WS-CHK AT 2518
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
033600	     ACCEPT WS-OPTION AT 2552
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
033660	   IF WS-OPTION = "N"
033670         GO TO BD15.
033680	     DISPLAY CLEAR-L25.
       BD21.
020300	   IF DEB-INV-TYPE = "E"
020310         MOVE "Y"      TO WS-OPTION
	   ELSE
	   IF DEB-INV-TYPE = "I"
020330	       MOVE "N"      TO WS-OPTION
	   ELSE
	       MOVE "X"      TO WS-OPTION.
033840	     DISPLAY "TAX Exclusive (" AT 2520
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
033850		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
033860		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
033870		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
033880		     ") or e" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
033890		     "X" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
033900		     "port?" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       BD22.
033910	     ACCEPT WS-OPTION AT 2551
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
033920			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD15
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N" OR "X")
               GO TO BD22.
033990       MOVE WS-OPTION      TO W90-EXP.
           IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
034010     IF WS-OPTION = "N"
034020         MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
034040     ELSE
034050	       MOVE W05-VATNO	 TO W90-TAX.
	     MOVE DEB-CONTACT	 TO W90-CONTACT.
020660	     ACCEPT W90-CONTACT AT 0529
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
       BD23.
	     DISPLAY CLEAR-L25.
      *	   IF WS-ADVALOREM = "N"
      *       MOVE "N"	 TO W90-ADV.
      *	       GO TO BD25.
      *	     DISPLAY "Ad Valorm tax (" AT 2512
      *		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
      *		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
      *		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
      *		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
      *		     ")? > <"
      *		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *       MOVE "N"		  TO WS-OPTION.
       BD24. 
020370*	     ACCEPT WS-OPTION AT 2534
      *		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
020380*			 UPDATE AUTO.
      *	   IF USER-FUNC
      *	       EVALUATE KEY-CODE-1
      *		 WHEN ESC-KEY
      *		     GO TO BD21
      *	       END-EVALUATE.
      *	     CALL "CBL_TOUPPER" USING WS-OPTION
      *				BY VALUE WS-LENGTH
      *				RETURNING WS-STATUS.
      *	   IF NOT (WS-OPTION = "Y" OR "N")
      *        GO TO BD24.
      *      MOVE WS-OPTION	  TO W90-ADV.
020550*	     DISPLAY CLEAR-L25.

034160 BD25.
020760	     ACCEPT W90-REG AT 0554
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD23
	       END-EVALUATE.
034190	     DISPLAY CLEAR-L25.
034300	     MOVE ZERO		 TO W90-DB W90-CR.
034310 BD30.
034320       MOVE 8 TO LIN.
034330 BD35.
034340       MOVE SPACES         TO W10-DESC  W10-ITEM W10-SERNO.
034350       MOVE ZERO           TO W10-SELL WS-SUPER.
034350       MOVE 1              TO W10-QUANT.
034360 BD40.
034370       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
020970	     ACCEPT (LIN, 6) W10-ITEM
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
		     GO TO BD40A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD40
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50
021010	       DISPLAY (LIN, 6) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
034430	       IF W10-ITEM = SPACES
034440             GO TO BD40.
       BD40A.
034450	   IF W10-ITEM = SPACES
034460         SUBTRACT 1        FROM WS-LAST
034470         GO TO BD85.
       BD41.
034480       PERFORM CA000.
034490     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
018630	       DISPLAY (LIN, 6) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
018640	       IF W10-ITEM = SPACES
018720             GO TO BD40
               ELSE
                   GO TO BD41.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD40.
034520	     MOVE WS-LAST	 TO WS-SALKEY.
034530       INITIALIZE SAL-RECORD1.
034540       MOVE WS-LAST        TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   C R E D I T   N O T E
      *
021140	     DISPLAY (LIN, 2) WS-LAST
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
034560	     MOVE STK-CODE	 TO SAL-ITEM.
034570	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
034580 BD44.
034590     IF STK-IND = 3
034600         MOVE ZERO         TO STK-AVGE
021200	       ACCEPT (LIN, 21) W10-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
034630	       DISPLAY "Index Unit cost" AT 2512
034640			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
034650         ACCEPT S26-AVGE AT LINE 25
034660     ELSE
034670         MOVE STK-DESC     TO W10-DESC
021270	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
034690	   IF STK-IND = 3
034700         IF STK-AVGE = ZERO
034710             DISPLAY "Unit cost is zero" AT 2512
034720			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
034730             GO TO BD44.
034740	     DISPLAY CLEAR-L25.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
034750       MOVE W10-ITEM       TO SAL-ITEM.
034760       MOVE W10-DESC       TO SAL-MSER.
034770       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
       BD44A.
034780       ACCEPT S26-QNT AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
021400		     GO TO BD35
		 END-EVALUATE.
034790	   IF W10-QUANT = ZERO
034800	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
034810         GO TO BD35.
034820	     DISPLAY CLEAR-L25.
             MOVE ZERO           TO WS-ERROR.
           IF STK-GUAR NOT = ZERO
               PERFORM BB030
           ELSE
               MOVE SPACES       TO W43-SERNO (1).
           IF WS-ERROR = 1
               GO TO BD44A.
      *
      *    ****    G E T   S E L L I N G   P R I C E 
      *
034830     IF STK-IND < 3
034960         IF W90-TAX NOT = SPACES
034970             IF WS-PRC = "R"
034980                 MOVE STK-SELL 
034990                           TO W10-SELL
035000             ELSE
035010             IF WS-PRC = "C"
035020                 MOVE STK-CASH 
035030                           TO W10-SELL
035040             ELSE
035050                 MOVE STK-WSALE 
035060                           TO W10-SELL
035070         ELSE
035080             IF WS-PRC = "R"
035090                 MOVE W10-VSELL 
035100                           TO W10-SELL
035110             ELSE
035120             IF WS-PRC = "C"
035130                 MOVE W10-VCASH 
035140                           TO W10-SELL
035150             ELSE
035160                 MOVE W10-VWSALE 
035170                           TO W10-SELL.
035180 BD45.
035190       ACCEPT S26-PRC AT LINE LIN
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD44A
	       END-EVALUATE.
035200	     MOVE W10-SELL	 TO W10-SPAPRC.
035220	     PERFORM BB100.
035230	   IF WS-ERROR = 1
035240	       MOVE ZERO	  TO WS-ERROR
035250	       GO TO BD45.
035260	     DISPLAY CLEAR-L25.
035270 BD50.
035280       MOVE W10-SELL       TO SAL-SELL.
035290 BD55.
035300       MOVE W10-QUANT      TO SAL-QNT.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
035310	     COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
035320       MOVE W25-VALUE      TO W25-CREDIT W90-VALUE.
035330       MULTIPLY -1         BY W25-VALUE.
035340       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
022330	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
035360	     DISPLAY "Discount %" AT 2518
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
035370       MOVE ZERO           TO W90-DISC W90-CDISC WS-DISC.
035380     IF DEB-DISC > ZERO
035390	       MOVE STK-LDG	  TO DPT-CODE.
035400         PERFORM READ-DEPART THRU READ-DEPART-EXIT
035410         MOVE DPT-DSETT (DEB-DISC) 
035420                           TO WS-DISC.
035430       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
022540		     DISPLAY CLEAR-L25
		     GO TO BD45
	       END-EVALUATE.
035440	   IF WS-DISC = ZERO
022540	       DISPLAY CLEAR-L25
035450         GO TO BD60.
035460	     COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
035470		    (W10-SELL * WS-DISC / 100.00000).
035480       MOVE WS-DISC        TO SAL-DSC.
035490	     COMPUTE W90-CDISC =
		   ((W90-VALUE * WS-DISC) / 100.00000).
035500	     MOVE W90-CDISC	 TO W100-PRC SAL-DISC.
035510	     DISPLAY W100-PRC AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
035520 BD60.
      *
      *    ****    A R E   D E T A I L S   C O R R E C T
      *
035630 BD62.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
019830     IF (STK-IND < 2) OR (STK-IND = 3)
035650         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
035660       ADD W90-CDISC       TO W90-LDISC.
035670       SUBTRACT W90-CDISC  FROM W90-VALUE.
035760     IF (STK-TAX = 0) OR (W90-EXP = "X")
035770         ADD W90-VALUE     TO W90-NONTAX
035780         MOVE W90-VALUE    TO SAL-NONTAX
035790     ELSE
035800         MOVE W90-VALUE    TO SAL-TAXVAL
035810         ADD W90-VALUE     TO W90-TAXVAL.
035830     IF W90-TAX NOT = SPACES
035840         COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
      *
      *    ****    A C C U M U L A T E   T O T A L   C R E D I T
      *
035860       ADD W25-VALUE       TO W90-CR.
035870 BD65.
035880       MOVE W90-CR         TO W90-TOTAL.
035890       ADD W90-LDISC       TO W90-TOTAL.
035900	     MOVE W90-CR	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
035920	     MOVE W90-LDISC	 TO W100-VAL.
023020	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
      *     IF W90-ADV = "Y"
      * 	COMPUTE W90-ADVTAX ROUNDED =
      * 	       (W90-TOTAL * WS-ADV-RATE / 100.00000).
035940*	     MOVE W05-VAT (WS-VAT-SUB)
      *				 TO W05-RATE.
035960*     IF W90-TAX = SPACES
035970* 	GO TO BD68.
035980*     IF W90-TAXVAL NOT = ZERO
035990* 	COMPUTE W25-TOTAL ROUNDED =
      * 	      ((W90-TAXVAL - W90-ADVTAX) * W05-RTE)
036010* 	MULTIPLY W25-TOTAL BY -1
036020*				 GIVING W100-VAL
023110*	     DISPLAY W100-VAL AT 2215
      *		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *			  BACKGROUND-COLOR 0.
036040*	       SUBTRACT W25-TOTAL
036050* 			  FROM W90-TOTAL.
023040	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
035970	       GO TO BD68.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BD67.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BD67.
036010	     MULTIPLY W25-TOTAL BY -1
036020				 GIVING W100-VAL.
036040	     SUBTRACT W25-TOTAL	 FROM W90-TOTAL.
023110	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

036060 BD68.
      *    IF W90-ADV = "Y"
      *        MOVE W90-ADVTAX	 TO W100-BAL
      *       DISPLAY W100-BAL AT 2241
      *	       WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *		    BACKGROUND-COLOR 0
      *       ADD W90-ADVTAX	 TO W90-TOTAL.
036070	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
036090 BD70.
036100	     DISPLAY "A" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036110		     "ccept, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036120		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036130		     "ancel > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
036140       MOVE "A" TO WS-OPTION.
036150 BD72.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
036160	     ACCEPT WS-OPTION AT 2528
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
036170			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
036230	   IF NOT (WS-OPTION = "A" OR "C")
036240         GO TO BD72.
036250	     DISPLAY CLEAR-L25.
036260     IF WS-OPTION = "C"
036270	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
036280         PERFORM BD76
036290         PERFORM BD65 THRU BD68
036300         GO TO BD35.               
036310 BD74.
             MOVE WS-SUPER       TO SAL-SUPER.
023390       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
023400       ADD 1               TO WS-LAST.
           IF W43-SERNO (1) = SPACES
               GO TO BD74E.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 1              TO WS-S1.
             MOVE SPACES         TO SAL-ITEM.
       BD74A.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SAL-MSER.
       BD74B.
             MOVE W43-SERNO (WS-S1)
                                 TO SAL-SNO (WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BD74C
               GO TO BD74E.
           IF WS-S3 < 3
               ADD 1             TO WS-S3
               GO TO BD74B.
       BD74C.
             ADD 1               TO SAL-SUB.         
021110       MOVE WS-LAST        TO WS-SALKEY.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
023400       ADD 1               TO WS-LAST.
       BD74D.
             ADD 1               TO WS-S1.
           IF WS-S1 NOT > WS-S2
               GO TO BD74A.
       BD74E.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
036340     IF LIN < 18
036350         ADD 1             TO LIN
036360         GO TO BD75B.
036370       MOVE 8              TO LIN.
036380       PERFORM BD80 UNTIL LIN > 18.
036390       GO TO BD30.
036400 BD75B.
036410	     DISPLAY "R" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036420		     "emarks, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036430		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036440		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036450		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036460		     "nd > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
036470	     MOVE WS-INPUT	 TO WS-OPTION.
036480	     ACCEPT WS-OPTION AT 2534
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
036490			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
036550	   IF NOT (WS-OPTION = "R" OR "N" OR "E")
036560         GO TO BD75B.
036570	     DISPLAY CLEAR-L25.
             MOVE WS-OPTION      TO WS-INPUT.
036580     IF WS-OPTION = "N"
036590         GO TO BD35.
036600     IF WS-OPTION = "E"
036610         SUBTRACT 1        FROM WS-LAST
036620         GO TO BD85.
036630	     MOVE SPACES	 TO W10-RMKS.
023710	     ACCEPT (LIN, 21) W10-RMKS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
036660	   IF W10-RMKS = SPACES
036670         GO TO BD75B.
023750	     DISPLAY (LIN, 2) WS-LAST
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
036690	     MOVE WS-LAST	 TO WS-SALKEY.
036700       INITIALIZE SAL-RECORD1.
036710       MOVE WS-LAST        TO SAL-CODE.
036720	     MOVE W10-RMKS	TO SAL-MSER.
036730       GO TO BD74.
036740 BD76.
036750       SUBTRACT W25-VALUE  FROM W90-CR.
036760       SUBTRACT W90-CDISC  FROM W90-LDISC.
036770       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
036780       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
019830     IF (STK-IND < 2) OR (STK-IND = 3)
036800         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
036810 BD80.
036820	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
036830       ADD 1 TO LIN.
036840 BD85.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
036850     IF WS-LAST = ZERO
036860         GO TO BD00.
      *
      *    ****    P R O C E S S   T H E   C R E D I T   N O T E
      *
036870 BD90.
036880	     DISPLAY CLEAR-L25.
036900	     DISPLAY "P" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036910		     "rocess Credit Note, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036920		     "A" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036930		     "mend sales, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036940		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036950		     "ancel or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
036960		     "M" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
036970		     "ore items"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
036980 BD95.
036990	     ACCEPT WS-OPTION AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
037050	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
037060         GO TO BD95.
037070	     DISPLAY CLEAR-L25.
037080     IF WS-OPTION = "M"
037090         ADD 1             TO WS-LAST
037100         GO TO BD40.
037110     IF WS-OPTION = "A"
037120         PERFORM BD200 THRU BD265
037130         GO TO BD90.
037140     IF WS-OPTION = "C"
037150         GO TO BD999.
037160       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
037170     IF W90-LDISC NOT = ZERO
037180         MOVE W90-LDISC    TO W90-TDISC
037190         GO TO BD105.
037200       MOVE W90-CR         TO W90-DVAL.
037210       GO TO BD105.
      *
      *     VERSION 6.0  - The following paragraph has been bypassed as
      *                    discount is only allowed per item at this 
      *                    stage.
      *
037220 BD100.
037230*      ACCEPT S26-TDSC AT LINE 21.
037240*      DISPLAY (25, 18) WS-CHK WITH FOREGROUND-COLOR 14.
037250*      ACCEPT (25, 52) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO.
037260*      CALL "_MSUPCASE" USING WS-OPTION
037270*                       LENGTH WS-OPTION
037280*                              WS-LENGTH
037290*                       LENGTH WS-LENGTH
037300*                              WS-STATUS.
037310*      DISPLAY CLEAR-L25.
037320*    IF WS-OPTION = "N"
037330*        GO TO BD100.
037340*    IF WS-TDISC = ZERO
037350*        GO TO BD105.
037360*      COMPUTE W90-TDISC = W90-DVAL * WS-TDISC / 100.
037370*      ADD W90-NONTAX W90-TAXVAL 
037380*                          GIVING W90-TOTAL.
037390*      COMPUTE WS-TAXRES = W90-TDISC / W90-TOTAL.
037400*      COMPUTE W90-VALUE = WS-TAXRES * W90-TAXVAL.
037410*      SUBTRACT W90-VALUE  FROM W90-TAXVAL.
037420*      SUBTRACT W90-VALUE  FROM W90-TDISC GIVING W90-TOTAL.
037430*      SUBTRACT W90-TOTAL  FROM W90-NONTAX.
037440 BD105.
037450       ADD W90-TDISC W90-CR 
037460                           GIVING W90-TSELL W90-TOTAL.
037470	     MOVE W90-TDISC	 TO W100-VAL.
024340	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
037490	     MOVE ZERO		 TO W90-TTAX.
	     MOVE 1		 TO WS-P1.

       BD110.
037540	   IF (W90-TAX = SPACES) OR (W90-TAXVAL(WS-P1) = ZERO) OR
	      (W90-EXP = "C" OR "X")
037570	       GO TO BD115.
023060	     MOVE W05-VAT(WS-P1) TO W05-RATE.
037590	     COMPUTE W90-TTAX ROUNDED = W90-TTAX +
		    (W90-TAXVAL(WS-P1) * W05-RTE).
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BD110.
	     MULTIPLY -1	 BY W90-TTAX.
037610	     MOVE W90-TTAX	 TO W100-VAL.
024480	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
037630	     ADD W90-TTAX	 TO W90-TOTAL.
038330 BD115.
038340	     DISPLAY CLEAR-L25.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *
038350       MOVE "M" TO WS-AGE.
           IF OPEN-ITEM
               GO TO BD118.
038380	     DISPLAY "AGE - " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038390		     "M" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038400		     "td, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038410		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038420		     "ur, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038430		     "3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038440		     "0 days, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038450		     "6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038460		     "0 days, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038470		     "9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038480		     "0 days or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038490		     "1" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
038500		     "20 days > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
038510 BD116.
038520	     ACCEPT WS-AGE AT 2557
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-AGE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
038580	   IF NOT (WS-AGE = "C" OR "M" OR "1" OR "3" OR "6" OR "9")
038590         GO TO BD116.
038600	     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   C R E D I T   N O T E
      *
038610 BD118.
	     MOVE WS-LAST	 TO W40-LAST.
	     MOVE WS-PRC	 TO W40-PRC.
	     MOVE WS-VAT-SUB	 TO W40-VAT-SUB.
	     MOVE W10-SMAN	 TO W40-SMAN.
	   IF DEB-TERMS = "3"
	       PERFORM BB000.
038630	     DISPLAY "Processing Credit Note" AT 2502 WITH
038640		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
029610	     CLOSE SALE.
             OPEN I-O SALE.
	     CALL PRG-NAME USING WS-PARID
				 LS-USER-ID
				 LS0-PROGRAMS
				 W40-COMPANY
				 W90-TRAN
		  ON EXCEPTION
		      GO TO BD119
	     END-CALL.
	     CANCEL PRG-NAME.
	     GO TO BD119A.
       BD119.
	     DISPLAY CLEAR-L25.
	     DISPLAY "Program : " AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		      PRG-NAME
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6
			   HIGHLIGHT
		     " not on disk, press "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6
				HIGHLIGHT
		     " key to continue"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       BD119A.
029690	     DISPLAY CLEAR-L25.
029610       CLOSE SALE.
029600       OPEN OUTPUT SALE.
029610       CLOSE SALE.
             OPEN I-O SALE.
029700	     DISPLAY "ENTER" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
029710		     " to continue - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029720		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
029730		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
029740	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
029800	   IF WS-OPTION = "E"
029820	       GO TO BD999.
029830	     GO TO BD00.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
       BD120A.
038770     IF W80-TYPE NOT = 1
038780         GO TO BD140.
038790       MOVE DEB-NAME       TO W60-NAME.
038800       PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
038810     IF DEB-LANG = "A"
038820         MOVE W75-ATITLE (DEB-TITLE) 
038830                           TO W60-NAME
038840     ELSE
038850         MOVE W75-ETITLE (DEB-TITLE) 
038860                           TO W60-NAME.
038870       MOVE 7              TO WS-S6.
038880 BD120.
038890     IF W60-NCHAR (WS-S6) NOT = SPACE
038900         GO TO BD125.
038910       SUBTRACT 1          FROM WS-S6.
038920     IF WS-S6 > 0
038930         GO TO BD120.
038940 BD125.
      *
      *    ****    F I R S T   I N I T I A L
      *
038950       MOVE 1              TO WS-S5.
038960       ADD 2               TO WS-S6.
038970       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
038980       ADD 1               TO WS-S6.
038990       MOVE "."            TO W60-NCHAR (WS-S6).
039000     IF W40-NCHAR (49) = SPACE
039010         GO TO BD130.
      *
      *    ****    S E C O N D   I N I T I A L
      *
039020       ADD 1               TO WS-S6.
039030       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
039040       ADD 1               TO WS-S6.
039050       MOVE "."            TO W60-NCHAR (WS-S6).
039060 BD130.
039070       ADD 2               TO WS-S6.
039080 BD135.
      *
      *    ****    S U R N A M E
      *
039090       MOVE W40-NCHAR (WS-S5) 
039100                           TO W60-NCHAR (WS-S6).
039110     IF WS-S5 < 24
039120         ADD 1             TO WS-S5
039130         ADD 1             TO WS-S6
039140         GO TO BD135.
039150       MOVE W60-NAME       TO W40-NAME.
039160 BD140.
039170       MOVE SPACES         TO W40-PC1 W40-PC2.
039180       MOVE DEB-ADDRESS    TO W60-ADDRESS.
039190       PERFORM CA175-ADDRESS.
039200     IF NOT (DEB-POST = ZERO OR 9999)
039210         MOVE DEB-POST     TO W40-PC1.
039220     IF W40-ADD4 = SPACES
039230         MOVE W40-PC1      TO W40-ADD4
039240         MOVE SPACES       TO W40-PC1.
039250     IF W40-ADD3 = SPACES
039260         MOVE W40-ADD4     TO W40-ADD3
039270         MOVE SPACES       TO W40-ADD4.
039280     IF W40-ADD2 = SPACES
039290         MOVE W40-ADD3     TO W40-ADD2
039300         MOVE SPACES       TO W40-ADD3.
039310     IF W40-ADD1 = SPACES
039320         MOVE W40-ADD2     TO W40-ADD1
039330         MOVE SPACES       TO W40-ADD2.
039340       MOVE DEB-PADDR      TO W60-ADDRESS1.
039350       PERFORM CA185-ADDRESS.
039360     IF NOT (DEB-PPOST = ZERO OR 9999)
039370         MOVE DEB-PPOST     TO W40-PC2.
039380     IF W40-PADD4 = SPACES
039390         MOVE W40-PC2      TO W40-PADD4
039400         MOVE SPACES       TO W40-PC2.
039410     IF W40-PADD3 = SPACES
039420         MOVE W40-PADD4    TO W40-PADD3
039430         MOVE SPACES       TO W40-PADD4.
039440     IF W40-PADD2 = SPACES
039450         MOVE W40-PADD3    TO W40-PADD2
039460         MOVE SPACES       TO W40-PADD3.
039470     IF W40-PADD1 = SPACES
039480         MOVE W40-PADD2    TO W40-PADD1
039490         MOVE SPACES       TO W40-PADD2.
       BD145.
	     EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
042910 BD200.
	     DISPLAY "Amend " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "H" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "eader, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "S" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ales details or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "xit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "S"            TO WS-OPTION.
       BD201.
             ACCEPT WS-OPTION AT 2539
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "H" OR "S")
               GO TO BD201.
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "S"
               GO TO BD202
           ELSE
           IF WS-OPTION = "E"
               GO TO BD265.
       BD201A.
	     DISPLAY "Amend " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "O" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "rd No, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "R" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ef No, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "S" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "alesman or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "xit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE SPACE          TO WS-OPTION.
       BD201B.
             ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "O" OR "R" OR "S")
               GO TO BD201B.
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "E"
               GO TO BD265.
           IF WS-OPTION = "O"
017660	       ACCEPT W90-ORD AT 0368
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0 UPDATE AUTO
	       GO TO BD201B
           ELSE
           IF WS-OPTION = "R"
	       ACCEPT W90-REG AT 0554
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0 UPDATE AUTO
	       GO TO BD201B.
       BD201C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
017720	   IF W10-SMAN > 50
017730         GO TO BD201C.
             MOVE SPACES         TO W40-ASSIST.
017740     IF W10-SMAN = ZERO
               DISPLAY "                              "  AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BD201B.
017820	     DISPLAY CLEAR-L25.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790         DISPLAY "Invalid Salesman Number" AT 2518
017800			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
017810         GO TO BD201C.
017820	     DISPLAY CLEAR-L25.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
	     MOVE PAR-SALESMAN	 TO W40-ASSIST.
             GO TO BD201A.
       BD202.
042920       MOVE 8              TO LIN.
042930       PERFORM BD80 UNTIL LIN > 18.
042940       MOVE 8              TO LIN.
042950       MOVE 1              TO WS-LINES WS-FIRST.
042960 BD205.
042970       MOVE WS-FIRST       TO WS-SALKEY.
042980 BD210.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
042990     IF WS-SALKEY > WS-LAST
043000         GO TO BD215.
043010       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030090	     DISPLAY (LIN, 2) SAL-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
043030	   IF SAL-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
043050	       GO TO BD212.
030130	     DISPLAY ( , 6) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030140	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
043080	     MOVE SAL-QNT	 TO W10-QUANT.
043090       DISPLAY S26-QNT AT LINE LIN.
043100       MOVE SAL-SELL       TO W10-SELL.
043110       DISPLAY S26-PRC AT LINE LIN.
043120       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BD212.
043140     IF LIN < 18
043150         ADD 1             TO LIN WS-SALKEY WS-LINES
043160         GO TO BD210.
043170 BD215.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
029400     IF WS-LINES < 11
029410         IF WS-FIRST = 1
029420		   DISPLAY "A" AT 2520
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029440         ELSE
029450		   DISPLAY "A" AT 2502
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029480             MOVE 2        TO WS-LINES
029490     ELSE
029500     IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029520		   DISPLAY "A" AT 2506
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029550             MOVE 1        TO WS-LINES
029560         ELSE
029570		   DISPLAY "A" AT 2504
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029600             MOVE 3        TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029630	       DISPLAY "A" at 2502
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "mend, "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "D" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "elete an Item - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "P" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "revious screen - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "xit > <"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
043440 BD220.
043450	     ACCEPT WS-OPTION AT 2550
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
043510	   IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
043520         GO TO BD220.
043530     IF WS-OPTION = "E"
043540         GO TO BD265.
043550     IF WS-OPTION = "A" OR "D"
043560         GO TO BD230.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
043570     IF WS-LINES = 2 OR 3
043580         IF WS-OPTION = "P"
043590             PERFORM BD225
043600		   DISPLAY CLEAR-L25
043610             SUBTRACT 11   FROM WS-FIRST
043620             GO TO BD205.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
043630     IF WS-LINES = 1 OR 3
043640         IF WS-OPTION = "N"
043650             PERFORM BD225
043660		   DISPLAY CLEAR-L25
043670             ADD 11        TO WS-FIRST
043680             GO TO BD205.
043690	     GO TO BD215.
043700 BD225.
      *
      *    ****    C L E A R   S C R E E N
      *
043710       MOVE 8              TO LIN.
043720       PERFORM BD80 UNTIL LIN > 18.
043730       MOVE 8              TO LIN.
043740       MOVE 1              TO WS-LINES.
043750 BD230.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
043760     IF WS-OPTION = "A"
043770         GO TO BD240.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
043910	     DISPLAY CLEAR-L25.
043780       DISPLAY "Are you sure Y/N?" AT 2501
043790		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
043800       MOVE "N"            TO WS-OPTION.
043810 BD235.
043820	     ACCEPT WS-OPTION AT 2519
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
043830			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
043890	   IF NOT (WS-OPTION = "N" OR "Y")
043900         GO TO BD235.              
043910	     DISPLAY CLEAR-L25.
043920     IF WS-OPTION = "N"
043930	       GO TO BD215.
043940       MOVE "D"            TO WS-OPTION.
043950 BD240.
043960       DISPLAY "Enter item No to Amend/Delete >  <" AT 2501
043970		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
043980       MOVE ZERO           TO WS-INO.
043990 BD245.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
044000	     ACCEPT WS-INO AT 2532
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
044010                                  UPDATE AUTO.
044020     IF WS-INO = ZERO
044030	       DISPLAY CLEAR-L25
044040	       GO TO BD215.
044050     IF WS-INO > WS-SALKEY
044060         GO TO BD245.
044070     IF WS-INO < WS-FIRST
044080         GO TO BD245.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
044090     IF WS-INO < 13
044100         ADD 7 WS-INO      GIVING LIN
044110     ELSE
044120         COMPUTE LIN = WS-INO - WS-FIRST + 8.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
044130       MOVE WS-INO         TO WS-SALKEY.
044140       PERFORM READ-SALE THRU READ-SALE-EXIT.
044150       MOVE SAL-ITEM       TO W10-ITEM STK-CODE.
044160       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
044170       MOVE SAL-QNT        TO W10-QUANT.
044180       MOVE SAL-COST       TO W10-COST.
044190       MOVE SAL-SELL       TO W10-SELL.
044200       MOVE SAL-DISC       TO W90-CDISC.
044210       MOVE SAL-DSC        TO WS-DISC.
044220       MOVE SAL-VAL        TO W25-VALUE.
      *
      *    ****    R E D U C E   T O T A L S 
      *
044230       SUBTRACT W25-VALUE  FROM W90-CR.
044240       SUBTRACT W90-CDISC  FROM W90-LDISC.
044250       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
044260       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
019830     IF (STK-IND < 2) OR (STK-IND = 3)
044280         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
044290     IF WS-OPTION = "A"
044300         GO TO BD255.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
044310       PERFORM BD65 THRU BD68.
044320 BD250.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
044330       MOVE WS-SALKEY      TO WS-SALSTR.
044340       ADD 1               TO WS-SALKEY.
044350     IF WS-SALKEY > WS-LAST
044360         SUBTRACT 1        FROM WS-LAST
044370         GO TO BD202.
044380       PERFORM READ-SALE THRU READ-SALE-EXIT.
044390       SUBTRACT 1          FROM SAL-CODE.
044400       MOVE WS-SALSTR      TO WS-SALKEY.
044410       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
044420       ADD 1               TO WS-SALKEY.
044430       GO TO BD250.
044440 BD255.
044450     IF SAL-ITEM = SPACES
031590	       ACCEPT (LIN, 21) SAL-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
044480	     GO TO BD262.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
044490       ACCEPT S26-QNT AT LINE LIN.
044500	     DISPLAY CLEAR-L25.
044510     IF W10-QUANT = ZERO
044520         DISPLAY "Quantity MAY NOT be zero" AT 2501
044530			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
044540         MOVE SAL-QNT      TO W10-QUANT
044550         GO TO BD255.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
044560       MULTIPLY W10-SELL BY W10-QUANT 
044570                           GIVING W25-VALUE.
044580       MOVE W25-VALUE      TO W100-EXT.
031720	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
044600	     DISPLAY WS-CHK AT 2518
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044610	     ACCEPT WS-OPTION AT 2552
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
044670	     DISPLAY CLEAR-L25.
044680     IF WS-OPTION = "N"
044690         GO TO BD255.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
044700       MOVE W10-QUANT      TO SAL-QNT.
044710       MULTIPLY -1         BY W25-VALUE.
044720       MOVE W25-VALUE      TO SAL-VAL.
044730     IF WS-DISC = ZERO
044740         GO TO BD260.
044750	     COMPUTE W90-CDISC = ((W25-VALUE * WS-DISC) / 100) * -1.
044760       MOVE W90-CDISC      TO SAL-DISC.
044770 BD260.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
044780       PERFORM BD62 THRU BD68.
044790 BD262.
044800       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
044810       GO TO BD202.
044820 BD265.
044830       EXIT.
       BD999.
	     EXIT.
      /
052700 ZA000-INIT	       SECTION 9.
052710 ZA000-OPEN.
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
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
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INV	 TO PRG-NUMBER.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
054270       OPEN OUTPUT SALE.
054280       CLOSE SALE.
054290       OPEN I-O SALE.
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
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	9.
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
056320         MOVE W02-NETWORK TO WS-FILE
056330         MOVE WS-NETKEY TO WS-KEY
056380     ELSE
056390     IF WS-F-ERROR = 6
056400         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420     ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056500	   ELSE
056510     IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
056580	   ELSE
056590     IF WS-F-ERROR = 19
056600         MOVE W02-SALE TO WS-FILE
056610         MOVE WS-SALKEY TO WS-KEY
056620     ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840     ELSE
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
