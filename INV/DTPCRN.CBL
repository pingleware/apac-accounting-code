      $SET LINKCOUNT"1024"
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
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      *  Jun 2008	- Include user number and name on the invoice  *
      * 		  header record to enable lookup of person     *
      * 		  who captured the Invoice/Credit note.        *
      * 							       *
      * August 2008    - New file (STKALT) - Stock alternate index     *
      * 		 included for lookups, using any word con-     *
      * 		 tained in the Stock description.	       *
      * 							       *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
      * 	    Sales Ledger code. Included two additional Debtor  *
      * 	    discount codes (11 and 12) each with a field to    *
      * 	    define which debtor discount code uses this as an  *
      * 	    alternate discount. The relevant Stock Item will   *
      * 	    have an indicator field to instruct the system to  *
      * 	    use the alternate discount. The alternate discount *
      * 	    is only applicable if the discount code for which  *
      * 	    this alternate discount is applicable is the code  *
      * 	    on the Debtor account record.		       *
      * 							       *
      *  Jan 2012 - Include Format 99 which calls the CRNF12W program  *
      * 	    to produce a Credit note via the Windows spooller. *
      * 							       *
      ******************************************************************
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPCRN.
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
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CARDEX.SL.

000150 COPY CONTROL.SL.

000140 COPY DEBALT.SL.

000190 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000210 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000270 COPY PARAM.SL.

000310 COPY SALE.SL.

000330 COPY STOCK.SL.

000140 COPY STKMEM.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CARDEX.FDE.

000440 COPY CONTROL.FDE.

000310 COPY DEBALT.FDE.

000480 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000500 COPY DEPART.FDE.

000250 COPY GARTEE.FDE.

000560 COPY PARAM.FDE.

000600 COPY SALE.FDE.

000620 COPY STOCK.FDE.

       COPY STKMEM.FDE.

       COPY WARHSE.FDE.

       COPY WSTOCK.FDE.

      /
002130 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002140 77  WS-DISC	   PIC	99V99	 COMP-3.
002150 77  WS-TDISC        PIC  99V99    COMP-3.
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
002450 77  WS-PRN1         PIC  9(01) VALUE 0.  
002460 77  WS-PRN2         PIC  9(01) VALUE 0.  
       77  WS-NUM          PIC  Z9.
       77  WS-CARDEX	   PIC	X(01).
002480 77  WS-START        PIC  9(08).
002490 77  WS-END          PIC  9(08).
002550 77  WS-PAR          PIC  9(01) VALUE 0.
002560 77  WS-INO          PIC  9(02) VALUE 0.
002570 77  WS-LINES        PIC  9(02) VALUE 0.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
       77  WS-SALPOS	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002600 77  WS-LASTINV      PIC  9(04)    COMP-5.
002630 77  WS-L1           PIC  9(04)    COMP-5.
002750 77  WS-OPTION	   PIC	X(01).
002770 77  WS-SKIP         PIC  X(01) VALUE " ".
002780 77  WS-LOOK         PIC  X(01) VALUE "S".
002790 77  WS-ETYPE        PIC  X(01) VALUE "I".
002820 77  WS-ERROR        PIC  9(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000210 77  WS-C-FACT	   PIC	X(01).
002860 77  WS-COMP	   PIC	9(01).
002890 77  WS-HEAD         PIC  X(01).
000750 77  WS-TYP	   PIC	X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE	   PIC	X(01).
002920 77  WS-PRC          PIC  X(01).
002920 77  WS-INPUT        PIC  X(01) VALUE "N".
002920 77  WS-COPY         PIC  X(01) VALUE "N".
002920 77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
       77  WS-MEASURE	   PIC	X(01) VALUE "N".
002950 77  WS-BATCH        PIC  9(05).
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
002980 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
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
       01  PRG-NAME3.
	   03  FILLER	   PIC	X(08) VALUE "INV\CRNF".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.

003050 01  WS-DB-LINE.
003060     03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "фммммммммммммммямммямммммм
      -        "мммммммммммммммммммммммяммммммммямммммммммямммммммммм╣".
003080     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "зддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "Ё              Ё   Ё
      -        "                       Ё        Ё         Ё          Ё".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "фммммммммммммммомммомммммм
      -        "мммммммммммммммммммммммоммммммммомммммммммомммммммммм╣".
003200     03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "юддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддды".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "фммммммммммммммммямммммммм
      -        "ммммммммммммммммммммммяммммммммммямммммммммяммммммммм╣".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "Ё                Ё
      -        "                      Ё          Ё         Ё         Ё".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "фммммммммммммммммомммммммм
      -        "ммммммммммммммммммммммоммммммммммомммммммммоммммммммм╣".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "фммммммямммммммммммммммммм
      -        "ммммммммммммммммммммммямммммммммммяммммммммммяммммммм╣".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "Ё      Ё
      -        "                      Ё           Ё          Ё       Ё".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "фммммммомммммммммммммммммм
      -        "ммммммммммммммммммммммомммммммммммоммммммммммоммммммм╣".
	       03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "м".

003480 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "CRN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003700 COPY CARDEX.ID.

003700 COPY CONTROL.ID.

       COPY DEBALT.ID.

003740 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY PARAM.ID.

003840 COPY SALE.ID.

003860 COPY STOCK.ID.

000140 COPY STKMEM.ID.

       COPY WARHSE.ID.

       COPY WSTOCK.ID.

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
004750	       05  W25-WHOLE   PIC 9(05).
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
005860	   03  W75-DNO	      OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).
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

007910 01  S07.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S07-3.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY DEBLUP.CRT.

008990 01  S25.
	   03  LINE  7 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "зддддддддддддддддддддддддддддддддддддддддддддддддддд©".
009000	   03  LINE  8 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "ЁAddresses:                 ".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Balance  ".
009010	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-BAL.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009030	   03  LINE  9 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё 1 ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009060	   03  LINE 10 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009090	   03  LINE 11 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009120	   03  LINE 12 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009150	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009180	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё 2 ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009210	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
009240	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 VALUE "    Ё".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 VALUE "   Ё".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё   ".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "Ё".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "юддддддддддддддддддддддддддддддддддддддддддддддддддды".

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
				 BACKGROUND-COLOR 5
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009720 01  S26-AVGE.
009730	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009740				 PIC Z(04)9.999 USING STK-AVGE AUTO.

       01  S26-DATE.
	   03  LINE  4 COLUMN 70 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  99/99/9999 USING W21-DATE AUTO.

009840 01  S027-CNOTE.
009850     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 
009860                           VALUE "CREDIT NOTE".
	   03  LINE  4 COLUMN 60 FOREGROUND-COLOR 3
				 BACKGROUND-COLOR 1
				 VALUE "Inv. Date╟".

009870 01  S28.
009880	   03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009890                           PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "з".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "©".
007450	   03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 VALUE "CLIENT DETAILS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Name    >".
	   03	       COLUMN 65 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Address >".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
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
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
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
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
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
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
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
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "ю".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "ы".

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
				 PIC X(08) USING W40-PC1 AUTO.

009930 01  S36.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.
009930 01  S38.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC ZZZZ9 USING W10-CASES AUTO.

       COPY WARLUP.CRT.

       COPY S99.CRT.

       01  S-MEASURE.
	   03  LINE  4 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "з".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "©".
007450	   03  LINE  5 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(04) FROM WS-SPACE.
	   03	       COLUMN 20 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 VALUE "MEASUREMENTS".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 PIC  X(03) FROM WS-SPACE.
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE  6 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Length  >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
007450	   03  LINE  7 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Breadth >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Pieces  >    <".
007460	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 VALUE "     Ё".
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "ю".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "ы".

       01  SA-MEASURE.
	   03  LINE  6 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
				 PIC ZZZ9.9999 USING W10-LENGTH AUTO.
	   03  LINE  7 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 15
				 PIC ZZZ9.9999 USING W10-BREADTH AUTO.
	   03  LINE  8 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZ9 USING W10-NUMBER AUTO.

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
      *	   *************************************************************
      *      To control Credit Note security it is necessary to use
      *      two of the levels in the System.
      * 	A Debtor level of 3 will allow credit notes.
      * 	Where the debtor level is less than 3, but not less
      *		than 1, then by giving the user a sales level of 2,
      * 	credit notes will be enabled.
      *    *************************************************************
003240	   IF (LS0-DBLEV < 3) AND
	      (LS0-DBLEV < 1 OR LS0-SALEV < 2)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AZ100.
010250	     PERFORM ZA000-INIT.
016330	     PERFORM BD000.
	     CLOSE SALE.

013280 AZ100.
013290       EXIT PROGRAM.

       COPY AA50DSW.LUP.

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

010930 COPY LOCKED.RC2.

       COPY OPTION.CRT.

      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P	O P T I O N S
      *    *************************************************************
010990 AA300		  SECTION 51.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011020		     "=Item,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011040		     "=Alt,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     "=Desc,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     "=Desc2,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011080		     "=X/Ref,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011080		     "=Word,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to end"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       R E A D	 F I L E S   R O U T I N E S	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
011110 AC000		  SECTION 52.

011150 COPY CONTROL.RD.

       COPY DEBALT.RD.

011190 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011270 COPY PARAM.RD.

011290 COPY SALE.RD.

000140 COPY STKMEM.RD.

011310 COPY STOCK.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁR E W R I T E   &	W R I T E   F I L E S	R O U T I N E SЁ
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
011350 AD000-WRITE	       SECTION 53.

011530 COPY SALE.WR.

011970 COPY DEBTOR.ALU.

011630 COPY DEBTOR.ILU.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

      *    *************************************************************
      *    ****      Read debtor record - using account number.	    ****
      *    *************************************************************
013750 CA155-GET-DEBTOR  SECTION 54.
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
013940 CA165-NAME	 SECTION 54.
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
014040 CA175-ADDRESS	 SECTION 54.
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
014150 CA185-ADDRESS	 SECTION 54.
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

       COPY CA500WH.STK.

      /
       BB000		   SECTION 55.
       BB00.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.

       BB05.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BB00.
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
       BB030	       SECTION 56.
       BB30.
             MOVE ZERO           TO WS-ERROR.
	     MOVE 1		 TO WS-S5.
	     MOVE W10-QUANT	 TO WS-S6.
             MOVE SPACES         TO W43-SERIALS.
	     PERFORM SAVE-SCREEN-3.
       BB40.
	     DISPLAY (LIN, 43) "Serial No:"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 7
			"                    (   )"
			 WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
       BB41.
	     MOVE WS-S5 	 TO W100-SER.
	     DISPLAY (LIN, 74) W100-SER
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
       BB42.
	     ACCEPT (LIN, 54) W43-SERNO (WS-S5)
                              WITH FOREGROUND-COLOR 0 
                                   BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W43-SERIALS
		     GO TO BB45
	       END-EVALUATE.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB42.
	   IF W43-SERNO (WS-S5) = SPACES
               GO TO BB42.
             MOVE STK-CODE       TO GUA-CODE.
	     MOVE W43-SERNO (WS-S5)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
	   IF WS-F-ERROR = 9
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
      *    ****    G E T   M E A S U R E M E N T S   (LENGTH/BREADTH)
      * 	   A N D   N U M B E R	 O F   P I E C E S
      *
       BB050	       SECTION 57.
       BB50.
034820	     DISPLAY CLEAR-L25.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
019090	     DISPLAY S-MEASURE.
	     PERFORM SCREEN-SHADOW.
       BB55.
	     ACCEPT SA-MEASURE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BB55.
	     COMPUTE W10-QUANT ROUNDED = W10-LENGTH * W10-BREADTH
				       * W10-NUMBER.
	     MOVE 1		 TO WS-P1.
	     MOVE "Y"		 TO WS-MEASURE.

       BB60.
	     PERFORM RESTORE-SCREEN-2.

       BB69.
             EXIT.

      /
017970 BB100		   SECTION 58.
017980 BB100-MARG.
017990	     MOVE ZERO		 TO WS-ERROR.
	     MOVE W10-SPAPRC	 TO W10-ORDER.
018010	   IF (W90-TAX = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
018020	       IF (STK-TAX NOT = ZERO) AND
		  (STK-EXEMPT = "N" OR DEB-ART11 = "N")
018030		   COMPUTE W10-ORDER ROUNDED = (W10-SPAPRC /
018040                (100.0000 + W05-RATE)) * 100. 
	   IF (WS-PRC = "1") AND (STK-SL(WS-P1) NOT > W10-ORDER)
015720	       GO TO BB110
           ELSE
	   IF (WS-PRC = "2") AND (STK-CSH(WS-P1) NOT > W10-ORDER)
015720	       GO TO BB110
           ELSE
018050	   IF STK-WSL(WS-P1) NOT > W10-ORDER
018060	       GO TO BB110.
018070	     COMPUTE W10-MARG = ((W10-ORDER - W10-COST) / W10-ORDER)
018080                          * 100.00.
018090     IF W10-MARG NOT < W85-MARG
018100         GO TO BB110.

018130 BB105.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
018200	   IF NOT (WS-OPTION = "N" OR "Y")
018210         GO TO BB105.
018220     IF WS-OPTION = "N"
018230	       MOVE 1		 TO WS-ERROR
018240         GO TO BB110.

018250 BB106.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-CSHPASS.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддд" AT 1128
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "©" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "Ё " AT 1228 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ю" AT 1328 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1245
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     PERFORM RESTORE-SCREEN.
018360	     MOVE 1		 TO WS-LENGTH.

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
018650	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 1		 TO WS-S9.

       BD03.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BD03.
018660	     MOVE ZERO		 TO W90-BDT W90-SALES
018670				    W90-BAL W90-LDISC
018680				    W90-CUR W90-NONTAX
				    W40-PARCELS W90-EXCTOT
				    W90-BCR.
032520	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
032530	     MOVE SPACES	 TO W90-TAX W40-SPAR.
032540       MOVE "C"            TO WS-CR.
             MOVE "N"            TO WS-INPUT.

032550 BD05.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
032600	     MOVE SPACES	 TO W80-ACNO.

032610 BD06.
032620	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
032630		     "=Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Debtor Lookup (Any Word)"
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
016560		     PERFORM AA50D THRU AA59
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
032730	       GO TO BD150.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

032740 BD08.
032760       PERFORM CA155-GET-DEBTOR.
032770     IF WS-ERROR = 1
032780	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
032790	       GO TO BD06.
032800     IF WS-ERROR = 2
032810	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
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
			  BACKGROUND-COLOR 5.
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
032920	     MOVE "'C'ontinue, 'R'ekey, 'E'xit, 'F9' Memo/Remarks"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

032980 BD09.
	     PERFORM OPT-MESSAGE.
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
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280	     DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     PERFORM RESTORE-SCREEN-3.

       BD09E.
	     EXIT.

       BD09-E.
             GO TO BD08A.

       BD09O.
033050	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
033060	       GO TO BD08A.
	     PERFORM RESTORE-SCREEN-2.
019690	     DISPLAY CLEAR-L25.
033090     IF WS-OPTION = "R"
033100         GO TO BD00.
033110     IF WS-OPTION = "E"
033130	       GO TO BD150.
           IF NOT VALID-PRICE
	       MOVE "1" 	 TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.

033250 BD09P.
033340	     MOVE SPACES	 TO W90-ORD W90-REG.

033350 BD10.
033360	     DISPLAY CLEAR-L25.
019960	     ACCEPT W90-ORD AT 0368
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
033400	     MOVE DEB-SMAN	 TO W10-SMAN.

033430 BD15.
	   IF W10-SMAN = ZERO
020030	       ACCEPT W10-SMAN AT 0411
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO.
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
033530	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033550         GO TO BD15.
             MOVE PAR-SALESMAN   TO W40-ASSIST.

       BD15A.
033570	     DISPLAY W40-ASSIST AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       BD16.
	     ACCEPT S26-DATE.
	     MOVE W21-DATE	 TO W10-EDTE.
008400       UNSTRING W10-DATE DELIMITED BY "/" OR " "
008410	       INTO W20-DD, W20-MM, W20-CCYY
008420	       OVERFLOW GO TO BD18.
008430       MOVE ZERO           TO WS-ERROR.
008440     IF W20-DATE NOT NUMERIC
008450	       GO TO BD18.
008460	     MOVE W20-DATE	 TO W22-DATE.
008470     IF W20-DAY < 01 OR > 31
008480	       GO TO BD18.
008490     IF W20-MONTH < 01 OR > 12
008500	       GO TO BD18.
008510     IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
008520	       GO TO BD20.
008530     IF W20-MONTH = 02 AND W20-DAY > 28
008540	       GO TO BD17.
008550     IF W20-DAY > 30
008560	       GO TO BD18.
008570	     GO TO BD20.
      
008580 BD17.
008590     IF W20-DAY > 29
008600	       GO TO BD18.
008610	     DIVIDE W20-CNYR BY 4 GIVING W25-RESULT.
008620	   IF W25-DECIMAL = 0
008630	       GO TO BD20.
      
008640 BD18.
008650	     MOVE "Invalid date" TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BD16.

033580 BD20.
	     PERFORM CHECK-CORRECT.
033660	   IF WS-OPTION = "N"
033670         GO TO BD15.

       BD21.
020300	   IF DEB-INV-TYPE = "E"
020310         MOVE "Y"      TO WS-OPTION
	   ELSE
	   IF DEB-INV-TYPE = "I"
020330	       MOVE "N"      TO WS-OPTION
	   ELSE
	       MOVE "X"      TO WS-OPTION.

       BD22.
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
		     GO TO BD15
	       END-EVALUATE.
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
			 BACKGROUND-COLOR 5 UPDATE AUTO.

034160 BD25.
020760	     ACCEPT W90-REG AT 0554
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD25
	       END-EVALUATE.
034190	     DISPLAY CLEAR-L25.
034300	     MOVE ZERO		 TO W90-DB W90-CR.
	     MOVE SPACES	 TO W40-CODE.
	   IF NOT (LS0-WHS = 2)
	       GO TO BD30.

       BD28.
             MOVE "Warehouse sales (Y/N)?  [N]"
                                 TO WS-ERR-MES.
             MOVE "N"            TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO BD25
               END-EVALUATE.
           IF NOT (WS-OPTION = "Y" OR "N")
               GO TO BD28.
           IF WS-OPTION = "Y"
               PERFORM HA000.

034310 BD30.
034320	     MOVE 8		 TO LIN.
      *    *************************************************************
      *    ****   I N I T I A L I Z E	W O R K   V A R I A B L E S
      *    *************************************************************
034330 BD35.
034340	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM W10-SERNO.
	   IF W10-STCK = "N"
	       MOVE ZERO	 TO W10-I14.
034350	     MOVE ZERO		 TO W10-SELL WS-SUPER W10-CASES.
034350	     MOVE 1		 TO W10-QUANT WS-P1 W10-QNT.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P	O P T I O N S
      *    *************************************************************
034360 BD40.
034370       PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is an 18 character stock/inventory item code being used?  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF WS-EXT-STK = "Y"
      *
      *    ***** Extended (18 character) stock item code
      *
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    **** 	A L P H A - N U M E R I C   C O D E	    ****
      *    *************************************************************
	   IF W10-STCK = "A"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is a 3 character extension code being used in addition to Ё
      *    Ё the 14 character stock/inventory item code?	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      *    **** NUMERIC CODES APPLY ****
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check the length of the numeric Stock/Inventory item code Ё
      *    Ё being used and allow the User to key in the code.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF W10-SLNGTH = 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   3 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   4 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   5 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   6 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   7 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   8 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   9 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   10 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   11 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   12 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   13 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   14 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
021050		     SUBTRACT 1  FROM WS-LAST
		     GO TO BD85
		 WHEN F1-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Apac Help Routine				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Lookup using 1 or more characters of Item Code      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё   Lookup using 1 or more characters of Alternate Code     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   Lookup using 1 or more characters of Item Description   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Lookup using 1 or more chars of Item Description 2  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё	   Lookup using 1 or more characters of X-Ref Code     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё  Lookup using any word or part in the Stock Description.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD40
	       END-EVALUATE
      *    *************************************************************
      *    *****    S T O C K / I N V E N T O R Y   L O O K U P	   *****
      *    *************************************************************
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF (W10-STCK = "A") AND
		  (WS-USE-ITM = "Y")
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
       BD40A.
034450	   IF W10-EXT-ITEM = SPACES
034470	       GO TO BD40.

       BD41.
034480       PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF (W10-STCK = "A") AND
	      (WS-USE-ITM = "Y")
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
034490	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF (W10-STCK = "A") AND
		  (WS-USE-ITM = "Y")
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BD40
               ELSE
                   GO TO BD41.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD40.
	   IF WS-F-ERROR = 52
               MOVE "No Record at W/H selected"
                                 TO WS-ERR-STRING
003260         PERFORM ERROR-MESSAGE
020140         GO TO BD40.
034520	     MOVE WS-LAST	 TO WS-SALKEY.
034530       INITIALIZE SAL-RECORD1.
034540       MOVE WS-LAST        TO SAL-CODE.
      *    *************************************************************
      *    ****    I T E M   N U M B E R   O N   C R E D I T   N O T E
      *    *************************************************************
034560	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
021160	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
034580 BD44.
034590     IF STK-IND = 3
034600         MOVE ZERO         TO STK-AVGE
021200	       ACCEPT (LIN, 21) W10-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
034630	       DISPLAY "Index Unit cost" AT 2512
034640			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
034650         ACCEPT S26-AVGE AT LINE 25
034660     ELSE
034670         MOVE STK-DESC     TO W10-DESC
021270	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
034690	   IF STK-IND = 3
034700         IF STK-AVGE = ZERO
034710		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
034730             GO TO BD44.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Until the condition that occasionaly causes the Average   Ё
      *    Ё to be negative is identified and corrected, the following Ё
      *    Ё precaution will remain in place.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (STK-AVGE < ZERO) OR
	      (STK-AVGE > (STK-COST * 2))
	       MOVE STK-COST	 TO STK-AVGE.
034740	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
       BD44-RESTART.
034750	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
034760       MOVE W10-DESC       TO SAL-MSER.
	     MOVE ZERO		 TO SAL-CASES.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
	     MOVE ZERO		 TO W10-LENGTH W10-BREADTH W10-NUMBER.
	     MOVE "N"		 TO WS-MEASURE.
      *    *************************************************************
      *    ****    T I M E   S T A T I O N E R S
      *    *************************************************************
	   IF PRG-NUMBER = 14
	       DISPLAY "╣" AT 0723
		       "Reference " WITH BACKGROUND-COLOR 5
					 FOREGROUND-COLOR 6 HIGHLIGHT
			STK-ACODE WITH BACKGROUND-COLOR 5
				       FOREGROUND-COLOR 7 HIGHLIGHT
		       "ф".
	     MOVE 1		 TO WS-P1.
	   IF (LS0-LEV < 4) OR
	      (STK-USE-PACKS = "N")
	       GO TO BD44A.
	     DISPLAY "╣" AT 0750
		      STK-PER WITH BACKGROUND-COLOR 5
				   FOREGROUND-COLOR 6 HIGHLIGHT
		     "ф".
	     GO TO BD44A.

       BD44-PACK.
	   IF NOT (STK-UNT(WS-P1) = ZERO)
	       DISPLAY STK-PR(WS-P1) AT 0751
		       WITH BACKGROUND-COLOR 5
			    FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	   IF WS-P1 = 1
	       DISPLAY "  Each  "  AT 0751
			WITH BACKGROUND-COLOR 5
			     FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	       SUBTRACT 1	 FROM WS-P1
	       GO TO BD44-PACK.

       BD44A.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to key in "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "- Cutting size (Wood, Glass etc) "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "or "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 1.
	   IF LS0-LEV > 3
	       IF STK-USE-PACKS = "Y"
		   DISPLAY "" AT 2560
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
			   " for Packs"
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1.
034780	     ACCEPT S26-QNT AT LINE LIN.
	   IF ADIS-FUNC
	       IF LS0-LEV < 3
		   GO TO BD44A
	       END-IF
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-P1 > 1
			 SUBTRACT 1  FROM WS-P1
			 GO TO BD44-PACK
		 WHEN DOWN-KEY
		     IF WS-P1 < 4
			 ADD 1	     TO WS-P1
			 GO TO BD44-PACK
	       END-EVALUATE
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY (LIN, 2) WS-MIV78
			     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
      *    *************************************************************
      * 	     ****    T I M E   S T A T I O N E R S
      *    *************************************************************
		     IF PRG-NUMBER = 14
			 DISPLAY "мммммммммммммммммммммм" AT 0723
				  WITH BACKGROUND-COLOR 1
				       FOREGROUND-COLOR 3
		     END-IF
021400		     GO TO BD35
		 WHEN F6-KEY
		     PERFORM BB050
021370		     DISPLAY S26-QNT AT LINE LIN
		 END-EVALUATE.
034790	   IF (W10-QUANT = ZERO)
034800	       DISPLAY (LIN, 2) WS-MIV78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
034810         GO TO BD35.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
	   IF STK-USE-CASES = "Y" AND WS-MEASURE = "N"
	       PERFORM JA000
	       IF W10-CASES = ZERO
		   GO TO BD44-RESTART.
      *    *************************************************************
      *    ****    T I M E   S T A T I O N E R S
      *    *************************************************************
	   IF PRG-NUMBER = 14
	       DISPLAY "мммммммммммммммммммммм" AT 0723
			WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
034820	     DISPLAY CLEAR-L25.
             MOVE ZERO           TO WS-ERROR.
           IF STK-GUAR NOT = ZERO
               PERFORM BB030
           ELSE
               MOVE SPACES       TO W43-SERNO (1).
           IF WS-ERROR = 1
               GO TO BD44A.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E 
      *    *************************************************************
	   IF WS-P1 > 1
	       MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
				 GIVING SAL-COST W10-COST
	   ELSE
021360	       MOVE STK-AVGE	 TO SAL-COST W10-COST.
	     MOVE W90-TAX	 TO W10-ENTRY.
      *    *************************************************************
      *    ****    C H E C K   I F   A R T 1 1 / 1 G   A P P L I E S
      *    *************************************************************
	   IF (STK-EXEMPT = "Y" AND DEB-ART11 = "Y")
	       MOVE ZERO	 TO WS-VAT-SUB
	       MOVE "X" 	 TO W90-TAX
	   ELSE
	       MOVE STK-TAX	 TO WS-VAT-SUB.
	   IF WS-VAT-SUB > 0
	       IF W22-DATE < WS-VAT-DATE(WS-VAT-SUB)
		   ADD 6	 TO WS-VAT-SUB.
034830	   IF STK-IND < 3
034960         IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
034970		   IF WS-PRC = "1"
021570		       MOVE STK-SL(WS-P1)
034990				 TO W10-SELL
035000             ELSE
035010		   IF WS-PRC = "2"
021610		       MOVE STK-CSH(WS-P1)
035030				 TO W10-SELL
035040             ELSE
021640		       MOVE STK-WSL(WS-P1)
035060				 TO W10-SELL
035070         ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
035080		   IF WS-PRC = "1"
021680		       MOVE W10-VSL(WS-P1)
035100				 TO W10-SELL
035110             ELSE
035120		   IF WS-PRC = "2"
021720		       MOVE W10-VCSH(WS-P1)
035140				 TO W10-SELL
035150             ELSE
021750		       MOVE W10-VWSL(WS-P1)
035170				 TO W10-SELL.
	     MOVE W10-ENTRY	 TO W90-TAX.
021790	     MOVE W10-SELL	 TO W10-SPAPRC.

035180 BD45.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Only Users with a sales security level or a debtor securityЁ
      *    Ё level greater than 2 or a sales security level and debtor	Ё
      *    Ё security level greater than 0 are allowed to change the	Ё
      *    Ё selling price.						Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (LS0-SALEV > 2) OR
	      (LS0-DBLEV > 2) OR
	      (LS0-SALEV > 0 AND LS0-DBLEV > 0)
035190	       ACCEPT S26-PRC AT LINE LIN
	   ELSE
	       DISPLAY S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD44A
	       END-EVALUATE.
	   IF W10-SELL < (W10-SPAPRC - 0.009)
021790	       MOVE W10-SELL	 TO W10-SPAPRC
021810	       PERFORM BB100
021820	       IF WS-ERROR = 1
021830		   MOVE ZERO	 TO WS-ERROR
035250		   GO TO BD45.
035260	     DISPLAY CLEAR-L25.

035270 BD50.
035280       MOVE W10-SELL       TO SAL-SELL.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
035290 BD55.
035300       MOVE W10-QUANT      TO SAL-QNT.
	     MOVE W10-CASES	 TO SAL-CASES.
	     MOVE WS-P1 	 TO SAL-PACK.
035310	     COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
035320       MOVE W25-VALUE      TO W25-CREDIT W90-VALUE.
035330       MULTIPLY -1         BY W25-VALUE.
035340       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
022330	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
035360	     DISPLAY "Discount %" AT 2518
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
035370	     MOVE ZERO		 TO W90-DISC W90-CDISC WS-DISC.
      *    *************************************************************
      *    ****   I F	D I S C O U N T	   A P P L I C A B L E
      * 	  G E T   P E R C E N T A G E	F R O M
      * 	  D E P A R T M E N T	F I L E
      *    *************************************************************
035380	   IF (DEB-DISC > 0 AND < 11)
035390	       MOVE STK-LDG	  TO DPT-CODE
035400         PERFORM READ-DEPART THRU READ-DEPART-EXIT
022390	       MOVE DPT-DSETT(DEB-DISC)
022400				 TO WS-DISC
      *    *************************************************************
      *    **  M U S T	 T H E	 A L T E R N A T E   D I S C O U N T  **
      *    **  B E   U S E D   F O R   T H I S	D E B T O R	      **
      *    *************************************************************
	       IF STK-ALT-DISC = "Y"
		   IF DPT-ALT-DD1 = DEB-DISC
		       MOVE DPT-DD11
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   ELSE
		   IF DPT-ALT-DD2 = DEB-DISC
		       MOVE DPT-DD12
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   END-IF
	       END-IF.
035430	     ACCEPT S26-DSC AT LINE 25.
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
	   IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
	       MOVE "Y" 	 TO W40-SPAR.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
035630 BD62.
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
035650         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
035660       ADD W90-CDISC       TO W90-LDISC.
035670	     SUBTRACT W90-CDISC  FROM W90-VALUE.
      *    *************************************************************
      *    ****   T A X   E X E M P T ?     ( V A T )
      *    *************************************************************
035760	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C") OR
	      (STK-EXEMPT = "Y" AND DEB-ART11 = "Y")
035770         ADD W90-VALUE     TO W90-NONTAX
035780         MOVE W90-VALUE    TO SAL-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
035790	   ELSE
035800         MOVE W90-VALUE    TO SAL-TAXVAL
	       IF W90-EXP = "I"
		   COMPUTE W90-TAXVAL(WS-VAT-SUB) ROUNDED =
			   W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
	       ELSE
022890		   ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
	       END-IF
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB.
      *    *************************************************************
      *    ****   T A X   E X E M P T ?     ( V A T )
      *    *************************************************************
	   IF (STK-TAX > 0) AND
	      (STK-EXEMPT = "Y" AND DEB-ART11 = "Y")
	       MOVE "Y"		 TO SAL-ART11
	   ELSE
	       MOVE "N" 	 TO SAL-ART11.
022910	   IF (W90-TAX NOT = SPACES) AND
	      (W90-EXP NOT = "C" AND "X") AND
	      (STK-EXEMPT = "N" OR DEB-ART11 = "N")
022920         COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
	   IF (W90-EXP = "I") AND (STK-TAX NOT = 0) AND
	      (STK-EXEMPT = "N" OR DEB-ART11 = "N")
	       COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
		      (100.0000 + W05-RATE)) * W05-RATE.
	   IF SAL-VAT-SUB > 0
	       ADD SAL-VAT	 TO W90-VAT(SAL-VAT-SUB).
      *    *************************************************************
      *    ****    A C C U M U L A T E   T O T A L   C R E D I T
      *    *************************************************************
035860       ADD W25-VALUE       TO W90-CR.

035870 BD65.
035880       MOVE W90-CR         TO W90-TOTAL.
035890       ADD W90-LDISC       TO W90-TOTAL.
035900	     MOVE W90-CR	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
035920	     MOVE W90-LDISC	 TO W100-VAL.
023020	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
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
036070	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

036090 BD70.
036100	     MOVE "'A'ccept or 'C'ancel  [A]"
				 TO WS-ERR-MES.
036140	     MOVE "A"		 TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *    *************************************************************
036150 BD72.
	     PERFORM OPT-MESSAGE.
036230	   IF NOT (WS-OPTION = "A" OR "C")
036240	       GO TO BD70.
036260     IF WS-OPTION = "C"
036270	       DISPLAY (LIN, 2) WS-MIV78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
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
	     MOVE SPACES	 TO SAL-EXT-ITEM.

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
           IF WS-S1 NOT > WS-S2
               GO TO BD74A.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
       BD74E.
036340	   IF LIN < 18
036350         ADD 1             TO LIN
036360         GO TO BD75B.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L	 D E T A I L   L I N E S   U P
      *    *************************************************************
       BD74-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BD74-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****    C L E A R   L A S T	 D E T A I L   L I N E
      *    *************************************************************
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

036400 BD75B.
	     MOVE SPACES     TO WS-ERR-MES.
036410	     STRING "'R'emarks, 'N'ext or 'E'nd  ["
		    DELIMITED SIZE
		    WS-INPUT DELIMITED SIZE
		    "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
036470	     MOVE WS-INPUT	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
036550	   IF NOT (WS-OPTION = "R" OR "N" OR "E")
036560         GO TO BD75B.
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
036690	     MOVE WS-LAST	 TO WS-SALKEY.
036700       INITIALIZE SAL-RECORD1.
036710       MOVE WS-LAST        TO SAL-CODE.
036720	     MOVE W10-RMKS	 TO SAL-MSER.
036730       GO TO BD74.

036740 BD76.
036750       SUBTRACT W25-VALUE  FROM W90-CR.
036760       SUBTRACT W90-CDISC  FROM W90-LDISC.
036770       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830     IF (STK-IND < 2) OR (STK-IND = 3)
036800         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
036810 BD80.
036820	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
036830	     ADD 1		 TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
036840 BD85.
036850	   IF WS-LAST = ZERO
036860         GO TO BD00.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   C R E D I T   N O T E
      *    *************************************************************
036870 BD90.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

036980 BD95.
	     PERFORM OPT-MESSAGE.
037050	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
037060	       GO TO BD90.
037080     IF WS-OPTION = "M"
037090         ADD 1             TO WS-LAST
037100         GO TO BD40.
037110     IF WS-OPTION = "A"
037120	       PERFORM BD200A
037130         GO TO BD90.
037140     IF WS-OPTION = "C"
037150	       GO TO BD150.
037160       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
037170     IF W90-LDISC NOT = ZERO
037180         MOVE W90-LDISC    TO W90-TDISC
037190         GO TO BD105.
037200       MOVE W90-CR         TO W90-DVAL.

037440 BD105.
037450       ADD W90-TDISC W90-CR 
037460                           GIVING W90-TSELL W90-TOTAL.
037470	     MOVE W90-TDISC	 TO W100-VAL.
024340	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
037490	     MOVE ZERO		 TO W90-TTAX.
	     MOVE 1		 TO WS-P1.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BD110.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
037570	       GO TO BD112.
023060	     MOVE W05-VAT(WS-P1) TO W05-RATE.
037590*	     COMPUTE W90-TTAX ROUNDED = W90-TTAX +
      *		    (W90-TAXVAL(WS-P1) * W05-RTE).
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Split the calculation into two COMPUTE statements as the Ё
      *    Ё  VAT report was occasionally out of balance (VAT dropping Ё
      *    Ё  the decimals of cents).				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     COMPUTE W90-VAT(WS-P1) ROUNDED =
		     W90-TAXVAL(WS-P1) * W05-RTE.
021490	     COMPUTE W90-TTAX ROUNDED = W90-TTAX + W90-VAT(WS-P1).
      *    *************************************************************
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *    *************************************************************
       BD112.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BD110.
	     MULTIPLY -1	 BY W90-TTAX.
037540	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
037570	       GO TO BD115.
037610	     MOVE W90-TTAX	 TO W100-VAL.
024480	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
037630	     ADD W90-TTAX	 TO W90-TOTAL.
      *    *************************************************************
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *    *************************************************************
038330 BD115.
038340	     DISPLAY CLEAR-L25.
038350	     MOVE "M"		 TO WS-AGE.
           IF OPEN-ITEM
               GO TO BD118.
038380	     DISPLAY "AGE - " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038390		     "M" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
038400		     "td, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038410		     "C" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
038420		     "ur, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038430		     "3" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
038440		     "0 days, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038450		     "6" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
038460		     "0 days, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038470		     "9" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
038480		     "0 days or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
038490		     "1" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 6 HIGHLIGHT
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
038600	     MOVE WS-AGE	 TO W90-AGE.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   C R E D I T   N O T E
      *    *************************************************************
038610 BD118.
	     MOVE WS-LAST	 TO W40-LAST.
	     MOVE WS-PRC	 TO W40-PRC.
	     MOVE W10-SMAN	 TO W40-SMAN.
	   IF DEB-TERMS = "3"
      *    *************************************************************
      *    ****   C O D   A C C O U N T
      *    *************************************************************
	       PERFORM BB000.
038630	     DISPLAY "Processing Credit Note" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
029610	     CLOSE SALE.
             OPEN I-O SALE.
	   IF PRG-NUMBER = 99
	       CALL "INV\CRNF12W" USING WS-PARID
					LS-USER-ID
					LS0-PROGRAMS
					LS0-SECURITY
					W40-COMPANY
					W90-TRAN
		     ON EXCEPTION
			GO TO BD119
	       END-CALL
	   ELSE
	       CALL PRG-NAME3 USING WS-PARID
				    LS-USER-ID
				    LS0-PROGRAMS
				    LS0-SECURITY
				    W40-COMPANY
				    W90-TRAN
		    ON EXCEPTION
			GO TO BD119
	       END-CALL.
	   IF PRG-NUMBER = 99
	       CANCEL "INV\CRNF12W"
	   ELSE
	       CANCEL PRG-NAME3
	   END-IF.
	     GO TO BD119A.

       BD119.
	     MOVE SPACES     TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME3 DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.

       BD119A.
029610       CLOSE SALE.
029600       OPEN OUTPUT SALE.
029610       CLOSE SALE.
             OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
029800	   IF WS-OPTION = "E"
029820	       GO TO BD150.
029830	     GO TO BD00.
      *    *************************************************************
      *    ****    T E S T   F O R   I N D I V I D U A L
      *    *************************************************************
       BD120A.
038770     IF W80-TYPE NOT = 1
038780         GO TO BD140.
038790       MOVE DEB-NAME       TO W60-NAME.
038800       PERFORM CA165-NAME.
      *    *************************************************************
      *    ****    T I T L E
      *    *************************************************************
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
      *    *************************************************************
      *    ****    F I R S T   I N I T I A L
      *    *************************************************************
038940 BD125.
038950	     MOVE 1		 TO WS-S5.
038960       ADD 2               TO WS-S6.
038970       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
038980       ADD 1               TO WS-S6.
038990       MOVE "."            TO W60-NCHAR (WS-S6).
039000     IF W40-NCHAR (49) = SPACE
039010         GO TO BD130.
      *    *************************************************************
      *    ****    S E C O N D   I N I T I A L
      *    *************************************************************
039020       ADD 1               TO WS-S6.
039030       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
039040       ADD 1               TO WS-S6.
039050       MOVE "."            TO W60-NCHAR (WS-S6).
039060 BD130.
039070       ADD 2               TO WS-S6.
      *    *************************************************************
      *    ****    S U R N A M E
      *    *************************************************************
039080 BD135.
039090	     MOVE W40-NCHAR (WS-S5)
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
039200	   IF NOT (DEB-POST = "0000" OR "9999")
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

       BD150.
	     EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BD200A	       SECTION 59.
042910 BD200.
	     MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
				 TO WS-ERR-MES.
             MOVE "S"            TO WS-OPTION.

       BD201.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "H" OR "S")
	       GO TO BD200.
           IF WS-OPTION = "S"
               GO TO BD202
           ELSE
           IF WS-OPTION = "E"
               GO TO BD265.

       BD201A.
	     MOVE "Amend - 'O'rd No, 'R'ef No, 'S'alesman or 'E'xit"
				 TO WS-ERR-MES.
             MOVE SPACE          TO WS-OPTION.

       BD201B.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "O" OR "R" OR "S")
	       GO TO BD201A.
           IF WS-OPTION = "E"
               GO TO BD265.
           IF WS-OPTION = "O"
017660	       ACCEPT W90-ORD AT 0368
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       GO TO BD201B
           ELSE
           IF WS-OPTION = "R"
	       ACCEPT W90-REG AT 0554
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       GO TO BD201B.

       BD201C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
017720	   IF W10-SMAN > 50
017730         GO TO BD201C.
             MOVE SPACES         TO W40-ASSIST.
	     MOVE W10-SMAN	 TO W40-SMAN.
017740	   IF W10-SMAN = ZERO
	       DISPLAY "╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟" AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BD201B.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810         GO TO BD201C.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE PAR-SALESMAN	 TO W40-ASSIST.
             GO TO BD201A.

       BD202.
042920       MOVE 8              TO LIN.
042930       PERFORM BD80 UNTIL LIN > 18.
042940       MOVE 8              TO LIN.
042950       MOVE 1              TO WS-LINES WS-FIRST.

042960 BD205.
042970	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
042980 BD210.
042990	   IF WS-SALKEY > WS-LAST
043000	       GO TO BD214.
043010       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
043030	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
043050	       GO TO BD212.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF (W10-STCK = "A") AND
	      (WS-USE-ITM = "Y")
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
043080	     MOVE SAL-QNT	 TO W10-QUANT.
	     MOVE SAL-CASES	 TO W10-CASES.
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

       BD214.
	     MOVE 7		 TO LIN.
	     DISPLAY CLEAR-L25.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
043170 BD215.
001020	     DISPLAY " " AT 2503
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
001030		     " highlight," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
001040		     "A" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
001050		     "mend," WITH FOREGROUND-COLOR 1
				  BACKGROUND-COLOR 3
001040		     "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
001050		     "elete or " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
	     PERFORM SAVE-SCREEN-2.
      *
      *    ****    S C R E E N	 A T T R I B U T E S
      *
       BD219.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
043440 BD220.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BD222
		 WHEN DOWN-KEY
001340		     GO TO BD223
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BD220
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BD265
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-DOWN
001340		     GO TO BD235
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-UP
001340		     GO TO BD230
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BD220
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 7)
		   DISPLAY CLEAR-L25
		   GO TO BD238.
	     CALL X"E5".
001280	     GO TO BD220.
      *    *************************************************************
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *    *************************************************************
       BD222.
	   IF (WS-SALPOS = 1) OR (LIN < 8)
	       GO TO BD224.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 8
	       SUBTRACT 1	 FROM LIN
	       GO TO BD224.
	     MOVE 18		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *    *************************************************************
       BD222-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 9
	       SUBTRACT 1	 FROM WS-S7
	       GO TO BD222-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
       BD222-DISPLAY.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO BD222-END-DISP.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF (W10-STCK = "A") AND
	      (WS-USE-ITM = "Y")
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150       MOVE SAL-QNT        TO W10-QUANT.
	     MOVE SAL-CASES	 TO W10-CASES.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.

       BD222-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO BD224.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *    *************************************************************
       BD223.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO BD224.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
	   IF LIN > 7
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO BD224.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *    *************************************************************
       BD223-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BD223-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO BD222-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
001980 BD224.
001990	     PERFORM BD219.
	   IF LIN > 7
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO BD220.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
030780 BD225.
030790	     MOVE 8		 TO LIN.
042930       PERFORM BD80 UNTIL LIN > 18.
030810	     MOVE 8		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *    *************************************************************
       BD230.
	   IF WS-FIRST = 1
	       GO TO BD220.
	   IF WS-FIRST > 11
	       SUBTRACT 11	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM BD225.
	     GO TO BD205.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BD235.
	   IF (WS-FIRST + 11) > WS-LAST
	       GO TO BD220.
	     ADD 11		 TO WS-FIRST.
	     PERFORM BD225.
	     GO TO BD205.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BD238.
030630	   IF WS-OPTION = "A"
043770	       GO TO BD240.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T	 A N   I T E M
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
043810 BD239.
043780	     MOVE "Are you sure Y/N?  [N]"
				 TO WS-ERR-MES.
043800       MOVE "N"            TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
043890	   IF NOT (WS-OPTION = "N" OR "Y")
043900	       GO TO BD239.
043920     IF WS-OPTION = "N"
043930	       GO TO BD215.
043940       MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
043950 BD240.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
044140	     PERFORM READ-SALE THRU READ-SALE-EXIT.
031240	   IF SAL-EXT-ITEM = SPACES
031250	       GO TO BD248.
044150	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
044160       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
044170       MOVE SAL-QNT        TO W10-QUANT.
	     MOVE SAL-CASES	 TO W10-CASES.
044180       MOVE SAL-COST       TO W10-COST.
044190       MOVE SAL-SELL       TO W10-SELL.
044200       MOVE SAL-DISC       TO W90-CDISC.
044210       MOVE SAL-DSC        TO WS-DISC.
044220       MOVE SAL-VAL        TO W25-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE SAL-PACK	 TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E	 T O T A L S
      *    *************************************************************
044230	     SUBTRACT W25-VALUE  FROM W90-CR.
044240       SUBTRACT W90-CDISC  FROM W90-LDISC.
044250       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF WS-VAT-SUB > ZERO
044260	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(WS-VAT-SUB).
019830     IF (STK-IND < 2) OR (STK-IND = 3)
044280         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S	 I T   A N   A M E N D M E N T
      *    *************************************************************
       BD248.
	     PERFORM BD219.
044290     IF WS-OPTION = "A"
044300         GO TO BD255.
      *    *************************************************************
      *    ****    R E - D I S P L A Y	 T O T A L S
      *    *************************************************************
031430	   IF SAL-EXT-ITEM NOT = SPACES
044310	       PERFORM BD65 THRU BD68.
      *    *************************************************************
      *    ****    D E L E T E	 I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
044320 BD250.
044330	     MOVE WS-SALKEY	 TO WS-SALSTR.
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
	     PERFORM BD219.
044450	   IF SAL-EXT-ITEM = SPACES
031590	       ACCEPT (LIN, 21) SAL-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
044480	       GO TO BD262.
	   IF (LS0-LEV > 2) AND
	      (STK-USE-PACKS = "Y")
	       DISPLAY "╣" AT 0750
			STK-PR(WS-P1)
			WITH BACKGROUND-COLOR 5
			     FOREGROUND-COLOR 6 HIGHLIGHT
		       "ф".
      *    *************************************************************
      *    ****    A M E N D   Q U A N T I T Y
      *    *************************************************************
044490	     ACCEPT S26-QNT AT LINE LIN.
044510     IF W10-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
044540         MOVE SAL-QNT      TO W10-QUANT
	       MOVE SAL-CASES	 TO W10-CASES
044550         GO TO BD255.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
044560	     MULTIPLY W10-SELL BY W10-QUANT
044570                           GIVING W25-VALUE.
044580	     MOVE W25-VALUE	 TO W100-EXT W90-VALUE.
031720	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     PERFORM CHECK-CORRECT.
044680	   IF WS-OPTION = "N"
044690         GO TO BD255.
      *    *************************************************************
      *    ****    U P D A T E	 S A L E   F I L E   R E C O R D
      *    *************************************************************
044700	     MOVE W10-QUANT	 TO SAL-QNT.
	     MOVE W10-CASES	 TO SAL-CASES.
044710       MULTIPLY -1         BY W25-VALUE.
044720       MOVE W25-VALUE      TO SAL-VAL.
	   IF NOT (STK-FACT = ZERO)
	       COMPUTE SAL-CONVERT = W10-QUANT * WS-P1 * STK-FACT.
044730	   IF WS-DISC = ZERO
044740         GO TO BD260.
044750	     COMPUTE W90-CDISC = ((W25-VALUE * WS-DISC) / 100) * -1.
044760       MOVE W90-CDISC      TO SAL-DISC.
      *    *************************************************************
      *    ****    R E - C A L C U L A T E   &	 D I S P L A Y
      *            T O T A L S
      *    *************************************************************
044770 BD260.
044780	     PERFORM BD62 THRU BD68.

044790 BD262.
044800       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
044810       GO TO BD202.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S
      *    *************************************************************
031940 BD265.
	     PERFORM BD225.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E
      *    *************************************************************
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 BD270.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO BD275.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF (W10-STCK = "A") AND
	      (WS-USE-ITM = "Y")
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150	     MOVE SAL-QNT	 TO W10-QUANT.
	     MOVE SAL-CASES	 TO W10-CASES.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
       BD275.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO BD270.

       BD280.
044830	     EXIT.
      /
      *    ****    W A R E H O U S E
      *
       HA000	       SECTION 60.
       HA00.
             PERFORM SAVE-SCREEN-3.
             MOVE 11             TO SHADE-ROW.
             MOVE 30             TO SHADE-COL.
             MOVE 24             TO SHADE-WIDTH.
             MOVE 2              TO SHADE-LINES.
             DISPLAY "здддддддддддддддддддддддд©" AT 1028
                      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
             DISPLAY "Ё Warehouse Code:        Ё" AT 1128
                      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
             DISPLAY "юдддддддддддддддддддддддды" AT 1228
                      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
             PERFORM SCREEN-SHADOW.
             MOVE SPACES         TO W40-CODE.

       HA05.
             DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
                     "=Help,"
                      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
                     " to do Warehouse Lookup,"
                      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
                     " to exit"
                      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             ACCEPT W40-CODE AT 1146
                    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 0 UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                      MOVE SPACES TO W40-CODE
                      GO TO HA999
                 WHEN F1-KEY
                     PERFORM HELP-ROUTINE
                     GO TO HA05
                 WHEN F2-KEY
016560		     PERFORM AA50W THRU AA59
                 WHEN OTHER
                     CALL X"E5"
                     GO TO HA05
               END-EVALUATE
               DISPLAY W40-CODE AT 1146
                       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
015830         IF W40-CODE = SPACE
015840             GO TO HA05.
019250       DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250       DISPLAY CLEAR-L25.
           IF W40-CODE = SPACES
               GO TO HA999.
013310       MOVE 6              TO W50-LENGTH.

       HA10.
013320       MOVE W40-CODE       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-5        TO W40-CODE
               GO TO HA10.
             CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380       MOVE W50-DENT       TO W40-CODE.
013380       MOVE W50-DENT       TO WAR-CODE.
             PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
015860     IF WS-F-ERROR = 51
017520         MOVE "Z"          TO WS-ETYPE
018620	       PERFORM AA50W THRU AA59
               DISPLAY W40-CODE AT 1146
                       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
018640         IF W40-CODE = SPACES
018720             GO TO HA05
               ELSE
                   GO TO HA10.
             PERFORM RESTORE-SCREEN-3.
             DISPLAY "╣" AT 0707
                     "W/H " WITH FOREGROUND-COLOR 0
                      W40-CODE WITH FOREGROUND-COLOR 0 "ф".
             DISPLAY "╣" AT 0721
                      WAR-SALE WITH FOREGROUND-COLOR 0 "ф".
             PERFORM CHECK-CORRECT.
020250     IF WS-OPTION = "N"
               PERFORM RESTORE-SCREEN-3
               GO TO HA00.

       HA999.
             EXIT.

       COPY WARHSE.LUS.

      /
      *    ****    C A S E S
      *
       JA000	       SECTION 66.
       JA00.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "зддддддддддддд©" AT 0833
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё NO OF CASES Ё" AT 0933
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "цддддддддддддд╢" AT 1033
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё             Ё" AT 1133
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "юддддддддддддды" AT 1233
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 35		 TO SHADE-COL.
	     MOVE 13		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE 1		 TO W10-CASES.

       JA05.
	     ACCEPT S38 AT 1138.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-CASES
		     GO TO JA10
	       END-EVALUATE.

       JA10.
	     PERFORM RESTORE-SCREEN.

       JA999.
	     EXIT.
      /
052700 ZA000-INIT	       SECTION 91.
052710 ZA000-OPEN.
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
052730	     DISPLAY CLR-SCREEN.
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
      *    *************************************************************
      *    ****       S A L E S	  -   H I S T O R Y   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    G U A R A N T E E	F I L E		    ****
      *    *************************************************************
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 		S A L E S   F I L E		    ****
      *    *************************************************************
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    **** 	    W A R E H O U S E	F I L E		    ****
      *    *************************************************************
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
      *    *************************************************************
      *    ****       W A R E H O U S E	  S T O C K   F I L E	    ****
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-C-FACT	 TO WS-C-FACT.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	     MOVE PAR-INV	 TO PRG-NUMBER.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
054270	     OPEN OUTPUT SALE.
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
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP)
      * 	  (System.ini Windows.me)
      *    *************************************************************
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT.
055330	     GO TO ZA205.

055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
056000		     " " WS-STATUS
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	92.
056100 OPEN-ERROR.
056110       DISPLAY CLR-SCREEN.
056120	     DISPLAY "Open error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
056130       PERFORM DISPLAY-FILE-NAME.
056140
056150 READ-ERROR.
056160       DISPLAY CLR-SCREEN.
056170	     DISPLAY "Read error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
056180       PERFORM DISPLAY-FILE-NAME.
056190
056200 WRITE-ERROR.
056210       DISPLAY CLR-SCREEN.
056220	     DISPLAY "Write error" AT 0812
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
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
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX
041840     ELSE
041770     IF WS-F-ERROR = 51
041780         MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE WAR-CODE     TO WS-KEYX
041840     ELSE
041770     IF WS-F-ERROR = 52
041780         MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
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
056910	     DISPLAY "File - " AT 1012 WS-FILE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
056920       DISPLAY "Status " AT 1212 
056930		      WS-STATUS WITH FOREGROUND-COLOR 3 HIGHLIGHT
056940		     ": " WS-STAT-MESSAGE
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
056950     IF WS-STATUS NOT = "94" 
056960         DISPLAY "Key    " AT 1412 
056970			WS-KEY WITH FOREGROUND-COLOR 3 HIGHLIGHT
056980         DISPLAY "Reverse backup or contact program Support"
056990                  AT 1612.
057000         DISPLAY "Please make a note of these details" AT 1812.
057010       STOP RUN.
