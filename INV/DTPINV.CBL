      $SET LINKCOUNT"1024"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	********  **	 **  **    **  *
      *   **	**     **     **    **	   **	  ***	 **  **    **  *
      *   **	**     **     **    **	   **	  ** *	 **  **    **  *
      *   **	**     **     *******	   **	  **  *  **  **    **  *
      *   **	**     **     **	   **	  **   * **   **  **   *
      *   **	**     **     **	   **	  **	***    ****    *
      *   *******      **     **	********  **	 **	**     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *               I N V O I C I N G   P R O G R A M                *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to be excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
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
      * November 2009	- Include words from Description 2 and from    *
      * 		  the Item code (Some item codes are comprised *
      * 		  of individual words and these will be        *
      * 		  included in the alternate Index)	       *
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
      *  Jan 2012 - Include Format 99 which calls the INVF12W program  *
      * 	    to produce an Invoice via the Windows spooller.    *	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPINV.
000030 AUTHOR.	       James William Lemmon.
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2012
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

000160 COPY DEBDEP.SL.

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

000330 COPY DEBDEP.FDE.

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
       77  WS-CARDEX	   PIC	X(01).
       77  WS-PROMPT	   PIC	X(01).
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
002770 77  WS-NOCHARGE	   PIC	X(01) VALUE " ".
002770 77  WS-SKIP         PIC  X(01) VALUE " ".
002780 77  WS-LOOK         PIC  X(01) VALUE "S".
002820 77  WS-ERROR	   PIC	9(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000210 77  WS-C-FACT	   PIC	X(01).
002860 77  WS-COMP	   PIC	9(01).
002890 77  WS-HEAD         PIC  X(01).
000750 77  WS-TYP	   PIC	X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE          PIC  X(01).
002920 77  WS-PRC          PIC  X(01).
       77  WS-RESTRICT	   PIC	X(01).
       77  WS-RESTCODE	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01) VALUE "N".
002920 77  WS-INPUT        PIC  X(01) VALUE "N".
002920 77  WS-COPY         PIC  X(01) VALUE "N".
       77  WS-PRNINV	   PIC	X(01) VALUE "Y".
       77  WS-NEGATIVE	   PIC	X(01) VALUE "N".
       77  WS-PARCELS	   PIC	X(01).
       77  WS-OVER-LIM	   PIC	X(01).
       77  WS-MEASURE	   PIC	X(01) VALUE "N".
       77  WS-ARREARS	   PIC	X(01).
       77  WS-INV-CHECK    PIC	X(01).
       77  WS-INV-AMT	   PIC	9(07) COMP-3.
       77  WS-SPLIT-BC	   PIC	X(01).
002950 77  WS-BATCH        PIC  9(05).
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  PRG-NAME2	   PIC	X(12) VALUE SPACES.
003020 77  WS-PER          PIC  Z(02)9.99.
003030 77  WS-CR           PIC  X(01).
003040 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
003040 77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
       01  PRG-NAME3.
	   03  FILLER	   PIC	X(08) VALUE "INV\INVF".
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
	   03  WS-PROG	       PIC  X(03) VALUE "INV".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003700 COPY CARDEX.ID.

003700 COPY CONTROL.ID.

       COPY DEBALT.ID.

000330 COPY DEBDEP.ID.

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

       01  W30-PRICES.
	   03  W30-SELECT-PRICE.
	       05  W30-PRICE1  PIC S9(08)V999 COMP-3.
	       05  W30-PRICE2  PIC S9(08)V999 COMP-3.
	       05  W30-PRICE3  PIC S9(08)V999 COMP-3.
	   03  W30-SELECT-PRC REDEFINES W30-SELECT-PRICE.
	       05  W30-PRICE   OCCURS 3
			       PIC S9(08)V999 COMP-3.

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

005830 01  W75-KEYS.
005840	   03  W75-S	       PIC 9(02) COMP-5.
005850	   03  W75-S1	       PIC 9(02) COMP-5.
	   03  W75-S2	       PIC 9(02) COMP-5.
005860	   03  W75-DNO	      OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).

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
007020	   03  W100-LBAL       PIC  Z(06)9.99-.
	   03  W100-LNGTH REDEFINES W100-LBAL.
	       05  FILLER      PIC  X(02).
	       05  W100-LENGTH PIC  ZZZ9.9999.
007030	   03  W100-LARR       PIC  Z(06)9.99.
	   03  W100-BRDTH REDEFINES W100-LARR.
	       05  FILLER      PIC  X(01).
	       05  W100-BREADTH PIC ZZZ9.9999.
007040	   03  W100-BAL        PIC Z(08)9.99-.
007050     03  W100-CUR        PIC Z(08)9.99-.
007060     03  W100-30         PIC Z(08)9.99-.
007070     03  W100-60         PIC Z(08)9.99-.
007080     03  W100-90         PIC Z(08)9.99-.
007090     03  W100-120        PIC Z(08)9.99-.
007100     03  W100-INT        PIC Z(08)9.99-.
007110     03  W100-CRL        PIC Z(06)9.
	   03  W100-NMB REDEFINES W100-CRL.
	       05  FILLER      PIC  X(02).
	       05  W100-NUMBER PIC  ZZZ9.
           03  W100-SER        PIC ZZ9.
007020	   03  W100-PRC        PIC Z(06)9.99-.
007100	   03  W100-VAL        PIC Z(08)9.99-.
	   03  W100-SELL REDEFINES W100-VAL
			       PIC Z(07)9.999.
007120	   03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
	   03  W100-Q5 REDEFINES W100-V2.
	       05  W100-QNT5   PIC  Z(05)9-.
	       05  FILLER      PIC  X(03).
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
				 PIC 9999/99/99 USING DEB-PMT.
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

009600 01  S26-QNT-N.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009620				 PIC Z(04)9.9- USING W10-QUANT AUTO.

004340 01  S26-WEIGHT.
004350	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
004360				 PIC Z(02)9.999- USING W10-QUANT AUTO.

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

009870 01  S28.
009880	   03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009890                           PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "з".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "©".
007450	   03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "CLIENT DETAILS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Name    >".
	   03	       COLUMN 65 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Address >".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
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
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "ы".

       01  S31.
	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(40) USING W40-NAME AUTO.
	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD1 AUTO.
	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD2 AUTO.
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD3 AUTO.
	   03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD4 AUTO.
	   03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W40-PC1 AUTO.

       01  S32.
	   03  LINE 12 COLUMN 38 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W40-PADD1 AUTO.
	   03  LINE 13 COLUMN 38 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(26) USING W40-PADD2 AUTO.
	   03  LINE 14 COLUMN 38 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(26) USING W40-PADD3 AUTO.
	   03  LINE 15 COLUMN 38 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(20) USING W40-PADD4 AUTO.
	   03  LINE 16 COLUMN 38 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W40-PC2 AUTO.

009930 01  S36.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

009930 01  S37.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC ZZ9 USING W40-PARCELS AUTO.

009930 01  S38.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC ZZZZ9 USING W10-CASES AUTO.

       COPY WARLUP.CRT.

       COPY S99.CRT.

       01  S-MEASURE.
	   03  LINE  4 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "з".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "©".
007450	   03  LINE  5 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(04) FROM WS-SPACE.
	   03	       COLUMN 20 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "MEASUREMENTS".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 PIC  X(03) FROM WS-SPACE.
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE  6 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Length  >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE  7 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Breadth >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
007450	   03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ё".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Pieces  >    <".
007460	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "     Ё".
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "ю".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "ы".

       01  SA-MEASURE.
	   03  LINE  6 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZ9.9999 USING W10-LENGTH AUTO.
	   03  LINE  7 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
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
003240	   IF (LS0-SALEV < 1) AND (LS0-DBLEV < 2)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AZ100.
010250	     PERFORM ZA000-INIT.
016330	     PERFORM BC000.
	     CLOSE DEBDEP
		   SALE.

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
      *    Ё   PERFORM OPT-MESSAGE (Use this format if function or any Ё
      *    Ё	       other keys are to be tested on exit)	       Ё
      *    Ё			     OR 			       Ё
      *    Ё   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       Ё
      *    Ё   (Use TEST AFTER if WS-OPTION is preloaded with a        Ё
      *    Ё   default option)					       Ё
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
      *    Ё then PERFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    Ё (The UNTIL and TEST AFTER options also apply here)        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

010930 COPY LOCKED.RC2.

      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *    *************************************************************
010990 AA300		  SECTION 51.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Itm,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Alt,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Dsc,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Dsc2,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=X/Rf,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F7" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Ord,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F8" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Prompt,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT "=Word,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT "ape"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       R E A D	 F I L E S   R O U T I N E S	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
011110 AC000		  SECTION 52.

011150 COPY CONTROL.RD.

       COPY DEBALT.RD.

011190 COPY DEBDEP.RD.

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
011350 AD000-WRITE	       SECTION 54.

011530 COPY SALE.WR.

011970 COPY DEBTOR.ALU.

011630 COPY DEBTOR.ILU.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

      *    *************************************************************
      *    ****      Read debtor record - using account number.	    ****
      *    *************************************************************
013750 CA155-GET-DEBTOR  SECTION 55.
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
013910	       MOVE DEB-TYPE	 TO W80-TYPE.

013920 CA160-EXIT.
013930       EXIT.
      *    *************************************************************
      *    ****  Unpack Surname, First name and Second name into W40
      *    *************************************************************
013940 CA165-NAME	 SECTION 55.
013950 CA165.
013960       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013970                           INTO W40-SURNAME 
013980                                W40-FNAME 
013990                                W40-SNAME
014000       OVERFLOW
014010           MOVE 1          TO WS-ERROR.
014020 CA170-EXIT.
014030       EXIT.
      *    *************************************************************
      *    ****  Unpack Street address (Line 1, 2, 3 and 4) into W40
      *    *************************************************************
014040 CA175-ADDRESS	 SECTION 55.
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
      *    *************************************************************
      *    ****  Unpack Postal address (Line 1, 2, 3 and 4) into W40
      *    *************************************************************
014150 CA185-ADDRESS	 SECTION 55.
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
       BB000		   SECTION 56.
       BB00.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
       BB05.
	     PERFORM OPT-MESSAGE
		     TEST AFTER UNTIL WS-OPTION = "N" OR "Y".
      *	     PERFORM OPT-MESSAGE.
020250*	   IF NOT (WS-OPTION = "N" OR "Y")
      *	       GO TO BB00.
           IF WS-OPTION = "N"
	       GO TO BB29.
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
       BB030	       SECTION 57.
       BB30.
             MOVE ZERO           TO WS-ERROR.
	     MOVE 1		 TO WS-S5.
	     MOVE W10-QUANT	 TO WS-S6.
             MOVE SPACES         TO W43-SERIALS.
	     PERFORM SAVE-SCREEN-3.
       BB40.
	     DISPLAY "Serial No:" AT LINE LIN COLUMN 43
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                    (   )"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
       BB41.
	     MOVE WS-S5 	 TO W100-SER.
	     DISPLAY W100-SER AT LINE LIN COLUMN 74
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 7.
       BB42.
	     ACCEPT W43-SERNO(WS-S5) AT LINE LIN COLUMN 54
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		   GO TO BB30
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
           IF WS-F-ERROR = ZERO
	       MOVE "Serial number duplicated"
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
      /    *************************************************************
      *    ****    G E T   M E A S U R E M E N T S   (LENGTH/BREADTH)
      * 	   A N D   N U M B E R	 O F   P I E C E S
      *    *************************************************************
       BB050	       SECTION 58.
       BB50.
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
017970 BB100		   SECTION 59.
017980 BB100-MARG.
017990	     MOVE ZERO		 TO WS-ERROR.
	     MOVE W10-SPAPRC	 TO W10-ORDER.
018010	   IF (W90-TAX = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
018020	       IF (STK-TAX NOT = ZERO) AND
		  (STK-EXEMPT = "N" OR DEB-ART11 = "N")
018030		   COMPUTE W10-ORDER ROUNDED = (W10-SPAPRC /
018040                (100.0000 + W05-RATE)) * 100. 
	   IF (W10-SPEC = "Y") AND (STK-P-PRC(WS-P1) NOT > W10-ORDER)
015720	       GO TO BB110
	   ELSE
	   IF (WS-PRC = "1") AND (STK-SL(WS-P1) NOT > W10-ORDER)
015720         GO TO BB110
           ELSE
	   IF (WS-PRC = "2") AND (STK-CSH(WS-P1) NOT > W10-ORDER)
015720         GO TO BB110
           ELSE
018050	   IF STK-WSL(WS-P1) NOT > W10-ORDER
018060         GO TO BB110.
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
	     MOVE ZERO		 TO WS-ERROR.
      *    *************************************************************
      *    ****	OVERRIDE PASSWORD - !@$^*) (Shift "124680")
      *    *************************************************************
       BB107.
	   IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.

       BB107A.
	   IF W10-SPAPRC < W10-COST
018110	       MOVE "Selling less than Cost price - Accept Y/N?"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
018200	       IF NOT (WS-OPTION = "N" OR "Y")
		   GO TO BB107A
	       END-IF
	       IF WS-OPTION = "N"
018470		   MOVE 1	 TO WS-ERROR
		   MOVE ZERO	 TO WS-SUPER.
	   GO TO BB110.
      *    *************************************************************
      *    ****	OVERRIDE PASSWORD - !#%&() (Shift "135790")
      *    *************************************************************
       BB108.
	   IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /
017970 BB150		   SECTION 60.
017980 BB150-ADD.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 37		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддд©" AT 0930
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё	  D E L I V E R Y   A D D R E S S   Ё" AT 1030
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "цддддддддддддддддддддддддддддддддддддд╢" AT 1130
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ЁLine 1                               Ё" AT 1230
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё     2                               Ё" AT 1330
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё     3                               Ё" AT 1430
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё     4                     	    Ё" AT 1530
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ЁP/Code     			    Ё" AT 1630
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "юддддддддддддддддддддддддддддддддддддды" AT 1730
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY S32.
	     PERFORM SCREEN-SHADOW.

       BB155.
	     ACCEPT S32.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BB155.

       BB160.
011320	     PERFORM RESTORE-SCREEN-2.
	     DISPLAY CLEAR-L25.

       BB165.
             EXIT.
      /
      *    ****    I N V O I C E   A N D   C A S H   S A L E
      *
018500 BC000	     SECTION 5.
018510 BC00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
018520       MOVE 5              TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 

018570 BC05.
      *    *************************************************************
      *    ****    I N V O I C E   S C R E E N
      *    *************************************************************
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S26.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 1		 TO WS-S9.

       BC08.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC08.
018660	     MOVE ZERO		 TO W90-BDT W90-SALES
018670				    W90-BAL W90-LDISC
018680				    W90-CUR W90-NONTAX
				    W40-PARCELS W90-EXCTOT.
018690	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
018700	     MOVE SPACES	 TO W90-TAX W40-SPAR.
018710       MOVE "I"            TO WS-CR.
             MOVE "N"            TO WS-INPUT.

018720 BC10.
      *    *************************************************************
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *    *************************************************************
018770       MOVE SPACES         TO W80-ACNO.

018780 BC11.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F7" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Prompts,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Create,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Debtor Lookup (Any Word)"
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
		     MOVE SPACE  TO WS-TYP
016560		     PERFORM AA50D THRU AA59
		 WHEN F7-KEY
		     PERFORM DA000
		     GO TO BC11
		 WHEN F8-KEY
003240		     IF LS0-DBLEV > 0
			 PERFORM GA000
			 MOVE LS0-DBT
				 TO W80-ACNO
		     END-IF
		 WHEN F9-KEY
		     MOVE "A"	 TO WS-TYP
016560		     PERFORM AA50D THRU AA59
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
018900	       GO TO BC250.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

018910 BC12.
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
012150	     ACCEPT W10-TIME FROM TIME.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
019010 BC15.
	     MOVE DEB-ACNO	 TO W40-ACNO.
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019070       COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
019080                          DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     PERFORM BC189 THRU BC215.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *    *************************************************************
             PERFORM BC18-MEMO THRU BC18E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
021280	     DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *    *************************************************************
           IF DEB-CAT = 80
	       MOVE "This account has been suspended"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC05.
	     MOVE "C"		 TO WS-OPTION.
      *    *************************************************************
      *    ****   M I N I M U M   P R O M P T S   O R	F A S T
      * 	  M O D E   ?	 ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "N" OR "F"
	       GO TO BC19.

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
019250	     DISPLAY CLEAR-L25.
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
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     DISPLAY S-DEB-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     PERFORM RESTORE-SCREEN-3.

       BC18E.
             EXIT.

       BC18-E.
             GO TO BC16.

       BC19.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
019230	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
019240	       GO TO BC16.
	     PERFORM RESTORE-SCREEN-2.

019270 BC20.
019280	     DISPLAY CLEAR-L25.
019290     IF WS-OPTION = "R"
019300         GO TO BC05.
019310     IF WS-OPTION = "E"
019330         GO TO BC250.
019340       MOVE "Y"            TO WS-OPTION.
      *    *************************************************************
      *    **** C H E C K   A R R E A R S   &	C R E D I T   L I M I T
      *    *************************************************************
	   IF WS-ARREARS = "N"
019350     IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
019360	       MOVE "Arrears > 30 days - Override Y/N"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
019450	       IF WS-OPTION = "Y"
019460             MOVE ZERO     TO WS-ERROR
019470             PERFORM BB106
019470             PERFORM BB108
019480             IF WS-ERROR = 1
019490                 MOVE "N"  TO WS-OPTION.                       
019510	   IF NOT (WS-OPTION = "Y")
019520         GO TO BC05.
	   IF WS-OVER-LIM = "N"
019530     IF DEB-CRL > ZERO
019540         IF DEB-BAL > DEB-CRL
019550		   MOVE "Credit limit exceded - Override Y/N"
				 TO WS-ERR-MES
		   MOVE SPACE	 TO WS-OPTION
		   PERFORM OPT-MESSAGE
019640		   IF WS-OPTION = "Y"
019650		       MOVE ZERO TO WS-ERROR
019660		       PERFORM BB106
019470		       PERFORM BB108
019670		       IF WS-ERROR = 1
019680			   MOVE "N"
				 TO WS-OPTION.
019700	   IF NOT (WS-OPTION = "Y")
019710         GO TO BC05.   

       BC21.
	     MOVE DEB-BAL	 TO WS-BAL.
           IF NOT VALID-PRICE
	       MOVE "1" 	 TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
	     MOVE SPACES	 TO WS-RESTRICT.
	     MOVE "N"		 TO WS-RESTCODE.
      *    *************************************************************
      *    ****   D E B T O R	S A L E S   R E S T R I C T I O N S
      *    *************************************************************
	   IF DEB-RESTRICT = "A" OR "E"
	       MOVE DEB-ACNO	 TO DBD-ACNO
	       PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT
	       IF WS-F-ERROR = 0
		   MOVE DEB-RESTRICT
				 TO WS-RESTRICT.

019920 BC25.
019930       MOVE SPACES         TO W90-ORD W90-REG.

019940 BC30.
019960	     ACCEPT W90-ORD AT 0368
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
      *    *************************************************************
      *    ****    S A L E S M A N   N U M B E R
      *    *************************************************************
019990	     MOVE DEB-SMAN	 TO W10-SMAN.

020020 BC35.
	   IF W10-SMAN = ZERO
020030	       ACCEPT W10-SMAN AT 0411
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO.
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
			  BACKGROUND-COLOR 5.
             MOVE PAR-SALESMAN   TO W40-ASSIST.

       BC36.
	   IF DEB-B-TEL = SPACES
	       MOVE DEB-H-TEL	 TO W40-TEL
	   ELSE
	       MOVE DEB-B-TEL	 TO W40-TEL.
	    DISPLAY W40-TEL AT 0466
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5.

020170 BC40.
      *    *************************************************************
      *    ****   C O N S I G N M E N T   D E B T O R	?
      *    *************************************************************
	   IF DEB-CAT = 79
	       MOVE "C" 	 TO W90-EXP
	       MOVE W05-VATNO	 TO W90-TAX
	       GO TO BC42A.
      *    *************************************************************
      *    ****   I N V O I C E   T Y P E   (INCLUSIVE / EXCLUSIVE)
      *    *************************************************************
020300	   IF DEB-INV-TYPE = "E"
020310         MOVE "Y"      TO WS-OPTION
	   ELSE
	   IF DEB-INV-TYPE = "I"
020330	       MOVE "N"      TO WS-OPTION
	   ELSE
	       MOVE "X"      TO WS-OPTION.
      *    *************************************************************
      *    ****   M I N I M U M   P R O M P T S   O R	F A S T
      * 	  M O D E   ?	 ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "N" OR "F"
	       GO TO BC42-A.

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

       BC42-A.
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
			 BACKGROUND-COLOR 5 UPDATE AUTO.

020750 BC45.
020760	     ACCEPT W90-REG AT 0554
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC45
	       END-EVALUATE.
020890	     MOVE ZERO		 TO W90-DB W90-CR.
	     MOVE SPACES	 TO W40-CODE.
      *    *************************************************************
      *    ****   W A R E H O U S I N G   A P P L I C A B L E	?
      *    *************************************************************
	   IF NOT (LS0-WHS = 2)
	       GO TO BC50.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 Is this stock from the warehouse?	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BC48.
             MOVE "Warehouse sales (Y/N)?  [N]"
                                 TO WS-ERR-MES.
             MOVE "N"            TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     GO TO BC45
               END-EVALUATE.
           IF NOT (WS-OPTION = "Y" OR "N")
               GO TO BC48.
           IF WS-OPTION = "Y"
               PERFORM HA000.

020900 BC50.
020910       MOVE 8              TO LIN.
      *    *************************************************************
      *    ****   I N I T I A L I Z E	W O R K   V A R I A B L E S
      *    *************************************************************
020920 BC55.
020930	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM
				    W10-SERNO.
	   IF W10-STCK = "N"
	       MOVE ZERO	 TO W10-I14.
020940	     MOVE ZERO		 TO W10-SELL WS-SUPER W10-CASES.
020940	     MOVE 1		 TO W10-QUANT WS-P1 W10-QNT.

020950 BC60.
	     DISPLAY "яммммммммя" AT 0750
		     WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P	O P T I O N S
      *    *************************************************************
020960	     PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is an 18 character stock/inventory item code being used?  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    **** ALPHA-NUMERIC CODE ****
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
		     IF WS-LAST > 0
			 GO TO BC125
		     ELSE
			 GO TO BC05
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
		 WHEN F7-KEY
		     PERFORM YA000
		     GO TO BC60
		 WHEN F8-KEY
		     PERFORM DA000
		     GO TO BC60
		 WHEN F9-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё  Lookup using any word or part in the Stock Description.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC60
	       END-EVALUATE
016560	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *    ****   E X T E N D E D   C O D E
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *    ****   A L P H A - N U M E R I C   C O D E
      *    *************************************************************
	       IF (W10-STCK = "A") AND
		  (WS-USE-ITM = "Y")
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.

       BC60A.
021040	   IF W10-EXT-ITEM = SPACES
021060	       GO TO BC60.

       BC61.
021070       PERFORM CA000.
      *    *************************************************************
      *    ****   E X T E N D E D   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    ****   A L P H A - N U M E R I C   C O D E
      *    *************************************************************
	   IF (W10-STCK = "A") AND
	      (WS-USE-ITM = "Y")
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
021080	   IF WS-F-ERROR = 22
	       IF WS-SPLIT-BC = "Y"
		   MOVE W10-EXT-ITEM
				 TO W10-EXT-CODE2
		   MOVE W10-IQ	 TO W10-QUANT
		   MOVE W10-I6	 TO W10-ENTRY
		   MOVE W10-ENTRY
				 TO W10-EXT-ITEM
		   PERFORM CA000
		   IF NOT (WS-F-ERROR = 22)
		       DISPLAY (LIN, 2) W10-ITEM
			       WITH FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 0
		       GO TO BC62
		   ELSE
		       MOVE W10-EXT-CODE2
				 TO W10-EXT-ITEM
		   END-IF
	       END-IF
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *    ****   E X T E N D E D   C O D E
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *    ****   A L P H A - N U M E R I C   C O D E
      *    *************************************************************
	       IF (W10-STCK = "A") AND
		  (WS-USE-ITM = "Y")
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BC60
               ELSE
                   GO TO BC61.

       BC62.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC60.
	   IF WS-F-ERROR = 52
               MOVE "No Record at W/H selected"
                                 TO WS-ERR-STRING
003260         PERFORM ERROR-MESSAGE
020140         GO TO BC60.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S	R E S T R I C T I O N S
      *    *************************************************************
	   IF WS-RESTRICT = "A" OR "E"
	       PERFORM MA000
	       IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
		  ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
		   MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
020140		   GO TO BC60.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Until the condition that occasionaly causes the Average   Ё
      *    Ё to be negative is identified and corrected, the following Ё
      *    Ё precaution will remain in place.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (STK-AVGE < ZERO) OR
	      (STK-AVGE > (STK-COST * 2))
	       MOVE STK-COST	 TO STK-AVGE.
      *    *************************************************************
      *    ****    C H E C K   F O R   S P E C I A L S
      *    *************************************************************
	   IF LS0-LEV > 2
	       PERFORM KA000.
021110	     MOVE WS-LAST	 TO WS-SALKEY.
021120       INITIALIZE SAL-RECORD1.
021130       MOVE WS-LAST        TO SAL-CODE.
      *    *************************************************************
      *    ****    I T E M   N U M B E R   O N   I N V O I C E
      *    *************************************************************
021150	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
021160	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
021170 BC65.
021180     IF STK-IND = 3
021190         MOVE ZERO         TO STK-AVGE
021200	       ACCEPT (LIN, 21) W10-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
021220         DISPLAY "Index Unit cost" AT 2512
021230			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
021240         ACCEPT S26-AVGE AT LINE 25
021250     ELSE
021260         MOVE STK-DESC     TO W10-DESC
021270	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
021280     IF STK-IND = 3
021290         IF STK-AVGE = ZERO
034710		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
021320		   GO TO BC65.
034740	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
       BC65-RESTART.
021340	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
021350	     MOVE W10-DESC	 TO SAL-MSER.
	     MOVE ZERO		 TO SAL-CASES.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
	     MOVE ZERO		 TO W10-LENGTH W10-BREADTH W10-NUMBER.
	     MOVE "N"		 TO WS-MEASURE.
      *    *************************************************************
      *    ****    F O R M A T	 1 4   I N V O I C E S
      *    *************************************************************
	   IF PRG-NUMBER = 14
	       DISPLAY "╣" AT 0723
		       "Reference " WITH BACKGROUND-COLOR 5
					 FOREGROUND-COLOR 6 HIGHLIGHT
			STK-ACODE WITH BACKGROUND-COLOR 5
				       FOREGROUND-COLOR 7 HIGHLIGHT
		       "ф".
	     MOVE 1		 TO WS-P1.
	   IF (LS0-LEV < 3) OR
	      (STK-USE-PACKS = "N")
	       GO TO BC66.
	     DISPLAY "╣" AT 0750
		      STK-PER WITH BACKGROUND-COLOR 5
				   FOREGROUND-COLOR 6 HIGHLIGHT
		     "ф".
	     GO TO BC66.

       BC65-PACK.
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
	       GO TO BC65-PACK.

       BC66.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to key in "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "- Cutting size (Wood, Glass etc),"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " Negative "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "or "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 1.
	   IF LS0-LEV > 2
	       IF STK-USE-PACKS = "Y"
		   DISPLAY "" AT 2560
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
			   " for Packs"
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1.

	   IF NOT(WS-SYS-ID = "MDE" OR "DEM")
	       GO TO BC66-D.

       BC66-A.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-CSHPASS.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 38		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "здддддддддддддддддддддддддддддддддддддд" AT 1122
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "©" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
042540	     DISPLAY "Ё " AT 1222 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Bin Location        Quantity         "
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "ю" AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "дддддддддддддддддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY CLEAR-L25.
	     DISPLAY STK-BIN AT 1237
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE STK-QUANT	 TO W100-QNT5.
	     DISPLAY W100-QNT5 AT 1253
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5
	     DISPLAY "Press " AT 2512
010850		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "Y" WITH BACKGROUND-COLOR 3
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " to accept  "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 7 HIGHLIGHT
		     "ape to Cancel"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.

       BC66-B.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "X"	 TO WS-OPTION
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC66-B
	       END-EVALUATE
	       GO TO BC66-C
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF NOT (WS-OPTION = "Y")
		   GO TO BC66-B
	       END-IF
	   ELSE
	       CALL X"E5"
	       GO TO BC66-B.

       BC66-C.
	     PERFORM RESTORE-SCREEN.
	   IF WS-OPTION = "X"
021390	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
021400	       GO TO BC55.

       BC66-D.
	   IF WS-NEGATIVE = "Y"
021370	       ACCEPT S26-QNT-N AT LINE LIN
	   ELSE
021370	       ACCEPT S26-QNT AT LINE LIN.
	   IF ADIS-FUNC
	       IF LS0-LEV < 3
		   GO TO BC66
	       END-IF
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-P1 > 1
			 SUBTRACT 1  FROM WS-P1
			 GO TO BC65-PACK
		 WHEN DOWN-KEY
		     IF WS-P1 < 4
			 ADD 1	     TO WS-P1
			 GO TO BC65-PACK
	       END-EVALUATE
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY (LIN, 2) WS-MIV78
			     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
      *    *************************************************************
      *    ****    F O R M A T	 1 4   I N V O I C E S
      *    *************************************************************
		     IF PRG-NUMBER = 14
			 DISPLAY "мммммммммммммммммммммм" AT 0723
				  WITH BACKGROUND-COLOR 1
				       FOREGROUND-COLOR 3
		     END-IF
021400		     GO TO BC55
		 WHEN F6-KEY
		     PERFORM BB050
021370		     DISPLAY S26-QNT AT LINE LIN
		 WHEN F9-KEY
		     MOVE "Y"	 TO WS-NEGATIVE
		     GO TO BC66-D
		 END-EVALUATE.
	   IF (W10-QUANT = ZERO)
021390	       DISPLAY (LIN, 2) WS-MIV78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
021400	       GO TO BC55.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
	   IF STK-USE-CASES = "Y" AND WS-MEASURE = "N"
	       PERFORM JA000
	       IF W10-CASES = ZERO
		   GO TO BC65-RESTART.
      *    *************************************************************
      *    ****    F O R M A T	 1 4   I N V O I C E S
      *    *************************************************************
	   IF PRG-NUMBER = 14
	       DISPLAY "мммммммммммммммммммммм" AT 0723
			WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
021410	     DISPLAY CLEAR-L25.
             MOVE ZERO           TO WS-ERROR.
           IF STK-GUAR NOT = ZERO
               PERFORM BB030
           ELSE
               MOVE SPACES       TO W43-SERNO (1).
           IF WS-ERROR = 1
               GO TO BC66.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *    *************************************************************
       BC67.
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
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check if the account is set up as a COST plus to calculate Ё
      *    Ё the SELLING price. 					Ё
      *    Ё								Ё
      *	   Ё If DEB-DISC = 99 then selling price is equal to cost price.Ё
      *	   Ё If DEB-DISC > 69, then subtract it from 100 to get the	Ё
      *    Ё markup percentage.						Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF DEB-DISC = 99
      *    **************************************************************
      *    ****        ROUND THE PRICE TO TWO DECIMAL PLACES	     ****
      *    **************************************************************
	       COMPUTE W25-RESULT = W10-COST * 1
	       MOVE W25-RESULT	 TO W10-SELL
	       MOVE "N" 	 TO SAL-SPEC
	       MOVE W10-SELL	 TO W10-SPAPRC
	       DISPLAY S26-PRC AT LINE LIN
	       GO TO BC71.
	   IF DEB-DISC > 69
	       COMPUTE W25-RESULT = 100 - DEB-DISC
	       COMPUTE W10-SELL ROUNDED = W10-COST +
		      (W10-COST * (W25-RESULT / 100))
	       MOVE "N" 	 TO SAL-SPEC
	       MOVE W10-SELL	 TO W10-SPAPRC
	       DISPLAY S26-PRC AT LINE LIN
	       GO TO BC71.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check if the account is set up as a TRADE plus to calculateЁ
      *    Ё the SELLING price. If DEB-DISC > 59, then subtract it from Ё
      *    Ё 70 to get the markup percentage.				Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF DEB-DISC > 59
	       COMPUTE W25-RESULT = 70 - DEB-DISC
	       COMPUTE W10-SELL ROUNDED = STK-WSL(WS-P1) +
		      (STK-WSL(WS-P1) * (W25-RESULT / 100))
	       MOVE "N" 	 TO SAL-SPEC
	       MOVE W10-SELL	 TO W10-SPAPRC
	       DISPLAY S26-PRC AT LINE LIN
	       GO TO BC71.
021420	   IF STK-IND < 3
021550         IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
		   IF W10-SPEC = "Y"
		       MOVE W10-P-PRICE(WS-P1)
021460				 TO W10-SELL
		   ELSE
021560		   IF WS-PRC = "1"
021570		       MOVE STK-SL(WS-P1)
021580                           TO W10-SELL
021590             ELSE
021600		   IF WS-PRC = "2"
021610		       MOVE STK-CSH(WS-P1)
021620                           TO W10-SELL
021630             ELSE
021640		       MOVE STK-WSL(WS-P1)
021650                           TO W10-SELL
021660         ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
		   IF W10-SPEC = "Y"
		       MOVE W10-P-VPRICE(WS-P1)
021460				 TO W10-SELL
		   ELSE
021670		   IF WS-PRC = "1"
021680		       MOVE W10-VSL(WS-P1)
021690                           TO W10-SELL
021700             ELSE
021710		   IF WS-PRC = "2"
021720		       MOVE W10-VCSH(WS-P1)
021730                           TO W10-SELL
021740             ELSE
021750		       MOVE W10-VWSL(WS-P1)
021760                           TO W10-SELL.
	     MOVE W10-ENTRY	 TO W90-TAX.
	   IF NOT (PRG-NUMBER = 03)
	       GO TO BC69.
      *    *************************************************************
      *    ****   F R E S H   P R O D U C E  (FORMAT 3 INVOICE)     ****
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This Client requires the Cost and Average be displayed so Ё
      *    Ё as to give an idea of the discount that can be given.     Ё
      *    Ё							       Ё
      *    Ё For any User that uses this format Invoive and does not   Ё
      *    Ё require these figures to be displayed, remove or change   Ё
      *    Ё these instructions to remarks.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
	     MOVE STK-COST	 TO W100-COST.
	     MOVE STK-AVGE	 TO W100-AVRG.

       BC68.
021550	   IF (W90-TAX NOT = SPACES) AND (STK-TAX > 0)
014710	       MOVE W05-VAT(STK-TAX)
014720				 TO W05-RATE
               COMPUTE WS-PER = ((W10-SELL - ((W10-SELL / 
                      (100.0000 + W05-RATE)) * W05-RATE)
		       - W10-COST) / W10-COST) * 100.000 + .005
           ELSE
	       COMPUTE WS-PER = ((W10-SELL - W10-COST) / W10-COST)
                                  * 100.000 + .005.
	     DISPLAY "Cost: " AT 2501 WITH FOREGROUND-COLOR 1
					   BACKGROUND-COLOR 3
		     W100-COST WITH FOREGROUND-COLOR 6 HIGHLIGHT
				    BACKGROUND-COLOR 3
		     " Average: " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
		     W100-AVRG WITH FOREGROUND-COLOR 6 HIGHLIGHT
				    BACKGROUND-COLOR 3
		     " Markup: " WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     WS-PER WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3.

       BC69.
021790	     MOVE W10-SELL	 TO W10-SPAPRC.
	     MOVE W10-SPEC	 TO SAL-SPEC.
	   IF NOT (W10-SPEC = "Y")
	       DISPLAY "F9" AT 2502
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
		       " to view prices"
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 1.
	     MOVE "N"		 TO WS-NOCHARGE.

021770 BC70.
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
		     GO TO BC66
		 WHEN F9-KEY
		     IF NOT (W10-SPEC = "Y")
			 PERFORM LA000
		     END-IF
		     GO TO BC70
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   F10 - Used for No Charge item	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 WHEN F10-KEY
		     MOVE ZERO	 TO W10-SELL
		     PERFORM BC260
		     IF WS-NOCHARGE = "N"
			 GO TO BC70
	       END-EVALUATE.
	   IF NOT (PRG-NUMBER = 03)
	       GO TO BC71.
	     PERFORM BC68.
021790*	     MOVE W10-SELL	 TO W10-SPAPRC.
      *    *************************************************************
      *    ****   M I N I M U M   P R O M P T S   O R	F A S T
      * 	  M O D E   ?	 ( Skip Prompts)
      *    *************************************************************
       BC71.
	   IF WS-PROMPT = "N" OR "F"
	       MOVE "C" 	 TO WS-OPTION
	       GO TO BC74.
	     MOVE "'C'ontinue or 'R'ekey   [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
017330	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO BC71.
           IF WS-OPTION = "R"
017340	       GO TO BC70.

       BC74.
	   IF (W10-SELL = ZERO) AND
	      (WS-NOCHARGE = "Y")
	       DISPLAY CLEAR-L25
	       GO TO BC75.
	   IF W10-SELL < (W10-SPAPRC - 0.009)
021790	       MOVE W10-SELL	 TO W10-SPAPRC
021810	       PERFORM BB100
021820	       IF WS-ERROR = 1
021830		   MOVE ZERO	 TO WS-ERROR
021840		   GO TO BC67.
021850	     DISPLAY CLEAR-L25.

021860 BC75.
021870       MOVE W10-SELL       TO SAL-SELL.
	     MOVE W10-SPEC	 TO SAL-SPEC.

021880 BC76.
021890	   IF STK-IND > 1
021900         GO TO BC80.
	   IF WS-PRC-IND = "N"
	       IF STK-IND > ZERO
		   GO TO BC80.
      *    *************************************************************
      *    ****    C H E C K   S T O C K   Q U A N T I T I E S
      *    *************************************************************
	   IF (NOT (W40-CODE = SPACES) AND
                   (WST-QUANT = ZERO))
               OR
              ((W40-CODE = SPACES) AND
021910        (STK-QUANT = ZERO))
021920         DISPLAY "No stock - Continue (Y/N) > <" AT 2512
021930			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
021940         DISPLAY "'Y' will make stock holding negative" AT 2542
021950			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
	       PERFORM TEST AFTER UNTIL WS-OPTION = "Y" OR "N"
021960		 ACCEPT WS-OPTION AT 2539
			WITH BACKGROUND-COLOR 5
			     FOREGROUND-COLOR 7 HIGHLIGHT AUTO
		 CALL "CBL_TOUPPER" USING WS-OPTION
				    BY VALUE WS-LENGTH
				    RETURNING WS-STATUS
	       END-PERFORM
022020	       DISPLAY CLEAR-L25
022030         IF WS-OPTION = "N"
022040		   MOVE W10-QNT  TO W10-REORD
022060             GO TO BC55.
      *    *************************************************************
      *    ****   W A R E H O U S E   S A L E	?
      *    *************************************************************
	   IF (NOT (W40-CODE = SPACES) AND
		   (W10-QNT NOT > WST-QUANT))
               OR
022070        ((W40-CODE = SPACES) AND
	       (W10-QNT	NOT > STK-QUANT))
022090         GO TO BC80.

       BC78.
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
018360	     MOVE 1		 TO WS-LENGTH.
	     PERFORM RESTORE-SCREEN.

       BC78E.
	     IF NOT (W40-CSHPASS = W85-SUPER (1) OR
		     W85-SUPER (7) OR "!@#$%^")
		 GO TO BC55.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
022290 BC80.
022300	     MOVE W10-QUANT	 TO SAL-QNT.
	     MOVE W10-CASES	 TO SAL-CASES.
	     MOVE WS-P1 	 TO SAL-PACK.
	   IF NOT (STK-FACT = ZERO)
	       COMPUTE SAL-CONVERT = W10-QUANT * WS-P1 * STK-FACT.
022310	     COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT)
022320       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
022330	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
022350       MOVE ZERO           TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****   F A S T   M O D E   ?     ( Skip Prompts)
      *    *************************************************************
	   IF (WS-PROMPT = "F") AND
	      (DEB-DISC = 0)
	       GO TO BC90.
022340	     DISPLAY "Discount %" AT 2518
		     WITH BACKGROUND-COLOR 3
			  FOREGROUND-COLOR 6 HIGHLIGHT.
      *    *************************************************************
      *    **	I F   D I S C O U N T	A P P L I C A B L E   G E T   **
      *    **	P E R C E N T A G E   F R O M	D E P A R T M E N T   **
      *    **	F I L E 					      **
      *    *************************************************************
022360	   IF (DEB-DISC > 0 AND < 11)
022370	       MOVE STK-LDG	 TO DPT-CODE
022380         PERFORM READ-DEPART THRU READ-DEPART-EXIT
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

022410 BC85.
022420       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
022540		     DISPLAY CLEAR-L25
		     GO TO BC70
	       END-EVALUATE.
022430     IF WS-DISC = ZERO
022540	       DISPLAY CLEAR-L25
022440         GO TO BC90.
	   IF (DEB-DISC > 0 and < 11)
	       IF WS-DISC > DPT-DSETT(DEB-DISC)
		   PERFORM BC78
		   IF W40-CSHPASS NOT = W85-SUPER(1)
		       MOVE DPT-DSETT(DEB-DISC)
                                 TO WS-DISC.
022450       COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
022460              (W10-SELL * WS-DISC / 100.00000).
      *    *************************************************************
      *    **** 	    C H E C K	M A R G I N		    ****
      *    *************************************************************
022470	   IF (DEB-DISC > 0 and < 11)
022480	       IF DPT-DSETT(DEB-DISC) = WS-DISC
022490             GO TO BC86.
022500       PERFORM BB100.
022510     IF WS-ERROR = 1
022520         GO TO BC85.

022530 BC86.
022540	     DISPLAY CLEAR-L25.
022550       MOVE WS-DISC        TO SAL-DSC.
022560	     COMPUTE W90-CDISC ROUNDED =
                   ((W25-VALUE * WS-DISC) / 100.00000).
022570	     MOVE W90-CDISC	 TO W100-PRC SAL-DISC.
022580	     DISPLAY W100-PRC AT 2540
		     WITH BACKGROUND-COLOR 3
			  FOREGROUND-COLOR 7 HIGHLIGHT.

022590 BC90.
      *    *************************************************************
      *    **** 	      V E R M O N T   S A L E S 	    ****
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Does this customer require Barcode Labels.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
	   IF PRG-NUMBER = 8
	       MOVE "Bar Code Lables Y/N  [N]"
				 TO WS-ERR-MES
	       MOVE "N" 	 TO WS-OPTION
	       PERFORM OPT-MESSAGE
017330	       IF NOT (WS-OPTION = "Y" OR "N")
		   GO TO BC90
	       END-IF
017340	       MOVE WS-OPTION	 TO SAL-STATUS
	   ELSE
	   IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
	       MOVE "Y" 	 TO SAL-STATUS W40-SPAR.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
022700 BC95.
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
022720	       COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
022730       ADD W90-CDISC       TO W90-LDISC.
022740       SUBTRACT W90-CDISC FROM W25-VALUE 
022750                           GIVING W90-VALUE.
022840	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C") OR
	      (STK-EXEMPT = "Y" AND DEB-ART11 = "Y")
022850         ADD W90-VALUE     TO W90-NONTAX
022860	       MOVE W90-VALUE	 TO SAL-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
022870     ELSE
022880         MOVE W90-VALUE    TO SAL-TAXVAL
	       IF W90-EXP = "I"
		   COMPUTE W90-TAXVAL(WS-VAT-SUB) ROUNDED =
			   W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
	       ELSE
022890		   ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
	       END-IF
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB.
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
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *    *************************************************************
022940       ADD W25-VALUE       TO W90-DB.

022950 BC100.
022960       MOVE W90-DB         TO W90-TOTAL.
022970       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022980	     MOVE W90-DB	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023000       MULTIPLY -1 BY W90-LDISC 
023010				 GIVING W100-VAL.
023020	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
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
			  BACKGROUND-COLOR 5.

023130 BC102.
023140	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

023160 BC105.
023210	     MOVE "A"		 TO WS-OPTION.
      *    *************************************************************
      *    ****   F A S T   M O D E   ?     ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "F"
	       GO TO BC111.
036100	     MOVE "'A'ccept or 'C'ancel   [A]"
				 TO WS-ERR-MES.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *    *************************************************************
023220 BC110.
	     PERFORM OPT-MESSAGE.
023300	   IF NOT (WS-OPTION = "A" OR "C")
023310	       GO TO BC105.
023330     IF WS-OPTION = "C"
023340	       DISPLAY (LIN, 2) WS-MIV78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
023350         PERFORM BC114
023360         PERFORM BC100 THRU BC102
023370         GO TO BC55.               

023380 BC111.
             MOVE WS-SUPER       TO SAL-SUPER.
023390       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
	     MOVE WS-SALKEY	 TO W100-SER.
	     DISPLAY "Lines" AT 2330
		     W100-SER AT 2336
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023400       ADD 1               TO WS-LAST.
           IF W43-SERNO (1) = SPACES
               GO TO BC111E.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 1              TO WS-S1.
	     MOVE SPACES	 TO SAL-EXT-ITEM.

       BC111A.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SAL-MSER.

       BC111B.
             MOVE W43-SERNO (WS-S1)
                                 TO SAL-SNO (WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BC111C
               GO TO BC111E.
           IF WS-S3 < 3
               ADD 1             TO WS-S3
               GO TO BC111B.

       BC111C.
             ADD 1               TO SAL-SUB.         
021110       MOVE WS-LAST        TO WS-SALKEY.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
	     MOVE WS-SALKEY	 TO W100-SER.
	     DISPLAY "Lines" AT 2330
		     W100-SER AT 2336
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
023400	     ADD 1		 TO WS-LAST.

       BC111D.
	   IF NOT (WS-S1 > WS-S2)
               GO TO BC111A.
      *    *************************************************************
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *    *************************************************************
       BC111E.
	   IF W10-LENGTH > ZERO AND
	      W10-BREADTH > ZERO AND
	      W10-NUMBER > ZERO
023410	       IF LIN < 18
023420		   ADD 1	 TO LIN
	       ELSE
023440		   MOVE 8	 TO WS-S7
023450		   PERFORM BC111-SHIFT
	       END-IF
023760	       MOVE WS-LAST	 TO WS-SALKEY
023770	       INITIALIZE SAL-RECORD1
023780	       MOVE WS-LAST	 TO SAL-CODE
	       MOVE SPACES	 TO W10-RMKS
	       MOVE W10-LENGTH	 TO W100-LENGTH
	       MOVE W10-BREADTH  TO W100-BREADTH
	       MOVE W10-NUMBER	 TO W100-NUMBER
	       STRING W100-LENGTH DELIMITED SIZE
		      " by " DELIMITED SIZE
		      W100-BREADTH DELIMITED SIZE
		      " X " DELIMITED SIZE
		      W100-NUMBER DELIMITED SIZE
		      INTO W10-RMKS
023790	       MOVE W10-RMKS	 TO SAL-MSER
	       MOVE WS-SUPER	 TO SAL-SUPER
023390	       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT
023710	       DISPLAY (LIN, 21) W10-RMKS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0 UPDATE AUTO
	       MOVE WS-SALKEY	 TO W100-SER
	       DISPLAY "Lines" AT 2330
		       W100-SER AT 2336
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
023400	       ADD 1		 TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
023410     IF LIN < 18
023420         ADD 1             TO LIN
023430         GO TO BC112.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L	 D E T A I L   L I N E S   U P
      *    *************************************************************
       BC111-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BC111-SHIFT.
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

023470 BC112.
	     MOVE SPACES     TO WS-ERR-MES.
      *    *************************************************************
      *    ****   F A S T   M O D E   ?      ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "F"
023660         GO TO BC55.
036410	     STRING "'R'emarks, 'N'ext or 'E'nd  ["
		    DELIMITED SIZE
		    WS-INPUT DELIMITED SIZE
		    "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
036470	     MOVE WS-INPUT	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023620	   IF NOT (WS-OPTION = "R" OR "N" OR "E")
023630         GO TO BC112.
             MOVE WS-OPTION      TO WS-INPUT.
023650     IF WS-OPTION = "N"
023660         GO TO BC55.
023670     IF WS-OPTION = "E"
023680         SUBTRACT 1        FROM WS-LAST
023690         GO TO BC120.
023700	     MOVE SPACES	 TO W10-RMKS W43-SERNO (1).
023710	     ACCEPT (LIN, 21) W10-RMKS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
023730	   IF W10-RMKS = SPACES
023740         GO TO BC112.
023760	     MOVE WS-LAST	 TO WS-SALKEY.
023770       INITIALIZE SAL-RECORD1.
023780       MOVE WS-LAST        TO SAL-CODE.
023790	     MOVE W10-RMKS	 TO SAL-MSER.
023800       GO TO BC111.

023810 BC114.
023820       SUBTRACT W25-VALUE  FROM W90-DB.
023830       SUBTRACT W90-CDISC  FROM W90-LDISC.
023840       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL
				 FROM W90-TAXVAL(SAL-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830     IF (STK-IND < 2) OR (STK-IND = 3)
023870         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
023880 BC115.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
023900       ADD 1               TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
023910 BC120.
023920	   IF WS-LAST = ZERO
023930         GO TO BC05.
      *    *************************************************************
      *    ****   F A S T   M O D E   ?     ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "F"
	       ADD 1		 TO WS-LAST
	       GO TO BC55.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   S A L E   ?
      *    *************************************************************
       BC125.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

024040 BC130.
	     PERFORM OPT-MESSAGE.
024110	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
024120	       GO TO BC125.
024140     IF WS-OPTION = "M"
024150         ADD 1             TO WS-LAST
024160         GO TO BC112.
024170     IF WS-OPTION = "A"
024180	       PERFORM BC270A
024190         GO TO BC125.
024200     IF WS-OPTION = "C"
	       GO TO BC250.
024220       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
024230     IF W90-LDISC NOT = ZERO
024240         MOVE W90-LDISC    TO W90-TDISC
024250         GO TO BC140.
024260       MOVE W90-DB         TO W90-DVAL.

024290 BC140.
024300       SUBTRACT W90-TDISC FROM W90-DB 
024310                           GIVING W90-TSELL W90-TOTAL.
024320       MULTIPLY -1 BY W90-TDISC 
024330				 GIVING W100-VAL.
024340	     DISPLAY W100-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
024350	     MOVE ZERO		 TO W90-TTAX.
	     MOVE 1		 TO WS-P1.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BC141.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BC142.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
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
       BC142.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BC141.
037540	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
024430	       GO TO BC144.
024470	     MOVE W90-TTAX	 TO W100-VAL.
024480	     DISPLAY W100-VAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
024490       ADD W90-TTAX        TO W90-TOTAL.
      *    *************************************************************
      *    ****    C H E C K   I F   H I G H   V A L U E
      * 	   W A R N I N G   R E Q U I R E D
      *    *************************************************************
024500 BC144.
	   IF WS-INV-CHECK = "N"
	       GO TO BC185.
	   IF NOT (WS-INV-AMT < W90-TOTAL)
	       GO TO BC185.

       BC145.
	     MOVE "High invoice value - Accept (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BC145.
           IF WS-OPTION = "N"
	       GO TO BC125.
      *    *************************************************************
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *    *************************************************************
025130 BC185.
025150	     MOVE "M"		 TO WS-AGE.
      *    *************************************************************
      *    ****   F A S T   M O D E   ?     ( Skip Prompts)
      *    *************************************************************
	   IF WS-PROMPT = "F"
	       MOVE "Y" 	 TO WS-PRNINV
	       GO TO BC186-FAST.
	     MOVE "Print Invoice Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PRNINV.
	   IF NOT (WS-PRNINV = "Y" OR "N")
	       GO TO BC185.

       BC186-FAST.
	     MOVE WS-PRNINV	 TO W40-PRNINV.
	     MOVE WS-LAST	 TO W40-LAST.
	     MOVE WS-PRC	 TO W40-PRC.
	     MOVE W10-SMAN	 TO W40-SMAN.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   S A L E
      *    *************************************************************
	   IF DEB-TERMS = "3"
      *    *************************************************************
      *    ****   C O D   A C C O U N T
      *    *************************************************************
	       PERFORM BB000.
	   IF WS-PARCELS = "Y"
      *    *************************************************************
      *    ****  G E T	 T H E	 N U M B E R   O F   P A R C E L S
      *    *************************************************************
	       PERFORM IA000.
	     DISPLAY CLEAR-L25.
025430	     DISPLAY "Processing Invoice" AT 2502 WITH
025440		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
029610	     CLOSE SALE.
	     OPEN I-O SALE.
	     MOVE "N"		 TO WS-OPTION.
	   IF NOT (PRG-NUMBER = 07)
	       MOVE WS-OPTION	 TO W40-REPRINT
	       GO TO BC186-CONTINUE.
	     MOVE "Print Invoice Heading (Y/N)?  [N]"
				 TO WS-ERR-MES.

       BC186-HEADING.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BC186-HEADING.
	   IF WS-OPTION = "Y"
	       MOVE "R" 	 TO W40-REPRINT
	   ELSE
	       MOVE WS-OPTION	 TO W40-REPRINT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  V A T   C A L C U L A T I O N   P R O B L E M  -	F I X  Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё Check if the VAT entries are in balance (W90-). If they   Ё
      *    Ё are out of balance, correct the taxable value.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *
      *    See paragraph BC141 which has been changed and if the problem
      *    persists, then this routine will be introduced.
      *
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Call the Invoice program to update the account, generate  Ё
      *    Ё integration entries, transaction record, VAT report entry,Ё
      *    Ё sales person entry, audit entries, update statistics and  Ё
      *    Ё Sales Ledgers.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BC186-CONTINUE.
	     MOVE WS-CR 	 TO W40-CR.
	   IF PRG-NUMBER = 99
	       CALL "INV\INVF12W" USING WS-PARID
					LS-USER-ID
					LS0-PROGRAMS
					LS0-SECURITY
					W40-COMPANY
					W90-TRAN
		     ON EXCEPTION
			GO TO BC187
	       END-CALL
	   ELSE
	       CALL PRG-NAME3 USING WS-PARID
				    LS-USER-ID
				    LS0-PROGRAMS
				    LS0-SECURITY
				    W40-COMPANY
				    W90-TRAN
		    ON EXCEPTION
		       GO TO BC187
	       END-CALL
	   END-IF.
	   IF PRG-NUMBER = 99
	       CANCEL "INV\INVF12W"
	   ELSE
	       CANCEL PRG-NAME3
	   END-IF.
	     GO TO BC188.

       BC187.
	     MOVE SPACES     TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME3 DELIMITED " "
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
029820         GO TO BC250.
029830       GO TO BC05.
      *    *************************************************************
      *    ****        T E S T	 F O R	 I N D I V I D U A L	    ****
      *    *************************************************************
       BC189.
025570	   IF W80-TYPE NOT = 1
025580         GO TO BC210.
025590       MOVE DEB-NAME       TO W60-NAME.
025600       PERFORM CA165-NAME.
      *    *************************************************************
      *    ****    T I T L E  ---  New version only has E as Language
      *    *************************************************************
025610     IF DEB-LANG = "A"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			 A F R I K A A N S		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
025620         MOVE W75-ATITLE (DEB-TITLE) 
025630                           TO W60-NAME
025640	   ELSE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			   E N G L I S H		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
025650	       MOVE W75-ETITLE (DEB-TITLE)
025660                           TO W60-NAME.
025670       MOVE 7              TO WS-S6.

025680 BC190.
025690     IF W60-NCHAR (WS-S6) NOT = SPACE
025700         GO TO BC195.
025710       SUBTRACT 1          FROM WS-S6.
025720     IF WS-S6 > 0
025730         GO TO BC190.
      *    *************************************************************
      *    **** 	    F I R S T	I N I T I A L		    ****
      *    *************************************************************
025740 BC195.
025750	     MOVE 1		 TO WS-S5.
025760       ADD 2               TO WS-S6.
025770       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
025780       ADD 1               TO WS-S6.
025790       MOVE "."            TO W60-NCHAR (WS-S6).
025800     IF W40-NCHAR (49) = SPACE
025810         GO TO BC200.
      *    *************************************************************
      *    **** 	   S E C O N D	 I N I T I A L		    ****
      *    *************************************************************
025820       ADD 1               TO WS-S6.
025830       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
025840       ADD 1               TO WS-S6.
025850       MOVE "."            TO W60-NCHAR (WS-S6).

025860 BC200.
025870       ADD 2               TO WS-S6.
      *    *************************************************************
      *    **** 		  S U R N A M E			    ****
      *    *************************************************************
025880 BC205.
025890	     MOVE W40-NCHAR (WS-S5)
025900                           TO W60-NCHAR (WS-S6).
025910     IF WS-S5 < 24
025920         ADD 1             TO WS-S5
025930         ADD 1             TO WS-S6
025940         GO TO BC205.
025950       MOVE W60-NAME       TO W40-NAME.
      *    *************************************************************
      *    ****    A D D R E S S E S
      *    *************************************************************
025960 BC210.
025980	     MOVE SPACES	 TO W40-PC1 W40-PC2.
025990       MOVE DEB-ADDRESS    TO W60-ADDRESS.
026000       PERFORM CA175-ADDRESS.
026010	   IF NOT (DEB-POST = "0000" OR "9999")
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
026170	   IF NOT (DEB-PPOST = "0000" OR "9999")
026180         MOVE DEB-PPOST    TO W40-PC2.
	     PERFORM BB150.
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
      *    *************************************************************
      *    **		 C l i e n t   V A T   n u m b e r	      **
      *    *************************************************************
	     MOVE DEB-TAX	 TO W40-TAX.

       BC215.
	     EXIT.

       BC250.
	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		N O   C H A R G E   P A S S W O R D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       BC260		SECTION 60.
       BC260-START.
      *    *************************************************************
      *    **  S A V E	 C U R R E N T	 S C R E E N   D E T A I L S  **
      *    *************************************************************
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-CSHPASS.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддд" AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "©" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "Ё " AT 1226 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "No Charge Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ю" AT 1326 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ддддддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1247
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     PERFORM RESTORE-SCREEN.
018360	     MOVE 1		 TO WS-LENGTH.
	   IF NOT(W40-CSHPASS = "NOCHAR")
	       MOVE "N" 	 TO WS-NOCHARGE
	   ELSE
	       MOVE "Y" 	 TO WS-NOCHARGE
	       DISPLAY S26-PRC AT LINE LIN
	       MOVE 99		 TO WS-SUPER.

       BC269.
	     EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BC270A		SECTION 61.
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
			   BACKGROUND-COLOR 5 UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "R"
               ACCEPT W90-REG AT 0554 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               GO TO BC271A.

       BC271C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BC271C.
             MOVE SPACES         TO W40-ASSIST.
	     MOVE W10-SMAN	 TO W40-SMAN.
017740     IF W10-SMAN = ZERO
	       DISPLAY "╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟"	 AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BC271A.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810	       GO TO BC271C.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
             GO TO BC271A.

       BC272.
029990       MOVE 8              TO LIN.
030000       PERFORM BC115 UNTIL LIN > 18.
030010       MOVE 8              TO LIN.
030020	     MOVE 1		 TO WS-LINES WS-FIRST.

030030 BC275.
030040	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
030050 BC280.
030060	   IF WS-SALKEY > WS-LAST
030070	       GO TO BC284.
030080       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120         GO TO BC282.
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
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
030210 BC282.
030220     IF LIN < 18
030230         ADD 1             TO LIN WS-SALKEY WS-LINES
030240         GO TO BC280.

       BC284.
	     MOVE 7		 TO LIN.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
030250 BC285.
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
      *    *************************************************************
      *    ****  S A V E   C U R R E N T   S C R E E N	 D E T A I L S
      *    *************************************************************
	     PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N	 A T T R I B U T E S
      *    *************************************************************
       BC289.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
030520 BC290.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BC292
		 WHEN DOWN-KEY
001340		     GO TO BC293
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC290
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BC335
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-DOWN
001340		     GO TO BC305
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-UP
001340		     GO TO BC300
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC290
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 7)
		   DISPLAY CLEAR-L25
		   GO TO BC310.
	     CALL X"E5".
001280	     GO TO BC290.
      *    *************************************************************
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *    *************************************************************
       BC292.
	   IF (WS-SALPOS = 1) OR (LIN < 8)
	       GO TO BC294.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 8
	       SUBTRACT 1	 FROM LIN
	       GO TO BC294.
	     MOVE 18		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *    *************************************************************
       BC292-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 9
	       SUBTRACT 1	 FROM WS-S7
	       GO TO BC292-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
       BC292-DISPLAY.
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
030120	       GO TO BC292-END-DISP.
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

       BC292-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO BC294.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *    *************************************************************
       BC293.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO BC294.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
	   IF LIN > 7
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO BC294.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *    *************************************************************
       BC293-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BC293-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO BC292-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
001980 BC294.
001990	     PERFORM BC289.
	   IF LIN > 7
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO BC290.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
030780 BC295.
030790	     MOVE 8		 TO LIN.
030800       PERFORM BC115 UNTIL LIN > 18.
030810	     MOVE 8		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *    *************************************************************
       BC300.
	   IF WS-FIRST = 1
	       GO TO BC290.
	   IF WS-FIRST > 11
	       SUBTRACT 11	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM BC295.
	     GO TO BC275.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BC305.
	   IF (WS-FIRST + 11) > WS-LAST
	       GO TO BC290.
	     ADD 11		 TO WS-FIRST.
	     PERFORM BC295.
	     GO TO BC275.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BC310.
030630	   IF WS-OPTION = "A"
030640	       GO TO BC320.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
       BC315.
043780	     MOVE "Are you sure Y/N?  [N]"
				 TO WS-ERR-MES.
030880	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF NOT (WS-OPTION = "N" OR "Y")
030980	       GO TO BC315.
031000     IF WS-OPTION = "N"
031010	       GO TO BC285.
031020       MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
       BC320.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
031240	   IF SAL-EXT-ITEM = SPACES
031250	       GO TO BC322.
031260	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
031270       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
031280       MOVE SAL-QNT        TO W10-QUANT.
	     MOVE SAL-CASES	 TO W10-CASES.
031290       MOVE SAL-COST       TO W10-COST.
031300       MOVE SAL-SELL       TO W10-SELL.
	     MOVE SAL-SPEC	 TO W10-SPEC.
031310       MOVE SAL-DISC       TO W90-CDISC.
031320       MOVE SAL-DSC        TO WS-DISC.
031330	     MOVE SAL-VAL	 TO W25-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE SAL-PACK	 TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E   T O T A L S 
      *    *************************************************************
031340       SUBTRACT W25-VALUE  FROM W90-DB.
031350       SUBTRACT W90-CDISC  FROM W90-LDISC.
031360	     SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF WS-VAT-SUB > ZERO
031370	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(WS-VAT-SUB).
019830     IF (STK-IND < 2) OR (STK-IND = 3)
031390         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S   I T   A N   A M E N D M E N T
      *    *************************************************************
031400 BC322.
	     PERFORM BC289.
031410     IF WS-OPTION = "A"
031420         GO TO BC325.
      *    *************************************************************
      *    ****    R E - D I S P L A Y   T O T A L S
      *    *************************************************************
031430	   IF SAL-EXT-ITEM NOT = SPACES
031440         PERFORM BC100 THRU BC102.
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
031450 BC324.
031460	     MOVE WS-SALKEY	 TO WS-SALSTR.
031470       ADD 1               TO WS-SALKEY.
031480     IF WS-SALKEY > WS-LAST
031490         SUBTRACT 1        FROM WS-LAST
031500         GO TO BC272.
031510       PERFORM READ-SALE THRU READ-SALE-EXIT.
031520       SUBTRACT 1          FROM SAL-CODE.
031530       MOVE WS-SALSTR      TO WS-SALKEY.
031540       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031550       ADD 1               TO WS-SALKEY.
031560	     GO TO BC324.

031570 BC325.
	     PERFORM BC289.
031580	   IF SAL-EXT-ITEM = SPACES
031590	       ACCEPT (LIN, 21) SAL-SDSC
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
031610         GO TO BC332.
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
031620       ACCEPT S26-QNT AT LINE LIN.
031640     IF W10-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
031670	       MOVE SAL-QNT	 TO W10-QUANT
	       MOVE SAL-CASES	 TO W10-CASES
031680         GO TO BC325.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
031690       MULTIPLY W10-SELL BY W10-QUANT 
031700                           GIVING W25-VALUE.
031710       MOVE W25-VALUE      TO W100-EXT.
031720	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     PERFORM CHECK-CORRECT.
044680	   IF WS-OPTION = "N"
044690	       GO TO BC325.
      *    *************************************************************
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *    *************************************************************
031830       MOVE W10-QUANT      TO SAL-QNT.
	     MOVE W10-CASES	 TO SAL-CASES.
031840       MOVE W25-VALUE      TO SAL-VAL.
	   IF NOT (STK-FACT = ZERO)
	       COMPUTE SAL-CONVERT = W10-QUANT * WS-P1 * STK-FACT.
031850	   IF WS-DISC = ZERO
031860         GO TO BC330.
031870       COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
031880       MOVE W90-CDISC      TO SAL-DISC.
      *    *************************************************************
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *    *************************************************************
031890 BC330.
031900	     PERFORM BC95 THRU BC102.

031910 BC332.
031920       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031930	     GO TO BC272.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S
      *    *************************************************************
031940 BC335.
	     PERFORM BC295.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E
      *    *************************************************************
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 BC340.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO BC345.
      *    *************************************************************
      *    ****    E X T E N D E D   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    ****    A L P H A - N U M E R I C   C O D E
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
       BC345.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO BC340.

       BC350.
031950	     EXIT.
      *    *************************************************************
      *    ****    P R O M P T S   ( C H O I C E )
      *    *************************************************************
       DA000	       SECTION 62.
       DA300.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "зддддддддддддддддд©" AT 0843
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё  P R O M P T S  Ё" AT 0943
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "цддддддддддддддддд╢" AT 1043
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё " AT 1143
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "P" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "rompts (Slow)  Ё"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё " AT 1243
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "M" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "inimum Prompts Ё"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё " AT 1343
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "F" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "ast Capture    Ё"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "юддддддддддддддддды" AT 1443
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 45		 TO SHADE-COL.
	     MOVE 17		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     PERFORM HIDE-THE-CURSOR.

       DA305.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO DA305.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "P" OR "M" OR "F")
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO DA305.
	   IF WS-OPTION = "M"
	       MOVE "N" 	 TO WS-OPTION.
	   IF WS-OPTION = "P"
	       MOVE "Y" 	 TO WS-OPTION.
	     PERFORM DISPLAY-THE-CURSOR.
	     PERFORM RESTORE-SCREEN.
	     MOVE WS-OPTION	 TO WS-PROMPT.

	DA999.
	     EXIT.
      *    *************************************************************
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *    *************************************************************
       GA000	       SECTION 63.
       GA00.
	     PERFORM SAVE-SCREEN.
004200	     CALL "DTP\DTP001"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "DTP\DTP001".
	     PERFORM RESTORE-SCREEN.
       GA99.
             EXIT.
      /    *************************************************************
      *    ****    W A R E H O U S E
      *    *************************************************************
       HA000	       SECTION 64.
       HA00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "здддддддддддддддддддддддд©" AT 1028
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ё Warehouse Code:        Ё" AT 1128
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "юдддддддддддддддддддддддды" AT 1228
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO W40-CODE.

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
		      PERFORM RESTORE-SCREEN-3
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
015830	       IF W40-CODE = SPACE
015840		   GO TO HA05.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
	   IF W40-CODE = SPACES
	       PERFORM RESTORE-SCREEN-3
	       GO TO HA999.
013310	     MOVE 6		 TO W50-LENGTH.

       HA10.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
	       GO TO HA10.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.
013380	     MOVE W50-DENT	 TO WAR-CODE.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50W THRU AA59
	       DISPLAY W40-CODE AT 1146
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
018640	       IF W40-CODE = SPACES
018720		   GO TO HA05
               ELSE
		   GO TO HA10.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "╣" AT 0707
		     "W/H " WITH FOREGROUND-COLOR 0
		      W40-CODE WITH FOREGROUND-COLOR 0 "ф".
	     DISPLAY "╣" AT 0721
		      WAR-SALE WITH FOREGROUND-COLOR 0 "ф".
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       PERFORM RESTORE-SCREEN-3
	       GO TO HA00.

       HA999.
	     EXIT.

       COPY WARHSE.LUS.

      /    *************************************************************
      *    ****    P A R C E L S   ( T I M E   S T A T I O N E R S )
      *    *************************************************************
       IA000	       SECTION 65.
       IA00.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "зддддддддддддддд©" AT 0843
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	   IF PRG-NUMBER = 14
	       DISPLAY "Ё NO OF PARCELS Ё" AT 0943
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 6
	   ELSE
	       DISPLAY "Ё NO OF CARTONS Ё" AT 0943
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 6.
	     DISPLAY "цддддддддддддддд╢" AT 1043
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё               Ё" AT 1143
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "юддддддддддддддды" AT 1243
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 45		 TO SHADE-COL.
	     MOVE 15		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

       IA05.
	     ACCEPT S37 AT 1150.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W40-PARCELS
		     GO TO IA10
	       END-EVALUATE.

       IA10.
	     PERFORM RESTORE-SCREEN.

       IA999.
	     EXIT.
      /    *************************************************************
      *    ****    C A S E S
      *    *************************************************************
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
      /    *************************************************************
      *    ****    P R O M O T I O N S	 &   S P E C I A L S
      *    *************************************************************
       KA000	       SECTION 67.
       KA00.
	   IF NOT (STK-IND < 2)
	       GO TO KA05.
	   IF STK-PRO-START > W12-T-YMD
	       GO TO KA05.
	   IF (STK-PRO-START = W12-T-YMD) AND
	      (STK-PRO-START-T > W10-HHMM)
	       GO TO KA05.
	   IF STK-PRO-END < W12-T-YMD
	       GO TO KA05.
	   IF (STK-PRO-END = W12-T-YMD) AND
	      (STK-PRO-END-T < W10-HHMM)
	       GO TO KA05.
	   IF (STK-PRO-PRICE = ZERO) AND
	      (STK-PRO-PRICE-2 = ZERO) AND
	      (STK-PRO-PRICE-3 = ZERO) AND
	      (STK-PRO-PRICE-4 = ZERO)
	       GO TO KA05.
	     MOVE "Y"		 TO W10-SPEC.
	     MOVE STK-PRO-PRICE  TO W10-PRO-PRICE.
	     MOVE STK-PRO-PRICE-2
				 TO W10-PRO-PRICE-2.
	     MOVE STK-PRO-PRICE-3
				 TO W10-PRO-PRICE-3.
	     MOVE STK-PRO-PRICE-4
				 TO W10-PRO-PRICE-4.
	   IF STK-TAX > 0
	       MOVE W05-VAT(STK-TAX)
				 TO W05-RATE.
	   IF NOT (W10-PRO-PRICE = ZERO)
	       COMPUTE W10-PRO-VPRICE ROUNDED
		     = W10-PRO-PRICE + (W10-PRO-PRICE * W05-RTE).
	   IF NOT (W10-PRO-PRICE-2 = ZERO)
	       COMPUTE W10-PRO-VPRICE-2 ROUNDED
		     = W10-PRO-PRICE-2 + (W10-PRO-PRICE-2 * W05-RTE).
	   IF NOT (W10-PRO-PRICE-3 = ZERO)
	       COMPUTE W10-PRO-VPRICE-3 ROUNDED
		     = W10-PRO-PRICE-3 + (W10-PRO-PRICE-3 * W05-RTE).
	   IF NOT (W10-PRO-PRICE-4 = ZERO)
	       COMPUTE W10-PRO-VPRICE-4 ROUNDED
		     = W10-PRO-PRICE-4 + (W10-PRO-PRICE-4 * W05-RTE).
	     GO TO KA999.

       KA05.
	     MOVE "N"		 TO W10-SPEC.

       KA999.
	     EXIT.
      /    *************************************************************
      *    ****    V I E W   P R I C E S   ( 1, 2, 3 )
      *    *************************************************************
       LA000	       SECTION 68.
       LA00.
021420	   IF STK-IND < 3
021550         IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
021570
		   MOVE STK-SL(WS-P1)
021580				 TO W30-PRICE1
021610		   MOVE STK-CSH(WS-P1)
021620				 TO W30-PRICE2
021640		   MOVE STK-WSL(WS-P1)
021650				 TO W30-PRICE3
021660         ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
021680		   MOVE W10-VSL(WS-P1)
021690				 TO W30-PRICE1
021720		   MOVE W10-VCSH(WS-P1)
021730				 TO W30-PRICE2
021750		   MOVE W10-VWSL(WS-P1)
021760				 TO W30-PRICE3
	       END-IF
	   ELSE
	       GO TO LA999.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "зддддддддддддддддддддд©" AT 0833
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё SELECT PRICE TO USE Ё" AT 0933
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё  1 =                Ё" AT 1033
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё  2 =                Ё" AT 1133
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "Ё  3 =                Ё" AT 1233
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "юддддддддддддддддддддды" AT 1333
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE W30-PRICE1	 TO W100-SELL.
	     DISPLAY W100-SELL AT 1041
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 6.
	     MOVE W30-PRICE2	 TO W100-SELL.
	     DISPLAY W100-SELL AT 1141
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 6.
	     MOVE W30-PRICE3	 TO W100-SELL.
	     DISPLAY W100-SELL AT 1241
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 35		 TO SHADE-COL.
	     MOVE 21		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

       LA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO LA05.
	   IF NOT (WS-OPTION = "1" OR "2" OR "3")
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO LA05.
	     MOVE WS-OPTION	 TO WS-S1.
	     MOVE W30-PRICE(WS-S1)
				 TO W10-SELL W10-SPAPRC.
	     PERFORM RESTORE-SCREEN.

       LA999.
	     EXIT.
      /    *************************************************************
      *    **** 	S A L E S   R E S T R I C T I O N S	    ****
      *    *************************************************************
       MA000	       SECTION 69.
       MA00.
	     MOVE 1		 TO WS-S3.
	     MOVE "N"		 TO WS-RESTCODE.

       MA05.
	   IF STK-LDG = DBD-DEPT-CDE(WS-S3)
	       MOVE "Y" 	 TO WS-RESTCODE
	   ELSE
	   IF WS-S3 < 10
	       ADD 1		 TO WS-S3
	       GO TO MA05.

       MA999.
	     EXIT.

      /    *************************************************************
      *    **** 	    S A L E S	O R D E R S
      *    *************************************************************
       YA000	       SECTION 90.
       YA00.
	     MOVE "STP\STPSOR"	 TO PRG-NAME2.
	     PERFORM YA100.
	     MOVE "STP\ORDPRN2"	 TO PRG-NAME2.
	     PERFORM YA100.
       YA99.
	     EXIT.

       YA100	       SECTION 90.
       YA101.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     PERFORM SAVE-SCREEN-3.
	     CALL PRG-NAME2
		  USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY
		  ON EXCEPTION
		      GO TO YA110
	     END-CALL.
	     CANCEL PRG-NAME2.
       YA105.
	     PERFORM RESTORE-SCREEN-3.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     GO TO YA199.
       YA110.
	     MOVE SPACES     TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME2 DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO YA105.

       YA199.
	     EXIT.
      /    *************************************************************
      *    ****  I N I T I A L I Z E   P R O G R A M
      *    *************************************************************
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
042000	     DISPLAY CLR-SCREEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     COPY FUNCTION.PRO.
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
      *    **** D E B T O R   S A L E S   R E S T R I T I O N	F I L E 	  ****
      *    *************************************************************
	     MOVE "DEBDEP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBDEP.
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
      *    **** 	   S T O C K   M E M O	 F I L E	    ****
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
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
	     MOVE PAR-PROMPT	 TO WS-PROMPT.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
	   IF NOT (PAR-OVER-LIM = "N" OR "Y")
	       MOVE "N" 	 TO PAR-OVER-LIM.
	   IF NOT (PAR-ARREARS = "N" OR "Y")
	       MOVE "N" 	 TO PAR-ARREARS.
	   IF NOT (PAR-INV-CHECK = "N" OR "Y")
	       MOVE "N" 	 TO PAR-INV-CHECK
	       MOVE ZERO	 TO PAR-INV-AMT.
	     MOVE PAR-OVER-LIM	 TO WS-OVER-LIM.
	     MOVE PAR-PARCELS	 TO WS-PARCELS.
	     MOVE PAR-ARREARS	 TO WS-ARREARS.
	     MOVE PAR-INV-CHECK  TO WS-INV-CHECK.
	     MOVE PAR-INV-AMT	 TO WS-INV-AMT.
	     MOVE PAR-SPLIT-BC	 TO WS-SPLIT-BC.
	   IF NOT (WS-SPLIT-BC = "N" OR "Y")
	       MOVE "N" 	 TO WS-SPLIT-BC.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-C-FACT	 TO WS-C-FACT.
054270	     OPEN OUTPUT SALE.
054280	     CLOSE SALE.
054290	     OPEN I-O SALE.
	     OPEN INPUT DEBDEP.
042220	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 33	 TO WS-F-ERROR
                   GO TO ZA50.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 33		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
054760	     MOVE 1		 TO WS-PARKEY.
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

       COPY ZA49.PRO.

039850 ZA50.
055350	   IF WS-F-ERROR = 33
055360	       MOVE "DEBDEP file"
055370				 TO WS-FILE
	   END-IF.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

055970 ZA200.

       COPY LOCKED.PRO.

056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	92.

       COPY ERRORS.PRO.

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
041760	   ELSE
041770	   IF WS-F-ERROR = 33
041780	       MOVE W02-DEBDEP	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBD-ACNO	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX
041840	   ELSE
041770     IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE WAR-CODE     TO WS-KEYX
041840     ELSE
041770     IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
