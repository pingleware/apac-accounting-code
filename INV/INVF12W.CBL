      $set LINKCOUNT"812"
      ******************************************************************
      *                                                                *
      * ******** **	** **	 ** ******** **    ******  **	    ** *
      *    **	 ***	** **	 ** **	    ***   **	** **	    ** *
      *    **	 ** *	** **	 ** **	     **        **  **	    ** *
      *    **	 **  *	** **	 ** *****    **      **    **	*   ** *
      *    **	 **   * **  **	**  **	     **    **	   **  ***  ** *
      *    **	 **    ***   ****   **	     **   **	    **** ****  *
      * ******** **	**    **    **	   ****** ********   **   **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * I N V O I C I N G   P R O G R A M   -	( C O N S O L - U S B) *
      * 							       *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Mar 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent     *
      * 		  print API's supplied with MF COBOL. An entry *
      * 		  in the APACFIDS file has been included for   *
      * 		  the system to access the bitmap file that is *
      * 		  used for the invoice and is accessed using   *
      * 		  "INVBMP" as the key.			       *
      * 							       *
      *  Aug 2011	- Fix problem with remarks on an Invoice re-   *
      * 		  print. (Columns were messed up.)	       *
      * 							       *
      *  Jan 2012	- Allow the User to decide if a copy Invoice   *
      * 		  must be printed.							       *
      * 							       *
      ******************************************************************
       IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     INVF12W.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 2010.

	   This program has been included to enable the USER to print on
	   a printer (usualy USB) via windows. The other Invoice programs
	   print directly to the LPT and COM ports. This program now
	   allows for USB, Network or any printers that need the Windows
	   print process. The system will select the default invoice
	   program for the previous printers.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2010 - 2012
                                     by James William Lemmon.
                                       (Id No. 4412165050082).

                   All rights reserved.

                   e-mail jwlemmon@gmail.com.

000050 SECURITY.
                   This program is free software; you can redistribute
                   it and/or modify it under the terms of the GNU General
                   Public License as published by the Free Software
		   Foundation; either version 3 of the License, or (at
                   your option) any later version.

                   This program is distributed in the hope that it will
                   be useful, but WITHOUT ANY WARRANTY; without even the
                   implied warranty of MERCHANTABILITY or FITNESS FOR A
                   PARTICULAR PURPOSE.  See the GNU General Public License
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

000130 COPY AUDIT.SL.

       COPY CARDEX.SL.

000150 COPY CONTROL.SL.

000170 COPY DBTRAN.SL.

000190 COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000250 COPY INVOIC.SL.

       COPY LEDTRF.SL.

000270 COPY PARAM.SL.

000290 COPY RECOVER.SL.

000310 COPY SALE.SL.

000290 COPY SPARTS.SL.

       COPY SHARED.SL.

000330 COPY STOCK.SL.

000350 COPY TXTRAN.SL.

       COPY WSTOCK.SL.

      /
000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000420 COPY AUDIT.FDE.

       COPY CARDEX.FDE.

000440 COPY CONTROL.FDE.

000460 COPY DBTRAN.FDE.

000480 COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000250 COPY GARTEE.FDE.

000540 COPY INVOIC.FDE.

       COPY LEDTRF.FDE.

000560 COPY PARAM.FDE.

000580 COPY RECOVER.GFD.

000600 COPY SALE.FDE.

000520 COPY SPARTS.FDE.

       COPY SHARED.FDE.

000620 COPY STOCK.FDE.

000640 COPY TXTRAN.FDE.

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
002380 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
002390 77  WS-SALKEY       PIC  9(04)    COMP-5.
002400 77  WS-NETKEY       PIC  9(04)    COMP-5.
002410 77  WS-AUDKEY       PIC  9(06)    COMP-5.
002420 77  WS-RECKEY       PIC  9(06)    COMP-5.
002430 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01).
002450 77  WS-GAR	   PIC	9(01) VALUE 0.
002450 77  WS-PRN1         PIC  9(01) VALUE 0.  
002460 77  WS-PRN2         PIC  9(01) VALUE 0.  
       77  WS-NUM          PIC  Z9.
002480 77  WS-START        PIC  9(08).
002490 77  WS-END          PIC  9(08).
002510 77  WS-TME2         PIC  9(08).
002520 77  WS-PORT         PIC  9(05) VALUE 1004.
002530 77  WS-OPEN         PIC  X(01) VALUE X"01".
002540 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002550 77  WS-PAR          PIC  9(01) VALUE 0.
002560 77  WS-INO          PIC  9(02) VALUE 0.
002570 77  WS-LINES	   PIC	9(02) VALUE 0.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002600 77  WS-LASTINV      PIC  9(04)    COMP-5.
002610 77  WS-LINE         PIC  9(04)    COMP-5.
       77  WS-DET-LINES    PIC	9(04)	 COMP-5 VALUE 52.
002620 77  WS-PAGE	   PIC	9(02)	 COMP-5.
002630 77  WS-L1           PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
       77  WS-PRN-TYPE	   PIC	X(01) VALUE " ".
002750 77  WS-OPTION	   PIC	X(01).
002770 77  WS-SKIP         PIC  X(01) VALUE " ".
002780 77  WS-SINGLE	   PIC	X(01) VALUE "S".
002790 77  WS-ETYPE        PIC  X(01) VALUE "I".
002820 77  WS-ERROR        PIC  9(01).
002840 77  WS-CHAR1	   PIC	X(01).
002850 77  WS-CHAR2        PIC  X(01).
002860 77  WS-COMP         PIC  9(01).
002870 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002880 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002890 77  WS-HEAD         PIC  X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE          PIC  X(01).
002920 77  WS-PRC          PIC  X(01).
002920 77  WS-DISCOUNT	   PIC	X(01) VALUE "N".
002920 77  WS-INPUT        PIC  X(01) VALUE "N".
002920 77  WS-COPY         PIC  X(01) VALUE "N".
002920 77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
       77  WS-VAT	   PIC S9(07)V99 COMP-3.
003020 77  WS-PER	   PIC	Z(02)9.99.
003030 77  WS-CR           PIC  X(01).
003040 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
003040 77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
      *    ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
      *    º	   V A T   C A L C U L A T I O N   P R O B L E M       º
      *    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Value Added Tax (VAT) is calculated per item and because  ³
      *    ³ some rates cause the calculation of tax to have more than ³
      *    ³ two decimals, results must be rounded per tax percentage. ³
      *    ³ Some Users require more detailed analysis of their sales  ³
      *    ³ and split their Inventory accross more than one category. ³
      *    ³							       ³
      *    ³ As the integration of sales into the General Ledger from  ³
      *    ³ the sales modules is processed per Sales Ledger, which    ³
      *    ³ requires the tax to be rounded per integration entry and  ³
      *    ³ rounding per sales integration entry which on occasion    ³
      *    ³ will result in the entries for the transaction to be out  ³
      *    ³ of balance.					       ³
      *    ³							       ³
      *    ³ To minimise this problem the system stores the exclusive  ³
      *    ³ price with three decimals. This only serves to minimise   ³
      *    ³ the amount by which the calculation may vary, which when  ³
      *    ³ incorrect is normally 0.01 too much or too little.        ³
      *    ³							       ³
      *    ³ EXAMPLE:						       ³
      *    ³   VAT rate = 14%.					       ³
      *    ³   Item one has an exclusive price of:		14.25  ³
      *    ³		       sales category	 :		0001   ³
      *    ³   This would result in the inclusive price being calcula- ³
      *    ³   ed as:	       14.25 + VAT 1.995 =		16.245 ³
      *    ³   As inclusive prices may only have two decimals the      ³
      *    ³   system will round this price to 16.25, when the item is ³
      *    ³   first loaded or if this was the last price, resulting   ³
      *    ³   in an exclusive price of 			14.254 ³
      *    ³   Item two has an inclusive price of:	       151.90  ³
      *    ³		       sales category	 :		0002   ³
      *    ³   This item will have an exclusive price recorded as      ³
      *    ³   133.246 after being rounded to three decimals.	       ³
      *    ³							       ³
      *    ³   1. Invoice for 1 of item one.			       ³
      *    ³	  Using the inclusive price, calculate the VAT and     ³
      *    ³	  the other values as follows:			       ³
      *    ³	  Inclusive price ö (100 + VAT Rate) x VAT rate        ³
      *    ³		    16.25 ö (100 + 14) x 14  =	1.9956	       ³
      *    ³	  Round VAT to 2 decimals	     =	2.00	       ³
      *    ³	  Subtract VAT from inclusive price  = 14.25 (excl)    ³
      *    ³						  Dt	 Cr    ³
      *    ³	  Integration entries are:  Debtor Cont	 16.25	       ³
      *    ³				    Income		14.25- ³
      *    ³				    VAT			 2.00- ³
      *    ³						______________ ³
      *    ³				    Totals	 16.25	16.25- ³
      *    ³							       ³
      *    ³   2. Invoice for 1 of item one and 1 of item two.	       ³
      *    ³	  VAT is calculated as the total on both items and     ³
      *    ³	  rounded to two decimals: This can be done, by using  ³
      *    ³	  the total inclusive or excluse value. 	       ³
      *    ³	  Exclusive: 14.254 + 133.246 = 147.50 x 14% = 20.65   ³
      *    ³	  Inclusive: 16.25 + 151.90 = 168.15 ö (100 + 14)      ³
      *    ³		     x 14 = 20.65.			       ³
      *    ³	  As the two items are from different catergories, two ³
      *    ³	  integration entries are required.		       ³
      *    ³	  These entries must be rounded to two decimals.       ³
      *    ³		     14.254 rounded =  14.25		       ³
      *    ³		    133.246 rounded = 133.25		       ³
      *    ³						  Dt	 Cr    ³
      *    ³	  Integration entries are:  Debtor Cont 168.15	       ³
      *    ³				    Income 1		14.25- ³
      *    ³				    Income 2	       133.25- ³
      *    ³				    VAT			20.65- ³
      *    ³						______________ ³
      *    ³				    Totals	168.15 168.15- ³
      *    ³							       ³
      *    ³	  From these examples, it is clear that the procedure  ³
      *    ³	  being used is correct. However, if both items had    ³
      *    ³	  either the first or second price the result would be ³
      *    ³	  as follows:					       ³
      *    ³	  1 - price 16.25 inclusive - VAT calculated as 3.99   ³
      *    ³	  Both items would have their integration entries      ³
      *    ³	  rounded to 14.25.				       ³
      *    ³						  Dt	 Cr    ³
      *    ³	  Integration entries are:  Debtor Cont	 32.50	       ³
      *    ³				    Income 1		14.25- ³
      *    ³				    Income 2		14.25- ³
      *    ³				    VAT			 3.99- ³
      *    ³						______________ ³
      *    ³				    Totals	 32.50	32.49- ³
      *    ³							       ³
      *    ³	  2 - price 151.90 inclusive - VAT calculated as 37.31 ³
      *    ³	  Both items would have their integration entries      ³
      *    ³	  rounded to 133.25.				       ³
      *    ³						  Dt	 Cr    ³
      *    ³	  Integration entries are:  Debtor Cont	303.80	       ³
      *    ³				    Income 1	       133.25- ³
      *    ³				    Income 2	       133.25- ³
      *    ³				    VAT			37.31- ³
      *    ³						______________ ³
      *    ³				    Totals	303.80 303.81- ³
      *    ³							       ³
      *    ³ The system can either, if required, process a balancing   ³
      *    ³ entry to a suspense account at the end of each day or     ³
      *    ³ balance each set of integration entries. The suspense     ³
      *    ³ account would need to have manual adjustment entries pro- ³
      *    ³ cessed and as the system already processes balancing      ³
      *    ³ entries if there has been a problem, it would not be a    ³
      *    ³ straight-forward procedure.			       ³
      *    ³							       ³
      *    ³ From the above examples, it would seem that when the cre- ³
      *    ³ dit side is more, then the income should be reduced and   ³
      *    ³ when the debit side is more, the income should be	       ³
      *    ³ increased. 					       ³
      *    ³							       ³
      *    ³ The system will have to choose one of the income entries  ³
      *    ³ randomly when income needs to be adjusted. This will need ³
      *    ³ the details of the first sales (income) integration entry ³
      *    ³ to be stored and add sales (income) values of subsequent  ³
      *    ³ sales entries and VAT (tax) values to this value and on   ³
      *    ³ completing the transaction this balancing value will be   ³
      *    ³ checked and if not zero it will be used as the balancing  ³
      *    ³ entry by changing the sign.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       77  WS-BAL-ENTRY		PIC  X(01) VALUE "N".
	   88  BALANCING-ENTRY-STORED	   VALUE "Y".

       01  WXT-LEDTRF.
	   03  WXT-DATE	      PIC  9(08)    COMP-3.
	   03  WXT-TYPE       PIC  9(02)    COMP-5.
	   03  WXT-LEDG.
	       05  WXT-TRN    PIC  9(02).
	       05  FILLER     PIC  X(02).
	   03  WXT-AC	      PIC  9(06)    COMP-3.
	   03  WXT-NAR.
	       05  WXT-DESC   PIC  X(13).
	       05  WXT-BOOK   PIC  X(27).
	   03  WXT-VALUE      PIC S9(09)V99 COMP-3.
      /
003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
003090	       05  WS-TCR  PIC X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110	       05  FILLER      PIC  X(01).
003120	       05  WS-TOP-COMP PIC  X(40).
003130	       05  FILLER      PIC  X(39).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".

003480 COPY WS.WS.

       COPY LEDTRF.WS.

003520 01  W00-TRAN-CODES.
003530     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
003540     03  W00-E-DESC  PIC  X(12).
003550     03  W00-A-DESC  PIC  X(12).
003560     03  W00-ACTION  PIC  9(01).
003570     03  W00-T-CODE  PIC  9(02)    COMP-5.
003580     03  W00-T-GL    PIC  9(06)    COMP-3.
003590     03  W00-T-DAY   PIC  9(06)    COMP-5.
003600     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
003610     03  W00-T-MTD   PIC  9(06)    COMP-5.
003620     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
003630     03  W00-T-YTD   PIC  9(06)    COMP-5.
003640     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

       COPY CARDEX.ID.

003700 COPY CONTROL.ID.

003720 COPY DBTRAN.ID.

003740 COPY DEBTOR.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY LEDTRF.ID.

003800 COPY INVOIC.ID.

       COPY PARAM.ID.

003820 COPY RECOVER.ID.

003840 COPY SALE.ID.

       COPY SHARED.ID.

002200 COPY SPARTS.ID.

003860 COPY STOCK.ID.

003880 COPY TXTRAN.ID.

       COPY WSTOCK.ID.

       01  W02-PRINT-RECORD.
	   03  FILLER	       PIC  X(132).

       01  W02-PRINT-NAME      REDEFINES W02-PRINT-RECORD.
	   03  INV-NAME        PIC  X(40).
	   03  INV-DADD        REDEFINES INV-NAME.
	     05  INV-VATH.
	       07  INV-PC1     PIC  X(04).
	       07  FILLER      PIC  X(03).
	     05  INV-VATNO.
	       07  FILLER      PIC  X(33).
	   03  FILLER	       PIC  X(06).
	   03  INV-H3	       PIC  X(08).
	   03  INV-DADD2.
	     05  INV-PC2       PIC  X(30).
	   03  FILLER	       PIC  X(04).
	   03  INV-H4	       PIC  X(19).
	   03  INV-ORD.
	     05  INV-TERMS.
	       07  FILLER      PIC  X(05).
	       07  INV-AC.
		 09  FILLER    PIC  X(03).
		 09  INV-PGE   PIC  ZZ9.
	     05  FILLER        PIC  X(01).
	   03  FILLER	       PIC  X(01).
	   03  FILLER	       PIC  X(12).
      *
       01  W02-PRINT-ITEM      REDEFINES W02-PRINT-RECORD.

	   03  INV-H5.
      *			       (20)
	     05  INV-ITM-MES.
	       07  FILLER      PIC  X(01).
001780	       07  INV-EXT-ITM.
		 09  INV-ITM   PIC  X(14).
		 09  INV-SL    PIC  X(01).
		 09  INV-EXT   PIC  X(03).
	       07  FILLER      PIC  X(01).

	   03  INV-H6.
      *			       (32)
	     05  FILLER        PIC  X(01).
	     05  INV-DESC      PIC  X(30).
	     05  FILLER        PIC  X(01).

	   03  INV-H7.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-QNT       PIC  Z(06)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H8.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-PRICE     PIC  Z(06)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H9.
      * 		       (10)
	     05  FILLER        PIC  X(02).
	     05  INV-DSC       PIC  Z9.99.
	     05  FILLER        PIC  X(03).

	   03  INV-H10.
      *			       (15)
	     05  FILLER        PIC  X(01).
	     05  INV-EXTEND    PIC  Z(08)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H11.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-TAX       PIC  Z(06)9.99- BLANK WHEN ZERO.
	     05  FILLER        PIC  X(01).

	   03  INV-H12.
      *			       (16)
	     05  FILLER        PIC  X(01).
	     05  INV-VAL       PIC  Z(09)9.99-.
	     05  FILLER        PIC  X(01).
      *
       01  INV-L5 REDEFINES W02-PRINT-RECORD.
	   03  FILLER	       PIC  X(11).
	   03  INV-RMK	       PIC  X(40).
	   03  FILLER	       PIC  X(81).
      *
       01  INV-L6 REDEFINES W02-PRINT-RECORD.
	   03  FILLER	       PIC  X(09).
	   03  INV-HD	       PIC  X(10).
	   03  INV-SERNOS.
	     05  FILLER        PIC  X(01).
	     05  INV-SERNO     PIC  X(10).
	     05  FILLER        PIC  X(25).
	   03  FILLER	       PIC  X(77).
      *
       01  INV-L7 REDEFINES W02-PRINT-RECORD.
	   03  FILLER	       PIC  X(08).
	   03  INV-REMKS.
	     05  FILLER        PIC  X(10).
	     05  INV-RMKS.
	       07  FILLER      PIC  X(40).
	     05  FILLER        PIC  X(13).
	   03  FILLER	       PIC  X(04).
	   03  INV-H13	       PIC  X(12).
	   03  INV-LINE        PIC  X(45).
      *
       01  INV-L8 REDEFINES W02-PRINT-RECORD.
	   03  INV-H14A	       PIC  X(08).
	   03  INV-DTE	       PIC  Z9/99/9999.
	   03  FILLER	       PIC  X(01).
	   03  INV-INVOICE.
	     05  FILLER        PIC  X(02).
	     05  INV-CRNOTE.
	       07  FILLER      PIC  X(06).
	       07  INV-COPY    PIC  X(12).
	       07  FILLER      PIC  X(06).
	     05  FILLER        PIC  X(03).
	   03  INV-H14	       PIC  X(08).
	   03  INV-REF	       PIC  X(10).

       COPY W05.VAT.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

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

004800 01  W30-MESSAGE.
004810	   03  W30-PERC        PIC  Z9.99.
004820     03  FILLER          PIC  X(40) VALUE 
004830         "% Settlement discount if paid in 30 DAYS".

       01  W30-MESSAGE-2.
	   03  W30-RMKS15 OCCURS 6
			       PIC  X(63).

       COPY W40.DBT.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

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

005810 COPY W70DEBT.IWS.

005830 01  W75-KEYS.
005840	   03  W75-S	      PIC  9(02)    COMP-5.
005850	   03  W75-S1	      PIC  9(02)    COMP-5.
	   03  W75-S2	      PIC  9(02)    COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860	   03  W75-DNO	      PIC  X(06) OCCURS 18.

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
006050	   03  W80-ACNO 	 PIC  X(06).
006060     03  W80-CRED REDEFINES W80-ACNO.
006070	       05  W80-CNO	 PIC  9(05).
006080         05  W80-CAC REDEFINES W80-CNO.
006090		   07  W80-CTYPE PIC  9(01).
006100		   07  W80-SER	 PIC  9(04).
006110	       05  FILLER	 PIC  9(01).
006120	   03  W80-TYPE 	 PIC  9(01).

       COPY W90.DBT.

       COPY W95.STM.

007010 01  W100-EDIT.
007020     03  W100-LBAL       PIC Z(06)9.99-.
007030     03  W100-LARR       PIC Z(06)9.99.
007040     03  W100-BAL        PIC Z(08)9.99-.
007050     03  W100-CUR        PIC Z(08)9.99-.
007060     03  W100-30         PIC Z(08)9.99-.
007070     03  W100-60         PIC Z(08)9.99-.
007080     03  W100-90         PIC Z(08)9.99-.
007090     03  W100-120        PIC Z(08)9.99-.
007100     03  W100-INT        PIC Z(08)9.99-.
007110     03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
007120     03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140     03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170     03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

      *  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *  ³  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  ³
      *  ³  USE STANDARD RAW CODE FOR FORMATING 		     ³
      *  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      ******************************************************************
      * KEY							       *
      * 							       *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):	       *
      * 							       *
      * SYNTAX:     Shows the CALL statement you could use to call     *
      * 	    the	routine.				       *
      * 							       *
      * 	    The optional RETURNING clause is also shown. Every *
      * 	    routine returns a value showing the result of the  *
      * 	    operation. Unless otherwise indicated, zero        *
      * 	    indicates success, nonzero indicates failure. This *
      * 	    value is left in the data item specified in the    *
      * 	    RETURNING clause, in this reference, status-code.  *
      * 	    If this clause is omitted, the value is left in    *
      * 	    the special register RETURN-CODE. (If call-        *
      * 	    convention bit two is set, RETURN-CODE is not      *
      * 	    changed.)					       *
      * 							       *
      * 	    status-code must be a numeric data item capable of *
      * 			holding positive values from 0 to      *
      * 			65535; for example, PIC X(2) COMP-5.   *
      * 							       *
      * 	    The name of the routine must be coded in upper     *
      * 	    case.					       *
      * 							       *
      * PARAMETERS: Describes any parameters shown in the RETURNING    *
      * 	    and USING clause. A parameter enclosed in brackets,*
      * 	    for example, [parameter1] is optional and might    *
      * 	    not be needed for all forms of the routine.        *
      * 							       *
      * ON ENTRY:   Indicates which of the parameters shown are passed *
      * 	    on entry.					       *
      * 							       *
      * ON EXIT:    Indicates which of the parameters shown are        *
      * 	    returned on exit.				       *
      * 							       *
      * 	    Where bits of one or more bytes are referenced,    *
      * 	    bit 0 is the least significant (rightmost) bit.    *
      * 							       *
      * COMMENTS:   Provides any additional information necessary for  *
      * 	    the successful use of the routine.		       *
      *								       *
      *    PC_PRINT_FILE	Print a file			       *
      *    PC_PRINTER_CLOSE	Close a printer channel 	       *
      *    PC_PRINTER_CONTROL	Send a printer command to a printer    *
      *    PC_PRINTER_FREE_BMP	Free bitmap from memory 	       *
      *    PC_PRINTER_INFO	Get printer information 	       *
      *    PC_PRINTER_LOAD_BMP	Load bitmap into memory 	       *
      *    PC_PRINTER_OPEN	Open a printer channel		       *
      *    PC_PRINTER_SET_COLOR	Set printer color		       *
      *    PC_PRINTER_SET_FONT	Set printer font		       *
      *    PC_PRINTER_WRITE	Write text to a printer 	       *
      *    PC_PRINTER_WRITE_BMP	Write bitmap to a printer	       *
      * 							       *
      *================================================================*
      * 							       *
      *    PC_WIN_CLOSE_PRINTER	   Close a printer channel	       *
      *    PC_WIN_FREE_PBMP	   Free bitmap from memory	       *
      *    PC_WIN_LOAD_PBMP	   Load bitmap into memory	       *
      *    PC_WIN_OPEN_PRINTER	   Open a printer channel	       *
      *    PC_WIN_OPEN_PRINTER_EXT Extended open a printer channel     *
      *    PC_WIN_PRINT_FILE	   Print a file 		       *
      *    PC_WIN_PRINT_FILE_EXT   Extended print a file	       *
      *    PC_WIN_PRINTER_CONTROL  Send a printer control	       *
      *    PC_WIN_PRINTER_INFO	   Get printer information	       *
      *    PC_WIN_SET_PCOLOR	   Set printer color		       *
      *    PC_WIN_SET_PDEFAULT	   Set or reset default printer        *
      *    PC_WIN_SET_PFONT	   Set printer font		       *
      *    PC_WIN_WRITE_PBMP	   Write bitmap to a printer	       *
      *    PC_WIN_WRITE_PRINTER	   Write to a printer		       *
      * 							       *
      * ______________________________________________________________ *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      Parameters for PC_PRINTER_DEFAULT_NAME	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
	   03  Default-Printer-Name-Len    pic x(02) comp-5.
	   03  Default-Printer-Name-Text   pic x(80).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      Parameters for PC_PRINTER_DEFAULT_FONT	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Font-Family-Name.
	   03  Default-Font-Name-Len	   pic x(02) comp-5.
	   03  Default-Font-Name-Text	   pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Parameters for PC_PRINTER_DEFAULT_PROPERTIES       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Properties-Flags      pic x(4) comp-5.
       01  Default-Properties.
	   03 pr-len	       pic  x(2) comp-5.
	   03 pr-papersize     pic s9(4) comp-5.
	   03 pr-paperlength   pic s9(4) comp-5.
	   03 pr-paperwidth    pic s9(4) comp-5.
	   03 pr-scale	       pic s9(4) comp-5.
	   03 pr-copies        pic s9(4) comp-5.
	   03 pr-papertray     pic s9(4) comp-5.
	   03 pr-printquality  pic s9(4) comp-5.
	   03 pr-color	       pic s9(4) comp-5.
	   03 pr-duplex        pic s9(4) comp-5.
	   03 pr-orientation   pic s9(4) comp-5.
	   03 pr-yresolution   pic s9(4) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_OPEN	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Printer-Handle     pic x(04) comp-5.
       01  Document-Title.
	   03  Title-Len      pic x(02) comp-5.
	   03  Title-Text     pic x(80).
       01  Printer-Flags      pic x(04) comp-5.
       01  Window-Handle      pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_WRITE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  Print-Buffer       pic x(136).
       01  Print-Buffer-Len   pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Parameters for PC_PRINTER_LOAD_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
       01  BmpFilename	      pic x(64).
       01  BmpID	      pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_WRITE_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)
       01  WriteBmpReserved   pic x(04) comp-5 value 0.
       01  BmpRow	      pic x(04) comp-5 value 1.
       01  BmpCol	      pic x(04) comp-5 value 6.
       01  BmpWidth	      pic x(04) comp-5 value 82.
       01  BmpHeigth	      pic x(04) comp-5 value 13.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_WRITE_BMP	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Parameters for PC_PRINTER_SET_COLOR	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  fore-or-back       pic x(2) comp-5.
       01  color-red	      pic x(2) comp-5.
       01  color-green	      pic x(2) comp-5.
       01  color-blue	      pic x(2) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Parameters for PC_PRINTER_CONTROL	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
      * ---print-command:  1 - Abort printing and close printer
      * 		   2 - Throw a page
      * 		   3 - Flush the print buffers
      * 		   4 - Start a new line
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_CLOSE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    Parameters for PC_PRINT_FILE	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Filename.
	   03  Filename-Len		       pic xx comp-5.
	   03  Filename-Text		       pic x(80).

       01  DocumentTitle.
	   03  DocumentTitle-Len	       pic xx comp-5.
	   03  DocumentTitle-Text	       pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Parameters for PC_PRINTER_INFO	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       78 PrinterNameBufferBegin	value next.
       01 PrinterNameBuffer         pic x(255).
       78 PrinterNameBufferLength       value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin        value next.
       01 PrinterTypeBuffer         pic x(255).
       78 PrinterTypeBufferLength       value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin      value next.
       01 PrinterDeviceBuffer       pic x(255).
       78 PrinterDeviceBufferLength     value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin    value next.
       01 PrinterLocationBuffer     pic x(255).
       78 PrinterLocationBufferLength   value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin     value next.
       01 PrinterCommentBuffer      pic x(255).
       78 PrinterCommentBufferLength    value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin   value next.
       01 PrinterPaperSizeBuffer    pic x(255).
       78 PrinterPaperSizeBufferLength  value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
	   03  PrinterInformationLength  pic xxxx comp-5.
	   03  DeviceContextHandle	 pic xxxx comp-5.
	   03  reserved 		 pic xxxx comp-5.
	   03  Orientation		 pic xxxx comp-5.
	   03  PageRowsCurrentFont	 pic xxxx comp-5.
	   03  PageColsCurrentFont	 pic xxxx comp-5.
	   03  RowsRemainingCurrentFont  pic xxxx comp-5.
	   03  MaxGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MaxGraphicsVerticalAxis	 pic xxxx comp-5.
	   03  MinGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MinGraphicsVerticalAxis	 pic xxxx comp-5.
      *
      *    -------  Horizontal
      *
	   03  CurrentCooridinateX	 pic xxxx comp-5.
      *
      *    -------  Vertical
      *
	   03  CurrentCooridinateY	 pic xxxx comp-5.
	   03  NumberOfCopies		 pic xx   comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
	   03  PrintQuality		 pic xx   comp-5.
      *
      *    -------  Monochrome or Color
      *
	   03  Color			 pic x	  comp-5.
	   03  reserved 		 pic x	  comp-5.
	   03  DriverVersionNumber	 pic xx   comp-5.
	   03  PrinterName.
	       05  PrinterNameLength	     pic xxxx comp-5
                   value PrinterNameBufferLength.
	       05  p-PrinterNameBuffer			   pointer.
	   03  PrinterType.
	       05  PrinterTypeLength	     pic xxxx comp-5
                   value PrinterTypeBufferLength.
	       05  p-PrinterTypeBuffer			   pointer.
	   03  PrinterDevice.
	       05 PrinterDeviceLength	    pic xxxx comp-5
                  value PrinterDeviceBufferLength.
	       05 p-PrinterDeviceBuffer 		  pointer.
	   03  PrinterLocation.
	       05 PrinterLocationLength     pic xxxx comp-5
                  value PrinterLocationBufferLength.
	       05 p-PrinterLocationBuffer		  pointer.
	   03  PrinterComment.
	       05 PrinterCommentLength	    pic xxxx comp-5
                  value PrinterCommentBufferLength.
	       05 p-PrinterCommentBuffer		  pointer.
	   03  PrinterPaperSize.
	       05 PrinterPaperSizeLength    pic xxxx comp-5
                 value PrinterPaperSizeBufferLength.
	       05 p-PrinterPaperSizeBuffer		  pointer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Used to display error messages for the Print Routines.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Operation			   pic x(30).

       01   printer-control.

	   03  Default-Printer-Name.
	       05  Default-Printer-Name-Len  pic x(02) comp-5.
	       05  Default-Printer-Name-Text pic x(80).

	   03  Font-family.
	       05  Font-family-namelen	pic x(2) comp-5 value 80.
	       05  Font-family-name	pic x(80).

	   03  FontTypeFace    pic x(13) value "Courier New" & x"00".
	   03  font-style      pic x(4) comp-5 value 0.
	   03  font-size       pic x(4) comp-5 value 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 5 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-5-CPI      pic x(4) comp-5 value 24.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 8 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-8-CPI      pic x(4) comp-5 value 14.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 12 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-12-CPI     pic x(4) comp-5 value 10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 15 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-15-CPI     pic x(4) comp-5 value 8.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 16 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-16-CPI     pic x(4) comp-5 value 7.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Set to print ñ 19 characters per inch	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   03  font-19-CPI     pic x(4) comp-5 value 5.

           03  abort           pic x(4) comp-5 value 1.
           03  prn-control     pic x(4) comp-5 value 2.
	   03  prn-flags       pic x(4) comp-5 value 1.
           03  prn-command     pic x(4) comp-5 value 4.
	   03  prn-handle      pic x(4) comp-5.

	   03  W02-PRINTER.
               05  W02-PRN     PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS      PIC  X(02).
               05  W02-US      PIC  X(01).
               05  W02-TERM    PIC  X(03).
               05  W02-US2     PIC  X(01).
               05  W02-REP     PIC  X(01).
               05  W02-P       PIC  X(01).
               05  W02-USER    PIC  X(03).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY ERROR.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

      /
010220 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY
				W40-COMPANY
				W90-TRAN.
010230 AA000-MAIN      SECTION.
010240 AA000-INIT.
010250       PERFORM ZA000-INIT.

       AA00-LOOP.
	     MOVE ZERO		 TO WS-PAGE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Must an Invoice heading be printed?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W40-ACNO = "newinv"
	       PERFORM BB050
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Must an Invoice be reprinted?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W40-REPRINT = "Y"
	       PERFORM BE000
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Generate a new Invoice.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
016330	       PERFORM BC000
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For a new Invoice a copy is required for inhouse filing.  ³
      *    ³ As these printers are single sheet printers the program   ³
      *    ³ will perform BE000 section to print a copy of the invoice.³
      *    ³ Allow the USER to decide whether a copy Invoice must be   ³
      *    ³ printed or not.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "Print a COPY Invoice (Y/N) [ ]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE
		     TEST AFTER UNTIL WS-OPTION = "N" OR "Y".
	   IF WS-OPTION = "Y"
	       MOVE 0		 TO WS-PAGE
	       PERFORM BE000.
010410	     GO TO AZ000-EOJ.

010930 COPY LOCKED.RC2.

010970 COPY DEBTOR.TRN.

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

      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011130 COPY AUDIT.RD.

011150 COPY CONTROL.RD.

011170 COPY DBTRAN.RD1.

011190 COPY DEBTOR.RDI.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

       COPY LEDTRF.RD.

011270 COPY PARAM.RD.

011290 COPY SALE.RD.

       COPY SHARED.RD.

011810 COPY SPARTS.RD.

011310 COPY STOCK.RD.

011330 COPY TXTRAN.RD.

       COPY WSTOCK.RD.

      /
011350 AD000-WRITE             SECTION.

011370 COPY AUDIT.WR.

       COPY CARDEX.WR.

011390 COPY CONTROL.WR.

011410 COPY DBTRAN.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011490 COPY INVOIC.WR.

       COPY LEDTRF.WR.

011510 COPY PARAM.WR.

       COPY SHARED.WR.

011550 COPY STOCK.WR.

011570 COPY TXTRAN.WR.

       COPY WSTOCK.WR.

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
011670 AY000	       SECTION.
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
011740 AY02.
011750       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011770       PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011780       MOVE DEB-RECORD1    TO REC-DEBTOR.
011790       GO TO AY50.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
011800 AY04.
011810       MOVE 04             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011830       MOVE TRA-RECORD1    TO REC-DBTRAN.
011840       GO TO AY50.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
      *    *************************************************************
      *    **** 	    G U A R A N T E E	F I L E		    ****
      *    *************************************************************
011850 AY09.
011860       MOVE 09             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE WS-ACTION      TO REC-TYPE.
011900       MOVE GUA-RECORD1    TO REC-GARTEE.
011910       GO TO AY50.
      *    *************************************************************
      *    **** 	D A I L Y   -	A U D I T   F I L E	    ****
      *    *************************************************************
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011880       MOVE WS-ACTION      TO REC-TYPE.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
      *    *************************************************************
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
012030 AY22.
012040       MOVE 22             TO REC-FILE.
012050       MOVE ZERO           TO REC-KEY.
012060       MOVE WS-ACTION      TO REC-TYPE.
012070       MOVE DOC-REC1       TO REC-INVOIC.
012080       GO TO AY50.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
012090 AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
012140       GO TO AY50.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
      *    *************************************************************
      *    ****       W A R E H O U S E	  S T O C K   F I L E	    ****
      *    *************************************************************
010970 AY052.
010980       MOVE 52             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
011000       MOVE WST-RECORD1    TO REC-WSTOCK.
011010       GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  End transaction in recovery log	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012200 AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 195 - Clear (Close and Open    ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012260	   IF WS-RECOVER > 195
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390         DISPLAY "Write error Recovery file - Status " AT 2212
012400			WITH FOREGROUND-COLOR 6 HIGHLIGHT
012410			WS-STATUS WITH FOREGROUND-COLOR 7 HIGHLIGHT
012420         STOP RUN.
012430	     CLOSE RECOVER.
012440	     OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.

012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   Start processing transaction 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Read PARAM record 4 - Lock record and start recovery   ³
      *    ³	file procedure. 				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Q   F U L L   -	 W A I T   F O R   4   S E C O N D S   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Read the DEBTOR control record in the NETWORK file    ³
      *    ³	 and lock it. Log details to the RECOVERY file.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   A R E   A N Y   U P D A T E S   I N   P R O G R E S S   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  A R E   D E B T O R	O R   S T O C K   F I L E S    ³
      *    ³	  B E I N G   U P D A T E D			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     Y E S   -	 S E T	 W A I T   P E R I O D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  I S	T H I S   P R O G R A M   I N	T H E  Q       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF PAR-USR(WS-SUB) = LS-USER
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ I S   I T	 N E X T   I N	 L I N E   T O	 P R O C E S S ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   S E T   W A I T   P E R I O D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   Write links back and unlock PARAM record 4 and      ³
      *    ³	   NETWORK record 1.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
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
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013100 AY999.
013110       EXIT.
      /
013120 AZ000-END               SECTION.
013130 AZ000-EOJ.
	     perform close-down-printer.
025520	     DISPLAY W90-REF AT 0510
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 0.
013160	     CLOSE RECOVER.

013280 AZ100.
013290       EXIT PROGRAM.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ The following routines are used for printing via the WINDOWS ³
      * ³ printing API's and are used for LASER and INK-JET printers   ³
      * ³ that do not allow for embedded control characters.	       ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	PRINT-DETAILS  SECTION.
	     perform print-line.

      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³  Check if paper must be advanced? (WS-ADVANCE will be > 0)   ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	   if WS-ADVANCE > 0
	       perform advance-printer WS-ADVANCE times.
      * ______________________________________________________________ *
      * PC_PRINTER_WRITE					       *
      * 							       *
      * Writes a text string to the printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE" using	 printer-handle        *
      * 					 print-buffer	       *
      * 			       by value  print-buff-len        *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-buffer	pic x(n).			       *
      * 	print-buff-len	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	printer-buffer	The buffer from which the bytes are to *
      * 			be printed			       *
      * 	printer-buff-len  The number of bytes to print	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
       print-line	  section.
             call "PC_PRINTER_WRITE" using    prn-handle
					      Print-Buffer
				     by value Print-Buffer-Len
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT					       *
      * 							       *
      * Sets the font on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_FONT" using     printer-handle    *
      * 					     font-family-name  *
      * 				   by value  font-size	       *
      * 				   by value  font-style        *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	font-family-name	Group item defined as:	       *
      * 	    name-len	pic x(2) comp-5.		       *
      * 	    name-text	pic x().			       *
      * 	font-size	Numeric literal or pic x(4) comp-5.    *
      * 	font-style	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	name-len	The length of the font family name.    *
      * 	name-text	The family name of the font to be used,*
      * 			such as Courier, Times and Symbol      *
      * 	font-size	Point size of the font		       *
      * 	font-style	Set of bits defining the required font *
      * 			style:				       *
      * 	  bit 3	 Bold					       *
      * 	  bit 2	 Strikeout				       *
      * 	  bit 1	 Underline				       *
      * 	  bit 0	 Italic 				       *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	set-printer-font   section.
	     call "PC_PRINTER_SET_FONT"
				     using prn-handle
					   font-family
				     by value font-size
				     by value font-style
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CONTROL					       *
      * 							       *
      * Sends a print command to a printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CONTROL" using      printer-handle    *
      * 				  by value   print-command     *
      * 				  returning  status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-command	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	print-command	The command to send to the printer:    *
      * 							       *
      * 	  1   Abort printing the document and close the        *
      * 	      printer					       *
      * 	  2   Throw a page				       *
      * 	  3   Flush the print buffers			       *
      * 	  4   Advance one line				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      *		None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³	     A D V A N C E   P A P E R	 1   L I N E	       ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	advance-printer    section.
	     move 4		 to prn-command.
             call "PC_PRINTER_CONTROL" using    prn-handle
                                       by value prn-command
	     end-call.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³	     A D V A N C E   P A P E R	 1   P A G E	       ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	Throw-a-page	   section.
	     move 2		 to prn-command.
	     call "PC_PRINTER_CONTROL" using	prn-handle
                                       by value prn-command
	     end-call.
      * ______________________________________________________________ *
      * PC_PRINTER_SET_COLOR					       *
      * 							       *
      * Sets the color on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_COLOR" using     printer-handle   *
      * 				    by value  fore-or-back     *
      * 				    by value  color-red        *
      * 				    by value  color-green      *
      * 				    by value  color-blue       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	fore-or-back	pic x(2) comp-5.		       *
      * 	color-red	pic x(2) comp-5.		       *
      * 	color-green	pic x(2) comp-5.		       *
      * 	color-blue	pic x(2) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle The printer handle returned when the    *
      * 		       printer was opened.		       *
      * 	fore-or-back   Whether to set foreground or background:*
      * 	    1	       foreground			       *
      * 	    2	       background			       *
      * 	color-red      Value between 0 and 255; intensity of   *
      * 						red	       *
      * 	color-green    Value between 0 and 255; intensity of   *
      * 						green	       *
      * 	color-blue     Value between 0 and 255; intensity of   *
      * 						blue	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You must open the printer before using this routine.   *
      * 							       *
      * 	If your printer does not support color, it is up to    *
      * 	the printer device driver to interpret these values.   *
      * 	For example, some perform gray-shading; others choose  *
      * 	either black or white for each color.		       *
      * 							       *
      * 	You specify the color you want by combinations of      *
      * 	values showing the intensity of the three primary      *
      * 	colors. For example, to get the colors in the	       *
      * 	following table, you set the RGB values as shown:      *
      * 							       *
      * 	      Color	   Red	   Green   Blue 	       *
      * 	      -----	   ---	   -----   ---- 	       *
      * 	      Red	   255	   0	   0		       *
      * 	      Yellow	   255	   255	   0		       *
      * 	      Green	   0	   255	   0		       *
      * 	      Cyan	   0	   255	   255		       *
      * 	      Blue	   0	   0	   255		       *
      * 	      Magenta	   255	   0	   255		       *
      * 	      White	   255	   255	   255		       *
      * 	      Black	   0	   0	   0		       *
      * 							       *
      * 	    Some other, more unusual, colors are:	       *
      * 							       *
      * 	      Dark orange  255	   140	   0		       *
      * 	      Navy blue	   0	   0	   128		       *
      * 	      Violet	   238	   120	   238		       *
      * 	      Beige	   245	   245	   220		       *
      * 							       *
      * ______________________________________________________________ *
	printer-colour	  section.
	     call "PC_PRINTER_SET_COLOR" using	  prn-handle
					 by value fore-or-back
					 by value color-red
					 by value color-green
					 by value color-blue
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE					       *
      * 							       *
      * Closes a previously opened printer channel.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CLOSE" using printer-handle	       *
      * 				returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 							       *
      * ON EXIT:						       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	close-down-printer section.
	     call "PC_PRINTER_CLOSE" using by reference prn-handle
	     end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_LOAD_BMP					       *
      * 							       *
      * Loads a bitmap into memory ready for the PC_PRINTER_WRITE_BMP  *
      * function.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_LOAD_BMP" using	printer-handle *
      * 				   by reference bmpfilename    *
      * 				   by reference bmp-id	       *
      * 				   returning	status-code    *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmpfilename	pic x(n).			       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmpfilename	The name of the file to be printed     *
      * 			(null or space terminated).	       *
      * 							       *
      * ON EXIT:						       *
      * 	bmp-id		A unique identifier for the loaded     *
      * 			bitmap. 			       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    3		Printer device not open 	       *
      * 	   18		Failed to load bitmap		       *
      * 	   19		Invalid bitmap id given 	       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      *     call "PC_PRINTER_LOAD_BMP" using	    printer-handle     *
      * 			       by reference "mflogo.bmp" & x"0"*
      * 			       by reference bmp-id	       *
      * 			       returning    printer-retcode    *
      *     end-call						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
       print-header-BMP    section.

	     call "PC_PRINTER_LOAD_BMP"	using prn-handle
					by reference BmpFilename
					by reference BmpID
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_LOAD_BMP" to Operation
               perform 9999-Display-Error
	   end-if.

      * ______________________________________________________________ *
      * PC_PRINTER_WRITE_BMP					       *
      * 							       *
      * Write a loaded bitmap to a printer at a specified position with*
      * a given size.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE_BMP" using     printer-handle   *
      * 				    by value  bmp-id	       *
      * 				    by value  reserved	       *
      * 				    by value  bmp-row	       *
      * 				    by value  bmp-col	       *
      * 				    by value  bmp-width        *
      * 				    by value  bmp-height       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	reserved	pic x(4) comp-5 value 0.	       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	bmp-row 	pic x(4) comp-5.		       *
      * 	bmp-col 	pic x(4) comp-5.		       *
      * 	bmp-width	pic x(4) comp-5.		       *
      * 	bmp-height	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmp-id	The unique identifier of the bitmap to print   *
      * 	bmp-row 					       *
      * 	bmp-col The position of the bitmap to print.	       *
      * 	bmp-width					       *
      * 	bmp-height  The size of the bitmap to print.	       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code  Printer status code:		       *
      * 	    0	     Successful 			       *
      * 	    3	     Printer device not open		       *
      * 	    4	     Out of memory while printing	       *
      * 	    5	     Disk full while spooling file	       *
      * 	    7	     Print job aborted, no file spooled to     *
      * 		     Print Manager			       *
      * 	   11	     Write failure			       *
      * 	   21	     Failed to print bitmap		       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	This routine works with PostScript and HP PCL printers.*
      * 	It is not supported on HP Deskjet or dot matrix        *
      * 	printers.					       *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 							       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_WRITE_BMP" using prn-handle
					 by value BmpID
					 by value WriteBmpReserved
					 by value BmpRow
					 by value BmpCol
					 by value BmpWidth
					 by value BmpHeigth
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_WRITE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.

      * ______________________________________________________________ *
      * PC_PRINTER_FREE_BMP					       *
      * 							       *
      * Frees a bitmap from memory, releasing its image and palette    *
      * back to the operating system.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using     printer-handle    *
      * 				   by value  bmp-id	       *
      * 				   returning status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmp-id	pic x(4) comp-5.			       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      *				printer was opened.		       *
      * 	bmp-id	The unique id of the loaded bitmap, returned   *
      * 		when the bitmap was loaded.		       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code Printer status code:		       *
      * 	    0	    Successful				       *
      * 	    3	    Printer device not open		       *
      * 	   20	    Failed to free bitmap		       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using printer-handle,       *
      * 				   by value  bmp-idlogo,       *
      * 				   returning printer-retcode   *
      * 	end-call					       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_FREE_BMP" using	prn-handle
					by value  BmpID

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.
      * ______________________________________________________________ *
      * PC_WIN_SET_PDEFAULT					       *
      * 							       *
      * Set the default printer for all PC_WIN printer routines.       *
      * 							       *
      * SYNTAX:     call "PC_WIN_SET_PDEFAULT" using	deflt-printer  *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS: deflt-printer  pic x(n).			       *
      * 	    status-code	   See Key in the Preface	       *
      * 							       *
      * ON ENTRY:   deflt-printer A null-terminated string specifying  *
      * 			  the default printer. If no name is   *
      * 			  specified (that is, the first        *
      * 			  character of the string is null)     *
      * 			  then the default printer is reset.   *
      * 							       *
      * ON EXIT:    status-code Printer status code:		       *
      * 							       *
      * 		0	Successful			       *
      * 	       17	Failed to find default printer	       *
      * 							       *
      * EXAMPLE:    call "PC_WIN_SET_PDEFAULT" using		       *
      * 			    "Local PostScript Printer" & x"00" *
      * 			     returning printer-retcode	       *
      * 	    end-call					       *
      * ______________________________________________________________ *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	   ³ Background colour is used to show columns, to format the  ³
      *    ³ document as the ASCII box characters can not be used with ³
      *    ³ the windows print utility. This helps with readability    ³
      *    ³ and gives the document a more professional look.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       PRINT-ITEM      SECTION.
	     move 2		 to fore-or-back.
	     move 0		 to WS-S1.
	     move 245		 to color-red.
	     move 245		 to color-green.
	     move 220		 to color-blue.
	     perform printer-colour.
	     move INV-H5	 to Print-Buffer.
	     move 20		 to Print-Buffer-Len.
	     move 0		 to WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
      *	   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move 255		 to color-red
	     move 255		 to color-green
	     move 255		 to color-blue
	     perform printer-colour
	     MOVE INV-H6	 to Print-Buffer
	     MOVE 32		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
	     move 245		 to color-red.
	     move 245		 to color-green.
	     move 220		 to color-blue.
	     perform printer-colour
	     MOVE INV-H7	 to Print-Buffer
	     MOVE 13		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move 255		 to color-red
	     move 255		 to color-green
	     move 255		 to color-blue
	     perform printer-colour
	     MOVE INV-H8	 to Print-Buffer
	     MOVE 13		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
	     move 245		 to color-red.
	     move 245		 to color-green.
	     move 220		 to color-blue.
	     perform printer-colour
	     MOVE INV-H9	 to Print-Buffer
	     MOVE 10		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move 255		 to color-red
	     move 255		 to color-green
	     move 255		 to color-blue
	     perform printer-colour
	     MOVE INV-H10	 to Print-Buffer
	     MOVE 15		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
	     move 245		 to color-red.
	     move 245		 to color-green.
	     move 220		 to color-blue.
	     perform printer-colour
	     MOVE INV-H11	 to Print-Buffer
	     MOVE 13		 to Print-Buffer-Len
	     PERFORM PRINT-DETAILS.
	     move 245		 to color-red
	     move 220		 to color-green
	     move 245		 to color-blue
	     perform printer-colour
	     MOVE INV-H12	 to Print-Buffer
	     MOVE 16		 to Print-Buffer-Len
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			 GL-TRF-UPDATE			       ³
      *	   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ GENERAL LEDGER:					       ³
      *    ³ This routine is used to UPDATE the INTEGRATION RECORDS    ³
      *    ³ for posting to the G/L ACCOUNTS.			       ³
      *    ³							       ³
      *    ³ THE KEY FOR EACH OF THE RECORDS IS:		       ³
      *    ³							       ³
      *    ³	RUN DATE (CCYYMMDD)  -	WX-YMD			       ³
      *    ³							       ³
      *    ³	TYPE -	WX-TYPE 				       ³
      *    ³		01 : DEBTORS				       ³
      *    ³		02 : CREDITORS				       ³
      *    ³		03 : BAD DEBTS				       ³
      *    ³		04 : SALES				       ³
      *    ³		05 : SALES (RETURNS)			       ³
      *    ³		06 : PURCHASES				       ³
      *    ³		07 : PURCHASES (RETURNS)		       ³
      *    ³		08 : COST-OF-SALES			       ³
      *    ³		09 : COST-OF-SALES (RETURNS)		       ³
      *    ³		10 : OUTPUT VAT (DEBTORS)		       ³
      *    ³		11 : INPUT VAT (DEBTORS)		       ³
      *    ³		12 : OUTPUT VAT (CREDITORS)		       ³
      *    ³		13 : INPUT VAT (CREDITORS)		       ³
      *    ³		14 : DEBTOR TRANSACTIONS		       ³
      *    ³							       ³
      *    ³	TRANSACTION CODE / SALES LEDGER NO - WX-LEDG	       ³
      *    ³							       ³
      *    ³ OTHER DETAILS ARE: 				       ³
      *    ³							       ³
      *    ³	ACCOUNT NUMBER -  WX-AC				       ³
      *    ³							       ³
      *    ³	NARRATIVE - WX-NAR				       ³
      *    ³							       ³
      *    ³	TRANSACTION VALUE - WX-VAL			       ³
      *    ³							       ³
      *    ³ The routine will first read the file using the DETAILS    ³
      *    ³ ABOVE as the KEY. If the record exists the routine WILL   ³
      *    ³ UPDATE the record. If no record exists a NEW record WILL  ³
      *    ³ be CREATED.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY LEDTRF.UPD.

      /
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *
014810 GENERATE-DEBTOR-TRAN SECTION 2.
014820
014830 DEBTOR-TRAN.
014910
014920 LINK-DEBTOR-TRANS.
015010       INITIALIZE TRA-RECORD1.
015090       MOVE W20-DTE        TO TRA-DATE.
015100       MOVE W20-DTE        TO DEB-ACTIVE.
015110       MOVE W90-CODE       TO TRA-CODE.
015120	     MOVE W90-REF	 TO TRA-REF.
	     MOVE W90-REG	 TO TRA-REG.
             MOVE W90-ORD        TO TRA-ORD.
015130       MOVE DEB-ACNO       TO TRA-AC.
015140     IF W90-DB NOT = ZERO
015150         MOVE W90-DB       TO TRA-VALUE
015160     ELSE
015170         MOVE W90-CR       TO TRA-VALUE.
015180     IF W00-ACTION = 1
015190         MULTIPLY -1 BY W90-NONTAX
015200                           GIVING TRA-TAXFREE
015210     ELSE
015220         MOVE W90-NONTAX   TO TRA-TAXFREE.
015230       MOVE W90-TTAX       TO TRA-TAX.
015240       ADD TRA-TAX         TO W00-T-VAT.
015290	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000220	     PERFORM AY04 THRU AY59.
015300       MOVE 1              TO TAX-ACTYPE.
015310       MOVE TRA-CODE       TO TAX-CODE.
015320       MOVE TRA-DATE       TO TAX-DATE.
015330       MOVE TRA-REF        TO TAX-REF.
015340       MOVE TRA-VALUE      TO TAX-VALUE.
015350       MOVE TRA-TAXFREE    TO TAX-TAXFREE.
015360*      MOVE TRA-TAX	 TO TAX-VAT.
015370       MOVE TRA-AC         TO TAX-AC.
015380       IF W90-CR NOT = ZERO
015390           MOVE "1"        TO TAX-TYPE
015400       ELSE
015410           MOVE "2"        TO TAX-TYPE.
015420       MOVE ZERO           TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       DEBTOR-TAX.
	     MOVE W90-TAXVAL(WS-S9)
				 TO TAX-VAL(WS-S9).
	     MOVE W90-VAT(WS-S9) TO TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DEBTOR-TAX.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.
015460	     ADD TRA-VALUE	 TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
015470       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
015480	     PERFORM UPDATE-TRAN-RECORD.
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE. 
015490     IF W90-CR NOT = ZERO
015500         GO TO DEBTOR-CR-TRANS.
015510       ADD W90-DB          TO DEB-DEBT
015520                              DEB-BAL
015530                              DEB-PURYTD
                                    DEB-MTHPUR (W20-MONTH).
015540       GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
	     MOVE "  DT"	 TO WX-LEDG.
	     MOVE W90-CODE	 TO WX-TRN.
	     MOVE W00-E-DESC	 TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
      *
      *    ****    Balancing the Integration entries
      * 	   DEBTOR/ACCOUNTS RECEIVABLE
      *
	     ADD WX-VALUE	 TO WXT-VALUE
	     PERFORM GL-TRF-UPDATE.
015550
015560 DEBTOR-CR-TRANS.
015570       SUBTRACT W90-CR     FROM DEB-CRED
                                      DEB-MTHPUR (W20-MONTH).
015580       ADD W90-CR          TO DEB-PURYTD.               
015590       ADD W90-CR          TO DEB-BAL.
           IF OPEN-ITEM
               GO TO DEBTOR-TRAN-EXIT.
015600     IF WS-AGE = "M"
015610         GO TO DEBTOR-TRAN-EXIT.
015620       MOVE ZERO           TO W90-CUR W90-30 W90-60 W90-90.
015630       MOVE W90-CR         TO W90-VALUE.
015640     IF WS-AGE = "1"
015650         ADD W90-VALUE     TO DEB-120
015660         MOVE W90-VALUE    TO W90-120.
015670     IF DEB-120 < ZERO
015680         SUBTRACT DEB-120  FROM W90-120
015690         MOVE DEB-120      TO W90-VALUE
015700         MOVE ZERO         TO DEB-120
015710         MOVE "9"          TO WS-AGE.
015720     IF WS-AGE = "9"
015730         ADD W90-VALUE     TO DEB-90
015740         MOVE W90-VALUE    TO W90-90.
015750     IF DEB-90 < ZERO
015760         SUBTRACT DEB-90   FROM W90-90
015770         MOVE DEB-90       TO W90-VALUE
015780         MOVE ZERO         TO DEB-90
015790         MOVE "6"          TO WS-AGE.
015800     IF WS-AGE = "6"
015810         ADD W90-VALUE     TO DEB-60
015820         MOVE W90-VALUE    TO W90-60.
015830     IF DEB-60 < ZERO
015840         SUBTRACT DEB-60   FROM W90-60
015850         MOVE DEB-60       TO W90-VALUE
015860         MOVE ZERO         TO DEB-60
015870         MOVE "3"          TO WS-AGE.
015880     IF WS-AGE = "3"
015890         ADD W90-VALUE     TO DEB-30
015900         MOVE W90-VALUE    TO W90-30.
015910     IF DEB-30 < ZERO
015920         SUBTRACT DEB-30   FROM W90-30
015930         MOVE DEB-30       TO W90-VALUE
015940         MOVE ZERO         TO DEB-30
015950         MOVE "C"          TO WS-AGE.
015960     IF WS-AGE = "C"
015970         ADD W90-VALUE     TO DEB-CUR
015980         MOVE W90-VALUE    TO W90-CUR.
015990     IF DEB-CUR < ZERO
016000         SUBTRACT DEB-CUR  FROM W90-CUR
016010         MOVE ZERO         TO DEB-CUR.

016030 DEBTOR-TRAN-EXIT.
016040       EXIT.
      /
017690 BB050		   SECTION.
017700 BB50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     Setup the printer font		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move FontTypeFace	 to Font-family-name.
	     move 12		 to Font-family-namelen.
	     move font-12-CPI	 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     Print header Bitmap		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     perform print-header-BMP.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Advance printer location to past the Bitmap to be printed ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     perform advance-printer 14 times.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Set to print ñ 5cpi and Bold		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-5-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   This heading is printed at 5cpi and centered        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W05-VATNO = SPACES
	       MOVE "            INVOICE/FAKTUUR"
				 TO Print-Buffer
	   ELSE
	       MOVE "     TAX INVOICE/BELASTING FAKTUUR"
				 TO Print-Buffer
	   END-IF.
	     MOVE 44		 TO Print-Buffer-Len.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  Set to print ñ 8 cpi and Bold		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-8-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE 66		 to Print-Buffer-Len.
017710	     MOVE SPACES	 TO W02-PRINT-RECORD.

017950 BB60.
017960       EXIT.
      /
016050 BA000			SECTION 50.
016840 BA80.
016850       MOVE W70-AUDIT      TO WS-AUDKEY.
016860       MOVE LOW-VALUES     TO AUD-REC1.
	     MOVE 1		 TO WS-ACTION.
016870       PERFORM AY16 THRU AY59.
016880	     MOVE STK-CODE	 TO AUD-EXT-CODE.
016890       MOVE 2 TO AUD-TYPE.
016900       MOVE ZERO           TO AUD-SUB.
016910       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016920       ADD 1               TO W70-AUDIT.
016930       MOVE 1              TO WS-AUDKEY.
	     MOVE 0		 TO WS-ACTION.
016940       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
016950       PERFORM AY16 THRU AY59.
016960       MOVE 1              TO AUD-REP2.
016970       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016980 BA90.
016990       MOVE W70-AUDIT      TO WS-AUDKEY.
017000       MOVE LOW-VALUES     TO AUD-REC1.
	     MOVE 1		 TO WS-ACTION.
017010       PERFORM AY16 THRU AY59.
017050	     MOVE 7		 TO AUD-TYPE.
017060       MOVE WS-DRAW        TO AUD-SUB.
017070       MOVE W90-REF        TO AUD-CSH.
017080       MOVE W90-EXCTOT     TO AUD-SELL.
017090       MOVE W90-TCOST      TO AUD-COST.
017100       MOVE W10-SMAN       TO AUD-SMAN.
017110       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017120       ADD 1               TO W70-AUDIT.
017130       MOVE 1              TO WS-AUDKEY.
	     MOVE 0		 TO WS-ACTION.
017140       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017150       PERFORM AY16 THRU AY59.
017190	     MOVE 1		 TO AUD-REP7.
017200       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017210 BA95.
017220       MOVE W70-AUDIT      TO WS-AUDKEY.
017230       MOVE LOW-VALUES     TO AUD-REC1.
	     MOVE 1		 TO WS-ACTION.
017240       PERFORM AY16 THRU AY59.
017290	     MOVE 3		 TO AUD-TYPE
017300	     MOVE DEB-ACNO	 TO AUD-AC.
017310       MOVE WS-DRAW        TO AUD-SUB.
017320       MOVE W90-REF        TO AUD-REF.
017330       MOVE W90-TTAX       TO AUD-TAX.
017340       MOVE W90-TOTAL      TO AUD-VAL.
017350       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017360       ADD 1               TO W70-AUDIT.
017370       MOVE 1              TO WS-AUDKEY.
	     MOVE 0		 TO WS-ACTION.
017380       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017390       PERFORM AY16 THRU AY59.
017430	     MOVE 1		 TO AUD-REP3.
017440       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017450 BA100.
017460       MOVE W70-AUDIT      TO WS-AUDKEY.
017470       MOVE LOW-VALUES     TO AUD-REC1.
	     MOVE 1		 TO WS-ACTION.
017480       PERFORM AY16 THRU AY59.
017520	     MOVE 8		 TO AUD-TYPE.
017530       MOVE WS-DRAW        TO AUD-SUB.
017540       MOVE W10-SMAN       TO AUD-SMAN.
017550       MOVE W90-REF        TO AUD-CSH.
017560       MOVE W90-VALUE      TO AUD-SEL.
017570       MOVE W90-ICST       TO AUD-CST.
017580	     MOVE STK-CODE	 TO AUD-EXT-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
	     MOVE WS-DISCOUNT	 TO AUD-DISC.
017590       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017600       ADD 1               TO W70-AUDIT.
017610       MOVE 1              TO WS-AUDKEY.
	     MOVE 0		 TO WS-ACTION.
017620       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017630       PERFORM AY16 THRU AY59.
017670	     MOVE 1		 TO AUD-REP8.
017680       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
      /
       BB070		   SECTION 50.
       BB70.
             INITIALIZE GUA-RECORD1.
             MOVE ZERO           TO WS-F-ERROR.
	     MOVE STK-CODE	 TO GUA-EXT-CODE.
             MOVE W10-SERNO      TO GUA-SERIAL.
           IF WS-GAR = 1
               GO TO BB80.
             MOVE STK-DESC       TO GUA-DESC.
             MOVE W40-NAME       TO GUA-NAME.
             MOVE W12-T-YMD      TO GUA-DATE1 W20-DTE.
             MOVE STK-GUAR       TO GUA-PERIOD.
             ADD GUA-PERIOD      TO W20-MONTH.
       BB75.
           IF W20-MONTH > 12
               SUBTRACT 12       FROM W20-MONTH
               IF W20-YEAR = 99
                   MOVE ZERO     TO W20-YEAR
		   ADD 1	 TO W20-CENT
                   GO TO BB75
               ELSE
                   ADD 1         TO W20-YEAR
                   GO TO BB75.
             MOVE W20-DTE        TO GUA-DATE2.
             MOVE W90-REF        TO GUA-INV.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             GO TO BB99.
       BB80.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO BB99.
             MOVE 2              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
       BB99.
             EXIT.
      /    ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
      *    º	     I N V O I C E   A N D   C A S H   S A L E	       º
      *    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
018500 BC000	     SECTION 50.
018510 BC00.
018690	     MOVE 1		 TO WS-FIRST WS-LAST.
018710       MOVE "I"            TO WS-CR.
018720 BC10.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
018770	     MOVE W40-ACNO	 TO W80-ACNO.
018930	     PERFORM CA155-GET-DEBTOR.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
019990	     MOVE W40-SMAN	 TO W10-SMAN.
	     MOVE W40-PRNINV	 TO WS-PRNINV.
	     MOVE W40-LAST	 TO WS-LAST.
      *	     MOVE W40-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE W40-PRC	 TO WS-PRC.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
025410 BC188.
025480       MOVE 1              TO WS-SALKEY.
025490       PERFORM AY60 THRU AY999.
025500       PERFORM AY02 THRU AY59.
025510       MOVE W70-INVOICE    TO W90-REF.
025520	     DISPLAY W90-REF AT 0510
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 0.
025530	   IF W70-INVOICE = 99999999
025540	       MOVE 1		 TO W70-INVOICE
025550	   ELSE
025560	       ADD 1		 TO W70-INVOICE.
      *
      *    ****    S K I P   P R I N T I N G   I N V O I C E ?
      *
026310 BC215.
	   IF WS-PRNINV = "N"
	       GO TO BC220.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
027940	     PERFORM BB050.
	     MOVE "DELIVERY NOTE/AFLEWERINGSNOTA"
				 TO INV-INVOICE.
	     move W02-print-record
				 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
017710	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Print the Date and Reference Number	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DATE  :"	 TO INV-H14A.
	     MOVE W12-TODAY	 TO INV-DTE.
      *
      *    ***	Date used for 2nd Copy of Invoice
      *
	     MOVE W12-T-YMD	 TO W40-DTE.
	     MOVE "REF No:"	 TO INV-H14.
026590	     MOVE W90-REF	 TO INV-REF.
	     move W02-print-record
				 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-15-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Clear the print buffer area		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     move W02-print-record
				 to Print-Buffer.
	     MOVE 120		 to Print-Buffer-Len.
	     perform advance-printer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Name, Postal & Delivery Address, Page Number, Account    ³
      *    ³  number, Order Number and VAT number.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DELIVERY / AFLEWRING"
				 TO INV-DADD2.
	     move W02-print-record
				 to Print-Buffer.
	     MOVE 120		 to Print-Buffer-Len.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026330	     MOVE W40-NAME	 TO INV-NAME.
	     MOVE "PAGE / BLADSY"
				 TO INV-H4.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO INV-PGE.
	     move W02-print-record
				 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026350       MOVE W40-ADD1       TO INV-NAME.
026360	     MOVE W40-PADD1	 TO INV-DADD2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026380	     MOVE W40-ADD2	 TO INV-NAME.
026390	     MOVE W40-PADD2	 TO INV-DADD2.
	     MOVE "ACCOUNT / REKENING"
				 TO INV-H4.
026550	     MOVE W80-ACNO	 TO INV-AC.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026410       MOVE W40-ADD3       TO INV-NAME.
026420	     MOVE W40-PADD3	 TO INV-DADD2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026440       MOVE W40-ADD4       TO INV-NAME.
026450	     MOVE W40-PADD4	 TO INV-DADD2.
	     MOVE "ORDER / BESTELLING"
				 TO INV-H4.
026560       MOVE W90-ORD        TO INV-ORD.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
026510       MOVE W40-PC1        TO INV-PC1.
026510	     MOVE W40-PC2	 TO INV-PC2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *
      *    ****    C L I E N T	 V A T	 N U M B E R
      *
	   IF NOT(W40-TAX = SPACES)
	       MOVE "Vat No"	 TO INV-VATH
	       MOVE W40-TAX	 TO INV-VATNO.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
	     MOVE "TERMS / TERME"
				 TO INV-H4.
028720	   IF DEB-TERMS = "0"
028730	       MOVE "7 Days/Dae" TO INV-TERMS
028740	   ELSE
028750	   IF DEB-TERMS = "2"
028760	       MOVE "60 Days/Dae"
				 TO INV-TERMS
028770	   ELSE
028780	   IF DEB-TERMS = "1"
028790	       MOVE "30 Days/Dae"
				 TO INV-TERMS
028800	   ELSE
028810	       MOVE "COD/KBA"	 TO INV-TERMS.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	   IF W10-SMAN NOT = ZERO
	       MOVE "Assist :"	 TO INV-H3
	       MOVE W40-ASSIST	 TO INV-DADD2.
	     MOVE 2		 TO WS-ADVANCE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 16cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-16-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Headings for line items		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *						      INV-H7 (13)
	     MOVE "   QUANTITY"	 TO INV-H7.
      *						      INV-H9 (10)
	     MOVE " DISCOUNT"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EXCLUSIVE" TO INV-H10.
      *						      INV-H12 (16)
	     MOVE "   NETT AMOUNT"
				 TO INV-H12.
	     move W02-print-record
				 TO Print-Buffer.
	     MOVE 132		 to Print-Buffer-Len.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Headings for line items		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *						      INV-H5  (20)
	     MOVE "   ITEM CODE/KODE"
				 TO INV-H5.
      *						      INV-H6  (32)
	     MOVE "     DESCRIPTION/BESKRYWING"
				 TO INV-H6.
      *						      INV-H7  (13)
	     MOVE " HOEVEELHEID" TO INV-H7.
      *						      INV-H8  (13)
	     MOVE "  PRICE/PRYS" TO INV-H8.
      *						      INV-H9  (10)
	     MOVE "  AFSLAG"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EKSKLUSIEF"
				 TO INV-H10.
      *						      INV-H11 (13)
	     MOVE "   VAT/BTW"	 TO INV-H11.
      *						      INV-H12 (16)
	     MOVE "   NETO BEDRAG"
				 TO INV-H12.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Switch off bold printing		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *       move 0		  to font-style.
      *       perform set-printer-font.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
026810	     MOVE WS-DET-LINES	 TO WS-LINE.

       BC220.
             EXIT.
      *
      *    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
      *
       BC222.
026490       MOVE WS-PAGE        TO DOC-PAGE.
026550       MOVE W80-ACNO       TO DOC-AC.
             MOVE 100            TO DOC-SQN.
             MOVE ZERO           TO DOC-TYPE.
             MOVE W40-NAME       TO DOC-NAME.
             MOVE ZERO           TO DOC-PICNO.
026640       MOVE WS-CR          TO DOC-DOC.
             MOVE W90-EXP        TO DOC-EXP.
             MOVE WS-PRC         TO DOC-PRC.
026560       MOVE W90-ORD        TO DOC-ORD.
026590       MOVE W90-REF        TO DOC-REF.
026600       MOVE W90-REG        TO DOC-OREF.
026680       MOVE W12-T-YMD      TO DOC-DTE.
026800       MOVE W10-SMAN       TO DOC-SMAN.
028820	     MOVE W90-TOTAL	 TO DOC-VALUE.
	     MOVE LS0-NO	 TO DOC-USER.
	     MOVE LS0-NAME	 TO DOC-UNME.
      *
      *    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
      *
       BC225.
	     PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
015400	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY22 THRU AY59.
026830 BC235.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   I N V O I C E
      *
026840     IF WS-SALKEY > WS-LAST
026850         GO TO BC255.
026860       PERFORM READ-SALE THRU READ-SALE-EXIT.
026870     IF WS-LINE > 1
026880         SUBTRACT 1        FROM WS-LINE
026890     ELSE
026900         PERFORM BC250.
026910	   IF SAL-EXT-ITEM = SPACES
026920         GO TO BC247.
026930	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
026940       PERFORM AY05 THRU AY59.
      *
      *    ****    W A R E H O U S E   S T O C K
      *
	   IF NOT (W40-CODE = SPACES) AND
		  (STK-IND < 2)
	      MOVE W40-CODE	 TO WST-WAR
	      MOVE SAL-EXT-ITEM  TO WST-EXT-CODE
	      PERFORM AY052 THRU AY59.
      *
      *    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
      *            R E C O R D
      *
	     MOVE W80-ACNO	 TO CRD-AC.
	     MOVE ZERO		 TO CRD-SEQ.
017540	     MOVE W10-SMAN	 TO CRD-SMAN.
017550	     MOVE W90-REF	 TO CRD-REF.
	     MOVE STK-SUPP	 TO CRD-SUPP.
	     MOVE W12-T-YMD	 TO CRD-DTE.
	     MOVE W40-NAME	 TO CRD-NAME.
	   IF STK-IND < 2
	       MOVE W40-CODE	 TO CRD-WHS
	   ELSE
	       MOVE SPACES	 TO CRD-WHS.
	     MOVE STK-LDG	 TO CRD-DEPT.
026960	     MOVE 1		 TO DOC-TYPE.
             INITIALIZE DOC-DET2.
026970	     MOVE SAL-EXT-ITEM	 TO DOC-EXT-ITEM CRD-EXT-CODE.
026980       MOVE SAL-DESC       TO DOC-DESC.
026990	     MOVE SAL-QNT	 TO W10-QUANT DOC-QNT.
	     MOVE SAL-PACK	 TO WS-P1.
      *
      *    ****    C H E C K   I F   D I S C O U N T   G I V E N
      *
	     MOVE "N"		 TO WS-DISCOUNT.
	     ADD .009 SAL-SELL	 GIVING W10-MSELL.
	   IF NOT (SAL-SPEC = "Y")
	      IF ((WS-PRC = "1") AND (W10-MSELL < STK-SL(WS-P1))) OR
		 ((WS-PRC = "2") AND (W10-MSELL < STK-CSH(WS-P1))) OR
		 ((WS-PRC = "3") AND (W10-MSELL < STK-WSL(WS-P1)))
		   MOVE "Y"	 TO WS-DISCOUNT.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
	     MOVE W10-QNT	 TO CRD-QNT.
040230       MOVE SAL-COST       TO W10-COST  DOC-COST.
	     COMPUTE CRD-COST = SAL-COST / STK-UNT(WS-P1).
027020	     MOVE SAL-VAT	 TO DOC-VAT.
027030	     MOVE SAL-SELL	 TO W10-SELL  DOC-SELL.
027050       MOVE SAL-VAL        TO W25-VALUE DOC-VAL.
026970	     MOVE SAL-EXT-ITEM	 TO INV-EXT-ITM.
026980	     MOVE SAL-DESC	 TO INV-DESC.
026990	     MOVE SAL-QNT	 TO INV-QNT.
027030	     MOVE SAL-SELL	 TO INV-PRICE.
027020	     MOVE SAL-VAT	 TO INV-TAX.
027080	     MOVE SAL-DSC	 TO WS-DISC INV-DSC.
027070	     MOVE SAL-DISC	 TO W90-CDISC W90-EXCDIS.
027090       SUBTRACT W90-CDISC FROM W25-VALUE
027100                           GIVING W90-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB
				    DOC-VAT-SUB.
	   IF WS-VAT-SUB = ZERO
	       MOVE ZERO	 TO DOC-V-PER
	   ELSE
	       MOVE W05-VAT(WS-VAT-SUB)
				 TO DOC-V-PER.
027110	   IF W90-TAX = SPACES
027120       IF W90-EXP NOT = "X"
040350	       IF WS-VAT-SUB > ZERO
027140		   MOVE W05-VAT(WS-VAT-SUB)
027150                           TO W05-RATE
027160		   COMPUTE W90-VALUE ROUNDED = (W90-VALUE -
027170                 (((W90-VALUE / (W05-RATE + 100.00000)) * 
027180			  W05-RATE)))
027190		   COMPUTE W90-EXCDIS ROUNDED = (W90-CDISC -
027200                 (((W90-CDISC / (W05-RATE + 100.00000)) *
027210			  W05-RATE))).
	     MOVE W90-VALUE	 TO INV-EXTEND.
	     ADD W90-VALUE SAL-VAT
				 GIVING INV-VAL.
	     MOVE SAL-TAXVAL	 TO DOC-TAXVAL.
             MOVE SAL-NONTAX     TO DOC-NONTAX.
             MOVE SAL-SUPER      TO WS-SUPER.
027220	     ADD W90-VALUE	 TO W90-EXCTOT.
	     COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
	   IF WS-CARDEX = "Y"
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59
	   END-IF.
	   IF WS-PRNINV = "Y"
	       MOVE 1		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-ITEM
027240	       MOVE SPACES	 TO W02-PRINT-RECORD
	   END-IF.
027250       ADD W25-VALUE       TO W90-STOTAL.
      *
      *    ****    H A S   L I N E   D I S C O U N T   B E E N   
      *            A P P L I E D
      *
027260     IF W90-CDISC = ZERO
027270	       GO TO BC240
	   END-IF.
      *
      *    ****    P R I N T   A N D   L O G   T H E   L I N E
      *            D I S C O U N T
      *
027360       MOVE WS-DISC        TO DOC-DSC.
027370       MULTIPLY -1         BY W90-CDISC 
027380                           GIVING DOC-DISC.
027420 BC240.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If this is a copy of the INVOICE do not update the various³
      *    ³ statistics, totals nor TAX fields. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC242.
           IF WS-COPY = "Y"
               GO TO BC246.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     U P D A T E   S A L E S   A N A L Y S I S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027430	     MOVE STK-LDG	 TO DPT-CODE.
027440       PERFORM AY38 THRU AY59.
027450	     ADD W90-VALUE	 TO DPT-SALES-DAY
027460                              DPT-SALES-MTD
027470                              DPT-SALES-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "SALES - DEBTORS"
                                 TO WX-NAR
               COMPUTE WX-VALUE = W90-VALUE * -1
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 04           TO WX-TYPE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      Balancing the Integration entries 	       ³
      *    ³		   SALES LEDGER/CATERGORY		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF BALANCING-ENTRY-STORED
		   ADD WX-VALUE  TO WXT-VALUE
	       ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Store first Income Integration entry	       ³
      *    ³  As items are selected randomly, storing the first income ³
      *    ³  entry each time will mean that INCOME ACCOUNTS are also  ³
      *    ³  randomly adjusted.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   MOVE WX-LEDTRF
				 TO WXT-LEDTRF
		   MOVE "Y"	 TO WS-BAL-ENTRY
	       END-IF
	       PERFORM GL-TRF-UPDATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     U P D A T E   C O S T - O F - S A L E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027480	     COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
027490       ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
027500                              DPT-COST-YTD.
           IF WX-INT = "Y"
               IF W90-BCR NOT = ZERO
                   MOVE W90-BCR  TO WX-VALUE
                   MOVE DPT-COST-GL
                                 TO WX-AC
                   MOVE 08       TO WX-TYPE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As this entry is double sided - it will not be used to    ³
      *    ³ check the balancing of the itegration entries.	       ³
      *	   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   PERFORM GL-TRF-UPDATE
                   COMPUTE WX-VALUE = W90-BCR * -1
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE 19       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	     U P D A T E   D I S C O U N T   G I V E N	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027510	     ADD W90-EXCDIS	 TO DPT-DISC-DAY
027520                              DPT-DISC-MTD
027530                              DPT-DISC-YTD
027540                              DPT-SALES-DAY
027550                              DPT-SALES-MTD
027560                              DPT-SALES-YTD.
027570	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
027580       MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D    
      *
	   IF WS-PRC-IND = "Y"
027590	       IF STK-IND > 1
027600		   GO TO BC245
	       END-IF
	   ELSE
	   IF STK-IND > ZERO
	       IF STK-IND = 1
		   PERFORM BC245
		   PERFORM DB000
013770		   GO TO BC246
	       END-IF
027600	       GO TO BC245.
      *
      *    ****    W A R E H O U S E   S T O C K
      *
	   IF NOT (W40-CODE = SPACES)
	       SUBTRACT W10-QNT  FROM WST-QUANT
           ELSE
027610	       SUBTRACT W10-QNT  FROM STK-QUANT
	       IF STK-USE-CASES = "Y"
		   SUBTRACT SAL-CASES
				 FROM STK-CASES.
	   IF STK-IND = 1
	       GO TO BC245.
027620	   IF STK-DISC > ZERO
027630         GO TO BC245.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
           IF W40-CODE = SPACES
027640     IF STK-QUANT NOT > STK-LEVEL
027650         MOVE 1            TO STK-DISC
027660         PERFORM BA80.

027670 BC245.
027680       ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
027690	     ADD W10-QNT	 TO STK-MTD STK-YTD.
027700       ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
027710	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    W A R E H O U S E   S T O C K
      *
	   IF NOT (W40-CODE = SPACES) AND (STK-IND < 2)
027690	       ADD W10-QUANT	 TO WST-MTD WST-YTD
027700         ADD W90-VALUE     TO WST-MTDV WST-YTDV
027710	       PERFORM REWRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
      *
      *    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
      *            A U D I T   F I L E
      *
027720       MOVE W90-BCR        TO W90-ICST.
027730       PERFORM BA100.
	     ADD 1		 TO DOC-SEQ.
             MOVE ZERO           TO DOC-SUB.
             PERFORM BC225.
      *
      *    ****    G O   A N D   G E T   T H E   N E X T   I T E M
      *
027740 BC246.
027750       ADD 1               TO WS-SALKEY.
027760       GO TO BC235.

027770 BC247.
027790       MOVE 3              TO DOC-TYPE.
             INITIALIZE DOC-DET2.
      *
      *     C H E C K   F O R   S E R I A L   N U M B E R S
      *
           IF SAL-SUB NOT = ZERO
               MOVE 1            TO WS-S1
               PERFORM BC248 
               MOVE "SERIAL NO:" TO INV-HD
               MOVE SAL-MSER     TO INV-SERNOS DOC-MSER
               ADD 1             TO DOC-SUB
           ELSE
               MOVE ZERO         TO DOC-SUB
               ADD 1             TO DOC-SEQ
027800	       MOVE SAL-DESC	 TO INV-DESC DOC-DESC.
           IF WS-PRNINV = "Y"
	       MOVE 1		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-ITEM.
027820	     MOVE SPACES	 TO W02-PRINT-RECORD.
             PERFORM BC225.
027830	     GO TO BC246.

       BC248.
	     MOVE SAL-SNO(WS-S1) TO W10-SERNO.
             MOVE ZERO           TO WS-GAR.
             PERFORM BB070.
             ADD 1               TO WS-S1.
	   IF WS-S1 < 3
	       IF NOT(SAL-SNO(WS-S1) = SPACES)
		   GO TO BC248.
      *
      *    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
      *
027840 BC250.
	   IF WS-PRNINV = "Y"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE 255		 TO color-red
	       MOVE 255		 TO color-green
	       MOVE 255		 TO color-blue
	       perform printer-colour
	       MOVE 1		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
027870	       MOVE "C/FWD - OORGEDRA"
027880				 TO INV-DADD2
027890	       MOVE W90-STOTAL
				 TO INV-VAL
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       move font-15-CPI	 to font-size
	       move 8		 to font-style
	       perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    P R I N T	R E M A R K S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF W30-RMKS15(1) = SPACES
		   MOVE "Please check your invoice."
				 TO INV-RMKS
	       ELSE
		   MOVE W30-RMKS15(1)
				 TO INV-REMKS
	       END-IF
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       IF W30-RMKS15(2) = SPACES
		   MOVE "Gaan asseblief u faktuur na."
				 TO INV-RMKS
	       ELSE
		   MOVE W30-RMKS15(2)
				 TO INV-REMKS
	       END-IF
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE W30-RMKS15(3)
				 TO INV-REMKS
	       MOVE 2		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 19cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       move font-19-CPI	 to font-size
	       move 8		 to font-style
	       perform set-printer-font
	       MOVE 1		 TO WS-ADVANCE
	       MOVE "Processed by APAC Accounting - +27825663095"
				 TO W02-PRINT-RECORD
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE SPACES	 TO W02-PRINT-RECORD
	       PERFORM Throw-a-page.
027950	     PERFORM BC215 THRU BC220.
           IF WS-PRNINV = "Y"
027970	       MOVE "B/FWD - O/B"
                                 TO INV-DESC
027980	       MOVE W90-STOTAL	 TO INV-VAL
	       MOVE 1		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-ITEM
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
	       PERFORM PRINT-ITEM
027990         SUBTRACT 2        FROM WS-LINE.
028010 BC255.
      *
      *    ****    I N V O I C E   T O T A L S
      *
           IF WS-PRNINV = "N"
               GO TO BC256.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Print the column background colours down to the TOTAL and ³
      *    ³ REMARKS line.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-ITEM WS-LINE TIMES.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set Bold printing on.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE all "_"	 to INV-LINE.
	     move W02-print-record
				 to Print-Buffer.
	     MOVE 132		 to Print-Buffer-Len.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     MOVE 1		 TO WS-ADVANCE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If this is a copy of the INVOICE do not update the various³
      *    ³ statistics, totals nor TAX fields. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC256.
	   IF WS-COPY = "Y"
	       GO TO BC258.
	   IF NOT (W40-PADD1 = SPACES)
	       MOVE 5		 TO DOC-TYPE
	       MOVE 99800	 TO DOC-SQN
	       INITIALIZE DOC-DET5
	       MOVE W40-PADD1	 TO DOC-AD1
	       MOVE W40-PADD2	 TO DOC-AD2
	       MOVE W40-PADD3	 TO DOC-AD3
	       MOVE W40-PADD4	 TO DOC-AD4
	       MOVE W40-PC2	 TO DOC-PC
	       PERFORM BC225.
	     MOVE 9		 TO DOC-TYPE.
             MOVE 99900          TO DOC-SQN.
             INITIALIZE DOC-DET3.
028070       MOVE W90-DB         TO DOC-SUBTOT.
	     MOVE DEB-TERMS	 TO DOC-TERMS.
      *
      *    ****    P R I N T   D I S C O U N T   A N D   U P D A T E
      *            T H E   S U N D R Y   D I S C O U N T   T O T A L S
      *
028150     IF W90-TDISC NOT = ZERO
028180         MULTIPLY -1 BY W90-TDISC 
028190                           GIVING DOC-DSCNT
028200         IF W90-LDISC = ZERO
		   IF WS-COPY = "N"
041420		       MOVE "YYYY"
				 TO DPT-CODE
028220                 PERFORM AY38 THRU AY59
028230                 ADD W90-TDISC
                                 TO DPT-DISC-DAY
028240                              DPT-DISC-MTD
028250                              DPT-DISC-YTD
                       IF WX-INT = "Y"
                           MOVE W90-EXCDIS   TO WX-VALUE
                           MOVE DPT-DISC-GL  TO WX-AC
			   MOVE DPT-CODE     TO WX-LEDG
                           MOVE 15           TO WX-TYPE
      *
      *    ****    Balancing the Integration entries
      * 	   DISCOUNT
      *
			   ADD WX-VALUE	     TO WXT-VALUE
			   PERFORM GL-TRF-UPDATE
                       END-IF
028260		       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    P R I N T   T H E   T A X   F I G U R E S   A N D 
      *            U P D A T E   T A X   T O T A L S
      *
      * 	   TAX CALCULATED IN DTPINV
      * 	       W90-NONTAX   =TOTAL NON TAXABLE VALUE
      * 	       W90-TTAX     =TOTAL TAX (ALL RATES)
      * 	       W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
      * 	       W90-VAT(?)   =TOTAL VAT PER RATE
      * 	       W90-TOTAL    =TOTAL FOR CREDIT NOTE
      *
      *ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
028660	     MOVE W05-RATE	 TO WS-PER.

       BC258.
	     MOVE "TOTAL/TOTAAL" TO INV-H8.
	     MOVE W90-EXCTOT	 TO INV-EXTEND.
	   IF WS-COPY = "N"
028680	       MOVE W90-TTAX	 TO DOC-TAX.
028680	     MOVE W90-TTAX	 TO INV-TAX.

028690 BC260.
      *
      *    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
      *
028820       MOVE W90-TOTAL      TO W90-DB.
           IF WS-COPY = "N"
028830         MOVE W90-BDT      TO W90-TCOST
028840         MOVE W90-DB       TO DOC-TOT
	       MOVE W90-EXCTOT	 TO DOC-TAXABLE
               MOVE W90-NONTAX   TO DOC-NON-TXBL
               MOVE W90-BAL      TO DOC-BAL
	       MOVE W90-CONTACT  TO DOC-CONTACT
	       MOVE W40-TEL	 TO DOC-TEL
               PERFORM BC225.
           IF WS-PRNINV = "Y"
028840	       MOVE W90-DB	 TO INV-VAL
	       MOVE 1		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE SPACES	 TO W02-PRINT-RECORD
	       MOVE all "_"	 to INV-LINE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE SPACES	 TO W02-PRINT-RECORD
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       move font-15-CPI	 to font-size
	       move 8		 to font-style
	       perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    P R I N T	R E M A R K S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF W30-RMKS15(1) = SPACES
		   MOVE "Please check your invoice."
				 TO INV-RMKS
	       ELSE
		   MOVE W30-RMKS15(1)
				 TO INV-REMKS
	       end-if
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
	       IF W30-RMKS15(2) = SPACES
		   MOVE "Gaan aseblief u faktuur na."
				 TO INV-RMKS
	       ELSE
		   MOVE W30-RMKS15(2)
				 TO INV-REMKS
	       end-if
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
027860	       MOVE SPACES	 TO W02-PRINT-RECORD
	       MOVE W30-RMKS15(3)
				 TO INV-REMKS
028100	       IF NOT(DEB-SDISC = ZERO) AND
		     (W30-RMKS15(3) = SPACES)
028110		   MOVE W15-DISC (DEB-SDISC)
028120                           TO W30-PERC
028130		   MOVE W30-MESSAGE
				 TO INV-REMKS
	       end-if
	       MOVE 2		 TO WS-ADVANCE
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE SPACES	 TO W02-PRINT-RECORD
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 19cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       move font-19-CPI	 to font-size
	       move 8		 to font-style
	       perform set-printer-font
	       MOVE 1		 TO WS-ADVANCE
	       MOVE "Processed by APAC Accounting - +27825663095"
				 TO W02-PRINT-RECORD
	       move W02-print-record
				 TO Print-Buffer
	       PERFORM PRINT-DETAILS
	       MOVE SPACES	 TO W02-PRINT-RECORD
	       PERFORM Throw-a-page.
028900	     MOVE W12-T-YMD	 TO W20-DTE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 U P D A T E   T H E   D E B T O R   A C C O U N T     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
028910	     MOVE 1		 TO W90-CODE.
028920	     PERFORM GET-DEBTOR-TRAN-RECORD.
028930	     PERFORM GENERATE-DEBTOR-TRAN.
014480	     MOVE 9		 TO WS-PARKEY.
014500       PERFORM AY01 THRU AY59.
	     MOVE 1		 TO W05-V-RATE.

       BC262-VAT.
	   IF W90-VAT(W05-V-RATE) = ZERO
	       GO TO BC262-CHECK-END.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
027840	     PERFORM AY38 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    V A T   S T A T I S T I C S 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027850	     ADD W90-VAT(W05-V-RATE)
				 TO DPT-O-VAT-DAY
027860				    DPT-O-VAT-MTD
027870				    DPT-O-VAT-YTD.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO DPT-V-SALES-DAY
				    DPT-V-SALES-MTD
				    DPT-V-SALES-YTD.
	   IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
		COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
		MOVE DPT-O-VAT-GL
				 TO WX-AC
		MOVE DPT-CODE	 TO WX-LEDG
		MOVE 10 	 TO WX-TYPE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Balancing the Integration entries	       ³
      *    ³			   OUTPUT VAT			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		ADD WX-VALUE	 TO WXT-VALUE
		PERFORM GL-TRF-UPDATE.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.

       BC262-CHECK-END.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO BC262-VAT.
	     ADD W90-NONTAX	 TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
028940	     MOVE ZERO		 TO W90-CUR W90-30 W90-60
028950                              W90-120 W90-90.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  V A T   C A L C U L A T I O N   P R O B L E M  -	F I X  ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³ Check if the Integration entries are in balance. If they  ³
      *    ³ are out of balance, process a balancing entry.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WX-INT = "Y"
	       IF NOT(WXT-VALUE = ZERO)
		   MOVE WXT-LEDTRF
				 TO WX-LEDTRF
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Change the sign of the amount that the entries are out of ³
      *    ³ balance and process.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		   COMPUTE WX-VALUE = WX-VALUE * -1
		   PERFORM GL-TRF-UPDATE
		   MOVE "N"	 TO WS-BAL-ENTRY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   L O G   T H E   S A L E S M A N	 &   I N V O I C E -   ³
      *    ³	       A U D I T   F I L E   D E T A I L S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029150	     PERFORM BA90.
029160	     PERFORM BA95.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 U P D A T E   T H E   D E B T O R   C A T E G O R Y   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029180	     ADD 121 DEB-CAT	 GIVING W90-RESULT.
029190       DIVIDE W90-RESULT BY 2
029200                           GIVING W90-CATKEY
029210                           REMAINDER W90-CS.
029220       ADD 1               TO W90-CS.
029230       MOVE W90-CATKEY     TO WS-PARKEY.
029240       PERFORM AY01 THRU AY59.
029250       ADD W90-DB          TO PAR-CDRMTD (W90-CS).
029260       ADD W90-DB          TO PAR-CDRYTD (W90-CS).
029270	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	U P D A T E   T H E   S A L E S M A N	R E C O R D    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029280	   IF W10-SMAN NOT = ZERO
029290         ADD 100 W10-SMAN  GIVING WS-PARKEY
029300         PERFORM AY01 THRU AY59
029310         ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
029320         ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
029330	       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 R E W R I T E	 T H E	 D E B T O R   R E C O R D     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029340	     PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  U P D A T E	T H E	D E B T O R   C O N T R O L    ³
      *    ³			    R E C O R D 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029350	     ADD W90-DB 	 TO W70-OUT
029360                              W70-DT.
029370       ADD W90-CUR         TO W70-MTD.
029380       ADD W90-30          TO W70-ONE.
029390       ADD W90-60          TO W70-TWO.
029400       ADD W90-90          TO W70-THREE.
029410       ADD W90-120         TO W70-FOUR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  U P D A T E	T H E	S T O C K   C O N T R O L      ³
      *    ³			 R E C O R D			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC265.
029440       SUBTRACT W90-BAL    FROM STK-TOTAL.
029450       ADD W90-SALES       TO STK-SMTD STK-SYTD.
029460 BC266.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  U P D A T E	T H E	C O S T - O F - S A L E S      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029470	     MOVE 8		 TO WS-PARKEY.
029480       PERFORM AY01 THRU AY59.
029490       ADD W90-BDT         TO PAR-CSHT PAR-CMTD PAR-CYTD.
029500       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
029510 BC267.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  E N D   O F F   T H E   R E C O V E R Y   L O G      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029520	     PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       C L O S E   F I L E S		       ³
      *    ³	     ( FORCE SYSTEM TO WRITE DETAILS TO DISK )	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
029530	     CLOSE PARAM
029540             DEPART 
029550             DEBTOR 
029560             AUDIT 
029570             STOCK 
029580             INVOICE.
029620       OPEN I-O PARAM 
029630                DEPART 
029640                DEBTOR 
029650                AUDIT 
029660                STOCK 
029670                INVOICE. 
029690	     DISPLAY CLEAR-L25.

031960 BC599.
031970       EXIT.
      /    ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
      *    º		  I N V O I C E	  R E - P R I N T	       º
      *    ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
044860 BE000	     SECTION 51.
044870 BE00.
018690	     MOVE 1		 TO WS-FIRST.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
018770	     MOVE W40-ACNO	 TO W80-ACNO.
018930	     PERFORM CA155-GET-DEBTOR.
019990	     MOVE W40-SMAN	 TO W10-SMAN.
	     MOVE W40-LAST	 TO WS-LAST.
	     MOVE W40-PRC	 TO WS-PRC.
	     MOVE W40-CR	 TO WS-CR.
	     MOVE W40-DTE	 TO W20-DTE.
046420	     DISPLAY "Re-printing Document" AT 2520 WITH
046430		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
	     MOVE 1		 TO WS-SALKEY.
047140 BE65.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
027940       PERFORM BB050.
047490	   IF WS-CR = "C"
047500	       MOVE "CREDIT NOTE/KREDIET NOTA"
047510				 TO INV-CRNOTE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
017710	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     MOVE "COPY/AFSKRIF" TO INV-COPY.
      *	     move W02-print-record
      *				 TO Print-Buffer.
      *	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Print the Date and Reference Number	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DATE  :"	 TO INV-H14A.
026670	     MOVE W20-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
002770	     MOVE W22-YY2	 TO W22-YY3.
002780	     MOVE W22-MM2	 TO W22-MM3.
002790	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO INV-DTE.
	     MOVE "REF No:"	 TO INV-H14.
026590	     MOVE W90-REF	 TO INV-REF.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-15-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   Clear the print buffer area		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     move W02-print-record
				 TO Print-Buffer.
	     MOVE 120		 to Print-Buffer-Len.
	     perform advance-printer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Name, Postal & Delivery Address, Page Number, Account    ³
      *    ³  number, Order Number and VAT number.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE "DELIVERY / AFLEWRING"
				 TO INV-DADD2.
	     move W02-print-record
				 TO Print-Buffer.
	     MOVE 120		 to Print-Buffer-Len.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026330	     MOVE W40-NAME	 TO INV-NAME.
	     MOVE "PAGE / BLADSY"
				 TO INV-H4.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO INV-PGE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026350       MOVE W40-ADD1       TO INV-NAME.
026360	     MOVE W40-PADD1	 TO INV-DADD2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026380	     MOVE W40-ADD2	 TO INV-NAME.
026390	     MOVE W40-PADD2	 TO INV-DADD2.
	     MOVE "ACCOUNT / REKENING"
				 TO INV-H4.
026550	     MOVE W80-ACNO	 TO INV-AC.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026410       MOVE W40-ADD3       TO INV-NAME.
026420	     MOVE W40-PADD3	 TO INV-DADD2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
026320	     MOVE SPACES	 TO W02-PRINT-RECORD.
026440       MOVE W40-ADD4       TO INV-NAME.
026450	     MOVE W40-PADD4	 TO INV-DADD2.
	     MOVE "ORDER / BESTELLING"
				 TO INV-H4.
026560       MOVE W90-ORD        TO INV-ORD.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
026510       MOVE W40-PC1        TO INV-PC1.
026510	     MOVE W40-PC2	 TO INV-PC2.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *
      *    ****    C L I E N T	 V A T	 N U M B E R
      *
	   IF NOT (W40-TAX = SPACES)
	       MOVE "Vat No"	 TO INV-VATH
	       MOVE W40-TAX	 TO INV-VATNO.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
	     MOVE "TERMS / TERME"
				 TO INV-H4.
028720	   IF DEB-TERMS = "0"
028730	       MOVE "7 Days/Dae" TO INV-TERMS
028740	   ELSE
028750	   IF DEB-TERMS = "2"
028760	       MOVE "60 Days/Dae"
				 TO INV-TERMS
028770	   ELSE
028780	   IF DEB-TERMS = "1"
028790	       MOVE "30 Days/Dae"
				 TO INV-TERMS
028800	   ELSE
028810	       MOVE "COD/KBA"	 TO INV-TERMS.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	   IF W10-SMAN NOT = ZERO
	       MOVE "Assist :"	 TO INV-H3
	       MOVE W40-ASSIST	 TO INV-DADD2.
	     MOVE 2		 TO WS-ADVANCE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 16cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-16-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *						      INV-H7 (13)
	     MOVE "   QUANTITY"	 TO INV-H7.
      *						      INV-H9 (10)
	     MOVE " DISCOUNT"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EXCLUSIVE" TO INV-H10.
      *						      INV-H12 (16)
	     MOVE "   NETT AMOUNT"
				 TO INV-H12.
	     move W02-print-record
				 TO Print-Buffer.
	     MOVE 132		 to Print-Buffer-Len.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Headings for line items		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *						      INV-H5  (20)
	     MOVE "   ITEM CODE/KODE"
				 TO INV-H5.
      *						      INV-H6  (32)
	     MOVE "     DESCRIPTION/BESKRYWING"
				 TO INV-H6.
      *						      INV-H7  (13)
	     MOVE " HOEVEELHEID" TO INV-H7.
      *						      INV-H8  (13)
	     MOVE "  PRICE/PRYS" TO INV-H8.
      *						      INV-H9  (10)
	     MOVE "  AFSLAG"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EKSKLUSIEF"
				 TO INV-H10.
      *						      INV-H11 (13)
	     MOVE "   VAT/BTW"	 TO INV-H11.
      *						      INV-H12 (16)
	     MOVE "   NETO BEDRAG"
				 TO INV-H12.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Switch off bold printing		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *       move 0		  to font-style.
      *       perform set-printer-font.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
026810	     MOVE WS-DET-LINES	 TO WS-LINE.

       BE70.
             EXIT.
      *
      *    ****    R E A D   T H E   S A L E	F I L E   A N D
      *            P R I N T   T H E   I N V O I C E
      *
047690 BE75.
026840	   IF WS-SALKEY > WS-LAST
026850         GO TO BE90.
026860       PERFORM READ-SALE THRU READ-SALE-EXIT.
026870     IF WS-LINE > 1
026880         SUBTRACT 1        FROM WS-LINE
026890     ELSE
026900         PERFORM BE88.
026910	   IF SAL-EXT-ITEM = SPACES
026920         GO TO BE86.
026930	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
013470       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
      *            R E C O R D
      *
026990       MOVE SAL-QNT        TO W10-QUANT.
027010       MOVE SAL-COST       TO W10-COST.
027030       MOVE SAL-SELL       TO W10-SELL.
027050	     MOVE SAL-VAL	 TO W25-VALUE.
027070	     MOVE SAL-DISC	 TO W90-CDISC W90-EXCDIS.
	  IF NOT (SAL-VAT = ZERO)
	      MOVE W05-VAT(SAL-VAT-SUB)
				 TO W05-RATE
	      COMPUTE WS-VAT ROUNDED = (SAL-VAL + W90-CDISC) * W05-RTE
	      IF WS-VAT < ZERO
		  COMPUTE WS-VAT = WS-VAT * -1
	      END-IF
	      IF NOT (WS-VAT = SAL-VAT)
		  COMPUTE WS-VAT ROUNDED = (SAL-VAL /
			 (100.00 + W05-RATE)) * W05-RATE
		  SUBTRACT WS-VAT FROM SAL-VAL
				 GIVING W25-VALUE.
026970	     MOVE SAL-EXT-ITEM	 TO INV-EXT-ITM.
026980	     MOVE SAL-DESC	 TO INV-DESC.
026990	     MOVE SAL-QNT	 TO INV-QNT.
027030	     MOVE SAL-SELL	 TO INV-PRICE.
	   IF SAL-VAL < ZERO
	       COMPUTE SAL-VAT = SAL-VAT * -1.
027020	     MOVE SAL-VAT	 TO INV-TAX.
027080	     MOVE SAL-DSC	 TO WS-DISC INV-DSC.
027070	     MOVE SAL-DISC	 TO W90-CDISC W90-EXCDIS.
027090	     ADD W90-CDISC W25-VALUE
027100                           GIVING W90-VALUE.
	     MOVE W90-VALUE	 TO INV-EXTEND.
	     ADD W90-VALUE SAL-VAT
				 GIVING INV-VAL.
	     MOVE SAL-SUPER	 TO WS-SUPER.
	     PERFORM PRINT-ITEM.
027240	     MOVE SPACES	 TO W02-PRINT-RECORD.
027250       ADD W25-VALUE       TO W90-STOTAL.
      *
      *    ****    G O   A N D   G E T   T H E   N E X T   I T E M
      *
027740 BE84.
027750       ADD 1               TO WS-SALKEY.
027760       GO TO BE75.
      *
      *     C H E C K   F O R   S E R I A L   N U M B E R S
      *
027770 BE86.
	   IF SAL-SUB NOT = ZERO
               MOVE "SERIAL NO:" TO INV-HD
               MOVE SAL-MSER     TO INV-SERNOS DOC-MSER
           ELSE
	   IF SAL-TYPE = "R"
	       MOVE SAL-MSER	 TO INV-RMK
	   ELSE
027800	       MOVE SAL-DESC	 TO INV-DESC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-ITEM.
027820	     MOVE SPACES	 TO W02-PRINT-RECORD.
027830       GO TO BE84.
      *
      *    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
      *
027840 BE88.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
027870	     MOVE "C/FWD - OORGEDRA"
027880				 TO INV-DADD2.
027890	     MOVE W90-STOTAL	 TO INV-VAL.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-15-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    P R I N T	R E M A R K S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W30-RMKS15(1) = SPACES
	       MOVE "Please check your invoice."
				 TO INV-RMKS
	   ELSE
	       MOVE W30-RMKS15(1)
				 TO INV-REMKS.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	   IF W30-RMKS15(2) = SPACES
	       MOVE "Gaan aseblief u faktuur na."
				 TO INV-RMKS
	   ELSE
	       MOVE W30-RMKS15(2)
				 TO INV-REMKS.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE W30-RMKS15(3)	 TO INV-REMKS.
	     MOVE 2		 TO WS-ADVANCE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 19cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-19-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE 1		 TO WS-ADVANCE.
	     MOVE "Processed by APAC Accounting - +27825663095"
				 TO W02-PRINT-RECORD.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     PERFORM Throw-a-page.
027950	     PERFORM BE65 THRU BE70.
027970	     MOVE "B/FWD - O/B"	 TO INV-DESC.
027980	     MOVE W90-STOTAL	 TO INV-VAL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-ITEM.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     PERFORM PRINT-ITEM.
027990       SUBTRACT 2          FROM WS-LINE.
      *
      *    ****    I N V O I C E   T O T A L S
      *
028010 BE90.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Print the column background colours down to the TOTAL and ³
      *    ³ REMARKS line.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-ITEM WS-LINE TIMES.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set Bold printing on.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  W H I T E   B A C K G R O U N D	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE all "_"	 to INV-LINE.
	     move W02-print-record
				 TO Print-Buffer.
	     MOVE 132		 to Print-Buffer-Len.
	     perform print-line.
	     perform advance-printer 2 times.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     MOVE "TOTAL/TOTAAL" TO INV-H8.
	     MOVE W90-EXCTOT	 TO INV-EXTEND.
028670     IF W90-TAX NOT = SPACES
028680	       MOVE W90-TTAX	 TO INV-TAX.
028820	     MOVE W90-TOTAL	 TO W90-DB.
028840	     MOVE W90-DB	 TO INV-VAL.

	     MOVE 1		 TO WS-ADVANCE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     MOVE all "_"	 to INV-LINE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 15cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-15-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    P R I N T	R E M A R K S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W30-RMKS15(1) = SPACES
	       MOVE "Please check your invoice."
				 TO INV-REMKS
	   ELSE
	       MOVE W30-RMKS15(1)
				 TO INV-REMKS
	   end-if.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
	   IF W30-RMKS15(2) = SPACES
	       MOVE "Gaan aseblief u faktuur na."
				 TO INV-REMKS
	   ELSE
	       MOVE W30-RMKS15(2)
				 TO INV-REMKS
	   end-if.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
027860	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     MOVE W30-RMKS15(3)  TO INV-REMKS.
028100	   IF NOT(DEB-SDISC = ZERO) AND
		 (W30-RMKS15(3) = SPACES)
028110	       MOVE W15-DISC(DEB-SDISC)
028120				 TO W30-PERC
028130	       MOVE W30-MESSAGE  TO INV-REMKS
	   end-if.
	     MOVE 1		 TO WS-ADVANCE.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Set to print ñ 19cpi		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     move font-19-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE "Processed by APAC Accounting - +27825663095"
				 TO W02-PRINT-RECORD.
	     move W02-print-record
				 TO Print-Buffer.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO W02-PRINT-RECORD.
	     PERFORM Throw-a-page.
028900	     MOVE W12-T-YMD	 TO W20-DTE.

       BE99.
             EXIT.
      /
009550 DB000		       SECTION 53.
009560 DB00.
	     MOVE SAL-EXT-ITEM	 TO PRT-EXT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510	   IF WS-F-ERROR = 21
	       GO TO DB999.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
021370	     MOVE 1		 TO WS-S6.
021420 DB05.
021430	   IF PRT-EXT-CODE (WS-S6) = SPACES
021440	       ADD 1		 TO WS-S6
021450	       GO TO DB15.
021470	     MOVE PRT-EXT-CODE (WS-S6)
				 TO STK-CODE.
013470       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
021500	   IF WS-F-ERROR = 22
021510	       ADD 1		 TO WS-S6
021520	       GO TO DB15.
	     PERFORM AY05 THRU AY59.
	     MULTIPLY W10-QUANT BY PRT-QUANT (WS-S6)
			 GIVING W10-PQUANT.
013780	     SUBTRACT W10-PQUANT FROM STK-QUANT.
013870	     ADD W10-PQUANT	 TO STK-MTD STK-YTD.
013790	   IF STK-DISC > ZERO
013800	       GO TO DB10.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
013810	   IF STK-QUANT NOT > STK-LEVEL
013820	       MOVE 1		 TO STK-DISC
013830	       PERFORM BA80.

021660 DB10.
013950	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	     ADD 1		 TO WS-S6.

021660 DB15.
021670	   IF WS-S6 < 31
021680	      GO TO DB05.

       DB999.
	    EXIT.
      /
052700 ZA000-INIT	       SECTION 90.
052710 ZA000-OPEN.
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-DRAW	 TO WS-DRAW.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
		   GO TO ZA00A
	       END-IF
	   END-IF.
             MOVE 110            TO WS-USUB.
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
	   IF WS-STAT1 = "9"
	       IF NET-ERROR OR COM-ERROR
		   PERFORM NETWORK-ERROR THRU LOCK-REC-EXIT
		   GO TO ZA00-READ-APACFIDS.
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
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
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
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    ****      I N V O I C I N G	 B I T M A P   F I L E	    ****
      *    *************************************************************
	     MOVE "INVBMP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     STRING AFID-PATH DELIMITED BY " "
		    X"00" DELIMITED SIZE
		    INTO BmpFilename.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
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
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *	   *************************************************************
      *    **** 	  S T O C K   P A R T S   F I L E	    ****
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.

       ZA01B.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
      * ______________________________________________________________ *
      * PC_PRINTER_OPEN 					       *
      * 							       *
      * Opens a printer channel, giving it a document title, and       *
      * optionally displaying the dialog boxes for printer control,    *
      * font selection, and progress indication.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_OPEN" using	 printer-handle        *
      * 					 document-title        *
      * 			       by value  flags		       *
      * 			       by value  window-handle	       *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	document-title	Group item defined as:		       *
      * 	    title-len	pic x(2) comp-5.		       *
      * 	    title-text	pic x.				       *
      * 	    flags	Numeric literal or pic x(4) comp-5.    *
      * 	    window-handle Numeric literal or pic x(4) comp-5.  *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	title-len	The length of the document title.      *
      * 	title-text	The title of the document to be        *
      * 			printed.			       *
      * 	flags	A set of bits to define printer options:       *
      * 	 bit 0	Display a dialog for defining printer	       *
      * 		characteristics (cannot be used with bit 2 or  *
      * 		bit 3)					       *
      * 	 bit 1	Display a font dialog box, so you can select   *
      * 		the font for the document		       *
      * 	 bit 2	Print in portrait orientation (cannot be used  *
      * 		with bit 0 or bit 3)			       *
      * 	 bit 3	Print in landscape orientation (cannot be used *
      * 		with bit 0 or bit 2)			       *
      * 	 bit 4	Display a progress indicator dialog box        *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * 	window-handle  Handle of the parent window. The parent *
      * 		       window is used as a reference for       *
      * 		       positioning dialog boxes on the screen. *
      * 		       If window-handle is 0 positioning is    *
      * 		       system dependent.		       *
      * 							       *
      * ON EXIT:						       *
      * 	printer-handle	Returns a handle to be used for        *
      * 			subsequent printer operations.	       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    1		Could not open printer device	       *
      * 	    5		Failed to open file		       *
      * 	    9		No default printer found. Select a     *
      * 			printer from Windows Control Panel.    *
      * 	   24		User pressed Cancel on the printer     *
      * 			setup dialog or the font selection     *
      * 			dialog. 			       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	     move 17		 to title-len.
	     move "INVF12W - Invoice" & x"00"
                                 to title-text.
	     call "PC_PRINTER_OPEN" using by reference prn-handle
                                          by reference document-title
                                          by value prn-flags
                                          by value 0
	     end-call.
053590	     MOVE SPACES	 TO W02-PRINT-RECORD.
054760       MOVE 1              TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
054800       MOVE PAR-COMPANY    TO W95-COMP.
054820       MOVE PAR-TELEPHONE  TO W95-TEL.
054830     IF PAR-STMINV = "I" OR "X"
054840         MOVE "N"          TO WS-HEAD
054850     ELSE
054860         MOVE "Y"          TO WS-HEAD.
054870       ADD 1               TO WS-PARKEY.
054880       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054890       MOVE PAR-ADDRESS1   TO W95-ADD1.
054900       MOVE PAR-ADDRESS2   TO W95-ADD2.
054910       MOVE PAR-ADDRESS3   TO W95-ADD3.
054920       MOVE PAR-ADDRESS4   TO W95-ADD4.
054930       MOVE PAR-POST-CDE   TO W95-POST.
054940     IF W95-ADD4 = SPACES
054950         MOVE W95-PC       TO W95-ADD4
054960         MOVE SPACES       TO W95-PC.
054970     IF W95-ADD3 = SPACES
054980         MOVE W95-ADD4     TO W95-ADD3
054990         MOVE SPACES       TO W95-ADD4.
055000     IF W95-ADD2 = SPACES
055010         MOVE W95-ADD3     TO W95-ADD2
055020         MOVE SPACES       TO W95-ADD3.
055030     IF W95-ADD1 = SPACES
055040         MOVE W95-ADD2     TO W95-ADD1
055050         MOVE SPACES       TO W95-ADD2.
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
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
	     MOVE PAR-DSGL	 TO WX-DSGL.
	     MOVE 271		 TO WS-PARKEY.
	     READ PARAM.
	   IF NOT (WS-STATUS = 00)
	       MOVE SPACES	 TO W30-RMKS15(1) W30-RMKS15(2)
				    W30-RMKS15(3) W30-RMKS15(4)
				    W30-RMKS15(5) W30-RMKS15(6)
	       GO TO ZA999.
	     MOVE 1		 TO WS-S1.

       ZA05.
	     MOVE PAR-RMKS1	 TO W30-RMKS15 (WS-S1).
	     ADD 1		 TO WS-S1.
	     MOVE PAR-RMKS2	 TO W30-RMKS15 (WS-S1).
	   IF WS-PARKEY < 273
	       ADD 1		 TO WS-PARKEY WS-S1
	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       GO TO ZA05.
055260	     GO TO ZA999.

       COPY ZA49.PRO.

055970 ZA200.

       COPY LOCKED.PRO.

056040 ZA205.
056050       EXIT PROGRAM.

056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	91.

       COPY ERRORS.PRO.

056260 DISPLAY-FILE-NAME.
056270     IF WS-F-ERROR = 1
056280	       MOVE W02-AUDITF	 TO WS-FILE
056290	       MOVE WS-AUDKEY	 TO WS-KEY
056300     ELSE
056310     IF WS-F-ERROR = 2
056320	       MOVE W02-NETWORK  TO WS-FILE
056330	       MOVE WS-NETKEY	 TO WS-KEY
056340     ELSE
056350     IF WS-F-ERROR = 5
056360	       MOVE W02-DBTRAN	 TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
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
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056460     ELSE
056470     IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
056500     ELSE
056510     IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
056540     ELSE
056550     IF WS-F-ERROR = 18
056560         MOVE W02-RECOVER  TO WS-FILE
056570         MOVE WS-RECKEY    TO WS-KEY
056580     ELSE
056590     IF WS-F-ERROR = 19
056600	       MOVE W02-SALE	 TO WS-FILE
056610	       MOVE WS-SALKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
056700	   ELSE
056710     IF WS-F-ERROR = 32
056720         MOVE W02-TXTRAN   TO WS-FILE
056730         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK	 TO WS-FILE
041790	       MOVE ZERO	 TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.

       9999-Errors     section 91.
       9999-Display-Error.

	   display Operation "Error: " Return-Code.
           evaluate Return-Code
               when 0  display "Success"
               when 1  display "Could not open printer device"
               when 2  display "Invalid printer control code specified"
               when 3  display
                   "No printer device associated with specified handle"
               when 4  display "Out of memory while printing"
               when 5  display "Failed to open file"
               when 6  display "Disk full while spooling file"
               when 7  display
                   "Print job aborted. No job sent to print spooler"
               when 8  display
                   "Printer information structure badly constructed"
               when 9  display "No default printer found"
               when 10 display "Error attempting to display dialog"
               when 11 display "Write failure"
               when 12 display "No fonts found usable with this printer"
               when 13 display "The font requested does not exist"
               when 14 display "User aborted print job"
               when 15 display "Reserved"
               when 16 display "Reserved"
               when 17 display "Reserved"
               when 18 display "Failed to load bitmap"
               when 19 display "Invalid bitmap id"
               when 20 display "Failed to free bitmap"
               when 21 display "Failed to print bitmap"
               when 22 display "Bad parameter"
               when 23 display "Internal error"
               when 24 display "User pressed Cancel on Dialog box"
               when other display "Unknown error"
	   end-evaluate.

	   stop run.
