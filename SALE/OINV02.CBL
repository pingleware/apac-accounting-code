      $set LINKCOUNT"772"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  **    **    ****     ******   *
      *   **	**     **     ***    **  **    **   **	**   **    **  *
      *   **	**     **     ** *   **  **    **  **	 **	  **   *
      *   **	**     **     **  *  **  **    **  **	 **	**     *
      *   **	**     **     **   * **   **  **   **	 **   **       *
      *   **	**     **     **    ***    ****     **	**   **        *
      *    ******   ********  **     **     **	     ****    ********  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R I N T   I N V O I C E   P R O G R A M                 *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  January 2012 -   Include user number and name on the invoice  *
      * 		  header record to enable lookup of person     *
      * 		  who captured the Invoice/Credit note.        *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     OINV02.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2010
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
000080                 CURSOR IS CSTART
000090                 CONSOLE IS CRT.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY AUDIT.SL.

       COPY CARDEX.SL.

000150 COPY CONTROL.SL.

000170 COPY DBTRAN.SL.

000150 COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000250 COPY INVOIC.SL.

       COPY LEDTRF.SL.

000270 COPY PARAM.SL.

000290 COPY RECOVER.SL.

000310 COPY SALE.SL.

       COPY SHARED.SL.

000250 COPY SORDER.SL.

000330 COPY STOCK.SL.

000350 COPY TXTRAN.SL.

           SELECT CSHDRW  ASSIGN DISK
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.

000370     SELECT PRNREP  ASSIGN W02-PRINTER
000380                    ORGANIZATION LINE SEQUENTIAL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

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

000600 COPY SALE.IFD.

       COPY SHARED.FDE.

000540 COPY SORDER.FDE.

000620 COPY STOCK.FDE.

000640 COPY TXTRAN.FDE.

      *
      *    O L D   S T Y L E   C A S H   D R A W E R
      *
       FD  CSHDRW    LABEL RECORD STANDARD
                     VALUE OF FILE-ID "COM1".
       01  CSH-REC.
           03  CSH-OPEN     PIC  X(06).
000660 FD  PRNREP    LABEL RECORD OMITTED
000670               LINAGE 66.
000680 01  REP-L1.
000690     03  REP-DETAIL1     PIC X(80).
000700 01  REP-L2.
000710     03  REP-DETAIL2     PIC X(96).
000720 01  REP-L3.
000730     03  REP-AC          PIC X(10).
000740     03  REP-ACNO        PIC X(06).
           03  FILLER          PIC X(01).
000750     03  REP-NAME        PIC X(40).
	   03  FILLER	       PIC X(06).
000760     03  REP-DATE        PIC X(06).
000770	   03  REP-DTE	       PIC Z9/99/9999.
000790 01  REP-L4.
000800     03  REP-H1          PIC X(29).
000810     03  REP-REF         PIC 9(08)/99.
000860     03  FILLER          PIC X(40).
000870 01  REP-L5.
000880     03  REP-H2          PIC X(15).
000890     03  REP-H3          PIC X(47).
000900     03  REP-H4          PIC X(10).
000930     03  REP-H5          PIC X(08).
000940 01  REP-L6.
000950     03  REP-ITM1        PIC X(14).
           03  FILLER          PIC X(01).
000960     03  REP-DESC.
               05  FILLER      PIC X(31).
               05  REP-QTY     PIC Z(05)9.999 BLANK WHEN ZERO.
               05  FILLER      PIC X(04).
000970     03  REP-DSC REDEFINES REP-DESC.
000980         05  REP-ITM2    PIC X(14).
000990         05  FILLER      PIC X(01).
               05  REP-DSCR    PIC X(30).
001410     03  REP-QNT         PIC Z(05)9.999.
001420     03  FILLER          PIC X(10).
       01  CIV-L1.
           03  FILLER          PIC  X(22).
           03  CIV-CN.
               05  CIV-INV     PIC  X(16).
           03  FILLER          PIC  X(52).
       01  CIV-L2.
           03  FILLER          PIC  X(01).
           03  CIV-COMPANY     PIC  X(40).
           03  FILLER          PIC  X(49).
       01  CIV-L3.
           03  FILLER          PIC  X(02).
           03  CIV-H1          PIC  X(19).
           03  CIV-TEL         PIC  X(14).
           03  FILLER          PIC  X(01).
           03  CIV-RMKS.
               05  FILLER      PIC  X(14).
               05  CIV-CFWD    PIC  X(16).
           03  CIV-H2          PIC  X(10).
           03  CIV-H3          PIC  X(06).
           03  CIV-PAGE        PIC  ZZ9.
           03  FILLER          PIC  X(04).
       01  CIV-L4.
           03  FILLER          PIC  X(02).
           03  CIV-H4          PIC  X(19).
           03  CIV-VATREG      PIC  X(14).
           03  FILLER          PIC  X(41).
           03  CIV-H5.
               05  CIV-H5A     PIC  X(06).
               05  CIV-REF     PIC  X(08).
       01  CIV-L5.
           03  FILLER          PIC  X(02).
           03  CIV-NADD.       
               05  CIV-PC1     PIC  X(04).
               05  FILLER      PIC  X(36).
           03  FILLER          PIC  X(02).
000930     03  CIV-ADD.
000950         05  CIV-PC      PIC  X(04).
000960         05  FILLER      PIC  X(26).
	   03  FILLER	       PIC  X(04).
           03  CIV-DTE.
	       05  CIV-DATE    PIC  Z9/99/9999.
	   03  CIV-ACNO REDEFINES CIV-DTE.
	       05  FILLER      PIC  X(02).
               05  CIV-AC      PIC  X(06).
               05  FILLER      PIC  X(02).
           03  FILLER          PIC  X(02).
       01  CIV-L6.
           03  FILLER          PIC  X(02).
           03  CIV-ITM         PIC  X(14).
           03  FILLER          PIC  X(01).
001790     03  CIV-REMKS.
001810         05  CIV-DESC    PIC  X(26).
               05  FILLER      PIC  X(01).
               05  CIV-QNT     PIC  Z(05)9.99.
001820     03  CIV-DISCOUNT REDEFINES CIV-REMKS.
001840         05  CIV-DISC    PIC  X(19).
001850         05  CIV-DSC     PIC  Z9.99.
001860         05  FILLER      PIC  X(12).
           03  FILLER          PIC  X(01).
           03  CIV-PRICE       PIC  Z(07)9.99.
           03  FILLER          PIC  X(01).
           03  CIV-TAX         PIC  Z(06)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC  X(01).
           03  CIV-VAL         PIC  Z(08)9.99-.
       01  CIV-L7.
           03  FILLER          PIC  X(02).
           03  CIV-MES.
               05  CIV-SM      PIC  X(09).
               05  CIV-SMAN    PIC  X(34).
           03  FILLER          PIC  X(21).
           03  CIV-H6          PIC  X(11).
           03  CIV-TOT         PIC  Z(08)9.99-.
      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002100 77  WS-DISC	   PIC	99V99	 COMP-3.
002110 77  WS-TDISC        PIC  99V99    COMP-3.
002120 77  WS-CASH-AC      PIC  X(06).
002130 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
002180 77  WS-P1	   PIC	9(04)	 COMP-5.
002140 77  WS-S1           PIC  9(04)    COMP-5.
002150 77  WS-S2           PIC  9(04)    COMP-5.
002160 77  WS-S3           PIC  9(04)    COMP-5.
002170 77  WS-S4           PIC  9(04)    COMP-5.
002180 77  WS-S5           PIC  9(04)    COMP-5.
002190 77  WS-S6           PIC  9(04)    COMP-5.
002200 77  WS-S7           PIC  9(04)    COMP-5.
002210 77  WS-S8           PIC  9(04)    COMP-5.
002220 77  WS-S9           PIC  9(04)    COMP-5.
002230 77  WS-IXS          PIC  9(04)    COMP-5.
002240 77  WS-IXS1         PIC  9(04)    COMP-5.
002250 77  WS-IXS2         PIC  9(04)    COMP-5.
002260 77  WS-IXS3         PIC  9(04)    COMP-5.
002270 77  WS-IXS4         PIC  9(04)    COMP-5.
002280 77  WS-PARKEY       PIC  9(04)    COMP-5.
002300 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
002330 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
002340 77  WS-SALKEY       PIC  9(04)    COMP-5.
002350 77  WS-NETKEY       PIC  9(04)    COMP-5.
002360 77  WS-AUDKEY       PIC  9(06)    COMP-5.
002370 77  WS-RECKEY       PIC  9(06)    COMP-5.
002380 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01).
002450 77  WS-INV          PIC  9(01).  
           88  STD-INV     VALUE 1.
           88  COND-INV    VALUE 2.
           88  ALT-INV     VALUE 3.
002450 77  WS-GAR          PIC  9(01) VALUE 0.  
002400 77  WS-PRN1         PIC  9(01) VALUE 0.  
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002430 77  WS-START        PIC  9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2	   PIC	9(08).
002520 77  WS-PORT	   PIC	9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1	   PIC	9(05) VALUE 260.
002480 77  WS-OPEN	   PIC	X(01) VALUE X"01".
002490 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO          PIC  9(02) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST	   PIC	9(03) VALUE 0.
002540 77  WS-LAST	   PIC	9(03) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
002590 77  WS-OPTION	   PIC	X(01).
002600 77  WS-VAT-GST      PIC  X(01).
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002650 77  WS-ERROR        PIC  9(01).
002660 77  WS-SELECT       PIC  X(13) VALUE "Select Option".
002670 77  WS-CHAR1        PIC  X(01).
002680 77  WS-CHAR2        PIC  X(01).
002690 77  WS-COMP         PIC  9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002770 77  WS-INDEX-STORE  PIC  X(04).
002780 77  WS-INDEX-NAME   PIC  X(04).
002830 77  WS-BATCH        PIC  9(05).
002840 77  WS-CHK          PIC  X(33) VALUE
002850                          "ENTER if correct - N if incorrect".
002860 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002870 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002880 77  WS-PER          PIC  Z(02)9.99.
002890 77  WS-CR           PIC  X(01) VALUE "I".
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
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
002910 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
003090	       05  WS-TCR  PIC X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110	       05  FILLER      PIC  X(01).
003120	       05  WS-TOP-COMP PIC  X(40).
003130	       05  FILLER      PIC  X(39).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003060	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
003260         05  WS-SRT-LINE
003270                     PIC X(50).
003280         05  FILLER  PIC X(29).
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

003340 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       COPY LEDTRF.WS.

003380 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC S9(09)V99 COMP-3.
003410     03  W00-E-DESC  PIC  X(12).
003420     03  W00-A-DESC  PIC  X(12).
003430     03  W00-ACTION  PIC  9(01).
003440     03  W00-T-CODE  PIC  9(02)    COMP-5.
003450     03  W00-T-GL    PIC  9(06)    COMP-3.
003460     03  W00-T-DAY   PIC  9(06)    COMP-5.
003470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
003480     03  W00-T-MTD   PIC  9(06)    COMP-5.
003490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
003500     03  W00-T-YTD   PIC  9(06)    COMP-5.
003510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

003530 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

       COPY CARDEX.ID.

003570 COPY CONTROL.ID.

003720 COPY DBTRAN.ID.

003610 COPY DEBTOR.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

003800 COPY INVOIC.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

003820 COPY RECOVER.ID.

003710 COPY SALE.ID.

       COPY SHARED.ID.

003670 COPY SORDER.ID.

003730 COPY STOCK.ID.

003880 COPY TXTRAN.ID.

003770 01  W02-FILE-IDS.
003910     03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "OINV01.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.VAT.

       COPY W10.SAL.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

004410 01  W15-DISCOUNT.
004420     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

004520 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004570     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004580     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004590     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE 
               "% Settlement discount if paid in 30 DAYS".

       01  W32-MESSAGE.
	   03  W32-REMARKS OCCURS 10
			       PIC  X(63).
       01  W34-HEADER.
	   03  W34-FORMAT      PIC  X(01).
	   03  W34-MARGIN      PIC  9(02) COMP-5.
	   03  W34-SALUTATION  PIC  X(63).

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

005390 01  W60-NME-ADD.
005400     03  W60-NAME.
005410         05  W60-NCHAR OCCURS 40 PIC X(01).
005420     03  W60-ADDRESS.
005430         05  W60-ACHAR OCCURS 60 PIC X(01).
005440     03  W60-ADDRESS1    PIC X(60).

005620 COPY W70DEBT.IWS.

005690 01  W75-TITLES.
005700     03  W75-DESCRIP.
005710         05  FILLER      PIC X(28)      VALUE
005720                     "Mr     Mnr    Mrs    Mev    ".
005730         05  FILLER      PIC X(28)      VALUE
005740                     "Miss   Mej    Ms     Ms     ".
005750         05  FILLER      PIC X(28)      VALUE
005760                     "Dr     Dr     Rev    Ds     ".
005770         05  FILLER      PIC X(28)      VALUE
005780                     "Prof   Prof   The HonSy Edel".
005790         05  FILLER      PIC X(28)      VALUE
005800                     "                            ".
005810     03  W75-CODES REDEFINES W75-DESCRIP.
005820         05  W75-TITLE  OCCURS 10.
005830             07  W75-ETITLE PIC X(07).
005840             07  W75-ATITLE PIC X(07).
005860 01  W80-ACCOUNT.
005870     03  W80-ACNO        PIC X(06).
005880     03  W80-CRED REDEFINES W80-ACNO.
005890         05  W80-CNO     PIC 9(05).
005900         05  W80-CAC REDEFINES W80-CNO.
005910             07  W80-CTYPE PIC 9(01).
005920             07  W80-SER   PIC 9(04).
005930         05  FILLER        PIC 9(01).
005940     03  W80-TYPE        PIC 9(01).

       COPY W90.SAL.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).
006610     03  W95-ADD1        PIC X(30).
006620     03  W95-ADD2        PIC X(30).
006630     03  W95-ADD3        PIC X(30).
006640     03  W95-ADD4        PIC X(30).
006650     03  W95-PC.
006660         05  W95-POST    PIC 9(04).
006670     03  W95-TEL         PIC X(14).
006680     03  W95-V1.
006690         05  W95-T       PIC Z(04)9.99.
006700     03  W95-V2.
006710         05  W95-B       PIC Z(06)9.99-.
006720     03  W95-D.
006730         05  W95-DTE     PIC Z9/99/99.
006740 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
006750     03  W100-BAL        PIC Z(08)9.99-.
006760     03  W100-CUR        PIC Z(08)9.99-.
006770     03  W100-30         PIC Z(08)9.99-.
006780     03  W100-60         PIC Z(08)9.99-.
006790     03  W100-90         PIC Z(08)9.99-.
006800     03  W100-120        PIC Z(08)9.99-.
006800     03  W100-INT        PIC Z(08)9.99-.
006810     03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
006820     03  W100-V2.
006830         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006840 SCREEN SECTION.

       COPY BLANK.CRT.

007420 01  S02.
007430     03  LINE 2  COLUMN 27 FOREGROUND-COLOR 15
007440                           VALUE "SALES/ORDER PROGRAM CONTROL".

       COPY INVOICE.CRT.

       01  S30.
           03  LINE  8 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G9.
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
           03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G10.
007450     03  LINE  9 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 34 FOREGROUND-COLOR 7
                                 VALUE "CLIENT DETAILS".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 10 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Name    >".
           03          COLUMN 65 FOREGROUND-COLOR 7
                                 VALUE "<".     
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 11 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Address >".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 12 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 13 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 14 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 15 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 29 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 16 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Tel.    >".
           03          COLUMN 39 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 17 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Id.No.  >".
           03          COLUMN 39 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03  LINE 18 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G11.
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
           03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G12.
       01  S31.
           03  LINE 10 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NM1 AUTO.
           03  LINE 11 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD1 AUTO.
           03  LINE 12 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD2 AUTO.
           03  LINE 13 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD3 AUTO.
           03  LINE 14 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD4 AUTO.
           03  LINE 15 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC AUTO.
           03  LINE 16 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-HTEL AUTO.
           03  LINE 17 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-ID AUTO.
009930 01  S36.
009940     03  FOREGROUND-COLOR 11 PIC X(06) USING W80-ACNO AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY
				W40-COMPANY
				W90-TRAN.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270	     PERFORM ZA000-INIT.
010430	     GO TO BA00.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

010970 COPY DEBTOR.TRN.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011130 COPY AUDIT.RD.

011170 COPY CONTROL.RD.

011170 COPY DBTRAN.RD1.

011210 COPY DEBTOR.RD.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011250 COPY INVOIC.RD.

       COPY LEDTRF.RD.

011290 COPY PARAM.RD.

011310 COPY SALE.RD.

       COPY SHARED.RD.

011270 COPY SORDER.RD.

011330 COPY STOCK.RD.

011330 COPY TXTRAN.RD.

      /
011370 AD000-WRITE             SECTION.

011370 COPY AUDIT.WR.

       COPY CARDEX.WR.

011410 COPY CONTROL.WR.

011410 COPY DBTRAN.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011490 COPY INVOIC.WR.

       COPY LEDTRF.WR.

011410 COPY PARAM.WR.

011530 COPY SALE.WR.

       COPY SHARED.WR.

011270 COPY SORDER.WR.

011550 COPY STOCK.WR.

011570 COPY TXTRAN.WR.

011590*COPY CASHDRAW.OP4.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
011740 AY02.
011750       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011770       PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011780       MOVE DEB-RECORD1    TO REC-DEBTOR.
011790       GO TO AY50.
011800 AY04.
011810       MOVE 04             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011830       MOVE TRA-RECORD1    TO REC-DBTRAN.
011840       GO TO AY50.
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
011850 AY09.
011860       MOVE 09             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE WS-ACTION      TO REC-TYPE.
011900       MOVE GUA-RECORD1    TO REC-GARTEE.
011910       GO TO AY50.
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
011920 AY17.
011930       MOVE 17             TO REC-FILE.
011940       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
011950       MOVE SOR-REC1       TO REC-SORDER.
011960       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
012030 AY22.
012040       MOVE 22             TO REC-FILE.
012050       MOVE ZERO           TO REC-KEY.
012060       MOVE WS-ACTION      TO REC-TYPE.
012070       MOVE DOC-REC1       TO REC-INVOIC.
012080       GO TO AY50.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
012090 AY38.
012100       MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
012140       GO TO AY50.
012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
012200 AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
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
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012430       CLOSE RECOVER.
012440       OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
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
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
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
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
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
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
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
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
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
      *            NETWORK record 1.
      *
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
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
013140       MOVE WS-10CPI       TO REP-DETAIL2.
013150       WRITE REP-L2 BEFORE 0 LINES.
013160	     CLOSE RECOVER
013250		   PRNREP
		   SALE.
013250 AZ100.
013260       EXIT PROGRAM.
      *
      *    ****    Read debtor record - using account number.
      *
013720 CA155-GET-DEBTOR  SECTION 2.
013730 CA155.
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
013890 CA160-EXIT.
013900       EXIT.
      *
013910 CA165-NAME        SECTION 2.
013920 CA165.
013930       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013940                           INTO W40-SURNAME 
013950                                W40-FNAME 
013960                                W40-SNAME
013970       OVERFLOW
013980           MOVE 1          TO WS-ERROR.
013990 CA170-EXIT.
014000       EXIT.
      *
014010 CA175-ADDRESS     SECTION 2.
014020 CA175.
014030       UNSTRING W60-ADDRESS DELIMITED BY "*"
014040                           INTO W40-ADD1 
014050                                W40-ADD2 
014060                                W40-ADD3 
014070                                W40-ADD4
014080       OVERFLOW
014090           MOVE 1          TO WS-ERROR.
014100 CA175-EXIT.
014110       EXIT.
      *
014120 CA185-ADDRESS     SECTION 2.
014130 CA185.
014140       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014150                           INTO W40-PADD1 
014160                                W40-PADD2 
014170                                W40-PADD3 
014180                                W40-PADD4
014190       OVERFLOW
014200           MOVE 1          TO WS-ERROR.
014210 CA185-EXIT.
014220       EXIT.
      *
      *    ****   G E T   S O R D E R   R E C O R D   U S I N G   SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT ORDER RECORD (QUOTE OR INVOICE)
      *                    3 = ORDER DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
013270 GET-SORDER           SECTION.

013280 GET-SORD-ORDNO.
	     MOVE 1		 TO WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
013310       MOVE W10-SORD       TO SOR-REF W90-RF.
013420       MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 1            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF SOR-DOC NOT = "O"
               MOVE 2            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
             MOVE 1              TO WS-LAST WS-SALKEY.
             GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
                                        
       GET-SORD-TEST-TYPE.
      *                      H E A D E R   R E C O R D
           IF SOR-TYPE = 0
               MOVE SOR-AC       TO W80-ACNO DEB-ACNO
               PERFORM CA155-GET-DEBTOR
               IF WS-ERROR NOT = ZERO
                   MOVE 4        TO WS-ERROR
                   GO TO GET-SORDER-EXIT
               END-IF
               MOVE SOR-OREF     TO W90-REG
               MOVE SOR-NAME     TO W40-NAME W40-NM1
               MOVE SOR-PICNO    TO W90-PICK
               MOVE SOR-ORD      TO W90-ORD
               MOVE SOR-SMAN     TO W10-SMAN
               MOVE SOR-EXP      TO W90-EXP
               MOVE SOR-PRC      TO WS-PRC
               MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL 
           ELSE
      *                      I T E M   D E T A I L S
           IF SOR-TYPE = 1
	       INITIALIZE SAL-RECORD1
	       MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
               MOVE SOR-DESC     TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
               MOVE SOR-COST     TO SAL-COST
               MOVE SOR-SELL     TO SAL-SELL
               MOVE SOR-VAT      TO SAL-VAT
	       MOVE SOR-VAT-SUB  TO SAL-VAT-SUB
               MOVE SOR-VAL      TO SAL-VAL
               MOVE SOR-INCTAX   TO SAL-INCTAX
               MOVE SOR-NONTAX   TO SAL-NONTAX
               MOVE SOR-TAXVAL   TO SAL-TAXVAL
               MOVE SOR-DSC      TO SAL-DSC
               MOVE SOR-DISC     TO SAL-DISC
	       MOVE SOR-PACK	 TO SAL-PACK
	       MOVE SOR-SQN	 TO SAL-SQN
	       IF SAL-VAT-SUB > ZERO
		   ADD SOR-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
023060		   MOVE W05-VAT(SAL-VAT-SUB)
023060				 TO W05-RATE
023080		   COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED =
			  (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
	       END-IF
	       PERFORM GET-SORD-WRITE-SALE
	   ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
	       MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
	       MOVE SOR-DESC	 TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
	       MOVE SOR-SERNO	 TO SAL-SERNO
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               INITIALIZE SAL-RECORD1
               MOVE SOR-SERIAL   TO SAL-MSER
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF SOR-TYPE = 4
               GO TO GET-SORD-NEXT
015160     ELSE
      *                      A D D R E S S 
015170     IF SOR-TYPE = 5
               MOVE SOR-AD1      TO W40-AD1
               MOVE SOR-AD2      TO W40-AD2
               MOVE SOR-AD3      TO W40-AD3
               MOVE SOR-AD4      TO W40-AD4
               MOVE SOR-PC       TO W40-PC
	   ELSE
      * 		     S A L U T A T I O N
015170	   IF SOR-TYPE = 7
	       MOVE SOR-TEXT	 TO W34-SALUTATION
	   ELSE
      * 		     T E X T
015170	   IF SOR-TYPE = 8
	       MOVE SOR-TEXT	 TO W32-REMARKS (WS-S7)
	       ADD 1		 TO WS-S7
	   ELSE
      *                      T R A I L E R
	       MOVE SOR-SUBTOT	 TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT    
                                 GIVING W90-TDISC
               MOVE W90-TDISC    TO W90-LDISC
               MOVE SOR-NON-TXBL TO W90-NONTAX
	       MOVE SOR-TAXABLE  TO W90-EXCTOT
               MOVE SOR-TAX      TO W90-TTAX
               MOVE SOR-TOT      TO W90-TOTAL
               MOVE SOR-BAL      TO W90-BAL
	       MOVE SOR-INSTRUCT TO W90-INSTRUCT
               MOVE SOR-PMETH    TO W90-PMETH
               MOVE SOR-STATUS   TO W40-STATUS
               MOVE SOR-HTEL     TO W40-HTEL
               MOVE SOR-ID       TO W40-ID
	       GO TO GET-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-WRITE-SALE.
             MOVE WS-LAST        TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST WS-SALKEY.
       
013470 GET-SORDER-EXIT.
013480       EXIT.

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
015060       MOVE W90-DISC       TO TRA-DISC.
015090       MOVE W20-DTE        TO TRA-DATE.
015100       MOVE W20-DTE        TO DEB-ACTIVE.
015110       MOVE W90-CODE       TO TRA-CODE.
015120       MOVE W90-REF        TO TRA-REF.
             MOVE W90-REG        TO TRA-REG.
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
015290       PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000900       MOVE 1              TO WS-ACTION.
000220       PERFORM AY04 THRU AY59.
015300       MOVE 1              TO TAX-ACTYPE.
015310       MOVE TRA-CODE       TO TAX-CODE.
015320       MOVE TRA-DATE       TO TAX-DATE.
015330       MOVE TRA-REF        TO TAX-REF.
015340       MOVE TRA-VALUE      TO TAX-VALUE.
015350       MOVE TRA-TAXFREE    TO TAX-TAXFREE.
015370	     MOVE TRA-AC	 TO TAX-AC.
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
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE. 
015480       PERFORM UPDATE-TRAN-RECORD.
015490     IF W90-CR NOT = ZERO
015500         GO TO DEBTOR-CR-TRANS.
015510	     ADD W90-DB 	 TO DEB-DEBT
015520                              DEB-BAL
015530                              DEB-PURYTD
                                    DEB-MTHPUR (W20-MONTH).
015540	     GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
	     MOVE "  DT"	 TO WX-LEDG.
	     MOVE W90-CODE	 TO WX-TRN.
             MOVE W00-E-DESC     TO WX-DESC.
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
015570	     SUBTRACT W90-CR	 FROM DEB-CRED
				      DEB-MTHPUR(W20-MONTH).
015580       ADD W90-CR          TO DEB-PURYTD.               
015590       ADD W90-CR          TO DEB-BAL.
	   IF OPEN-ITEM
	       GO TO DEBTOR-TRAN-EXIT.
015600	   IF WS-AGE = "M"
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
016060 BA000			SECTION 50.
016070 BA00.
016790	     MOVE WS-8LPI	 TO CIV-L1.
016800	     WRITE CIV-L1 BEFORE 0 LINES.
	     PERFORM BC000.
016820	     MOVE WS-6LPI	 TO CIV-L1.
016830	     WRITE CIV-L1 BEFORE 0 LINES.
	     GO TO AZ000-EOJ.
016840 BA80.
016850       MOVE W70-AUDIT      TO WS-AUDKEY.
016860       MOVE LOW-VALUES     TO AUD-REC1.
016870       PERFORM AY16 THRU AY59.
016880       MOVE STK-CODE       TO AUD-CODE.
016890       MOVE 2 TO AUD-TYPE.
016900       MOVE ZERO           TO AUD-SUB.
016910       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016920       ADD 1               TO W70-AUDIT.
016930       MOVE 1              TO WS-AUDKEY.
016940       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
016950       PERFORM AY16 THRU AY59.
016960       MOVE 1              TO AUD-REP2.
016970       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016980 BA90.
016990       MOVE W70-AUDIT      TO WS-AUDKEY.
017000       MOVE LOW-VALUES     TO AUD-REC1.
017010       PERFORM AY16 THRU AY59.
017050       MOVE 7              TO AUD-TYPE.
017060       MOVE WS-DRAW        TO AUD-SUB.
017070       MOVE W90-REF        TO AUD-CSH.
017080       MOVE W90-EXCTOT     TO AUD-SELL.
017090       MOVE W90-TCOST      TO AUD-COST.
017100       MOVE W10-SMAN       TO AUD-SMAN.
017110       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017120       ADD 1               TO W70-AUDIT.
017130       MOVE 1              TO WS-AUDKEY.
017140       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017150       PERFORM AY16 THRU AY59.
017190       MOVE 1              TO AUD-REP7.
017200       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017210 BA95.
017220       MOVE W70-AUDIT      TO WS-AUDKEY.
017230       MOVE LOW-VALUES     TO AUD-REC1.
017240       PERFORM AY16 THRU AY59.
017290       MOVE 3              TO AUD-TYPE.
017300       MOVE DEB-ACNO       TO AUD-AC.
017310       MOVE WS-DRAW        TO AUD-SUB.
017320       MOVE W90-REF        TO AUD-REF.
017330       MOVE W90-TTAX       TO AUD-TAX.
017340       MOVE W90-TOTAL      TO AUD-VAL.
017350       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017360       ADD 1               TO W70-AUDIT.
017370       MOVE 1              TO WS-AUDKEY.
017380       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017390       PERFORM AY16 THRU AY59.
017430       MOVE 1              TO AUD-REP3.
017440       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017450 BA100.
017460       MOVE W70-AUDIT      TO WS-AUDKEY.
017470       MOVE LOW-VALUES     TO AUD-REC1.
017480       PERFORM AY16 THRU AY59.
017520       MOVE 8              TO AUD-TYPE.
017530       MOVE WS-DRAW        TO AUD-SUB.
017540       MOVE W10-SMAN       TO AUD-SMAN.
017550       MOVE W90-REF        TO AUD-CSH.
017560       MOVE W90-VALUE      TO AUD-SEL.
017570       MOVE W90-ICST       TO AUD-CST.
017580	     MOVE STK-CODE	 TO AUD-EXT-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
017590       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017600       ADD 1               TO W70-AUDIT.
017610       MOVE 1              TO WS-AUDKEY.
017620       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017630       PERFORM AY16 THRU AY59.
017670       MOVE 1              TO AUD-REP8.
017680       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
      /
017690 BB050		   SECTION 50.
017700 BB50.
017710       MOVE SPACES         TO REP-DETAIL2.
       BB56.
             MOVE WS-EXPP        TO CIV-L1.
             WRITE CIV-L1 BEFORE 0 LINES.
             MOVE SPACES         TO CIV-L1.
017750       MOVE W95-COMP       TO CIV-COMPANY.
             WRITE CIV-L1 BEFORE 1 LINES.
             MOVE WS-ECAN        TO CIV-L1.
             WRITE CIV-L1 BEFORE 0 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE "Telephone/Telefoon"
                                 TO CIV-H1.
             MOVE W95-TEL        TO CIV-TEL.    
             MOVE W95-ADD1       TO CIV-ADD.
             WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
           IF W05-VATNO NOT = SPACES
               MOVE "VAT/BTW Reg No/Nr."
                                 TO CIV-H4
               MOVE W05-VATNO    TO CIV-VATREG.
             MOVE W95-ADD2       TO CIV-ADD.
             WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE W95-ADD3       TO CIV-ADD.
             WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE W95-ADD4       TO CIV-ADD.
             WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
017950 BB60.
017960       EXIT.
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
             MOVE W90-REF        TO GUA-INV.
             MOVE W20-DTE        TO GUA-DATE2.
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
      /
      *    ****    P R O D U C E   I N V O I C E
      *
018500 BC000	     SECTION 50.
       BC00.
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
			  BACKGROUND-COLOR 5.
018660	     MOVE ZERO		 TO W90-BDT W90-LDISC W90-NONTAX
018670				    W90-BAL W90-SALES W90-EXCTOT
				    W90-CUR W90-PMETH W40-STATUS
				    W90-CR  W90-DB.
018680       MOVE 1              TO WS-FIRST WS-LAST.
018690	     MOVE SPACES	 TO W90-TAX W90-INSTRUCT.
	     MOVE 1		 TO WS-S9.

       BC08.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC08.
	     MOVE LS-SLS-ORDNO	 TO W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
               DISPLAY "Database corrupted - Press ANY KEY" AT 2502 
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       ACCEPT WS-OPTION AT 2537
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO
               GO TO BC599.
             MOVE W10-SORD       TO W90-REG.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
018680       MOVE 1              TO WS-FIRST.
018820       DISPLAY S36 AT 0308
019040       DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR 11.
019900       DISPLAY W90-ORD AT 0368 WITH FOREGROUND-COLOR 11.
019970       DISPLAY W10-SMAN AT 0411 WITH FOREGROUND-COLOR 11.
020010     IF W10-SMAN NOT = ZERO
020030         ADD 100 W10-SMAN    GIVING WS-PARKEY
020040         PERFORM READ-PARAM THRU READ-PARAM-EXIT
020100         DISPLAY PAR-SALESMAN AT 0420 WITH FOREGROUND-COLOR 11
               MOVE PAR-SALESMAN TO W40-ASSIST
           ELSE
               MOVE SPACES       TO W40-ASSIST.
	     DISPLAY W40-CONTACT AT 0529
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020760       DISPLAY W90-REG AT 0554 WITH FOREGROUND-COLOR 11.
             MOVE 1              TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
       BC15.
022610       MOVE W90-DB         TO W90-TOTAL.
022620       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022630       MOVE W90-DB         TO W100-BAL.
022640       DISPLAY W100-BAL AT 2015 WITH FOREGROUND-COLOR 11.
022650       MULTIPLY -1 BY W90-LDISC 
022660                           GIVING W100-BAL.
022670       DISPLAY W100-BAL AT 2115 WITH FOREGROUND-COLOR 11.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
022690	   IF W90-TAX = SPACES
022700         GO TO BC20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC18.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC18.
022740	     MOVE W25-TOTAL	 TO W100-BAL.
022760	     ADD W25-TOTAL	 TO W90-TOTAL.
022750	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022770 BC20.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790       DISPLAY W100-BAL AT 2315 WITH FOREGROUND-COLOR 11.
             GO TO BC185.
023880 BC115.
036820	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
023900	     ADD 1		 TO LIN.
025130 BC185.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *
025140	     DISPLAY CLEAR-L25.
025150       MOVE "M" TO WS-AGE.
025310 BC186.
025400	     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
025410 BC188.
025420     IF DEB-TERMS NOT = "3"
025430         DISPLAY "Processing Invoice" AT 2520 WITH
025440			BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
025450     ELSE
025460         DISPLAY "Processing COD sale" AT 2520 WITH
025470			BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
025480       MOVE 1              TO WS-SALKEY.
025490       PERFORM AY60 THRU AY999.
025500       PERFORM AY02 THRU AY59.
025510       MOVE W70-INVOICE    TO W90-REF.
025520       DISPLAY W90-REF AT 0510 WITH FOREGROUND-COLOR 11.
025530     IF W70-INVOICE = 99999999
025540         MOVE 1            TO W70-INVOICE
025550     ELSE
025560         ADD 1             TO W70-INVOICE.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
025570     IF W80-TYPE NOT = 1
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
025740 BC195.
      *
      *    ****    F I R S T   I N I T I A L
      *
025750       MOVE 1              TO WS-S5.
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
025880 BC205.
      *
      *    ****    S U R N A M E
      *
025890       MOVE W40-NCHAR (WS-S5) 
025900                           TO W60-NCHAR (WS-S6).
025910     IF WS-S5 < 24
025920         ADD 1             TO WS-S5
025930         ADD 1             TO WS-S6
025940         GO TO BC205.
025950       MOVE W60-NAME       TO W40-NAME.
025960 BC210.
025970       MOVE ZERO           TO WS-PAGE.
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
038660	       DISPLAY "Processing Invoice" AT 2520 WITH
038670			BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
026310 BC215.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
026320       MOVE SPACES         TO REP-DETAIL2.
026330	     MOVE W40-NAME	 TO CIV-NADD.
             MOVE W95-PC         TO CIV-PC.
             MOVE "Ref :"        TO CIV-H5A.
026590       MOVE W90-REF        TO CIV-REF.
026340       WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
026350       MOVE W40-ADD1       TO CIV-NADD.
026370       WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
026380       MOVE W40-ADD2       TO CIV-NADD.
026480       ADD 1               TO WS-PAGE.
             MOVE "Page:"        TO CIV-H3.
             MOVE WS-PAGE        TO CIV-PAGE.
026400       WRITE CIV-L1 BEFORE 1 LINES.
           IF W05-VATNO NOT = SPACES
               MOVE WS-EXPP      TO CIV-L1
               WRITE CIV-L1 BEFORE 0 LINES
               MOVE SPACES       TO CIV-L1
               MOVE " TAX/BELASTING"
                                 TO CIV-INV
               WRITE CIV-L1 BEFORE 0 LINES
               MOVE WS-ECAN      TO CIV-L1
               WRITE CIV-L1 BEFORE 0 LINES.
             MOVE SPACES         TO CIV-L1.
026410       MOVE W40-ADD3       TO CIV-NADD.
026430       WRITE CIV-L1 BEFORE 1 LINES.
             MOVE WS-EXPP        TO CIV-L1.
             WRITE CIV-L1 BEFORE 0 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE "INVOICE/FAKTUUR"
                                 TO CIV-INV.
             WRITE CIV-L1 BEFORE 0 LINES.
             MOVE WS-ECAN        TO CIV-L1.
             WRITE CIV-L1 BEFORE 0 LINES.
             MOVE SPACES         TO CIV-L1.
026440       MOVE W40-ADD4       TO CIV-NADD.
026670       MOVE W12-TODAY      TO CIV-DATE.
026460       WRITE CIV-L1 BEFORE 1 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE W40-PC1        TO CIV-PC1.
026550       MOVE W80-ACNO       TO CIV-AC.
             WRITE CIV-L1 BEFORE 2 LINES.
             MOVE SPACES         TO CIV-L1.
             MOVE "  VAT/BTW"    TO CIV-H2.
             WRITE CIV-L1 BEFORE 2 LINES.
             MOVE SPACES         TO CIV-L1.
026810       MOVE 19             TO WS-LINE.
       BC220.
             EXIT.

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
028820       MOVE W90-TOTAL      TO DOC-VALUE.
	     MOVE LS0-NO	 TO DOC-USER.
	     MOVE LS0-NAME	 TO DOC-UNME.
      *
      *    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
      *
       BC225.
             PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
015400       MOVE 1              TO WS-ACTION.
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
026910     IF SAL-ITEM = SPACES
026920         GO TO BC247.
026930       MOVE SAL-ITEM       TO STK-CODE.
026940       PERFORM AY05 THRU AY59.
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
	     MOVE SPACES	 TO CRD-WHS.
	     MOVE STK-LDG	 TO CRD-DEPT.
026960	     MOVE 1		 TO DOC-TYPE.
             INITIALIZE DOC-DET2.
026970	     MOVE SAL-EXT-ITEM	 TO DOC-EXT-ITEM CRD-EXT-CODE.
026980	     MOVE SAL-DESC	 TO DOC-DESC.
026990	     MOVE SAL-QNT	 TO W10-QUANT DOC-QNT.
	     MOVE SAL-PACK	 TO WS-P1.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
	     MOVE W10-QNT	 TO CRD-QNT.
040230       MOVE SAL-COST       TO W10-COST  DOC-COST.
	     COMPUTE CRD-COST = SAL-COST / STK-UNT(WS-P1).
027020	     MOVE SAL-VAT	 TO DOC-VAT.
027030       MOVE SAL-SELL       TO W10-SELL  DOC-SELL.
027050       MOVE SAL-VAL        TO W25-VALUE DOC-VAL.
026970	     MOVE SAL-EXT-ITEM	 TO CIV-ITM.
026980	     MOVE SAL-DESC	 TO CIV-DESC.
026990	     MOVE SAL-QNT	 TO CIV-QNT.
027020	     MOVE SAL-VAT	 TO CIV-TAX .
027030	     MOVE SAL-SELL	 TO CIV-PRICE.
027050	     MOVE SAL-VAL	 TO CIV-VAL.
027070	     MOVE SAL-DISC	 TO W90-CDISC W90-EXCDIS.
027080       MOVE SAL-DSC        TO WS-DISC.
027090       SUBTRACT W90-CDISC FROM W25-VALUE
027100                           GIVING W90-VALUE.
	     MOVE SAL-TAXVAL	 TO DOC-TAXVAL.
             MOVE SAL-NONTAX     TO DOC-NONTAX.
             MOVE SAL-SERNO      TO W10-SERNO DOC-SERNO.
             MOVE SAL-SUPER      TO WS-SUPER.
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
027160		   COMPUTE W90-VALUE ROUNDED = W90-VALUE -
027170                 (((W90-VALUE / (W05-RATE + 100.00000)) * 
027180			  W05-RATE))
027190		   COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
027200                 (((W90-CDISC / (W05-RATE + 100.00000)) *
027210			  W05-RATE)).
027220*       ADD W90-VALUE	  TO W90-EXCTOT.
	     COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
	   IF WS-CARDEX = "Y"
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59.
027230	     WRITE CIV-L1 BEFORE 1 LINES.
027240	     MOVE SPACES	 TO REP-DETAIL2.
027250       ADD W25-VALUE       TO W90-STOTAL.
      *
      *    ****    H A S   L I N E   D I S C O U N T   B E E N   
      *            A P P L I E D
      *
027260     IF W90-CDISC = ZERO
027270         GO TO BC240.
027280     IF WS-LINE > 1
027290         SUBTRACT 1        FROM WS-LINE
027300     ELSE
027310         PERFORM BC250.
      *
      *    ****    P R I N T   A N D   L O G   T H E   L I N E
      *            D I S C O U N T
      *
027360       MOVE WS-DISC        TO DOC-DSC.
027370       MULTIPLY -1         BY W90-CDISC 
027380                           GIVING DOC-DISC.
027340	     MOVE "DISCOUNT/AFSLAG %"
027350				 TO CIV-DISC.
027360	     MOVE WS-DISC	 TO CIV-DSC.
027380	     MOVE DOC-DISC	 TO CIV-PRICE.
027400	     WRITE CIV-L1 BEFORE 1 LINES.
027410	     MOVE SPACES	 TO REP-DETAIL2.
027420 BC240.
       BC242.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
027430	     MOVE STK-LDG	 TO DPT-CODE.
027440	     PERFORM AY38 THRU AY59.
027450       ADD W90-VALUE       TO DPT-SALES-DAY
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
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
027480       COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
027490       ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
027500                              DPT-COST-YTD.
           IF (WX-INT = "Y") AND (W90-BCR NOT = ZERO)
	       MOVE W90-BCR	 TO WX-VALUE
               MOVE DPT-COST-GL  TO WX-AC
               MOVE 08           TO WX-TYPE
               PERFORM GL-TRF-UPDATE
               COMPUTE WX-VALUE = W90-BCR * -1
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 19           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
027510       ADD W90-EXCDIS      TO DPT-DISC-DAY
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
027600	       GO TO BC245.
027610	     SUBTRACT W10-QNT	 FROM STK-QPIC.
	   IF STK-USE-CASES = "Y"
	       SUBTRACT SAL-CASES
				 FROM STK-CASES.
027590	   IF STK-IND = 1
027600         GO TO BC245.
027620	   IF STK-DISC > ZERO
027630         GO TO BC245.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
027640     IF STK-QUANT NOT > STK-LEVEL
027650         MOVE 1            TO STK-DISC
027660         PERFORM BA80.
027670 BC245.
027680       ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
027690	     ADD W10-QNT	 TO STK-MTD STK-YTD.
027700	     ADD W90-VALUE	 TO STK-MTDV W90-SALES STK-YTDV.
027710	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
      *            A U D I T   F I L E
      *
027720       MOVE W90-BCR        TO W90-ICST.
027730       PERFORM BA100.
           IF W10-SERNO NOT = SPACES
               MOVE ZERO         TO WS-GAR
               PERFORM BB070.
             ADD 1               TO DOC-SEQ.
             MOVE ZERO           TO DOC-SUB.
             PERFORM BC225.
      *
      *    ****    G O   A N D   G E T   T H E   N E X T   I T E M
      *
027740 BC246.
027750       ADD 1               TO WS-SALKEY.
027760       GO TO BC235.
027770 BC247.
	     GO TO BC246.
027790*      MOVE 3		 TO DOC-TYPE.
      *      INITIALIZE DOC-DET2.
      *
      *     C H E C K   F O R   S E R I A L   N U M B E R S
      *
      *    IF SAL-SUB NOT = ZERO
      *        MOVE 1		 TO WS-S1
      *        PERFORM BC248
      *        MOVE "SERIAL NO:" TO INV-HD
      *        MOVE SAL-MSER	 TO INV-SERNOS DOC-DESC
      *        ADD 1		 TO DOC-SUB
      *    ELSE
      *        MOVE ZERO	 TO DOC-SUB
      *        ADD 1		 TO DOC-SEQ
027800*        MOVE SAL-DESC	 TO INV-RMKS DOC-DESC.
027810*      WRITE INV-L1 BEFORE 1 LINES.
027820*      MOVE SPACES	 TO REP-DETAIL2.
      *      PERFORM BC225.
027830*      GO TO BC246.
       BC248.
             MOVE SAL-SNO (WS-S1)
                                 TO W10-SERNO.
             MOVE ZERO           TO WS-GAR.
             PERFORM BB070.
             ADD 1               TO WS-S1.
	   IF WS-S1 < 3
               IF SAL-SNO (WS-S1) NOT = SPACES
                   GO TO BC248.
027840 BC250.
      *
      *    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
      *
027850	     WRITE CIV-L1 BEFORE 1 LINES
027860	     MOVE SPACES	 TO CIV-L1.
027870	     MOVE "C/FWD-OORGEDRA"
027880				 TO CIV-CFWD.
027890	     MOVE W90-STOTAL	 TO CIV-VAL.
027900	     WRITE CIV-L1 BEFORE 14 LINES.
027940       PERFORM BB050.
027950       PERFORM BC215 THRU BC220.
027970	     MOVE "B/FWD - OORGEBRING"
				 TO CIV-DESC.
027980	     MOVE W90-STOTAL	 TO CIV-VAL.
028000	     WRITE CIV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO CIV-L1.
027990       SUBTRACT 2          FROM WS-LINE.
028010 BC255.
      *
      *    ****    I N V O I C E   T O T A L S
      *
	     WRITE CIV-L1 BEFORE WS-LINE LINES.
	     MOVE SPACES	 TO CIV-L1.
	     WRITE CIV-L1 BEFORE 1 LINES.
      *
      *    ****    P R I N T   S U B - T O T A L
      *
028050	     MOVE "SUB TOTAL"	 TO CIV-H2.
	     MOVE W90-DB	 TO CIV-TOT.
028720	   IF DEB-TERMS = "0"
028730	       MOVE "Terms - 7 Days"
                                 TO CIV-MES 
028740	   ELSE
028750	   IF DEB-TERMS = "2"
028760	       MOVE "Terms - 60 Days"
                                 TO CIV-MES
028770	   ELSE
028780	   IF DEB-TERMS = "1"
028790	       MOVE "Terms - 30 Days"
                                 TO CIV-MES
028800	   ELSE
	   IF DEB-TERMS = "3"
028810	       MOVE "Terms - COD"
				 TO CIV-MES.
	     WRITE CIV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO CIV-L1.
	   IF W10-SMAN NOT = ZERO
	       MOVE "Assist :"	 TO CIV-SM
	       MOVE W40-ASSIST	 TO CIV-SMAN.
	     WRITE CIV-L1 BEFORE 1 LINES.
       BC256.
	     MOVE 9		 TO DOC-TYPE.
             MOVE 99900          TO DOC-SQN.
             INITIALIZE DOC-DET3.
028070       MOVE W90-DB         TO DOC-SUBTOT.
028090       MOVE SPACES         TO REP-DETAIL2.
      *
      *    ****    P R I N T   D I S C O U N T   A N D   U P D A T E
      *            T H E   S U N D R Y   D I S C O U N T   T O T A L S
      *
028150     IF W90-TDISC NOT = ZERO
028180         MULTIPLY -1 BY W90-TDISC 
028190                           GIVING DOC-DSCNT
028160	       MOVE "DISCOUNT"	 TO CIV-H2
	       MOVE DOC-DSCNT	 TO CIV-TOT
028200	       IF W90-LDISC = ZERO
                   IF W90-ORG = "I" 
028210		       MOVE "YYYY"
				 TO DPT-CODE
028220		       PERFORM AY38 THRU AY59
028230                 ADD W90-TDISC
                                 TO DPT-DISC-DAY
028240                              DPT-DISC-MTD
028250                              DPT-DISC-YTD
                       IF WX-INT = "Y"
                           MOVE W90-TDISC    TO WX-VALUE
                           MOVE DPT-DISC-GL  TO WX-AC
                           MOVE 15           TO WX-TYPE
                           PERFORM GL-TRF-UPDATE
                       END-IF
028260		       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
028270	     WRITE CIV-L1 BEFORE 1 LINES.
028280       MOVE SPACES         TO REP-DETAIL2.
      *
      *    ****    P R I N T   T H E   T A X   F I G U R E S   A N D 
      *            U P D A T E   T A X   T O T A L S
      *
028290	   IF (W90-TAX NOT = SPACES) OR
027120        (W90-EXP = "X")
028300	       MOVE "TAX / BTW"  TO CIV-H2
028310	   ELSE
028320     IF W05-VATNO NOT = SPACES
028330	       MOVE "Vat inclusive / BTW ingesluit"
028340				 TO CIV-RMKS.
      *
      * 	   TAX CALCULATED IN SLS003
      * 	       W90-NONTAX   =TOTAL NON TAXABLE VALUE
      * 	       W90-TTAX     =TOTAL TAX (ALL RATES)
      * 	       W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
      * 	       W90-VAT(?)   =TOTAL VAT PER RATE
      * 	       W90-TOTAL    =TOTAL FOR CREDIT NOTE
      *
028660	     MOVE W05-RATE	 TO WS-PER.
028690 BC260.
028680	     MOVE W90-TTAX	 TO DOC-TAX.
028670	   IF W90-TAX NOT = SPACES
028680	       MOVE W90-TTAX	   TO CIV-TOT.
028700	     WRITE CIV-L1 BEFORE 2 LINES.
028710       MOVE SPACES         TO REP-DETAIL2.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
             MOVE DEB-TERMS      TO DOC-TERMS.
      *
      *    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
      *
028820       MOVE W90-TOTAL      TO W90-DB.
028830       MOVE W90-BDT        TO W90-TCOST.
028840       MOVE W90-DB         TO DOC-TOT.
	     MOVE W90-EXCTOT	 TO DOC-TAXABLE.
             MOVE W90-NONTAX     TO DOC-NON-TXBL.
             MOVE W90-BAL        TO DOC-BAL.
	     MOVE W40-CONTACT	 TO DOC-CONTACT.
	     MOVE W40-TEL	 TO DOC-TEL.
	     PERFORM BC225.
028840	     MOVE W90-DB	 TO CIV-TOT.
028880	     WRITE CIV-L1 BEFORE 2 LINES.
028890       MOVE SPACES         TO REP-DETAIL2.
028900       MOVE W12-T-YMD      TO W20-DTE.
      *
      *    ****    U P D A T E   T H E   D E B T O R   A C C O U N T
      *
028910       MOVE 1              TO W90-CODE.
028920       PERFORM GET-DEBTOR-TRAN-RECORD.
028930       PERFORM GENERATE-DEBTOR-TRAN.
	   IF DEB-TERMS NOT = 3
	       MOVE "NEW BALANCE / NU"
				 TO CIV-CFWD
	       MOVE "WE SALDO"	 TO CIV-H2
	       MOVE DEB-BAL	 TO CIV-TOT.
	     WRITE CIV-L1 BEFORE 6 LINES.
	     MOVE SPACES	 TO CIV-L1.
014480	     MOVE 9		 TO WS-PARKEY.
014500       PERFORM AY01 THRU AY59.
	     MOVE 1		 TO W05-V-RATE.

       BC262-VAT.
	   IF W90-VAT(W05-V-RATE) = ZERO
	       GO TO BC262-CHECK-END.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
027840	     PERFORM AY38 THRU AY59.
      *
      *    ****    V A T   S T A T I S T I C S
      *
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
      *
      *    ****    L O G   T H E   S A L E S M A N   &   I N V O I C E -
      *            A U D I T   F I L E   D E T A I L S
      *
029150       PERFORM BA90.
029160       PERFORM BA95.
      *
      *    ****    P R I N T   T H E   N E X T   I N V O I C E
      *            H E A D I N G 
      *            ( OVERLAP WITH DISK PROCESSING )
      * 
029170       PERFORM BB050.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C A T E G O R Y
      *
029180       ADD 121 DEB-CAT     GIVING W90-RESULT.
029190       DIVIDE W90-RESULT BY 2
029200                           GIVING W90-CATKEY
029210                           REMAINDER W90-CS.
029220       ADD 1               TO W90-CS.
029230       MOVE W90-CATKEY     TO WS-PARKEY.
029240       PERFORM AY01 THRU AY59.
029250       ADD W90-DB          TO PAR-CDRMTD (W90-CS).
029260       ADD W90-DB          TO PAR-CDRYTD (W90-CS).
029270	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    U P D A T E   T H E   S A L E S M A N   R E C O R D
      *
029280     IF W10-SMAN NOT = ZERO
029290         ADD 100 W10-SMAN  GIVING WS-PARKEY
029300         PERFORM AY01 THRU AY59
029310         ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
029320         ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
029330	       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    R E W R I T E   T H E   D E B T O R   R E C O R D
      *
029340       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C O N T R O L
      *            R E C O R D
      *
029350       ADD W90-DB          TO W70-OUT
029360                              W70-DT.
029370       ADD W90-CUR         TO W70-MTD.
029380       ADD W90-30          TO W70-ONE.
029390       ADD W90-60          TO W70-TWO.
029400       ADD W90-90          TO W70-THREE.
029410       ADD W90-120         TO W70-FOUR.
      *
      *    ****    U P D A T E   T H E   S T O C K   C O N T R O L
      *            R E C O R D
      *
029440       SUBTRACT W90-BAL    FROM STK-TOTAL.
029450       ADD W90-SALES       TO STK-SMTD STK-SYTD.
029460 BC266.
      *
      *    ****    U P D A T E   T H E   C O S T - O F - S A L E S
      *
029470       MOVE 8              TO WS-PARKEY.
029480       PERFORM AY01 THRU AY59.
029490       ADD W90-BDT         TO PAR-CSHT PAR-CMTD PAR-CYTD.
029500       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
029510 BC267.
      *
      *    ****    U P D A T E   T H E   O R D E R   F I L E
      *
             MOVE W10-SORD       TO SOR-REF.
             MOVE 100            TO SOR-SQN.
	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             PERFORM AY17 THRU AY59.
             MOVE "I"            TO SOR-DOC.
	     PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE 99900          TO SOR-SQN.
	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             PERFORM AY17 THRU AY59.
             MOVE W90-REF        TO SOR-INVOICE.
	     PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
029520       PERFORM AY70 THRU AY999.
      *
      *    ****    C L O S E   F I L E S   
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *
029530	     CLOSE PARAM
029540             DEPART 
029550             DEBTOR 
029560             AUDIT 
029570             STOCK 
029580             INVOICE 
029590		   SALE.
029600       OPEN OUTPUT SALE.
029610       CLOSE SALE.
029620       OPEN I-O PARAM 
029630                DEPART 
029640                DEBTOR 
029650                AUDIT 
029660                STOCK 
029670                INVOICE 
029680                SALE.
029690	     DISPLAY CLEAR-L25.
029820       GO TO BC599.

       BC599.
	     EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
049200	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
049210       DISPLAY "PRINT INVOICE PROGRAM" AT 1231
049220                WITH FOREGROUND-COLOR 15.
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
      *    *************************************************************
      *    **** 	D A I L Y   -	A U D I T   F I L E	    ****
      *    *************************************************************
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
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.

       ZA01.
049320       MOVE 3              TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INV	 TO WS-INV.
             MOVE PAR-VAT-REG    TO W05-VATNO.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-INVP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN2 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
       ZA01B.
049880       OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
           IF WS-INV = 2
               MOVE WS-17CPI     TO REP-DETAIL2
           ELSE
053570         MOVE WS-12CPI     TO REP-DETAIL2.
053580       WRITE REP-L2 BEFORE 0 LINES.
053590       MOVE SPACES         TO REP-DETAIL2.
053600       OPEN OUTPUT RECOVER.
053610       CLOSE RECOVER.
053620       OPEN I-O RECOVER.
054270	     OPEN OUTPUT SALE.
054280       CLOSE SALE.
054290       OPEN I-O SALE.
050580	     MOVE 1		 TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
050640       MOVE PAR-TELEPHONE  TO W95-TEL.
050650     IF PAR-STMINV = "I" OR "X"
050660         MOVE "N"          TO WS-HEAD
050670     ELSE
050680         MOVE "Y"          TO WS-HEAD.
050690       ADD 1               TO WS-PARKEY.
050700       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050710       MOVE PAR-ADDRESS1   TO W95-ADD1.
050720       MOVE PAR-ADDRESS2   TO W95-ADD2.
050730       MOVE PAR-ADDRESS3   TO W95-ADD3.
050740       MOVE PAR-ADDRESS4   TO W95-ADD4.
050750       MOVE PAR-POST-CDE   TO W95-POST.
050760     IF W95-ADD4 = SPACES
050770         MOVE W95-PC       TO W95-ADD4
050780         MOVE SPACES       TO W95-PC.
050790     IF W95-ADD3 = SPACES
050800         MOVE W95-ADD4     TO W95-ADD3
050810         MOVE SPACES       TO W95-ADD4.
050820     IF W95-ADD2 = SPACES
050830         MOVE W95-ADD3     TO W95-ADD2
050840         MOVE SPACES       TO W95-ADD3.
050850     IF W95-ADD1 = SPACES
050860         MOVE W95-ADD2     TO W95-ADD1
050870         MOVE SPACES       TO W95-ADD2.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050560	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
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
             MOVE PAR-DSGL       TO WX-DSGL.
051530	     GO TO ZA999.

       COPY ZA49.PRO.

       ZA50.
041850     IF WS-F-ERROR = 5
041860         MOVE "DEBTOR TRANS"
                                 TO WS-FILE
           ELSE
055350     IF WS-F-ERROR = 6 
055360         MOVE "DEBTOR file"
055370                           TO WS-FILE
055380     ELSE
055350     IF WS-F-ERROR = 9 
055360         MOVE "GUARANTEE file"
055370                           TO WS-FILE
055380     ELSE
055350     IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
055370                           TO WS-FILE
055380     ELSE
041850     IF WS-F-ERROR = 20
041860         MOVE "SALES ORDER" TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE "STOCK file" TO WS-FILE
055410     ELSE
055420     IF WS-F-ERROR = 32
055430         MOVE "TAX file"   TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
051540 ZA200.

       COPY LOCKED.PRO.

       ZA205.
051600       EXIT PROGRAM.
051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION 91.

       COPY ERRORS.PRO.

051790 DISPLAY-FILE-NAME.
056270     IF WS-F-ERROR = 1
056280         MOVE W02-AUDITF TO WS-FILE
056290         MOVE WS-AUDKEY TO WS-KEY
056300     ELSE
051840     IF WS-F-ERROR = 2
051850         MOVE W02-NETWORK TO WS-FILE
051860         MOVE WS-NETKEY TO WS-KEY
056340     ELSE
056350     IF WS-F-ERROR = 5
056360         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
051910     ELSE
051920     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420     ELSE
056430     IF WS-F-ERROR = 7
056440	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056460     ELSE
056470     IF WS-F-ERROR = 12
041860         MOVE W02-INVOIC   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
052070     ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
056540     ELSE
056550     IF WS-F-ERROR = 18
056560         MOVE W02-RECOVER  TO WS-FILE
056570         MOVE WS-RECKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170         MOVE W02-SALE TO WS-FILE
052180         MOVE WS-SALKEY TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
052190     ELSE
052200     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
056700     ELSE
056710     IF WS-F-ERROR = 32
056720         MOVE W02-TXTRAN   TO WS-FILE
056730         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
