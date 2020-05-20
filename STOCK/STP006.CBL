      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	    ****     ******    *
      *   **	**     **     **    **	 **  **    **  **   **	  **   *
      *   **	       **     **    **	**    **  **	**  **	       *
      *    ******      **     *******	**    **  **	**  *******    *
      * 	**     **     **	**    **  **	**  **	  **   *
      *   **	**     **     **	 **  **    **  **   **	  **   *
      *    ******      **     **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        A M E N D   T H E   S U P P L I E R   C O D E	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP006.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   SEPTEMBER 2005.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2005 - 2008
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
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000160 COPY CREDIT.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000380 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	    PIC  999V99    COMP-3.
001710 77  WS-SUB	    PIC  9(04)	   COMP-5.
001710 77  WS-S1	    PIC  9(04)	   COMP-5.
001720 77  WS-S2	    PIC  9(04)	   COMP-5.
001730 77  WS-S3	    PIC  9(04)	   COMP-5.
001740 77  WS-S4	    PIC  9(04)	   COMP-5.
001750 77  WS-S5	    PIC  9(04)	   COMP-5.
001760 77  WS-S6	    PIC  9(04)	   COMP-5.
001770 77  WS-S7	    PIC  9(04)	   COMP-5.
001780 77  WS-S8	    PIC  9(04)	   COMP-5.
001790 77  WS-IXS	    PIC  9(04)	   COMP-5.
001800 77  WS-IXS1	    PIC  9(04)	   COMP-5.
001810 77  WS-IXS2	    PIC  9(04)	   COMP-5.
001820 77  WS-IXS3	    PIC  9(04)	   COMP-5.
001830 77  WS-IXS4	    PIC  9(04)	   COMP-5.
001840 77  WS-TRFKEY	    PIC  9(06)	   COMP-5.
001840 77  WS-PRCKEY	    PIC  9(06)	   COMP-5.
001860 77  WS-PARKEY	    PIC  9(06)	   COMP-5.
001870 77  WS-NETKEY	    PIC  9(06)	   COMP-5.
001890 77  WS-RECKEY	    PIC  9(06)	   COMP-5.
001900 77  WS-RECOVER	    PIC  9(06)	   COMP-5.
001900 77  WS-RECORDS	    PIC  9(06)	   COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	    PIC  9(04)	   COMP-5.
001930 77  WS-GST-RATE      PIC S9(03)V99 COMP-3.
001940 77  WS-MONTHS        PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	    PIC  9(04)	   COMP-5.
001980 77  WS-ITM	    PIC  X(14).
       77  WS-ETYPE	    PIC  X(01).
002010 77  WS-PAGE	    PIC  9(04)	   COMP-5.
003050 77  WS-NORM	    PIC  X(04).
003060 77  WS-COND	    PIC  X(04).
003070 77  WS-EXPP	    PIC  X(04).
003080 77  WS-ECAN	    PIC  X(04).
       77  WS-10CPI	    PIC  X(04).
       77  WS-12CPI	    PIC  X(04).
       77  WS-17CPI	    PIC  X(04).
002020 77  WS-OPTION	    PIC  X(01).
002020 77  WS-SKIP	    PIC  X(01).
002040 77  WS-ADJ	    PIC  X(01).
002070 77  WS-ERROR	    PIC  9(01).
002180 77  WS-USE-CASES     PIC	 X(01).
002190 77  WS-USE-PACKS     PIC	 X(01).
       77  WS-USE-ITM	    PIC	 X(01).
       77  WS-EXT-STK	    PIC	 X(01).
002080 77  WS-IND	    PIC  9(01)	   VALUE 0.
002090 77  WS-INDB	    PIC  9(01)	   VALUE 0.
002100 77  WS-INDP	    PIC  9(01)	   VALUE 0.
002110 77  WS-INDS	    PIC  9(01)	   VALUE 0.
002120 77  WS-AMEND	    PIC  9(01)	   VALUE 0.
002140 77  WS-CONREC	    PIC  9(01) VALUE ZERO.
002150 77  WS-LEDG	    PIC  X(04).
002160 77  WS-LEDG1	    PIC  X(04).
002200 77  WS-COMP	    PIC  9(01).
002210 77  WS-WEEK	    PIC  9(01).
002220 77  WS-TYPE	    PIC  X(01).
002260 77  WS-BATCH	    PIC  9(05).
002270 77  WS-PASSWORD	    PIC  X(08).
       77  WS-PRINT	    PIC  9(01).
002280 77  WS-LABCDE	    PIC  X(08).
002280 77  WS-PASS	    PIC  X(08).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  WS-NODEP	    PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME	    PIC	 X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	    PIC	 9(08) COMP-5.
       77  WS-USUB	    PIC	 9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
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
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
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
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
002450	   03  WS-MD-LNE.
002460	       05  WS-MCR  PIC X(80) VALUE "³      ³
      - 	 "                     ³               ³             ³".
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000160 COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

       01  W09-STOCK.
	   03  W09-S-MARKUPS.
002780	     05  W09-MARKUP	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-2	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-2	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-2	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-3	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-3	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-3	PIC S9(03)V9999  COMP-3.
002780	     05  W09-MARKUP-4	PIC S9(03)V9999  COMP-3.
002790	     05  W09-CMARKUP-4	PIC S9(03)V9999  COMP-3.
002790	     05  W09-WMARKUP-4	PIC S9(03)V9999  COMP-3.
	   03  W09-ST-MARKUPS REDEFINES W09-S-MARKUPS.
	       05  W09-STK-MARK OCCURS 4.
000300		   07  W09-MKUP  PIC S9(03)V9999  COMP-3.
000300		   07  W09-CMKUP PIC S9(03)V9999  COMP-3.
000300		   07  W09-WMKUP PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

      * COPY W15.STK.

004470 01  W15-INPUT-AREA.
004480	   03  W15-DATE        PIC 9(08).
	   03  W15-DTE REDEFINES W15-DATE.
001380	       05  W15-DATE-CHAR
001390			       PIC  X(01)     OCCURS 8.
004490	   03  W15-DATE1       REDEFINES W15-DATE.
004500         05  W15-DD      PIC 9(02).
004510         05  W15-MM      PIC 9(02).
	       05  W15-CC      PIC 9(02).
004520         05  W15-YY      PIC 9(02).
	   03  W15-DATE2       REDEFINES W15-DATE.
	       05  FILLER      PIC  9(04).
	       05  W15-CCYY    PIC  9(04).
004530	   03  W15-ACNO        PIC X(06).
004540     03  W15-SNO         REDEFINES W15-ACNO.
004550         05  W15-APREF   PIC X(01).
004560	       05  W15-SERIAL  PIC X(05).
004580	   03  W15-NAME.
001490         05  W15-NCHAR   PIC X(01) OCCURS 40.
001500     03  W15-N           REDEFINES W15-NAME.
001510         05  W15-NIX.
001520             07  W15-ENTRY PIC X(08).
001530             07  FILLER  PIC X(16).
001540         05  FILLER      PIC X(16).
	   03  W15-ENCRYPT REDEFINES W15-NAME.
	       05  W15-N1-2    PIC  X(02).
	       05  W15-N3-4    PIC  X(02).
	       05  W15-N5-6    PIC  X(02).
	       05  W15-N7-8    PIC  X(02).
	       05  W15-N9-10   PIC  X(02).
	       05  W15-N11-12  PIC  X(02).
	       05  W15-N13-14  PIC  X(02).
	       05  W15-N15-16  PIC  X(02).
	       05  W15-N17-18  PIC  X(02).
	       05  W15-N19-20  PIC  X(02).
	       05  W15-N21-22  PIC  X(02).
	       05  W15-N23-24  PIC  X(02).
	       05  W15-N25-26  PIC  X(02).
	       05  W15-N27-28  PIC  X(02).
	       05  W15-N29-30  PIC  X(02).
	       05  FILLER      PIC  X(10).
001760	   03  W15-ADDRESS     PIC  X(120).
001770     03  W15-ADD  REDEFINES W15-ADDRESS.              
001780	       05  W15-ADDR1   PIC  X(30).
001790	       05  W15-ADDR2   PIC  X(30).
001800	       05  W15-ADDR3   PIC  X(30).
001810	       05  W15-ADDR4   PIC  X(30).
001820     03  W15-ADD2 REDEFINES W15-ADDRESS.                     
001830	       05  W15-ACHAR   PIC  X(01) OCCURS 120.
001840	   03  W15-PCODE       PIC  9(04).
	   03  W15-CONTACT     PIC  X(30).
001620	   03  W15-STYLE       PIC  X(08).
	   03  W15-TEL	       PIC  X(14).
	   03  W15-FAX	       PIC  X(14).
	   03  W15-TAX	       PIC  X(01).
	   03  W15-TERMS       PIC  X(01).
004590	   03  W15-UN	       PIC 9(05)V99.
004600	   03  W15-UNIT REDEFINES W15-UN.
004610         05  FILLER      PIC 9(01).
004620         05  W15-UNT     PIC 9(04).
               05  W15-UDEC    PIC 9(02).
004630     03  W15-ORD         PIC 9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650         05  W15-OPREF   PIC 9(08).
004660         05  W15-OSUB    PIC 9(02).
004670	   03  W15-DESCR.
004680	       05  W15-DESC    PIC X(18).
004690	       05  FILLER      PIC X(12).
	   03  W15-CURRENCY    PIC X(10).
004700	   03  W15-OCOST       PIC S9(09)V999 COMP-3.
004700	   03  W15-RATE        PIC S9(03)V999 COMP-3.
004700	   03  W15-COST        PIC S9(09)V999 COMP-3.
004710	   03  W15-SELL        PIC S9(09)V999 COMP-3.
004720	   03  W15-SETT        PIC 9(06).
004730     03  W15-SET         REDEFINES W15-SETT.
004740         05  W15-30      PIC 9(01).
004750         05  W15-60      PIC 9(01).
004760         05  W15-90      PIC 9(01).
004770         05  W15-120     PIC 9(01).
004780         05  W15-150     PIC 9(01).
004790         05  W15-180     PIC 9(01).
004800     03  W15-PAY         REDEFINES W15-SETT.
004810         05  W15-MONTH   PIC 9(01) OCCURS 6.
	   03  W15-SET-SHIFT   REDEFINES W15-SETT.
	       05  W15-F5      PIC 9(05).
	       05  FILLER      PIC 9(01).
	   03  W15-SET-SHFT    REDEFINES W15-SETT.
	       05  FILLER      PIC 9(01).
	       05  W15-L5      PIC 9(05).
004820	   03  W15-DEL	       PIC 9(08).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
	       05  W15-DEL-CC  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
	   03  W15-LEDG.
004830	       05  W15-CAT     PIC 9(02).
	       05  FILLER      PIC X(02).
004840	   03  W15-GRNN        PIC X(08).
004850	   03  W15-GRND        PIC 9(08)     COMP-3.
004860	   03  W15-INVN        PIC X(08).
004870	   03  W15-INVD        PIC 9(08)     COMP-3.
004880     03  W15-TCOST       PIC S9(09)V99 COMP-3.
004890     03  W15-BAL         PIC S9(09)V99 COMP-3.
004900     03  W15-TSELL       PIC S9(09)V99 COMP-3.
004910     03  W15-QUANT       PIC S9(05)V9999 COMP-3.
004920	   03  W15-XQNT        PIC S9(09)V99 COMP-3.
004930	   03  W15-UCOST       PIC S9(09)V999 COMP-3.
004940	   03  W15-USELL       PIC S9(09)V999 COMP-3.
004950	   03  W15-UCASH       PIC S9(09)V999 COMP-3.
004950	   03  W15-UWSALE      PIC S9(09)V999 COMP-3.
           03  W15-RSELL       PIC S9(09)V99.
	   03  W15-RSR1 REDEFINES W15-RSELL.
               05  W15-RSR     PIC S9(09).
               05  W15-RSRANDS REDEFINES W15-RSR.
                   07  W15-RSR8
                               PIC S9(08).
                   07  W15-RSR9
                               PIC  9(01).
               05  W15-RSC     PIC  9(02).
           03  W15-RSRC2 REDEFINES W15-RSELL.
               05  FILLER      PIC S9(08).
               05  W15-RSRCC   PIC  9V99.
004960     03  W15-RAIL        PIC S9(09)V99 COMP-3.
004970     03  W15-DISCOUNT    PIC S9(09)V99 COMP-3.
004980	   03  W15-DISC1       PIC  99V999.
	   03  W15-DSC1 REDEFINES W15-DISC1
			       PIC  V9(05).
004990	   03  W15-DISC2       PIC 99V999.
	   03  W15-DSC2 REDEFINES W15-DISC2
			       PIC  V9(05).
	   03  W15-TAXABLE	OCCURS 12.
006790	       05  W15-TAXVAL  PIC S9(09)V99  COMP-3.
	       05  W15-VATVAL  PIC S9(09)V99  COMP-3.
	   03  W15-TXBL        PIC S9(09)V999 COMP-3.
	   03  W15-INCVAL      PIC S9(09)V99  COMP-3.
	   03  W15-IVAT        PIC S9(07)V99  COMP-3.
	   03  W15-U-VAT       PIC S9(07)V999 COMP-3.
	   03  W15-U-PNETT     PIC S9(07)V999 COMP-3.
	   03  W15-U-NETT      PIC S9(07)V999 COMP-3.
	   03  W15-U-TXBL      PIC S9(07)V999 COMP-3.
	   03  W15-U-NTXBL     PIC S9(07)V999 COMP-3.
	   03  W15-U-INCVAL    PIC S9(07)V999 COMP-3.
	   03  W15-VAT	       PIC S9(07)V99  COMP-3.
	   03  W15-TOTVAT      PIC S9(09)V99  COMP-3.
005000	   03  W15-TOTAL       PIC S9(09)V99  COMP-3.
005010	   03  W15-IUN	       PIC S9(08)V999 COMP-3.
005010	   03  W15-PUN	       PIC S9(07)V9(04) COMP-3.
005010	   03  W15-AVRG        PIC S9(08)V9999 COMP-3.
005020	   03  W15-ICOST       PIC S9(08)V9999 COMP-3.
005020     03  W15-INCOST      PIC S9(09)V999 COMP-3.
005020     03  W15-ILCOST      PIC S9(09)V999 COMP-3.
005020	   03  W15-PCOST       PIC S9(09)V999 COMP-3.
           03  W15-RMKS        PIC  X(08) VALUE "Account".
           03  W15-PMT         PIC  X(08) VALUE "Deliver".
           03  W15-PSLIP       PIC  X(08).
	   03  W15-PMNT        PIC  X(12).
	   03  W15-STM	       PIC  X(01).
	   03  W15-WHSE        PIC  X(06) VALUE SPACES.
004910	   03  W15-WQUANT      PIC S9(08)V9999 COMP-3.

       COPY W20.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       01  W30-SUPPLIER.
004530	   03  W30-ACNO       PIC X(06).
004530	   03  W30-PACNO      PIC X(06).
004580	   03  W30-NAME.
001490	       05  W30-NCHAR  PIC X(01) OCCURS 40.
004580	   03  W30-PNAME.
001490	       05  W30-PNCHAR PIC X(01) OCCURS 40.

       01  W31-SUPPLIER.
004530	   03  W31-ACNO       PIC X(06).
004530	   03  W31-PACNO      PIC X(06).

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       COPY W75.STK.
	   03  W75-CNO OCCURS 18.
	       05  W75-WHCDE.
		   07  W75-PKEY	PIC  9(05).
		   07  FILLER	PIC  X(01).

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000	       05  W80-DATE   PIC Z9/99/9999.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
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
004130 SCREEN SECTION.

       COPY BLANK.CRT.

004910 01  S04S.
004920	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
004930				 VALUE "AMEND THE SUPPLIER".

       01  S04-S.
	   03  LINE 19 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W30-ACNO AUTO.

       01  S04-AS.
	   03  LINE 20 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W30-PACNO AUTO.

       COPY STOCK.CRT.

       COPY CRDLUP.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	       Check the security level of the USER.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
007780	     PERFORM BA000.
	     CLOSE RECOVER.

       AA49.
	     EXIT PROGRAM.

       COPY AA50CS.LUP.

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
      *    ³   PERFORM OPT-MESSAGE.				       ³
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
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

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

       COPY DATE.CNV.

       COPY AA300.STK.

      *
      *    ****    D I S P L A Y   D E P A R T M E N T	 N A M E
      *
       AA400              SECTION.
       AA401.
	     MOVE W10-LEDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF (WS-F-ERROR = 7) OR
	      (DPT-DESC = SPACES)
	       MOVE WS-NODEP	 TO W10-DEPT
	   ELSE
	       MOVE DPT-DESC	 TO W10-DEPT.
	     DISPLAY W10-DEPT AT 0623
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       AA405.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
	     PERFORM SAVE-SCREEN.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0526
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "¿"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023190	     DISPLAY "³" AT 0626
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Insufficient security     "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023200	     DISPLAY "³" AT 0726
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Press ENTER to continue   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 0826
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
023210	     ACCEPT WS-OPTION AT 0752
		    WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7 AUTO.
	     PERFORM RESTORE-SCREEN.

       AA505.
             EXIT.


       COPY DATE.CHK.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
009850 AD000-WRITE	       SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

000140 COPY STKMEM.WR.

       COPY STOCK.WR.

       COPY STOCK.1ST.

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
011430 AY000	       SECTION.
011440 AY01.
011450       MOVE 01             TO REC-FILE.
011460       MOVE WS-PARKEY      TO REC-KEY.
011470       MOVE PAR-RECORD1    TO REC-PARAM.
011480       GO TO AY50.

011490 AY05.
011500       MOVE 05             TO REC-FILE.
011510       MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	     MOVE WS-ACTION	 TO REC-TYPE.
011520       MOVE STK-RECORD1    TO REC-STOCKF.
011530       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

       AY38.
             MOVE 38             TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011120 AY49.
011700       MOVE 99             TO REC-FILE.
011710       MOVE ZERO           TO REC-KEY.
011720       MOVE SPACES         TO REC-DETAIL.
011730       PERFORM AY50 THRU AY59.
011740       ADD 1               TO WS-TRANS.
011750     IF WS-RECOVER > 95
011760         CLOSE RECOVER
011770         OPEN OUTPUT RECOVER
011780         CLOSE RECOVER
011790         OPEN I-O RECOVER
011800         MOVE ZERO         TO WS-RECOVER.
011810       GO TO AY59.

011820 AY50.
011830	     ADD 1		 TO WS-RECOVER.
011840       MOVE WS-RECOVER     TO WS-RECKEY.
011850       MOVE WS-TRANS       TO REC-TRAN.
011860       WRITE REC-RECORD.
011870     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
011900         STOP RUN.
011930 AY59.
011940       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
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
012620	     MOVE "ST"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
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
      *            NETWORK record 3.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

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

       AY85.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

012330 AY999.
012340       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    ****    A M E N D   T H E   S U P P L I E R
      *
043030 BA000		   SECTION 50.
043040 BA00.
043050       DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Has the User logged in to do more restrictive amendments  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
043060	   IF LS-OK = "N"
               PERFORM AA500
043070	       GO TO BA999.
019430	     DISPLAY S04S.
019440	     DISPLAY S04-N.
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/ITM" AT 0312
		       "/" AT 0332.
	   IF W10-ART11 = "Y"
	       DISPLAY "VAT - Article 11/1g" AT 0849.
019460	     MOVE SPACE 	 TO W10-EXT-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
019470 BA05.
             PERFORM AA300.
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S05-ITM
	       ELSE
021070		   ACCEPT S05
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S05-3
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S05-4
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S05-5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S05-6
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S05-7
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S05-8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S05-9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S05-10
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S05-11
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S05-12
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S05-13
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S05-14.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BA05
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE
	       PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
	       IF W10-EXT-ITEM = SPACES
		   GO TO BA05.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019490	   IF W10-EXT-ITEM = SPACE
019500	       GO TO BA999.

       BA06.
019510       PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-ITM
	       ELSE
018630		   DISPLAY S05
	       END-IF
	   ELSE
	       DISPLAY S05.
019520	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO BA05
               ELSE
		   GO TO BA06.
	   IF STK-IND = 2
	       MOVE "Text Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA00.
	   IF STK-IND = 3
	       MOVE "Labour Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA00.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA00.
	   IF STK-IND = 8 OR 9
	       MOVE "Redundant Item"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA00.

019560 BA10.
019570	     MOVE STK-CODE	 TO W10-EXT-ITEM.
019580	     MOVE STK-XREF	 TO W10-EXT-CODE2.
019590	     MOVE STK-BIN	 TO W10-BIN.
019600       MOVE STK-DESC       TO W10-DESC.
019600	     MOVE STK-DESC2	 TO W10-DESC2.
019610       MOVE STK-TAX        TO W10-TAX.
019640	     MOVE STK-IND	 TO W10-IND.
019650       MOVE STK-QUANT      TO W10-QUANT.
	     MOVE STK-PER	 TO W10-PER.
	     MOVE STK-PRC	 TO W10-PRC.
019660       MOVE STK-LEVEL      TO W10-REORD.
019670       MOVE STK-LTIME      TO W10-LTIME.
019680       MOVE STK-COST       TO W10-COST.
037250	     MOVE STK-MARKUP	 TO W10-MARKUP.
037260       MOVE STK-CMARKUP    TO W10-CMARKUP.
037270       MOVE STK-WMARKUP    TO W10-WMARKUP.
037330	     MOVE STK-SELL	 TO W10-SELL.
037340       MOVE STK-CASH       TO W10-CASH.
037350       MOVE STK-WSALE      TO W10-WSALE.
019710	     MOVE STK-LDG	 TO W10-LEDG.
             MOVE STK-GUAR       TO W10-GUAR.
019720       MOVE STK-ORD        TO W10-ORDER.
019730	     MOVE STK-DTE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3       TO W10-ODTE.
037400	     MOVE STK-PUR	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE3	 TO W10-PURCH.
019800	     MOVE STK-AVGE	 TO W10-AVRG.
019810       MOVE STK-ACODE      TO W10-ACODE.
	     MOVE STK-EXEMPT	 TO W10-EXEMPT.
	     MOVE STK-FACT	 TO W10-FACT.
	     MOVE 1		 TO WS-S1.

       BA12-MKUPS.
	     MOVE STK-UNT(WS-S1) TO W10-QNT.
	   IF WS-S1 = 1
	       MOVE W10-COST	 TO W10-ICOST
	   ELSE
	       COMPUTE W10-ICOST ROUNDED = W10-COST * W10-QNT.
010310	     COMPUTE W09-WMKUP(WS-S1) ROUNDED =
		   ((STK-WSL(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
010310	     COMPUTE W09-MKUP(WS-S1) ROUNDED =
		   ((STK-SL(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
010330	     COMPUTE W09-CMKUP(WS-S1) ROUNDED =
		   ((STK-CSH(WS-S1) - W10-ICOST) * 100.000) / W10-ICOST.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BA12-MKUPS.
021150	   IF STK-ACT = ZERO
021160	       MOVE SPACE	 TO W10-ACTIVE
021170     ELSE
021180	       MOVE STK-ACT	 TO W22-DTE2
	       PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
	       MOVE W22-DTE3	 TO W10-ADATE.
021220	     MOVE STK-MTDC	 TO W10-MTDC.
021230       MOVE STK-YTDC       TO W10-YTDC.
021240	   IF LS-OK = "N"
021250         MOVE ZERO         TO W10-COST W10-MARKUP 
021260                              W10-AVRG W10-CMARKUP 
                                    W10-YTDC W10-WMARKUP 
                                    W10-MTDC. 
	     DISPLAY S06A.
	     DISPLAY "Supplier      " AT 1903.
	     DISPLAY "Alternate Supp" AT 2003.
	     PERFORM AA400.

       BA15.
	     MOVE SPACES	 TO W30-ACNO W30-PACNO
				    W31-ACNO W31-PACNO
				    W30-NAME W30-PNAME.
	   IF NOT (STK-SUPP = SPACES OR LOW-VALUES)
               MOVE STK-SUPP     TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
		   MOVE P-NUMBER TO W30-ACNO W31-ACNO
		   MOVE P-NAME	 TO W30-NAME.
	   IF NOT (STK-PSUPP = SPACES OR LOW-VALUES)
	       MOVE STK-PSUPP	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               IF WS-F-ERROR = ZERO
		   MOVE P-NUMBER TO W30-PACNO W31-PACNO
		   MOVE P-NAME	 TO W30-PNAME.
	     DISPLAY W30-ACNO AT 1918
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-NAME AT 1925
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-PACNO AT 2018
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-PNAME AT 2025
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       BA20.
	     DISPLAY "F1" AT 2501
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA25.
	     ACCEPT S04-S.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      MOVE W31-ACNO TO W30-ACNO
		      GO TO BA00
		 WHEN F1-KEY
		      PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		      PERFORM AA50C THRU AA59
		      MOVE W15-ACNO TO W30-ACNO
		 WHEN OTHER
		      CALL X"E5"
		      GO TO BA25
	       END-EVALUATE
	       DISPLAY S04-S
	       IF W30-ACNO = SPACES
015990		   MOVE SPACES	 TO W30-NAME.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY CLEAR-L25.
	   IF NOT (W30-ACNO = SPACES)
	       MOVE W30-ACNO	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
	       IF WS-F-ERROR = 3
		   MOVE "Invalid Supplier account number"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BA20
	       END-IF
	       MOVE P-NAME	 TO W30-NAME.
	     DISPLAY W30-ACNO AT 1918
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-NAME AT 1925
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM BA20.

       BA30.
	     ACCEPT S04-AS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      MOVE W31-PACNO  TO W30-PACNO
		      GO TO BA00
		 WHEN F1-KEY
		      PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		      PERFORM AA50C THRU AA59
		      MOVE W15-ACNO TO W30-PACNO
		 WHEN OTHER
		      CALL X"E5"
		      GO TO BA30
	       END-EVALUATE
	       DISPLAY S04-S
	       IF W30-PACNO = SPACES
		   MOVE SPACES	 TO W30-PNAME.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-PACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY CLEAR-L25.
	   IF NOT (W30-PACNO = SPACES)
	       MOVE W30-PACNO	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
	       IF WS-F-ERROR = 3
		   MOVE "Invalid Alternate Supplier A/C"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BA30
	       END-IF
	       MOVE P-NAME	 TO W30-PNAME.
	     DISPLAY W30-PACNO AT 2018
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W30-PNAME AT 2025
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
012180	     PERFORM CHECK-CORRECT.
014450	   IF WS-OPTION = "N"
014460	       GO TO BA20.
020430	     PERFORM AY60 THRU AY999.
020410	     MOVE 0		 TO WS-ACTION.
020440       PERFORM AY05 THRU AY59.
	     MOVE W30-ACNO	 TO STK-SUPP.
	     MOVE W30-PACNO	 TO STK-PSUPP.
	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
020960	     PERFORM AY70 THRU AY999.
	     GO TO BA00.

044110 BA999.
044120       EXIT.
      /
      *    ****    C R E D I T O R   L O O K U P   R O U T I N E
      *
      *            PERFORMED FROM AA50 
      *            (P R O C E S S   F U N C T I O N   K E Y S)
      *
012510 CREDITOR-LOOKUP SECTION 64.

012520 CREDIT-LUP-SCREEN.
012530       DISPLAY CLR-SCREEN. 
             DISPLAY S-CRD.
             MOVE SPACE          TO WS-OPTION.

       CREDIT-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE SPACES TO W15-NAME.

       CREDIT-LUP-DATA.
             ACCEPT SA-CRD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W15-ACNO
000150		     GO TO CREDIT-LOOKUP-EXIT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-LUP-DATA.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY SPACE AT 2501
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE 6		 TO CLIN.
	     MOVE 1		 TO W75-S W75-S1.

       CREDIT-LUP-TAB-LOOP.
             MOVE SPACES         TO W75-CNO (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
               GO TO CREDIT-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       CREDIT-LUP-ENTRY-TYPE.
           IF W15-NAME = SPACES
               MOVE "!"          TO W15-NAME.
             MOVE 40             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-NAME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W15-NAME       TO P-NAME.
             PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO CREDIT-NO-RECORD.

       CREDIT-GET-CRED-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
               GO TO CREDIT-SKIP-CRED-READ.
027520       PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               SUBTRACT 1        FROM W75-S
               GO TO CREDIT-FULL-SCREEN.

       CREDIT-SKIP-CRED-READ.
             MOVE P-NUMBER       TO W75-CNO (W75-S).
	     DISPLAY SD-CRD AT LINE CLIN.
027700	   IF CLIN < 21
027710         ADD 1 TO CLIN W75-S
027720         GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " highlight - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     " select - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape" WITH FOREGROUND-COLOR 7.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
             MOVE 0601           TO CPOS.

       CREDIT-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       CREDIT-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO CREDIT-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO CREDIT-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO CREDIT-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO CREDIT-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO CREDIT-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO CREDIT-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CREDIT-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO CREDIT-GET-USER-OPT.

001240 CREDIT-GET-NEXT-PAGE.
           IF W75-S < 16
               GO TO CREDIT-GET-USER-OPT.

       CREDIT-NEW-PAGE.
             MOVE 0601           TO CPOS.
             PERFORM CREDIT-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES TO W75-KEYS.
             MOVE 1 TO W75-S W75-S1.
             PERFORM CREDIT-LUP-TAB-LOOP.
027830       GO TO CREDIT-GET-CRED-RECORD.

       CREDIT-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       CREDIT-GET-PREV-LOOP.
             PERFORM READ-CREDIT-PREV THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
	       MOVE SPACE	 TO WS-OPTION
               GO TO CREDIT-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
               GO TO CREDIT-GET-PREV-LOOP.
             GO TO CREDIT-NEW-PAGE.

027970 CREDIT-NO-RECORD.
027730	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CREDIT-GET-USER-OPT-2.
027990	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
027750       CALL "_MSUPCASE" USING WS-OPTION
023170                        LENGTH WS-OPTION
023180                               WS-LENGTH
023190                        LENGTH WS-LENGTH
023200                               WS-STATUS.
           IF NOT (WS-OPTION = "N" OR "Q")
               GO TO CREDIT-GET-USER-OPT-2.
027800     IF WS-OPTION = "N"
027810         GO TO CREDIT-LUP-SCREEN.
             MOVE SPACES         TO W15-ACNO.
028000       GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
             GO TO CREDIT-HIGHLIGHT.

       CREDIT-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1 TO W75-S1
               GO TO CREDIT-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1 TO W75-S1 CLIN.

       CREDIT-HIGHLIGHT.
             PERFORM CREDIT-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
             GO TO CREDIT-GET-USER-OPT.

       CREDIT-CHK-SELECT.
           IF W75-S1 = ZERO
               GO TO CREDIT-GET-USER-OPT.
             MOVE CSTART TO WS1-CURSOR.
             MOVE W75-CNO (W75-S1) TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	     MOVE ZERO		 TO COL-AND-LINE.
             MOVE P-NUMBER       TO W15-ACNO.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-CRT-EDGE.
002130	     DISPLAY WS-MCR1 AT CPOS WITH BACKGROUND-COLOR 0
					  FOREGROUND-COLOR 3
		     WS-MCR78 WITH FOREGROUND-COLOR 3
		     WS-MCR80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
019260	     ADD 1		 TO CLIN.

       CREDIT-LOOKUP-EXIT.
             EXIT.

      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
044130 ZA000-INIT	       SECTION 81.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
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
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF NOT (PAR-ART11 = "Y" OR "N")
	       MOVE "N" 	 TO PAR-ART11.
	     MOVE PAR-ART11	 TO W10-ART11.

       ZA01B.
044540	     OPEN OUTPUT RECOVER.
044550       CLOSE RECOVER.
044560       OPEN I-O RECOVER.
044580       MOVE 1              TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
042520       MOVE 5              TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-ROUND	 TO W05-ROUND.
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
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H1  TO WS-DESC-H1.
	   IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H2  TO WS-DESC-H2.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 82.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800	   ELSE
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041800	   ELSE
041810	   IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER  TO WS-FILE
041870         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
041880	   IF WS-STATUS = "10"
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
042170	     STOP RUN.
