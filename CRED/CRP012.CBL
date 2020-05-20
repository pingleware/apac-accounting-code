      $set LINKCOUNT"596"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	     **      ******    *
      *   **	**  **	  **  **    **	 **  **     ***     **	  **   *
      *   **	    **	  **  **    **	**    **     ** 	 **    *
      *   **	    ******    *******	**    **     **        **      *
      *   **	    **	 **   **	**    **     **      **        *
      *   **	**  **	  **  **	 **  **      **     **	       *
      *    ******   **	  **  **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   D E B I T   N O T E S   ( S T O C K )    *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP012.
000030 AUTHOR.		       James William Lemmon.
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

000200 COPY CRETRN.SL.

000200 COPY CRTRAN.SL.

       COPY DEPART.SL.

       COPY LEDTRF.SL.

000140 COPY PARAM.SL.

000300 COPY PURCH.SL.

000300 COPY PURDEX.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY STOCK.SL.

000200 COPY TXTRAN.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000330 COPY CRETRN.FDE.

000330 COPY CRTRAN.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000290 COPY PARAM.FDE.

000300 COPY PURCH.FD.

000300 COPY PURDEX.FDE.

000390 COPY RECOVER.GFD.

000140 COPY SHARED.FDE.

000200 COPY STOCK.FDE.

000200 COPY TXTRAN.FDE.

      /
003280 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001790 77  WS-SUB	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1         PIC  9(04)    COMP-5.
001810 77  WS-IXS2         PIC  9(04)    COMP-5.
001820 77  WS-IXS3         PIC  9(04)    COMP-5.
001830 77  WS-IXS4         PIC  9(04)    COMP-5.
003340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003360 77  WS-RECKEY       PIC  9(04)    COMP-5.
003370 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003370 77  WS-PURKEY       PIC  9(04)    COMP-5.
003380 77  WS-KEYSTR       PIC  9(04)    COMP-5.
003390 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
003400 77  WS-PURLAST      PIC  9(04)    COMP-5.
003410 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
003420 77  WS-ERROR        PIC  9(01)    VALUE 0.
       77  WS-ETYPE        PIC  X(01).
003430 77  WS-DEL	   PIC	X(01).
003470 77  WS-S 	   PIC	9(04)	 COMP-5.
003480 77  WS-S1           PIC  9(04)    COMP-5.
003490 77  WS-S2           PIC  9(04)    COMP-5.
003500 77  WS-S3           PIC  9(04)    COMP-5.
003510 77  WS-S4           PIC  9(04)    COMP-5.
003510 77  WS-S9	   PIC	9(04)	 COMP-5.
       77  WS-CRETRN	   PIC	9(04)	 COMP-5.
       77  WS-VAT	   PIC S9(07)V99 COMP-3.
       77  WS-IVAT	   PIC S9(07)V99 COMP-3.
003530 77  WS-ORD	   PIC	X(01).
003540 77  WS-OPTION       PIC  X(01).
001820 77  WS-ALLOCATE	   PIC	X(01).
003540 77  WS-SKIP	   PIC	X(01)	 VALUE "Y".
003540 77  WS-LOOK	   PIC	X(01)	 VALUE "S".
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003560 77  WS-CHAR1	   PIC	X(01).
003610 77  WS-SKEY	   PIC	9(05).
003620 77  WS-KEY1	   PIC	9(04)	 COMP-5.
003630 77  WS-LINES	   PIC	9(04)	 COMP-5 VALUE 0.
003640 77  WS-PAGE	   PIC	9(04)	 COMP-5 VALUE 0.
003700 77  WS-ER1	   PIC X(09)	 VALUE "No record".
003710 77  WS-HDTE	   PIC X(04)	 VALUE "DATE".
003720 77  WS-HACNO        PIC X(04)     VALUE "ACNO".
003730 77  WS-HCRED        PIC X(09)     VALUE "CREDITOR:".
003740 77  WS-HQNT         PIC X(05)     VALUE "QUANT".
003750 77  WS-HDESC        PIC X(11)     VALUE "DESCRIPTION".
003760 77  WS-HCOST        PIC X(10)     VALUE "COST PRICE".
003770 77  WS-HSELL        PIC X(10)     VALUE "SELL PRICE".
003780 77  WS-HNAME        PIC X(04)     VALUE "NAME".
003790 77  WS-HAGE         PIC X(12)     VALUE "AGE 30 - 180".
003810 77  WS-HSETT	   PIC X(22)	 VALUE
003820                     "30  60  90 120 150 180".
003830 77  WS-SDTE         PIC X(04)     VALUE "Date".
003840 77  WS-SACNO        PIC X(04)     VALUE "Acno".
003850 77  WS-SCRED        PIC X(09)     VALUE "Creditor:".
003860 77  WS-SQNT         PIC X(05)     VALUE "Quant".
003870 77  WS-SDESC        PIC X(11)     VALUE "Description".
003880 77  WS-SCOST        PIC X(10)     VALUE "Cost price".
003890 77  WS-SSELL        PIC X(10)     VALUE "Sell price".
003900 77  WS-SNAME        PIC X(04)     VALUE "Name".
003910 77  WS-SAGE         PIC X(12)     VALUE "Age 30 - 180".
003930 77  WS-SSETT	   PIC X(22)	 VALUE
003940                     "30  60  90 120 150 180".
003950 77  WS-H1           PIC X(05)     VALUE "DATE:".
003960 77  WS-H3           PIC X(05)     VALUE "PAGE:".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
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
	       05  WS-SPC  PIC	X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".
002430     03  WS-TP-LNE.
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

003980 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "012".

       COPY LEDTRF.WS.

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

004000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000160 COPY CREDIT.ID.

000180 COPY CRETRN.ID.

000180 COPY CRTRAN.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000300 COPY PURCH.ID.

       COPY PURDEX.ID.

000280 COPY RECOVER.ID.

000140 COPY SHARED.ID.

000200 COPY STOCK.ID.

000200 COPY TXTRAN.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).
	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DET	   PIC X(140).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-H1 	   PIC X(06).
	       05  R-DTE1	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(14).
	       05  R-H2 	   PIC X(40).
	       05  FILLER	   PIC X(17).
	       05  R-H3 	   PIC X(06).
	       05  R-PGE	   PIC ZZ9.

	   03  R-L2A REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(38).
	       05  R-HEAD	   PIC X(20).
	       05  FILLER	   PIC X(38).

	   03  R-L3 REDEFINES W02-PRN-LINE.
	       05  R-ORD	   PIC Z(07).
	       05  FILLER	   PIC X(01).
	       05  R-ITM	   PIC X(14).
	       05  FILLER	   PIC X(01).
	       05  R-DESC	   PIC X(18).
	       05  FILLER	   PIC X(01).
	       05  R-QNT	   PIC Z(04)9.99.
	       05  FILLER	   PIC X(01).
	       05  R-COST	   PIC Z(04)9.999.
	       05  FILLER	   PIC X(01).
	       05  R-SELL	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  R-EXT	   PIC Z(06)9.99-.

	   03  R-L3A REDEFINES W02-PRN-LINE.
	       05  I-H1 	   PIC X(08).
	       05  I-H2 	   PIC X(15).
	       05  I-H3 	   PIC X(22).
	       05  I-H4 	   PIC X(07).
	       05  I-H5 	   PIC X(10).
	       05  I-H6 	   PIC X(11).
	       05  I-H7 	   PIC X(09).

	   03  R-L4 REDEFINES W02-PRN-LINE.
	       05  G-H1 	   PIC X(10).
	       05  G-SUPP	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  G-NAME	   PIC X(40).
	       05  FILLER	   PIC X(23).

	   03  R-L4A REDEFINES W02-PRN-LINE.
	       05  G-H3 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-INVN	   PIC X(08).
	       05  FILLER	   PIC X(01).
	       05  G-H4 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-INVD	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  G-H5 	   PIC X(05).
	       05  FILLER	   PIC X(01).
	       05  G-RAIL	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H6 	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-DISC	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H6A	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  G-VAT	   PIC Z(05)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H7 	   PIC X(06).
	       05  G-TOTAL	   PIC Z(07)9.99-.

	   03  R-L9 REDEFINES W02-PRN-LINE.
	       05  REP-PG	   PIC X(06).
	       05  REP-P-N	   PIC Z(03)9.
	       05  FILLER	   PIC X(16).
	       05  REP-COMP	   PIC X(40).
	       05  FILLER	   PIC X(02).
	       05  REP-REPORT	   PIC X(40).
	       05  FILLER	   PIC X(08).
	       05  REP-DAT	   PIC X(06).
	       05  REP-DT	   PIC Z9/99/9999.

	   03  DS-L1 REDEFINES W02-PRN-LINE.
	       05  DS-H1	   PIC X(06).
	       05  DS-DATE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(25).
	       05  DS-COMP	   PIC X(41).
	       05  DS-H2	   PIC X(40).
	       05  DS-H3	   PIC X(06).
	       05  DS-PAGE	   PIC ZZZ9.

	   03  DS-L2 REDEFINES W02-PRN-LINE.
	       05  DS-H4	   PIC X(09).
	       05  DS-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  DS-NAME	   PIC X(40).
	       05  FILLER	   PIC X(02).
	       05  DS-H5	   PIC X(11).
	       05  DS-DNO	   PIC X(08).
	       05  FILLER	   PIC X(02).
	       05  DS-H6	   PIC X(05).
	       05  DS-DNDTE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(02).
	       05  DS-H7	   PIC X(11).
	       05  DS-INV	   PIC X(08).
	       05  FILLER	   PIC X(01).
	       05  DS-H8	   PIC X(06).
	       05  DS-IDTE	   PIC Z9/99/9999.

	   03  DS-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(01).
	       05  DS-H9	   PIC X(16).
	       05  DS-H10	   PIC X(29).
	       05  DS-H11	   PIC X(13).
	       05  DS-H12	   PIC X(13).
	       05  DS-H13	   PIC X(12).
	       05  DS-H14	   PIC X(15).
	       05  DS-H15	   PIC X(11).
	       05  DS-H16	   PIC X(22).

	   03  DS-L4 REDEFINES W02-PRN-LINE.
	       05  DS-ITEM	   PIC X(14).
	       05  FILLER	   PIC X(03).
	       05  DS-DESC	   PIC X(24).
	       05  FILLER	   PIC X(04).
	       05  DS-UCOST	   PIC Z(05)9.999.
	       05  FILLER	   PIC X(04).
	       05  DS-USELL	   PIC Z(05)9.99.
	       05  FILLER	   PIC X(02).
	       05  DS-QNT	   PIC Z(05)9.99.
	       05  FILLER	   PIC X(03).
	       05  DS-COST	   PIC Z(06)9.999.
	       05  FILLER	   PIC X(04).
	       05  DS-SELL	   PIC Z(07)9.99.
	       05  FILLER	   PIC X(02).
	       05  DS-30	   PIC X(01).
	       05  FILLER	   PIC X(03).
	       05  DS-60	   PIC X(01).
	       05  FILLER	   PIC X(03).
	       05  DS-90	   PIC X(01).
	       05  FILLER	   PIC X(03).
	       05  DS-120	   PIC X(01).
	       05  FILLER	   PIC X(03).
	       05  DS-150	   PIC X(01).
	       05  FILLER	   PIC X(03).
	       05  DS-180	   PIC X(01).
	       05  FILLER	   PIC X(01).

       COPY W05.WS.

       COPY W05.VAT.

004420 01  W10-MONTHS.
004430     03  W10-MONTH-NAMES PIC X(36)     VALUE
004440                        "JanFebMarAprMayJunJulAugSepOctNovDec".
004450     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
004460                         PIC X(03)     OCCURS 12.

       COPY W10.CRP.

       COPY W15.CRP.

       01  W16-TOTALS.
           03  FILLER          PIC  X(12) VALUE "TOTAL COST: ".
           03  W16-COST        PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(12) VALUE " TOTAL VAT: ".
           03  W16-VAT         PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(08) VALUE " TOTAL: ".
           03  W16-TOTAL       PIC  Z(08)9.99DB.

005030 01  W20-CALCS.
005040     03  W20-RESULT      PIC 9(09)V99.
005050     03  W20-RESULT-1    REDEFINES W20-RESULT.
005060         05  W20-WHOLE   PIC 9(09).
005070         05  W20-DECIMAL PIC 9(02).
005080     03  W20-ORD         PIC 9(07)     COMP-3.
	   03  W20-DATE.
	       05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
			       PIC 9(08).
	   03  W21-DATE.
	       05  W21-CC      PIC 99.
	       05  W21-YY      PIC 99.
	       05  W21-MM      PIC 99.
	       05  W21-DD      PIC 99.
	   03  W21-DTE REDEFINES W21-DATE
			       PIC 9(08).
	   03  W21-DTE2 REDEFINES W21-DATE.
	       05  W21-CY      PIC 9(04).
	       05  FILLER      PIC 9(04).

005030 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
	   03  W25-INVD        PIC 9(08).
	   03  W25-PDTE REDEFINES W25-INVD.
	       05  W25-DD      PIC 9(02).
	       05  W25-MM      PIC 9(02).
	       05  W25-CC      PIC 9(02).
	       05  W25-YY      PIC 9(02).

005090 01  W25-VALUES.
005100     03  W25-CFLOW OCCURS 6 PIC S9(09)V99  COMP-3.
005110     03  W25-V1          PIC S9(09)V99 COMP-3.
005120     03  W25-V2          PIC S9(09)V99 COMP-3.

005090 01  W25-VALUES-2.
005100     03  W25-CFLW2 OCCURS 6 PIC S9(09)V99  COMP-3.
005110     03  W25-V1-2        PIC S9(09)V99 COMP-3.
005120     03  W25-V2-2        PIC S9(09)V99 COMP-3.

005130 01  W25-EDIT.
005140     03  W25-5.
005150         05  W25-1       PIC ZZZZ9.
005160     03  W25-3V2.
005170         05  W25-2       PIC ZZ9.99.
005180     03  W25-7V2.
005190         05  W25-3       PIC Z(06)9.99.
005200     03  W25-6V2.
005210         05  W25-3A      PIC Z(06)9.99.
005220     03  W25-3N.
005230         05  W25-4       PIC ZZ9.
002060     03  W25-S7V2A.
002070         05  W25-CVAL  PIC  Z(06)9.99-.
005240     03  W25-DTE.
005250	       05  W25-DATE    PIC 99/99/9999.
005260     03  W25-6.
005270         05  W25-NO      PIC Z(07)9.
005300     03  W25-S5V2.
005310	       05  W25-SVAL    PIC Z(07)9.99.
005320     03  W25-S7V2.
005330         05  W25-VAL2    PIC Z(06)9.99DB.
005280     03  W25-5V2 REDEFINES W25-S7V2.
005290         05  W25-VAL     PIC Z(07)9.99-.
005320     03  W25-72.
005330         05  W25-VAL3    PIC S9(07)V99.
005340     03  W25-PRC.
005350         05  W25-IPRC    PIC Z(06)9.999.

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
               05  W40-ADDRS   PIC  X(60).
               05  FILLER      PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR   PIC  X(01) OCCURS 40.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).

       COPY FUNCTION.WS.

005450 COPY W50.WS.

005470 01  W50-H4.
005480     03  W50-HEAD        PIC X(20).

005490 01  W55-DEL.
005500     03  W55-MNTH        PIC X(03).
005510     03  FILLER          PIC X(01)     VALUE ",".
005520     03  W55-YY          PIC 9(02).

005530 01  W60-H2.
005540     03  W60-COMPANY     PIC X(40).
005550     03  W60-DESC        PIC X(21)     VALUE
005560                         "CREDITORS AUDIT TRAIL".

005600 01  W65-CREDITOR.
005610     03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.
005110     03  W65-V1          PIC S9(09)V99 COMP-3.
005120     03  W65-V2          PIC S9(09)V99 COMP-3.

005680 COPY W70CRED.WS.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02)    COMP-5.
	   03  W75-S1	      PIC  9(02)    COMP-5.
	   03  W75-S2	      PIC  9(02)    COMP-5.
	   03  W75-KEY	      PIC  9(06)    COMP-5.
	   03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
	   03  W75-CNO	      PIC  X(06)    OCCURS 18.

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004040	   03  W90-DB	       PIC S9(09)V99 COMP-3.
004050     03  W90-CR          PIC S9(09)V99 COMP-3.
004040     03  W90-ALLOCATED   PIC S9(09)V99 COMP-3.
003430	   03  W90-DISCOUNT    PIC S9(09)V99 COMP-3.
004040	   03  W90-DEBIT       PIC S9(09)V99 COMP-3.
004050     03  W90-CREDIT      PIC S9(09)V99 COMP-3.

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
           03  W100-S8V2       PIC  Z(07)9.99DB.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005700 SCREEN SECTION.

       COPY BLANK.CRT.

006450 01  S06.
006460     02  S06A.
006470	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006480                           VALUE "CREDITOR DEBIT NOTE".
006140	   03  LINE  4 COLUMN  2 VALUE "Creditor Ac. :".
006500	   03  LINE  5 COLUMN  2 VALUE "D/N no:".
006510	   03	       COLUMN 20 VALUE "D/N date:".
006520	   03	       COLUMN 42 VALUE "Inv no:".
006530	   03	       COLUMN 60 VALUE "Inv date:".
006540     03  LINE  7 COLUMN  2 VALUE "Item code    :".
006550     03  LINE  8 COLUMN  2 VALUE "Description  :".
006560     03  LINE  9 COLUMN  2 VALUE "Quantity     :".
006570     03  LINE 10 COLUMN  2 VALUE "Unit cost    :".
006580     03  LINE 11 COLUMN  2 VALUE "Discount-1   :".
006590     03  LINE 12 COLUMN  2 VALUE "Discount-2   :".
006600     03  LINE 13 COLUMN  2 VALUE "Unit nett    :".
006610     03  LINE 14 COLUMN  2 VALUE "Total cost   :".
006620     03  LINE 16 COLUMN  2 VALUE "Unit retail  :".
006630     03  LINE 17 COLUMN  2 VALUE "Total retail :".
006640     02  S06B.
006650	   03  LINE 19 COLUMN  2 VALUE "Allocation - 30:".
	   03	       COLUMN 22 VALUE "60:".
	   03	       COLUMN 29 VALUE "90:".
	   03	       COLUMN 36 VALUE "120:".
	   03	       COLUMN 44 VALUE "150:".
	   03	       COLUMN 52 VALUE "180:".
           03  LINE 20 COLUMN 54 VALUE "Sub total :".
           03  LINE 21 COLUMN 54 VALUE "VAT       :".
006670     03  LINE 22 COLUMN 54 VALUE "Totals    :".

007910 01  S07.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				  PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC X(14) USING W10-ITEM AUTO.
	   03  COLUMN 32 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S07-3.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				 PIC Z(14) USING W10-I14 AUTO.

007450 01  S19.
007460	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC X(06) USING W15-ACNO AUTO.

007610 01  S27.
007620	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC X(08) USING W15-GRNN AUTO.

007630 01  S28.
007640	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9/99/9999 USING W15-GRND AUTO.

007650 01  S29.
007660	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC X(08) USING W15-INVN AUTO.

007670 01  S30.
007680	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9/99/9999 USING W15-INVD AUTO.

007710 01  S32.
007720	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-USELL AUTO.

007780 01  S35.
007790	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC1 AUTO.

007800 01  S36.
007810	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC2 AUTO.

007820 01  S37.
007830	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-IUN AUTO.

007840 01  S38.
007850	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.999 USING W15-ICOST AUTO.

007530 01  S40.
007540	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-COST AUTO.

       01  S42.
	   02  S42A.
	   03  LINE  4 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 24 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(40) FROM WS-SPC.
	   03  LINE  5 COLUMN 10 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(08) FROM WS-SPC.
	   03	       COLUMN 30 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(10) FROM WS-SPC.
	   03	       COLUMN 50 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(08) FROM WS-SPC.
	   03	       COLUMN 70 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(10) FROM WS-SPC.
	   02  S42B.
	   03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(14) FROM WS-SPC.
	   03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM WS-SPC.
	   03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(05) FROM WS-SPC.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(05) FROM WS-SPC.
	   03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 19 COLUMN 19 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 49 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 57 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(01) FROM WS-SPC.
	   02  S42C.
	   03  LINE 20 COLUMN 66 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 21 COLUMN 66 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 22 COLUMN 66 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(13) FROM WS-SPC.

       01  S43.
007540	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99DB USING W15-VAT AUTO.

       01  S44.
007540	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99DB USING W15-TCOST AUTO.

       COPY CRDLUP.CRT.

       01  S-CRED.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
	   03	       COLUMN 12 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
                                 PIC Z(08)9.99DB USING P-BAL.
           03          COLUMN 28 VALUE "Due :".
	   03	       COLUMN 34 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
                                 PIC Z(08)9.99DB USING P-OUT.

       01  S-ALLOC.
	   03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-BALANCE.
	   03	       COLUMN 51 VALUE "Allocated : ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-CTRN.
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC X(08) FROM CRT-REF.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC 9999/99/99 FROM CRT-SDTE.
	   03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC X(12) FROM W00-C-DESC.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC Z(08)9.99 FROM CRT-VALUE.
	   03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC Z(08)9.99 FROM CRT-OUTSTD.
	   03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC Z(08)9.99 USING CRT-ALLOC AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
003240	   IF LS0-CRLEV < 1
	       DISPLAY CLEAR-L25
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
004940	     PERFORM EA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005260	     CLOSE PURCH
		   RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY AA50CS.LUP.

       AA050-UTILS     SECTION.
      *
      *    ****    C A L L   S T O C K   C R E A T I O N
      *
       AA100.
	     PERFORM SAVE-SCREEN.
004200	     CALL "STP\STP010"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "STP010".
	     OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
012150       MOVE ZERO           TO WS-RECOVER.
	     PERFORM RESTORE-SCREEN.
       AA199.             
             EXIT.

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

       COPY LOCKED.REC.

       COPY PRINTUTL.AS4.

       COPY CLEAR.CRT.

       COPY CREDITOR.TRN.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011010 AA300              SECTION.
011020 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Item/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     "=Alt/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     "=Desc/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     "=Desc2/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     "=X/Ref Find,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
		     " Create"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011110 AA305.
011120       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CRETRN.RD.

       COPY CRTRAN.RD.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY PURCH.RD.

000140 COPY SHARED.RD.

       COPY STOCK.RD.

000200 COPY TXTRAN.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010390 AC000-WRITE   SECTION.

       COPY CONTROL.WR.

       COPY CRETRN.WR.

       COPY CRTRAN.WR.

       COPY CREDIT.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY PURCH.WR.

       COPY PURDEX.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

000200 COPY TXTRAN.WR.

      /
012720 AD000         SECTION.
012730 AD00.
012740       MOVE ZERO           TO WS-ERROR.
012750     IF W15-DD < 01 OR > 31
012760         GO TO AD15.
012770     IF W15-MM < 01 OR > 12
012780         GO TO AD15.
012790     IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
012800         GO TO AD999.
012810     IF W15-MM = 02 AND W15-DD > 28
012820         GO TO AD05.
012830     IF W15-DD > 30
012840         GO TO AD15.
012850       GO TO AD999.

012860 AD05.
012870     IF W15-DD > 29
012880         GO TO AD15.
012890	     DIVIDE W15-CCYY BY 4
				 GIVING W20-RESULT.
012900     IF W20-DECIMAL > ZERO
012910         GO TO AD15.
012920       GO TO AD999.

012930 AD15.
012940       MOVE 1              TO WS-ERROR.
012950 AD999.
012960       EXIT.

       COPY CREDIT.ILU.

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
011730 AY000	       SECTION.
011740 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011780       GO TO AY50.

011790 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
011890	       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011830       GO TO AY50.

011840 AY07.
008010       MOVE 07 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
008030       MOVE P-REC          TO REC-CREDIT.
008040       GO TO AY50.

011890 AY08.
011900       MOVE 08             TO REC-FILE.
011910	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011920	     MOVE CT-REC	 TO REC-CRTRAN.
011930	     GO TO AY50.

011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

012090 AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
012140	     GO TO AY50.

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

011070 AY056.
011080	     MOVE 56		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE PRD-REC1	 TO REC-PURDEX.
011110       GO TO AY50.

012040 AY49.
012050       MOVE 99             TO REC-FILE.
012060       MOVE ZERO           TO REC-KEY.
012070       MOVE SPACES         TO REC-DETAIL.
012080       PERFORM AY50.
012090       ADD 1               TO WS-TRANS.
012100     IF WS-RECOVER > 95
012110         CLOSE RECOVER
012120         OPEN OUTPUT RECOVER
012130         CLOSE RECOVER
012140         OPEN I-O RECOVER
012150         MOVE ZERO         TO WS-RECOVER.
012160       GO TO AY59.

012170 AY50.
012180       ADD 1               TO WS-RECOVER.
012190       MOVE WS-RECOVER     TO WS-RECKEY.
012200       MOVE WS-TRANS       TO REC-TRAN.
012210       WRITE REC-RECORD.
012220     IF WS-STATUS NOT = "00"
012230         DISPLAY "Write error Recovery file - Status " AT 2312
                        WITH FOREGROUND-COLOR 14 
                        WS-STATUS WITH FOREGROUND-COLOR 15
012250         STOP RUN.

012280 AY59.
012290       EXIT.
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
012620	     MOVE "CS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
	     MOVE NET-CREDITOR	 TO W70-CREDCON.
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
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
					  OR "ST" OR "DS"
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
      * 	   NETWORK record 2.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012970	     MOVE 2		 TO WS-NETKEY.
012980       MOVE W70-CREDCON    TO NET-CREDITOR.
012990       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
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
013090	     COMMIT.

011860 AY999.
011870       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

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

       COPY CA200.STK.

       COPY CA500.STK.

       CA600	     SECTION.
       CA601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.

       CA699.
             EXIT.

       CA700         SECTION.
       CA701.
             MOVE 3              TO TAX-ACTYPE.
             MOVE CT-CODE        TO TAX-CODE.
             MOVE W20-DTE        TO TAX-DATE.
             MOVE CT-REF         TO TAX-REF.
             MULTIPLY -1 BY CT-COST
                                 GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE
                                 GIVING TAX-TAXFREE.
	     MOVE CT-CRED	 TO TAX-AC.
	   IF W00-C-ACT = 1
               IF CT-CODE = 03
                   MOVE "3"      TO TAX-TYPE
               ELSE
                   MOVE "2"      TO TAX-TYPE
           ELSE
               MOVE "1"          TO TAX-TYPE.
	     MOVE ZERO		 TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       CA705.
	     MOVE W15-TAXVAL(WS-S9)
				 TO TAX-VAL(WS-S9).
	     MOVE W15-VATVAL(WS-S9)
				 TO TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO CA705.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.

       CA799.
             EXIT.

       COPY LEDTRF.UPD.

       COPY CREDIT.ALL.

      /
024820 EA000	     SECTION 50.
024830 EA00.
024840	   IF LS0-STP NOT = 2
024850         DISPLAY CLR-SCREEN
024860	       MOVE "Stock not installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
024890	       GO TO EA999.
024900       MOVE ZERO           TO W15-TOTAL WS-PAGE W15-TOTVAT.
      *
      *    ****    Print the audit trail HEADING
      *
024930 EA05.
	     MOVE "H"		 TO W02-PRN-TYPE.
024940	     MOVE SPACES	 TO R-DET.
024950       MOVE WS-H1          TO DS-H1.
024960       MOVE W60-COMPANY    TO DS-COMP.
024970       MOVE "- DEBIT NOTES"
                                 TO DS-H2.
024980       MOVE WS-H3          TO DS-H3.
024990	     ADD 1		 TO WS-PAGE.
025000       MOVE W05-DATE       TO DS-DATE.
025010       MOVE WS-PAGE        TO DS-PAGE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025030	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.
      *
      *    ****	   S C R E E N	 L A Y O U T .
      *
025040 EA10.
025050       DISPLAY CLR-SCREEN.
025060       DISPLAY S06.
	     DISPLAY S42.
025070       MOVE 1              TO WS-PURLAST.
      *
      *                                ENTER CREDIT ACCOUNT NUMBER.
      *
             MOVE SPACES         TO W15-ACNO.

025080 EA15.
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
025090	     ACCEPT S19 AT 0417.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO EA16
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		     PERFORM AA50C THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA15
	       END-EVALUATE
018820	       DISPLAY S19 AT 0417
018830	       IF W15-ACNO = SPACES
018840		   GO TO EA15.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       EA16.
	     DISPLAY CLEAR-L25.
025100     IF W15-ACNO = SPACES
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
025130	       MOVE W15-TOTAL	 TO W16-COST
               MOVE W15-TOTVAT   TO W16-VAT
	       ADD W15-TOTAL W15-TOTVAT
                                 GIVING W16-TOTAL
               MOVE W16-TOTALS   TO DS-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
025150	       MOVE SPACES	 TO R-DET
025160	       GO TO EA999.
025190	     PERFORM CA155-GET-CREDITOR.
025200     IF WS-ERROR NOT = ZERO
012280	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
025220	       GO TO EA15.
025230       MOVE P-NAME         TO W15-NAME.
025240	     DISPLAY P-NAME AT 0424
		     WITH FOREGROUND-COLOR 1
			  BACKGROUND-COLOR 7.
           IF P-TAX = "E"
	       DISPLAY "(Exclusive)" AT 0465
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
           ELSE
           IF P-TAX = "I"
	       DISPLAY "(Inclusive)" AT 0465
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
           ELSE
	       DISPLAY "(No VAT)" AT 0465
			WITH FOREGROUND-COLOR 3 HIGHLIGHT.
             DISPLAY S-CRED.
	     PERFORM CHECK-CORRECT.
025320	   IF WS-OPTION = "N"
025330         GO TO EA15.

       EA19.
025350       MOVE ZERO           TO W30-CUR W30-30  
025360                              W30-120 W30-60  
                                    W30-150 W30-90
                                    W30-180 W15-TXBL.
025370	     MOVE ZERO		 TO W15-GRND W15-INVD.
	     MOVE SPACES	 TO W15-GRNN W15-INVN.
025380	   IF W02-LINAGE NOT < (W02-PRN-LENGTH - 8)
025390         PERFORM EA05.

025400 EA20.
025410	     ACCEPT S27 AT 0510.
025420	   IF W15-GRNN = SPACES
025430         GO TO EA20.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-GRNN
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

025440 EA25.
025450       ACCEPT S28 AT 0530.
025460       MOVE W15-GRND       TO W15-DATE.
025470       PERFORM AD000.
025480     IF WS-ERROR = 1
025490         GO TO EA25.
	     MOVE W15-DD	 TO W20-DD W25-DD.
	     MOVE W15-MM	 TO W20-MM W25-MM.
	     MOVE W15-CC	 TO W20-CC W25-CC.
	     MOVE W15-YY	 TO W20-YY W25-YY.

025500 EA30.
025510	     ACCEPT S29 AT 0550.
025520	   IF W15-INVN = SPACES
025530         GO TO EA30.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-INVN
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

025540 EA35.
025550	     ACCEPT S30 AT 0570.
025560       MOVE W15-INVD       TO W15-DATE.
025570       PERFORM AD000.
025580     IF WS-ERROR = 1
025590         GO TO EA35.
	     PERFORM CHECK-CORRECT.
025680	   IF WS-OPTION = "N"
025690         GO TO EA20.
025700	     DISPLAY CLEAR-L25.
	     MOVE W15-DD	 TO W21-DD.
	     MOVE W15-MM	 TO W21-MM.
	     MOVE W15-CC	 TO W21-CC.
	     MOVE W15-YY	 TO W21-YY.
014750	     MOVE ZERO		 TO W15-QUANT W15-TCOST W15-DISCOUNT
                                    W15-DISC1 W15-DISC2 W15-INCVAL
                                    W15-TSELL W15-TXBL  W15-VAT
				    W15-IVAT  W15-RAIL.
	     MOVE 1		 TO WS-S9.

       EA36.
018660	     MOVE ZERO		 TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO EA36.

025720 EA38.
025730	     MOVE SPACES	 TO W10-EXT-ITEM W15-DESCR.
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

025740 EA40.
020730       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT 0717
	   ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT 0717
	       ELSE
021070		   ACCEPT S07 AT 0717
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT 0717
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT 0717
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT 0717
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT 0717
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT 0717
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT 0717
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT 0717
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT 0717
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT 0717
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT 0717
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT 0717
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT 0717.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO EA40A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
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
		 WHEN F8-KEY
		     PERFORM AA100
		     GO TO EA40
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA38
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT 0717
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT 0717
		   ELSE
021070		       DISPLAY S07 AT 0717
		   END-IF
	       END-IF
016580	       IF W10-EXT-ITEM = SPACES
016590		   GO TO EA38.

       EA40A.
025770	   IF W10-EXT-ITEM = SPACES
025780         SUBTRACT 1        FROM WS-PURLAST
025790         GO TO EA72.

       EA41.
025800       PERFORM CA000.
018700	   IF WS-F-ERROR NOT = ZERO
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT 0717
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT 0717
		   ELSE
021070		       DISPLAY S07 AT 0717
		   END-IF
	       END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO EA40
               ELSE
                   GO TO EA41.
025840	     DISPLAY CLEAR-L25.
      *
      *    ****    GET THE APPLICABLE VAT RATE (W05-RATE, WS-VAT-SUB)
      *
	     MOVE STK-TAX	 TO WS-VAT-SUB.
      *
      *    ****    DOES THE PREVIOUS RATE APPLY
      *
	   IF (WS-VAT-SUB > 0 AND < 7)
	       IF W21-DTE < WS-VAT-DATE(WS-VAT-SUB)
		   ADD 6	 TO WS-VAT-SUB.
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE
	   ELSE
	       MOVE ZERO	 TO W05-RATE.
025850	   IF STK-IND = 3
025860         MOVE ZERO         TO W15-ICOST W15-USELL
025870     ELSE
025880         MOVE STK-COST     TO W15-ICOST
025890         MOVE STK-SELL     TO W15-USELL.
      *
      *    ****    I S	 I T   A   T E X T   I T E M
      *
025900	   IF STK-IND = 3
025910	       ACCEPT W15-DESCR AT 0817
		      WITH FOREGROUND-COLOR 0
			   BACKGROUND-COLOR 7 AUTO
025920     ELSE
025930         MOVE STK-DESC     TO W15-DESCR
025940	       DISPLAY STK-DESC AT 0817
		       WITH FOREGROUND-COLOR 1
			    BACKGROUND-COLOR 7.
025950       MOVE ZERO           TO W15-IUN.
      *
      *    ****    Q U A N T I T Y
      *
025960 EA45.
025970       ACCEPT S37 AT 0917.
025980     IF W15-IUN = ZERO
025990         GO TO EA40.
      *
      *    ****    C A L C U L A T E   I N C L U S I V E   C O S T
      *
	   IF (P-TAX = "I") AND (STK-TAX > ZERO)
	      COMPUTE W15-ICOST ROUNDED = W15-ICOST +
		     (W15-ICOST * W05-RTE).
      *
      *    ****    U N I T   C O S T
      *
026000 EA50.
026010       ACCEPT S38 AT 1017.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO EA45
		 WHEN OTHER
		     CALL X"E5"
		     GO TO EA50
	       END-EVALUATE.
026020	   IF W15-ICOST = ZERO
026030	       GO TO EA50.
026050       DISPLAY S38 AT 1317.
	     MOVE ZERO		 TO W15-U-TXBL
				    W15-U-NTXBL
				    W15-U-VAT.
	   IF STK-TAX NOT = ZERO
               IF P-TAX = "N"
		   MOVE W15-ICOST
				 TO W15-U-NTXBL
                                    W15-U-INCVAL
				    W15-U-NETT
               ELSE
               IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E   V A L U E
      *
		   MOVE W15-ICOST
				 TO W15-U-INCVAL
      *
      *    ****    E X C L U S I V E   V A L U E
      *
		   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL -
                       ((W15-U-INCVAL / (100.00000 + W05-RATE)) 
                       * W05-RATE)
                   MOVE W15-U-TXBL
				 TO W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
		   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL
                                 GIVING W15-U-VAT
               ELSE
      *
      *    ****    E X C L U S I V E   V A L U E
      *
		   MOVE W15-ICOST
				 TO W15-U-TXBL
				    W15-U-NETT
      *
      *    ****    V A T   V A L U E
      *
		   COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE
      *
      *    ****    I N C L U S I V E   V A L U E
      *
		   ADD W15-U-TXBL W15-U-VAT
                                 GIVING W15-U-INCVAL
           ELSE
      *
      *    ****    N O N   T A X A B L E
      *
	       MOVE W15-ICOST	 TO W15-U-NTXBL
				    W15-U-INCVAL
				    W15-U-NETT.
      *
      *    ****    T O T A L   C O S T	 ( L I N E )
      * 	   (W15-COST)
      *
026060	     MULTIPLY W15-IUN BY W15-ICOST
				 GIVING W15-COST ROUNDED.
026080       DISPLAY S40 AT 1417.
      *
      *      ***** CALCULATE COST (IN/EXCLUSIVE) PRICE AFTER DISCOUNT
      * 	   (W15-ICOST)
      *
026090       ACCEPT S35 AT 1123.
026100     IF W15-DISC1 = ZERO
026110         GO TO EA55.
026120       COMPUTE W25-V1 ROUNDED = (W15-COST -
026130		     (W15-COST * W15-DSC1)).
026140	     COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
026160	     DISPLAY S38 AT 1317.
026170       MOVE W25-V1         TO W15-COST.
026080       DISPLAY S40 AT 1417.
      *
      *            2nd DISCOUNT
      *
026180	     ACCEPT S36 AT 1223.
026190     IF W15-DISC2 = ZERO
026200         GO TO EA55.
026210       COMPUTE W25-V1 ROUNDED = (W15-COST -
026220		     (W15-COST * W15-DSC2)).
026230	     COMPUTE W15-ICOST ROUNDED = W25-V1 / W15-IUN.
026250	     DISPLAY S38 AT 1317.
026260       MOVE W25-V1         TO W15-COST.
026290       DISPLAY S40 AT 1417.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   C O S T
      * 	   (W15-UCOST)
      *
026270 EA55.
	   IF (P-TAX = "I") AND (STK-TAX > ZERO)
	       COMPUTE W15-UCOST ROUNDED = W15-ICOST -
		      (W15-ICOST / (100.0000 + W05-RATE) * W05-RATE)
	   ELSE
	       MOVE W15-ICOST	 TO W15-UCOST.

       EA55A.
026310	     ACCEPT S32 AT 1617.
026320	   IF W15-USELL = ZERO OR W15-USELL < W15-UCOST
	       MOVE "Retail price < cost price"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
026330	       GO TO EA55A.
026340       MULTIPLY W15-IUN BY W15-USELL 
                                 GIVING W15-SELL.
026350       MOVE W15-SELL       TO W25-SVAL.
026360	     DISPLAY W25-S5V2 AT 1717
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.

026370 EA65.
	     MOVE P-TERMS	 TO W15-SET.
	   IF NOT (W21-MM = W05-MM)
	       IF W21-CY < W05-CY
		   ADD 12 W05-MM GIVING WS-IXS4
		   SUBTRACT W21-MM
				 FROM WS-IXS4
	       ELSE
		   SUBTRACT W21-MM FROM W05-MM
				 GIVING WS-IXS4
	       END-IF
	       PERFORM EA71 THRU EA71E.
026380	     ACCEPT W15-30 AT 1919
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
026390	     ACCEPT W15-60 AT 1926
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
026400	     ACCEPT W15-90 AT 1933
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
026410	     ACCEPT W15-120 AT 1941
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
026430	     ACCEPT W15-150 AT 1949
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
026450	     ACCEPT W15-180 AT 1957
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
026540	   IF WS-OPTION = "N"
026550         GO TO EA40.
	     MOVE ZERO		 TO W15-IVAT.
	   IF STK-TAX > ZERO
	       IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E
      * 	   (W15-INCVAL)
      *
		   ADD W15-COST  TO W15-INCVAL
		   COMPUTE W15-IVAT ROUNDED = W15-COST / (100.0000
			  + W05-RATE) * W05-RATE
		   COMPUTE W15-TXBL ROUNDED = W15-TXBL +
			  (W15-COST - W15-IVAT)
	       ELSE
      *
      *    ****    E X C L U S I V E
      * 	   (W15-TXBL)
      *
		   COMPUTE W15-IVAT ROUNDED = W15-COST * W05-RTE
		   ADD W15-COST	 TO W15-TXBL.
      *
      *    ****    T O T A L   C O S T	 ( D / N O T E )
      * 	   (W15-TCOST)
      *
	   IF NOT (WS-VAT-SUB = ZERO)
	       ADD W15-TXBL	 TO W15-TAXVAL(WS-VAT-SUB)
	       ADD W15-IVAT	 TO W15-VATVAL(WS-VAT-SUB).
	     ADD W15-COST	 TO W15-TCOST.
026580       MOVE WS-PURLAST     TO WS-PURKEY.
026590       INITIALIZE PUR-RECORD1.
026600       MOVE W15-SETT       TO PUR-TERM.
026610	     MOVE W10-EXT-ITEM	 TO PUR-EXT-STOCK.
026620       MOVE W15-IUN        TO PUR-IUN.
026630*      MOVE W15-ICOST	 TO PUR-ICOST.
026630	     MOVE W15-UCOST	 TO PUR-ICOST.
026630	     MOVE W15-ICOST	 TO PUR-INCVAL.
026640       MOVE W15-USELL      TO PUR-USELL.
026650       MOVE W15-COST       TO PUR-COST.
026660       MOVE W15-SELL       TO PUR-SELL.
026670	     PERFORM WRITE-PURCH THRU WRITE-PURCH-EXIT.
      *                                DISPLAY RUNNING TOTAL.
026690       DISPLAY S44 AT 2066.

026720 EA70.
026730	     DISPLAY S42B.

       EA70-CHK.
016400	     MOVE "'N'ext Item or 'E'nd  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
016420 EA70A.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "N" OR "E")
	       GO TO EA70-CHK.
	   IF NOT (WS-OPTION = "N" OR "E")
               GO TO EA70A.
016490     IF WS-OPTION = "E"
016500         GO TO EA72.
026770       ADD 1               TO WS-PURLAST.
026790       GO TO EA38.
       EA71.
	     MOVE W15-L5	 TO W15-F5.
	     MOVE ZERO		 TO W15-180.
	     SUBTRACT 1 	 FROM WS-IXS4.
	   IF WS-IXS4 > ZERO
	       GO TO EA71.
       EA71E.
	     EXIT.
026800 EA72.
	     DISPLAY CLEAR-L25.
026810     IF WS-PURLAST = ZERO
026820         GO TO EA10.
016610 EA73.
016600	     MOVE "P"	     TO WS-OPTION.
	     MOVE SPACES     TO WS-ERR-MES.
016580	     STRING "'P'rocess, 'C'ancel or 'M'ore items  ["
		    DELIMITED SIZE
		    WS-OPTION DELIMITED SIZE
		    "]" DELIMITED SIZE
			     INTO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "P" OR "M")
	       GO TO EA73.
026940     IF WS-OPTION = "C"
026950         GO TO EA10.
           IF WS-OPTION = "M"
               GO TO EA70-CHK.
       EA74.
	     MOVE 1		 TO WS-S9.
	     MOVE ZERO		 TO W15-VAT.

       EA74-TAX.
	     ADD W15-VATVAL(WS-S9)
				 TO W15-VAT.
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO EA74-TAX.
	     MOVE W15-VAT	 TO WS-VAT.
      *      COMPUTE W15-VAT ROUNDED = W15-TXBL * W05-RTE.
	   IF P-TAX = "E"
	       ACCEPT S43 AT 2166.
	   IF P-TAX = "E"
	       ADD W15-TCOST W15-VAT
				 GIVING W25-SVAL
	   ELSE
	       MOVE W15-TCOST	 TO W25-SVAL.
	     DISPLAY W25-S5V2 AT 2266
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     PERFORM CHECK-CORRECT.
016860	   IF WS-OPTION = "N"
016870         GO TO EA74.
026980       MOVE ZERO           TO WS-PURKEY.
026990	     DISPLAY "Processing Debit Note" AT 2512 WITH
027000		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
      *
      *    ****    Start Recovery Logging.
      *
027010	     PERFORM AY60 THRU AY999.
      *
      *    ****    Lock Creditor record and Log to Recovery file
      *
             MOVE 0              TO WS-ACTION.
027040       PERFORM AY07 THRU AY59.
      *                                PRINT DEBIT NOTE HEADING.
027050 EA75.
027060       MOVE WS-HCRED       TO DS-H4.
027070       MOVE "D/NOTE NO:"   TO DS-H5.
027080       MOVE WS-H1          TO DS-H6 DS-H8.
027090       MOVE "INVOICE NO:"  TO DS-H7.
027100       MOVE W15-ACNO       TO DS-ACNO.
027110       MOVE W15-NAME       TO DS-NAME.
027120       MOVE W15-GRNN       TO DS-DNO.
027130       MOVE W15-GRND       TO DS-DNDTE.
027140       MOVE W15-INVN       TO DS-INV.
027150       MOVE W15-INVD       TO DS-IDTE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
027170	     MOVE SPACES	 TO R-DET.
027180       MOVE "ITEM CODE"    TO DS-H9.
027190       MOVE "DESCRIPTION"  TO DS-H10.
027200       MOVE "UNT COST"     TO DS-H11.
027210       MOVE "UNT-SELL"     TO DS-H12.
027220       MOVE "QUANT"        TO DS-H13.
027230       MOVE "TOT COST"     TO DS-H14.
027240       MOVE "TOT SELL"     TO DS-H15.
027250       MOVE "30  60  90 120 150 180" TO DS-H16.
	     PERFORM CALL-PRINTUTL.
027270	     MOVE SPACES	 TO R-DET.

027280 EA80.
027290     IF WS-PURKEY NOT < WS-PURLAST
027300         GO TO EA125.
027310       ADD 1               TO WS-PURKEY.
027320       PERFORM READ-PURCH THRU READ-PURCH-EXIT.
      *
      *    ****    Lock Stock record and Log to Recovery file
      *
027330	     MOVE PUR-EXT-STOCK	 TO STK-CODE.
             MOVE 0              TO WS-ACTION.
027350       PERFORM AY05 THRU AY59.
027360       MOVE PUR-IUN        TO W15-IUN.
027370	     MOVE PUR-INCVAL	 TO W15-ICOST.
027370	     MOVE PUR-ICOST	 TO W15-UCOST.
027380	     MOVE PUR-USELL	 TO W15-USELL.
027390       MOVE PUR-COST       TO W15-COST.
027400       MOVE PUR-SELL       TO W15-SELL.
027410	     MOVE STK-CODE	 TO W10-EXT-ITEM.
027420	     MOVE STK-DESC	 TO W15-DESC.
027430       MOVE PUR-TERM       TO W15-SETT.
      *
      *    ****    U P D A T E   S T O C K   R E C O R D
      *
027440	     MULTIPLY W15-IUN BY W15-UCOST
                                 GIVING W25-V2 ROUNDED.
027450	   IF STK-IND > 1
027460         GO TO EA85.
027470       MULTIPLY STK-QUANT BY STK-AVGE 
                                 GIVING W25-V1.
027480       SUBTRACT W15-IUN    FROM STK-QUANT.
027490     IF STK-QUANT > ZERO
027500         SUBTRACT W25-V2   FROM W25-V1
027510         DIVIDE W25-V1 BY STK-QUANT 
                                 GIVING STK-AVGE.
027520	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	     MOVE P-NUMBER	 TO PRD-SUPP.
	     MOVE W10-EXT-ITEM	 TO PRD-EXT-CODE.
	     MOVE W05-DATE-YMD	 TO PRD-DTE.
	     ADD 1		 TO PRD-SEQ.
	     MOVE "D"		 TO PRD-TRAN.
	   IF W15-GRNN NOT = SPACES
040810	       MOVE W15-GRNN	 TO PRD-REF
           ELSE
	       MOVE W15-INVN	 TO PRD-REF.
000250	     MULTIPLY -1 BY W15-IUN
				 GIVING PRD-QNT.
	     MULTIPLY -1 BY W15-COST
				 GIVING PRD-COST.
	     MOVE W15-NAME	 TO PRD-NAME.
      *	     MOVE W15-WHS	 TO PRD-WHSE.
	     PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT
000900	     MOVE 1		 TO WS-ACTION
	     PERFORM AY056 THRU AY59.
      *
      *    ****    U P D A T E   D E P A R T M E N T   C O S T
      *
027530 EA85.
027540	     MOVE STK-LDG	 TO DPT-CODE.
027560       PERFORM AY38 THRU AY59.
027570       ADD W25-V2          TO DPT-PRET-MTD
027580                              DPT-PRET-YTD
                                    DPT-PRET-DAY.
           IF WX-INT = "Y"
	       MOVE W20-DTE	 TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "RETURNS - CREDITORS"
                                 TO WX-NAR
               COMPUTE WX-VALUE = W25-V2 * -1
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 07           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
027590	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    U P D A T E   S T O C K   C O N T R O L
      *
027630       SUBTRACT W25-V2     FROM STK-TOTAL.
027650       MOVE ZERO           TO WS-S.
027660       MOVE 1              TO WS-S2.
      *                                CALCULATE AGEING.
027670 EA90.
027680	     MOVE ZERO		 TO W25-CFLOW(WS-S2).
027690	   IF W15-MONTH(WS-S2) > ZERO
027700         ADD 1             TO WS-S.
027710     IF WS-S2 < 6
027720         ADD 1             TO WS-S2
027730         GO TO EA90.
027740       MOVE ZERO           TO W25-V1 W25-V2.
027750       MOVE 1              TO WS-S3.
027760     IF WS-S < 2
027770         MOVE W15-COST     TO W25-V2
027780         GO TO EA95.
027790       MOVE W15-COST       TO W20-RESULT.
027800       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
027810       SUBTRACT 1          FROM WS-S.
027820       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
027830       ADD 1               TO WS-S.
027840 EA95.
027850       MULTIPLY -1         BY W25-V1.
027860       MULTIPLY -1         BY W25-V2.
027870     IF WS-S = ZERO
027880         GO TO EA120.
027890 EA100.
027900	   IF W15-MONTH(WS-S3) = ZERO
027910         ADD 1             TO WS-S3
027920         GO TO EA100.
027930     IF WS-S > 1
027940	       MOVE W25-V1	 TO W25-CFLOW(WS-S3)
027950         SUBTRACT 1        FROM WS-S
027960         ADD 1             TO WS-S3
027970         GO TO EA100.
027980	     MOVE W25-V2	 TO W25-CFLOW(WS-S3).
027990       MOVE ZERO           TO W25-V1 W25-V2.
028000       MOVE 1              TO WS-S2.
028010 EA105.
028020	   IF W25-CFLOW(WS-S2) = ZERO
028030         GO TO EA110.
028040       ADD W05-MM WS-S2    GIVING WS-S3.
028050     IF WS-S3 > 12
028060         SUBTRACT 12       FROM WS-S3.

028070 EA110.
028080     IF WS-S2 < 6
028090         ADD 1             TO WS-S2
028100         GO TO EA105.
028110     IF W25-V2 NOT = ZERO
028120         GO TO EA120.
028130       MOVE 1              TO WS-S WS-S2.

028140 EA115.
028150	   IF W15-MONTH(WS-S) NOT = ZERO
028160         ADD WS-S2         TO CT-AGE.
028170     IF WS-S < 6
028180         MULTIPLY 2        BY WS-S2
028190         ADD 1             TO WS-S
028200         GO TO EA115.
      *                                ACCUMULATE TOTALS.
028210 EA120.
028220       ADD W15-SELL        TO W15-TSELL.
028230	     ADD W25-CFLOW(1)	 TO W30-30.
028240	     ADD W25-CFLOW(2)	 TO W30-60.
028250	     ADD W25-CFLOW(3)	 TO W30-90.
028260	     ADD W25-CFLOW(4)	 TO W30-120.
028270	     ADD W25-CFLOW(5)	 TO W30-150.
028280	     ADD W25-CFLOW(6)	 TO W30-180.
028290       ADD W25-V2          TO W30-CUR.
      *
      *      ****    P R I N T   I T E M   D E T A I L S
      *
028300	     MOVE W10-EXT-ITEM	 TO DS-ITEM.
028310	     MOVE W15-DESC	 TO DS-DESC.
028320	     MOVE W15-ICOST	 TO DS-UCOST.
028330       MOVE W15-USELL      TO DS-USELL.
028340       MOVE W15-IUN        TO DS-QNT.
028350       MOVE W15-COST       TO DS-COST.
028360       MOVE W15-SELL       TO DS-SELL.
028370	   IF W15-MONTH(1) > ZERO
028380         MOVE "Y"          TO DS-30
028390     ELSE
028400         MOVE "N"          TO DS-30.
028410	   IF W15-MONTH(2) > ZERO
028420         MOVE "Y"          TO DS-60
028430     ELSE
028440         MOVE "N"          TO DS-60.
028450	   IF W15-MONTH(3) > ZERO
028460         MOVE "Y"          TO DS-90
028470     ELSE
028480         MOVE "N"          TO DS-90.
028490	   IF W15-MONTH(4) > ZERO
028500         MOVE "Y"          TO DS-120
028510     ELSE
028520         MOVE "N"          TO DS-120.
028530	   IF W15-MONTH(5) > ZERO
028540         MOVE "Y"          TO DS-150
028550     ELSE
028560         MOVE "N"          TO DS-150.
028570	   IF W15-MONTH(6) > ZERO
028580         MOVE "Y"          TO DS-180
028590     ELSE
028600         MOVE "N"          TO DS-180.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
028650	       PERFORM EA05
028660         PERFORM EA75.
028670       MOVE SPACES         TO R-DET.
028680       GO TO EA80.
      *                                DEBIT NOTE TOTAL.
028690 EA125.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "SUB-TOTAL"	 TO DS-H13.
028720       MOVE W15-TCOST      TO DS-COST.
028730       MOVE W15-TSELL      TO DS-SELL.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
028790	     MOVE SPACES	 TO R-DET.
             MOVE "VAT"          TO DS-H13.
             MOVE W15-VAT        TO DS-COST.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
028790	     MOVE SPACES	 TO R-DET.
028740       ADD W15-TCOST       TO W15-TOTAL.
	     ADD W15-VAT	 TO W15-TOTVAT.
	   IF P-TAX = "I"
	      SUBTRACT W15-VAT	 FROM W15-TOTAL.
	   IF P-TAX = "E"
	       ADD W15-VAT	 TO W15-TCOST.
028710       MOVE "TOTAL"        TO DS-H13.
028720       MOVE W15-TCOST      TO DS-COST.
028750	   IF W02-LINAGE < (W02-PRN-LENGTH - 10)
	       MOVE 3		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
028790       MOVE SPACES         TO R-DET.
      *
      *    ****    G E N E R A T E   T R A N S A C T I O N
      *
029000 EA130.
	     INITIALIZE CT-REC.
029030	     MOVE W15-GRND	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO CT-DTE.
	   IF W15-GRNN NOT = SPACES
040810         MOVE W15-GRNN     TO CT-REF
           ELSE
               MOVE W15-INVN     TO CT-REF.
029050       MOVE W15-INVN       TO CT-SINV.
029060	     MOVE W15-INVD	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO CT-SDTE.
	     MOVE "U"		 TO CT-TYPE.
	     MOVE "I"		 TO CT-PJ.
029070	     MOVE W15-ACNO	 TO CT-CRED.
	     MOVE SPACES	 TO CT-ORD.
029080	     MOVE ZERO		 TO CT-PAID
				    CT-SEQ.
029090       MULTIPLY -1 BY W15-TCOST 
                                 GIVING CT-COST.
029100       MULTIPLY -1 BY W15-TSELL 
                                 GIVING CT-SELL.
             MULTIPLY -1 BY W15-VAT
                                 GIVING CT-TAX
029110       MOVE 02             TO CT-CODE.
029120	     MOVE ZERO		 TO CT-AGE W15-SETT.
	     COMPUTE CT-TAXFREE = CT-COST -
		 (CT-TAX + (W15-TXBL * -1)).
	     MOVE SPACES	 TO CT-DREF.
	     PERFORM CA600.
           IF WX-INT = "Y"
               MOVE W20-DTE      TO WX-DATE
	       MOVE "  CR"	 TO WX-LEDG
	       MOVE W90-CODE	 TO WX-TRN
               MOVE W00-C-DESC   TO WX-DESC
               MOVE "- CREDITORS"
                                 TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL     TO WX-AC
               MOVE 02           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM CA700.
029130     IF W30-30 NOT = ZERO
029140         MOVE 1            TO W15-30.
029150     IF W30-60 NOT = ZERO
029160         MOVE 1            TO W15-60.
029170     IF W30-90 NOT = ZERO
029180         MOVE 1            TO W15-90.
029190     IF W30-120 NOT = ZERO
029200         MOVE 1            TO W15-120.
029210     IF W30-150 NOT = ZERO
029220         MOVE 1            TO W15-150.
029230     IF W30-180 NOT = ZERO
029240         MOVE 1            TO W15-180.
029250       MOVE 1              TO WS-S WS-S2.
029260 EA140.
029270	   IF W15-MONTH(WS-S) NOT = ZERO
029280         ADD WS-S2         TO CT-AGE.
029290     IF WS-S < 6
029300         ADD 1             TO WS-S
029310         MULTIPLY 2        BY WS-S2
029320         GO TO EA140.
029330 EA145.
029340       ADD CT-COST         TO P-BAL.
029350       ADD CT-COST         TO P-DT.
029360       ADD W30-180         TO P-180D.
029370     IF P-180D < ZERO
               SUBTRACT P-180D   FROM W30-180
029380         ADD P-180D        TO W30-150
029390         MOVE ZERO         TO P-180D.
029400       ADD W30-150         TO P-150D.
029410     IF P-150D < ZERO
               SUBTRACT P-150D   FROM W30-150
029420         ADD P-150D        TO W30-120
029430         MOVE ZERO         TO P-150D.
029440       ADD W30-120         TO P-120D.
029450     IF P-120D < ZERO
               SUBTRACT P-120D   FROM W30-120
029460         ADD P-120D        TO W30-90
029470         MOVE ZERO         TO P-120D.
029480       ADD W30-90          TO P-90D.
029490     IF P-90D < ZERO
               SUBTRACT P-90D    FROM W30-90
029500         ADD P-90D         TO W30-60
029510         MOVE ZERO         TO P-90D.
029520       ADD W30-60          TO P-60D.
029530     IF P-60D < ZERO
               SUBTRACT P-60D    FROM W30-60
029540         ADD P-60D         TO W30-30
029550         MOVE ZERO         TO P-60D.
029560       ADD W30-30          TO P-30D.
029570     IF P-30D < ZERO
               SUBTRACT P-30D    FROM W30-30
029580         ADD P-30D         TO W30-CUR
029590         MOVE ZERO         TO P-30D.
029600       ADD W30-CUR         TO P-OUT.
029610       PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY08 THRU AY59.
029630	     PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
      *
      *    ****    U P D A T E   C R E D I T O R   C O N T R O L
      *
029680       MULTIPLY -1         BY W15-TCOST.
029690       ADD W15-TCOST       TO W70-CLOSE.
029760       ADD W15-TCOST       TO W70-DEBT.
019120       ADD W30-180         TO W70-DUE-180.
019130       ADD W30-150         TO W70-DUE-150.
019140       ADD W30-120         TO W70-DUE-120.
019150       ADD W30-90          TO W70-DUE-90.
019160       ADD W30-60          TO W70-DUE-60.
019170       ADD W30-30          TO W70-DUE-30.
             ADD W30-CUR         TO W70-DUE-NOW.
      *
      *    ****    VAT Control 
      *
	     MOVE 1		 TO W05-V-RATE.

       EA146.
	     ADD 6 W05-V-RATE	 GIVING WS-VAT-SUB.
	   IF (W15-VATVAL(W05-V-RATE) = ZERO) AND
	      (W15-VATVAL(WS-VAT-SUB) = ZERO)
	       GO TO EA147.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     MOVE 0		 TO WS-ACTION.
             PERFORM AY38 THRU AY59.
	     ADD W15-VATVAL(W05-V-RATE)
				 TO DPT-I-VRET-DAY
				    DPT-I-VRET-MTD
				    DPT-I-VRET-YTD.
	     ADD W15-VATVAL(WS-VAT-SUB)
				 TO DPT-I-VRET-DAY
				    DPT-I-VRET-MTD
				    DPT-I-VRET-YTD.
	     ADD W15-TAXVAL(W05-V-RATE)
				 TO DPT-V-PRET-DAY
				    DPT-V-PRET-MTD
				    DPT-V-PRET-YTD.
	     ADD W15-TAXVAL(WS-VAT-SUB)
				 TO DPT-V-PRET-DAY
				    DPT-V-PRET-MTD
				    DPT-V-PRET-YTD.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       EA147.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO EA146.
	   IF (WX-INT = "Y") AND (W15-VAT NOT = ZERO)
	       COMPUTE WX-VALUE = W15-VAT * -1
	       MOVE DPT-I-VAT-GL TO WX-AC
	       MOVE 12		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.
      *
      *    ****    End Recovery Logging.
      *
       EA148.
	     CLOSE CREDIT.
	     OPEN I-O CREDIT.
029780	     PERFORM AY70 THRU AY999.
029790	     CLOSE PURCH.
029800       OPEN OUTPUT PURCH.
029810       CLOSE PURCH.
029820	     OPEN I-O PURCH.
029840       GO TO EA10.

029880 EA999.
029890       EXIT.
      /
019640 ZA000-INIT    SECTION 89.
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
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ****  C R E D I T O R   T R A N S A C T I O N   F I L E  ****
      *    *************************************************************
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
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
	     MOVE "PURCH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-PURCH.
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
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
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
042300       OPEN OUTPUT PURCH.
050440     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
042310       CLOSE PURCH.
042320       OPEN I-O PURCH.
019960       MOVE 1              TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980	     MOVE PAR-COMPANY	 TO W60-COMPANY.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
001070       MOVE PAR-DEBGL      TO WX-DEBGL.
001080       MOVE PAR-CREGL      TO WX-CREGL.
001090       MOVE PAR-INTGL      TO WX-INTGL.
001100       MOVE PAR-BNKGL      TO WX-BNKGL.
001110       MOVE PAR-UNPROF     TO WX-UNPROF.
001120       MOVE PAR-REDGL      TO WX-REDGL.
001130       MOVE PAR-ADJGL      TO WX-ADJGL.
001140       MOVE PAR-RLGL       TO WX-RLGL.
001150       MOVE PAR-DSGL       TO WX-DSGL. 
042540	     DISPLAY CLR-SCREEN.
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
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1)
				    WS-TCR(WS-S1) WS-BCR(WS-S1).
041250	     MOVE WS-G8 	 TO WS-TCH(WS-S1) WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
021930	     MOVE WS-G2 	 TO WS-TCHR(18) WS-TCHR(49)
021940				    WS-TCHR(60) WS-TCHR(70)
021930				    WS-TCR(8) WS-TCR(50)
021940				    WS-TCR(66).
021950	     MOVE WS-G3 	 TO WS-MCHR(1)	WS-MCHR(80)
				    WS-MCHR(18) WS-MCHR(49)
021960				    WS-MCHR(60) WS-MCHR(70)
021950				    WS-MCR(8)	WS-MCR(50)
021960				    WS-MCR(66)	WS-MCR(1)
				    WS-MCR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(18) WS-BCHR(49)
021980				    WS-BCHR(60) WS-BCHR(70)
021970				    WS-BCR(8)	WS-BCR(50)
021980				    WS-BCR(66).
051190	     MOVE WS-G9 	 TO WS-TCH(1).
051200	     MOVE WS-G10	 TO WS-TCH(80).
051210	     MOVE WS-G11	 TO WS-BCH(1).
051220	     MOVE WS-G12	 TO WS-BCH(80).
051230	     MOVE WS-G14	 TO WS-TCR(1)	WS-BCR(1)
				    WS-TCHR(1)	WS-BCHR(1).
051260	     MOVE WS-G13	 TO WS-TCR(80)	WS-BCR(80)
				    WS-TCHR(80) WS-BCHR(80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
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
020210 ZB000-I-O-ERROR SECTION 90.
      
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660	       MOVE W02-CREDIT	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041680	   ELSE
041690     IF WS-F-ERROR = 4
041700	       MOVE W02-CRTRAN	 TO WS-FILE
041710	       MOVE ZERO	 TO WS-KEY
	       MOVE CT-KEY	 TO WS-KEYX
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
052220	       MOVE ZERO	 TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 35
041860	       MOVE W02-CRETRN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE CRT-KEY	 TO WS-KEYX
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
041850	   IF WS-F-ERROR = 56
041860	       MOVE W02-PURDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRD-KEY	 TO WS-KEYX.
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
