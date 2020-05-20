      $set LINKCOUNT"596"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	     **     ********   *
      *   **	**  **	  **  **    **	 **  **     ***     **	       *
      *   **	    **	  **  **    **	**    **     **     *******    *
      *   **	    ******    *******	**    **     ** 	  **   *
      *   **	    **	 **   **	**    **     ** 	  **   *
      *   **	**  **	  **  **	 **  **      **     **	  **   *
      *    ******   **	  **  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   O R D E R S			       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP015.
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

       COPY DEPART.SL.

000140 COPY PARAM.SL.

000260 COPY PORDER.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000140 COPY STKMEM.SL.

000200 COPY STOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000260 COPY PORDER.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000140 COPY STKMEM.FDE.

000200 COPY STOCK.FDE.

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
000630 77  WS-ENT          PIC  X(08).
000640 77  WS-ENT1         PIC  X(08).
003450 77  WS-INDS         PIC  9(01)    VALUE 0.
003460 77  WS-INDI         PIC  9(01)    VALUE 0.
003470 77  WS-S            PIC  9(04)    COMP-5.
003480 77  WS-S1           PIC  9(04)    COMP-5.
003490 77  WS-S2           PIC  9(04)    COMP-5.
003500 77  WS-S3           PIC  9(04)    COMP-5.
003510 77  WS-S4           PIC  9(04)    COMP-5.
003510 77  WS-S9           PIC  9(04)    COMP-5.
003520 77  WS-ITM	   PIC	X(18).
003520 77  WS-ITM1	   PIC	X(18).
003530 77  WS-ORD          PIC  X(01).
003540 77  WS-OPTION       PIC  X(01).
003540 77  WS-SKIP         PIC  X(01) VALUE "Y".
003540 77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
003560 77  WS-CHAR1	   PIC	X(01).
003580 77  WS-IND	   PIC  9(01) VALUE 0.
003610 77  WS-SKEY         PIC  9(05).
003620 77  WS-KEY1         PIC  9(04)     COMP-5.
003630 77  WS-LINES        PIC  9(04)     COMP-5 VALUE 0.
003640 77  WS-PAGE         PIC  9(04)     COMP-5 VALUE 0.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003700 77  WS-ER1	   PIC X(09)	 VALUE "No record".
       77  WS-ER2          PIC X(25)     VALUE 
                           "May not be less than cost".
000930 77  WS-ER8          PIC  X(09) VALUE "No record".
003710 77  WS-HDTE         PIC X(04)     VALUE "DATE".
003720 77  WS-HACNO        PIC X(04)     VALUE "ACNO".
003730 77  WS-HCRED        PIC X(09)     VALUE "CREDITOR:".
003740 77  WS-HQNT         PIC X(05)     VALUE "QUANT".
003750 77  WS-HDESC        PIC X(11)     VALUE "DESCRIPTION".
003760 77  WS-HCOST        PIC X(10)     VALUE "COST PRICE".
003770 77  WS-HSELL        PIC X(10)     VALUE "SELL PRICE".
003780 77  WS-HNAME        PIC X(04)     VALUE "NAME".
003790 77  WS-HAGE         PIC X(12)     VALUE "AGE 30 - 180".
003800 77  WS-HCHQ         PIC X(06)     VALUE "CHEQUE".
003810 77  WS-HSETT        PIC X(22)     VALUE
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
003920 77  WS-SCHQ         PIC X(06)     VALUE "Cheque".
003930 77  WS-SSETT        PIC X(22)     VALUE
003940                     "30  60  90 120 150 180".
003950 77  WS-H1           PIC X(05)     VALUE "DATE:".
003960 77  WS-H3           PIC X(05)     VALUE "PAGE:".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
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
	   03  WS-PROG	       PIC  X(03) VALUE "015".

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

       COPY DEPART.ID.

       COPY PARAM.ID.

000260 COPY PORDER.ID.

000280 COPY RECOVER.ID.

       COPY SHARED.ID.

000140 COPY STKMEM.ID.

000200 COPY STOCK.ID.

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
	       05  G-H3 	   PIC X(05).
	       05  G-INVN	   PIC Z(06)9.
	       05  FILLER	   PIC X(01).
	       05  G-H4 	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  G-INVD	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  G-H5 	   PIC X(06).
	       05  G-RAIL	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H6 	   PIC X(06).
	       05  G-DISC	   PIC Z(04)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H6A	   PIC X(05).
	       05  G-VAT	   PIC Z(05)9.99-.
	       05  FILLER	   PIC X(01).
	       05  G-H7 	   PIC X(06).
	       05  G-TOTAL	   PIC Z(06)9.99-.

	   03  R-L5 REDEFINES W02-PRN-LINE.
	       05  J-H1 	   PIC X(07).
	       05  J-H2 	   PIC X(39).
	       05  J-H3 	   PIC X(10).
	       05  J-H4 	   PIC X(15).
	       05  J-H5 	   PIC X(13).
	       05  J-H6 	   PIC X(12).

	   03  R-L5A REDEFINES W02-PRN-LINE.
	       05  J-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  J-NAME	   PIC X(35).
	       05  FILLER	   PIC X(01).
	       05  J-DATE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  J-COST	   PIC Z(08)9.99-.
	       05  FILLER	   PIC X(02).
	       05  J-SELL	   PIC Z(08)9.99-.
	       05  FILLER	   PIC X(02).
	       05  J-30 	   PIC X(02).
	       05  J-60 	   PIC X(02).
	       05  J-90 	   PIC X(02).
	       05  J-120	   PIC X(02).
	       05  J-150	   PIC X(02).
	       05  J-180	   PIC X(02).

	   03  R-L8 REDEFINES W02-PRN-LINE.
	       05  OR-H1	 PIC  X(12).
	       05  OR-H2.
		 07  FILLER	 PIC  X(04).
		 07  OR-H2A	 PIC  X(27).
	       05  OR-H3	 PIC  X(06).
	       05  OR-H4	 PIC  X(12).
	       05  OR-H5	 PIC  X(10).
	       05  OR-H6	 PIC  X(09).
	       05  OR-H7	 PIC  X(22).
	       05  OR-H8	 PIC  X(13).
	       05  OR-H9	 PIC  X(09).
	       05  OR-H10	 PIC  X(08).

	   03  R-L8A REDEFINES W02-PRN-LINE.
	       05  OR-ORD	 PIC  Z(07)9/99.
	       05  FILLER	 PIC  X(01).
	       05  OR-DESC.
		 07  OR-EXT-ITEM.
		   09  OR-ITEM	 PIC  X(14).
		   09  OR-SL	 PIC  X(01).
		   09  OR-ITM	 PIC  X(03).
		 07  FILLER	 PIC  X(12).
	       05  FILLER	 PIC  X(01).
	       05  OR-QUANT	 PIC  Z(04)9.
	       05  FILLER	 PIC  X(01).
	       05  OR-CURRENCY.
		 07  FILLER	 PIC  X(01).
		 07  OR-RATE	 PIC  Z(02)9.99.
		 07  FILLER	 PIC  X(03).
	       05  FILLER	 PIC  X(01).
	       05  OR-COST	 PIC  Z(06)9.99.
	       05  FILLER	 PIC  X(01).
	       05  OR-PMT.
		 07  FILLER	 PIC  X(01).
		 07  OR-DISC	 PIC  Z9.99 BLANK WHEN ZERO.
		 07  FILLER	 PIC  X(02).
	       05  FILLER	   PIC X(01).
	       05  OR-NAME.
		   07  OR-CONTACT.
		     09  OR-ACNO   PIC X(06).
		     09  FILLER    PIC X(15).
		   07  FILLER	   PIC X(01).
		   07  OR-TEL	   PIC X(12).
	       05  FILLER	   PIC X(01).
	       05  OR-DEL	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(01).
	       05  OR-RMKS.
		   07  OR-PSLIP    PIC X(06).

	   03  R-L9 REDEFINES W02-PRN-LINE.
	       05  REP-PG	 PIC X(06).
	       05  REP-P-N	 PIC Z(03)9.
	       05  FILLER	 PIC X(16).
	       05  REP-COMP	 PIC X(40).
	       05  FILLER	 PIC X(02).
	       05  REP-REPORT	 PIC X(40).
	       05  FILLER	 PIC X(08).
	       05  REP-DAT	 PIC X(06).
	       05  REP-DT	 PIC Z9/99/9999.

       COPY W05.WS.

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
           03  W05-HLF-VAL     PIC  9(01)V99.
           03  W05-TQTR-VAL    PIC  9(01)V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

004420 01  W10-MONTHS.
004430	   03  W10-MONTH-NAMES PIC  X(36)     VALUE
004440                        "JanFebMarAprMayJunJulAugSepOctNovDec".
004450     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
004460			       PIC  X(03)     OCCURS 12.

       COPY W10.CRP.

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
004530	   03  W15-ACNO        PIC  X(06).
004540     03  W15-SNO         REDEFINES W15-ACNO.
004550	       05  W15-APREF   PIC  X(01).
004560	       05  W15-SERIAL  PIC  X(05).
004580	   03  W15-NAME.
001490	       05  W15-NCHAR   PIC  X(01) OCCURS 40.
001500     03  W15-N           REDEFINES W15-NAME.
001510         05  W15-NIX.
001520             07  W15-ENTRY PIC X(08).
001530		   07  FILLER  PIC  X(16).
001540	       05  FILLER      PIC  X(16).
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
	   03  W15-TEL	       PIC  X(14).
	   03  W15-FAX	       PIC  X(14).
004590	   03  W15-UN	       PIC  9(05)V99.
004600	   03  W15-UNIT REDEFINES W15-UN.
004610	       05  FILLER      PIC  9(01).
004620	       05  W15-UNT     PIC  9(04).
	       05  W15-UDEC    PIC  9(02).
004630	   03  W15-ORD	       PIC  9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650	       05  W15-OPREF   PIC  9(08).
004660	       05  W15-OSUB    PIC  9(02).
	   03  W15-CURRENCY    PIC  X(10).
004700     03  W15-OCOST       PIC S9(09)V999 COMP-3.
004700     03  W15-RATE        PIC S9(03)V999 COMP-3.
004700	   03  W15-COST        PIC S9(09)V999 COMP-3.
004710	   03  W15-SELL        PIC S9(09)V999 COMP-3.
004720	   03  W15-SETT        PIC  9(06).
004730     03  W15-SET         REDEFINES W15-SETT.
004740	       05  W15-30      PIC  9(01).
004750	       05  W15-60      PIC  9(01).
004760	       05  W15-90      PIC  9(01).
004770	       05  W15-120     PIC  9(01).
004780	       05  W15-150     PIC  9(01).
004790	       05  W15-180     PIC  9(01).
004800     03  W15-PAY         REDEFINES W15-SETT.
004810	       05  W15-MONTH   PIC  9(01) OCCURS 6.
004820	   03  W15-DEL	       PIC 9(08).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
	       05  W15-DEL-CC  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
004830	   03  W15-CAT	       PIC  9(02).
004840	   03  W15-GRNN        PIC X(08).
004850	   03  W15-GRND        PIC 9(08)     COMP-3.
004860	   03  W15-INVN        PIC X(08).
004870	   03  W15-INVD        PIC 9(08)     COMP-3.
004880	   03  W15-TCOST       PIC S9(09)V99 COMP-3.
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
002860	   03  W15-CHECK       PIC S9(07)V99  COMP-3.
004960     03  W15-RAIL        PIC S9(09)V99 COMP-3.
004970     03  W15-DISCOUNT    PIC S9(09)V99 COMP-3.
004980	   03  W15-DISC1       PIC  99V99.
	   03  W15-DSC1 REDEFINES W15-DISC1
			       PIC  V9(04).
004990	   03  W15-DISC2       PIC  99V99.
	   03  W15-DSC2 REDEFINES W15-DISC2
			       PIC  V9(04).
	   03  W15-TAXABLE	OCCURS 12.
006790	       05  W15-TAXVAL  PIC S9(09)V99 COMP-3.
	       05  W15-VATVAL  PIC S9(09)V99 COMP-3.
	   03  W15-TXBL        PIC S9(09)V99 COMP-3.
           03  W15-INCVAL      PIC S9(09)V99 COMP-3.
           03  W15-IVAT        PIC S9(07)V99 COMP-3.
	   03  W15-U-VAT       PIC S9(07)V999 COMP-3.
	   03  W15-U-NETT      PIC S9(07)V999 COMP-3.
	   03  W15-U-TXBL      PIC S9(07)V999 COMP-3.
	   03  W15-U-NTXBL     PIC S9(07)V999 COMP-3.
	   03  W15-U-INCVAL    PIC S9(07)V999 COMP-3.
	   03  W15-VAT	       PIC S9(07)V99  COMP-3.
           03  W15-TOTVAT      PIC S9(07)V99 COMP-3.
005000     03  W15-TOTAL       PIC S9(09)V99 COMP-3.
005010     03  W15-IUN         PIC S9(09)V99 COMP-3.
005010	   03  W15-AVRG	       PIC S9(08)V999 COMP-3.
005020     03  W15-ICOST       PIC S9(09)V999 COMP-3.
005020     03  W15-INCOST      PIC S9(09)V999 COMP-3.
005020     03  W15-ILCOST      PIC S9(09)V999 COMP-3.
           03  W15-RMKS        PIC  X(08) VALUE "Account".
           03  W15-PMT         PIC  X(08) VALUE "Deliver".
           03  W15-PSLIP       PIC  X(08).
	   03  W15-PMNT        PIC  X(12).

       01  W16-TOTALS.
           03  FILLER          PIC  X(12) VALUE "TOTAL COST: ".
           03  W16-COST        PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(12) VALUE " TOTAL VAT: ".
           03  W16-VAT         PIC  Z(08)9.99DB.
           03  FILLER          PIC  X(08) VALUE " TOTAL: ".
           03  W16-TOTAL       PIC  Z(08)9.99DB.

005030 01  W20-CALCS.
005040	   03  W20-RESULT      PIC  9(09)V99.
005050     03  W20-RESULT-1    REDEFINES W20-RESULT.
005060	       05  W20-WHOLE   PIC  9(09).
005070	       05  W20-DECIMAL PIC  9(02).
005080	   03  W20-ORD	       PIC  9(07)    COMP-3.
	   03  W20-DATE.
	       05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
			       PIC 9(08).
	   03  W21-DATE.
	       05  W21-CC      PIC  99.
	       05  W21-YY      PIC  99.
	       05  W21-MM      PIC  99.
	       05  W21-DD      PIC  99.
	   03  W21-DTE REDEFINES W21-DATE
			       PIC  9(08).

005030 01  W25-CALCS.
005040	   03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).

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
005150	       05  W25-1       PIC  ZZZZ9.
005160     03  W25-3V2.
005170	       05  W25-2       PIC  ZZ9.99.
005180     03  W25-7V2.
005190	       05  W25-3       PIC  Z(06)9.99.
005200     03  W25-6V2.
005210	       05  W25-3A      PIC  Z(06)9.99.
005220     03  W25-3N.
005230	       05  W25-4       PIC  ZZ9.
002060     03  W25-S7V2A.
002070	       05  W25-CVAL    PIC  Z(06)9.99-.
005240     03  W25-DTE.
005250	       05  W25-DATE    PIC  99/99/9999.
005260     03  W25-6.
005270	       05  W25-NO      PIC  Z(07)9.
005300     03  W25-S5V2.
005310	       05  W25-SVAL    PIC  Z(07)9.99DB.
005320     03  W25-S7V2.
005330	       05  W25-VAL2    PIC  Z(06)9.99DB.
005280     03  W25-5V2 REDEFINES W25-S7V2.
005290	       05  W25-VAL     PIC  Z(07)9.99-.
005320     03  W25-72.
005330         05  W25-VAL3    PIC S9(07)V99.
005340     03  W25-PRC.
005350	       05  W25-IPRC    PIC  Z(06)9.999.

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
002200	       05  W40-ACHAR   PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS   PIC  X(60).
               05  FILLER      PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR   PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

005450 COPY W50.WS.

005470 01  W50-H4.
005480	   03  W50-HEAD        PIC  X(20).

005530 01  W60-H2.
005540	   03  W60-COMPANY     PIC  X(40).
005550	   03  W60-DESC        PIC  X(21)     VALUE
005560                         "CREDITORS AUDIT TRAIL".

005600 01  W65-CREDITOR.
005610     03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.
005110     03  W65-V1          PIC S9(09)V99 COMP-3.
005120     03  W65-V2          PIC S9(09)V99 COMP-3.

005680 COPY W70CRED.WS.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-CNO	      PIC  X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
	       05  W75-CODE   PIC  X(18).
	       05  W75-SELL   PIC  9(07)V99 COMP-3.
	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
	       05  W75-CASH   PIC  9(07)V99 COMP-3.
	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
	       05  W75-VWSALE PIC  9(07)V99 COMP-3.

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC  Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
	       05  W100-PRICE  PIC  X(09).
	       05  FILLER      PIC  X(01).
           03  W100-V4 REDEFINES W100-V2.
	       05  W100-EXT    PIC  Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(05)9.99-.
	   03  W100-S8V2       PIC  Z(07)9.99DB.
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
005700 SCREEN SECTION.

       COPY BLANK.CRT.

006760 01  S08.
006770	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
006780                           VALUE "PURCHASE ORDERS".
006810	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "C".
006820	   03	       COLUMN 13 VALUE "reate, ".
006830	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "D".
006840	   03	       COLUMN 21 VALUE "elete Order - ".
010550	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "E".
010560	   03	       COLUMN 36 VALUE "xit or ".
	   03	       COLUMN 43 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 46 VALUE "ape".

006880 01  S09.
006890	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006900                           VALUE "STOCK PURCHASE ORDERS".

006910 01  S10.
006920     02  S10A.
006930	   03  LINE  4 COLUMN  6 VALUE "Order number   ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
006940	   03  LINE  5 COLUMN  6 VALUE "Item code      ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPC.
	   03	       COLUMN 41 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPC.
006950	   03  LINE  6 COLUMN  6 VALUE "Creditor number".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 38 BACKGROUND-COLOR 5
				 PIC  X(40) FROM WS-SPC.
	   03  LINE  7 COLUMN  6 VALUE "Contact        ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPC.
	   03	       COLUMN 53 VALUE "Telephone".
	   03	       COLUMN 63 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPC.
006970	   03  LINE  8 COLUMN  6 VALUE "Quantity       ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
006980	   03  LINE  9 COLUMN  6 VALUE "Cost price     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 36 VALUE "Rate".
	   03	       COLUMN 43 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.
	   03	       COLUMN 53 VALUE "Currency".
	   03	       COLUMN 63 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.
	   03  LINE 10 COLUMN  6 VALUE "Rand price     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
006990	   03  LINE 11 COLUMN  6 VALUE "Selling -W/Sale".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
	   03	       COLUMN 36 VALUE "-Cash".
	   03	       COLUMN 43 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
	   03	       COLUMN 54 VALUE "-Retail".
	   03	       COLUMN 63 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
	   03  LINE 12 COLUMN  6 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  (Inclusive)  ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
	   03	       COLUMN 43 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
	   03	       COLUMN 63 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
	   03  LINE 13 COLUMN  6 VALUE "Payment method ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.
	   03  LINE 14 COLUMN  6 VALUE "Discount %     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 15 COLUMN  6 VALUE "Total cost     ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 16 COLUMN  6 VALUE "Remarks        ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.
007000	   03  LINE 17 COLUMN  6 VALUE "Delivery date  ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
007010	   02  S10B.
007020	   03  LINE 18 COLUMN  6 VALUE "Settlement     ".
007030	   03  LINE 18 COLUMN 22 VALUE "30".
	   03	       COLUMN 25 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 27 VALUE "60".
	   03	       COLUMN 30 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 32 VALUE "90".
	   03	       COLUMN 35 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 37 VALUE "120".
	   03	       COLUMN 41 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 43 VALUE "150".
	   03	       COLUMN 47 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03	       COLUMN 49 VALUE "180".
	   03	       COLUMN 53 BACKGROUND-COLOR 5
				 PIC  X(01) FROM WS-SPC.
	   03  LINE 19 COLUMN  6 VALUE "Picking slip   ".
	   03	       COLUMN 22 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPC.

007050 01  S11.
007060	   03  LINE  2 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
007070                           VALUE "DELETE STOCK ORDER".

007910 01  S12.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(14) USING W10-ITEM AUTO.

       01  S12-ITM.
005540	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S12-EXT.
005540	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S12-3.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S12-4.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S12-5.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S12-6.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S12-7.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S12-8.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S12-9.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S12-10.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S12-11.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S12-12.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S12-13.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S12-14.
007920	   03	       COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

007450 01  S19.
007460	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W15-ACNO AUTO.

007470 01  S20.
007480	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(04)9.99 USING W15-UN AUTO.

007490 01  S21.
007500	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9 USING W15-OPREF AUTO.

007510 01  S22.
007520	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 99 USING W15-OSUB AUTO.

007530 01  S23.
007540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(06)9.99 USING W15-COST AUTO.

       01  S23C.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(08) USING W15-CURRENCY AUTO.

007530 01  S23O.
007540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.999 USING W15-OCOST AUTO.

007530 01  S23R.
007540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(02)9.999 USING W15-RATE AUTO.

007570 01  S25.
007580	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-DEL AUTO.

007690 01  S31.
007700	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99 USING W15-TCOST AUTO.

007780 01  S35.
007790	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9.99 USING W15-DISC1 AUTO.

007690 01  S49.
007700	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(08) USING W15-PSLIP AUTO.

       01  S50.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(30) USING W15-CONTACT AUTO.

       01  S50A.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W15-TEL AUTO.

007530 01  S51.
           02  S51A.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-UWSALE AUTO.
           02  S51B.
	   03  LINE 11 COLUMN 43 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-UCASH AUTO.
           02  S51C.
	   03  LINE 11 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W15-USELL AUTO.
           02  S51D.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VWSALE.
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VCASH.
	   03	       COLUMN 63 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(06)9.99 USING W10-VSELL.

       COPY CRDLUP.CRT.

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
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
004940	     PERFORM GA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005260	     CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY AA50CS.LUP.

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

       COPY LOCKED.REC.

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

       COPY PRINTUTL.AS4.

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
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12
		     " Create"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011110 AA305.
011120       EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

       COPY SHARED.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010390 AC000-WRITE   SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY PORDER.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

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
011940 AY10.
008160       MOVE 10 TO REC-FILE.
008170       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-PORDER THRU READ-PORDER-EXIT.
008180       MOVE ORD-RECORD1 TO REC-PORDER.
008190       GO TO AY50.

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
012130       MOVE DPT-RECORD     TO REC-DEPART.
012140       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
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
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    R O U N D I N G   O F   S E L L I N G   P R I C E S
      *
      *    This routine will round the selling price as follows:
      *
      *    If the rounding factor is less than 50 cents or greater
      *    than R1.00 but less than five rand, then the selling price
      *    (.CC or R.CC portion) will be rounded to a multiple of the
      *    rounding factor.
      *
      *    If the rounding factor is R1.00, then if the selling price
      *    is greater than 79 cents it will be rounded to the next rand.
      *
      *    If the rounding factor is greater than 49 cents and less than
      *    R1.00 or greater than R4.99 then the selling price (.CC or
      *    R.CC portion) will be changed to the rounding value. If the 
      *    rounding portion of the selling price is greater than the
      *    factor then the system will amend these figures and change
      *    the adjoining value.
      *
       CB100         SECTION.
       CB101.
      *
      *    ROUND TO THE NEXT RAND
      *
           IF W05-ROUND-VAL = 1.00
	       IF W15-RSC = ZERO
                   GO TO CB199
               ELSE
      *
      *    SELLING PRICE LESS THAN ONE RAND
      *
	       IF W15-RSELL < 1.00
      *
      *    SELLING PRICE IS MORE THAN 79 CENTS - CHANGE TO R1.00
      *
		   IF W15-RSC > 79
		       MOVE 1.00 TO W15-RSELL
                       GO TO CB199
                   ELSE
      *
      *    IF THE SELLING PRICE IS LESS THAN 80 CENTS DO NOT ROUND UP
      *
                       GO TO CB199
      *
      *    SELLING PRICE MORE THAN ONE RAND
      *
               ELSE
      *
      *    CENTS PORTION OF PRICE IS LESS THAN TEN CENTS - DROP CENTS
      *
		   IF W15-RSC < 10
		       MOVE ZERO TO W15-RSC
                       GO TO CB199
                   ELSE
      *
      *    ROUND UP TO NEXT R1.00
      *
		       MOVE ZERO TO W15-RSC
		       ADD 1	 TO W15-RSR
                       GO TO CB199.
      *
      *    ROUNDING CENTS ONLY
      *
           IF W05-ROUND-VAL < 1.00
      *
      *    ROUNDING VALUE AND CENTS PORTION ARE EQUAL - EXIT
      *
	       IF W15-RSC = W05-CENTS
                   GO TO CB199
               ELSE
      *
      *    ROUNDING VALUE IS 50 CENTS OR MORE
      *
               IF W05-ROUND-VAL > .50
      *
      *    IF CENTS PORTION TO BE ROUNDED IS LESS THAN ROUNDING VALUE
      *
		   IF W15-RSC < W05-CENTS
                       MOVE W05-CENTS
				 TO W15-RSC
                       GO TO CB199
      *
      *    IF CENTS PORTION TO BE ROUNDED IS MORE THAN ROUNDING VALUE
      *
                   ELSE
                       MOVE W05-CENTS
				 TO W15-RSC
		       ADD 1	 TO W15-RSR
                       GO TO CB199
      *
      *    ROUNDING VALUE IS LESS THAN 50 CENTS
      *
               ELSE       
                   GO TO CB120.
      *
      *    IF ROUNDING VALUE EQUALS THE R.CC OF VALUE TO BE ROUNDED - EXIT
      *
	   IF W15-RSRCC = W05-ROUND-VAL
               GO TO CB199.
      *
      *    SELLING PRICE IS NOT MORE THAN HALF THE ROUNDING FACTOR - EXIT
      *
	   IF W15-RSELL NOT > W05-HLF-VAL
               GO TO CB199.
           IF W05-ROUND-VAL > 5.00
               GO TO CB110.
      *
      *    ROUNDING FACTOR IS LESS THAN FIVE RAND 
      *
      *    R.CC OF VALUE TO BE ROUNDED IS LESS THAN ROUNDING FACTOR
      *
	   IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL
				 TO W15-RSRCC
               GO TO CB199.
             MOVE W05-ROUND-VAL  TO W05-CALC-VAL.
       CB105.
	   IF W15-RSRCC = W05-CALC-VAL
               GO TO CB199.
	   IF W15-RSRCC < W05-CALC-VAL
	       MOVE W05-CALC-VAL TO W15-RSRCC
               GO TO CB199.
             ADD W05-ROUND-VAL   TO W05-CALC-VAL.
           IF W05-CALC-VAL < 10.00
               GO TO CB105.
	     ADD 1		 TO W15-RSR8.
	     MOVE W05-RVRCC	 TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUNDING FACTOR GREATER THAN R5.00
      *
      *    SELLING PRICE IS NOT MORE THAN 75% OF ROUNDING FACTOR - EXIT
      *
       CB110.
	   IF W15-RSELL NOT > W05-TQTR-VAL
               GO TO CB199.
       CB115.
	   IF W15-RSRCC < W05-ROUND-VAL
               MOVE W05-ROUND-VAL 
				 TO W15-RSRCC
               GO TO CB199.
	     ADD 1		 TO W15-RSR8.
	     MOVE W05-ROUND-VAL  TO W15-RSRCC.
             GO TO CB199.
      *
      *    ROUND UP VALUES WHERE ROUNDING FACTOR IS LESS THAN 50 CENTS
      *
       CB120.
	   IF W05-CENTS = 2 OR 4 OR 5 OR 10 OR 20 OR 25
	       MOVE ZERO	 TO WS-SUB
	   ELSE
	       MOVE W05-CENTS	 TO WS-SUB.
       CB125.
	   IF W15-RSC = WS-SUB
               GO TO CB199.
	   IF W15-RSC < WS-SUB
	       MOVE WS-SUB	 TO W15-RSC
               GO TO CB199.
             ADD W05-CENTS       TO WS-SUB.
           IF WS-SUB < 100
               GO TO CB125.
	     ADD 1		 TO W15-RSR.
             SUBTRACT 100 FROM WS-SUB
				 GIVING W15-RSC.
       CB199.
             EXIT.
      *
      *    ****    C A L C U L A T E   E X C L U S I V E   P R I C E
      *              F R O M   T H E   I N C L U S I V E   P R I C E
      *
       CB200         SECTION.
       CB201.
           IF STK-TAX NOT = ZERO
	       COMPUTE W15-USELL ROUNDED= (W10-VSELL /
				   (100.00000 + W05-RATE)) * 100.00
	       COMPUTE W15-UWSALE ROUNDED = (W10-VWSALE /
				    (100.00000 + W05-RATE)) * 100.00
	       COMPUTE W15-UCASH ROUNDED = (W10-VCASH /
				   (100.00000 + W05-RATE)) * 100.00
           ELSE
	       MOVE W10-VWSALE	 TO W15-UWSALE
	       MOVE W10-VCASH	 TO W15-UCASH
	       MOVE W10-VSELL	 TO W15-USELL.
	   IF STK-TAX NOT = ZERO
	       COMPUTE W15-CHECK
		   = W15-USELL + (W15-USELL * W05-RTE)
	       IF W15-CHECK > W10-VSELL
		   SUBTRACT 0.01 FROM W15-USELL
	       ELSE
	       IF W15-CHECK < W10-VSELL
		   ADD 0.01	 TO W15-USELL
	       END-IF
	       COMPUTE W15-CHECK
		   = W15-UCASH + (W15-UCASH * W05-RTE)
	       IF W15-CHECK > W10-VCASH
		   SUBTRACT 0.01 FROM W15-UCASH
	       ELSE
	       IF W15-CHECK < W10-VCASH
		   ADD 0.01	 TO W15-UCASH
	       END-IF
	       COMPUTE W15-CHECK
		   = W15-UWSALE + (W15-UWSALE * W05-RTE)
	       IF W15-CHECK > W10-VWSALE
		   SUBTRACT 0.01 FROM W15-UWSALE
	       ELSE
	       IF W15-CHECK < W10-VWSALE
		   ADD 0.01	 TO W15-UWSALE.
       CB299.
             EXIT.

      /
032190 GA000	     SECTION 50.
032200 GA00A.
032220	   IF LS0-STP = 2
032230         GO TO GA00.
032240       DISPLAY CLR-SCREEN.
032250       DISPLAY "Stock not installed - 'ENTER'" AT 1212
032260                WITH FOREGROUND-COLOR 14.
032270	     ACCEPT WS-OPTION AT 1242
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
032280       MOVE ZERO           TO WS-PAGE.
032290       GO TO GA999.

032300 GA00B.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
008540	     MOVE SPACES	 TO R-DET.
008550       MOVE "Page:"        TO REP-PG.
008560       MOVE W60-COMPANY    TO REP-COMP.
008570       MOVE "Date:"        TO REP-DAT.
008580	     ADD 1		 TO WS-PAGE.
008590       MOVE W05-DATE       TO REP-DT.
008600       MOVE WS-PAGE        TO REP-P-N.
           IF WS-DEL = "D"
032210         MOVE "STOCK PURCHASE ORDERS DELETED" 
                                 TO REP-REPORT
           ELSE
032210         MOVE "STOCK PURCHASE ORDERS" 
                                 TO REP-REPORT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
008650	     MOVE SPACES	 TO R-DET.
032320       MOVE "ORDER.NO"     TO OR-H1.
032330       MOVE "ITEM CODE"    TO OR-H2.
032350       MOVE "QUANT"        TO OR-H3.
             MOVE "CURRENCY"     TO OR-H4.
032360       MOVE "UNIT COST"    TO OR-H5.
             MOVE "PAYMENT"      TO OR-H6.
032370       MOVE "CREDITOR"     TO OR-H7.
             MOVE "FAX NO"       TO OR-H8.
032380       MOVE "DELIVERY"     TO OR-H9.
             MOVE "PCK-SLIP"     TO OR-H10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
032410	     MOVE SPACES	 TO R-DET.
             MOVE "/DESCRIPTION" TO OR-H2A.
             MOVE " /RATE"       TO OR-H4.
             MOVE "  /RAND"      TO OR-H5.
             MOVE " /DISC"       TO OR-H6.
             MOVE "/NAME/CONTACT"
                                 TO OR-H7.
             MOVE "/TELEPHONE"   TO OR-H8.
032390       MOVE "/REMARKS"     TO OR-H10.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
032410	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

032420 GA00.
032430       DISPLAY CLR-SCREEN.
032440       DISPLAY S08.

032450 GA005.
032460	     ACCEPT WS-OPTION AT 0650
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO GA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GA005
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
032470	   IF WS-OPTION = "E"
032480         GO TO GA999.
032490	   IF WS-OPTION = "C"
               MOVE ZERO         TO W15-ORD
               MOVE "O"          TO WS-DEL
               PERFORM GA10
	       PERFORM GB000-TRAN
032500	       GO TO GA00.
032510	   IF WS-OPTION = "D"
               MOVE ZERO         TO W15-ORD
               MOVE "D"          TO WS-DEL
               PERFORM GA10
	       PERFORM GD000-TRAN
032520	       GO TO GA00.
032530       GO TO GA005.

       GA10.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DET.
             PERFORM GA00B.             

       GA15.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.

032560 GA999.
032570       EXIT.
      /
032580 GB000-TRAN    SECTION 51.
       GB00.
032640	     MOVE ZERO		 TO W15-30 W15-120
032650                              W15-60 W15-150 W15-DEL
032660                              W15-90 W15-180.
	     MOVE SPACES	 TO W15-CONTACT W15-TEL P-NUMBER
				    W15-FAX W15-ACNO.
             MOVE SPACES         TO W15-CURRENCY.
             MOVE 1              TO W15-RATE.
             MOVE "Account"      TO W15-PMT.
             MOVE "Deliver"      TO W15-RMKS.

032590 GB000.
032600       DISPLAY CLR-SCREEN.
032610       DISPLAY S09.
032620       DISPLAY S10.
	   IF WS-EXT-STK = "Y"
	       DISPLAY "    " AT 0536
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/" AT 0536
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5
		       "   " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
032630	     MOVE SPACES	 TO W10-DESC.
032640       MOVE ZERO           TO W15-UN W15-SELL W15-ICOST 
                                           W15-COST.
032670	     MOVE SPACES	 TO W10-EXT-ITEM W15-PSLIP.
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

       GB02.
	     MOVE "'E'xisting,'N'ew order number 'ESC' to exit  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.

       GB03.
033910	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-ORD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM GA15
		     GO TO GB999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB03
	       END-EVALUATE.
030750	   IF NOT (WS-ORD = "E" OR "N")
030760	       GO TO GB02.
	   IF WS-ORD = "N"
               PERFORM GB00
               GO TO GB15.

032680 GB05.
032690       ACCEPT S21 AT 0422.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-OPREF
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB05
	       END-EVALUATE.
	   IF W15-OPREF = ZERO
               PERFORM GA15
032720	       GO TO GB999.
             MOVE W15-OPREF      TO ORD-ORD.
             MOVE ZERO           TO ORD-LINE.
             PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO GB08.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF W15-OPREF NOT = ORD-ORD
               GO TO GB08.
             GO TO GB010.

       GB08.
	     MOVE "Order number invalid"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
             GO TO GB02.

032680 GB010.
             MOVE ORD-LINE       TO W15-OSUB.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
032770     IF WS-F-ERROR = ZERO
               IF W15-OPREF = ORD-ORD
                   GO TO GB010.
             ADD 1               TO W15-OSUB.
032730	     DISPLAY "/" AT 0430
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
032740       DISPLAY S22 AT 0431.
	     MOVE ORD-CRED	 TO W15-ACNO.
             GO TO GB015.

       GB15.
012010       MOVE 2              TO WS-NETKEY.
012020       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE NET-CREDITOR   TO W70-CREDCON.
           IF W70-PORDER = 99999999
               MOVE 1            TO W70-PORDER
           ELSE
               ADD 1             TO W70-PORDER
               MOVE W70-PORDER   TO W15-OPREF
               MOVE ZERO         TO W15-OSUB.
032690       DISPLAY S21 AT 0422.
032730	     DISPLAY "/" AT 0430
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
032740       DISPLAY S22 AT 0431.

032810 GB015.
020730       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF WS-EXT-STK = "Y"
	       ACCEPT S12-EXT AT LINE 5
	   ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S12-ITM AT LINE 5
	       ELSE
021070		   ACCEPT S12 AT LINE 5
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S12-3 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S12-4 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S12-5 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S12-6 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S12-7 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S12-8 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S12-9 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S12-10 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S12-11 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S12-12 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S12-13 AT LINE 5
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S12-14 AT LINE 5.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO GB15E
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F8-KEY
		     PERFORM AA100
		     GO TO GB015
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB015
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S12-EXT AT LINE 5
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S12-ITM AT LINE 5
		   ELSE
021070		       DISPLAY S12 AT LINE 5.
016580	       IF W10-EXT-ITEM = SPACES
016590		   GO TO GB015.
       GB15E.
	     DISPLAY CLEAR-L25.
032840	   IF W10-EXT-ITEM = SPACES
032850         GO TO GB000.
       GB015A.
032870       PERFORM CA000.
032880     IF WS-F-ERROR NOT = ZERO
	       MOVE "Z"		 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S12-EXT AT LINE 5
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S12-ITM AT LINE 5
		   ELSE
021070		       DISPLAY S12 AT LINE 5
		   END-IF
	       END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO GB015
               ELSE
		   GO TO GB015A.
033010	     MOVE STK-DESC	 TO W10-DESC.
033030	     DISPLAY W10-DESC AT 0541
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GB015.

032920	     MOVE STK-COST	 TO W15-COST.
           IF W15-OSUB NOT = ZERO
               IF W15-ACNO NOT = SPACES
032970             PERFORM CA155-GET-CREDITOR
032940             DISPLAY S19 AT 0622
		   DISPLAY P-NAME AT 0637
			   WITH FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
                   MOVE ORD-CONTACT
                                 TO W15-CONTACT
                   MOVE ORD-TEL  TO W15-TEL
                   MOVE ORD-FAX  TO W15-FAX
                   MOVE ORD-TERMS
                                 TO W15-SETT
                   GO TO GB16A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
032930 GB16.
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
032940	     ACCEPT S19 AT 0622.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO GB000
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		     PERFORM AA50C THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16
	       END-EVALUATE
018820	       DISPLAY S19 AT 0622
018830	       IF W15-ACNO = SPACES
018840		   GO TO GB16.
	     DISPLAY CLEAR-L25.
032950     IF W15-ACNO = SPACES
032960         GO TO GB000.
032970	     PERFORM CA155-GET-CREDITOR.
032980     IF WS-ERROR NOT = ZERO
032990	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033000         GO TO GB16.
	     DISPLAY P-NAME AT 0637
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033020       MOVE P-TERMS        TO W15-SETT.
             MOVE P-CONTACT      TO W15-CONTACT.
             MOVE P-TEL          TO W15-TEL.
             MOVE P-FAX          TO W15-FAX.
       GB16A.
	     DISPLAY "Press " AT 2501
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       GB16AA.
             ACCEPT S50 AT 0722.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16AA
	       END-EVALUATE.
       GB16AB.
             ACCEPT S50A AT 0763.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16AB
	       END-EVALUATE.
033040 GB16B.
033050	     ACCEPT S20 AT 0825.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16B
	       END-EVALUATE.
033060	   IF W15-UN = ZERO
033070         GO TO GB16B.
033080       MOVE STK-COST       TO W15-OCOST.
033090 GB16C.
	     ACCEPT S23O AT 0922.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16C
	       END-EVALUATE.
033110	   IF W15-OCOST = ZERO
033120         GO TO GB16C.
       GB16CA.
	     ACCEPT S23R AT 0944.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16CA
	       END-EVALUATE.
033110	   IF W15-RATE = ZERO
033120         GO TO GB16CA.
       GB16CB.
           IF W15-RATE = 1.00
	       MOVE W15-OCOST	 TO W15-COST
               MOVE "Rand"       TO W15-CURRENCY
	       DISPLAY S23C AT 0963
               GO TO GB16CD.
       GB16CC.
	     ACCEPT S23C AT 0963.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16CC
	       END-EVALUATE.
	   IF W15-CURRENCY = SPACES
               GO TO GB16CC.
	     COMPUTE W15-COST ROUNDED = W15-OCOST * W15-RATE.
       GB16CD.
033100	     DISPLAY S23 AT 1023.
015930	     COMPUTE W15-USELL ROUNDED = (W15-COST +
015940		 (W15-COST * STK-MARKUP / 100.00000)).
015930	     COMPUTE W15-UWSALE ROUNDED = (W15-COST +
015940		 (W15-COST * STK-WMARKUP / 100.00000)).
015930	     COMPUTE W15-UCASH ROUNDED = (W15-COST +
015940		 (W15-COST * STK-CMARKUP / 100.00000)).
           IF W05-ROUND = "E"
               MOVE W15-USELL    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-USELL
               MOVE W15-UWSALE   TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-UWSALE
               MOVE W15-UCASH    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W15-UCASH.
           IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (1)	 TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W15-USELL +
                                           (W15-USELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W15-UWSALE +
                                            (W15-UWSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W15-UCASH +
                                           (W15-UCASH * W05-RTE)
           ELSE
               MOVE W15-UWSALE   TO W10-VWSALE
               MOVE W15-UCASH    TO W10-VCASH
               MOVE W15-USELL    TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W15-RSELL
               PERFORM CB100
               MOVE W15-RSELL    TO W10-VCASH
               PERFORM CB200.
033130       MOVE W15-USELL      TO W15-SELL.
             DISPLAY S51.
033150 GB16D.
             ACCEPT W15-PMT AT 1322 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16D
	       END-EVALUATE.
       GB16E.
	     ACCEPT S35 AT 1428.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16E
	       END-EVALUATE.
	     COMPUTE W15-TCOST ROUNDED = W15-COST * W15-UN.
             COMPUTE W15-TCOST = W15-TCOST - 
                                (W15-TCOST * W15-DISC1 /100).
	     DISPLAY S31 AT 1522.
       GB16F.
             ACCEPT W15-RMKS AT 1622 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16F
	       END-EVALUATE.
033190 GB16G.
033200       ACCEPT S25 AT 1722.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16G
	       END-EVALUATE.
	     MOVE W15-DEL	 TO W15-DATE.
             PERFORM AD000.
033210     IF WS-ERROR NOT = ZERO
033220         GO TO GB16G.

033230 GB16H.
033240	     ACCEPT W15-30 AT 1825
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
033250	     ACCEPT W15-60 AT 1830
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
033260	     ACCEPT W15-90 AT 1835
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
033270	     ACCEPT W15-120 AT 1841
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
033290	     ACCEPT W15-150 AT 1847
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
033310	     ACCEPT W15-180 AT 1853
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO GB000
		 WHEN OTHER
		     CALL X"E5"
		     GO TO GB16H
	       END-EVALUATE.
026560	     DISPLAY CLEAR-L25.
             ACCEPT S49 AT 1922.
033350 GB017.
012180	     PERFORM CHECK-CORRECT.
033430	   IF WS-OPTION = "N"
               IF WS-ORD = "N"
                   GO TO GB015
               ELSE
033440             GO TO GB05.
033450 GB050.
033470       MOVE ZERO           TO WS-S.
033480     IF W15-30 > ZERO
033490         ADD 1             TO WS-S.
033500     IF W15-60 > ZERO
033510         ADD 1             TO WS-S
033520     IF W15-90 > ZERO
033530         ADD 1             TO WS-S.
033540     IF W15-120 > ZERO
033550         ADD 1             TO WS-S.
033560     IF W15-150 > ZERO
033570         ADD 1             TO WS-S.
033580     IF W15-180 > ZERO
033590         ADD 1             TO WS-S.
033600 GB065.
      *
      *    ****    Start Recovery Logging.
      *
033610	     PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
033630       PERFORM AY07 THRU AY59.
033650       PERFORM AY05 THRU AY59.
           IF WS-ORD = "N"
               IF W70-PORDER = 99999999
                   MOVE 1        TO W70-PORDER
               ELSE
                   ADD 1         TO W70-PORDER
                   MOVE W70-PORDER
                                 TO W15-OPREF
                   MOVE ZERO     TO W15-OSUB.
032690       DISPLAY S21 AT 0422.
032730	     DISPLAY "/" AT 0430
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
032740       DISPLAY S22 AT 0431.
             INITIALIZE ORD-RECORD1.
033790       MOVE W05-DATE-YMD   TO ORD-DATE STK-DTE.
033800       MOVE W15-ACNO       TO ORD-CRED.
	     MOVE W10-EXT-ITEM	 TO ORD-EXT-ITEM.
033820	     MOVE W15-OPREF	 TO ORD-ORD.
             MOVE W15-OSUB       TO ORD-LINE.
033830	     MOVE W15-UN	 TO ORD-QUANT.
033840	     MOVE W15-COST	 TO ORD-UCOST.
033840       MOVE W15-OCOST      TO ORD-OCOST.
             MOVE W15-RATE       TO ORD-DRATE.
             MOVE W15-CURRENCY   TO ORD-CURRENCY.
033840       MOVE W15-TCOST      TO ORD-COST.
033850	     MOVE W15-SETT	 TO ORD-TERMS.
      *
      *    ****    S W I T C H	 D A T E   T O	CCYYMMDD
      *
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO ORD-DELIV.
             MOVE W15-SELL       TO ORD-SELL.
             MOVE W15-PSLIP      TO ORD-PSLIP.
             MOVE W15-PMT        TO ORD-PMT.
             MOVE W15-RMKS       TO ORD-INST.
             MOVE W15-DISC1      TO ORD-DISC.
             MOVE W15-CONTACT    TO ORD-CONTACT.
             MOVE W15-TEL        TO ORD-TEL.
             MOVE W15-FAX        TO ORD-FAX.
033870       ADD W15-UN          TO STK-ORD.
034480       PERFORM WRITE-PORDER THRU WRITE-PORDER-EXIT.
033690       MOVE 1              TO WS-ACTION.
033750       PERFORM AY10 THRU AY59.

033880 GB070.
033890       MOVE W15-DEL-MM     TO WS-S2.
033900       MOVE ZERO           TO W25-CFLOW (1) W25-CFLOW (2) 
                                    W25-CFLOW (3) W25-CFLOW (4) 
                                    W25-CFLOW (5) W25-CFLOW (6)
033920                              W25-V1        W25-V2.
033930       MOVE 1              TO WS-S3.
033940     IF WS-S = 1
033950         MOVE W15-TCOST    TO W25-V2
033960         GO TO GB075.
033970       DIVIDE W15-TCOST BY WS-S 
                                 GIVING W20-RESULT ROUNDED.
033980       MOVE W20-RESULT     TO W25-V1.
033990       SUBTRACT 1          FROM WS-S.
034000       MULTIPLY WS-S       BY W20-RESULT.
034010       SUBTRACT W20-RESULT FROM W15-TCOST 
                                 GIVING W25-V2.
034020       ADD 1               TO WS-S.
034030 GB075.
034040     IF W15-MONTH (WS-S3) = ZERO
034050         ADD 1             TO WS-S3
034060         GO TO GB075.
034070     IF WS-S > 1
034080         MOVE W25-V1       TO W25-CFLOW (WS-S3)
034090         SUBTRACT 1        FROM WS-S
034100         ADD 1             TO WS-S3
034110         GO TO GB075.
034120       MOVE W25-V2         TO W25-CFLOW (WS-S3).
034130       MOVE 1              TO WS-S2.
034140 GB080.
034150     IF W25-CFLOW (WS-S2) = ZERO
034160         GO TO GB085.
034170       ADD W15-DEL-MM WS-S2
                                 GIVING WS-S3.
034180     IF WS-S3 > 12
034190         SUBTRACT 12       FROM WS-S3.
034200 GB085.
034210     IF WS-S2 < 6
034220         ADD 1             TO WS-S2
034230         GO TO GB080.
034250 GB090.
034260       MOVE W15-ORD        TO OR-ORD.
	   IF WS-EXT-STK = "Y"
	       MOVE W10-EXT-ITEM TO OR-EXT-ITEM
	   ELSE
	   IF WS-USE-ITM = "Y"
	       MOVE W10-ITEM	 TO OR-ITEM
	       MOVE "/" 	 TO OR-SL
	       MOVE W10-ITM	 TO OR-ITM
	   ELSE
033810	       MOVE W10-ITEM	 TO OR-ITEM.
034300	     MOVE W15-UN	 TO OR-QUANT.
             MOVE W15-CURRENCY   TO OR-CURRENCY.
034310       MOVE W15-OCOST      TO OR-COST.
             MOVE W15-PMT        TO OR-PMT.
034280       MOVE W15-ACNO       TO OR-ACNO.
             MOVE W15-FAX        TO OR-TEL.
034370       MOVE W15-DEL        TO OR-DEL.
             MOVE W15-PSLIP      TO OR-PSLIP.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
034450	     MOVE SPACES	 TO R-DET.
034290	     MOVE W10-DESC	 TO OR-DESC.
             MOVE W15-RATE       TO OR-RATE.
034310	     MOVE W15-COST	 TO OR-COST.
             MOVE W15-DISC1      TO OR-DISC.
             MOVE P-NAME         TO OR-NAME.
             MOVE W15-RMKS       TO OR-RMKS.
	     PERFORM CALL-PRINTUTL.
034450	     MOVE SPACES	 TO R-DET.
             MOVE W15-CONTACT    TO OR-CONTACT.
             MOVE W15-TEL        TO OR-TEL.

034380 GB100.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
034440	       PERFORM GA00B.
034450       MOVE SPACES         TO R-DET.
034460	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
034520       PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
      *
      *    ****    End Recovery Logging.
      *
	     CLOSE CREDIT.
	     OPEN I-O CREDIT.
034540	     PERFORM AY70 THRU AY999.
       GB105.
	     MOVE "'C'ontinue or 'E'xit  [ ]"
				 TO WS-ERR-MES.
             MOVE "C"            TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
033430	   IF NOT (WS-OPTION = "C" OR "E")
033440         GO TO GB105.
	   IF WS-OPTION = "E"
               PERFORM GA15
032720	       GO TO GB999.
034580       GO TO GB000.
       GB999.
	     EXIT.
      /
034590 GD000-TRAN    SECTION 52.
034600 GD000.
034610       DISPLAY CLR-SCREEN.
034620       DISPLAY S11.
034630       DISPLAY S10.
034640       MOVE ZERO           TO W15-ORD.
034650 GD005.
034660       ACCEPT S21 AT 0422.
034670     IF W15-OPREF = ZERO
034680	       GO TO GD999.
034690	     DISPLAY "/" AT 0430
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
034700       ACCEPT S22 AT 0431.
	     MOVE W15-ORD	 TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
034730     IF WS-F-ERROR = 16
034740	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
034750         GO TO GD005.
034760       MOVE ORD-CRED       TO W15-ACNO.
034770	     MOVE ORD-EXT-ITEM	 TO W10-EXT-ITEM.
034790       PERFORM CA155-GET-CREDITOR.
034800       PERFORM CA000.
	   IF NOT (WS-F-ERROR = ZERO)
	       MOVE "Item not on file"
				 TO W10-DESC
	       MOVE 1		 TO WS-IND
	   ELSE
	       MOVE ZERO	 TO WS-IND
034810	       MOVE STK-DESC	 TO W10-DESC.
034820	     MOVE ORD-QUANT	 TO W15-UN.
      *
      *    ****    S W I T C H	 D A T E   T O	DDMMCCYY
      *
	     MOVE ORD-DELIV	 TO W21-DTE.
	     MOVE W21-DD	 TO W15-DEL-DD.
	     MOVE W21-MM	 TO W15-DEL-MM.
	     MOVE W21-CC	 TO W15-DEL-CC.
	     MOVE W21-YY	 TO W15-DEL-YY.
034840	     MOVE ORD-UCOST	 TO W15-COST.
             MOVE ORD-SELL       TO W15-SELL.
             MOVE ORD-OCOST      TO W15-OCOST.
             MOVE ORD-COST       TO W15-TCOST.
             MOVE ORD-FAX        TO W15-FAX.
             MOVE ORD-TEL        TO W15-TEL.
             MOVE ORD-DRATE      TO W15-RATE.
             MOVE ORD-CONTACT    TO W15-CONTACT.
             MOVE ORD-PMT        TO W15-PMT.
             MOVE ORD-INST       TO W15-RMKS.
             MOVE ORD-DISC       TO W15-DISC1.
             MOVE ORD-PSLIP      TO W15-PSLIP.
             MOVE ORD-CURRENCY   TO W15-CURRENCY.
033850       MOVE ORD-TERMS      TO W15-SETT.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S12-EXT AT LINE 5
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S12-ITM AT LINE 5
	       ELSE
021070		   DISPLAY S12 AT LINE 5.
034890	     DISPLAY W10-DESC AT 0541
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
034880       DISPLAY S19 AT 0622.
	     DISPLAY P-NAME AT 0637
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             DISPLAY S50 AT 0722.
             DISPLAY S50A AT 0763.
034900	     DISPLAY S20 AT 0825.
034920	     DISPLAY S23O AT 0922.
034920	     DISPLAY S23R AT 0944.
034920	     DISPLAY S23C AT 0963.
             PERFORM GB16CD.
	     DISPLAY W15-PMT AT 1322
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
034940	     DISPLAY S35 AT 1428.
034940	     DISPLAY S31 AT 1522.
	     DISPLAY W15-RMKS AT 1622
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
034950       DISPLAY S25 AT 1722.
033240	     DISPLAY W15-30 AT 1825
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033250	     DISPLAY W15-60 AT 1830
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033260	     DISPLAY W15-90 AT 1835
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033270	     DISPLAY W15-120 AT 1841
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033290	     DISPLAY W15-150 AT 1847
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033310	     DISPLAY W15-180 AT 1853
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             DISPLAY S49 AT 1922.
012180	     PERFORM CHECK-CORRECT.
035030	   IF WS-OPTION = "N"
035040         GO TO GD000.
      *
      *    ****    Start Recovery Logging.
      *
             MOVE "N"            TO WS-SKIP.
035050       PERFORM AY60 THRU AY999.
035060       MOVE 0              TO WS-ACTION.
035070       PERFORM AY05 THRU AY59.
             MOVE 2              TO WS-ACTION.             
035080       PERFORM AY10 THRU AY59.
035110     IF ORD-TERMS NOT = ZERO
035120         GO TO GD08.
035130       MOVE P-TERMS        TO ORD-TERMS.

035140 GD08.
	   IF WS-IND = ZERO
035150	       IF STK-ORD NOT > ORD-QUANT
035160		   MOVE ZERO	 TO STK-ORD STK-DTE STK-DISC
035170	       ELSE
035180		   SUBTRACT ORD-QUANT
				 FROM STK-ORD.
      *
      *    ****    S W I T C H	 D A T E   T O	DDMMCCYY
      *
	     MOVE ORD-DELIV	 TO W21-DTE.
	     MOVE W21-DD	 TO W15-DEL-DD.
	     MOVE W21-MM	 TO W15-DEL-MM.
	     MOVE W21-CC	 TO W15-DEL-CC.
	     MOVE W21-YY	 TO W15-DEL-YY.
035190	     MOVE W15-DEL-MM	 TO WS-S.
035200       MOVE ORD-TERMS      TO W15-SETT.
035210       MOVE 0              TO WS-S.
035220       MOVE 1              TO WS-S2.

035230 GD010.
035240       MOVE ZERO           TO W25-CFLOW (WS-S2).
035250     IF W15-MONTH (WS-S2) > 0
035260         ADD 1             TO WS-S.
035270     IF WS-S2 < 6
035280         ADD 1             TO WS-S2
035290         GO TO GD010.
035300       MOVE ZERO           TO W25-V1 W25-V2.
035310       MOVE 1              TO WS-S3.
035320     IF WS-S = 1 OR 0
035330         MOVE ORD-COST     TO W25-V2
035340         GO TO GD015.
035350       DIVIDE ORD-COST BY WS-S 
                                 GIVING W25-V1 ROUNDED.
035360       SUBTRACT 1          FROM WS-S.
035370       COMPUTE W25-V2 = ORD-COST - (W25-V1 * WS-S).
035380       ADD 1               TO WS-S.

035390 GD015.
035400     IF W15-MONTH (WS-S3) = ZERO
035410         ADD 1             TO WS-S3
035420         GO TO GD015.
035430     IF WS-S > 1
035440         MOVE W25-V1       TO W25-CFLOW (WS-S3)
035450         SUBTRACT 1        FROM WS-S
035460         ADD 1             TO WS-S3
035470         GO TO GD015.
035480       MOVE W25-V2         TO W25-CFLOW (WS-S3).
035490       MOVE 1              TO WS-S2.

035500 GD020.
035510     IF W25-CFLOW (WS-S2) = ZERO
035520         GO TO GD025.
035530       ADD W15-DEL-MM WS-S2 
                                 GIVING WS-S3.
035540     IF WS-S3 > 12
035550         SUBTRACT 12       FROM WS-S3.
035560 GD025.
035570     IF WS-S2 < 6
035580         ADD 1             TO WS-S2
035590         GO TO GD020.
035600       PERFORM GB090.
035610       MOVE "DELETION"     TO OR-RMKS.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
035660	       PERFORM GA00B.
035670       MOVE SPACES         TO R-DET.
035100       PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT.
	   IF WS-IND = ZERO
035690	       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    End Recovery Logging.
      *
	     CLOSE CREDIT.
	     OPEN I-O CREDIT.
035720	     PERFORM AY70 THRU AY999.
035760       GO TO GD000.
       GD999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
019640 ZA000-INIT    SECTION 90.
019650 ZA000-START.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019660	     DISPLAY CLR-SCREEN.
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
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
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
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
019960       MOVE 1              TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980	     MOVE PAR-COMPANY	 TO W60-COMPANY.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-ROUND	 TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
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
042540	     DISPLAY CLR-SCREEN.
020100       GO TO ZA999-EXIT.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCR  (WS-S1) WS-BCR  (WS-S1).
041250       MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70)
021930                              WS-TCR  (8)  WS-TCR  (50)
021940                              WS-TCR  (66).
021950       MOVE WS-G3          TO WS-MCHR (1)  WS-MCHR (80)
                                    WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
021950                              WS-MCR  (8)  WS-MCR  (50)
021960                              WS-MCR  (66) WS-MCR  (1)
				    WS-MCR  (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70)
021970                              WS-BCR  (8)  WS-BCR  (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR  (1)
                                    WS-TCHR (1)  WS-BCHR (1).
051260       MOVE WS-G13         TO WS-TCR  (80) WS-BCR  (80)
                                    WS-TCHR (80) WS-BCHR (80).
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
020210 ZB000-I-O-ERROR SECTION 91.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
