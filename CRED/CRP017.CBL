      $set LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	     **     ********   *
      *   **	**  **	  **  **    **	 **  **     *** 	 **    *
      *   **	    **	  **  **    **	**    **     ** 	**     *
      *   **	    ******    *******	**    **     **        **      *
      *   **	    **	 **   **	**    **     **       **       *
      *   **	**  **	  **  **	 **  **      **       **       *
      *    ******   **	  **  **	  ****	   ******     **       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   D E B I T   N O T E S  (A N A L Y S E D) *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP017.
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

000200 COPY CRTRAN.SL.

       COPY DEPART.SL.

       COPY LEDTRF.SL.

000140 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY TXTRAN.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000330 COPY CRTRAN.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

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
003300 77  WS-CTRKEY       PIC  9(06)    COMP-5 VALUE 1.
003340 77  WS-PARKEY       PIC  9(04)    COMP-5 VALUE 1.
003360 77  WS-RECKEY       PIC  9(04)    COMP-5.
003370 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
003370 77  WS-PURKEY       PIC  9(04)    COMP-5.
003380 77  WS-KEYSTR       PIC  9(04)    COMP-5.
003390 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
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
003510 77  WS-S9           PIC  9(04)    COMP-5.
003540 77  WS-OPTION	   PIC	X(01).
003540 77  WS-SKIP         PIC  X(01) VALUE "Y".
003540 77  WS-LOOK         PIC  X(01) VALUE "S".
003610 77  WS-SKEY	   PIC	9(05).
003620 77  WS-KEY1         PIC  9(04)     COMP-5.
003630 77  WS-LINES        PIC  9(04)     COMP-5 VALUE 0.
003640 77  WS-PAGE         PIC  9(04)     COMP-5 VALUE 0.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
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
003920 77  WS-SCHQ         PIC X(06)     VALUE "Cheque".
003930 77  WS-SSETT        PIC X(22)     VALUE
003940                     "30  60  90 120 150 180".
003950 77  WS-H1           PIC X(05)     VALUE "DATE:".
003960 77  WS-H3           PIC X(05)     VALUE "PAGE:".
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
               05  FILLER  PIC  X(60) VALUE SPACES.
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
	   03  WS-PROG	       PIC  X(03) VALUE "017".

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

000180 COPY CRTRAN.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000280 COPY RECOVER.ID.

       COPY SHARED.ID.

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
	       05  OR-H1	 PIC X(12).
	       05  OR-H2.
		 07  FILLER	 PIC X(04).
		 07  OR-H2A	 PIC X(27).
	       05  OR-H3	 PIC X(06).
	       05  OR-H4	 PIC X(12).
	       05  OR-H5	 PIC X(10).
	       05  OR-H6	 PIC X(09).
	       05  OR-H7	 PIC X(22).
	       05  OR-H8	 PIC X(13).
	       05  OR-H9	 PIC X(09).
	       05  OR-H10	 PIC X(08).

	   03  R-L8A REDEFINES W02-PRN-LINE.
	       05  OR-ORD	 PIC Z(07)9/99.
	       05  FILLER	 PIC X(01).
	       05  OR-DESC.
		 07  OR-ITEM	 PIC X(14).
		 07  FILLER	 PIC X(16).
	       05  FILLER	 PIC X(01).
	       05  OR-QUANT	 PIC Z(04)9.
	       05  FILLER	 PIC X(01).
	       05  OR-CURRENCY.
		 07  FILLER	 PIC X(01).
		 07  OR-RATE	 PIC Z(02)9.99.
		 07  FILLER	 PIC X(03).
	       05  FILLER	 PIC X(01).
	       05  OR-COST	 PIC Z(06)9.99.
	       05  FILLER	 PIC X(01).
	       05  OR-PMT.
		 07  FILLER	 PIC X(01).
		 07  OR-DISC	 PIC Z9.99 BLANK WHEN ZERO.
		 07  FILLER	 PIC X(02).
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

	   03  BL-L1 REDEFINES W02-PRN-LINE.
	       05  BL-DATE	   PIC X(06).
	       05  BL-DTE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(17).
	       05  BL-COMP	   PIC X(40).
	       05  FILLER	   PIC X(12).
	       05  BL-HEAD	   PIC X(20).
	       05  FILLER	   PIC X(18).
	       05  BL-PAGE	   PIC X(06).
	       05  BL-PGE	   PIC ZZ9.

	   03  BL-L2 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(05).
	       05  BL-H1	   PIC X(09).
	       05  BL-H2	   PIC X(46).
	       05  BL-H3	   PIC X(12).
	       05  BL-H4	   PIC X(11).
	       05  BL-H5	   PIC X(13).
	       05  BL-H6	   PIC X(11).
	       05  BL-H8	   PIC X(17).
	       05  BL-H7	   PIC X(12).

	   03  BL-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(04).
	       05  BL-AC	   PIC X(06).
	       05  FILLER	   PIC X(04).
	       05  BL-NAME	   PIC X(40).
	       05  FILLER	   PIC X(04).
	       05  BL-TDTE	   PIC Z9/99/9999 BLANK WHEN ZERO.
	       05  FILLER	   PIC X(03).
	       05  BL-REF	   PIC X(08).
	       05  FILLER	   PIC X(03).
	       05  BL-IDTE	   PIC Z9/99/9999.
	       05  FILLER	   PIC X(03).
	       05  BL-INV	   PIC X(08).
	       05  FILLER	   PIC X(04).
	       05  BL-VAL	   PIC Z(08)9.99DB.
	       05  FILLER	   PIC X(04).
	       05  BL-ORD	   PIC Z(07).
	       05  FILLER	   PIC X(04).

	   03  BL-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  BL-LEDG	   PIC X(04).
	       05  FILLER	   PIC X(02).
	       05  BL-DESC	   PIC X(30).
	       05  FILLER	   PIC X(02).
	       05  BL-ALLOCATE	   PIC Z(06)9.99DB.
	       05  FILLER	   PIC X(71).

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
004430     03  W10-MONTH-NAMES PIC X(36)     VALUE
004440                        "JanFebMarAprMayJunJulAugSepOctNovDec".
004450     03  W10-MONTH       REDEFINES W10-MONTH-NAMES
004460                         PIC X(03)     OCCURS 12.
       01  W10-STOCK.
002850     03  W10-VSELL       PIC S9(09)V99 COMP-3.
002860     03  W10-VCASH       PIC S9(09)V99 COMP-3.
002860     03  W10-VWSALE      PIC S9(09)V99 COMP-3.

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

005030 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

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
005310         05  W25-SVAL    PIC Z(07)9.99DB.
005320     03  W25-S7V2.
005330	       05  W25-VAL2    PIC Z(07)9.99DB.
005280     03  W25-5V2 REDEFINES W25-S7V2.
005290	       05  W25-VAL     PIC Z(08)9.99-.
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
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
	   03  W75-CNO	      PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

003980 01  W90-TRAN.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.

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

007200 01  S15.
007210	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
007220                           VALUE "CREDITOR DEBIT NOTE".

007260 01  S17.
007270     03  LINE  4 COLUMN  2 PIC X(09) USING WS-SCRED.
007280	   03  LINE  6 COLUMN  3 VALUE "Tran date".
007290	   03	       COLUMN 14 VALUE "Tran ref".
007300	   03	       COLUMN 25 VALUE "Inv date".
007310	   03	       COLUMN 37 VALUE "Inv no".
007330	   03	       COLUMN 48 VALUE "Inv Value".
007340     03          COLUMN 60 VALUE "30".
007350     03          COLUMN 63 VALUE "60".
007360     03          COLUMN 66 VALUE "90".
007370     03          COLUMN 69 VALUE "120".
007380     03          COLUMN 73 VALUE "150".
007390     03          COLUMN 77 VALUE "180".

007400 01  S18.
007410     03  LINE 12 COLUMN  9 VALUE "Purchases".
007420     03  LINE 13 COLUMN 10 VALUE "Ledger".
007430     03  LINE 13 COLUMN 24 VALUE "Description".
007440     03  LINE 13 COLUMN 67 VALUE "Value".

007450 01  S19.
007460	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W15-ACNO AUTO.

007530 01  S23.
007540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99 USING W15-COST AUTO.

007590 01  S26.
007600	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(04) USING W15-LEDG AUTO.

007610 01  S27.
007620	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(08) USING W15-GRNN AUTO.

007630 01  S28.
007640	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-GRND AUTO.

007650 01  S29.
007660	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(08) USING W15-INVN AUTO.

007670 01  S30.
007680	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z9/99/9999 USING W15-INVD AUTO.

007840 01  S38.
007850	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.999 USING W15-ICOST AUTO.

       01  S43.
007540	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(07)9.99DB USING W15-VAT AUTO.

       COPY CRDLUP.CRT.

       COPY DEPART.CRT.

       01  S-CRED.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
	   03	       COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-BAL.
           03          COLUMN 28 VALUE "Due :".
	   03	       COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC Z(08)9.99DB USING P-OUT.

       COPY S99.CRT.

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
	       GO TO AA48.
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
004940	     PERFORM IA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005260	     CLOSE RECOVER.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Ensure than LS-OK is cleared as the program might have    ³
      *    ³ been called to do TAKE-ON and LS-OK would be set to "N".  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA48.
	     MOVE SPACE 	 TO LS-OK.

       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011180	     PERFORM CREDITOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370	     EXIT.

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

       COPY CLEAR.CRT.

       COPY PRINTUTL.AS4.

       COPY CREDITOR.TRN.

      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CRTRAN.RD.

       COPY CREDIT.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

000200 COPY TXTRAN.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010390 AC000-WRITE   SECTION.

       COPY CONTROL.WR.

       COPY CRTRAN.WR.

       COPY CREDIT.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

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
012900	   IF W20-DECIMAL > ZERO
012910         GO TO AD15.
012920       GO TO AD999.
012930 AD15.
012940       MOVE 1              TO WS-ERROR.
012950 AD999.
012960       EXIT.

       COPY CREDIT.ILU.

       COPY DEPART.LUP.

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
011910       MOVE WS-CTRKEY      TO REC-KEY.
011920       MOVE CT-REC         TO REC-CRTRAN.
011930       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.

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

011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
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
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
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
012620	     MOVE "CR"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
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
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
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
             MOVE ZERO           TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       BG705.
	     MOVE W15-TAXVAL(WS-S9)
				 TO TAX-VAL(WS-S9).
	     MOVE W15-VATVAL(WS-S9)
				 TO TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BG705.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.
       CA799.
             EXIT.

       COPY LEDTRF.UPD.

      /
037540 IA000	     SECTION 5.
037550 IA0.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE W05-VAT (1)	 TO W05-RATE.

037580 IA00A.
037590       DISPLAY CLR-SCREEN.
037770	     MOVE "CREDITOR DEBIT NOTES"
				 TO W50-HEAD.
037800       MOVE ZERO           TO W15-TOTAL WS-PAGE.
037810       MOVE W05-DATE       TO W15-GRND.

037820 IA00B.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
037830	     MOVE SPACES	 TO R-DET.
037840       MOVE WS-H1          TO BL-DATE.
037850       MOVE W05-DATE       TO BL-DTE.
037860       MOVE W60-COMPANY    TO BL-COMP.
037870       MOVE W50-HEAD       TO BL-HEAD.
037880       MOVE WS-H3          TO BL-PAGE.
037890       ADD 1               TO WS-PAGE.
037900       MOVE WS-PAGE        TO BL-PGE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
037920	     MOVE SPACES	 TO R-DET.
037930       MOVE "ACNO"         TO BL-H1.
037940       MOVE "CREDITOR"     TO BL-H2.
037950       MOVE "TR DATE"      TO BL-H3.
037960       MOVE "REF NO"       TO BL-H4.
037970       MOVE "INV DATE"     TO BL-H5.
037980       MOVE "INV NO"       TO BL-H6.
037990       MOVE "ORDR NO"      TO BL-H7.
038000       MOVE "VALUE"        TO BL-H8.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
038020	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

038030 IA00.
038040       DISPLAY CLR-SCREEN.
038090	     DISPLAY S15.
038130       DISPLAY S17.
             MOVE SPACES         TO W15-ACNO.

038140 IA05.
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
038150	     ACCEPT S19 AT 0412.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO IA06
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA05
	       END-EVALUATE
018820	       DISPLAY S19 AT 0412
018830	       IF W15-ACNO = SPACES
018840		   GO TO IA05.

       IA06.
	     DISPLAY CLEAR-L25.
038160     IF W15-ACNO = SPACES
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
038180	       MOVE "BATCH TOTAL:"
				 TO BL-H5
038190         MOVE W15-TOTAL    TO BL-VAL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
038210	       MOVE SPACES	 TO R-DET
038220	       GO TO IA999.
038250	     PERFORM CA155-GET-CREDITOR.
038260     IF WS-ERROR NOT = ZERO
038270	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
038280         GO TO IA05.
038290       MOVE P-NAME         TO W15-NAME.
038300	     DISPLAY P-NAME AT 0419
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
           IF P-TAX = "E"
	       DISPLAY "(Exclusive)" AT 0462
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5
           ELSE
	       DISPLAY "(Inclusive)" AT 0462
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 5.
             DISPLAY S-CRED.
012180	     PERFORM CHECK-CORRECT.
038380	   IF WS-OPTION = "N"
038390         GO TO IA05.
	     MOVE SPACES	 TO W15-GRNN W15-INVN.
038410	     MOVE ZERO		 TO W15-INVD W15-COST W15-ORD.
038420       MOVE ZERO           TO W65-CFLOW (1) W65-CFLOW (2)
                                    W65-CFLOW (3) W65-CFLOW (4) 
                                    W65-CFLOW (5) W65-CFLOW (6)
                                    W65-V1        W65-V2.
	     MOVE 1		 TO WS-S9.

       IA10.
018660	     MOVE ZERO		 TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO IA10.

038440 IA12.
038450       ACCEPT S28 AT 0802.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA12
	       END-EVALUATE.
038460	     MOVE W15-GRND	 TO W15-DATE.
038470       PERFORM AD000.
038480     IF WS-ERROR = 1
038490         GO TO IA12.
             MOVE W15-DD         TO W20-DD.
             MOVE W15-MM         TO W20-MM.
	     MOVE W15-CC	 TO W20-CC.
             MOVE W15-YY         TO W20-YY.

038500 IA12A.
038510	     ACCEPT S27 AT 0814.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA05
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA12
	       END-EVALUATE.

038520 IA13.
038530	     ACCEPT S30 AT 0824.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA12A
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA13
	       END-EVALUATE.
038540	     MOVE W15-INVD	 TO W15-DATE.
038550       PERFORM AD000.
038560     IF WS-ERROR = 1
038570         GO TO IA13.

038580 IA13A.
038590	     ACCEPT S29 AT 0836.
038630	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA13
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA13A
	       END-EVALUATE.
038610	   IF W15-INVN = SPACES
038620	       GO TO IA13A.

       IA13B.
012180	     PERFORM CHECK-CORRECT.
038730	   IF WS-OPTION = "N"
038740         GO TO IA05.

038750 IA15.
038760	     ACCEPT S23 AT 0846.
038630	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA13B
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA15
	       END-EVALUATE.
038770	   IF W15-COST = ZERO
038780         GO TO IA05.
038800	     MULTIPLY -1 BY W15-COST
				 GIVING W25-VAL2 W25-VAL3.
038830	     DISPLAY W25-S7V2 AT 0846
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
           IF P-TAX = "E"
	       COMPUTE W15-VAT ROUNDED = W25-VAL3 * W05-RTE
           ELSE
               MOVE ZERO         TO W15-VAT.
           IF P-TAX = "I"
	       COMPUTE W15-VAT ROUNDED = ((W15-COST /
                   (100.00000 + W05-RATE)) * W05-RATE)
	       SUBTRACT W15-VAT  FROM W15-COST
	       GO TO IA15B.
	     DISPLAY "VAT :" AT 1040.

       IA15A.
	     ACCEPT S43 AT 1047.
038630	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO IA15
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA15A
	       END-EVALUATE.
012180	     PERFORM CHECK-CORRECT.
038730	   IF WS-OPTION = "N"
038740         GO TO IA15A.
	     MULTIPLY -1	 BY W15-VAT.

       IA15B.
	   IF NOT (W15-VAT = ZERO)
	       ADD W15-COST	 TO W15-TAXVAL(1)
	       ADD W15-VAT	 TO W15-VATVAL(1).
038840	     MOVE P-TERMS	 TO W15-SET.
038850	     DISPLAY W15-30 AT 0860
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038860	     DISPLAY W15-60 AT 0863
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038870	     DISPLAY W15-90 AT 0866
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038880	     DISPLAY W15-120 AT 0870
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038890	     DISPLAY W15-150 AT 0874
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038900	     DISPLAY W15-180 AT 0878
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
038910	     DISPLAY "Ageing all 0 = Update due" AT 2212
038920		      WITH FOREGROUND-COLOR 14.
038930	     MOVE "Do you want to change the ageing (Y/N)"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
039010	   IF NOT (WS-OPTION = "Y")
039030	       GO TO IA30.
039040	     ACCEPT W15-30 AT 0860
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
039050	     ACCEPT W15-60 AT 0863
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
039060	     ACCEPT W15-90 AT 0866
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
039070	     ACCEPT W15-120 AT 0870
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
039090	     ACCEPT W15-150 AT 0874
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
039110	     ACCEPT W15-180 AT 0878
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
030170	     DISPLAY WS-BLNK78 AT 2202 WITH FOREGROUND-COLOR 3.

039140 IA30.
012180	     PERFORM CHECK-CORRECT.
039220	   IF WS-OPTION = "N"
039230         GO TO IA15.
039250	     ADD W15-COST	 TO W15-TOTAL.
	     ADD W15-VAT	 TO W15-TOTAL.
	     ADD W15-COST W15-VAT
				 GIVING W20-RESULT.

039270 IA40.
039280       MOVE ZERO           TO W25-CFLOW (1) W25-CFLOW (2) 
                                    W25-CFLOW (3) W25-CFLOW (4) 
                                    W25-CFLOW (5) W25-CFLOW (6)
039300                              W25-V1        W25-V2.
039310       MOVE 0              TO WS-S.
039320       MOVE 1              TO WS-S2.
039330     IF W15-SETT = ZERO
039340         GO TO IA55.

039350 IA45.
017920       MOVE ZERO           TO W25-CFLOW (WS-S2).
039360     IF W15-MONTH (WS-S2) > 0
039370         ADD 1             TO WS-S.
039380     IF WS-S2 < 6
039390         ADD 1             TO WS-S2
039400         GO TO IA45.
017980       MOVE ZERO           TO W25-V1 W25-V2.
039410       MOVE 1              TO WS-S3.
039440     IF WS-S = 0 OR 1
039450         MOVE W20-RESULT   TO W25-V2
039460         GO TO IA49.
039470       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
039480       SUBTRACT 1          FROM WS-S.
039490       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
039500       ADD 1               TO WS-S.

039510 IA49.
039530	     MULTIPLY -1	 BY W25-V1.
039540	     MULTIPLY -1	 BY W25-V2.

039550 IA50.
039560     IF W15-MONTH (WS-S3) = ZERO
039570         ADD 1             TO WS-S3
039580         GO TO IA50.
039590     IF WS-S > 1
039600         MOVE W25-V1       TO W25-CFLOW (WS-S3)
039610         SUBTRACT 1        FROM WS-S
039620         ADD 1             TO WS-S3
039630         GO TO IA50.
039640       MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V2.
039650       MOVE 1              TO WS-S2.

039660 IA55.
039670     IF W25-CFLOW (WS-S2) = ZERO
039680         GO TO IA60.
039690       ADD W05-MM WS-S2    GIVING WS-S3.
039700     IF WS-S3 > 12
039710         SUBTRACT 12       FROM WS-S3.

039720 IA60.
039730     IF WS-S2 < 6
039740         ADD 1             TO WS-S2
039750         GO TO IA55.

039760 IA65.
039770       MOVE W15-ACNO       TO BL-AC.
039780       MOVE P-NAME         TO BL-NAME.
039790       MOVE W15-GRND       TO BL-TDTE.
039800       MOVE W15-GRNN       TO BL-REF.
039810       MOVE W15-INVD       TO BL-IDTE.
039820       MOVE W15-INVN       TO BL-INV
039830*      MOVE W15-ORD	 TO BL-ORD.
	     ADD W15-COST W15-VAT
				 GIVING BL-VAL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
039860	     MOVE SPACES	 TO R-DET.
      *
      *    ****    Start Recovery Logging.
      *
039870	     PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
039890       PERFORM AY07 THRU AY59.
039920       MOVE W15-COST       TO W15-TCOST.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If this program is called with LS-OK set to "N" then no   ³
      *    ³ department totals nor GL integration will be performed.   ³
      *    ³ This is used for TAKE-ON of transactions.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF LS-OK = "N"
	       GO TO IA80.
039930	     DISPLAY S18.

039940 IA65A.
039950       DISPLAY "Balance :" AT 1710.
039960       MOVE W15-TCOST      TO W25-3.
039970	     DISPLAY W25-3 AT 1720
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
039980	     MOVE ZERO		 TO W15-LEDG W15-ICOST.

039990 IA68.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Purchase Ledger Lookup, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to post as UNSPECIFIED PURCHASES"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       IA68A.
040000	     ACCEPT S26 AT 1512.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-LEDG
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO IA68A
		 WHEN OTHER
		     CALL X"E5"
		     GO TO IA68A
	       END-EVALUATE.
039140	   IF W15-LEDG = SPACES
039150	       MOVE "XXXX"	 TO DPT-CODE
039160     ELSE
	       MOVE W15-LEDG	 TO DPT-CODE.
040050	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
040060	   IF WS-F-ERROR = 7
040070	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
040080         GO TO IA68.
040090	     DISPLAY DPT-DESC AT 1524
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
040170	   IF WS-OPTION = "N"
040180         GO TO IA68.

040200 IA70.
040210       ACCEPT S38 AT 1562.
040230	   IF W15-ICOST > W15-TCOST
040240	       MOVE "Value allocated exceeds tran-balance"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
040260         GO TO IA70. 
012180	     PERFORM CHECK-CORRECT.
040340	   IF WS-OPTION = "N"
040350         GO TO IA70.
040370	     MOVE W15-LEDG	 TO BL-LEDG.
040380       MOVE DPT-DESC       TO BL-DESC.
040410       PERFORM AY38 THRU AY59.
      *
      *      ***** UPDATE DEPARTMENT RETURNS *****
      *
040470	     ADD W15-ICOST	 TO DPT-PRET-MTD
040480                              DPT-PRET-YTD
                                    DPT-PRET-DAY
	   IF WX-INT = "Y"
	       MOVE W20-DTE	 TO WX-DATE
	       MOVE W15-LEDG	 TO WX-LEDG
	       MOVE "RETURNS - CREDITORS"
				 TO WX-NAR
	       COMPUTE WX-VALUE = W15-ICOST * -1
	       MOVE DPT-PUR-GL	 TO WX-AC
	       MOVE 07		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.
040490       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
040500       MOVE W15-ICOST      TO BL-ALLOCATE.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
040550	       PERFORM IA00B.
040560       MOVE SPACES         TO R-DET.
040570       SUBTRACT W15-ICOST  FROM W15-TCOST.
040580     IF W15-TCOST > ZERO
               MOVE 1501         TO CPOS
040590         PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23
040600         GO TO IA65A.

040610 IA75.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
040660	       PERFORM IA00B.
040670       MOVE SPACES         TO R-DET.

040680 IA80.

040770 IA85.
	     INITIALIZE CT-REC.
040800	     MOVE W15-GRND	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO CT-DTE.
	   IF W15-GRNN NOT = SPACES
040810         MOVE W15-GRNN     TO CT-REF
           ELSE
               MOVE W15-INVN     TO CT-REF.
040820       MOVE W15-INVN       TO CT-SINV.
040830	     MOVE W15-INVD	 TO W15-DEL.
	     MOVE W15-DEL-DD	 TO W21-DD.
	     MOVE W15-DEL-MM	 TO W21-MM.
	     MOVE W15-DEL-CC	 TO W21-CC.
	     MOVE W15-DEL-YY	 TO W21-YY.
	     MOVE W21-DTE	 TO CT-SDTE.
040840       MOVE W15-ORD        TO CT-ORD.
040860	     MULTIPLY -1	 BY W15-COST.
040870	     MOVE W15-COST	 TO CT-COST.
040860	     MULTIPLY -1	 BY W15-VAT.
	     ADD W15-VAT	 TO CT-COST.
040880       MOVE W15-ACNO       TO CT-CRED.
040900	     MOVE 02		 TO CT-CODE.
	     MOVE "U"		 TO CT-TYPE.
	     MOVE "I"		 TO CT-PJ.
040960	     MOVE ZERO		 TO CT-AGE
040970				    CT-SELL.
             MOVE W15-VAT        TO CT-TAX.
           IF CT-TAX = ZERO
               MOVE CT-COST      TO CT-TAXFREE.
             PERFORM CA600.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If this program is called with LS-OK set to "N" then no   ³
      *    ³ department totals nor GL integration will be performed.   ³
      *    ³ This is used for TAKE-ON of transactions.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF LS-OK = "N"
	       GO TO IA86.
	     PERFORM CA700.
           IF WX-INT = "Y"
               MOVE W20-DTE      TO WX-DATE
	       MOVE "  CA"	 TO WX-LEDG
	       MOVE W90-CODE	 TO WX-TRN
	       MOVE W00-C-DESC	 TO WX-DESC
               MOVE "- CREDITORS"
                                 TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL     TO WX-AC
               MOVE 02           TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.

040990 IA86.
041000       MOVE W25-VALUES     TO W65-CREDITOR.
041040       MOVE 1              TO WS-S2 WS-S.

041050 IA90.
041060     IF W65-CFLOW (WS-S) NOT = ZERO
041070         ADD WS-S2         TO CT-AGE.
041080     IF WS-S < 6
041090         ADD 1             TO WS-S
041100         MULTIPLY 2        BY WS-S2
041110         GO TO IA90.
041120	     ADD W15-COST	 TO P-BAL P-DT.
041140	     ADD W15-VAT	 TO P-BAL P-DT.
041170     IF W15-SETT = ZERO
	       ADD W15-COST W15-VAT
				 GIVING W25-V2
	   ELSE
               MOVE ZERO         TO W25-V2.
041190       ADD W65-CFLOW (6)   TO P-180D.
041200     IF P-180D < ZERO
               SUBTRACT P-180D   FROM W65-CFLOW (6)
041210         ADD P-180D        TO W65-CFLOW (5)
041220         MOVE ZERO         TO P-180D.
041230       ADD W65-CFLOW (5)   TO P-150D.
041240     IF P-150D < ZERO
               SUBTRACT P-150D   FROM W65-CFLOW (5)
041250         ADD P-150D        TO W65-CFLOW (4)
041260         MOVE ZERO         TO P-150D.
041270       ADD W65-CFLOW (4)   TO P-120D.
041280     IF P-120D < ZERO
               SUBTRACT P-120D   FROM W65-CFLOW (4)
041290         ADD P-120D        TO W65-CFLOW (3)
041300         MOVE ZERO         TO P-120D.
041310       ADD W65-CFLOW (3)   TO P-90D.
041320     IF P-90D < ZERO
               SUBTRACT P-90D    FROM W65-CFLOW (3)
041330         ADD P-90D         TO W65-CFLOW (2)
041340         MOVE ZERO         TO P-90D.
041350       ADD W65-CFLOW (2)   TO P-60D.
041360     IF P-60D < ZERO
               SUBTRACT P-60D    FROM W65-CFLOW (2)
041370         ADD P-60D         TO W65-CFLOW (1)
041380         MOVE ZERO         TO P-60D.
041390       ADD W65-CFLOW (1)   TO P-30D.
041400     IF P-30D < ZERO
               SUBTRACT P-30D    FROM W65-CFLOW (1)
041410         MOVE P-30D        TO W25-V2
041420         MOVE ZERO         TO P-30D.
	     ADD W25-V2 	 TO P-OUT.
041430	     PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
             PERFORM AY08 THRU AY59.
041450	     PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
041490	     ADD W15-COST	 TO W70-CLOSE W70-DEBT.
041590	     ADD W15-VAT	 TO W70-CLOSE W70-DEBT.
019120       ADD W65-CFLOW (6)   TO W70-DUE-180.
019130       ADD W65-CFLOW (5)   TO W70-DUE-150.
019140       ADD W65-CFLOW (4)   TO W70-DUE-120.
019150       ADD W65-CFLOW (3)   TO W70-DUE-90.
019160       ADD W65-CFLOW (2)   TO W70-DUE-60.
019170       ADD W65-CFLOW (1)   TO W70-DUE-30.
             ADD W25-V2          TO W70-DUE-NOW.
      *
      *    ****    VAT Control 
      * 			   MULTIPLE RATE TO BE INCLUDED
      *
           IF W15-VAT = ZERO
               GO TO IA94.
	     MOVE "VAT1"	 TO DPT-CODE.
	     PERFORM AY38 THRU AY59.
	     ADD W15-VATVAL(1)	 TO DPT-I-VRET-DAY
				    DPT-I-VRET-MTD
				    DPT-I-VRET-YTD.
	     ADD W15-VATVAL(7)	 TO DPT-I-VRET-DAY
				    DPT-I-VRET-MTD
				    DPT-I-VRET-YTD.
	     ADD W15-TAXVAL(1)	 TO DPT-V-PRET-DAY
				    DPT-V-PRET-MTD
				    DPT-V-PRET-YTD.
	     ADD W15-TAXVAL(7)	 TO DPT-V-PRET-DAY
				    DPT-V-PRET-MTD
				    DPT-V-PRET-YTD.
	   IF WX-INT = "Y"
	       MOVE W15-VAT	 TO WX-VALUE
	       MOVE DPT-I-VAT-GL TO WX-AC
	       MOVE 12		 TO WX-TYPE
	       PERFORM GL-TRF-UPDATE.
027880       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    End Recovery Logging.
      *
       IA94.
041740       PERFORM AY70 THRU AY999.
041770       GO TO IA00.

041810 IA999.
041820       EXIT.

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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
019960	     MOVE 1		 TO WS-PARKEY.
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

       COPY ZA49.PRO.

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
020210 ZB000-I-O-ERROR SECTION 9.

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
041680	   ELSE
041690     IF WS-F-ERROR = 4
041700         MOVE W02-CRTRAN TO WS-FILE
041710         MOVE WS-CTRKEY TO WS-KEY 
041800	   ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
