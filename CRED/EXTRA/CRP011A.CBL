      $set DATALIT LINKCOUNT"596"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	     **        **      *
      *   **	**  **	  **  **    **	 **  **     ***       ***      *
      *   **	    **	  **  **    **	**    **     **        **      *
      *   **	    ******    *******	**    **     **        **      *
      *   **	    **	 **   **	**    **     **        **      *
      *   **	**  **	  **  **	 **  **      **        **      *
      *    ******   **	  **  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   I N V O I C E S	( S T O C K )	       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP011.
000030 AUTHOR.		       James William Lemmon.
000040 DATE-WRITTEN.	       JUNE, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 1999
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY BARCOD.SL.

       COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000200 COPY CRTRAN.SL.

       COPY DEPART.SL.

       COPY LEDTRF.SL.

000140 COPY PARAM.SL.

000260 COPY PORDER.SL.

000300 COPY PRCLAB.SL.

000300 COPY PURCH.SL.

000300 COPY PURDEX.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY STOCK.SL.

000200 COPY TXTRAN.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY BARCOD.FD.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000330 COPY CRTRAN.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000290 COPY PARAM.FDE.

000260 COPY PORDER.FDE.

000300 COPY PRCLAB.FD.

000300 COPY PURCH.FD.

000300 COPY PURDEX.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000200 COPY STOCK.FDE.

000200 COPY TXTRAN.FDE.

       COPY WARHSE.FDE.

       COPY WSTOCK.FDE.

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
003370 77  WS-PRCKEY       PIC  9(04)    COMP-5 VALUE 1.
003370 77  WS-PURKEY       PIC  9(04)    COMP-5.
003380 77  WS-KEYSTR       PIC  9(04)    COMP-5.
003390 77  WS-RECOVER      PIC  9(04)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
003400 77  WS-PURLAST      PIC  9(04)    COMP-5.
       77  WS-VIEWKEY	   PIC	9(04)	 COMP-5.
       77  WS-V-FIRST	   PIC	9(04)	 COMP-5.
       77  WS-V-LAST	   PIC	9(04)	 COMP-5.
003410 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
003420 77  WS-ERROR        PIC  9(01)    VALUE 0.
       77  WS-ETYPE        PIC  X(01).
001770 77  WS-LOOP	   PIC	X(01)	 VALUE SPACE.
003430 77  WS-DEL          PIC  X(01).
003430 77  WS-NUM          PIC  Z9.
003470 77  WS-S            PIC  9(04)    COMP-5.
003480 77  WS-S1           PIC  9(04)    COMP-5.
003490 77  WS-S2           PIC  9(04)    COMP-5.
003500 77  WS-S3           PIC  9(04)    COMP-5.
003510 77  WS-S4           PIC  9(04)    COMP-5.
003510 77  WS-S9           PIC  9(04)    COMP-5.
003530 77  WS-ORD	   PIC	X(01).
003540 77  WS-OPTION       PIC  X(01).
003330 77  WS-LAB	   PIC	X(01).
003330 77  WS-PRICE        PIC  X(01).
003540 77  WS-SKIP	   PIC	X(01) VALUE "Y".
003540 77  WS-LOOK         PIC  X(01) VALUE "S".
003560 77  WS-CHAR1	   PIC	X(01).
       77  WS-USE-3	   PIC	X(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-BAR-CODE	   PIC	X(01).
003580 77  WS-IND	   PIC  9(01) VALUE 0.
003610 77  WS-SKEY         PIC  9(05).
003620 77  WS-KEY1         PIC  9(04)     COMP-5.
003640 77  WS-PAGE	   PIC	9(04)	  COMP-5 VALUE 0.
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
003950 77  WS-H1	   PIC X(05)	 VALUE "Date:".
003960 77  WS-H3	   PIC X(05)	 VALUE "Page:".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430	   03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	      03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC	X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
	       05  FILLER  PIC	X(01) VALUE "³".
	       05  WS-SPC  PIC	X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
002450	   03  WS-MD-LNE.
002460	       05  WS-MCR  PIC X(80) VALUE "³      ³
      - 	 "                     ³               ³             ³".
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).

003980 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "011".

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

       COPY BARCOD.ID.

       COPY CONTROL.ID.

000160 COPY CREDIT.ID.

000180 COPY CRTRAN.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000260 COPY PORDER.ID.

000300 COPY PRCLAB.ID.

000300 COPY PURCH.ID.

       COPY PURDEX.ID.

000280 COPY RECOVER.ID.

       COPY SHARED.ID.

000200 COPY STOCK.ID.

000200 COPY TXTRAN.ID.

       COPY WARHSE.ID.

       COPY WSTOCK.ID.

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
	       05  R-ITM	   PIC X(18).
	       05  FILLER	   PIC X(01).
	       05  R-DESC	   PIC X(24).
	       05  FILLER	   PIC X(01).
	       05  R-QNT	   PIC Z(04)9.99.
	       05  FILLER	   PIC X(01).
	       05  R-COST	   PIC Z(05)9.9999.
	       05  FILLER	   PIC X(01).
	       05  R-SELL	   PIC Z(05)9.99-.
	       05  FILLER	   PIC X(01).
	       05  R-EXT	   PIC Z(07)9.99-.

	   03  R-L3A REDEFINES W02-PRN-LINE.
	       05  I-H1 	   PIC X(08).
	       05  I-H2 	   PIC X(19).
	       05  I-H3 	   PIC X(30).
	       05  I-H4 	   PIC X(09).
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
	       05  G-T-HD.
		   07  FILLER	   PIC X(01).
		   07  G-H6A	   PIC X(04).
		   07  FILLER	   PIC X(01).
		   07  G-VAT	   PIC Z(05)9.99-.
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

       COPY W05.WS.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-SRTE        PIC S9(03)V99 COMP-3.
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
005310         05  W25-SVAL    PIC Z(07)9.99DB.
005320     03  W25-S7V2.
005330         05  W25-VAL2    PIC Z(06)9.99DB.
005280     03  W25-5V2 REDEFINES W25-S7V2.
005290         05  W25-VAL     PIC Z(07)9.99-.
005320     03  W25-72.
005330         05  W25-VAL3    PIC S9(07)V99.
005340     03  W25-PRC.
005350         05  W25-IPRC    PIC Z(06)9.999.

       COPY W40.WS.

       COPY FUNCTION.WS.

005450 COPY W50.WS.

005470 01  W50-H4.
005480     03  W50-HEAD        PIC X(20).

       01  W55-CALCS.
	   03  W55-VAT	       PIC S9(07)V99  COMP-3.

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
	   03  W75-S		PIC 9(02) COMP-5.
	   03  W75-S1		PIC 9(02) COMP-5.
	   03  W75-S2		PIC 9(02) COMP-5.
	   03  W75-CNO OCCURS 18.
	       05  W75-WHCDE.
		   07  W75-PKEY	PIC  9(05).
		   07  FILLER	PIC  X(01).
           03  W75-ITEMS OCCURS 18.
	       05  W75-CODE.
		   07  W75-POR	PIC  9(10).
		   07  FILLER	PIC  X(04).

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
	   03  W100-S8V2       PIC  Z(07)9.999.
	   03  W100-S8V3       PIC  Z(06)9.9999-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005700 SCREEN SECTION.

       COPY BLANK.CRT.

006110 01  S03.
006120	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006130                           VALUE "CREDITOR INVOICE".
006140	   03  LINE  4 COLUMN  2 VALUE "Creditor Ac. :".
006150	   03  LINE  5 COLUMN  2 VALUE "Invoice no   :".
006160	   03	       COLUMN 56 VALUE "Invoice date:".
006170     03  LINE  6 COLUMN  2 VALUE "Order number :".
006180     03  LINE  7 COLUMN  2 VALUE "Item code    :".
006200	   03  LINE  8 COLUMN  2 VALUE "Quantity(Pur):".
	   03	       COLUMN 31 VALUE "Sell(Units):".
006210	   03  LINE  9 COLUMN  2 VALUE "Unit cost    :".
      *    03	       COLUMN 31 VALUE "Rate       :".
      *    03	       COLUMN 58 VALUE "Currency :".
      *    03  LINE 10 COLUMN  2 VALUE "Rand cost    :".
           03          COLUMN 31 VALUE "Current    :".
006220     03  LINE 11 COLUMN  2 VALUE "Discount-1   :".
006230     03          COLUMN 31 VALUE "Discount-2 :".
006240	   03  LINE 12 COLUMN  2 VALUE "Unit nett Pur:".
           03          COLUMN 31 VALUE "VAT        :".
	   03	       COLUMN 56 VALUE "New average :".
           03  LINE 13 COLUMN  2 VALUE "Sell. prices -".
	   03	       COLUMN 17 VALUE " Wholesale".
	   03	       COLUMN 38 VALUE " Cash".
	   03	       COLUMN 52 VALUE " Retail".
           03  LINE 14 COLUMN  2 VALUE "VAT exclusive:".
           03  LINE 15 COLUMN  2 VALUE "VAT inclusive:".
           03  LINE 16 COLUMN  2 VALUE "Cur exclusive:".
           03  LINE 17 COLUMN  2 VALUE "VAT          :".
006250     03  LINE 18 COLUMN  2 VALUE "Excl - cost  :".
           03          COLUMN 54 VALUE "Sub total :".
006280     03  LINE 19 COLUMN  2 VALUE "Incl - cost  :".
           03          COLUMN 54 VALUE "Freight   :".
006290     03  LINE 20 COLUMN 54 VALUE "Discount  :".
006290     03  LINE 21 COLUMN 54 VALUE "VAT       :".
006300	   03  LINE 22 COLUMN 54 VALUE "Total     :".

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
005550				  PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03  FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
005550				  PIC Z(14) USING W10-I14 AUTO.

       01  S08.
	   03  LINE 10 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "¿".
	   03  LINE 11 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 27 FOREGROUND-COLOR 4
				 BACKGROUND-COLOR 7
				 VALUE " Number of Cases           ".
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 26 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "À".
	   03	       COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S09.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZZZZZZZ9 USING W10-CASES AUTO.

007450 01  S19.
007460	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC X(06) USING W15-ACNO AUTO.

007490 01  S21.
007500	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9 USING W15-OPREF AUTO.

007510 01  S22.
007520	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC 99 USING W15-OSUB AUTO.

007650 01  S29.
007660	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC X(08) USING W15-INVN AUTO.

007670 01  S30.
007680	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9/99/9999 USING W15-INVD AUTO.

007730 01  S33.
007740	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99DB USING W15-RAIL AUTO.

007750 01  S34.
007760	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99DB
007770                                 USING W15-DISCOUNT AUTO.

007780 01  S35.
007790	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC1 AUTO.

007800 01  S36.
007810	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z9.99 USING W15-DISC2 AUTO.

      *
      *    ****    F O R   C O N V E R T I O N S
      * 	   PURCHASE AND SELLING UNITS ARE DIFFERENT
      * 	   BUY PER CASE - SELL SINGLES
      * 	   BUY PER TON - SELL PER KG
      *
007820 01  S37.
007830	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(06)9.9(04) USING W15-PUN AUTO.

007820 01  S37S.
007830	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(06)9.999 USING W15-IUN AUTO.

007820*01  S37.
007830*    03  FOREGROUND-COLOR 0
      *        BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-IUN AUTO.

007820 01  S37-3.
007830	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(06)9.999 USING W15-IUN AUTO.


007840 01  S38.
007850	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(06)9.9999 USING W15-PCOST AUTO.

007530 01  S40.
007540	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.999 USING W15-COST AUTO.

007530 01  S41.
           02  S41A.
	   03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W15-UWSALE AUTO.
           02  S41B.
	   03  LINE 14 COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W15-UCASH AUTO.
           02  S41C.
	   03  LINE 14 COLUMN 49 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W15-USELL AUTO.
           02  S41D.
	   03  S41D-1.
	   05  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W10-VWSALE AUTO.
	   03  S41D-2.
	   05  LINE 15 COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W10-VCASH AUTO.
	   03  S41D-3.
	   05  LINE 15 COLUMN 49 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC Z(07)9.999 USING W10-VSELL AUTO.

       01  S42.
	   02  S42A.
	   03  LINE  4 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 24 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(40) FROM WS-SPC.
	   03  LINE  5 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(08) FROM WS-SPC.
	   03	       COLUMN 70 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(10) FROM WS-SPC.
	   02  S42B.
	   03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(14) FROM WS-SPC.
	   03	       COLUMN 36 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM WS-SPC.
	   03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 44 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 44 PIC  X(12) FROM WS-SPC.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(05) FROM WS-SPC.
	   03	       COLUMN 50 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(05) FROM WS-SPC.
	   03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 44 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03	       COLUMN 70 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(10) FROM WS-SPC.
	   03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 49 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 33 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 49 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03  LINE 16 COLUMN 17 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 33 PIC  X(12) FROM WS-SPC.
	   03	       COLUMN 49 PIC  X(12) FROM WS-SPC.
	   03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(12) FROM WS-SPC.
	   03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(11) FROM WS-SPC.
	   02  S42C.
	   03  LINE 18 COLUMN 66 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(13) FROM WS-SPC.
	   03  LINE 19 COLUMN 66 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(13) FROM WS-SPC.
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

007690 01  S45.
007700	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-IVAT AUTO.

007690 01  S46.
007700	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-INCOST AUTO.

007690 01  S47.
007700	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.999 USING W15-U-PNETT AUTO.

007690 01  S48.
007700	   03  FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 PIC Z(07)9.99 USING W15-U-VAT AUTO.

       01  S-POR.
	   03  LINE  6 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "³                     O R D E R
      -	       "   L O O K U P                      ³".
	   03  LINE  8 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ´".
	   03  LINE 21 COLUMN 9 FOREGROUND-COLOR 0
	       BACKGROUND-COLOR 7 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ".

       01  SD-POR.
	   03  COLUMN 10 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC Z(07)9 FROM ORD-ORD.
	   03  COLUMN 18 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
			 VALUE "/".
	   03  COLUMN 19 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC 99 FROM ORD-LINE.
	   03  COLUMN 22 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC X(14) FROM STK-CODE.
	   03  COLUMN 37 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC X(30) FROM STK-DESC.
	   03  COLUMN 68 FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
			 PIC Z(04)9.99 FROM ORD-QUANT.

       COPY S99.CRT.

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

       COPY WARLUP.SCR.

       COPY ERROR.CRT.

       COPY OPT.CRT.

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

       AA05.
004940	     PERFORM BA000.
	   IF NOT (WS-OPTION = "X")
	       PERFORM BA036
	       MOVE "Y" 	 TO WS-LOOP
019720	       GO TO AA05.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005260	     CLOSE BARCOD
		   PRCLAB
		   PURCH
		   RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY AA50CSW.LUP.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY OPTION.CRT.

      *
      *    ****    P R I N T   U T I L I T Y   -   A L L O W S
      * 	   F O R   P R I N T E R   A N D   D I S K
      *
       COPY PRINTUTL.AS4.

      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CRTRAN.RD.

      *
      *     ***  ****  ***** ****  *** *****     ***** *** *     *****
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *     ****  ***   *   *  *    *       ***    *  *     ***
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *     ***  *   * ***** ****  ***   *       *     *** ***** *****
      *
006080 READ-CREDIT.
006090       READ CREDIT
               KEY IS P-NUMBER.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT.
006200       GO TO READ-CREDIT-EXIT.

006210 READ-CREDIT-LOCK.
006220       READ CREDIT WITH KEPT LOCK
               KEY IS P-NUMBER.
006230     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006240         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006250     IF WS-STAT1 = "2" OR "3" OR "4"
006260         MOVE 3            TO WS-F-ERROR
006270         PERFORM READ-ERROR.
006280     IF RECORD-LOCKED
006290         PERFORM LOCKED-RECORD
006300         GO TO READ-CREDIT-LOCK.
006310     IF TOO-MANY-LOCKS
006320         UNLOCK CREDIT
006330         GO TO READ-CREDIT-LOCK.
006360       GO TO READ-CREDIT-EXIT.

006080 READ-CREDIT-NEXT.
006090       READ CREDIT NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT-NEXT.
006200       GO TO READ-CREDIT-EXIT.

006080 READ-CREDIT-PREV.
006090       READ CREDIT PREVIOUS.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT-PREV.
006200       GO TO READ-CREDIT-EXIT.

006080 START-AT-CRED-NAME.
006090       START CREDIT
               KEY >= P-NKEY.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.

006520 READ-CREDIT-EXIT.
006530       EXIT.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

       COPY PURCH.RD.

       COPY SHARED.RD.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
000010 READ-STOCK.
000020       READ STOCK
000030         KEY IS STK-CODE.
000040	   IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
		   MOVE 22	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT.
000070	   IF WS-STATUS = "23"
000080         MOVE 22           TO WS-F-ERROR
000090         GO TO READ-STOCK-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110         MOVE 22           TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000130     IF RECORD-LOCKED
000140         PERFORM LOCKED-RECORD
000150         GO TO READ-STOCK.
000160       GO TO READ-STOCK-EXIT.

000380 READ-STOCK-NEXT.
000390       READ STOCK NEXT.
000400     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-NEXT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450         MOVE 22           TO WS-F-ERROR
000460         GO TO READ-STOCK-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480         MOVE 22           TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000500     IF RECORD-LOCKED
000510         PERFORM LOCKED-RECORD
000520         GO TO READ-STOCK-NEXT.
000530       GO TO READ-STOCK-EXIT.

000380 READ-STOCK-PREV.
000390       READ STOCK PREVIOUS.
000400     IF WS-STATUS = "00"
	       IF STK-DISC NOT = 9
000050		   MOVE ZERO	 TO WS-F-ERROR
000060		   GO TO READ-STOCK-EXIT
	       ELSE
000060		   GO TO READ-STOCK-PREV.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450         MOVE 22           TO WS-F-ERROR
000460         GO TO READ-STOCK-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480         MOVE 22           TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000500     IF RECORD-LOCKED
000510         PERFORM LOCKED-RECORD
000520         GO TO READ-STOCK-PREV.
000530       GO TO READ-STOCK-EXIT.

000750 START-AT-STOCK-CODE.
000760       START STOCK 
000770         KEY >= STK-CODE.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800         GO TO READ-STOCK-EXIT.
000810     IF WS-STATUS = "23"
000820         MOVE 22           TO WS-F-ERROR
000830         GO TO READ-STOCK-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850         MOVE 22           TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870       GO TO READ-STOCK-EXIT.

000890 START-AT-ALT-CODE.
000900       START STOCK
000910         KEY >= STK-ACODE.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940         GO TO READ-STOCK-EXIT.
000950     IF WS-STATUS = "23"
000960         MOVE 22           TO WS-F-ERROR
000970         GO TO READ-STOCK-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990         MOVE 22           TO WS-F-ERROR
001000         PERFORM READ-ERROR.
001010       GO TO READ-STOCK-EXIT.

001310 START-AT-STOCK-DESC.
001320       START STOCK
001330         KEY >= STK-DKEY.
001340     IF WS-STATUS = "00"
001350         MOVE ZERO         TO WS-F-ERROR
001360         GO TO READ-STOCK-EXIT.
001370     IF WS-STATUS = "23"
001380         MOVE 22           TO WS-F-ERROR
001390         GO TO READ-STOCK-EXIT.
001400     IF WS-STAT1 = "2" OR "3" OR "4"
001410         MOVE 22           TO WS-F-ERROR
001420         PERFORM READ-ERROR.
001430       GO TO READ-STOCK-EXIT.

001450 START-AT-STOCK-XREF.
001460       START STOCK
001470         KEY >= STK-XREF.
001480     IF WS-STATUS = "00"
001490         MOVE ZERO         TO WS-F-ERROR
001500         GO TO READ-STOCK-EXIT.
001510     IF WS-STATUS = "23"
001520         MOVE 22           TO WS-F-ERROR
001530         GO TO READ-STOCK-EXIT.
001540     IF WS-STAT1 = "2" OR "3" OR "4"
001550         MOVE 22           TO WS-F-ERROR
001560         PERFORM READ-ERROR.

001580 READ-STOCK-EXIT.
001590       EXIT.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

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

       COPY PORDER.WR.

000300 COPY PRCLAB.WR.

       COPY PURCH.WR.

       COPY PURDEX.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

000200 COPY TXTRAN.WR.

       COPY WSTOCK.WR.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011730 AY000           SECTION.
011740 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
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
008010	     MOVE 07		 TO REC-FILE.
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
011920       MOVE CT-REC         TO REC-CRTRAN.
011930       GO TO AY50.

011940 AY10.
008160	     MOVE 10		 TO REC-FILE.
008170       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-PORDER THRU READ-PORDER-EXIT
	       IF WS-F-ERROR = 16
		   GO TO AY59.
008180       MOVE ORD-RECORD1 TO REC-PORDER.
008190       GO TO AY50.

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

011070 AY056.
011080	     MOVE 56		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE PRD-REC1	 TO REC-PURDEX.
011110       GO TO AY50.

010970 AY052.
010980       MOVE 52             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
011000       MOVE WST-RECORD1    TO REC-WSTOCK.
011010       GO TO AY50.

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
	     CLOSE RECOVER.
             OPEN I-O RECOVER.

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
           IF WS-SKIP = "N"
	       IF WS-STOCK = "Y"
012710		   MOVE 3	 TO WS-SHARED
012720		   PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
		   PERFORM AY37 THRU AY59
		   MOVE SHR-RECORD
				 TO NET-RECORD
	       ELSE
012710		   MOVE 3	 TO WS-NETKEY
012720		   PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730		   PERFORM AY39 THRU AY59.
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
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   COMMIT
		   GO TO AY999.

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
011870	     EXIT.

       COPY CA000.STK.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   
      *           A L T E R N A T E   C O D E
      *
013490 CA100         SECTION.
013500 CA101.
013510       MOVE 10             TO W50-LENGTH.
013520 CA105.
013530       MOVE W10-ACODE      TO W50-DATA.
013540     IF W50-1 = SPACE
013550         MOVE W50-9        TO W10-ACODE
013560         GO TO CA105.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013620	     MOVE W50-ACODE	 TO STK-ACODE W10-ACODE.

013700 CA119.
013710       EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY LEDTRF.UPD.

      /
013960 BA000	     SECTION 5.
013970 BA00A.
	   IF WS-LOOP = "Y"
	       GO TO BA00.
013980       MOVE "GOODS RECEIVED" 
                                 TO W50-HEAD.
013990	     MOVE ZERO		 TO W15-TOTAL WS-PAGE.

014180 BA00.
014190       DISPLAY CLR-SCREEN.
014200       DISPLAY S03.
	     DISPLAY S42.
	   IF WS-EXT-STK = "Y"
	       DISPLAY "    " AT 0731
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	   ELSE
	   IF WS-USE-ITM = "Y"
	       DISPLAY "/" AT 0731
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 1
		       "   " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
014210       MOVE 1              TO WS-PURLAST.
             MOVE SPACES         TO W15-ACNO.

014220 BA05.
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
014230	     ACCEPT S19 AT 0417.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO BA06
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
018810		     PERFORM AA50C THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE
018820	       DISPLAY S19 AT 0417
018830	       IF W15-ACNO = SPACES
018840		   GO TO BA05.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       BA06.
014300	     DISPLAY CLEAR-L25.
014240     IF W15-ACNO = SPACES
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014260	       MOVE "TOTAL"	 TO G-T-HD
014270         MOVE W15-TOTAL    TO G-TOTAL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-L1
	       MOVE "X" 	 TO WS-OPTION
014290	       GO TO BA35-EXIT.
014320	     PERFORM BF155-GET-CREDITOR.
014330     IF WS-ERROR NOT = ZERO
014340	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
014350         GO TO BA05.
014360       MOVE P-NAME         TO W15-NAME.
014370	     DISPLAY P-NAME AT 0424
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
012180	     PERFORM CHECK-CORRECT.
014450	   IF WS-OPTION = "N"
014460         GO TO BA05.
	     MOVE SPACES	 TO W15-INVN.
014480	     MOVE ZERO		 TO W15-INVD.
014490       MOVE 1              TO WS-S.

014500 BA10.
014510       MOVE ZERO           TO W65-CFLOW (WS-S).
014520     IF WS-S < 6
014530         ADD 1             TO WS-S
014540         GO TO BA10.

014550 BA12.
014560       ACCEPT S29 AT 0517.
014570	   IF W15-INVN = SPACES
014580         GO TO BA12.

014590 BA13.
014600	     ACCEPT S30 AT 0570.
014610       MOVE W15-INVD       TO W15-DATE.
014620	     PERFORM BC000.
	     MOVE W15-DD	 TO W20-DD W25-DD.
	     MOVE W15-MM	 TO W20-MM W25-MM.
	     MOVE W15-CC	 TO W20-CC W25-CC.
	     MOVE W15-YY	 TO W20-YY W25-YY.
014630     IF WS-ERROR = 1
014640         GO TO BA13.
014730     IF WS-OPTION = "N"
014740         GO TO BA05.
	     MOVE SPACES	 TO W15-WHSE.
      *
      *    ****   W A R E H O U S I N G   A P P L I C A B L E	?
      *
	   IF NOT (LS0-WHS = 2)
	       GO TO BA14-A.

       BA14.
	     MOVE "Warehouse purchases Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
033910	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BA13
               END-EVALUATE.
014920	   IF NOT (WS-OPTION = "N" OR "Y")
014930	       GO TO BA14.
	   IF WS-OPTION = "Y"
               PERFORM HA000.

       BA14-A.
014750	     MOVE ZERO		 TO W15-QUANT W15-TCOST W15-DISCOUNT
                                    W15-DISC1 W15-DISC2 W15-INCVAL
                                    W15-TSELL W15-TXBL  W15-VAT
				    W15-IVAT  W15-RAIL.
014770	     MOVE SPACES	 TO WS-ORD.
	     MOVE 1		 TO WS-S9.

       BA14-B.
018660	     MOVE ZERO		 TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BA14-B.

014780 BA15.
014790       MOVE ZERO           TO W15-ORD.
014830	     MOVE "Order no (Y/N)?  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.

014850 BA16.
033910	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-ORD.
014920	   IF NOT (WS-ORD = "N" OR "Y")
014930	       GO TO BA15.
014940	     DISPLAY CLEAR-L25.

014950 BA20.
014960     IF WS-ORD = "N"
014970         GO TO BA23A.
	     DISPLAY  "F2" AT 2502 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 6 HIGHLIGHT
		      "=View Orders or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
014980	     ACCEPT S21 AT 0617.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ORD
		     DISPLAY "        " AT 0617
			      WITH FOREGROUND-COLOR 0
				   BACKGROUND-COLOR 7
		 WHEN F2-KEY
018810		     PERFORM PORDER-LOOKUP
		     IF W15-ORD = ZERO
			 GO TO BA20
		     END-IF
014980		     DISPLAY S21 AT 0617
015020		     DISPLAY "/" AT 0625
015030		     DISPLAY S22 AT 0626
		     GO TO BA22
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA20
	       END-EVALUATE.
014990	   IF W15-OPREF = ZERO
015000         SUBTRACT 1        FROM WS-PURLAST
015010         GO TO BA35.
015020       DISPLAY "/" AT 0625.

       BA21.
015030       ACCEPT S22 AT 0626.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W15-ORD
		     DISPLAY "        " AT 0617
015000		     SUBTRACT 1	 FROM WS-PURLAST
015010		     GO TO BA35
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA21
	       END-EVALUATE.
015040	     DISPLAY CLEAR-L25.

015090 BA22.
	     MOVE W15-ORD	 TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
015190	       MOVE WS-ER1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015200         GO TO BA20.
	     MOVE ZERO		 TO W10-CASES.
015140       GO TO BA24.

015230 BA23A.
015240	     MOVE SPACES	 TO W10-EXT-ITEM W15-DESCR.
015250	     MOVE ZERO		 TO ORD-TERMS W15-ICOST
				    W10-CASES W15-PCOST.
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

015260 BA23B.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
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
		     GO TO BA23BE
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
		     PERFORM BD100
		     MOVE LS-STOCK-CODE
				 TO W10-EXT-ITEM
		     MOVE SPACES TO LS-STOCK-CODE
		     GO TO BA23B
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA23B
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
021070		       DISPLAY S07 AT 0717.
016580	       IF W10-EXT-ITEM = SPACES
016590		   GO TO BA23B.

       BA23BE.
	     DISPLAY CLEAR-L25.
015290	   IF W10-EXT-ITEM = SPACES
015300         SUBTRACT 1        FROM WS-PURLAST
015310         GO TO BA35.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.

       BA23-GET-STOCK.
	     MOVE W05-RATE	 TO W05-SRTE.
015320	     PERFORM CA000.
	   IF STK-PRC = ZERO
	       MOVE 1		 TO STK-PRC.
	   IF STK-IND < 2
	       IF (STK-PCOST = ZERO) OR (STK-PRC = 1)
		   MOVE STK-COST TO STK-PCOST.
	     MOVE W05-SRTE	 TO W05-RATE.
015330	   IF NOT (WS-F-ERROR = ZERO)
	       MOVE "Z"		 TO W10-ETYPE
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
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BA23B
               ELSE
018720             GO TO BA23-GET-STOCK.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA23B.
	     MOVE ZERO		 TO W15-WQUANT.
      *
      *    ****   W A R E H O U S I N G   A P P L I C A B L E	?
      *
	   IF LS0-WHS = 2
	       PERFORM IA000.

015370 BA23C.
015390	     MOVE ZERO		 TO W15-IUN W15-PUN.
015400       MOVE W05-DATE       TO W15-DEL.
015410       GO TO BA24A.

015420 BA24.
015460	     MOVE ORD-EXT-ITEM	 TO W10-EXT-ITEM.
015470	     MOVE ORD-QUANT	 TO W15-PUN.
015480       MOVE ORD-DELIV      TO W15-DEL.
	     MOVE ORD-UCOST	 TO W15-PCOST.
      *      MOVE ORD-CURRENCY	 TO W15-CURRENCY.
      *      MOVE ORD-DRATE	 TO W15-RATE.
015550	     DISPLAY W10-EXT-ITEM AT 0717
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     MOVE W05-RATE	 TO W05-SRTE.
015320	     PERFORM CA000.
	     MOVE W05-SRTE	 TO W05-RATE.
	   IF WS-F-ERROR NOT = ZERO
	       MOVE "Stock record not on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BA20.
	     MOVE ZERO		 TO W15-WQUANT.
      *
      *    ****   W A R E H O U S I N G   A P P L I C A B L E	?
      *
	   IF LS0-WHS = 2
	       PERFORM IA000.

015500 BA24A.
	     DISPLAY STK-DESC AT 0736
		     WITH FOREGROUND-COLOR 1
			  BACKGROUND-COLOR 7.
      *
      *    ****    GET THE APPLICABLE VAT RATE (W05-RATE, WS-VAT-SUB)
      *
	     MOVE STK-TAX	 TO WS-VAT-SUB.
      *
      *    ****    DOES THE PREVIOUS RATE APPLY
      *
	   IF (WS-VAT-SUB > 0 AND < 7)
	       IF W20-DTE < WS-VAT-DATE(WS-VAT-SUB)
		   ADD 6	 TO WS-VAT-SUB.
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE
	   ELSE
	       MOVE ZERO	 TO W05-RATE.
	   IF STK-PCOST = ZERO
	       MOVE STK-COST	 TO STK-PCOST.
	   IF WS-ORD = "N"
015510	       IF STK-IND > 1
015520		   MOVE ZERO	 TO W15-PCOST W100-S8V3
015530	       ELSE
015540		   MOVE STK-PCOST
				 TO W15-PCOST W100-S8V3
	       END-IF
	   ELSE
	       MOVE STK-COST	 TO W100-S8V3.
      *
      *    ****    C A L C U L A T E   I N C L U S I V E   C O S T
      *
	   IF P-TAX = "I"
	       COMPUTE W15-PCOST ROUNDED = W15-PCOST +
		   (W15-PCOST * W05-RTE)
	       COMPUTE W100-S8V3 ROUNDED = STK-PCOST +
		   (STK-PCOST * W05-RTE).
015550*      DISPLAY W10-EXT-ITEM AT 0717
      *		 WITH FOREGROUND-COLOR 0
      *		      BACKGROUND-COLOR 7.
      *
      *    ****    I S	 I T   A   T E X T   I T E M
      *
015560     IF STK-IND = 3
015570	       ACCEPT W15-DESCR AT 0736
		      WITH FOREGROUND-COLOR 0
			   BACKGROUND-COLOR 7 AUTO
015580     ELSE
015590         MOVE STK-DESC     TO W15-DESCR
015600	       DISPLAY STK-DESC AT 0736
		       WITH FOREGROUND-COLOR 1
			    BACKGROUND-COLOR 7.
	     PERFORM CHECK-CORRECT.
016090	   IF WS-OPTION = "N"
016100         GO TO BA20.
	     DISPLAY W100-S8V3 AT 0944 WITH FOREGROUND-COLOR 14.
             MOVE STK-WSALE      TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1617 WITH FOREGROUND-COLOR 14.
             MOVE STK-CASH       TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1633 WITH FOREGROUND-COLOR 14.
             MOVE STK-SELL       TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1649 WITH FOREGROUND-COLOR 14.

015610 BA25.
      *	   IF WS-USE-3 = "Y"
      *	       ACCEPT S37-3 AT 0817
      *	   ELSE
015620	     ACCEPT S37 AT 0817.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA23B
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA25
	       END-EVALUATE.
      *
      *    ****    C O N V E R T I O N	 F R O M   P U R C H A S E
      * 	   U N I T   T O   S E L L I N G   U N I T
      *
015630	   IF W15-PUN = ZERO
015640	       GO TO BA25.
	   IF STK-PRC = ZERO
	       MOVE 1		 TO STK-PRC.

       BA25B.
	     MULTIPLY W15-PUN BY STK-PRC
				 GIVING W15-IUN.
015620	     ACCEPT S37S AT 0844.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA25B
	       END-EVALUATE.
015630	   IF W15-IUN = ZERO
015640	       GO TO BA25B.
      *
      *    ****   A R E   C A S E S   C O N T R O L L E D
      *
	   IF WS-USE-CASES= "Y"
	       IF STK-USE-CASES = "Y"
      * 			     GET THE NUMBER OF CASES RECEIVED
		   PERFORM BF000.

015650 BA26.
015660	     ACCEPT S38 AT 0917.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA26
	       END-EVALUATE.
015670	   IF W15-PCOST = ZERO
015680         GO TO BA26.
      *
      *    ****   C O S T   P E R   S E L L I N G   U N I T
      *
	    COMPUTE W15-ICOST ROUNDED =
		   (W15-PCOST * W15-PUN) / W15-IUN.
      *     MOVE W15-OCOST	 TO W15-ICOST.
      * BA26A.
      *	     ACCEPT S23R AT 0949.
      *	 IF USER-FUNC
      *	     EVALUATE KEY-CODE-1
      *	 WHEN ESC-KEY	 GO TO BA20
      *	 WHEN OTHER
      *	     CALL X"E5"
      *	     GO TO BA26A
      *	     END-EVALUATE.
033110*     IF W15-RATE = ZERO
033120* 	GO TO BA26A.
      * BA26B.
      *    IF W15-RATE = 1.00
      * 	MOVE W15-OCOST	  TO W15-ICOST
      *        MOVE "Rand"	 TO W15-CURRENCY
      *        DISPLAY S23C AT 0970
      *        GO TO BA26D.
      *BA26C.
      *      ACCEPT S23C AT 0970.
      *   IF USER-FUNC
      *       EVALUATE KEY-CODE-1
      *	 WHEN ESC-KEY	 GO TO BA20
      *	 WHEN OTHER
      *	     CALL X"E5"
      *	     GO TO BA26C
      *       END-EVALUATE.
      *    IF W15-CURRENCY = SPACES
      *        GO TO BA26C.
      *      COMPUTE W15-ICOST ROUNDED = W15-OCOST * W15-RATE.
      *BA26D.
033100*      DISPLAY S38 AT 1017.

       BA28.
             MOVE ZERO           TO W15-U-TXBL
				    W15-U-NTXBL
				    W15-U-VAT.
           IF STK-TAX NOT = ZERO
               IF P-TAX = "N"
		   MOVE W15-PCOST
				 TO W15-U-NTXBL
                                    W15-U-INCVAL
				    W15-U-PNETT
               ELSE
               IF P-TAX = "I"
      *
      *    ****    I N C L U S I V E   V A L U E
      *
		   MOVE W15-PCOST
				 TO W15-U-INCVAL
      *
      *    ****    E X C L U S I V E   V A L U E
      *
		   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL -
                       ((W15-U-INCVAL / (100.00000 + W05-RATE)) 
                       * W05-RATE)
                   MOVE W15-U-TXBL
				 TO W15-U-PNETT
      *
      *    ****    V A T   V A L U E
      *
		   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL
                                 GIVING W15-U-VAT
               ELSE
      *
      *    ****    E X C L U S I V E   V A L U E
      *
		   MOVE W15-PCOST
				 TO W15-U-TXBL
				    W15-U-PNETT
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
	       MOVE W15-PCOST	 TO W15-U-NTXBL
				    W15-U-INCVAL
				    W15-U-PNETT.
      *
      *    ****   U N I T   N E T   C O S T
      *
	     COMPUTE W15-U-NETT ROUNDED =
		    (W15-PCOST * W15-PUN) / W15-IUN.
      *
      *    ****   L I N E   C O S T
      *
	   IF W15-U-TXBL NOT = ZERO
	       MULTIPLY W15-PUN BY W15-U-TXBL
				 GIVING W15-COST ROUNDED
           ELSE
015690	       MULTIPLY W15-PUN BY W15-U-NTXBL
				 GIVING W15-COST ROUNDED.
      *
      *    ****   L I N E   I N C L U S I V E	C O S T
      *
	   IF (P-TAX = "E") AND (STK-TAX NOT = ZERO)
               COMPUTE W15-INCOST ROUNDED = (W15-COST +
                   (W15-COST * W05-RTE))
           ELSE
015690	       MULTIPLY W15-PUN BY W15-U-INCVAL
				 GIVING W15-INCOST ROUNDED.
             SUBTRACT W15-COST FROM W15-INCOST
				 GIVING W15-IVAT ROUNDED.
015690	     MULTIPLY W15-PUN BY W15-PCOST
				 GIVING W15-ILCOST ROUNDED.
      *
      *      ***** UNIT NETT COST *****
      *
             DISPLAY S47 AT 1217.
      *
      *      ***** VAT PER UNIT *****
      *
             DISPLAY S48 AT 1244.
      *
      *      ***** TOTAL VAT *****
      *
015710       DISPLAY S45 AT 1717.
      *
      *      ***** COST EXCLUDING VAT *****
      *
015710       DISPLAY S40 AT 1817.
      *
      *      ***** COST INCLUDING VAT *****
      *
015710       DISPLAY S46 AT 1917.
      *
      *      ***** CALCULATE NEW COST PRICE *****
      *            1st DISCOUNT
      *
       BA29.
015720	     ACCEPT S35 AT 1123.
015730     IF W15-DISC1 = ZERO
015740         GO TO BA30A.
015750       COMPUTE W25-V1 ROUNDED = (W15-ILCOST -
015760		     (W15-ILCOST * W15-DSC1)).
015770	     COMPUTE W15-PCOST ROUNDED = W25-V1 / W15-PUN.
015770	     COMPUTE W15-ICOST ROUNDED =
		    (W15-PCOST * W15-PUN) / W15-IUN.
             PERFORM BA28.
      *
      *            2nd DISCOUNT
      *
015810       ACCEPT S36 AT 1150.
015820     IF W15-DISC2 = ZERO
015830         GO TO BA30A.
015840       COMPUTE W25-V1 ROUNDED = (W15-ILCOST -
015850		     (W15-ILCOST * W15-DSC2)).
015770	     COMPUTE W15-PCOST ROUNDED = W25-V1 / W15-PUN.
015770	     COMPUTE W15-ICOST ROUNDED =
		    (W15-PCOST * W15-PUN) / W15-IUN.
015710	     PERFORM BA28.
      *
      *      ***** CHECK IF AVERAGE COST TO BE CALCULATED *****
      *
       BA30A.
	   IF STK-IND > 1
017600         COMPUTE W15-AVRG ROUNDED = W15-COST / W15-IUN
               GO TO BA31.
017590	   IF (STK-QUANT + W15-WQUANT) < 0.01
017600         COMPUTE W15-AVRG ROUNDED = W15-COST / W15-IUN
017610     ELSE
017620	       COMPUTE W25-V1 = (STK-QUANT + W15-WQUANT) * STK-AVGE
017630         ADD W15-COST      TO W25-V1
017640         COMPUTE W15-AVRG ROUNDED = W25-V1 / 
				(STK-QUANT + W15-WQUANT + W15-IUN).
      *
      *    ****    N E W   A V E R A G E    ****
      *
       BA31.
	     MOVE W15-AVRG	 TO W100-S6V2.
	     DISPLAY W100-V2 AT 1270
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.

015900 BA32.
	   IF STK-MARKUP = ZERO
	       PERFORM BA100.
      *
      *      ***** CALCULATE NEW SELLING PRICE *****
      *
           IF W15-AVRG > W15-U-NETT
015930         COMPUTE W15-USELL ROUNDED = (W15-AVRG + 
015940             (W15-AVRG * STK-MARKUP / 100.00000))
015930         COMPUTE W15-UWSALE ROUNDED  = (W15-AVRG + 
015940             (W15-AVRG * STK-WMARKUP / 100.00000))
015930         COMPUTE W15-UCASH ROUNDED = (W15-AVRG + 
015940             (W15-AVRG * STK-CMARKUP / 100.00000))
           ELSE
015930         COMPUTE W15-USELL ROUNDED = (W15-U-NETT + 
015940             (W15-U-NETT * STK-MARKUP / 100.00000))
015930         COMPUTE W15-UWSALE ROUNDED = (W15-U-NETT + 
015940             (W15-U-NETT * STK-WMARKUP / 100.00000))
015930         COMPUTE W15-UCASH ROUNDED = (W15-U-NETT + 
015940             (W15-U-NETT * STK-CMARKUP / 100.00000)).
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

       BA32A.
           IF STK-TAX NOT = ZERO
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
             DISPLAY S41.

       BA32B.
           IF W15-USELL = STK-SELL
		GO TO BA32D.
	     MOVE SPACE		 TO WS-OPTION.

       BA32C.
	     MOVE "Use new selling prices Y/N  [ ]"
				 TO WS-ERR-MES.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO BA32C.
014730     IF WS-OPTION = "Y"
014740	       GO TO BA32D.
	   IF NOT (STK-WSALE = ZERO)
017510	       MOVE STK-WSALE	 TO W15-UWSALE.
	   IF NOT (STK-CASH = ZERO)
017510	       MOVE STK-CASH	 TO W15-UCASH.
	   IF NOT (STK-SELL = ZERO)
017510	       MOVE STK-SELL	 TO W15-USELL.
	     PERFORM BA32A.

       BA32D.
	   IF NOT (STK-TAX = ZERO)
	       GO TO BA32E.

       BA32DA.
	     MOVE "Adjust selling prices Y/N  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO BA32E.
	   IF WS-OPTION = "N"
	       GO TO BA33B-1.
	     GO TO BA33.

       BA32E.
	     MOVE "Adjust 'E'x/'I'nclusive price or 'N'one  [E]"
				 TO WS-ERR-MES.
	     MOVE "E"		 TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "E" OR "I" OR "N")
	       GO TO BA32E.
	   IF WS-OPTION = "N"
	       GO TO BA33B-1.
	   IF WS-OPTION = "E"
	       GO TO BA33.

       BA32F.
015960	     ACCEPT S41D-1.
	   IF W10-VWSALE = ZERO
	       GO TO BA32F.
	     COMPUTE W15-UWSALE ROUNDED = (W10-VWSALE /
		     (100.000 + W05-RATE)) * 100.000
015970	   IF W15-UWSALE < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980	       GO TO BA32F.

015950 BA32G.
015960	     ACCEPT S41D-2.
	   IF W10-VCASH = ZERO
	       GO TO BA32G.
	     COMPUTE W15-UCASH ROUNDED = (W10-VCASH /
		     (100.000 + W05-RATE)) * 100.000
015970	   IF W15-UCASH < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980	       GO TO BA32G.

015950 BA32H.
015960	     ACCEPT S41D-3.
	   IF W10-VSELL = ZERO
	       GO TO BA32G.
	     COMPUTE W15-USELL ROUNDED = (W10-VSELL /
		     (100.000 + W05-RATE)) * 100.000
015970	   IF W15-USELL < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980	       GO TO BA32H.
015990	     GO TO BA33B-1.

015950 BA33.
015960       ACCEPT S41A.
015970	   IF W15-UWSALE < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980         GO TO BA33.

015950 BA33A.
015960       ACCEPT S41B.
015970	   IF W15-UCASH < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980         GO TO BA33A.

015950 BA33B.
015960       ACCEPT S41C.
015970	   IF W15-USELL < W15-ICOST
	       MOVE WS-ER2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015980         GO TO BA33B.
	     PERFORM BA32A.

       BA33B-1.
	     EXIT.

       BA33B-PACKS.
	   IF WS-USE-PACKS = "Y"
	       IF STK-USE-PACKS = "Y"
		   PERFORM BG000
		   IF WS-ERROR = 9
		       MOVE "X"  TO WS-OPTION
		       GO TO BA35-EXIT.

       BA33B-1A.
015990       MULTIPLY W15-USELL BY W15-IUN GIVING W15-SELL.
012180	     PERFORM CHECK-CORRECT.
016090	   IF WS-OPTION = "N"
	       DISPLAY S42B
016100         GO TO BA20.
           IF P-TAX NOT = "N"
               IF STK-TAX > ZERO
                   ADD W15-COST  TO W15-TXBL.
             ADD W15-INCOST      TO W15-INCVAL.
016120       ADD W15-IUN         TO W15-QUANT.
016130       ADD W15-COST        TO W15-TCOST.
016140       ADD W15-SELL        TO W15-TSELL.
	   IF NOT (WS-VAT-SUB = ZERO)
	       ADD W15-COST	 TO W15-TAXVAL(WS-VAT-SUB)
	       ADD W15-IVAT	 TO W15-VATVAL(WS-VAT-SUB).
016150       MOVE WS-PURLAST     TO WS-PURKEY.

       BA33C.
	     MOVE "'P'rice,'B'arcode,'N'o labels  [ ]"
				 TO WS-ERR-MES.
             MOVE SPACE          TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "B" OR "N" OR "P")
               GO TO BA33C.
016160	     INITIALIZE PUR-RECORD1.
016170     IF W15-ORD NOT = ZERO
016180         MOVE W15-ORD      TO PUR-ORD.
016220	     MOVE W10-EXT-ITEM	 TO PUR-EXT-STOCK.
	     MOVE W15-PUN	 TO PUR-PUN.
016230       MOVE W15-IUN        TO PUR-IUN.
016240       MOVE W15-ICOST      TO PUR-ICOST.
016240	     MOVE W15-PCOST	 TO PUR-PCOST.
016250       MOVE W15-UWSALE     TO PUR-UWSALE.
016250       MOVE W15-UCASH      TO PUR-UCASH.
016250	     MOVE W15-USELL	 TO PUR-USELL.
016260       MOVE W15-COST       TO PUR-COST.
             MOVE W15-INCOST     TO PUR-INCVAL.
016270       MOVE W15-SELL       TO PUR-SELL.
             MOVE WS-OPTION      TO PUR-LAB.
	     MOVE W15-IVAT	 TO PUR-IVAT.
	     MOVE WS-VAT-SUB	 TO PUR-VAT-SUB.
	   IF WS-USE-PACKS = "Y"
	       MOVE W10-WSALE-2	 TO PUR-WSALE-2
	       MOVE W10-CASH-2	 TO PUR-CASH-2
	       MOVE W10-SELL-2	 TO PUR-SELL-2
	       MOVE W10-WSALE-3	 TO PUR-WSALE-3
	       MOVE W10-CASH-3	 TO PUR-CASH-3
	       MOVE W10-SELL-3	 TO PUR-SELL-3
	       MOVE W10-WSALE-4	 TO PUR-WSALE-4
	       MOVE W10-CASH-4	 TO PUR-CASH-4
	       MOVE W10-SELL-4	 TO PUR-SELL-4.
	   IF WS-USE-CASES = "Y"
	       MOVE W10-CASES	 TO PUR-CASES.
016280       PERFORM WRITE-PURCH THRU WRITE-PURCH-EXIT.

       BA33D.
016300       MOVE W15-TCOST      TO W25-SVAL.
016320	     DISPLAY W25-S5V2 AT 1866
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 0.
	     MOVE 1		 TO WS-S9.
	     MOVE ZERO		 TO W15-VAT.
      *
      *    ***** Accumulate the tax *****
      *
       BA33-TAX.
	     ADD W15-VATVAL(WS-S9)
				 TO W15-VAT.
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BA33-TAX.
      *
      *    ***** Display the total VAT *****
      *
	     DISPLAY S43 AT 2166.
016770	     COMPUTE W25-SVAL = W15-TCOST + W15-VAT.
016780	     DISPLAY W25-S5V2 AT 2266
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 0.
	     DISPLAY S42B.

       BA34-CHK.
016400	     MOVE "'N'ext Item, 'V'iew or 'E'nd  [N]"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.

016420 BA34A.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "N" OR "E" OR "V")
	       GO TO BA34-CHK.
016490     IF WS-OPTION = "E"
016500         GO TO BA35.
	   IF WS-OPTION = "V"
	       PERFORM BE000
	       GO TO BA33D.
      *
      *    ***** Get the next item *****
      *
016530       ADD 1               TO WS-PURLAST.
016540	     GO TO BA15.
      *
      *    ***** Were there any entries? *****
      *
016550 BA35.
016560     IF WS-PURLAST = ZERO
016570	       GO TO BA00.
      *
      *    ***** Users last chance to amend or cancel *****
      *
016610 BA35A.
016600	     MOVE "P"	     TO WS-OPTION.
	     MOVE SPACES     TO WS-ERR-MES.
016580	     STRING "'P'rocess, 'C'ancel or 'M'ore items  ["
		    DELIMITED SIZE
		    WS-OPTION DELIMITED SIZE
		    "]" DELIMITED SIZE
			     INTO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "P" OR "M")
               GO TO BA35A.
016690     IF WS-OPTION = "C"
016700         GO TO BA00.
016710     IF WS-OPTION = "M"
016720	       GO TO BA34-CHK.
      *
      *    ***** Transport / Railage / Freight *****
      *
016730 BA35B.
016750       ACCEPT S33 AT 1966.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BA34-CHK
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA35B
	       END-EVALUATE.
      *
      *    ***** Calculate Sub-Total *****
      *
016770	     COMPUTE W25-SVAL = W15-TCOST + W15-RAIL +
				W15-VAT.
016780	     DISPLAY W25-S5V2 AT 2266
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     DISPLAY "Press '-' key for negative" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
      *
      *    ***** Discount / Adjustment *****
      *
       BA35C.
016760       ACCEPT S34 AT 2066.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BA34-CHK
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA35C
	       END-EVALUATE.
016740	     DISPLAY CLEAR-L25.
	     MOVE 1		 TO WS-S9.
	     MOVE ZERO		 TO W15-VAT.
      *
      *    ***** Accumulate the tax *****
      *
       BA35-TAX.
	     ADD W15-VATVAL(WS-S9)
				 TO W15-VAT.
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BA35-TAX.
	     MOVE W15-VAT	 TO W55-VAT.
016770	     COMPUTE W25-SVAL = W15-TCOST + W15-RAIL +
                                W15-VAT + W15-DISCOUNT.
016780	     DISPLAY W25-S5V2 AT 2266
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
      *      MOVE W05-VAT (WS-VAT-SUB)
      *				 TO W05-RATE.
	   IF P-TAX = "E"
      *        COMPUTE W15-VAT ROUNDED =
      *	      (W15-TXBL + W15-RAIL + W15-DISCOUNT) * W05-RTE
016770*       COMPUTE W25-SVAL = W15-TCOST + W15-RAIL +
      *			  W15-VAT + W15-DISCOUNT
016780*       DISPLAY W25-S5V2 AT 2266
      *	       WITH FOREGROUND-COLOR 0
      *		    BACKGROUND-COLOR 7
      *
      *    ***** Accept the total VAT (Allow for adjustments) *****
      * 	 E X C L U S I V E
      *
	       ACCEPT S43 AT 2166
      *
      *    ***** If VAT has been adjusted - Re-allocate to the various
      * 	 VAT percentages *****
      *
	       IF NOT (W15-VAT = W55-VAT)
		   PERFORM BA00C
	       END-IF
	   ELSE
      *
      *    ***** No VAT *****
      *
	   IF NOT (P-TAX = "I")
      *	       COMPUTE W15-VAT ROUNDED = W15-VAT +
      * 	     ((W15-RAIL + W15-DISCOUNT) * W05-RTE)
      *    ELSE
               MOVE ZERO         TO W15-VAT.
016770	     COMPUTE W25-SVAL = W15-TCOST + W15-RAIL +
                                W15-VAT + W15-DISCOUNT.
016780	     DISPLAY W25-S5V2 AT 2266
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     DISPLAY "Esc" AT 2562 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to exit" WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 1.
       BA35D.
012180	     PERFORM CHECK-CORRECT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA00
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA35D
	       END-EVALUATE.
016880	     DISPLAY CLEAR-L25.
016860     IF WS-OPTION = "N"
016870	       GO TO BA35B.

       BA35-EXIT.
	     EXIT.
      *
      *     ****    P R O C E S S   T H E   I N V O I C E
      *
       BA036	       SECTION 50.
       BA36-START.
016890	     MOVE ZERO		 TO WS-PURKEY.
016900	     DISPLAY "Processing Invoice" AT 2512 WITH
016910		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
	   IF WS-PAGE = ZERO
	       PERFORM BA00B.
	     INITIALIZE PRD-REC1.
	     MOVE ZERO		 TO PRD-SEQ.
      *
      *    ****    Start Recovery Logging.
      *
             MOVE "N"            TO WS-SKIP.
016920       PERFORM AY60 THRU AY999.
      *
      *    ****    Lock Creditor record and Log to Recovery file
      *
             MOVE 0              TO WS-ACTION.
016950       PERFORM AY07 THRU AY59.

016960 BA36.
016970     IF WS-PURKEY NOT < WS-PURLAST
016980         GO TO BA70.
016990       ADD 1               TO WS-PURKEY.
017000       PERFORM READ-PURCH THRU READ-PURCH-EXIT.
      *
      *    ****    If Order - Read Order record. Log 
      *            the order record to the Recovery file. 
      *
017020     IF PUR-ORD NOT = ZERO
017030         MOVE "Y"          TO WS-ORD
017040	       MOVE PUR-ORD	 TO ORD-KEY
017060         MOVE ZERO         TO WS-ACTION
017090         PERFORM AY10 THRU AY59
017100         MOVE PUR-ORD      TO W15-ORD
           ELSE
	       MOVE ZERO	 TO W15-ORD
017010	       MOVE "N" 	 TO WS-ORD.
      *
      *    ****    Lock Stock record and Log to Recovery file
      *
017110	     MOVE PUR-EXT-STOCK	 TO STK-CODE.
             MOVE ZERO           TO WS-ACTION.
017130       PERFORM AY05 THRU AY59.
      *
      *    ****    W A R E H O U S E   S T O C K
      *
	   IF NOT (W15-WHSE = SPACES)
	       MOVE W15-WHSE	 TO WST-WAR
	       MOVE PUR-EXT-STOCK
				 TO WST-EXT-CODE
      *	       MOVE SPACES	 TO WST-ITM
	       PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT
	       IF NOT (WS-STATUS = "00")
      *
      *    ****    C R E A T E	 W / H	 S T O C K   R E C O R D
      *
		   MOVE W15-WHSE  TO WST-WAR
		   MOVE PUR-EXT-STOCK
				  TO WST-EXT-CODE
		   MOVE SPACES	  TO WST-BIN
		   MOVE ZERO	  TO WST-MTD WST-MTDV WST-QUANT
				     WST-YTD WST-YTDV
		   PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT
		   MOVE 1	 TO WS-ACTION
	       END-IF
	       PERFORM AY052 THRU AY59.
	     MOVE W05-RATE	 TO W05-SRTE.
	     PERFORM CA500.
	     MOVE W05-SRTE	 TO W05-RATE.
	     MOVE PUR-PUN	 TO W15-PUN.
017140	     MOVE PUR-IUN	 TO W15-IUN.
017150       MOVE PUR-ICOST      TO W15-ICOST.
017150	     MOVE PUR-PCOST	 TO W15-PCOST.
017160       MOVE PUR-UWSALE     TO W15-UWSALE
017160       MOVE PUR-UCASH      TO W15-UCASH.
017160       MOVE PUR-USELL      TO W15-USELL.
017170       MOVE PUR-COST       TO W15-COST.
017180       MOVE PUR-SELL       TO W15-SELL.
	     MOVE PUR-INCVAL	 TO W15-INCOST.
	     MOVE PUR-IVAT	 TO W15-IVAT.
	     MOVE PUR-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE PUR-WSALE-2	 TO W10-WSALE-2.
	     MOVE PUR-CASH-2	 TO W10-CASH-2.
	     MOVE PUR-SELL-2	 TO W10-SELL-2.
	     MOVE PUR-WSALE-3	 TO W10-WSALE-3.
	     MOVE PUR-CASH-3	 TO W10-CASH-3.
	     MOVE PUR-SELL-3	 TO W10-SELL-3.
	     MOVE PUR-WSALE-4	 TO W10-WSALE-4.
	     MOVE PUR-CASH-4	 TO W10-CASH-4.
	     MOVE PUR-SELL-4	 TO W10-SELL-4.
	     MOVE PUR-CASES	 TO W10-CASES.
017190	     MOVE STK-CODE	 TO W10-EXT-ITEM.
017200	     MOVE STK-DESC	 TO W15-DESC.
	   IF PUR-LAB = "P"
	       MOVE W10-EXT-ITEM TO PLB-ITEM
               MOVE PUR-IUN      TO PLB-QUANT
               MOVE ZERO         TO PLB-FORM
	       MOVE W15-INVN	 TO PLB-INV
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
	       ADD 1		 TO WS-PRCKEY
	   ELSE
	   IF PUR-LAB = "B"
	       MOVE W10-EXT-ITEM TO BAR-ITEM BAR-CODE
	       MOVE PUR-IUN	 TO BAR-QNT
	       MOVE STK-DESC	 TO BAR-DESC
	       IF WS-BAR-CODE = "X"
		   MOVE STK-XREF TO BAR-CODE
	       END-IF
	       WRITE BAR-REC.
017210	   IF WS-ORD = "N"
017220         MOVE W15-COST     TO W20-RESULT
017230         GO TO BA37.
017240	   IF W15-IUN > ORD-QUANT
017250	       SUBTRACT ORD-QUANT FROM W15-IUN
                                 GIVING W15-XQNT
017260     ELSE
017270         MOVE ZERO         TO W15-XQNT.
017280       MOVE W05-DATE       TO W15-DATE.
017290     IF W15-MM NOT = W15-DEL-MM
017300         PERFORM BB000.
017310     IF W15-COST > ORD-COST
017320         MOVE ORD-COST     TO W20-RESULT
017330     ELSE
017340         MOVE W15-COST     TO W20-RESULT.
017350       MOVE ZERO           TO W20-RESULT.
017360	   IF W15-IUN NOT > ORD-QUANT
017370	       SUBTRACT W15-IUN  FROM ORD-QUANT
017380     ELSE
017390	       MOVE ZERO	 TO ORD-QUANT.
017400     IF W15-COST NOT > ORD-COST
017410         SUBTRACT W15-COST FROM ORD-COST
017420     ELSE
017430         SUBTRACT ORD-COST FROM W15-COST 
                                 GIVING W20-RESULT
017440         MOVE ZERO         TO ORD-COST.
      *
      *    ***** UPDATE THE STOCK RECORD *****
      *
017480 BA37.
             COMPUTE W15-UCOST ROUNDED = (W15-COST / W15-IUN).
017500       MOVE W15-UCOST      TO STK-COST.
	     MOVE W15-PCOST	 TO STK-PCOST.
             MOVE STK-AVGE       TO W15-AVRG.
017510	     MOVE W15-UWSALE	 TO STK-WSALE.
017510       MOVE W15-UCASH      TO STK-CASH.
017510       MOVE W15-USELL      TO STK-SELL.
      *
      *    ***** UPDATE THE PACK PRICES IF NECESSARY *****
      *
	   IF STK-USE-PACKS = "Y"
	       MOVE W10-WSALE-2	 TO STK-WSALE-2
	       MOVE W10-CASH-2	 TO STK-CASH-2
	       MOVE W10-SELL-2	 TO STK-SELL-2
	       MOVE W10-WSALE-3	 TO STK-WSALE-3
	       MOVE W10-CASH-3	 TO STK-CASH-3
	       MOVE W10-SELL-3	 TO STK-SELL-3
	       MOVE W10-WSALE-4	 TO STK-WSALE-4
	       MOVE W10-CASH-4	 TO STK-CASH-4
	       MOVE W10-SELL-4	 TO STK-SELL-4.
	     ADD W10-CASES	 TO STK-CASES.
	     MOVE P-NUMBER	 TO STK-SUPP.
      *
      *      ***** CALCULATE AVERAGE COST *****
      *
017570	   IF STK-IND > 1
017580         GO TO BA38.
017590	   IF (STK-QUANT + W15-WQUANT) < 0.01
017600         COMPUTE STK-AVGE = W15-COST / W15-IUN
017610     ELSE
017620	       COMPUTE W25-V1 = (STK-QUANT + W15-WQUANT) * STK-AVGE
017630         ADD W15-COST      TO W25-V1
017640	       COMPUTE STK-AVGE	ROUNDED = W25-V1 /
				(STK-QUANT + W15-WQUANT + W15-IUN).
017650     IF STK-AVGE < ZERO
017660         COMPUTE STK-AVGE = STK-AVGE * -1.
	   IF NOT (W15-WHSE = SPACES)
	       ADD W15-IUN	 TO WST-QUANT
	   ELSE
017670	       ADD W15-IUN	 TO STK-QUANT.
	   IF STK-ORD > ZERO
017680	       SUBTRACT W15-IUN	 FROM STK-ORD.

017690 BA38.
	     MOVE W05-DATE-YMD	 TO STK-PUR.
017700     IF STK-ORD < ZERO
017710         MOVE ZERO         TO STK-ORD.
017720     IF STK-ORD = ZERO
017730         MOVE ZERO         TO STK-DTE STK-DISC.
      *
      *      ***** UPDATE DEPARTMENT COST *****
      *
017740	     MOVE STK-LDG	 TO DPT-CODE.
             MOVE 0              TO WS-ACTION.
017760       PERFORM AY38 THRU AY59.
017770       ADD W15-COST        TO DPT-PUR-DAY
                                    DPT-PUR-MTD
017780                              DPT-PUR-YTD.
           IF WX-INT = "Y"
               MOVE W20-DTE      TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "PURCHASES - CREDITORS"
                                 TO WX-NAR
               MOVE W15-COST     TO WX-VALUE
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 06           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
017790	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
017800     IF WS-ORD = "Y"
017810         IF W05-MM = W15-DEL-MM
017820             GO TO BA65.
017830     IF W20-RESULT = ZERO
017840         GO TO BA65.

017850 BA40.
017860     IF ORD-TERMS = ZERO
017870         MOVE P-TERMS      TO ORD-TERMS.
017880       MOVE ORD-TERMS      TO W15-SETT.
017890       MOVE 0              TO WS-S.
017900       MOVE 1              TO WS-S2.

017910 BA45.
017920       MOVE ZERO           TO W25-CFLOW (WS-S2).
017930     IF W15-MONTH (WS-S2) > 0
017940	       ADD 1		 TO WS-S.
017950     IF WS-S2 < 6
017960         ADD 1             TO WS-S2
017970         GO TO BA45.
017980       MOVE ZERO           TO W25-V1 W25-V2.
017990       MOVE 1              TO WS-S3.
018020     IF WS-S = 0 
	       MOVE 1		 TO WS-S
	       GO TO BA54.
      *				    W15-30.
018020     IF WS-S = 1
018030         MOVE W20-RESULT   TO W25-V2
018040         GO TO BA50.
018050       DIVIDE W20-RESULT BY WS-S 
                                 GIVING W25-V1 ROUNDED.
018060       SUBTRACT 1          FROM WS-S.
018070       COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
018080       ADD 1               TO WS-S.

018090 BA50.
018100     IF W15-MONTH (WS-S3) = ZERO
018110         ADD 1             TO WS-S3
018120         GO TO BA50.
018130     IF WS-S > 1
018140         MOVE W25-V1       TO W25-CFLOW (WS-S3)
018150         SUBTRACT 1        FROM WS-S
018160         ADD 1             TO WS-S3
018170         GO TO BA50.
018180       MOVE W25-V2         TO W25-CFLOW (WS-S3).

       BA54.
018190       MOVE 1              TO WS-S2.

018200 BA55.
018210     IF W25-CFLOW (WS-S2) = ZERO
018220         GO TO BA60.
018230       ADD W05-MM WS-S2    GIVING WS-S3.
018240     IF WS-S3 > 12
018250         SUBTRACT 12       FROM WS-S3.

018260 BA60.
018270     IF WS-S2 < 6
018280         ADD 1             TO WS-S2
018290         GO TO BA55.

018300 BA65.
018310     IF WS-ORD = "Y"
	       IF ORD-QUANT = ZERO
                   MOVE 2        TO WS-ACTION
                   PERFORM AY10 THRU AY59
                   PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT
               ELSE
018320		   PERFORM REWRITE-PORDER THRU WRITE-PORDER-EXIT.
018330       MOVE W15-ORD        TO R-ORD.
018340	     MOVE W10-EXT-ITEM	 TO R-ITM.
018350	     MOVE W15-DESC	 TO R-DESC.
018360       MOVE W15-IUN        TO R-QNT.
018370       MOVE W15-UCOST      TO R-COST.
018380       MOVE W15-USELL      TO R-SELL.
018390       MOVE W15-COST       TO R-EXT.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
018440	       PERFORM BA00B.
018450       MOVE SPACES         TO R-DET.
	   IF NOT (W15-WHSE = SPACES)
	       PERFORM REWRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
018460	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	     MOVE P-NUMBER	 TO PRD-SUPP.
	     MOVE W10-EXT-ITEM	 TO PRD-EXT-CODE.
	     MOVE W05-DATE-YMD	 TO PRD-DTE.
	     ADD 1		 TO PRD-SEQ.
	     MOVE "I"		 TO PRD-TRAN.
	     MOVE W15-INVN	 TO PRD-REF.
000250	     MOVE W15-IUN	 TO PRD-QNT.
	     MOVE W15-COST	 TO PRD-COST.
	     MOVE W15-NAME	 TO PRD-NAME.
	     MOVE W15-WHSE	 TO PRD-WHSE.
	     PERFORM WRITE-PURDEX THRU WRITE-PURDEX-EXIT
000900	     MOVE 1		 TO WS-ACTION
	     PERFORM AY056 THRU AY59.
018470       GO TO BA36.

018480 BA70.
      *
      *    ****    U P D A T E   S T O C K   C O N T R O L
      *
018520       ADD W15-TCOST       TO STK-TOTAL.
018530       ADD W15-TCOST       TO STK-CMTD.
018540       ADD W15-TCOST       TO STK-CYTD.
      *
      *    ****    I N V O I C E   T O T A L
      *
018560       ADD W15-RAIL        TO W15-TCOST.
018570       ADD W15-DISCOUNT    TO W15-TCOST.
018560       ADD W15-VAT         TO W15-TCOST.

018580 BA75.
018600       MOVE 6              TO WS-PARKEY.
018620       PERFORM AY01 THRU AY59.

018710 BA80.
019210	     ADD W15-RAIL	 TO PAR-RAILD.
019220       ADD W15-RAIL        TO PAR-RAILM.
019230       ADD W15-DISCOUNT    TO PAR-DISCD.
019240       ADD W15-DISCOUNT    TO PAR-DISCM.
019250	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

018800 BA85.
	     INITIALIZE CT-REC.
018830	     MOVE W05-DATE-YMD	 TO CT-DTE.
018850       MOVE W15-INVN       TO CT-SINV CT-REF.
	     MOVE W20-DTE	 TO CT-SDTE.
	     MOVE "U"		 TO CT-TYPE.
	     MOVE "I"		 TO CT-PJ.
	   IF W15-OPREF = ZERO
	       MOVE SPACES	 TO CT-ORD
	   ELSE
018870	       MOVE W15-OPREF	 TO CT-ORD.
018880       MOVE W15-TCOST      TO CT-COST.
018890       MOVE W15-ACNO       TO CT-CRED.
018900       MOVE 01             TO CT-CODE.
018910	     MOVE ZERO		 TO CT-AGE.
018920       MOVE W15-TSELL      TO CT-SELL.
             MOVE W15-VAT        TO CT-TAX.
	     PERFORM BG600.
	     PERFORM BG700.
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
               PERFORM GL-TRF-UPDATE
               IF W15-RAIL NOT = ZERO
                   MOVE W15-RAIL TO WX-VALUE
                   MOVE WX-RLGL  TO WX-AC
                   MOVE 21       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
               END-IF
               IF W15-DISCOUNT NOT = ZERO
                   MOVE W15-DISCOUNT
                                 TO WX-VALUE
                   MOVE WX-DSGL  TO WX-AC
                   MOVE 23       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
018930       MOVE 1              TO WS-S WS-S2.
018940       MOVE W15-TCOST      TO W20-RESULT.
018950	     PERFORM BA40 THRU BA54.
018960	     MOVE 1		 TO WS-S2.

018970 BA86.
018980       MOVE W25-CFLOW (WS-S2) 
                                 TO W65-CFLOW (WS-S2).
018990	   IF WS-S2 < 6
019000         ADD 1             TO WS-S2
019010         GO TO BA86.
019020       MOVE 1              TO WS-S2.

019030 BA90.
019040     IF W65-CFLOW (WS-S) NOT = ZERO
019050         ADD WS-S2         TO CT-AGE.
019060     IF WS-S < 6
019070         ADD 1             TO WS-S
019080         MULTIPLY 2        BY WS-S2
019090         GO TO BA90.
019100       ADD W15-TCOST       TO P-BAL.
019110       ADD W15-TCOST       TO P-CR.
019120       ADD W65-CFLOW (6)   TO P-180D.
019130       ADD W65-CFLOW (5)   TO P-150D.
019140       ADD W65-CFLOW (4)   TO P-120D.
019150       ADD W65-CFLOW (3)   TO P-90D.
019160       ADD W65-CFLOW (2)   TO P-60D.
019170       ADD W65-CFLOW (1)   TO P-30D.
	  IF CT-AGE = ZERO
	      ADD W15-TCOST	 TO P-OUT W70-DUE-NOW.
019180	     PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY08 THRU AY59.
019200	     PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
019290       ADD W15-TCOST       TO W70-CLOSE.
019300       ADD W15-TCOST       TO W70-CRED.
019120       ADD W65-CFLOW (6)   TO W70-DUE-180.
019130       ADD W65-CFLOW (5)   TO W70-DUE-150.
019140       ADD W65-CFLOW (4)   TO W70-DUE-120.
019150       ADD W65-CFLOW (3)   TO W70-DUE-90.
019160       ADD W65-CFLOW (2)   TO W70-DUE-60.
019170       ADD W65-CFLOW (1)   TO W70-DUE-30.
      *       ADD W65-V2	  TO W70-DUE-NOW.
      *
      *    ****    VAT Control 
      *
	     MOVE 1		 TO W05-V-RATE.

       BA92.
	     ADD 6 W05-V-RATE	 GIVING WS-VAT-SUB.
	   IF (W15-VATVAL(W05-V-RATE) = ZERO) AND
	      (W15-VATVAL(WS-VAT-SUB) = ZERO)
	       GO TO BA93.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     MOVE 0		 TO WS-ACTION.
             PERFORM AY38 THRU AY59.
	     ADD W15-VATVAL(W05-V-RATE)
				 TO DPT-I-VAT-DAY
				    DPT-I-VAT-MTD
				    DPT-I-VAT-YTD.
	     ADD W15-VATVAL(WS-VAT-SUB)
				 TO DPT-I-VAT-DAY
				    DPT-I-VAT-MTD
				    DPT-I-VAT-YTD.
	     ADD W15-TAXVAL(W05-V-RATE)
				 TO DPT-V-PUR-DAY
				    DPT-V-PUR-MTD
				    DPT-V-PUR-YTD.
	     ADD W15-TAXVAL(WS-VAT-SUB)
				 TO DPT-V-PUR-DAY
				    DPT-V-PUR-MTD
				    DPT-V-PUR-YTD.
027880       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       BA93.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO BA92.
	   IF (WX-INT = "Y") AND (W15-VAT NOT = ZERO)
                MOVE W15-VAT     TO WX-VALUE 
		MOVE DPT-I-VAT-GL
				 TO WX-AC
                MOVE 13          TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
      *
      *    ****    End Recovery Logging.
      *
       BA95.
	   IF WS-PRCKEY > 2
               MOVE WS-PRCKEY    TO WS-KEYSTR
               MOVE 1            TO WS-PRCKEY
               INITIALIZE PLB-REC1
               MOVE WS-KEYSTR    TO PLB-QUANT
               PERFORM REWRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               MOVE WS-KEYSTR    TO WS-PRCKEY.
	     CLOSE CREDIT.
	     OPEN I-O CREDIT.
019380	     PERFORM AY70 THRU AY999.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
019430	       PERFORM BA00B.
019440       MOVE SPACES         TO R-DET.
      *
      *      ***** CREDITOR INVOICE TOTAL *****
      *
019450       MOVE WS-HCRED       TO G-H1.
019460	     MOVE W15-ACNO	 TO G-SUPP.
019470       MOVE W15-NAME       TO G-NAME.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
019490	     MOVE SPACES	 TO R-DET.
019500	     MOVE "INV:"	 TO G-H3.
019510	     MOVE "DTE:"	 TO G-H4.
019520       MOVE "RAIL:"        TO G-H5.
019530	     MOVE "DSC:"	 TO G-H6.
             MOVE "VAT:"         TO G-H6A.
019540       MOVE "TOTAL:"       TO G-H7.
019550       MOVE W15-INVN       TO G-INVN.
019560	     MOVE W25-INVD	 TO G-INVD.
019570       MOVE W15-RAIL       TO G-RAIL.
019580       MOVE W15-DISCOUNT   TO G-DISC.
             MOVE W15-VAT        TO G-VAT.
019590       MOVE W15-TCOST      TO G-TOTAL.
019600       ADD W15-TCOST       TO W15-TOTAL.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
019650	       PERFORM BA00B.
019660       MOVE SPACES         TO R-DET.
019670	     CLOSE PURCH.
019680       OPEN OUTPUT PURCH.
019690       CLOSE PURCH.
019700	     OPEN I-O PURCH.

       BA99.
	     EXIT.
      *
      *    ****    I N S E R T	 M A R K U P   P E R C E N T A G E S
      *
       BA100	     SECTION 50.
       BA101.
017740	     MOVE STK-LDG	 TO DPT-CODE.
017760	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE 1		 TO WS-S9.

       BA105.
	     MOVE DPT-MKUP(WS-S9)
				 TO STK-MKUP(WS-S9).
	     MOVE DPT-CMKUP(WS-S9)
				 TO STK-CMKUP(WS-S9).
	     MOVE DPT-WMKUP(WS-S9)
				 TO STK-WMKUP(WS-S9).
	   IF WS-S9 < 4
	       ADD 1		 TO WS-S9
	       GO TO BA105.
018460	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

019730 BA199.
019740       EXIT.

019730 BA999.
019740       EXIT.
      /
       BA00B	     SECTION 51.
014070 BA0B.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
008540	     MOVE SPACES	 TO R-DET.
008550       MOVE WS-H1          TO R-H1.
008560       MOVE W60-COMPANY    TO R-H2.
008570       MOVE WS-H3          TO R-H3.
008580	     ADD 1		 TO WS-PAGE.
008590       MOVE W05-DATE       TO R-DTE1.
008600       MOVE WS-PAGE        TO R-PGE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
008620	     MOVE SPACES	 TO R-DET.
008630       MOVE W60-DESC       TO R-H2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
008650	     MOVE SPACES	 TO R-DET.
008670       MOVE W50-H4         TO R-HEAD.
	     PERFORM CALL-PRINTUTL.
008690	     MOVE SPACES	 TO R-DET.
014090	     MOVE "ORD.NO."	 TO I-H1.
014100	     MOVE "ITEM CODE"	 TO I-H2.
014110       MOVE WS-HDESC       TO I-H3.
014120       MOVE WS-HQNT        TO I-H4.
014130       MOVE "UN-COST"      TO I-H5.
014140       MOVE "UN-SELL"      TO I-H6.
014150       MOVE "EXTENDED"     TO I-H7.
	     PERFORM CALL-PRINTUTL.
014170	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BA999B.
	     EXIT.
      /
      *    R E - A L L O C A T E   V A T   T O	 V A R I O U S	 R A T E S
      *
       BA00C	     SECTION 51.
014070 BA0C.
	     MOVE 12		 TO WS-S9.
	     MOVE W15-VAT	 TO W55-VAT.

       BA05C.
	   IF NOT (W15-VATVAL(WS-S9) = ZERO)
	       IF W15-VATVAL(WS-S9) > W55-VAT
		    MOVE W55-VAT TO W15-VATVAL(WS-S9)
	       END-IF
	       SUBTRACT W15-VATVAL(WS-S9)
				 FROM W55-VAT
	       MOVE WS-S9	 TO WS-S4.
	   IF W55-VAT > ZERO
	       IF WS-S9 > 1
		   SUBTRACT 1	 FROM WS-S9
		   GO TO BA05C
	       ELSE
		   ADD W55-VAT	 TO W15-VATVAL(WS-S4)
		   SUBTRACT W15-VATVAL(WS-S9)
				 FROM W55-VAT.

       BA10C.
	   IF WS-S9 > 1
	       SUBTRACT 1	 FROM WS-S9
	       MOVE ZERO	 TO W15-VATVAL(WS-S9)
	       GO TO BA10C.

       BA999C.
	     EXIT.
      /
019750 BB000	     SECTION 52.
019760 BB00.
019770       MOVE W15-COST       TO W20-RESULT.
019780       PERFORM BA40 THRU BA60.
019790     IF W15-COST NOT > ORD-COST
019800         GO TO BB05.
019810       MOVE ORD-COST       TO W20-RESULT.
019820	     PERFORM BA40 THRU BA54.

019830 BB05.
019840	     MOVE ORD-DELIV	 TO W05-MM.
019850       MOVE 1              TO WS-S2.

019860 BB10.
019870       MULTIPLY -1         BY W25-CFLOW (WS-S2).
019880     IF WS-S2 < 6
019890         ADD 1             TO WS-S2
019900         GO TO BB10.
019910       MOVE 1              TO WS-S2.
019920       PERFORM BA55 THRU BA60.
019930       MOVE W05-MONTH      TO W05-MM.

019940 BB099.
019950       EXIT.

      /
012720 BC000	     SECTION 53.
012730 BC00.
012740       MOVE ZERO           TO WS-ERROR.
012750     IF W15-DD < 01 OR > 31
012760	       GO TO BC15.
012770     IF W15-MM < 01 OR > 12
012780	       GO TO BC15.
012790     IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
012800	       GO TO BC999.
012810     IF W15-MM = 02 AND W15-DD > 28
012820	       GO TO BC05.
012830     IF W15-DD > 30
012840	       GO TO BC15.
012850	     GO TO BC999.

012860 BC05.
012870     IF W15-DD > 29
012880	       GO TO BC15.
012890	     DIVIDE W15-CCYY BY 4
				 GIVING W20-RESULT.
012900     IF W20-DECIMAL > ZERO
012910	       GO TO BC15.
012920	     GO TO BC999.

012930 BC15.
012940       MOVE 1              TO WS-ERROR.

012950 BC999.
012960       EXIT.

      *
      *    ****    C A L L   S T O C K   C R E A T I O N
      *
       BD100	       SECTION 54.
       BD102.
	     PERFORM SAVE-SCREEN.
	     CLOSE RECOVER.
004200	     CALL "STP\STP010"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "STP010".
	     OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
012150       MOVE ZERO           TO WS-RECOVER.
	     PERFORM RESTORE-SCREEN.

       BD199.
             EXIT.
      *
      *    ****    V I E W   T R A N S A C T I O N   D E T A I L S
      *
       BE000	       SECTION 55.
       BE00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO WS-VIEWKEY
				    WS-V-FIRST
				    WS-V-LAST
				    W75-S1
				    W75-S.

       BE02.
	     MOVE ZERO		 TO W75-PKEY(W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO BE02.
	     MOVE ZERO		 TO W75-S1.

       BE05.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄ
      -		     "ÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´" AT 0601
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³    Item Code     ³       Description        ³ Qu
      -		     "antity ³Unit Price³ Extended ³" AT 0701
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄ
      -		     "ÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´" AT 0801
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     MOVE 0901		 TO CPOS.

       BE10.
	     DISPLAY "³                  ³                          ³
      -		     "       ³          ³          ³" AT CPOS
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BE10.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄ
      -		     "ÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ" AT 2401
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
000340	     DISPLAY CLEAR-L25.

       BE15.
	     MOVE WS-VIEWKEY	 TO WS-PURKEY.
017000	     PERFORM READ-PURCH THRU READ-PURCH-EXIT.
	     MOVE 0901		 TO CPOS.

       BE20.
	     MOVE 02		 TO CCOL.
	     MOVE PUR-EXT-STOCK  TO W10-EXT-ITEM.
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT CPOS
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT CPOS
	       ELSE
021070		   DISPLAY S07 AT CPOS.
	     MOVE PUR-EXT-STOCK	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	     MOVE 21		 TO CCOL.
	     DISPLAY STK-DSC26 AT CPOS
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     MOVE 48		 TO CCOL.
	     MOVE PUR-IUN	 TO W100-EXT.
	     DISPLAY W100-V4 AT CPOS
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     MOVE 59		 TO CCOL.
	     MOVE PUR-ICOST	 TO W100-EXT.
	     DISPLAY W100-V4 AT CPOS
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     MOVE 70		 TO CCOL.
	     MOVE PUR-COST	 TO W100-EXT.
	     DISPLAY W100-V4 AT CPOS
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	     MOVE WS-PURKEY	 TO W75-PKEY(W75-S)
				    WS-VIEWKEY.
	   IF CLIN > 22
	       GO TO BE25.
      *
      *    ****    T E S T   F O R   E N D   O F   I N P U T   I T E M S
      *
       BE22.
016970	   IF WS-PURKEY NOT < WS-PURLAST
016980	       GO TO BE25.
016990	     ADD 1		 TO WS-PURKEY
				    W75-S
				    CLIN.
017000	     PERFORM READ-PURCH THRU READ-PURCH-EXIT.
	     GO TO BE20.
      *
      *    ****    D I S P L A Y   I N S T R U C T I O N S   O N
      * 	   L I N E   2 5   W H E N   P A G E   F U L L
      *
       BE25.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     " highlight - " WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
001040		     "ENTER" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     " select - " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 14
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
       BE30.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0901		 TO CPOS.
      *
      *    ****    S C R E E N	 A T T R I B U T E S
      *
       BE35.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
       BE40.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BE45
		 WHEN DOWN-KEY
001340		     GO TO BE50
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO BE60
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE40
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE995
		 WHEN PAGE-DOWN
001340		     GO TO BE65
		 WHEN PAGE-UP
001340		     GO TO BE75
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BE40.
	     CALL X"E5".
001280	     GO TO BE40.
      *
      *    ****    U P	 K E Y
      *
       BE45.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO BE55.
      *
      *    ****    D O W N   K E Y
      *
       BE50.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO BE55.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
001980 BE55.
001990	     PERFORM BE35.
002000	     DISPLAY W43-DET (CLIN) AT CPOS
		     WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 0.
002010	     GO TO BE40.
      *
      *    ****    G E T   S E L E C T E D   R E C O R D
      *
       BE60.
	     PERFORM RESTORE-SCREEN-3.
	   IF W75-S1 = ZERO
	       GO TO BE40.
	     MOVE W75-PKEY(W75-S1)
				 TO WS-PURKEY.
017000	     PERFORM READ-PURCH THRU READ-PURCH-EXIT.
	     GO TO BE80.
      *
      *    ****    P A G E   D O W N
      *
       BE65.
	   IF W75-S < 15
	       GO TO BE35.
      *
      *    ****    N E W   P A G E
      *
       BE70.
	     MOVE 0901		 TO CPOS.
	     PERFORM BE10.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM BE02.
	     MOVE WS-VIEWKEY	 TO WS-PURKEY.
027830	     GO TO BE22.
      *
      *    ****    P A G E   U P
      *
       BE75.
	   IF WS-VIEWKEY > 29
	       SUBTRACT 30	 FROM WS-VIEWKEY
	   ELSE
	       MOVE ZERO	 TO WS-VIEWKEY.
	     GO TO BE70.

       BE80.
	     MOVE PUR-EXT-STOCK	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       BE85A.
016220	     MOVE PUR-EXT-STOCK	 TO W10-EXT-ITEM.
	     MOVE PUR-PUN	 TO W15-PUN.
016230	     MOVE PUR-IUN	 TO W15-IUN.
016240	     MOVE PUR-PCOST	 TO W15-PCOST.
016240	     MOVE PUR-ICOST	 TO W15-ICOST.
016250	     MOVE PUR-UWSALE	 TO W15-UWSALE.
016250	     MOVE PUR-UCASH	 TO W15-UCASH.
016250	     MOVE PUR-USELL	 TO W15-USELL.
016260	     MOVE PUR-COST	 TO W15-COST.
	     MOVE PUR-INCVAL	 TO W15-INCOST.
016270	     MOVE PUR-SELL	 TO W15-SELL.
	     MOVE PUR-IVAT	 TO W15-IVAT.
	     MOVE PUR-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE PUR-WSALE-2	 TO W10-WSALE-2.
	     MOVE PUR-CASH-2	 TO W10-CASH-2.
	     MOVE PUR-SELL-2	 TO W10-SELL-2.
	     MOVE PUR-WSALE-3	 TO W10-WSALE-3.
	     MOVE PUR-CASH-3	 TO W10-CASH-3.
	     MOVE PUR-SELL-3	 TO W10-SELL-3.
	     MOVE PUR-WSALE-4	 TO W10-WSALE-4.
	     MOVE PUR-CASH-4	 TO W10-CASH-4.
	     MOVE PUR-SELL-4	 TO W10-SELL-4.
	     MOVE PUR-CASES	 TO W10-CASES.
	   IF NOT (WS-VAT-SUB = ZERO)
	       MOVE W05-VAT(WS-VAT-SUB)
				 TO W05-RATE
	   ELSE
	       MOVE ZERO	 TO W05-RATE.
      *
      *    ****    R E S T O R E   O R I G I N A L   S C R E E N
      *
       BE85B.
	     PERFORM RESTORE-SCREEN-3.
      *
      *    ****    R E D U C E	 R U N N I N G	 T O T A L
      *
	   IF P-TAX NOT = "N"
               IF STK-TAX > ZERO
		   SUBTRACT W15-COST
				 FROM W15-TXBL.
	     SUBTRACT W15-COST	 FROM W15-TCOST.
	     SUBTRACT W15-INCOST FROM W15-INCVAL.
016120	     SUBTRACT W15-IUN	 FROM W15-QUANT.
016140	     SUBTRACT W15-SELL	 FROM W15-TSELL.
	     SUBTRACT W15-IVAT	 FROM W15-VATVAL(WS-VAT-SUB).
	     SUBTRACT W15-COST	 FROM W15-TAXVAL(WS-VAT-SUB).
      *
      *    ****    D I S P L A Y   S E L E C T E D   I T E M
      * 	   D E T A I L S   A S	 C A P T U R E D
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
021070		   DISPLAY S07 AT 0717.
	     DISPLAY STK-DESC AT 0736
		     WITH FOREGROUND-COLOR 1
			  BACKGROUND-COLOR 7.
       BE85C.
      *	   IF WS-USE-3 = "Y"
      *	       DISPLAY S37-3 AT 0817
      *	   ELSE
015620	       DISPLAY S37 AT 0817.
	   IF STK-PRC = ZERO
	       MOVE 1		 TO STK-PRC.
	     MULTIPLY W15-PUN BY STK-PRC
				 GIVING W15-IUN.
015620	     DISPLAY S37S AT 0844.
015660	     DISPLAY S38 AT 0917.
	     MOVE W15-ICOST	 TO W100-S8V3.
	     DISPLAY W100-S8V3 AT 0944 WITH FOREGROUND-COLOR 14.
      *
      *    ****    C A L C U L A T E   T A X A B L E / N O N -
      * 	   T A X A B L E   A M O U N T
      *
       BE86.
	     MOVE ZERO		 TO W15-U-TXBL
				    W15-U-NTXBL
				    W15-U-VAT.
           IF STK-TAX NOT = ZERO
      *
      *    ****    N O T   A   T A X   I N V O I C E
      *
	       IF P-TAX = "N"
		   MOVE W15-PCOST
				 TO W15-U-NTXBL
                                    W15-U-INCVAL
				    W15-U-PNETT
               ELSE
      *
      *    ****    T A X   I N V O I C E   ( I N C L U S I V E )
      *
	       IF P-TAX = "I"
		   MOVE W15-PCOST
				 TO W15-U-INCVAL
                   COMPUTE W15-U-TXBL ROUNDED = W15-U-INCVAL -
                       ((W15-U-INCVAL / (100.00000 + W05-RATE)) 
                       * W05-RATE)
                   MOVE W15-U-TXBL
				 TO W15-U-PNETT
                   SUBTRACT W15-U-TXBL FROM W15-U-INCVAL
                                 GIVING W15-U-VAT
               ELSE
      *
      *    ****    T A X   I N V O I C E   ( E X C L U S I V E )
      *
		   MOVE W15-PCOST
				 TO W15-U-TXBL
				    W15-U-PNETT
                   COMPUTE W15-U-VAT ROUNDED = W15-U-TXBL * W05-RTE
                   ADD W15-U-TXBL W15-U-VAT
                                 GIVING W15-U-INCVAL
	       END-IF
           ELSE
      *
      *    ****    N O N   T A X A B L E   I T E M
      *
	       MOVE W15-PCOST	 TO W15-U-NTXBL
				    W15-U-INCVAL
				    W15-U-PNETT.
	     COMPUTE W15-U-NETT ROUNDED =
		    (W15-PCOST * W15-PUN) / W15-IUN.
           IF W15-U-TXBL NOT = ZERO
	       MULTIPLY W15-PUN BY W15-U-TXBL
				 GIVING W15-COST ROUNDED
           ELSE
015690	       MULTIPLY W15-PUN BY W15-U-NTXBL
				 GIVING W15-COST ROUNDED.
      *
      *    ****    C A L C U L A T E   V A T   /   I N C L U S I V E
      *
	   IF (P-TAX = "E") AND (STK-TAX NOT = ZERO)
               COMPUTE W15-INCOST ROUNDED = (W15-COST +
                   (W15-COST * W05-RTE))
           ELSE
015690	       MULTIPLY W15-PUN BY W15-U-INCVAL
				 GIVING W15-INCOST ROUNDED.
             SUBTRACT W15-COST FROM W15-INCOST
				 GIVING W15-IVAT ROUNDED.
015690	     MULTIPLY W15-PUN BY W15-PCOST
				 GIVING W15-ILCOST ROUNDED.
      *
      *      ***** UNIT NETT COST *****
      *
             DISPLAY S47 AT 1217.
      *
      *      ***** VAT PER UNIT *****
      *
             DISPLAY S48 AT 1244.
      *
      *      ***** SELLING PRICES *****
      *
	     MOVE W15-UWSALE	 TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1617 WITH FOREGROUND-COLOR 14.
	     MOVE W15-UCASH	 TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1633 WITH FOREGROUND-COLOR 14.
	     MOVE W15-USELL	 TO W100-S8V2.
             DISPLAY W100-S8V2 AT 1649 WITH FOREGROUND-COLOR 14.
      *
      *      ***** TOTAL VAT *****
      *
015710       DISPLAY S45 AT 1717.
      *
      *      ***** COST EXCLUDING VAT *****
      *
015710       DISPLAY S40 AT 1817.
      *
      *      ***** COST INCLUDING VAT *****
      *
015710       DISPLAY S46 AT 1917.
       BE88.
      *
      *      ***** EXCLUSIVE (RUNNING TOTAL) *****
      *
016300	     MOVE W15-TCOST	 TO W25-SVAL.
016320	     DISPLAY W25-S5V2 AT 1866
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 0.
	     MOVE 1		 TO WS-S9.
	     MOVE ZERO		 TO W15-VAT.
      *
      *    ***** Accumulate the tax *****
      *
       BE88-TAX.
	     ADD W15-VATVAL(WS-S9)
				 TO W15-VAT.
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BE88-TAX.
      *
      *      ***** V A T   (RUNNING TOTAL)
      *
	     DISPLAY S43 AT 2166.
      *
      *      ***** INCLUSIVE (RUNNING TOTAL) *****
      *
016770	     COMPUTE W25-SVAL = W15-TCOST + W15-VAT.
016780	     DISPLAY W25-S5V2 AT 2266
		     WITH BACKGROUND-COLOR 7
			  FOREGROUND-COLOR 0.
      *
      *    ****    A L L O W   A M E N D M E N T S   T O
      * 	   S E L E C T E D   L I N E
      *
       BE89.
	     MOVE "'A'mend/'D'elete or 'E'xit"
				 TO WS-ERR-MES
	     PERFORM OPT-MESSAGE.
030590	   IF NOT (WS-OPTION = "A" OR "D" OR "E")
030600	       GO TO BE89.
030610     IF WS-OPTION = "E"
	       ADD W15-IVAT	 TO W15-VATVAL(WS-VAT-SUB)
	       ADD W15-COST	 TO W15-TAXVAL(WS-VAT-SUB)
030620	       GO TO BE990.
	   IF WS-OPTION = "D"
	       GO TO BE105.
      *
      *    ****    A L L O W   Q U A N T I T Y	 A M E N D M E N T
      *
015610 BE90.
      *	   IF WS-USE-3 = "Y"
      *	       ACCEPT S37-3 AT 0817
      *	   ELSE
015620	       ACCEPT S37 AT 0817.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM BE85A
		     GO TO BE990
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE90
	       END-EVALUATE.
015630*	   IF W15-IUN = ZERO
015640*	       GO TO BE90.
      *
      *    ****    C O N V E R T I O N	 F R O M   P U R C H A S E
      * 	   U N I T   T O   S E L L I N G   U N I T
      *
015630	   IF W15-PUN = ZERO
015640	       GO TO BE90.
	   IF STK-PRC = ZERO
	       MOVE 1		 TO STK-PRC.

       BE90B.
	     MULTIPLY W15-PUN BY STK-PRC
				 GIVING W15-IUN.
015620	     ACCEPT S37S AT 0844.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE90
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE90B
	       END-EVALUATE.
015630	   IF W15-IUN = ZERO
015640	       GO TO BE90B.
      *
      *    ****   A R E   C A S E S   C O N T R O L L E D
      *
	   IF WS-USE-CASES= "Y"
	       IF STK-USE-CASES = "Y"
      * 			     GET THE NUMBER OF CASES RECEIVED
		   PERFORM BF000.
      *
      *    ****   A L L O W   P U R C H A S E - U N I T
      * 	  C O S T   A M E N D M E N T
      *
015650 BE95.
015660	     ACCEPT S38 AT 0917.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM BE85A
		     GO TO BE990
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE95
	       END-EVALUATE.
015670	   IF W15-PCOST = ZERO
015680	       GO TO BE95.
	     COMPUTE W15-ICOST ROUNDED =
		    (W15-PCOST * W15-PUN) / W15-IUN.
	     PERFORM BE86.
	     PERFORM BA29 THRU BA33B-1.
015990	     MULTIPLY W15-USELL BY W15-IUN GIVING W15-SELL.
012180	     PERFORM CHECK-CORRECT.
016090	   IF WS-OPTION = "N"
	       PERFORM BE85A
016100	       GO TO BE85C.
      *	   IF (P-TAX = "I") AND (STK-TAX NOT = ZERO)
      * 	ADD W15-IVAT	  TO W15-VAT.
	   IF NOT (WS-VAT-SUB = ZERO)
	       ADD W15-COST	 TO W15-TAXVAL(WS-VAT-SUB)
	       ADD W15-IVAT	 TO W15-VATVAL(WS-VAT-SUB).

       BE100.
	     MOVE "'P'rice,'B'arcode,'N'o labels  [ ]"
				 TO WS-ERR-MES.
             MOVE SPACE          TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "B" OR "N" OR "P")
	       GO TO BE100.
016230       MOVE W15-IUN        TO PUR-IUN.
016240       MOVE W15-ICOST      TO PUR-ICOST.
016250       MOVE W15-UWSALE     TO PUR-UWSALE.
016250       MOVE W15-UCASH      TO PUR-UCASH.
016250       MOVE W15-USELL      TO PUR-USELL.
016260       MOVE W15-COST       TO PUR-COST.
             MOVE W15-INCOST     TO PUR-INCVAL.
016270       MOVE W15-SELL       TO PUR-SELL.
             MOVE WS-OPTION      TO PUR-LAB.
016280	     PERFORM REWRITE-PURCH THRU WRITE-PURCH-EXIT.
	     GO TO BE990.
      *
      *    ****    D E L E T E	 A   R E C O R D
      *
       BE105.
	     MOVE WS-PURKEY	 TO WS-V-FIRST.
	     ADD 1 WS-V-FIRST	 GIVING WS-V-LAST.
      *
      *    ****    S H I F T   R E C O R D S   U P
      *
       BE110.
016970	   IF NOT (WS-V-LAST > WS-PURLAST)
	       MOVE WS-V-LAST	 TO WS-PURKEY
017000	       PERFORM READ-PURCH THRU READ-PURCH-EXIT
	       MOVE WS-V-FIRST	 TO WS-PURKEY
016280	       PERFORM REWRITE-PURCH THRU WRITE-PURCH-EXIT
	       ADD 1		 TO WS-V-FIRST
				    WS-V-LAST
	       GO TO BE110.
	     MOVE WS-V-FIRST	 TO WS-PURKEY.
	     PERFORM READ-PURCH THRU READ-PURCH-EXIT.
000170	     DELETE PURCH.
	     SUBTRACT 1 	 FROM WS-PURLAST.
	     GO TO BE995.

       BE990.
	   IF P-TAX NOT = "N"
               IF STK-TAX > ZERO
                   ADD W15-COST  TO W15-TXBL.
	     ADD W15-INCOST	 TO W15-INCVAL.
016120       ADD W15-IUN         TO W15-QUANT.
016130       ADD W15-COST        TO W15-TCOST.
016140       ADD W15-SELL        TO W15-TSELL.

       BE995.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE CSTART	 TO WS1-CURSOR.
	     MOVE ZERO		 TO COL-AND-LINE.
	     MOVE WS-PURLAST	 TO WS-PURKEY.

       BE999.
	     EXIT.
      *
      *    ****    C A S E S
      *
       BF000	       SECTION 56.
       BF00.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY S08.
	     PERFORM SCREEN-SHADOW.

       BF10.
	     ACCEPT S09.
	   IF W10-CASES = ZERO
	       MOVE 15		 TO SLIN
	       MOVE 64		 TO SCOL
	       MOVE "Cases may not be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-SETUP THRU ERROR-CENTRE
	       PERFORM SAVE-SCREEN
	       PERFORM ERROR-DISPLAY THRU ERROR-EXIT
	       GO TO BF10.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
	       GO TO BF00.

       BF999.
	     EXIT.
      *
      *    ****    P A C K S
      *
       BG000	       SECTION 57.
       BG00.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE 2		 TO WS-S9.

       BG05.
	     MOVE STK-UNT(WS-S9) TO W10-UNT(WS-S9).
	     MOVE STK-PR(WS-S9)  TO W10-PR(WS-S9).
	     MOVE STK-SL(WS-S9)  TO W10-SL(WS-S9).
	     MOVE STK-CSH(WS-S9) TO W10-CSH(WS-S9).
	     MOVE STK-WSL(WS-S9) TO W10-WSL(WS-S9).
	   IF STK-TAX NOT = ZERO
	       COMPUTE W10-VSL(WS-S9) ROUNDED = W10-SL(WS-S9) +
				     (W10-SL(WS-S9) * W05-RTE)
	       COMPUTE W10-VCSH(WS-S9) ROUNDED = W10-CSH(WS-S9) +
				      (W10-CSH(WS-S9) * W05-RTE)
	       COMPUTE W10-VWSL(WS-S9) ROUNDED = W10-WSL(WS-S9) +
				      (W10-WSL(WS-S9) * W05-RTE).
	     MOVE STK-MKUP(WS-S9)
				 TO W10-MKUP(WS-S9).
	     MOVE STK-CMKUP(WS-S9)
				 TO W10-CMKUP(WS-S9).
	     MOVE STK-WMKUP(WS-S9)
				 TO W10-WMKUP(WS-S9).
	   IF WS-S9 < 4
	       ADD 1		 TO WS-S9
	       GO TO BG05.
	     PERFORM SAVE-SCREEN.
	     CALL "CRP\CRPPACKS" USING WS-PARID
				       LS-USER-ID
				       W10-STOCK
		   ON EXCEPTION
		   MOVE "Program- CRPPACKS not on disk, press ANY key"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   MOVE 9	 TO WS-ERROR
		   PERFORM RESTORE-SCREEN
		   GO TO BG999.
	      CANCEL "CRP\CRPPACKS".
	      PERFORM RESTORE-SCREEN.

       BG999.
	      EXIT.
      /
       PORDER-LOOKUP	       SECTION 58.
012520 PORDER-LUP-SCREEN.
011050	     PERFORM SAVE-SCREEN.
	     DISPLAY S-POR.

       PORDER-LUP-CLR-SCREEN.
	     MOVE 0909		 TO CPOS.
	     PERFORM PORDER-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 11		 TO SHADE-COL.
	     MOVE 66		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 9		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

       PORDER-LUP-TAB-LOOP.
	     MOVE ZERO		 TO W75-POR (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO PORDER-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       PORDER-LUP-START.
	     MOVE P-NUMBER	 TO ORD-CRED.
	     MOVE SPACES	 TO ORD-EXT-ITEM.
	     PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO PORDER-NO-RECORD.

       PORDER-GET-ORD-RECORD.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO PORDER-SKIP-ORD-READ.
027520	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
               SUBTRACT 1        FROM W75-S
	       GO TO PORDER-FULL-SCREEN.
	   IF NOT (ORD-CRED = P-NUMBER)
	       SUBTRACT 1	 FROM W75-S
	       GO TO PORDER-FULL-SCREEN.

       PORDER-SKIP-ORD-READ.
	     MOVE ORD-KEY	 TO W75-CODE(W75-S).
	     MOVE ORD-EXT-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       SUBTRACT 1	 FROM W75-S
	       GO TO PORDER-GET-ORD-RECORD.
	     DISPLAY SD-POR AT LINE CLIN.
027700	   IF CLIN < 20
027710	       ADD 1		 TO CLIN W75-S
027720	       GO TO PORDER-GET-ORD-RECORD.

       PORDER-FULL-SCREEN.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     " highlight - " WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
001040		     "ENTER" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     " select - " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 14
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.

       PORDER-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0910		 TO CPOS.

       PORDER-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       PORDER-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO PORDER-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO PORDER-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO PORDER-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PORDER-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO PORDER-RESTORE-SCREEN
		 WHEN PAGE-DOWN
001340		     GO TO PORDER-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO PORDER-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO PORDER-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO PORDER-GET-USER-OPT.

001240 PORDER-GET-NEXT-PAGE.
	   IF W75-S < 12
	       GO TO PORDER-CRT-ATTRIB.
      *	       GO TO PORDER-GET-USER-OPT.

       PORDER-NEW-PAGE.
	     MOVE 0909		 TO CPOS.
	     PERFORM PORDER-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM PORDER-LUP-TAB-LOOP.
027830	     GO TO PORDER-GET-ORD-RECORD.

       PORDER-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       PORDER-GET-PREV-LOOP.
	     PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       MOVE SPACE	 TO WS-OPTION
	       GO TO PORDER-NEW-PAGE.
	   IF NOT (ORD-CRED = P-NUMBER)
	       MOVE SPACE	 TO WS-OPTION
	       GO TO PORDER-NEW-PAGE.
	   IF WS-IXS1 < 24
               ADD 1             TO WS-IXS1
	       GO TO PORDER-GET-PREV-LOOP.
	     GO TO PORDER-NEW-PAGE.

027970 PORDER-NO-RECORD.
027730	     DISPLAY "No outstandig orders - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO PORDER-RESTORE-SCREEN.

       PORDER-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO PORDER-HIGHLIGHT.

       PORDER-CHK-NO-KEYS.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO PORDER-HIGHLIGHT.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       PORDER-HIGHLIGHT.
	     PERFORM PORDER-CRT-ATTRIB.
	     DISPLAY W43-DET2 (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 7.
	     GO TO PORDER-GET-USER-OPT.

       PORDER-CHK-SELECT.
           IF W75-S1 = ZERO
	       GO TO PORDER-GET-USER-OPT.
	     MOVE CSTART	 TO WS1-CURSOR.
	     MOVE W75-CODE(W75-S1)
				 TO ORD-KEY.
	     PERFORM READ-PORDER THRU READ-PORDER-EXIT.
	     MOVE ZERO		 TO COL-AND-LINE.
	     MOVE ORD-KEY	 TO W15-ORD.
	     GO TO PORDER-RESTORE-SCREEN.

       PORDER-CRT-EDGE.
012890	     DISPLAY "³           ³              ³
      -		     "        ³        ³" AT CPOS
		      WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
019260	     ADD 1		 TO CLIN.

       PORDER-RESTORE-SCREEN.
011320	     PERFORM RESTORE-SCREEN.

       PORDER-LOOKUP-EXIT.
             EXIT.
      /
011010 BF155-GET-CREDITOR	SECTION 59.
011020 BF155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
011210 BF160-EXIT.
011220       EXIT.

       BG600	     SECTION 60.
       BG601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.
       BG699.
             EXIT.

       BG700	     SECTION 60.
       BG701.
             MOVE 3              TO TAX-ACTYPE.
             MOVE CT-CODE        TO TAX-CODE.
             MOVE W20-DTE        TO TAX-DATE.
             MOVE CT-REF         TO TAX-REF.
             MULTIPLY -1 BY CT-COST
                                 GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE
                                 GIVING TAX-TAXFREE.
      *      MULTIPLY -1 BY CT-TAX
      * 			 GIVING TAX-VAT.
             MOVE CT-CRED        TO TAX-AC.
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
	     MULTIPLY -1 BY W15-TAXVAL(WS-S9)
				 GIVING TAX-VAL(WS-S9).
	     MULTIPLY -1 BY W15-VATVAL(WS-S9)
				 GIVING TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BG705.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.
       BG799.
             EXIT.

       COPY CREDIT56.TRN.

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
       CB100	     SECTION 62.
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
       CB200	     SECTION 63.
       CB201.
           IF STK-TAX NOT = ZERO
	       COMPUTE W15-USELL ROUNDED = (W10-VSELL /
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
	       COMPUTE W10-CHECK ROUNDED
		   = W15-USELL + (W15-USELL * W05-RTE)
	       IF W10-CHECK > W10-VSELL
		   SUBTRACT 0.01 FROM W15-USELL
	       ELSE
	       IF W10-CHECK < W10-VSELL
		   ADD 0.01	 TO W15-USELL
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W15-UCASH + (W15-UCASH * W05-RTE)
	       IF W10-CHECK > W10-VCASH
		   SUBTRACT 0.01 FROM W15-UCASH
	       ELSE
	       IF W10-CHECK < W10-VCASH
		   ADD 0.01	 TO W15-UCASH
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W15-UWSALE + (W15-UWSALE * W05-RTE)
	       IF W10-CHECK > W10-VWSALE
		   SUBTRACT 0.01 FROM W15-UWSALE
	       ELSE
	       IF W10-CHECK < W10-VWSALE
		   ADD 0.01	 TO W15-UWSALE.
       CB299.
             EXIT.
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
             MOVE ZERO TO COL-AND-LINE.
             MOVE P-NUMBER       TO W15-ACNO.
             GO TO CREDIT-LOOKUP-EXIT.

       CREDIT-CRT-EDGE.
002130	     DISPLAY WS-MCR1 AT CPOS WITH BACKGROUND-COLOR 0
					  FOREGROUND-COLOR 3
		     WS-MCR78 WITH FOREGROUND-COLOR 3
		     WS-MCR80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
019260	     ADD 1 TO CLIN.

       CREDIT-LOOKUP-EXIT.
             EXIT.

      /
      *    ****    W A R E H O U S E
      *
       HA000	       SECTION 65.
       HA00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 30		 TO SHADE-COL.
	     MOVE 24		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1028
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Warehouse Code:        ³" AT 1128
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1228
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO W15-WHSE.

       HA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Warehouse Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT W15-WHSE AT 1146
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		      MOVE SPACES TO W15-WHSE
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
	       DISPLAY W15-WHSE AT 1146
		       WITH FOREGROUND-COLOR 0
			    BACKGROUND-COLOR 7
015830	       IF W15-WHSE = SPACE
015840		   GO TO HA05.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
	   IF W15-WHSE = SPACES
	       PERFORM RESTORE-SCREEN-3
	       GO TO HA999.
013310	     MOVE 6		 TO W50-LENGTH.

       HA10.
013320	     MOVE W15-WHSE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W15-WHSE
	       GO TO HA10.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W15-WHSE.
013380	     MOVE W50-DENT	 TO WAR-CODE.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50W THRU AA59
	       DISPLAY W15-WHSE AT 1146
		       WITH FOREGROUND-COLOR 0
			    BACKGROUND-COLOR 7
018640	       IF W15-WHSE = SPACES
018720		   GO TO HA05
               ELSE
		   GO TO HA10.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "W/H " AT 0629
		      W15-WHSE WITH FOREGROUND-COLOR 0
				    BACKGROUND-COLOR 7.
	     DISPLAY WAR-SALE AT 0640
		     WITH FOREGROUND-COLOR 3
			  BACKGROUND-COLOR 7.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       PERFORM RESTORE-SCREEN-3
	       GO TO HA00.

       HA999.
	     EXIT.
      /
      *    ****    W A R E H O U S E   Q U A N T I T I E S
      *
       IA000	       SECTION 66.
       IA00.
	     MOVE ZERO		 TO W15-WQUANT.
	     MOVE W10-EXT-ITEM	 TO WST-EXT-CODE.
	     MOVE SPACES	 TO WST-WAR.
	     PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO IA999.

       IA05.
	     PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO IA999.
	   IF NOT (WST-EXT-CODE = W10-EXT-ITEM)
	       GO TO IA999.
	   IF NOT (WST-QUANT < 0.01)
	       ADD WST-QUANT	 TO W15-WQUANT.
	     GO TO IA05.

	IA999.
	     EXIT.
      /
      *    ****    W A R E H O U S E   L O O K U P   R O U T I N E
      *
000010 WARHSE-LOOKUP   SECTION 67.

       WHSE-SAVE-LIN.
	     MOVE LIN		 TO SLIN.

       WHSE-LUP-SCREEN.
000040       DISPLAY CLR-SCREEN. 
	     DISPLAY S-WAR.

       WHSE-LUP-CLR-SCREEN.
	     MOVE 0602		 TO CPOS.
	     PERFORM WHSE-CRT-EDGE UNTIL CLIN = 22.

       WHSE-LUP-DATA.
	   IF W10-ETYPE = "Z"
	       NEXT SENTENCE
	   ELSE
	       ACCEPT SA-WAR.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
000140		     MOVE SPACES TO W15-WHSE
000150		     GO TO WHSE-RESET-LIN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO WHSE-LUP-DATA.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
000170	     DISPLAY CLEAR-L25.
000180	     MOVE 6		 TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       WHSE-LUP-TAB-LOOP.
	     MOVE ZERO		 TO W75-WHCDE (W75-S1).
	   IF W75-S1 < 18
	       ADD 1		 TO W75-S1
	       GO TO WHSE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.        

       WHSE-LUP-ENTRY-TYPE.
	   IF W15-WHSE = SPACES
	       MOVE "!" 	 TO W15-WHSE.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-WHSE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W15-WHSE	 TO WAR-CODE.
	     PERFORM START-AT-LOCAT THRU READ-WARHSE-EXIT.
	   IF WS-F-ERROR = 51
	       GO TO WHSE-NO-RECORD.

000910 WHSE-GET-WHSE-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO WHSE-SKIP-WHSE-READ.
000920	     PERFORM READ-WARHSE-NEXT THRU READ-WARHSE-EXIT.
000930	   IF WS-F-ERROR = 51
000940	       SUBTRACT 1	 FROM W75-S
000950	       GO TO WHSE-FULL-SCREEN.

       WHSE-SKIP-WHSE-READ.
000960	     MOVE WAR-CODE	 TO W75-WHCDE (W75-S).

       WHSE-DISP-DET.
	     DISPLAY (CLIN, 2) WAR-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY ( , 9) WAR-NAME
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY ( , 50) WAR-TEL
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY ( , 65) WAR-FAX
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF CLIN < 21
	       ADD 1		 TO CLIN W75-S
	       GO TO WHSE-GET-WHSE-REC.

       WHSE-FULL-SCREEN.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " /" WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     "," WITH FOREGROUND-COLOR 7
001040		     "ENTER" WITH FOREGROUND-COLOR 14
001050		     " select," WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape to quit" WITH FOREGROUND-COLOR 7.

       WHSE-POSITION-CURSOR.
	     PERFORM SAVE-SCREEN-2.
001220	     MOVE 0		 TO W75-S1.
001230       MOVE 0601           TO CPOS.

001250 WHSE-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001290	     MOVE 2000 TO W41-LENGTH.

001310 WHSE-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO WHSE-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO WHSE-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO WHSE-CHK-SELECT
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO WHSE-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO WARHSE-LUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO WHSE-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO WHSE-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO WHSE-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO WHSE-GET-USER-OPT.

002120 WHSE-CRT-EDGE.
002130	     DISPLAY "      ³                                        ³
      -		     "            ³               " AT CPOS
		      WITH FOREGROUND-COLOR 3.
002140	     ADD 1		 TO CLIN.

       WHSE-GET-NEXT-PAGE.
001550	   IF W75-S < 16
001560	       GO TO WHSE-GET-USER-OPT.

       WHSE-NEW-PAGE.
001570	     MOVE 0601		 TO CPOS.
	     PERFORM WHSE-CRT-EDGE UNTIL CLIN = 22.
001590	     MOVE 6		 TO CLIN.
001600       MOVE LOW-VALUES TO W75-KEYS.
001610	     MOVE 1		 TO W75-S W75-S1.
001620	     PERFORM WHSE-LUP-TAB-LOOP.
001630	     GO TO WHSE-GET-WHSE-REC.

       WHSE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"            TO WS-OPTION.

       WHSE-GET-PREV-LOOP.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
	   IF WS-F-ERROR = 51
	       MOVE SPACE	 TO WS-OPTION
	       GO TO WHSE-NEW-PAGE.
           IF WS-IXS1 < 32
               ADD 1             TO WS-IXS1
	       GO TO WHSE-GET-PREV-LOOP.
	     GO TO WHSE-NEW-PAGE.

001650 WHSE-NO-RECORD.
001660	     DISPLAY "No record - " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001670		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001680		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001690		     "Q" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001700		     "uit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

001720 WHSE-GET-USER-OPT-2.
001730	     ACCEPT WS-OPTION AT 2538
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
001790	   IF NOT (WS-OPTION = "N" OR "Q")
001800	       GO TO WHSE-GET-USER-OPT-2.
001810     IF WS-OPTION = "N"
001820	       GO TO WHSE-LUP-SCREEN.
001830	     MOVE SPACES	 TO W15-WHSE.
001840	     GO TO WHSE-RESET-LIN.

001860 WHSE-ADJ-NO-KEYS.
001870     IF W75-S1 > 1
001880         SUBTRACT 1        FROM W75-S1 CLIN.
001890	     GO TO WHSE-HIGHLIGHT.

001910 WHSE-CHK-NO-KEYS.
001920     IF W75-S1 = ZERO
001930         MOVE 1            TO W75-S1
001940	       GO TO WHSE-HIGHLIGHT.
001950     IF W75-S1 < W75-S
001960         ADD 1             TO W75-S1 CLIN.

001980 WHSE-HIGHLIGHT.
001990	     PERFORM WHSE-CRT-ATTRIB.
002000       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
002010	     GO TO WHSE-GET-USER-OPT.

002030 WHSE-CHK-SELECT.
002040     IF W75-S1 = ZERO
002050	       GO TO WHSE-GET-USER-OPT.
002060	     MOVE CSTART	 TO WS1-CURSOR.
002070	     MOVE W75-WHCDE (W75-S1)
				 TO W15-WHSE WAR-CODE.
002080	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.
002090	     MOVE ZERO		 TO COL-AND-LINE.
002100	     GO TO WHSE-RESET-LIN.

002160 WHSE-RESET-LIN.
	     MOVE SLIN		  TO LIN.

       WARHSE-LUP-EXIT.
	     EXIT.

      /
019640 ZA000-INIT    SECTION 91.
019650 ZA000-START.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = WS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
	     MOVE LS-PARID	 TO WS-PARID.
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
	     MOVE "BARCOD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-BARCOD.
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
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
	     MOVE "PRCLAB"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-PRCLAB.
	     MOVE "PURCH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-PURCH.
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
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
	     MOVE PAR-USE-3-DEC  TO WS-USE-3.
	   IF NOT (PAR-USE-3-DEC = "Y")
	       MOVE "N" 	 TO WS-USE-3.
	     MOVE PAR-BAR-CODE	 TO WS-BAR-CODE.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     OPEN I-O PRCLAB.
	   IF NOT (WS-STATUS = "00")
               CLOSE PRCLAB
               OPEN OUTPUT PRCLAB
               INITIALIZE PLB-REC1
               MOVE 2            TO PLB-QUANT
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               CLOSE PRCLAB
               OPEN I-O PRCLAB.
	     READ PRCLAB.
             MOVE PLB-QUANT      TO WS-PRCKEY.
	     OPEN EXTEND BARCOD.
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

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
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
020210 ZB000-I-O-ERROR SECTION 92.
      
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
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE ORD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 17
041860	       MOVE W02-PURCH	 TO WS-FILE
041870	       MOVE WS-PURKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 37
041860         MOVE W02-PRCLAB   TO WS-FILE
041870         MOVE WS-PRCKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041770     IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE WAR-CODE     TO WS-KEYX
041840     ELSE
041770     IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX
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
