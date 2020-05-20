      $SET LINKCOUNT "512"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	    ****       **      *
      *   **	**  **	      **    **	 **  **    **  **     ***      *
      *   **	**  **	      **    **	**    **  **	**     **      *
      *   **	**  **	      *******	**    **  **	**     **      *
      *   **	**  **	      **	**    **  **	**     **      *
      *   **	**  **	      **	 **  **    **  **      **      *
      *   *******   ********  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  S T A N D I N G   D E L I V E R I E S        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP001.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000120 COPY PARAM.SL.

000140 COPY DEBTOR.ISL.

000160 COPY DEBORD.SL.

000140 COPY DEBMEM.SL.

000220 COPY RECOVER.SL.

       COPY ROUTE.SL.

000200 COPY SALE.SL.

       COPY SHARED.SL.

000140 COPY STKMEM.SL.

000330 COPY STOCK.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DEBORD.FDE.

000140 COPY DEBMEM.FDE.

000390 COPY RECOVER.FDE.

       COPY ROUTE.FDE.

000410 COPY SALE.IFD.

       COPY SHARED.FDE.

000140 COPY STKMEM.FDE.

000330 COPY STOCK.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
002780 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002790 77  WS-SUB	   PIC	9(04)	 COMP-5.
002790 77  WS-SUB-1	   PIC	9(04)	 COMP-5.
002800 77  WS-SUB-2	   PIC	9(04)	 COMP-5.
002810 77  WS-SUB-3	   PIC	9(04)	 COMP-5.
002820 77  WS-SUB-4	   PIC	9(04)	 COMP-5.
002830 77  WS-SUB-5	   PIC	9(04)	 COMP-5.
002840 77  WS-SUB-6	   PIC	9(04)	 COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	 COMP-5.
002860 77  WS-IXS2	   PIC	9(04)	 COMP-5.
002870 77  WS-IXS3	   PIC	9(04)	 COMP-5.
002880 77  WS-IXS4	   PIC	9(04)	 COMP-5.
002890 77  WS-MSUB	   PIC	9(04)	 COMP-5.
002900 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
002380 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
002910 77  WS-SALKEY	   PIC	9(06)	 COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
002930 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
002940 77  WS-WRKEY	   PIC	9(04)	 COMP-5.
001720 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
002170 77  WS-SUPER	   PIC	9(02)	 COMP-5.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002950 77  WS-GST-RATE	   PIC S9(03)V99 COMP-3.
002960 77  WS-NTP          PIC 9(01).
002480 77  WS-START	   PIC	9(08).
002490 77  WS-END          PIC  9(08).
002560 77  WS-INO	   PIC	9(02) VALUE 0.
002970 77  WS-LINES	   PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
002780 77  WS-LOOK	   PIC	X(01) VALUE "S".
002790 77  WS-ETYPE        PIC  X(01) VALUE "I".
003030 77  WS-ERROR	   PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-IND	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO         PIC X(06).
003260 77  WS-S1           PIC 9(04)     COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
003360 77  WS-ERR1	   PIC X(22)	  VALUE
003370                     "Invalid account number".
003380 77  WS-ERR2         PIC X(09) VALUE "No record".
003390 77  WS-A            PIC X(19) VALUE "Any key to continue".
003400 77  WS-C            PIC X(07) VALUE "COMPANY".
003410 77  WS-O            PIC X(08) VALUE "INTERNAL".
003420 77  WS-AC           PIC X(07) VALUE "Account".
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
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
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

003440 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DLP".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

003460 01  W00-TRAN-CODES.
003470     03  W00-TRAN-TYPES  OCCURS 100.
003480         05  W00-TRAN-CODE PIC 9(02)   COMP-5.
003490         05  W00-ACTION  PIC 9(02)     COMP-5.
003500         05  W00-E-DESC  PIC X(12).
003510         05  W00-A-DESC  PIC X(12).

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

       COPY DEBORD.ID.

000140 COPY DEBMEM.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY ROUTE.ID.

000410 COPY SALE.ID.

       COPY SHARED.ID.

000140 COPY STKMEM.ID.

000330 COPY STOCK.ID.

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003920 01  W05-TAX-RATES.
003930     03  W05-VAT-RATES.
003940         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
003950     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
003960         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
003970         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
003980         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
003990         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
004000         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
004010         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
004020     03  W05-RATE        PIC S9(03)V99 COMP-3.
004030     03  W05-RTE REDEFINES W05-RATE
004040                         PIC S9V9(04)  COMP-3.
004050     03  W05-VATNO       PIC  X(14).

       COPY W10.STK.

003730 01  W12-DATE.
003740     03  W12-TODAY       PIC 9(06).
003750     03  W12-DATE-DMY REDEFINES W12-TODAY.
003760         05  W12-DAY     PIC 9(02).
003770         05  W12-MONTH   PIC 9(02).
003780         05  W12-YEAR    PIC 9(02).
003790     03  W12-T-YMD       PIC 9(06).
003800     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003810         05  W12-YY      PIC 9(02).
003820         05  W12-MM      PIC 9(02).
003830         05  W12-DD      PIC 9(02).

       01  W15-DATES.
004120	   03  W15-DATE        PIC 9(06).
004120     03  W15-START       PIC 9(06).
004130     03  W15-SDTE-DMY REDEFINES W15-START.
004140         05  W15-SDAY    PIC 9(02).
004150         05  W15-SMNTH   PIC 9(02).
004160         05  W15-SYEAR   PIC 9(02).
004170     03  W15-S-YMD       PIC 9(06).
004180     03  W15-STRT-YMD REDEFINES W15-S-YMD.
004190         05  W15-SYY     PIC 9(02).
004200         05  W15-SMM     PIC 9(02).
004210         05  W15-SDD     PIC 9(02).
004120     03  W15-END         PIC 9(06).
004130     03  W15-EDTE-DMY REDEFINES W15-END.
004140         05  W15-EDAY    PIC 9(02).
004150         05  W15-EMNTH   PIC 9(02).
004160         05  W15-EYEAR   PIC 9(02).
004170     03  W15-E-YMD       PIC 9(06).
004180     03  W15-END-YMD REDEFINES W15-E-YMD.
004190         05  W15-EYY     PIC 9(02).
004200         05  W15-EMM     PIC 9(02).
004210         05  W15-EDD     PIC 9(02).

003840 01  W20-DATE.
003850     03  W20-YY.
003860         05  W20-YEAR    PIC 9(02).
003870     03  W20-MM.
003880         05  W20-MONTH   PIC 9(02).
003890     03  W20-DD.
003900         05  W20-DAY     PIC 9(02).
003910 01  W20-DTE1 REDEFINES W20-DATE.
003920     03  W20-DTE         PIC 9(06).

       01  W22-PERIOD.
	   03  W22-DTE1        PIC  9(06)    COMP-3.
	   03  W22-DTE2        PIC  9(06)    COMP-3.

003930 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
003980     03  W25-TOTAL       PIC S9(07)V99 COMP-3.
003990     03  W25-VALUE       PIC S9(07)V99 COMP-3.

000030 01  W30-DEBORD.
000200	   03  W30-DEBT        PIC  X(06).
	   03  W30-CONT	       PIC  X(08).
	   03  W30-SEQ	       PIC  9(03).
	   03  W30-ROUTE       PIC  X(08).
	   03  W30-CALL        PIC  X(03).
000040	   03  W30-ITEM        PIC  X(14).
	   03  W30-ITM	       PIC  X(03).
	   03  W30-FREQ        PIC  X(01).
      *
      *    ****    D A Y   O F	 W E E K   (SMTWTFS)
      *
	   03  W30-DAYS        PIC  X(07).
	   03  W30-DAY REDEFINES W30-DAYS
			       PIC  X(01)    OCCURS 7.
	   03  W30-NEXT        PIC  9(01).
	   03  W30-START       PIC  9(06)    COMP-3.
000060	   03  W30-DATE        PIC  9(06)    COMP-3.
000070	   03  W30-QUANT       PIC S9(07)V99 COMP-3.
	   03  W30-QNT REDEFINES W30-QUANT
			       PIC S9(09)    COMP-3.
	   03  FILLER	       PIC  X(03).

       COPY W40.DBT.

       COPY W40.WS.

005020 01  W45-TRAN.
005030     03  W45-CODE        PIC 9(02).
005040     03  W45-ENG         PIC X(12).
005050     03  W45-AFR         PIC X(12).
005060     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

005170 COPY W50.WS.

       01  W55-STATS.
           03  W55-H1         PIC  X(09).
           03  W55-H2         PIC  X(09).
           03  W55-H3         PIC  X(09).
           03  W55-H4         PIC  X(09).
           03  W55-H5         PIC  X(09).
           03  W55-H6         PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1   PIC  9(02) COMP-5.
               05  W55-MTH2   PIC  9(02) COMP-5.
               05  W55-MTH3   PIC  9(02) COMP-5.
               05  W55-MTH4   PIC  9(02) COMP-5.
               05  W55-MTH5   PIC  9(02) COMP-5.
               05  W55-MTH6   PIC  9(02) COMP-5.
           03  W55-VALUES.
               05  W55-VAL1   PIC S9(09)V99 COMP-3.
               05  W55-VAL2   PIC S9(09)V99 COMP-3.
               05  W55-VAL3   PIC S9(09)V99 COMP-3.
               05  W55-VAL4   PIC S9(09)V99 COMP-3.
               05  W55-VAL5   PIC S9(09)V99 COMP-3.
               05  W55-VAL6   PIC S9(09)V99 COMP-3.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).

005240 01  W65-ENTRY.
005250     03  W65-FOUR.
005260         05  W65-CHAR OCCURS 8 PIC X(01).
005270     03  W65-PREFIX REDEFINES W65-FOUR.
005280         05  W65-PREF    PIC X(02).
005290         05  FILLER      PIC X(06).

003670 01  W65-INDEX.
003680     03  W65-INPUT.
003690         05  W65-ICHAR OCCURS 8 PIC X(01).

005330 01  W75-TITLES.
005340     03  W75-DESCRIP.
005350         05  FILLER      PIC X(28)       VALUE
005360                     "Mr     Mnr    Mrs    Mev    ".
005370         05  FILLER      PIC X(28)       VALUE
005380                     "Miss   Mej    Ms     Ms     ".
005390         05  FILLER      PIC X(28)       VALUE
005400                     "Dr     Dr     Rev    Ds     ".
005410         05  FILLER      PIC X(28)       VALUE
005420                     "Prof   Prof   The HonSy Edel".
005430         05  FILLER      PIC X(28)       VALUE
005440                     "                            ".
005450     03  W75-CODES REDEFINES W75-DESCRIP.
005460         05  W75-TITLE  OCCURS 10.
005470             07  W75-ETITLE  PIC X(07).
005480             07  W75-ATITLE  PIC X(07).

005830 01  W75-KEYS.
005840     03  W75-S          PIC 9(02) COMP-5.
005850     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
	   03  W75-FREQ       PIC  X(07).
005860	   03  W75-DNO OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).
           03  W75-ITEMS OCCURS 18.
	       05  W75-CODE.
		07  W75-ROUTE.
		 09 W75-ORDER PIC  X(08).
		07  FILLER    PIC  X(06).
	       05  W75-SELL   PIC  9(07)V99 COMP-3.
	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
	       05  W75-CASH   PIC  9(07)V99 COMP-3.
	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
	       05  W75-VWSALE PIC  9(07)V99 COMP-3.

005490 01  W80-ACCOUNT.
005500     03  W80-ACNO        PIC X(06).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530         05  W80-SERIAL  PIC 9(06).
005520     03  W80-TYPE        PIC 9(01).

005540 01  W85-BATCHES.
005550         05  W85-BATCH   PIC 9(05).

005560 01  W90-TRAN.
005570     03  W90-DATE        PIC X(08).
005580     03  W90-DTE REDEFINES W90-DATE
005590                         PIC 99/99/99.
005600     03  W90-CODE        PIC 9(02).
005610     03  W90-REF         PIC X(08).
005620     03  W90-DB          PIC S9(07)V99 COMP-3.
005630     03  W90-CR          PIC S9(07)V99 COMP-3.
005640     03  W90-BDT         PIC S9(07)V99 COMP-3.
005650     03  W90-BCR         PIC S9(07)V99 COMP-3.
005660     03  W90-DESC        PIC X(12).
005670     03  W90-120         PIC S9(07)V99 COMP-3.
005670     03  W90-90          PIC S9(07)V99 COMP-3.
005680     03  W90-60          PIC S9(07)V99 COMP-3.
005690     03  W90-30          PIC S9(07)V99 COMP-3.
005700     03  W90-CUR         PIC S9(07)V99 COMP-3.
005700     03  W90-MTD         PIC S9(07)V99 COMP-3.
005710     03  W90-BAL         PIC S9(07)V99 COMP-3.
005720     03  W90-IN          PIC 9(04)     COMP-5.
005730     03  W90-IV          PIC S9(07)V99 COMP-3.
005740     03  W90-RN          PIC 9(04)     COMP-5.
005750     03  W90-RV          PIC S9(07)V99 COMP-3.
005760     03  W90-DN          PIC 9(04)     COMP-5.
005770     03  W90-DV          PIC S9(07)V99 COMP-3.
005780     03  W90-CN          PIC 9(04)     COMP-5.
005790     03  W90-CV          PIC S9(07)V99 COMP-3.
005800     03  W90-INT         PIC S9(07)V99 COMP-3.
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020	   03  W90-CS	       PIC  9(02)    COMP-5.

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).
005830     03  W95-ADD1        PIC X(30).
005840     03  W95-ADD2        PIC X(30).
005850     03  W95-ADD3        PIC X(30).
005860     03  W95-ADD4        PIC X(30).
005870     03  W95-EDIT        REDEFINES W95-ADD4.
005880         05  W95-V1.
005890             07  W95-T   PIC Z(04)9.99.
005900         05  W95-V2.
005910             07  W95-B   PIC Z(06)9.99-.
005920         05  FILLER      PIC X(11).
005930     03  W95-POST        PIC 9(04).
005940     03  W95-TEL         PIC X(14).

005950 01  W100-HEAD.
005960     03  FILLER          PIC X(26)    VALUE
005970                       "DEBTOR ACCOUNTS LISTING -".
005980     03  W100-DESC       PIC X(11).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500	   03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.
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
005990 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
				"STANDING ORDERS".
	   03  LINE  4 COLUMN  4 VALUE "Account No  ".
	   03	       COLUMN 39 VALUE "(".
	   03	       COLUMN 40 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "DD".
	   03	       COLUMN 42 VALUE "/".
	   03	       COLUMN 43 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "MM".
	   03	       COLUMN 45 VALUE "/".
	   03	       COLUMN 46 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "YY".
	   03	       COLUMN 48 VALUE ")".
	   03  LINE  5 COLUMN  4 VALUE "Order Ref.  ".
	   03	       COLUMN 26 VALUE "Starting Date".
	   03	       COLUMN 49 VALUE "Route/Call".
	   03	       COLUMN 68 VALUE "/".
	   03  LINE  6 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Indicate day/s by entering a Y under the day".
	   03	       COLUMN 56 VALUE "(SMTWTFS)".
	   03  LINE  7 COLUMN  4 VALUE "Frequency   ".
	   03	       COLUMN 19 VALUE "(".
	   03	       COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 21 VALUE "aily/".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "W".
	   03	       COLUMN 27 VALUE "eekly/".
	   03	       COLUMN 33 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "M".
	   03	       COLUMN 34 VALUE "onthly)".
	   03	       COLUMN 42 VALUE "Day/s of Week".
	   03  LINE  8 COLUMN 10 VALUE "ÚÄNoÂÄÄItem CodeÄÄÄÂÄÄÄÄÄÄÄÄÄDes
      -				       "criptionÄÄÄÄÄÄÄÄÄÄÂÄQuantityÄ¿".
	   03  LINE 23 COLUMN 10 VALUE "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ
      - 			       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ".

       01  S02.
	   02  S02A.
	   03  LINE  4 COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(06) USING W30-DEBT AUTO.

	   02  S02B.
	   03  LINE  5 COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W30-CONT AUTO.

	   02  S02C.
	   03  LINE  5 COLUMN 40 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 99/99/99 USING W15-START AUTO.

	   02  S02D.
	   03  LINE  5 COLUMN 60 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W30-ROUTE AUTO.

	   02  S02DA.
	   03  LINE  5 COLUMN 69 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(03) USING W30-CALL AUTO.

	   02  S02E.
	   03  LINE  7 COLUMN 17 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(01) USING W30-FREQ AUTO.

	   02  S02F.
	   03  LINE  7 COLUMN 57 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(07) USING W30-DAYS AUTO.

       01  S04.
	   03  COLUMN 15 BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 PIC X(14) USING W10-ITEM AUTO.

005530 01  S04-3.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I3 AUTO.

005530 01  S04-4.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S04-5.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S04-6.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S04-7.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S04-8.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S04-9.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S04-10.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S04-11.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S04-12.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S04-13.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S04-14.
007920	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       01  S05.
	   03  COLUMN 30 BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT
			 PIC X(30) FROM W10-DESC.

       01  S06.
	   03  COLUMN 61 BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 PIC Z(06)9.99 USING W30-QUANT AUTO.

       COPY DEBLUP.CRT.

       COPY DEBORD.CRT.

       COPY ROUTE.CRT.

       COPY STKLUP.CRT.

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
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
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

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM DA000.
	     CLOSE DEBORD
013040		   RECOVER
		   SALE.

       AA49.
006950	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010460 AA50D.
010470	     MOVE "D"		 TO WS-LOOK.
	     GO TO AA50.

       AA50O.
	     MOVE "O"		 TO WS-LOOK.
	     GO TO AA50.

010460 AA50R.
010470	     MOVE "R"		 TO WS-LOOK.

010480 AA50.
011050	     PERFORM SAVE-SCREEN.
010740	   IF WS-LOOK = "O"
010750	       PERFORM DEBORD-LOOKUP
	   ELSE
010740	   IF WS-LOOK = "R"
010750	       PERFORM ROUTE-LOOKUP
	   ELSE
011080	       MOVE 4		 TO W44-FUNCTION
011090	       PERFORM SCREEN-CONTENTS
010740	       IF WS-LOOK = "D"
010750		   PERFORM DEBTOR-LOOKUP
010760	       ELSE
	       IF WS-LOOK = "S"
010770		   PERFORM STOCK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010890	     MOVE "S" TO WS-LOOK.
010900 AA99.
010910       EXIT.
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
011080		     " X/Ref Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to end"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.

       COPY DATE.CHK.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD1.
      
       COPY DEBORD.RD.

000140 COPY DEBMEM.RD.

       COPY ROUTE.RD.

011310 COPY SALE.RD.

       COPY SHARED.RD.

000140 COPY STKMEM.RD.

011310 COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DEBORD.WR.

       COPY DEBTOR.WR.

       COPY PARAM.WR.

011310 COPY SALE.WR.

       COPY DEBORD.LUP.

       COPY DEBTOR.LUP.

011610 COPY ROUTE.LUP.

011610 COPY STOCK.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

010970 AY053.
010980	     MOVE 53		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011000	     MOVE DOR-RECORD1	 TO REC-DEBORD.
011010       GO TO AY50.

011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.

011250 AY50.
           IF WS-SKIP = "Y"
               GO TO AY59.
011260       ADD 1               TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011340         STOP RUN.

011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
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
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010	     MOVE 1		 TO WS-NETKEY.
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
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
008930 CA155-GET-DEBTOR        SECTION 2.
008940 CA155-ACCOUNT.
012010       MOVE W80-ACNO       TO DEB-ACNO.
014330       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
009100 CA160-EXIT.
009110       EXIT.

009120 CA165-NAME              SECTION 2.
009130 CA165-EXTRACT.
009140       MOVE ZERO TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180           MOVE 1 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.

009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230       MOVE 0 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250         INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
009260         OVERFLOW
009270           MOVE 1 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    ****    D E B T O R	 S T A N D I N G   O R D E R
      *
037050 DA000	       SECTION 5.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
020330	     DISPLAY S01.
018620	     MOVE 9		 TO LIN.
018630	     PERFORM DA915 UNTIL LIN > 22.
	     INITIALIZE W30-DEBORD.
	     MOVE "NNNNNNN"	 TO W30-DAYS.
	     DISPLAY S02.
020370       MOVE SPACES         TO W80-ACNO.
032520	     MOVE 1		 TO WS-FIRST WS-LAST
	     MOVE ZERO		 TO WS-IND.

020360 DA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
020380	     ACCEPT S02A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W30-DEBT
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA50
		     MOVE W80-ACNO TO W30-DEBT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
016570	       DISPLAY S02A
016580	       IF W30-DEBT = SPACES
016590		   GO TO DA05.
	     DISPLAY CLEAR-L25.
020390	   IF W30-DEBT = SPACES
020400	       GO TO DA999.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-DEBT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W30-DEBT	 TO DEB-ACNO W80-ACNO.
020410       PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
020430	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020440	       GO TO DA05.
020450     IF WS-ERROR = 2
020460	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO DA05.
032840	     MOVE DEB-NAME	 TO W40-NAME.
032850     IF W80-TYPE = 1
032860         INSPECT W40-NAME REPLACING ALL "*" BY " "
032870         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060	     DISPLAY W40-NAME AT 0424
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
	       GO TO DA05.
	     MOVE DEB-AREA	 TO W30-ROUTE.
       DA10.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Order Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S02B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA05
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50O THRU AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA10
	       END-EVALUATE
016570	       DISPLAY S02B
016580	       IF W30-CONT = SPACES
016590		   GO TO DA10.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-CONT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W30-DEBT	 TO DOR-DEBT
	     MOVE W30-CONT	 TO DOR-CONT.
	     MOVE ZERO		 TO DOR-SEQ.
	     PERFORM READ-DEBORD THRU READ-DEBORD-EXIT.
	   IF WS-F-ERROR = 0
	       GO TO DA100.

       DA15.
	     ACCEPT S02C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA10
	       END-EVALUATE.
027380	     MOVE W15-START	 TO W10-EDTE.
027390       PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA15.
	     MOVE W22-DTE1	 TO W15-S-YMD.

       DA18.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Route Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S02D.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA15
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50R THRU AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA18
	       END-EVALUATE
	       DISPLAY S02D.
	   IF W30-ROUTE = SPACES
	       GO TO DA18.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-ROUTE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       DA20.
	     ACCEPT S02DA.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA15
	       END-EVALUATE.
	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-CALL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

       DA22.
	     ACCEPT S02E.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA20
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING W30-FREQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W30-FREQ = "D" OR "W" OR "M")
027410	       MOVE "Frequency code invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA22.

       DA25.
	     ACCEPT S02F.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA22
	       END-EVALUATE.
	     MOVE 7		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-DAYS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE 1		 TO WS-S1
	     MOVE ZERO		 TO WS-S2.

       DA30.
	   IF NOT (W30-DAY (WS-S1) = "N" OR "Y")
027410	       MOVE "Days of week invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA25.
	   IF W30-DAY (WS-S1) = "Y"
	       ADD 1		 TO WS-S2.
	   IF WS-S1 < 7
	       ADD 1		 TO WS-S1
	       GO TO DA30.
	   IF (W30-FREQ = "D" OR "W") AND
	      (W30-DAYS = "NNNNNNN")
	       MOVE "No day of week specified"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA25.
	   IF (W30-FREQ = "D") AND
	      (WS-S2 = 1)
	       MOVE "Daily frequency! Should be weekly?"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA25.
	   IF (W30-FREQ = "W") AND
	      (WS-S2 > 1)
	       MOVE "Weekly frequency! Should be Daily?"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO DA25.
020910	     MOVE 9		 TO LIN.

020920 DA35.
020930	     MOVE SPACES	 TO W10-DESC W10-ITEM.
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
020940	     MOVE ZERO		 TO W10-SELL WS-SUPER.
020940	     MOVE 1		 TO W30-QUANT.

020950 DA40.
020960       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF W10-STCK = "A"
021070	       ACCEPT S04 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S04-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S04-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S04-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S04-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S04-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S04-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S04-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S04-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S04-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S04-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S04-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S04-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
021050		     SUBTRACT 1  FROM WS-LAST
		     GO TO DA75
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
		     GO TO DA40
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50
021010	       DISPLAY (LIN, 15) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 5.
021040	   IF W10-ITEM = SPACES
021060	       GO TO DA40.

       DA45.
021070       PERFORM CA000.
021080     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
018630	       DISPLAY (LIN, 15) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 5
018640         IF W10-ITEM = SPACES
018720		   GO TO DA40
               ELSE
		   GO TO DA45.
	   IF STK-IND = 2
	       MOVE "Labour Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA40.
	   IF STK-IND = 3
	       MOVE "Miscellaneous Item"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA40.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA40.
021110	     MOVE WS-LAST	 TO WS-SALKEY.
021120       INITIALIZE SAL-RECORD1.
021130       MOVE WS-LAST        TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   I N V O I C E
      *
021140	     DISPLAY (LIN, 11) SAL-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
021150       MOVE STK-CODE       TO SAL-ITEM.
021160	     DISPLAY CLEAR-L25.
021260	     MOVE STK-DESC	 TO W10-DESC SAL-DESC.
021270	     DISPLAY (LIN, 30) W10-DESC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.

       DA50.
021370	     ACCEPT S06 AT LINE LIN.
	   IF (W30-QUANT = ZERO)
	       DISPLAY (LIN, 10) "³   ³              ³
      -				 "            ³          ³"
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
021400	       GO TO DA35.

       DA55.
036100	     MOVE "'A'ccept or 'C'ancel  [A]"
				 TO WS-ERR-MES.
023210	     MOVE "A"		 TO WS-OPTION.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
	     PERFORM OPT-MESSAGE.
023300	   IF NOT (WS-OPTION = "A" OR "C")
023310	       GO TO DA55.
023330     IF WS-OPTION = "C"
	       DISPLAY (LIN, 10) "³   ³              ³
      -				 "            ³          ³"
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
021400	       GO TO DA35.
022300	     MOVE W30-QUANT	 TO SAL-QNT.
011960	     PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
011970	     ADD 1		 TO WS-LAST.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
011980	   IF LIN < 22
011990	       ADD 1		 TO LIN
012000	       GO TO DA35.
012010	     MOVE 9		 TO LIN.
012020	     PERFORM DA915 UNTIL LIN > 22.
012030	     PERFORM DA60 THRU DA70.
012040	     GO TO DA35.

       DA60.
	     SUBTRACT 14 FROM WS-LAST
				 GIVING WS-SALKEY.
	     MOVE 9		 TO LIN.

       DA65.
016180	     PERFORM READ-SALE THRU READ-SALE-EXIT.
016190	     DISPLAY (LIN, 11) SAL-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
016200	     DISPLAY ( , 15) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
016210	     DISPLAY ( , 30) SAL-DESC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
016220	     MOVE SAL-QNT	 TO W30-QUANT.
016230	     DISPLAY S06 AT LINE LIN.
	     ADD 1		 TO LIN.
	   IF WS-SALKEY < (WS-LAST - 1)
	       ADD 1		 TO WS-SALKEY
	       GO TO DA65.
       DA70.
	     EXIT.

023910 DA75.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
023920     IF WS-LAST = ZERO
023930	       GO TO DA00.
      *
      *    ****    P R O C E S S   T H E   O R D E R
      *
023940 DA80.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
024040 DA85.
	     PERFORM OPT-MESSAGE.
024110	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
024120	       GO TO DA80.
024140     IF WS-OPTION = "M"
024150         ADD 1             TO WS-LAST
024160	       GO TO DA35.
024170     IF WS-OPTION = "A"
024180	       PERFORM EA000
024190	       GO TO DA80.
024200     IF WS-OPTION = "C"
	       IF WS-IND = 0
024210		   GO TO DA999.
      *
      *    ****    P R O C E S S   T H E   O R D E R
      *
	     DISPLAY CLEAR-L25.
	   IF WS-IND = 1
	       DISPLAY "Updating Order" AT 2502 WITH
025440			BACKGROUND-COLOR 3
			FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	   ELSE
025430	       DISPLAY "Processing Order" AT 2502 WITH
025440			BACKGROUND-COLOR 3
			FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
018690	     MOVE 1		 TO WS-SALKEY.
025490	     PERFORM AY60 THRU AY999.
	     MOVE ZERO		 TO W30-SEQ.
	     MOVE W15-S-YMD	 TO W30-START W30-DATE.

       DA90.
	     MOVE W30-DEBORD	 TO DOR-RECORD1.
	     PERFORM WRITE-DEBORD THRU WRITE-DEBORD-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY053 THRU AY59.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   I N V O I C E
      *
026840     IF WS-SALKEY > WS-LAST
026850	       GO TO DA95.
026860       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-CODE	 TO W30-SEQ.
	     MOVE SAL-ITEM	 TO W30-ITEM.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     ADD 1		 TO WS-SALKEY.
	     GO TO DA90.

       DA95.
	     PERFORM AY70 THRU AY999.
029530	     CLOSE DEBORD
029570		   SALE.
	     OPEN OUTPUT SALE.
	     CLOSE SALE.
029620	     OPEN I-O DEBORD
029670		      SALE.
029690	     DISPLAY CLEAR-L25.
	     GO TO DA00.
      *
      *    ****    E X I S T I N G   O R D E R
      *
       DA100.
	     MOVE DOR-RECORD1	 TO W30-DEBORD.
	     MOVE W30-START	 TO W15-S-YMD.
	     MOVE W15-SDD	 TO W15-SDAY.
	     MOVE W15-SMM	 TO W15-SMNTH.
	     MOVE W15-SYY	 TO W15-SYEAR.
	     DISPLAY S02.
	     MOVE SPACE 	 TO WS-OPTION.

       DA105.
	     MOVE "Existing order - 'A'mend or 'N'ext [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "A" OR "N")
	       GO TO DA105.
	   IF WS-OPTION = "N"
	       GO TO DA00.
	     MOVE 1		 TO WS-IND.
	     PERFORM AY60 THRU AY999.
	     MOVE 0		 TO WS-LAST.
021120       INITIALIZE SAL-RECORD1.

       DA110.
	     MOVE 2		 TO WS-ACTION.
	     PERFORM AY053 THRU AY59.
	     PERFORM DELETE-DEBORD-REC THRU WRITE-DEBORD-EXIT.
	     PERFORM READ-DEBORD-NEXT THRU READ-DEBORD-EXIT.
	   IF NOT (WS-F-ERROR = 0)
	       GO TO DA115.
	   IF NOT (DOR-DEBT = W30-DEBT)
	       GO TO DA115.
	   IF NOT (DOR-CONT = W30-CONT)
	       GO TO DA115.
011970	     ADD 1		 TO WS-LAST.
021140	     MOVE DOR-SEQ	 TO SAL-CODE.
021150	     MOVE DOR-ITEM	 TO SAL-ITEM STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
021260	     MOVE STK-DESC	 TO SAL-DESC.
022300	     MOVE DOR-QUANT	 TO SAL-QNT.
021110	     MOVE WS-LAST	 TO WS-SALKEY.
011960	     PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
	     GO TO DA110.

       DA115.
	     PERFORM AY70 THRU AY999.
024180	     PERFORM EA000.
024190	     GO TO DA80.

       DA915.
	     DISPLAY (LIN, 10) "³   ³              ³
      -			       "          ³          ³".
	     ADD 1		 TO LIN.

       DA999.
	     EXIT.

      /
      *    ****    A M E N D   O R D E R   D E T A I L S
      *
       EA000		SECTION 57.
029980 EA00.
	     MOVE "Amend - 'H'eader, 'O'rder details or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE "O"		 TO WS-OPTION.
       EA05.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "H" OR "O")
	       GO TO EA00.
	   IF WS-OPTION = "O"
	       GO TO EA25
           ELSE
           IF WS-OPTION = "E"
	       GO TO EA999.
       EA10.
	     MOVE "Amend - 'S'tart date,'F'requency,'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
       EA15.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "F" OR "S")
	       GO TO EA10.
           IF WS-OPTION = "E"
	       GO TO EA999.
	   IF WS-OPTION = "F"
	       GO TO EA20.
      *
      *    ****    S T A R T   D A T E
      *
	     MOVE W15-S-YMD	 TO W15-DATE.
       EA16.
	     ACCEPT S02C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE W15-DATE
				 TO W15-S-YMD
		     GO TO EA10
	       END-EVALUATE.
027380	     MOVE W15-START	 TO W10-EDTE.
027390       PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO EA15.
	     MOVE W22-DTE1	 TO W15-S-YMD.
	     GO TO EA10.
      *
      *    ****    F R E Q U E N C Y
      *
       EA20.
	     ACCEPT S02E.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO EA10
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING W30-FREQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W30-FREQ = "D" OR "W" OR "M")
027410	       MOVE "Frequency code invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO EA20.

       EA22.
	     ACCEPT S02F.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO EA20
	       END-EVALUATE.
	     MOVE 7		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-FREQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE 1		 TO WS-S1.

       EA24.
	   IF NOT (W30-DAY (WS-S1) = "N" OR "Y")
027410	       MOVE "Days of week invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO EA22.
	   IF W30-DAYS = "NNNNNNN"
	       MOVE "No day of week specified"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
027420	       GO TO EA22.
	     GO TO EA10.

       EA25.
018620	     MOVE 9		 TO LIN.
018630	     PERFORM DA915 UNTIL LIN > 22.
030010	     MOVE 9		 TO LIN.
030020       MOVE 1              TO WS-LINES WS-FIRST.

030030 EA30.
030040	     MOVE WS-FIRST	 TO WS-SALKEY.

030050 EA35.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
030060     IF WS-SALKEY > WS-LAST
030070	       GO TO EA45.
030080       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030090	     DISPLAY (LIN, 11) SAL-CODE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
030130	     DISPLAY ( , 15) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5.
030140	     DISPLAY ( , 30) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
030150	     MOVE SAL-QNT	 TO W30-QUANT.
030160	     DISPLAY S06 AT LINE LIN.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
030210 EA40.
030220	   IF LIN < 22
030230         ADD 1             TO LIN WS-SALKEY WS-LINES
030240	       GO TO EA35.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
030250 EA45.
029400	   IF WS-LINES < 14
029410         IF WS-FIRST = 1
029420		   MOVE "'A'mend/'D'elete an Item - 'E'xit"
				 TO WS-ERR-MES
029440	       ELSE
029450		   MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES
029480		   MOVE 2	 TO WS-LINES
029490     ELSE
029500     IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029450		   MOVE "'A'mend/'D'elete an Item,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029550		   MOVE 1	 TO WS-LINES
029560         ELSE
029450		   MOVE "'A'mend/'D'elete,'P'revious,'N'ext - 'E'xit"
				 TO WS-ERR-MES
029600		   MOVE 3	 TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029450	       MOVE "'A'mend/'D'elete an Item,'P'revious - 'E'xit"
				 TO WS-ERR-MES.
030520 EA50.
	     PERFORM OPT-MESSAGE.
030590	   IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
030600	       GO TO EA45.
030610     IF WS-OPTION = "E"
030620	       GO TO EA999.
030630     IF WS-OPTION = "A" OR "D"
030640	       GO TO EA60.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
030650     IF WS-LINES = 2 OR 3
030660         IF WS-OPTION = "P"
030670		   PERFORM EA55
030690		   SUBTRACT 14	 FROM WS-FIRST
030700		   GO TO EA30.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
030710     IF WS-LINES = 1 OR 3
030720         IF WS-OPTION = "N"
030730		   PERFORM EA55
030750		   ADD 14	 TO WS-FIRST
030760		   GO TO EA30.
030770	     GO TO EA45.
030780 EA55.
      *
      *    ****    C L E A R   S C R E E N
      *
030790	     MOVE 9		 TO LIN.
030800	     PERFORM DA915 UNTIL LIN > 22.
030810	     MOVE 9		 TO LIN.
030820       MOVE 1              TO WS-LINES.
030830 EA60.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
030840     IF WS-OPTION = "A"
030850	       GO TO EA70.
030890 EA65.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
043780	     MOVE "Are you sure Y/N?  [ ]"
				 TO WS-ERR-MES.
030880	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF NOT (WS-OPTION = "N" OR "Y")
030980	       GO TO EA65.
031000     IF WS-OPTION = "N"
031010	       GO TO EA45.
031020       MOVE "D"            TO WS-OPTION.
031030 EA70.
031040	     DISPLAY CLEAR-L25.
031050       DISPLAY "Enter item No to Amend/Delete >  <" AT 2501
031060		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
031070       MOVE ZERO           TO WS-INO.
031080 EA75.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
031090	     ACCEPT WS-INO AT 2532
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
031100                                  UPDATE AUTO.
031110     IF WS-INO = ZERO
031120	       DISPLAY CLEAR-L25
031130	       GO TO EA45.
031140     IF WS-INO > WS-SALKEY
031150	       GO TO EA75.
031160     IF WS-INO < WS-FIRST
031170	       GO TO EA75.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
031180	   IF WS-INO < 15
031190	       ADD 8 WS-INO	 GIVING LIN
031200     ELSE
031210	       COMPUTE LIN = WS-INO - WS-FIRST + 9.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
031220       MOVE WS-INO         TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
031240     IF SAL-ITEM = SPACES
031250	       GO TO EA80.
031260       MOVE SAL-ITEM       TO W10-ITEM STK-CODE.
031270       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
031280	     MOVE SAL-QNT	 TO W30-QUANT.
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
031400 EA80.
031410     IF WS-OPTION = "A"
031420	       GO TO EA90.
031450 EA85.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
031460       MOVE WS-SALKEY      TO WS-SALSTR.
031470       ADD 1               TO WS-SALKEY.
031480     IF WS-SALKEY > WS-LAST
031490         SUBTRACT 1        FROM WS-LAST
031500	       GO TO EA25.
031510       PERFORM READ-SALE THRU READ-SALE-EXIT.
031520       SUBTRACT 1          FROM SAL-CODE.
031530       MOVE WS-SALSTR      TO WS-SALKEY.
031540       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031550       ADD 1               TO WS-SALKEY.
031560	     GO TO EA85.
031570 EA90.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
031620	     ACCEPT S06 AT LINE LIN.
031640	   IF W30-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
031670	       MOVE SAL-QNT	 TO W30-QUANT
031680	       GO TO EA90.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
031830	     MOVE W30-QUANT	 TO SAL-QNT.
031910 EA100.
031920       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031930	     GO TO EA00.
031940 EA999.
031950       EXIT.
      /
025020 ZA00-INIT	      SECTION 90.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
042000	     DISPLAY CLR-SCREEN.
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
	     MOVE "DEBORD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBORD.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "ROUTE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-ROUTE.
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
025110	     MOVE 3		 TO WS-PARKEY.
025120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
054270	     OPEN OUTPUT SALE.
054280       CLOSE SALE.
054290       OPEN I-O SALE.
044300	     OPEN I-O DEBORD.
           IF WS-STATUS = "05"
	       CLOSE DEBORD
	       OPEN OUTPUT DEBORD
	       CLOSE DEBORD
	       OPEN I-O DEBORD.
	   IF WS-STATUS = "39"
	       MOVE 53		 TO WS-F-ERROR
	       GO TO ZA50.
044310	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 53	 TO WS-F-ERROR
                   GO TO ZA50.
       ZA02.
025430	     MOVE 11		 TO WS-PARKEY.
025440	     MOVE 1		 TO WS-SUB-2.
025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025490	     MOVE 1		 TO WS-SUB-1.
025500 ZA20-TRAN-LOOP.
025510	     MOVE PAR-T-CODE (WS-SUB-1)
				 TO W00-TRAN-CODE (WS-SUB-2).
025520	     MOVE PAR-E-DESC (WS-SUB-1)
				 TO W00-E-DESC (WS-SUB-2).
025530	     MOVE PAR-A-DESC (WS-SUB-1)
				 TO W00-A-DESC (WS-SUB-2).
025540	     MOVE PAR-ACTION (WS-SUB-1)
				 TO W00-ACTION (WS-SUB-2).
025550	     ADD 1		 TO WS-SUB-2.
025560     IF WS-SUB-1 < 2
025570	       ADD 1		 TO WS-SUB-1
025580         GO TO ZA20-TRAN-LOOP.
025590     IF WS-PARKEY < 60
025600	       ADD 1		 TO WS-PARKEY
025610         GO TO ZA15-READ-TRANS.
025620 ZA25.
	     MOVE 4		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-AGE	 TO WS-AGE.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830       GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 53
041860	       MOVE "STANDING ORDERS"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
025840 ZA200.
025850       DISPLAY "Files locked - Try later" AT 2312
025860                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
025870       DISPLAY "Press any key" AT 2512 
025880		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
025890	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 91.
      
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
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
056580	   ELSE
056590     IF WS-F-ERROR = 19
056600	       MOVE W02-SALE	 TO WS-FILE
056610	       MOVE WS-SALKEY	 TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
	   ELSE
041850	   IF WS-F-ERROR = 30
041860	       MOVE W02-ROUTE	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE RTE-ROUTE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 53
041860	       MOVE W02-DEBORD	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DOR-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
