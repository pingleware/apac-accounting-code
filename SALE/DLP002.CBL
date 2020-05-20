      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	    ****     ******    *
      *   **	**  **	      **    **	 **  **    **  **   **	  **   *
      *   **	**  **	      **    **	**    **  **	**	 **    *
      *   **	**  **	      *******	**    **  **	**     **      *
      *   **	**  **	      **	**    **  **	**   **	       *
      *   **	**  **	      **	 **  **    **  **   **	       *
      *   *******   ********  **	  ****	    ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E L I V E R Y   R O U T E   S C H E D U L E	       *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP002.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1994.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2010
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

000160 COPY DELSCH.SL.

000220 COPY RECOVER.SL.

       COPY ROUTE.SL.

000200	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       COPY VEHICLE.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DEBORD.FDE.

000290 COPY DELSCH.FDE.

000390 COPY RECOVER.FDE.

       COPY ROUTE.FDE.

000280 COPY VEHICLE.FDE.

      /
000330 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000680 01  REP-L1.
000710	   03  REP-DETAIL2     PIC X(96).
000700 01  REP-L2.
000690     03  REP-DETAIL1     PIC X(80).
000720 01  REP-L3.
000730     03  REP-AC          PIC X(10).
000740     03  REP-ACNO        PIC X(06).
           03  FILLER          PIC X(01).
000750     03  REP-NAME        PIC X(40).
           03  FILLER          PIC X(08).
000760     03  REP-DATE        PIC X(06).
           03  REP-TIME.
000770         05  REP-DTE     PIC Z9/99/99.
       01  REP-L3A.
           03  FILLER          PIC X(10).
           03  REP-TEL         PIC X(14).
           03  REP-CNTCT       PIC X(10).
           03  REP-CONTACT     PIC X(30).
           03  FILLER          PIC X(32).
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
002790 77  WS-SUB-1	   PIC 9(04)	 COMP-5.
002800 77  WS-SUB-2        PIC 9(04)     COMP-5.
002810 77  WS-SUB-3        PIC 9(04)     COMP-5.
002820 77  WS-SUB-4        PIC 9(04)     COMP-5.
002830 77  WS-SUB-5        PIC 9(04)     COMP-5.
002840 77  WS-SUB-6        PIC 9(04)     COMP-5.
002850 77  WS-IXS1         PIC 9(04)     COMP-5.
002860 77  WS-IXS2         PIC 9(04)     COMP-5.
002870 77  WS-IXS3         PIC 9(04)     COMP-5.
002880 77  WS-IXS4         PIC 9(04)     COMP-5.
002890 77  WS-MSUB         PIC 9(04)     COMP-5.
002900 77  WS-PARKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEPKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEBKEY       PIC 9(06)     COMP-5.
002930 77  WS-NETKEY       PIC 9(06)     COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
002950 77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
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
	   03  WS-PROG	       PIC  X(03) VALUE "002".

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

       COPY DELSCH.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY ROUTE.ID.

000360 COPY VEHICLE.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DLP002.".
               05  W02-USER   PIC X(05) VALUE SPACES.

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

003710 01  W10-SPARES.
003720     03  W10-DATE        PIC X(08).
           03  W10-RDATE REDEFINES W10-DATE.
               05  W10-DTE     PIC 99/99/99.

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

003930 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
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

       01  W31-DAYS.
           03  W31-DAY-NUMBERS.
               05  W31-DAYNO   PIC  X(36)    VALUE 
                               "000031059090120151181212243273303334".
               05  W31-DN REDEFINES W31-DAYNO.
		   07  W31-NO  PIC  9(03) OCCURS 12.
003260	   03  W31-DS	       PIC  9(02) COMP-5.
002860	   03  W31-RESULT      PIC  9(05)V99.
002870	   03  W31-RESULT1 REDEFINES W31-RESULT.
002880	       05  W31-WHOLE   PIC  9(05).
002890	       05  W31-DECIMAL PIC  9(02).
	   03  W31-WEEKS       PIC  9(05).

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

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

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
004020     03  W90-CS          PIC  9(02)    COMP-5.

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
005960	   03  FILLER	       PIC X(36)    VALUE
005970			     "DEBTOR ACCOUNTS: CONTRACT LISTING -".
005980	   03  W100-DESC       PIC X(28).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99-.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005990 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 34 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
				"ROUTE SCHEDULE".
	   03  LINE  4 COLUMN  4 VALUE "Route".

       01  S02.
	   02  S02A.
	   03  LINE  4 COLUMN 10 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(08) USING W30-ROUTE AUTO.
	   02  S02B.
	   03  LINE  4 COLUMN 20 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC X(40) USING RTE-DESC AUTO.


       COPY ROUTE.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-SALEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM DA000.
013530	     MOVE WS-NORM	 TO REP-DETAIL1.
013540	     WRITE REP-LINE1 BEFORE 0 LINES.
	     CLOSE DEBORD
		   DELSCH
		   RECOVER
		   PRNREP.

       AA49.
006950	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010460 AA50R.
010470	     MOVE "R"		 TO WS-LOOK.
	     GO TO AA50.

       AA50V.
	     MOVE "V"		 TO WS-LOOK.

010480 AA50.
011050	     PERFORM SAVE-SCREEN.
010740	   IF WS-LOOK = "R"
010750	       PERFORM ROUTE-LOOKUP
	   ELSE
	       PERFORM VEHICLE-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010900 AA99.
010910       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD1.
      
       COPY DEDORD.RD.

       COPY DELSCH.RD.

       COPY ROUTE.RD.

       COPY VEHICLE.RD.

       COPY APAC.HLP.

011610 COPY ROUTE.LUP.

       COPY VEHICLE.LUP.

      /
008930 CA155-GET-ROUTE	       SECTION 2.
008940 CA155-ROUTE.
012010	     MOVE W30-ROUTE	 TO RTE-ROUTE.
014330	     PERFORM READ-ROUTE THRU READ-ROUTE-EXIT.
	   IF WS-F-ERROR = 30
               MOVE 1            TO WS-ERROR
014380     ELSE
014390	   IF W30-ROUTE NOT = RTE-ROUTE
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420	       MOVE 0		 TO WS-ERROR.

009100 CA160-EXIT.
009110       EXIT.

009120 CA165-NAME	       SECTION 2.
009130 CA165-EXTRACT.
009140       MOVE ZERO TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180           MOVE 1 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.

      /
      *    ****    D E L I V E R Y   R O U T E	 S C H E D U L E
      *
037050 DA000	       SECTION 50.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
020330	     DISPLAY S01.

020360 DA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Route Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT S02A
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50R THRU AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE
	       DISPLAY S02A.
	   IF W30-ROUTE = SPACES
	       GO TO DA05.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W30-ROUTE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM CA155-GET-ROUTE.
020420	   IF WS-ERROR = 1
026950	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA05
	   ELSE
020450     IF WS-ERROR = 2
026950	       MOVE "Route File corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO DA05.
	     DISPLAY S02B.

       DA08.
036100	     MOVE "'C'ontinue or 'R'ekey  [C]"
				 TO WS-ERR-MES.
023210	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023300	   IF NOT (WS-OPTION = "C" OR "R")
023310	       GO TO DA08.
	   IF WS-OPTION = "R"
	       GO TO DA00.
	     INITIALIZE DOR-RECORD1.
	     MOVE W30-ROUTE	 TO DOR-ROUTE.

	     PERFORM START-AT-DORD-RKEY THRU READ-DEBORD-EXIT.
	   IF WS-F-ERROR = 53
	       MOVE "No details for this code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA00.


	     ACCEPT WS-OPTION AT 0443
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = SPACES
	       GO TO DA999.
018520	   IF NOT (WS-OPTION = "A" OR "R" OR "S")
	       GO TO DA05.
	     MOVE WS-OPTION	 TO WS-SEQ.
	   IF WS-SEQ = "A"
015140	       DISPLAY "Busy printing Rental & Service contracts"
			AT 0810
	       MOVE "RENTAL AND SERVICE AGREEMENTS"
				 TO W100-DESC
	   ELSE
	   IF WS-SEQ = "R"
015140	       DISPLAY "Busy printing Rental contracts" AT 0810
	       MOVE "RENTAL AGREEMENTS"
				 TO W100-DESC
	   ELSE
015140	       DISPLAY "Busy printing Service contracts" AT 0810
	       MOVE "SERVICE AGREEMENTS"
				 TO W100-DESC.
	     DISPLAY "Reading :" AT 1010.
011960	     MOVE WS-COND	 TO REP-DETAIL1.
011970	     WRITE REP-LINE1 BEFORE 0 LINES.

       DA10.
015350	     MOVE SPACES	 TO REP-DETAIL1.
015360	     MOVE "PAGE:"	 TO REP-PAGE.
015370	     MOVE "DATE:"	 TO REP-DATE.
015380	     ADD 1		 TO WS-PAGE.
015390	     MOVE W12-TODAY	 TO REP-DTE.
015400	     MOVE WS-PAGE	 TO REP-P-NO.
015440	     MOVE W100-HEAD	 TO REP-RESTART.
015450	     WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE WS-EXPP	 TO REP-EXPP.
015410	     MOVE W95-COMP	 TO REP-CO.
011760       WRITE REP-LINE1 BEFORE 0 LINES.
011760       WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-ECAN	 TO REP-LINE1.
015420       WRITE REP-LINE1 BEFORE 2 LINES.
015460	     MOVE SPACES	 TO REP-DETAIL1.
015470	     MOVE "AC-NO"	 TO REP-HEAD7.
	     MOVE "ACCOUNT NAME" TO REP-HEAD8.
	     MOVE "CONT-NO"	 TO REP-HEAD9.
	     MOVE "CONTRACT DESCRIPTION"
				 TO REP-HEAD10.
	     MOVE "   PAYMENT"	 TO REP-HEAD11.
	     MOVE "TYPE"	 TO REP-HEAD12.
	     MOVE "FREQ"	 TO REP-HEAD13.
	     MOVE "ESCAL"	 TO REP-HEAD14.
	     MOVE "ANV-DATE"	 TO REP-HEAD15.
	     MOVE " UNIT RATE"	 TO REP-HEAD16.
015480	     WRITE REP-LINE1 BEFORE 2 LINES.
015490       MOVE SPACES TO REP-DETAIL1.

015500 DA15.
	     MOVE " !"		 TO DBF-AC.
	     PERFORM START-AT-DBFT-KEY THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO DA25.

       DA20.
	     PERFORM READ-DBFOTO-NEXT THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO DA25.
	     DISPLAY DBF-AC AT 1020
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY DBF-CONT AT 1027
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF WS-SEQ = "R" OR "S"
	       IF NOT (WS-SEQ = DBF-TYPE)
		   GO TO DA20.
	   IF DBF-AC NOT = DEB-ACNO
	       MOVE DBF-AC	 TO DEB-ACNO
	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
	       MOVE DEB-ACNO	 TO REP-AC
015630	       INSPECT DEB-NAME REPLACING FIRST "*" BY ","
015640         INSPECT DEB-NAME REPLACING FIRST "*" BY " "
015650	       INSPECT DEB-NAME REPLACING FIRST "!" BY " "
	       MOVE DEB-NAME	 TO REP-NAME.
	     MOVE DBF-CONT	 TO REP-CONT.
	     MOVE DBF-DESC	 TO REP-DESC.
	     MOVE DBF-RENTAL	 TO REP-INST.
	   IF DBF-TYPE = "R"
	       MOVE "RENT"	 TO REP-TYPE
	   ELSE
	   IF DBF-TYPE = "S"
	       MOVE "SERV"	 TO REP-TYPE
	   ELSE
	       MOVE "OTHR"	 TO REP-TYPE.
	   IF DBF-FREQ = "1"
	       MOVE "Mthly"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "2"
	       MOVE "2 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "3"
	       MOVE "3 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "4"
	       MOVE "4 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "6"
	       MOVE "6 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "A"
	       MOVE "Annual"	 TO REP-FREQ.
	     MOVE DBF-ESC	 TO REP-ESC.
	     MOVE DBF-ADATE	 TO REP-ADATE.
	     MOVE DBF-RATE	 TO REP-RATE.
016020	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
016030	       WRITE REP-LINE1 BEFORE 1 LINES
016040     ELSE
016050	       WRITE REP-LINE1 BEFORE PAGE
016060	       PERFORM DA10.
015860       MOVE SPACES TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
                                 TO REP-DETAIL1
014980         WRITE REP-LINE1 BEFORE PAGE
014990	       GO TO DA999.
	     GO TO DA20.

       DA25.
	     WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE "****    E N D   O F   R E P O R T   ****"
				 TO REP-CO.
	     WRITE REP-LINE1 BEFORE PAGE.

       DA999.
	     EXIT.

      /
025020 ZA00-INIT	      SECTION 9.
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
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.

025110       MOVE 3              TO WS-PARKEY.
025120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025140	     MOVE WS-PTH15	 TO W02-PTH42.
044300	     OPEN INPUT DBFOTO.
044310	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 42	 TO WS-F-ERROR
                   GO TO ZA50.
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
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN4 (WS-S6)
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
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
       ZA02.
025430	     MOVE 11		 TO WS-PARKEY.
025440	     MOVE 1		 TO WS-SUB-2.

025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

025620 ZA25.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
      *
      *    ****    C A L C U L A T E   D A Y   O F   W E E K
      *
      *      THIS ROUTINE CALCULATES THE DAY OF THE WEEK AND STARTS
      *      THE WEEK WITH A SUNDAY (1) AND ENDS WITH SATURDAY (7).
      *
      *      NUMBER OF DAYS FROM 1900 TO START OF CURRENT YEAR
      *
      *      W12-TODAY (W12-DAY/W12-MONTH/W12-YEAR) CONTAINS THE
      *      CURRENT DATE.
      *
	     COMPUTE W31-RESULT = W12-YEAR * 365.25.
      *
      *      ADD NUMBER OF DAYS FROM START OF CURRENT YEAR TO START
      *      OF CURRENT MONTH
      * 	JAN=000, FEB=031, MAR=059, APR=090, MAY=120, JUN=151,
      * 	JUL=181, AUG=212, SEP=243, OCT=273, NOV=303, DEC=334.
      *
	     ADD W31-NO (W12-MONTH)
				 TO W31-RESULT.
      *
      *      ADD NUMBER OF DAYS FROM START OF CURRENT MONTH
      *
	     ADD W12-DAY	 TO W31-RESULT.
      *
      *      IF THIS IS A LEAP YEAR AND THE MONTH IS > 2 ADD THE
      *      EXTRA DAY.
      *
	   IF W31-DEC = ZERO
	       IF W12-MONTH > 2
		   ADD 1	 TO W31-RESULT.
	     DIVIDE W31-WHOLE BY 7
				 GIVING W31-WEEKS
				 REMAINDER W31-DS.
      *
      *    AS SATURDAYS ARE DIVISABLE BY SEVEN THE REMAINDER WILL BE
      *    ZERO AND SHOULD BE CHANGED TO SEVEN.
      *
	   IF W31-DS = ZERO
	       MOVE 7		 TO W31-DS.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
025820	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
025830	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 42
041860	       MOVE "RENTAL file"
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

       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 9.
      
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
041840	   ELSE
041850	   IF WS-F-ERROR = 42
041860	       MOVE W02-DBFOTO	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DBF-KEY	 TO WS-KEYX.
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
