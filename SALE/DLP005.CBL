      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	    ****    ********   *
      *   **	**  **	      **    **	 **  **    **  **   **	       *
      *   **	**  **	      **    **	**    **  **	**  *******    *
      *   **	**  **	      *******	**    **  **	**	  **   *
      *   **	**  **	      **	**    **  **	**	  **   *
      *   **	**  **	      **	 **  **    **  **   **	  **   *
      *   *******   ********  **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P O S T   C O N T R A C T	T R A N S A C T I O N S        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP005.
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

000160 COPY DBFOTO.SL.

000160 COPY DBTRAN.SL.

000140 COPY DEBTOR.ISL.

       COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY TXPTRN.SL.

       COPY TXTRAN.SL.

000240     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000290 COPY DBFOTO.FDE.

000290 COPY DBTRAN.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000250 COPY PARAM.FDE.

000390 COPY RECOVER.FDE.

       COPY SHARED.FDE.

       COPY TXPTRN.FDE.

       COPY TXTRAN.FDE.

      /
000410 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000680 01  REP-LINE1.
000690	   03  REP-DETAIL1     PIC X(132).
000700 01  REP-L1.
000710	   03  FILLER	       PIC X(12).
000720	   03  BAL-REP	       PIC X(108).
000730	   03  FILLER	       PIC X(12).
000740 01  REP-LINE2.
000750	   03  REP-DETAIL2     PIC X(132).
000910 01  INV-L1.
	   03  INV-EXP	       PIC  X(01).
000920	   03  FILLER	       PIC  X(03).
	   03  INV-EXPNAME     PIC  X(40).
	   03  FILLER	       PIC  X(88).
       01  INV-L2.
	   03  FILLER	       PIC  X(06).
000930	   03  INV-NADD        PIC  X(44).
000940     03  INV-RADD      REDEFINES INV-NADD.
000950	       05  INV-PC      PIC  X(04).
000960	       05  FILLER      PIC  X(40).
	   03  FILLER	       PIC  X(40).
001120	   03  INV-H2	       PIC  X(16).
001130	   03  INV-TEL	       PIC  X(14).
	   03  INV-GNO REDEFINES INV-TEL
			       PIC  X(14).
	   03  FILLER	       PIC  X(12).
000980 01  INV-L3.
000990	   03  FILLER	       PIC  X(06).
001000	   03  INV-NAME        PIC  X(40).
001010     03  INV-DADD      REDEFINES INV-NAME.
001020	       05  INV-PC1     PIC  X(04).
001030	       05  FILLER      PIC  X(36).
001040	   03  FILLER	       PIC  X(06).
	   03  INV-H3	       PIC  X(08).
001060	   03  INV-DADD2.
001070	       05  INV-PC2     PIC  X(30).
001090	   03  FILLER	       PIC  X(10).
	   03  INV-H4	       PIC  X(19).
001550	   03  INV-ORD.
	       05  INV-TERMS.
		   07  FILLER  PIC  X(05).
		   07  INV-AC.
		       09  FILLER  PIC	X(03).
		       09  INV-PGE PIC	ZZ9.
	       05  FILLER      PIC  X(01).
	   03  FILLER	       PIC  X(01).
001100 01  INV-L4.
001110	   03  FILLER	       PIC  X(01).
001490	   03  INV-H5	       PIC  X(17).
001140	   03  INV-H6	       PIC  X(32).
001510	   03  INV-H7	       PIC  X(14).
	   03  INV-H8	       PIC  X(13).
	   03  INV-H9	       PIC  X(13).
	   03  INV-H10	       PIC  X(15).
	   03  INV-H11	       PIC  X(14).
	   03  INV-H12	       PIC  X(13).
001150 01  INV-L5.
001750	   03  FILLER	       PIC  X(01).
001780	   03  INV-ITM	       PIC  X(14).
	   03  FILLER	       PIC  X(03).
001790	   03  INV-MES.
001810	       05  INV-DESC    PIC  X(30).
	   03  FILLER	       PIC  X(03).
001760	   03  INV-QNT	       PIC  Z(08)9.
001770	   03  FILLER	       PIC  X(02).
001930	   03  INV-PRICE       PIC  Z(05)9.999-.
	   03  FILLER	       PIC  X(04).
001850	   03  INV-DSC	       PIC  Z9.99.
	   03  FILLER	       PIC  X(04).
	   03  INV-EXTEND      PIC  Z(08)9.99-.
	   03  FILLER	       PIC  X(02).
001910	   03  INV-TAX	       PIC  Z(06)9.99- BLANK WHEN ZERO.
001940	   03  FILLER	       PIC  X(03).
001950	   03  INV-VAL	       PIC  Z(09)9.99-.
001960	   03  FILLER	       PIC  X(01).
       01  INV-L6.
	   03  FILLER	       PIC  X(09).
           03  INV-HD          PIC  X(10).
           03  INV-SERNOS.
               05  FILLER      PIC  X(01).
               05  INV-SERNO   PIC  X(10).
	       03  FILLER      PIC  X(25).
	   03  FILLER	       PIC  X(77).
001970 01  INV-L7.
001980	   03  FILLER	       PIC  X(18).
001990	   03  INV-REMKS.
002010	       05  INV-RMKS.
		 07  FILLER    PIC  X(40).
002020	   03  FILLER	       PIC  X(15).
	   03  INV-H13	       PIC  X(12).
	   03  FILLER	       PIC  X(01).
	   03  INV-LINE        PIC  X(45).
	   03  FILLER	       PIC  X(01).
       01  INV-L8.
	   03  INV-EXP2        PIC  X(01).
001540	   03  FILLER	       PIC  X(17).
	   03  INV-INVOICE.
	       05  FILLER      PIC  X(03).
	       05  INV-CRNOTE.
		   07  FILLER  PIC  X(06).
		   07  INV-COPY
			       PIC  X(12).
		   07  FILLER  PIC  X(06).
	       05  FILLER      PIC  X(03).
	   03  FILLER	       PIC  X(03).
	   03  INV-H14	       PIC  X(08).
	   03  INV-REF.
	       05  INV-DTE     PIC  99/99/99.
000430 01  R-L1.
000440     03  R-DET1          PIC  X(132).
000450 01  R-L2.
000460     03  R-DET2          PIC  X(80).
000470 01  R-L3.
000530     03  R-DATE          PIC  X(06).
000540     03  R-DTE           PIC  Z9/99/99.
000500     03  FILLER          PIC  X(06).
000770     03  R-H5            PIC  X(46).
000800     03  R-H6            PIC  X(07).
000780     03  R-BATCH         PIC  Z(05)9.
000520     03  FILLER          PIC  X(07).
000480     03  R-PAGE          PIC  X(06).
000490     03  R-P-NO          PIC  Z(03)9.
       01  R-L3A.
           03  FILLER          PIC  X(04).
           03  R-CO            PIC  X(40).
           03  FILLER          PIC  X(04).
000550 01  R-L4.
000560     03  FILLER          PIC  X(11).
000570     03  R-H1            PIC  X(04).
000580     03  FILLER          PIC  X(06).
000590     03  R-H2            PIC  X(15).
000600     03  FILLER          PIC  X(06).
000610     03  R-H3            PIC  X(15).
000620     03  FILLER          PIC  X(06).
000630     03  R-H4            PIC  X(06).
000640     03  FILLER          PIC  X(11).
000650 01  R-L5.
000660     03  FILLER          PIC  X(12).
000670     03  R-TRAN          PIC  9(02).
000680     03  FILLER          PIC  X(09).
000690     03  R-ENG           PIC  X(12).
000700     03  FILLER          PIC  X(09).
000710     03  R-AFR           PIC  X(12).
000720     03  FILLER          PIC  X(10).
000730     03  R-ACT           PIC  X(01).
000740     03  FILLER          PIC  X(13).
000820 01  R-L7.
000830     03  R-H7            PIC  X(07).
000840     03  R-H7A           PIC  X(27).
000850     03  R-H8            PIC  X(07).
000860     03  R-H10           PIC  X(09).
000870	   03  R-H11	       PIC  X(11).
000880	   03  R-H12	       PIC  X(15).
000890	   03  R-H13	       PIC  X(10).
000890	   03  R-H14	       PIC  X(10).
000900 01  R-L8.
000910     03  R-TAC           PIC  X(06).
000920     03  FILLER          PIC  X(01).
000930     03  R-DNME          PIC  X(24).
000940     03  FILLER          PIC  X(01).
000950     03  R-TDTE          PIC  X(08).
000960     03  FILLER          PIC  X(01).
000990     03  R-TDESC         PIC  X(08).
001000     03  FILLER          PIC  X(01).
001010     03  R-TREF          PIC  X(08).
001020     03  FILLER          PIC  X(01).
001030     03  R-VAL           PIC  Z(07)9.99-.
001020     03  FILLER          PIC  X(01).
           03  R-TAX           PIC  Z(06)9.99-.
001020     03  FILLER          PIC  X(01).
001040     03  R-TOT           PIC  Z(07)9.99-.
001050 01  R-L9.
001060     03  FILLER          PIC  X(54).
001070     03  R-BTOT          PIC  Z(06)9.99-.
001080     03  FILLER          PIC  X(15).
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
002790 77  WS-SUB	   PIC 9(04)	 COMP-5.
002790 77  WS-SUB-1        PIC 9(04)     COMP-5.
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
002930 77  WS-NETKEY	   PIC 9(06)	 COMP-5.
002930 77  WS-RECKEY	   PIC 9(06)	 COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
001720 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
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
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
003150 77  WS-CHAR1	   PIC X(01).
003160 77  WS-CHAR2        PIC X(01).
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-IND	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003210 77  WS-INDEX-STORE  PIC X(04).
003220 77  WS-INDEX-NAME   PIC X(04).
003230 77  WS-BATCH        PIC 9(05).
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
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
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
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

       COPY LEDTRF.WS.

002250 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC  9(09)V99 COMP-3.
001410     03  W00-E-DESC  PIC  X(12).
001420     03  W00-A-DESC  PIC  X(12).
001430     03  W00-ACTION  PIC  9(01).
001440     03  W00-T-CODE  PIC  9(02)    COMP-5.
001450     03  W00-T-GL    PIC  9(06)    COMP-3.
001460     03  W00-T-DAY   PIC  9(06)    COMP-5.
001470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-T-MTD   PIC  9(06)    COMP-5.
001490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-T-YTD   PIC  9(06)    COMP-5.
001510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

       COPY DEPART.ID.

       COPY DBTRAN.ID.

       COPY DBFOTO.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY TXTRAN.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DTP035.".
               05  W02-USER   PIC X(05) VALUE SPACES.

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

       01  W05-TAX-RATES.
000760     03  W05-VAT-RATES.
000770         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
000780     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
000790         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
000800         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
000810         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
000820         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
000830         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
000840         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
000630     03  W05-DISCOUNT.
000640         05  W05-DSETT   PIC S9(03)V99 COMP-3 OCCURS 10.
002340	   03  W05-RATE        PIC S9(03)V99 COMP-3.
002350     03  W05-RTE REDEFINES W05-RATE
002360                         PIC S9V9(04)  COMP-3.
004050     03  W05-VATNO       PIC  X(14).

003710 01  W10-SPARES.
003720     03  W10-DATE        PIC X(08).
           03  W10-RDATE REDEFINES W10-DATE.
               05  W10-DTE     PIC 99/99/99.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

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

002730 01  W22-DATES.
002740     03  W22-DTE1        PIC 9(06).
002750     03  W22-DTE2        PIC 9(06).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
002770         05  W22-YY2     PIC 9(02).
002780         05  W22-MM2     PIC 9(02).
002790         05  W22-DD2     PIC 9(02).
002800     03  W22-DTE3        PIC 9(06).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820         05  W22-DD3     PIC 9(02).
002830         05  W22-MM3     PIC 9(02).
002840         05  W22-YY3     PIC 9(02).

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER      PIC  X(09) VALUE " January ".
               05  FILLER      PIC  X(09) VALUE " February".
               05  FILLER      PIC  X(09) VALUE "  March  ". 
               05  FILLER      PIC  X(09) VALUE "  April  ". 
	       05  FILLER      PIC  X(09) VALUE "   May   ".
               05  FILLER      PIC  X(09) VALUE "   June  ".
               05  FILLER      PIC  X(09) VALUE "   July  ".
               05  FILLER      PIC  X(09) VALUE "  August ".
               05  FILLER      PIC  X(09) VALUE "September".
               05  FILLER      PIC  X(09) VALUE " October ".
               05  FILLER      PIC  X(09) VALUE " November".
               05  FILLER      PIC  X(09) VALUE " December".
           03  W24-EACH-MONTH REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH   PIC  X(09) OCCURS 12.
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

000030 01  W30-DBFOTO.
	   03  W30-KEY.
000170	       05  W30-AC      PIC  X(06).
	       05  W30-CONT    PIC  X(08).
	   03  W30-DESC        PIC  X(30).
000060	   03  W30-DATE        PIC  9(06)     COMP-3.
	   03  W30-RENTAL      PIC S9(07)V99  COMP-3.
	   03  W30-TYPE        PIC  X(01).
	   03  W30-FREQ        PIC  X(01).
	   03  W30-ADATE       PIC  9(06)     COMP-3.
	   03  W30-ESC	       PIC  9(03)V99  COMP-3.
	   03  W30-RATE        PIC S9(06)V999 COMP-3.
000110	   03  W30-RDTE        PIC  9(06)     COMP-3.
000100	   03  W30-STRDNG      PIC  9(08)     COMP-5.
000100	   03  W30-LSTRDNG     PIC  9(08)     COMP-5.
000100	   03  W30-READING     PIC  9(08)     COMP-5.
000120	   03  W30-VALUE       PIC S9(07)V99  COMP-3.
000120	   03  W30-TAX	       PIC S9(05)V99  COMP-3.
	   03  W30-TCODE       PIC  X(01)     COMP-X.
	   03  W30-VG.
	       05  W30-VAT     PIC  9(01).

       01  W30-AGE-DATES.
           03  W30-120         PIC  9(06) COMP-3.
           03  W30-90          PIC  9(06) COMP-3.
           03  W30-60          PIC  9(06) COMP-3.
           03  W30-30          PIC  9(06) COMP-3.
           03  W30-CUR         PIC  9(06) COMP-3.

004760 01  W40-COMPANY.
002930	   03  W40-NAME1.
002940         05  W40-NAME.
002950             07  W40-ENTRY PIC X(08).
002960             07  FILLER  PIC X(32).
002970         05  FILLER      PIC X(32).
002980     03  W40-NAME2 REDEFINES W40-NAME1.
002990         05  W40-SURNAME PIC X(24).
003000         05  W40-FNAME   PIC X(24).
003010         05  W40-SNAME   PIC X(24).
003020     03  W40-NAME3 REDEFINES W40-NAME1.
003030         05  W40-NCHAR OCCURS 72 PIC X(01).
003040     03  W40-ANAME       PIC X(40).
           03  W40-CONTACT     PIC  X(30).
003050     03  W40-DEBTREC.
               05  W40-ACNO      PIC X(06).
003060         05  FILLER        PIC X(456).
003070     03  W40-ADD REDEFINES W40-DEBTREC.
003080       05  W40-ADDRESS1.
003090         07  W40-ADD1    PIC X(30).
003100         07  W40-ADD2    PIC X(30).
003110         07  W40-ADD3    PIC X(30).
003120         07  W40-ADD4    PIC X(30).
003130       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140         07  W40-ACHAR OCCURS 120 PIC X(01).
005160	     05  W40-PC1       PIC  X(04).
003150       05  W40-PADDRESS.
003160         07  W40-PADD1   PIC X(30).
003170         07  W40-PADD2   PIC X(30).
003180         07  W40-PADD3   PIC X(30).
003190         07  W40-PADD4   PIC X(30).
003200       05  W40-PADDR2   REDEFINES W40-PADDRESS.
003210         07  W40-PCHAR OCCURS 120 PIC X(01).
005240	     05  W40-PC2       PIC  X(04).
             05  W40-SPACES    PIC X(30).
             05  W40-TOP.
               07  W40-T1      PIC  X(01).
               07  W40-T2-27   PIC  X(26).
               07  W40-T28     PIC  X(01).
             05  W40-MID.
               07  W40-M1      PIC  X(01).
               07  W40-M2-27   PIC  X(26).
               07  W40-M28     PIC  X(01).
             05  W40-BOT.
               07  W40-B1      PIC  X(01).
               07  W40-B2-27   PIC  X(26).
               07  W40-B28     PIC  X(01).
003220       05  FILLER        PIC X(111).
003230     03  W40-POST        PIC 9(04).
003240     03  W40-PPOST       PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-HTEL        PIC X(14).
003260     03  W40-FAX         PIC X(14).
003270     03  W40-ID.
003280         05  W40-IDNO    PIC X(14).
003290     03  W40-TAX REDEFINES W40-ID.
003300         05  W40-TAXNO   PIC X(14).
003310     03  W40-TITLE       PIC 9(02).
003320     03  W40-LANG        PIC X(01).
003330     03  W40-CAT         PIC 9(02).
003330     03  W40-OCAT        PIC 9(02).
003340     03  W40-LEDG        PIC 9(03).
	   03  W40-PRICE       PIC X(01).
003350	   03  W40-FIN	       PIC X(01).
003360     03  W40-CRL         PIC S9(07)    COMP-3.
003370     03  W40-ONE         PIC S9(09)V99 COMP-3.
003380     03  W40-MTD         PIC S9(09)V99 COMP-3.
003390     03  W40-CUR         PIC S9(09)V99 COMP-3.
003400     03  W40-30          PIC S9(09)V99 COMP-3.
003410     03  W40-60          PIC S9(09)V99 COMP-3.
003420     03  W40-90          PIC S9(09)V99 COMP-3.
003420     03  W40-120         PIC S9(09)V99 COMP-3.
003430     03  W40-INT         PIC S9(09)V99 COMP-3.   
003440     03  W40-TERMS       PIC X(01).  
003440     03  W40-INV-TYPE    PIC X(01).  
003450     03  W40-SDISC       PIC 9(02). 
003450     03  W40-DISC        PIC 9(02). 
003460     03  W40-SMAN        PIC 9(02). 
           03  W40-LAB         PIC  X(01).
           03  W40-OI          PIC  X(01).
           03  W40-OOI         PIC  X(01).

       COPY W40.WS.

005020 01  W45-TRAN.
005030     03  W45-CODE        PIC 9(02).
005040     03  W45-ENG         PIC X(12).
005050     03  W45-AFR         PIC X(12).
005060     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

005170 COPY W50.WS.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).
003600	   03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
	   03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
	   03  W75-DNO	      PIC X(06) OCCURS 18.

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
005520	   03  W80-TYPE        PIC 9(01).

005540 01  W85-BATCHES.
005550         05  W85-BATCH   PIC 9(05).

003980 01  W90-TRAN.
003990	   03  W90-DATE        PIC  9(06)    COMP-3.
004000     03  W90-DTE         PIC  99/99/99.
004020     03  W90-CODE        PIC  9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.
004030     03  W90-REF         PIC  X(08).
004040     03  W90-DB          PIC S9(09)V99 COMP-3.
004050     03  W90-CR          PIC S9(09)V99 COMP-3.
004040     03  W90-ALLOCATED   PIC S9(09)V99 COMP-3.
004040     03  W90-DEBIT       PIC S9(09)V99 COMP-3.
004050     03  W90-CREDIT      PIC S9(09)V99 COMP-3.
004050     03  W90-VATTOT      PIC S9(09)V99 COMP-3.
004040     03  W90-VAL         PIC S9(09)V99 COMP-3.
004040     03  W90-VAT         PIC S9(07)V99 COMP-3.
004060     03  W90-BDT         PIC S9(09)V99 COMP-3.
004070     03  W90-BCR         PIC S9(09)V99 COMP-3.
004080     03  W90-DESC        PIC  X(12).
004090     03  W90-120         PIC S9(09)V99 COMP-3.
004090     03  W90-90          PIC S9(09)V99 COMP-3.
004100     03  W90-60          PIC S9(09)V99 COMP-3.
004110     03  W90-30          PIC S9(09)V99 COMP-3.
004120     03  W90-CUR         PIC S9(09)V99 COMP-3.
004130     03  W90-BAL         PIC S9(09)V99 COMP-3.
004300     03  W90-PER         PIC S9(09)V99 COMP-3.
004310     03  W90-GST         PIC S9(09)V99 COMP-3.
004320     03  W90-DISC        PIC S9(03)V99 COMP-3.
004320     03  W90-GV          PIC S9(09)V99 COMP-3.
004330     03  W90-DS          PIC S9(09)V99 COMP-3.
004340     03  W90-CAP         PIC S9(09)V99 COMP-3.
004350     03  W90-EXP         PIC S9(09)V99 COMP-3.
004360     03  W90-TAX         PIC S9(09)V99 COMP-3.
004370     03  W90-IDTE        PIC 9(06)     COMP-3.
004380     03  W90-INT         PIC S9(09)V99 COMP-3.
004340     03  W90-TCAP        PIC S9(09)V99 COMP-3.
004350     03  W90-TEXP        PIC S9(09)V99 COMP-3.
004360     03  W90-TTAX        PIC S9(09)V99 COMP-3.
004060     03  W90-TBDT        PIC S9(09)V99 COMP-3.
004070     03  W90-TBCR        PIC S9(09)V99 COMP-3.
	   03  W90-PRICE       PIC S9(08)V999 COMP-3.
	   03  W90-UNITS       PIC  9(08)    COMP-3.
	   03  W90-DEXC        PIC S9(09)V99 COMP-3.
	   03  W90-DTAX        PIC S9(09)V99 COMP-3.
	   03  W90-DTOT        PIC S9(09)V99 COMP-3.

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).
005830     03  W95-ADD1        PIC X(30).
005840     03  W95-ADD2        PIC X(30).
005850     03  W95-ADD3        PIC X(30).
005860     03  W95-ADD4        PIC X(30).
005870	   03  W95-EDIT        REDEFINES W95-ADD4.
005880         05  W95-V1.
005890             07  W95-T   PIC Z(04)9.99.
005900         05  W95-V2.
005910             07  W95-B   PIC Z(06)9.99-.
005920         05  FILLER      PIC X(11).
006920	   03  W95-PC.
006930         05  W95-POST    PIC 9(04).
006940     03  W95-TEL         PIC X(14).

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

007640 01  S27.
007650     03  LINE  5 COLUMN 12 FOREGROUND-COLOR 15 BACKGROUND-COLOR 4
007660                           VALUE "This routine does not include a
007670-                          "recovery procedure. Security".
007680     03  LINE  6 COLUMN 12 FOREGROUND-COLOR 15 BACKGROUND-COLOR 4
007690                           VALUE "copies  of the data files should
007700-                          " be made as a power  failure".
007710     03  LINE  7 COLUMN 12 FOREGROUND-COLOR 15 BACKGROUND-COLOR 4
007720                           VALUE "or any other problem during this
007730-                          " procedure would require the".
007740     03  LINE  8 COLUMN 12 FOREGROUND-COLOR 15 BACKGROUND-COLOR 4
007750                           VALUE "DATA FILES from the previous bac
007760-                          "kup to be restored !!!      ".
007770     03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM DA000.
013140	     MOVE WS-10CPI	 TO REP-DETAIL2.
013150       WRITE REP-LINE2 BEFORE 0 LINES.
	     CLOSE DBFOTO
		   RECOVER
		   PRNREP.

       AA49.
006950	     EXIT PROGRAM.

       COPY DATE.CHK.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY DEBTOR.TRN.

       COPY ABORT.PRN.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DEBTOR.RD.
      
       COPY DBTRAN.RD.

       COPY DBFOTO.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY DBFOTO.WR.

       COPY DBTRAN.WR.

       COPY DEBTOR.WR.

       COPY PARAM.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY TXPTRN.WR.

       COPY TXTRAN.WR.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.
010970 AY02.
010980       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000	     MOVE DEB-RECORD1	 TO REC-DEBTOR.
011010       GO TO AY50.
011070 AY04.
011080       MOVE 04             TO REC-FILE.
011090       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
011100       MOVE TRA-RECORD1    TO REC-DBTRAN.
011110       GO TO AY50.
       AY18.
	     MOVE 18		 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
	     MOVE TXP-RECORD1	 TO REC-TXPTRN.
             GO TO AY50.
       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.
       AY38.
             MOVE 38             TO REC-FILE.
             MOVE WS-DEPKEY      TO REC-KEY.
             MOVE DPT-RECORD     TO REC-DEPART.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
010970 AY42.
010980	     MOVE 42		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011000	     MOVE DBF-RECORD1	 TO REC-DBFOTO.
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
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
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
      /
017690 BB050	       SECTION.
017700 BB50.
017710       MOVE SPACES         TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 2 LINES.
	     MOVE X"0E" 	 TO INV-EXP2.
	     MOVE "TAX INVOICE/BELASTING FAKTUUR"
				 TO INV-INVOICE.
	     WRITE INV-L1 BEFORE 0 LINES.
	     WRITE INV-L1 BEFORE 2 LINES.
017710       MOVE SPACES         TO REP-DETAIL2.
	     MOVE X"0E" 	 TO INV-EXP2.
	     MOVE "  RENTAL,SERVICE/HUUR,DIENS"
				 TO INV-INVOICE.
	     WRITE INV-L1 BEFORE 0 LINES.
	     WRITE INV-L1 BEFORE 2 LINES.
017710       MOVE SPACES         TO REP-DETAIL2.
	     MOVE X"0E" 	 TO INV-EXP.
017750	     MOVE W95-COMP	 TO INV-EXPNAME.
017760	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE WS-ECAN	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
017770	     MOVE W95-ADD1	 TO INV-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO INV-H2
026710         MOVE W05-VATNO    TO INV-GNO.
017780       WRITE INV-L1 BEFORE 1 LINES.
017790       MOVE SPACES         TO REP-DETAIL2.
017800       MOVE W95-ADD2       TO INV-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO INV-H2
017930         MOVE W95-TEL      TO INV-TEL.
017810       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
017820       MOVE W95-ADD3       TO INV-NADD.
017830       WRITE INV-L1 BEFORE 1 LINES.
017840       MOVE W95-ADD4       TO INV-NADD.
017850	     WRITE INV-L1 BEFORE 1 LINES.
017860 BB55.
017870       MOVE SPACES         TO REP-DETAIL2.
017880       MOVE W95-PC         TO INV-PC.
017890	     WRITE INV-L1 BEFORE 2 LINES.
017900       MOVE SPACES         TO REP-DETAIL2.
	     MOVE "All items appearing on this invoice will be shown on
      - 	  "the statement as they are for individual contracts"
				 TO BAL-REP.
017890	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE "Alle items wat op hierdie faktuur verskyn sal ook op
      -	       "die staat verskyn omdat hulle verskulende kontrakte is"
				 TO BAL-REP.
017890	     WRITE INV-L1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.

017950 BB60.
017960       EXIT.
      /
       BC000	       SECTION 2.
       BC189.
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
       BC215.
	       EXIT.

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

014150 CA185-ADDRESS	 SECTION 2.
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

       COPY DEBTOR.TRA.

       COPY LEDTRF.UPD.

      /
      *    ****    P O S T   R E N T A L   &   S E R V I C E   T R A N S
      *
037050 DA000	       SECTION 5.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
	     DISPLAY "POST RENTAL AND SERVICE CONTRACT DEBITS" AT 0221
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
024780	     DISPLAY S27.
024790 DA05.
024800	     ACCEPT WS-OPTION AT 1043
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024860	   IF NOT (WS-OPTION = "Y" OR "N")
024870	       GO TO DA05.
024880     IF WS-OPTION = "N"
024890	       GO TO DA999.
	     DISPLAY "Processing :" AT 1212.
             MOVE ZERO           TO W90-DEBIT W90-CREDIT W90-VATTOT.
026480	     MOVE ZERO		 TO W90-BDT W90-30
026510                              W90-BCR W90-60 
                                    W90-CUR W90-90 
                                    W90-INT W90-DISC
                                    W90-120 W40-SDISC.
       DA08.
	     MOVE WS-17CPI	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 0 LINES.
026570 DA10.
	     MOVE SPACES	 TO W80-ACNO DEB-ACNO.
026830       MOVE ZERO           TO W90-CODE.
026840	     MOVE W12-TODAY	 TO W90-DTE W90-DATE W10-DTE.
	     PERFORM CHECK-DATE.
	   IF W22-DTE1 < WS-VAT-DATE
	       MOVE 2		 TO WS-VAT-SUB
	   ELSE
	       MOVE 1		 TO WS-VAT-SUB.

026850 DA15.
026860       MOVE ZERO           TO W90-DB W90-CR W90-VAT W90-EXP.
026870       MOVE SPACES         TO W90-REF.
	     MOVE " !"		 TO DBF-AC
				    DBF-CONT.
	     PERFORM START-AT-DBFT-KEY THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO DA999.

       DA20.
	     PERFORM READ-DBFOTO-NEXT THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       IF NOT (W80-ACNO = SPACES)
		   PERFORM DA50
	       END-IF
	       GO TO DA100.
	     MOVE DBF-RECORD1	 TO W30-DBFOTO.
	   IF NOT (W30-VG = "0" OR "1")
	       MOVE 1		 TO W30-VAT.
	     MOVE W30-RDTE	 TO W15-START.
	     MOVE W15-SDAY	 TO W15-SDD.
	     MOVE W15-SMNTH	 TO W15-SMM.
	     MOVE W15-SYEAR	 TO W15-SYY.
	   IF (W12-T-YMD < W15-S-YMD) AND
	      ((W12-MM < W15-SMM) OR (W12-YY < W15-SYY))
	       GO TO DA20.
	      DISPLAY DBF-AC AT 1225
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	      DISPLAY DBF-CONT AT 1425
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	   IF DBF-AC = DEB-ACNO
	       GO TO DA25.
	   IF NOT (W80-ACNO = SPACES)
	       PERFORM DA50.
026560	     MOVE 0		 TO WS-PAGE.
	     MOVE ZERO		 TO W90-DEXC W90-DTAX W90-DTOT.
	     MOVE DBF-AC	 TO W80-ACNO DEB-ACNO.
	     PERFORM CA155-GET-DEBTOR.
	   IF WS-ERROR = 1 OR 2
	       GO TO DA20.
	     MOVE DEB-ACNO	 TO W40-ACNO.
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
027060	     DISPLAY W40-NAME AT 1232
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF W22-DTE1 < WS-VAT-DATE
	       MOVE 2		 TO WS-VAT-SUB
	   ELSE
	       MOVE 1		 TO WS-VAT-SUB.
	     PERFORM BC000.
	     PERFORM BB050.
	     PERFORM DA40.

       DA25.
	     MOVE ZERO		 TO W30-VALUE W30-TAX.
	     MOVE W30-TCODE	 TO W90-CODE.
	     PERFORM GET-DEBTOR-TRAN-DESC.
027560     IF W00-T-CODE = ZERO
027570	       GO TO DA20.
027580       MOVE W00-E-DESC     TO W90-DESC.
	     MOVE W30-CONT	 TO W90-REF.
	   IF W30-RENTAL > ZERO
	       MOVE W30-RENTAL	 TO W90-VAL W90-PRICE
	       MOVE 1		 TO W90-UNITS
	       PERFORM CG000
	       ADD W90-DB	 TO W30-VALUE W90-DTOT
	       ADD W90-VAT	 TO W30-TAX W90-DTAX
	       ADD W90-VAL	 TO W90-DEXC.
	   IF W30-READING = ZERO
	       GO TO DA35.
	     SUBTRACT W30-LSTRDNG FROM W30-READING
				 GIVING W90-UNITS.
	     COMPUTE W90-VAL ROUNDED = W30-RATE * W90-UNITS.
	   IF W90-VAL = ZERO
	       GO TO DA35.
	     MOVE W30-RATE	 TO W90-PRICE.
	     PERFORM CG000.
	     ADD W90-DB 	 TO W30-VALUE W90-DTOT.
	     ADD W90-VAT	 TO W30-TAX W90-DTAX.
	     ADD W90-VAL	 TO W90-DEXC.

       DA35.
	     MOVE W30-RDTE	 TO W15-START.
	   IF W30-FREQ = "A"
	       ADD 1		 TO W15-SYEAR
	   ELSE
	       MOVE W30-FREQ	 TO WS-NTP
	       ADD WS-NTP	 TO W15-SMNTH.
	   IF W15-SMNTH > 12
	       SUBTRACT 12	 FROM W15-SMNTH
	       ADD 1		 TO W15-SYEAR.
	     MOVE W15-START	 TO W30-RDTE.
	     MOVE W30-ADATE	 TO W15-START.
	     MOVE W15-SDAY	 TO W15-SDD.
	     MOVE W15-SMNTH	 TO W15-SMM.
	     MOVE W15-SYEAR	 TO W15-SYY.
	     MOVE W12-T-YMD	 TO W15-E-YMD.
	     MOVE ZERO		 TO W15-EDD.
	     ADD 1		 TO W15-EMM.
	   IF W15-EMM > 12
	       SUBTRACT 12	 FROM W15-EMM
	       ADD 1		 TO W15-EYY.
	   IF W15-E-YMD NOT < W15-S-YMD
	       IF W30-ESC NOT = ZERO
		   COMPUTE W30-RENTAL ROUNDED = W30-RENTAL +
			   (W30-RENTAL * (W30-ESC / 100.0000))
		   ADD 1	 TO W15-SYEAR
		   MOVE W15-START
				 TO W30-ADATE.
	     MOVE W30-READING	 TO W30-LSTRDNG.
	     MOVE W30-DBFOTO	 TO DBF-RECORD1.
	     PERFORM REWRITE-DBFOTO THRU WRITE-DBFOTO-EXIT.
	     GO TO DA20.

       DA40.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
026320       MOVE SPACES         TO REP-DETAIL2.
	     MOVE X"0E" 	 TO INV-EXP.
	     MOVE "REF No:"	 TO INV-H14.
026590       MOVE W90-REF        TO INV-REF.
026370	     WRITE INV-L1 BEFORE 0 LINES.
026370	     WRITE INV-L1 BEFORE 2 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
	     MOVE X"0E" 	 TO INV-EXP.
	     MOVE "DATE  :"	 TO INV-H14.
026670       MOVE W12-TODAY      TO INV-DTE.
026370	     WRITE INV-L1 BEFORE 0 LINES.
026370       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE WS-ECAN	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 0 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "DELIVERY / AFLEWRING"
				 TO INV-DADD2.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
026330       MOVE W40-NAME       TO INV-NAME.
	     MOVE "PAGE / BLADSY"
				 TO INV-H4.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO INV-PGE.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
026350       MOVE W40-ADD1       TO INV-NAME.
026360	     MOVE W40-PADD1	 TO INV-DADD2.
026370	     WRITE INV-L1 BEFORE 1 LINES.
026380       MOVE W40-ADD2       TO INV-NAME.
026390	     MOVE W40-PADD2	 TO INV-DADD2.
	     MOVE "ACCOUNT / REKENING"
				 TO INV-H4.
026550	     MOVE W80-ACNO	 TO INV-AC.
026370	     WRITE INV-L1 BEFORE 1 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
026410       MOVE W40-ADD3       TO INV-NAME.
026420	     MOVE W40-PADD3	 TO INV-DADD2.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
026440       MOVE W40-ADD4       TO INV-NAME.
026450	     MOVE W40-PADD4	 TO INV-DADD2.
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
026340	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
026510       MOVE W40-PC1        TO INV-PC1.
026510	     MOVE W40-PC2	 TO INV-PC2.
026460	     WRITE INV-L1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "CONTRACT NUMBER"
				 TO INV-H5.
	     MOVE "  QUANTITY"	 TO INV-H7.
	     MOVE "DISCOUNT"	 TO INV-H9.
	     MOVE "EXCLUSIVE"	 TO INV-H10.
	     MOVE "NETT AMOUNT"  TO INV-H12.
026430	     WRITE INV-L1 BEFORE 0 LINES.
026430	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE " KONTRAK NOMMER"
				 TO INV-H5.
	     MOVE "DESCRIPTION/BESRYWING"
				 TO INV-H6.
	     MOVE "HOEWEELHEID"  TO INV-H7.
	     MOVE "PRICE/PRYS"	 TO INV-H8.
	     MOVE " AFSLAG"	 TO INV-H9.
	     MOVE "EKSKLUSIEF"	 TO INV-H10.
	     MOVE "VAT/BTW"	 TO INV-H11.
	     MOVE "NETO BEDRAG"  TO INV-H12.
026430	     WRITE INV-L1 BEFORE 0 LINES.
026430	     WRITE INV-L1 BEFORE 2 LINES.
026500	     MOVE SPACES	 TO REP-DETAIL2.

       DA45.
	     EXIT.

       DA50.
	     SUBTRACT LINAGE-COUNTER FROM WS-PRN-LENGTH
				 GIVING WS-LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE WS-LINES LINES.
028030	     MOVE ALL "-"	 TO INV-LINE.
      *
      *    ****    P R I N T   S U B - T O T A L
      *
	     MOVE "Please check your invoice."
				 TO INV-REMKS.
028040	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "TOTAL/TOTAAL" TO INV-H13.
	     MOVE "Gaan asseblief u faktuur na."
				 TO INV-REMKS.
	     MOVE W90-DEXC	 TO INV-EXTEND.
	     MOVE W90-DTAX	 TO INV-TAX.
	     MOVE W90-DTOT	 TO INV-VAL.
028880	     WRITE INV-L1 BEFORE 0 LINES.
	     MOVE SPACES	 TO INV-REMKS.
028880	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE ALL "="	 TO INV-LINE.
028080	     WRITE INV-L1 BEFORE PAGE.
      *      WRITE INV-L1 BEFORE 3 LINES.
028890	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE SPACES	 TO W80-ACNO.

       DA55.
	     EXIT.

       DA60.
027850	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
027870	     MOVE "C/FWD - OORGEDRA"
027880				 TO INV-DADD2.
	     MOVE W90-DEXC	 TO INV-EXTEND.
	     MOVE W90-DTAX	 TO INV-TAX.
	     MOVE W90-DTOT	 TO INV-VAL.
027900	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "Please check your invoice."
				 TO INV-REMKS.
027900	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE "Gaan aseblief u faktuur na."
				 TO INV-REMKS.
027900	     WRITE INV-L1 BEFORE 4 LINES.
027940	     PERFORM BB050.
	     PERFORM DA40.
027970	     MOVE "B/FWD - O/B"  TO INV-DESC.
	     MOVE W90-DEXC	 TO INV-EXTEND.
	     MOVE W90-DTAX	 TO INV-TAX.
	     MOVE W90-DTOT	 TO INV-VAL.
028000	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.

       DA100.
026560	     MOVE 0		 TO WS-PAGE.
             MOVE WS-12CPI       TO R-DET1.
             WRITE R-L1 BEFORE 0 LINES.
026580       MOVE SPACES         TO R-DET1.
026590       MOVE "PAGE:"        TO R-PAGE.
026600       MOVE "DATE:"        TO R-DATE.
026610       ADD 1               TO WS-PAGE.
026620       MOVE W12-TODAY      TO R-DTE.
026630       MOVE WS-PAGE        TO R-P-NO.
026700	     MOVE "DEBTOR RENTAL AND SERVICE CONTRACTS"
                                 TO R-H5.
026650       WRITE R-L1 BEFORE 2 LINES.
026660       MOVE SPACES         TO R-DET1.
             MOVE WS-EXPP        TO R-L1.
011760       WRITE R-L1 BEFORE 0 LINES.
             MOVE SPACES         TO R-DET1.
015410       MOVE W95-COMP       TO R-CO.
011760       WRITE R-L1 BEFORE 0 LINES.
011760       WRITE R-L1 BEFORE 0 LINES.
             MOVE WS-ECAN        TO R-L1.
026710       WRITE R-L1 BEFORE 2 LINES.
	     MOVE "Exclusive"	 TO R-H12.
	     MOVE "V.A.T."	 TO R-H13.
	     MOVE "Inclusive"	 TO R-H14.
026710	     WRITE R-L1 BEFORE 1 LINES.
032040       MOVE "Total"        TO R-TDESC.
032050	     COMPUTE R-VAL = W90-DEBIT - W90-VATTOT.
             MOVE W90-VATTOT     TO R-TAX.
032050	     MOVE W90-DEBIT	 TO R-TOT.
032060       WRITE R-L8 BEFORE PAGE.

       DA999.
	     EXIT.

       CG000	       SECTION 6.
       CG00.
	   IF W30-VAT = 1
	       MOVE W05-VAT (WS-VAT-SUB)
				 TO W05-RATE
	       COMPUTE W90-VAT ROUNDED = (W90-VAL * W05-RTE)
	   ELSE
	       MOVE W90-VAL	 TO W90-EXP
	       MOVE ZERO	 TO W90-VAT.
	     ADD W90-VAL W90-VAT GIVING W90-DB.
	     PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
	     PERFORM AY60 THRU AY999.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY01 THRU AY59.
	     PERFORM AY02 THRU AY59.
	     MOVE "Y"		 TO WS-SKIP.
028260	     PERFORM GENERATE-DEBTOR-TRAN.
	     MOVE "N"		 TO WS-SKIP.
028270	     ADD W90-DB	       TO W90-BDT W90-DEBIT.
	     ADD 121 DEB-CAT   GIVING W90-RESULT.
	     DIVIDE W90-RESULT BY 2
			       GIVING W90-CATKEY
			       REMAINDER W90-CS.
	     ADD 1	       TO W90-CS.
040190	     MOVE W90-CATKEY   TO WS-PARKEY.
028310	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     PERFORM AY01 THRU AY59.
028320	     ADD W90-DB	       TO PAR-CDRMTD (W90-CS)
028330				  PAR-CDRYTD (W90-CS).
028360	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
030870	     MOVE W90-REF	 TO INV-ITM.
030860	     MOVE W30-DESC	 TO INV-DESC.
	     MOVE W90-UNITS	 TO INV-QNT.
	     MOVE W90-PRICE	 TO INV-PRICE.
	     MOVE W90-VAL	 TO INV-EXTEND.
	     MOVE W90-VAT	 TO INV-TAX.
030890	     MOVE W90-DB	 TO INV-VAL.
030920	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 2)
030930         WRITE R-L8 BEFORE 1 LINES
030940     ELSE
	       PERFORM DA60
030960	       PERFORM DA10 THRU DA15.
030970	     MOVE SPACES	 TO REP-DETAIL1.
030980       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
031350	     ADD W25-VALUE	 TO DEB-OUT.
031360       ADD W90-BDT         TO DEB-DT.
031370       ADD W90-BCR         TO DEB-CR.
031380       SUBTRACT W90-120    FROM DEB-FOUR.
031380       SUBTRACT W90-90     FROM DEB-THREE.
031390       SUBTRACT W90-60     FROM DEB-TWO.
031400       SUBTRACT W90-30     FROM DEB-ONE.
031410       SUBTRACT W90-CUR    FROM DEB-MTD.
031830       SUBTRACT W90-INT    FROM DEB-INTEREST.
031870 CG70A.
             MOVE 9              TO WS-PARKEY.
031900       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             PERFORM AY01 THRU AY59.
             MOVE 300            TO WS-DEPKEY.
             PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
             PERFORM AY38 THRU AY59.
           IF W90-VAL NOT = ZERO
031880         IF W90-TAX = ZERO
                   IF W90-DB = ZERO
                       ADD W90-VAL 
                                 TO PAR-CNGS-DAY
                                    PAR-CNGS-MTD
                                    PAR-CNGS-YTD
                   ELSE
                       ADD W90-VAL 
                                 TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD
               ELSE
               IF W90-DB = ZERO
                   ADD W90-VAL   TO PAR-CVAT-DAY
                                    PAR-CVAT-MTD
                                    PAR-CVAT-YTD
                   ADD W90-TAX   TO DPT-RET-DAY
                                    DPT-RET-MTD
                                    DPT-RET-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TAX * -1
                       MOVE DPT-PUR-GL   TO WX-AC
                       MOVE 11           TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
               ELSE
                   ADD W90-VAL   TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD
                   ADD W90-TAX   TO DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-TAX * -1
                       MOVE DPT-SALE-GL
                                     TO WX-AC
                       MOVE 10       TO WX-TYPE
                       PERFORM GL-TRF-UPDATE.
031930       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
031930       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
031860       PERFORM AY70 THRU AY999.

       CG999.
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

	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.


025110	     MOVE 3		 TO WS-PARKEY.
025120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025140	     MOVE WS-PTH15	 TO W02-PTH2
				    W02-PTH5
				    W02-PTH6
				    W02-PTH7
				    W02-PTH18
				    W02-PTH32
				    W02-PTH35
				    W02-PTH40
				    W02-PTH42.
042160	     MOVE LS-USER	 TO W02-USE18.
053550       MOVE PAR-VAT-REG    TO W05-VATNO.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
044300	     OPEN I-O DBFOTO.
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
             MOVE 4 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE TO WS-AGE.
025630       MOVE 1 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650       MOVE PAR-COMPANY TO W95-COMP.
054820       MOVE PAR-TELEPHONE  TO W95-TEL.
025660       MOVE PAR-DMY TO W12-TODAY.
025670       MOVE PAR-YMD TO W12-T-YMD.
054870	     ADD 1		 TO WS-PARKEY.
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
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DISCOUNT   TO W05-DISCOUNT.
042600       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
	     MOVE PAR-RATE-DATE	 TO WS-VAT-DATE.
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
025730	     MOVE 1 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     DISPLAY "Waiting for printer response" AT 1412
                      WITH FOREGROUND-COLOR 14 BLINK.
042780       OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD      TO W22-DTE2.
	   IF W22-DD2 > 27
                ADD 1            TO W22-MM2.
       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
               SUBTRACT 1        FROM W22-YY2.
           IF W22-MM2 = 02
               MOVE 28           TO W22-DD2
           ELSE
           IF W22-MM2 = 04 OR 06 OR 09 OR 11
               MOVE 30           TO W22-DD2
           ELSE
               MOVE 31           TO W22-DD2.
       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
             MOVE W22-DTE2       TO W30-CUR.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
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
056340	   ELSE
056350     IF WS-F-ERROR = 5
056360         MOVE W02-DBTRAN TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041810	   IF WS-F-ERROR = 7
041820         MOVE W02-DEPART TO WS-FILE
041830         MOVE WS-DEPKEY TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER TO WS-FILE
041870         MOVE WS-RECKEY TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 35
041860	       MOVE W02-TXPTRN	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TXP-KEY	 TO WS-KEYX
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
