      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *  ********   ******   *******	*******   *******   **	   **  *
      *     **	   **	 **  **    **	**    **  **	**  ***    **  *
      *     **	   **	     **    **	**    **  **   **   ** *   **  *
      *     **	   **	     *******	*******   ******    **	*  **  *
      *     **	   **	     ** 	**	  **   **   **	 * **  *
      *  ** **	   **	 **  ** 	**	  **	**  **	  ***  *
      *   ***	    ******   ** 	**	  **	**  **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R I N T   J O B   D E T A I L S   P R O G R A M         *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCPPRN.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 1999
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000150 COPY DEBTOR.ISL.

000190 COPY DEPART.SL.

000270 COPY JOBCST.SL.

000270 COPY JOBHDRN.SL.

000270 COPY PARAM.SL.

000310 COPY SALE.SL.

       COPY SHARED.SL.

       COPY SPARTS.SL.

000330 COPY STOCK.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000480 COPY DEBTOR.FDE.

000420 COPY DEPART.FDE.

000500 COPY JOBCSTN.FDE.

000500 COPY JOBHDRN.FDE.

000560 COPY PARAM.FDE.

000600 COPY SALE.JFD.

       COPY SHARED.FDE.

       COPY SPARTS.FDE.

000620 COPY STOCK.FDE.

      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002100 77  WS-DISC	   PIC	99V99	 COMP-3.
002110 77  WS-TDISC        PIC  99V99    COMP-3.
002120 77  WS-CASH-AC      PIC  X(06).
002130 77  WS-SUB          PIC  9(04)    COMP-5.
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
002390 77  WS-TRANS	   PIC	9(06)	 COMP-5 VALUE 1.
002430 77  WS-START	   PIC	9(08).
002440 77  WS-END          PIC  9(08).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002460 77  WS-TME2	   PIC	9(08).
002470 77  WS-PORT         PIC  9(05) VALUE 1004.
002480 77  WS-OPEN         PIC  X(01) VALUE X"01".
002490 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO          PIC  9(02) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST        PIC  9(02) VALUE 0.
002540 77  WS-LAST         PIC  9(02) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
002590 77  WS-OPTION	   PIC	X(01).
002610 77  WS-SKIP	   PIC	X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002640 77  WS-DEM          PIC  9(01).
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP	   PIC	9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002830 77  WS-BATCH	   PIC	9(05).
002860 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
002870 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002880 77  WS-PER          PIC  Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
001350 01  WS-CRT-LINES.
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001440	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MJOB REDEFINES WS-MID-LINE.
	       05  FILLER      PIC  X(01).
	       05  WS-JOB78    PIC  X(78).
	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
001500	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
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
               05  FILLER  PIC  X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
001590	   03  WS-MID-LNE.
001600	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
001650	   03  WS-MD-LNE.
001660	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
001710	   03  WS-MD-LINE.
001720	       05  WS-MCA   PIC  X(80) VALUE "³        ³      ³
      -        "                       ³        ³        ³           ³".
	   03  WS-JB78-L REDEFINES WS-MD-LINE.
	       05  WS-MJB1     PIC  X(01).
	       05  WS-MJB78    PIC  X(78).
	       05  WS-MJB80    PIC  X(01).
001710	   03  WS-MD-LIN.
001720	       05  WS-MCB   PIC  X(80) VALUE "³  ³        ³            ³
      -        "                                ³         ³          ³".
	   03  WS-RQ78-L REDEFINES WS-MD-LIN.
	       05  WS-RQ1      PIC  X(01).
	       05  WS-RQ78     PIC  X(78).
	       05  WS-RQ80     PIC  X(01).

003340 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "JCP".
	   03  WS-PROG	       PIC  X(03) VALUE "PRN".

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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003530 01  W02-FID.

       COPY APACFIDS.ID.

003570 COPY CONTROL.ID.

003610 COPY DEBTOR.ID.

002120 COPY DEPART.ID.

003670 COPY JOBCST.ID.

003670 COPY JOBHDR.ID.

       COPY PARAM.ID.

003710 COPY SALE.ID.

       COPY SHARED.ID.

       COPY SPARTS.ID.

003730 COPY STOCK.ID.

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

	   03  REP-L1 REDEFINES W02-PRN-LINE.
	       05  REP-DETAIL1	   PIC X(132).

	   03  REP-L2 REDEFINES W02-PRN-LINE.
	       05  REP-DETAIL2	   PIC X(96).

	   03  REP-L3 REDEFINES W02-PRN-LINE.
	       05  REP-AC	   PIC X(10).
	       05  REP-ACNO	   PIC X(06).
	       05  FILLER	   PIC X(01).
	       05  REP-NAME	   PIC X(40).
	       05  FILLER	   PIC X(06).
	       05  REP-DATE	   PIC X(06).
	       05  REP-TIME.
		   07  REP-DTE	   PIC Z9/99/9999.

	   03  REP-L3A REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(10).
	       05  REP-TEL	   PIC X(14).
	       05  REP-CNTCT	   PIC X(10).
	       05  REP-CONTACT	   PIC X(30).
	       05  FILLER	   PIC X(32).

	   03  REP-L4 REDEFINES W02-PRN-LINE.
	       05  REP-H1	   PIC X(29).
	       05  REP-REF	   PIC 9(08).
	       05  FILLER	   PIC X(42).

	   03  REP-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-H2	   PIC X(19).
	       05  REP-H3	   PIC X(32).
	       05  REP-H4	   PIC X(09).
	       05  REP-H5	   PIC X(15).
	       05  REP-H6	   PIC X(12).
	       05  REP-H7	   PIC X(05).
	       05  FILLER	   PIC X(02).

	   03  REP-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(02).
	       05  REP-EXT-ITEM.
000950		   07  REP-ITM1	   PIC	X(14).
		   07  REP-SL	   PIC	X(01).
		   07  REP-ITM	   PIC	X(03).
	       05  FILLER	   PIC X(01).
	       05  REP-DESC	   PIC X(30).
	       05  FILLER	   PIC X(01).
	       05  REP-QNT	   PIC Z(05)9.99.
	       05  FILLER	   PIC X(01).
	       05  REP-NC.
		   07  REP-PRICE   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  REP-VAT	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(01).
	       05  REP-VAL	   PIC Z(06)9.99.
	       05  FILLER	   PIC X(02).

	   03  REP-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  REP-SMAN	   PIC X(30).
	       05  FILLER	   PIC X(01).
	       05  REP-H8	   PIC X(10).
	       05  REP-TERMS	   PIC X(28).

	   03  REP-L8 REDEFINES W02-PRN-LINE.
	       05  FILLER.
		   07  REP-H9	   PIC	X(10).
		   07  FILLER	   PIC	X(01).
	       05  REP-MODEL.
		   07  REP-ORD	   PIC	X(14).
		   07  FILLER	   PIC	X(10).
	       05  FILLER	   PIC	X(01).
	       05  REP-ITMDESC	   PIC	X(42).

	   03  REP-L9 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(08).
	       05  REP-REMK	   PIC X(40).
	       05  FILLER	   PIC X(48).

       COPY W05.VAT.

003890 01  W05-INV.
003900     03  W05-ENTRY       PIC  ZZ9.
003910     03  FILLER          PIC  X(01) VALUE SPACES.
003920     03  W05-DATE        PIC  Z9/99/99.
003930     03  FILLER          PIC  X(01) VALUE SPACES.
003940     03  W05-TYPE        PIC  X(03).
003950     03  FILLER          PIC  X(03) VALUE SPACES.
003960     03  W05-REF         PIC  9(07).
003970     03  FILLER          PIC  X(01) VALUE SPACES.
003980     03  W05-VALUE       PIC  Z(06)9.99-.

       COPY W10.JCP.

       COPY W12.WS.

003390 01  W14-JOB.
003400	   03  W14-REF	      PIC  Z(08).
003410	   03  W14-AC	      PIC  X(06).
003420     03  W14-NAME.
003430	       05  W14-NME    PIC  X(32).
003440	       05  FILLER     PIC  X(08).
003450	   03  W14-DATE       PIC  Z9/99/9999.
003470	   03  W14-DOC	      PIC  X(01).
003480	   03  W14-DDOC.
003490	       05  W14-INV    PIC  Z(08).
003500	   03  W14-VALUE      PIC  Z(07)9.99.
	   03  W14-STATUS     PIC  X(01).

003510 01  W14-DELETE.
003520     03  W14-SUB         PIC 9(03)     COMP-3.
003530     03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

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
001660     03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE 
               "% Settlement discount if paid in 30 DAYS".

       01  W30-REMRKS.
	   03  W30-RMKS        PIC  X(90) OCCURS 4.

       01  W30-PAYMENT-CODES.
	   03  W30-P-CODES.
	       05  FILLER      PIC  X(22) VALUE "RPREPAIRS".
	       05  FILLER      PIC  X(22) VALUE "FSFIELD SUPPORT".
	       05  FILLER      PIC  X(22) VALUE
					 "MCON SITE MAINT. CONT.".
	       05  FILLER      PIC  X(22) VALUE "W WARRANTY".
	       05  FILLER      PIC  X(22) VALUE "PMPREVENTATIVE MAINT.".
	       05  FILLER      PIC  X(22) VALUE "SRSPECIAL RATES".
	       05  FILLER      PIC  X(22) VALUE "RNREPEAT NO CHARGE".
	       05  FILLER      PIC  X(22) VALUE "CBCALL BACK NO CHARGE".
	       05  FILLER      PIC  X(22) VALUE "I INSTALLATION".
	       05  FILLER      PIC  X(22) VALUE "C CONTRACT".
	   03  W30-PAYMENTS REDEFINES W30-P-CODES.
	       05  W30-PMETH   OCCURS 10 TIMES.
		   07  W30-CDE PIC  X(02).
		   07  W30-DSC PIC  X(20).

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

005640 01  W75-KEYS.
005650     03  W75-S          PIC 9(02) COMP-5.
005660     03  W75-S1         PIC 9(02) COMP-5.
005660	   03  W75-S2	      PIC 9(02) COMP-5.
005670     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-CODE       PIC X(14) OCCURS 18.
	   03  W75-JOBS.
	       05  W75-JCOST OCCURS 18.
004810		   07  W75-JCST	 PIC  9(08).
		   07  W75-IDESC PIC  X(30).

005860 01  W80-ACCOUNT.
005870     03  W80-ACNO        PIC X(06).
005880     03  W80-CRED REDEFINES W80-ACNO.
005890         05  W80-CNO     PIC 9(05).
005900         05  W80-CAC REDEFINES W80-CNO.
005910             07  W80-CTYPE PIC 9(01).
005920             07  W80-SER   PIC 9(04).
005930         05  FILLER        PIC 9(01).
005940     03  W80-TYPE        PIC 9(01).

       COPY W90.JCP.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).
006610	   03  W95-ADDRESS.
	       05  W95-CHAR    PIC  X(01) OCCURS 120.
	   03  W95-ADD-RED REDEFINES W95-ADDRESS.
	       05  W95-ADDPRN  PIC  X(70).
	       05  FILLER      PIC  X(50).
	   03  W95-ADD1.
	       05  W95-CHR1    PIC  X(01) OCCURS 30.
006620	   03  W95-ADD2.
	       05  W95-CHR2    PIC  X(01) OCCURS 30.
006630	   03  W95-ADD3.
	       05  W95-CHR3    PIC  X(01) OCCURS 30.
006640	   03  W95-ADD4.
	       05  W95-CHR4    PIC  X(01) OCCURS 30.
006650     03  W95-PC.
006660         05  W95-POST    PIC 9(04).
006670     03  W95-TEL         PIC X(14).
006680     03  W95-V1.
006690         05  W95-T       PIC Z(04)9.99.
006700     03  W95-V2.
006710         05  W95-B       PIC Z(06)9.99-.
006720     03  W95-D.
006730	       05  W95-DTE     PIC Z9/99/9999.
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

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

       COPY JOBLUP.CRT.

007910 01  S07.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

009620 01  S26-QNT.
009630     03          COLUMN 51 FOREGROUND-COLOR 11
009640                           PIC Z(04)9.99 USING W10-QUANT AUTO.
009650 01  S26-PRC.
009660     03          COLUMN 60 FOREGROUND-COLOR 11
009670                           PIC Z(05)9.99 USING W10-SELL AUTO.
009680 01  S26-DSC.
009690     03          COLUMN 30 FOREGROUND-COLOR 11
009700                           PIC Z9.99 USING WS-DISC AUTO.
009710 01  S26-TDSC.
009720     03          COLUMN 15 FOREGROUND-COLOR 11
009730                           PIC Z9.99 USING WS-TDISC AUTO.
009740 01  S26-AVGE.
009750     03          COLUMN 28 FOREGROUND-COLOR 11
009760				 PIC Z(04)9.999 USING STK-AVGE AUTO.
009890 01  S28.
009900     03  LINE 25 COLUMN 25 FOREGROUND-COLOR 15
009910                           PIC ZZ9 USING WS-SALKEY AUTO.
009290 01  S30.
010030     03  LINE  2 COLUMN 36 FOREGROUND-COLOR 15 
010040                           VALUE "JOB CARD".
009310     03  LINE  3 COLUMN  2 VALUE "Ac No".
009320     03          COLUMN 15 VALUE "Name".
010070	   03	       COLUMN 61 VALUE "Ord No°°°°°°°°°°°°°".
010080	   03  LINE  4 COLUMN  2 VALUE "Tech.".
           03          COLUMN 61 VALUE "Tel.".
010090     03  LINE  5 COLUMN  2 VALUE "Job No".
           03          COLUMN 20 VALUE "Contact".
009380     03          COLUMN 67 VALUE "Date".
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
010190	   03  LINE 20 COLUMN  3 VALUE "Sub Total ".
010200	   03  LINE 21 COLUMN  3 VALUE "Discount  ".
010210     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
010230	   03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
           03  FOREGROUND-COLOR 11 PIC Z(08) USING W10-JCST AUTO.

009950 01  S36.
009960     03  FOREGROUND-COLOR 11 PIC X(06) USING W80-ACNO AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270	     PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
010430	     GO TO BA00.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011210	     PERFORM JOBHDR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

       COPY PRINTUTL.AP6.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011010 AA400              SECTION.
011020 AA401.
011590	     DISPLAY "F1" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 3
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011600		     "=Job no," WITH FOREGROUND-COLOR 1
				     BACKGROUND-COLOR 3
011610		     "F3" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
011620		     "=Acnt no," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F4" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Item desc," WITH FOREGROUND-COLOR 1
					BACKGROUND-COLOR 3
		     "F5" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Client name," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "F6" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Contact," WITH FOREGROUND-COLOR 1
				      BACKGROUND-COLOR 3
		     "F7" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			       BACKGROUND-COLOR 3
		     "=Date Lookup" WITH FOREGROUND-COLOR 1
					 BACKGROUND-COLOR 3.
011110 AA405.
011120       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011170 COPY CONTROL.RD.

011210 COPY DEBTOR.RD.

011730 COPY DEPART.RD.

011290 COPY PARAM.RD.

011310 COPY SALE.RD.

011270 COPY JOBCST.RD.

011270 COPY JOBHDR.RD.

       COPY SHARED.RD.

       COPY SPARTS.RD.

011330 COPY STOCK.RD.

      /
011370 AD000-WRITE             SECTION.

011410 COPY CONTROL.WR.

011410 COPY PARAM.WR.

011530 COPY SALE.WR.

011270 COPY JOBCST.WR.

       COPY APAC.HLP.

       COPY JOBCST.LUP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
012470 AY60.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
012690*      MOVE 5		 TO WS-NETKEY.
012700*      PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012720	     GO TO AY999.
012950 AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
012990       UNLOCK NETWORK.
013070       COMMIT.
013080 AY999.
013090       EXIT.
      /
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
	     CLOSE SALE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AZ100.
013260       EXIT PROGRAM.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013270 CA000         SECTION.
013280 CA00.
013290       MOVE 14             TO W50-LENGTH.
013300 CA05.
013310       MOVE W10-ITEM       TO W50-DATA.
013320     IF W50-1 = SPACE
013330         MOVE W50-13       TO W10-ITEM
013340         GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013400	     MOVE W50-ITEM	 TO W10-ITEM.
013410 CA06.
013420       MOVE W50-ITEM       TO STK-CODE.
013430 CA10.
013440       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
013470 CA99.
013480       EXIT.
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
      *    ****   G E T   J O B C S T	R E C O R D   U S I N G   JOB-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT JOB RECORD (INVOICE)
      *                    3 = JOB DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
014550 GET-JOBCST	    SECTION 2.

014570 GET-JCST-JOBNO.
             INITIALIZE W14-DELETE.
             MOVE 1              TO W14-SUB.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
014580	     MOVE W10-JCST	 TO JHD-REF.
014600       MOVE ZERO           TO WS-ERROR.
014610	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
014620	   IF WS-F-ERROR = 14
014630         MOVE 1            TO WS-ERROR
014640         GO TO GET-JOBCST-EXIT.
014650	   IF JHD-DOC NOT = "J"
014660         MOVE 2            TO WS-ERROR
014670         GO TO GET-JOBCST-EXIT.
014680       MOVE 1              TO WS-LAST WS-SALKEY.
014790	     MOVE JHD-AC	 TO W80-ACNO DEB-ACNO.
014800	     PERFORM CA155-GET-DEBTOR.
014810	   IF WS-ERROR NOT = ZERO
014820	       MOVE 4		 TO WS-ERROR
014830	       GO TO GET-JOBCST-EXIT.
	     MOVE JHD-DTE	 TO W40-DTE.
	     MOVE JHD-ITEM	 TO W10-IDESC.
014860	     MOVE JHD-NAME	 TO W40-NAME.
	     MOVE JHD-AD1	 TO W40-AD1.
	     MOVE JHD-AD2	 TO W40-AD2.
	     MOVE JHD-AD3	 TO W40-AD3.
	     MOVE JHD-AD4	 TO W40-AD4.
	     MOVE JHD-PC	 TO W40-PC.
	     MOVE JHD-TEL	 TO W40-TEL.
	     MOVE JHD-HTEL	 TO W40-HTEL.
	     MOVE JHD-CONTACT	 TO W40-CONTACT.
	     MOVE JHD-ID	 TO W40-ID.
014870	     MOVE JHD-ORD	 TO W90-ORD.
014850	     MOVE JHD-OREF	 TO W90-REG.
014890	     MOVE JHD-EXP	 TO W90-EXP.
014910	     MOVE JHD-PRC	 TO WS-PRC.
014880	     MOVE JHD-SMAN	 TO W10-SMAN.
	     MOVE JHD-STATUS	 TO W90-STATUS.
	     MOVE JHD-COMMENT	 TO W10-COMMENT.
	     MOVE JHD-COMMENT2	 TO W10-COMMENT2.
	     MOVE JHD-START	 TO W90-START.
015230	     MOVE JHD-SUBTOT	 TO W90-DB.
015240	     MULTIPLY -1 BY JHD-DSCNT
015250				 GIVING W90-TDISC.
015260	     MOVE W90-TDISC	 TO W90-LDISC.
015270	     MOVE JHD-NON-TXBL	 TO W90-NONTAX.
015280	     MOVE JHD-TAXABLE	 TO W90-EXCTOT.
015290	     MOVE JHD-TAX	 TO W90-TTAX.
015300	     MOVE JHD-TOT	 TO W90-TOTAL.
015310	     MOVE JHD-BAL	 TO W90-BAL.
	     MOVE JHD-WCOMP	 TO W90-WCOMP.
	     MOVE JHD-PMETH	 TO W90-PMETH.
	     MOVE JHD-LOCATION	 TO W90-LOCATION.
	     MOVE 1		  TO WS-S7.

       GET-JCST-PMT-LOOP.
	   IF W90-PMETH = W30-CDE (WS-S7)
	       GO TO GET-JCST-PMT-DESC.
	     ADD 1		  TO WS-S7
	   IF WS-S7 < 11
	       GO TO GET-JCST-PMT-LOOP.
	     MOVE 1		  TO WS-S7.
	     MOVE "RP"		  TO W90-PMETH.

       GET-JCST-PMT-DESC.
	     MOVE W30-DSC (WS-S7) TO W40-TERMS.
      *
014580	     MOVE W10-JCST	 TO JOB-REF.
022140       MOVE 100            TO JOB-SQN.
014610	     PERFORM READ-JOBCST THRU READ-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST WS-SALKEY.
014620     IF WS-F-ERROR = 13
014640         GO TO GET-JOBCST-EXIT.
014690       GO TO GET-JCST-TEST-TYPE.

014710 GET-JCST-NEXT.
014720       PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
014730     IF WS-F-ERROR = 13
014750         GO TO GET-JOBCST-EXIT.
	   IF JOB-REF NOT = W10-JCST
	       MOVE W10-JCST   TO JOB-REF
	       GO TO GET-JOBCST-TEST-EXIT.

014770 GET-JCST-TEST-TYPE.
      * 		     I T E M   D E T A I L S
014930     IF JOB-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
014940             INITIALIZE SAL-RECORD1
		   MOVE "S"	 TO SAL-TYPE
014950		   MOVE JOB-EXT-ITEM
				 TO SAL-EXT-ITEM
014960		   MOVE JOB-DESC TO SAL-DESC
014970             MOVE JOB-QNT  TO SAL-QNT
014980             MOVE JOB-COST TO SAL-COST
014990             MOVE JOB-SELL TO SAL-SELL
015000             MOVE JOB-VAT  TO SAL-VAT
015010             MOVE JOB-VAL  TO SAL-VAL
015020             MOVE JOB-INCTAX
                                 TO SAL-INCTAX
015030             MOVE JOB-NONTAX
                                 TO SAL-NONTAX
015040             MOVE JOB-TAXVAL   
                                 TO SAL-TAXVAL
015050             MOVE JOB-DSC  TO SAL-DSC
015060             MOVE JOB-DISC TO SAL-DISC
015070             MOVE JOB-SQN  TO SAL-SQN
                   MOVE JOB-TECH TO SAL-TECH
		   MOVE JOB-VAT-SUB
				 TO SAL-VAT-SUB
		   IF SAL-VAT-SUB > ZERO
		       ADD JOB-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
023060		       MOVE W05-VAT(SAL-VAT-SUB)
023060				 TO W05-RATE
023080		       COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED =
			      (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
		   END-IF
015080		   PERFORM GET-JCST-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
015090     ELSE
      *                      P A R T S   D E T A I L S
015100     IF JOB-TYPE = 2
015110         INITIALIZE SAL-RECORD1
	       MOVE "T" 	 TO SAL-TYPE
014950	       MOVE JOB-EXT-ITEM TO SAL-EXT-ITEM
014960	       MOVE JOB-DESC	 TO SAL-DESC
014970         MOVE JOB-QNT      TO SAL-QNT
015140         MOVE JOB-SQN      TO SAL-SQN
015150*        PERFORM GET-JCST-WRITE-SALE
015160     ELSE
      *                      R E M A R K S
015170     IF JOB-TYPE = 3
015180         INITIALIZE SAL-RECORD1
	       MOVE "R" 	 TO SAL-TYPE
015190         MOVE JOB-SERIAL   TO SAL-DESC
015200         MOVE JOB-SQN      TO SAL-SQN
015210         PERFORM GET-JCST-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF JOB-TYPE = 4 
               INITIALIZE SAL-RECORD1
	       MOVE "I" 	 TO SAL-TYPE
               MOVE JOB-DESC     TO SAL-DESC
               MOVE JOB-SQN      TO SAL-SQN
               MOVE 1            TO SAL-SUPER
	       PERFORM GET-JCST-WRITE-SALE.
015340	     GO TO GET-JCST-NEXT.

015360 GET-JCST-WRITE-SALE.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.
015400 
       GET-CHECK-STOCK-FILE.
	     MOVE JOB-EXT-ITEM	 TO STK-CODE.
013560	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
		    JOB-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
             MOVE JOB-SQN        TO SAL-SQN W14-SQN (W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-JCST-WRITE-SALE.

       GET-CH-TO-RMK-EXIT.
	     GO TO GET-JCST-NEXT.

       GET-JOBCST-TEST-EXIT.
	       IF NOT (W14-SUB > 1)
015330             GO TO GET-JOBCST-EXIT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
             MOVE W14-SQN (W14-SUB)
                                 TO JOB-SQN.
             PERFORM READ-JOBCST-LOCK THRU READ-JOBCST-EXIT.
025000       SUBTRACT JOB-VAL    FROM W90-DB.
025010       SUBTRACT JOB-DISC   FROM W90-LDISC.
025020       SUBTRACT JOB-NONTAX FROM W90-NONTAX.
025030	     SUBTRACT JOB-TAXVAL FROM W90-EXCTOT.
025050       COMPUTE W90-BAL = W90-BAL - (JOB-COST * JOB-QNT).
             MOVE 3              TO JOB-TYPE.
	     STRING JOB-EXT-ITEM DELIMITED BY SIZE
		    JOB-DESC DELIMITED BY SIZE
                                 INTO JOB-DESC.
	     MOVE SPACES	 TO JOB-EXT-ITEM JOB-SERNO.
	     MOVE ZERO		 TO JOB-QNT JOB-COST JOB-INCTAX
                                    JOB-VAT JOB-SELL JOB-NONTAX
                                    JOB-VAL JOB-DISC JOB-TAXVAL
                                    JOB-DSC.
             PERFORM REWRITE-JOBCST-UNLOCK THRU WRITE-JOBCST-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

015410 GET-JOBCST-EXIT.
015420       EXIT.

      /
016060 BA000                    SECTION.
016070 BA00.
	   IF LS-SLS-JOBNO NOT = SPACES
	       PERFORM BD000
	       GO TO AZ000-EOJ.

       BA05.
016080*      DISPLAY CLR-SCREEN.
016090*      DISPLAY S02.

016150 BA10.
016100	     MOVE "'P'rint a job card or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE " "		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "P" OR "E")
               GO TO BA10.
      *                            E X I T   P R O G R A M
016240     IF WS-OPTION = "E"
016250         GO TO AZ000-EOJ.
      *                            P R I N T   J O B   C A R D
016340       PERFORM BC000
016350       GO TO BA05.
      /
017690 BB050               SECTION.
017700 BB50.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO REP-DETAIL1.
	   IF LS-RET = "F"
	       MOVE "JOB CARD  -  REFERENCE NO:"
				 TO REP-H1
	   ELSE
017750	       MOVE "JOB REQUEST - REFERENCE NO:"
                                 TO REP-H1.
	     MOVE W90-REF	 TO REP-REF.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF LS-RET = "R"
	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE "REPLACE PREVIOUS JOB REQUEST"
				 TO REP-H1
	       PERFORM CALL-PRINTUTL.
	     MOVE W95-COMP	 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE W95-ADDPRN	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017710	     MOVE SPACES	 TO REP-DETAIL1.
	   IF W95-TEL NOT = SPACES
	       MOVE "Tel:"	 TO REP-AC
	       MOVE W95-TEL	 TO REP-TEL
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO REP-DETAIL1.
	       PERFORM CALL-PRINTUTL.

       BB60.
             MOVE "Account :"    TO REP-AC.
017770       MOVE W80-ACNO       TO REP-ACNO.
             MOVE W40-NAME       TO REP-NAME.
             MOVE "Date:"        TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE " Contact:"    TO REP-CNTCT.
             MOVE W40-CONTACT    TO REP-CONTACT.
             MOVE "Tel :"        TO REP-AC.
             MOVE W40-TEL        TO REP-TEL.
             MOVE "Time:"        TO REP-DATE.
012150       ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
             MOVE W95-TIME       TO REP-TIME.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
	   IF W90-ORD NOT = SPACES
               MOVE "Your Ref :" TO REP-AC
	       MOVE W90-ORD	 TO REP-ORD
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017790	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Item/Desc:"	 TO REP-H9.
	     MOVE JHD-MODEL	 TO REP-MODEL.
	     MOVE JHD-ITEM	 TO REP-ITMDESC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
	   IF W10-SMAN NOT = ZERO
               ADD 200 W10-SMAN  GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE "Technicn :" TO REP-AC
               MOVE PAR-TECHNICIAN 
                                 TO REP-SMAN
               IF W40-TERMS NOT = SPACES
                   MOVE "Payment :" 
                                 TO REP-H8
                   MOVE W40-TERMS
                                 TO REP-TERMS
               END-IF
	       PERFORM CALL-PRINTUTL
017790	       MOVE SPACES	 TO REP-DETAIL1
           ELSE
           IF W40-TERMS NOT = SPACES
               MOVE "Payment :"  TO REP-AC
               MOVE W40-TERMS    TO REP-SMAN
	       PERFORM CALL-PRINTUTL
017790	       MOVE SPACES	 TO REP-DETAIL1.
	   IF NOT (W10-COMMENT = SPACES AND
		   W10-COMMENT2 = SPACES)
	       MOVE "Comments/Notes :"
				 TO REP-H2
	       MOVE W10-COMMENT  TO REP-NAME
	       PERFORM CALL-PRINTUTL
017790	       MOVE SPACES	 TO REP-DETAIL1
	       IF NOT (W10-COMMENT2 = SPACES)
		   MOVE W10-COMMENT2
				 TO REP-NAME
		   PERFORM CALL-PRINTUTL
017790		   MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE ALL "="	 TO REP-DETAIL2.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO REP-DETAIL1.
	   IF LS-RET = "N"
	       MOVE "FAULT DESCRIPTION"
				 TO REP-H1
	   ELSE
017800	       MOVE "ITEM CODE"	 TO REP-H2
017820	       MOVE "DESCRIPTION"
				 TO REP-H3
017840	       MOVE "QUANTITY"	 TO REP-H4
	       MOVE "UNT-PRICE"	 TO REP-H5
	       MOVE "V.A.T."	 TO REP-H6
	       MOVE "VALUE"	 TO REP-H7.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.

017950 BB65.
017960       EXIT.
      /
      *    ****    J O B   C A R D   D E T A I L S
      *
018500 BC000	     SECTION 50.
018570 BC05.
      *
      *    ****    J O B C A R D   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S30.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570 WITH FOREGROUND-COLOR 11.
018660	     MOVE ZERO		 TO W90-BDT W90-NONTAX W90-LDISC
018670				    W90-BAL W90-SALES
				    W90-CUR W90-EXCTOT.
018680       MOVE 1              TO WS-FIRST WS-LAST.
018690       MOVE SPACES         TO W90-TAX.
             MOVE ZERO           TO W10-JCST.
	     MOVE 1		 TO WS-S9.

       BC05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC05A.

018710 BC10.
             PERFORM AA400.
025260       ACCEPT S30A AT 0510.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-JCST
		     GO TO BC599
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "J"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "N"	 TO WS-ETYPE
		 WHEN F6-KEY
		     MOVE "C"	 TO WS-ETYPE
		 WHEN F7-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC10
	       END-EVALUATE
020770	       PERFORM AA50
020780         DISPLAY S30A AT 0510
020790         IF W10-JCST = ZERO
020800             GO TO BC10.
018850	     DISPLAY CLEAR-L25.
020810     IF W10-JCST = ZERO
020830         GO TO BC599.
             MOVE W10-JCST       TO W90-REF.
       BC12.
             PERFORM GET-JOBCST.
           IF WS-ERROR = 1
	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC10.
           IF WS-ERROR = 2
	       MOVE "Not a job"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 3
	       MOVE "Job corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 4
	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
018820       DISPLAY S36 AT 0308
019040       DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR 11.
019900       DISPLAY W90-ORD AT 0368 WITH FOREGROUND-COLOR 11.
019970       DISPLAY W10-SMAN AT 0411 WITH FOREGROUND-COLOR 11.
      *
      *    ****    G E T   T E C H N I C I A N	 D E T A I L S
      *
020010     IF W10-SMAN NOT = ZERO
033840	       ADD 200 W10-SMAN    GIVING WS-PARKEY
033850         PERFORM READ-PARAM THRU READ-PARAM-EXIT
033860         DISPLAY PAR-TECHNICIAN AT 0420 WITH FOREGROUND-COLOR 11.
	     DISPLAY W40-TEL AT 0466 WITH FOREGROUND-COLOR 11.
             DISPLAY W40-CONTACT AT 0528 WITH FOREGROUND-COLOR 11.
	   IF NOT (W10-IDISP = SPACES)
	       DISPLAY "¸" AT 0720
			W10-IDISP WITH FOREGROUND-COLOR 6 HIGHLIGHT
		       "Õ" AT 0750 WITH FOREGROUND-COLOR 3
					BACKGROUND-COLOR 1.
	     MOVE 1		 TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.

       BC15.
022610       MOVE W90-DB         TO W90-TOTAL.
022620       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022630       MOVE W90-DB         TO W100-BAL.
022640	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
022650	     MULTIPLY -1 BY W90-LDISC
022660                           GIVING W100-BAL.
022670	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
022690	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
022700	       GO TO BC20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC16.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC16.
022740	     MOVE W25-TOTAL	 TO W100-BAL.
022750	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
022760	     ADD W25-TOTAL     TO W90-TOTAL.

022770 BC20.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      *
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *
029120 BC25.
029130       MOVE 8              TO LIN.
029140       PERFORM BC115 UNTIL LIN > 18.
029150       MOVE 8              TO LIN.
029160       MOVE 1              TO WS-LINES WS-FIRST.

029170 BC30.
029180	     MOVE WS-FIRST	 TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
029190 BC35.
029200	   IF WS-SALKEY > WS-LAST
029210         GO TO BC45.
029220       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
034280	   IF SAL-TYPE = "I" OR "R"
031130	       DISPLAY (LIN, 10) SAL-DESC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       DISPLAY (LIN, 5) "("
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
			SAL-TYPE WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 0
			")" WITH FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 0
029260	       GO TO BC40.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
031160	     DISPLAY (LIN, 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
029290	     MOVE SAL-QNT	 TO W10-QUANT.
029300       DISPLAY S26-QNT AT LINE LIN.
029310       MOVE SAL-SELL       TO W10-SELL.
029320       DISPLAY S26-PRC AT LINE LIN.
	   IF SAL-TYPE = "P"
	       DISPLAY ( , 70) "Requisition"
				WITH FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 0
	   ELSE
034370	       MOVE SAL-VAL	 TO W100-EXT
034380	       DISPLAY ( , 70) W100-EXT
			       WITH FOREGROUND-COLOR 3 HIGHLIGHT
				    BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
029350 BC40.
029360     IF LIN < 18
029370         ADD 1             TO LIN WS-SALKEY WS-LINES
029380         GO TO BC35.

       BC45.
             EXIT.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
029390 BC50.
029400	   IF WS-LINES < 11
029410         IF WS-FIRST = 1
029420		   DISPLAY "Print " AT 2526
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "J" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "obcard - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029440         ELSE
029450		   DISPLAY "Print " AT 2508
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "J" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "obcard - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029480             MOVE 2        TO WS-LINES
029490     ELSE
029500     IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029520		   DISPLAY "Print " AT 2512
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "J" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "obcard - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029550             MOVE 1        TO WS-LINES
029560         ELSE
029570		   DISPLAY "Print " AT 2502
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "J" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "obcard - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029600             MOVE 3        TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029630	       DISPLAY "Print " at 2508
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "J" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "obcard - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "P" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "revious screen - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "xit > <"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

029660 BC55.
029670	     ACCEPT WS-OPTION AT 2548
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
029730	   IF NOT (WS-OPTION = "J" OR "E" OR "N" OR "P")
029740         GO TO BC55.
029750     IF WS-OPTION = "E"
029760         GO TO BC05.
029770     IF WS-OPTION = "J"
029780         GO TO BC65.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
029790     IF WS-LINES = 2 OR 3
029800         IF WS-OPTION = "P"
029810             PERFORM BC60
029820		   DISPLAY CLEAR-L25
029830             SUBTRACT 11   FROM WS-FIRST
029840             GO TO BC30.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
029850     IF WS-LINES = 1 OR 3
029860         IF WS-OPTION = "N"
029870             PERFORM BC60
029880		   DISPLAY CLEAR-L25
029890             ADD 11        TO WS-FIRST
029900             GO TO BC30.
029910       GO TO BC55.
      *
      *    ****    C L E A R   S C R E E N
      *
029920 BC60.
029930	     MOVE 8		 TO LIN.
029940       PERFORM BC115 UNTIL LIN > 18.
029950       MOVE 8              TO LIN.
029960       MOVE 1              TO WS-LINES.
      *
      *    ****    R E A D   D E T A I L S   A N D   P R I N T
      *                          J O B C A R D
      *
       BC65.
	     DISPLAY CLEAR-L25.
025200       DISPLAY "Printing Jobcard" AT 2502 WITH
025210		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 6
		      HIGHLIGHT BLINK.
025220       MOVE ZERO           TO WS-SALKEY.
025230       PERFORM AY60 THRU AY999.
      *
      *    ****    R E A D   J O B C S T   F I L E   A N D   P R I N T
      *
026210 BC70.
026330	     MOVE W90-REF	 TO JOB-REF.
             MOVE ZERO           TO JOB-SQN.
             PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
           IF WS-F-ERROR = 13
	       MOVE "Database corrupted"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC599.
	     ADD 1		 TO W90-PICK.
	     PERFORM BB050.

       BC75.
             PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
           IF WS-F-ERROR = 13
               GO TO BC100.
           IF JOB-REF NOT = W90-REF
               GO TO BC100.
	   IF JOB-TYPE < 1
	       GO TO BC75.
	   IF JOB-TYPE = 9
               GO TO BC100.
           IF JOB-TYPE NOT = 1
               IF JOB-TYPE = 2
                   GO TO BC75
               ELSE
		   MOVE JOB-RMKS TO REP-REMK
                   PERFORM BC80
                   GO TO BC75.
	     MOVE JOB-EXT-ITEM	  TO PRT-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE JOB-ITEM	 TO REP-ITM1
	       MOVE "/" 	 TO REP-SL
	       MOVE JOB-ITM	 TO REP-ITM
	   ELSE
	       MOVE JOB-EXT-ITEM TO REP-EXT-ITEM.
	     MOVE JOB-DESC	 TO REP-DESC.
             MOVE JOB-QNT        TO REP-QNT.
           IF JOB-SELL = ZERO
               MOVE "  N/C"      TO REP-NC
           ELSE
               MOVE JOB-SELL     TO REP-PRICE.
             MOVE JOB-VAT        TO REP-VAT.
	     MOVE JOB-VAL	 TO REP-VAL.
       BC80.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       MOVE "JOBCARD - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X"		 TO W02-PRN-TYPE
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       PERFORM BB60 THRU BB65.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BC85.
021370       MOVE 1              TO WS-S6.
021420 BC90.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440	       ADD 1		 TO WS-S6
021450         GO TO BC95.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE PRT-CODE(WS-S6)
				 TO REP-ITM1
	       MOVE "/" 	 TO REP-SL
	       MOVE PRT-PITM(WS-S6)
				 TO REP-ITM
	   ELSE
	       MOVE PRT-EXT-CODE(WS-S6)
				 TO REP-EXT-ITEM.
021480	     MULTIPLY PRT-QUANT(WS-S6) BY JOB-QNT
                                 GIVING REP-QNT.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BC95.
021540       MOVE STK-DESC       TO REP-DESC.
             PERFORM BC80
021600	     ADD 1		 TO WS-S6.

021660 BC95.
021670     IF WS-S6 < 31
021680         GO TO BC90.
             GO TO BC75.
      *
      *    ****    U P D A T E   O R D E R   F I L E 
      *
       BC100.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO REP-DETAIL2.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 8)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
017710	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       MOVE "JOBCARD - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X"		 TO W02-PRN-TYPE
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       PERFORM BB60 THRU BB65
	       MOVE 1		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017790	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "Sub Total:"   TO REP-H5.
	     MOVE JHD-SUBTOT	 TO REP-VAL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF JHD-DSCNT NOT = ZERO
               MOVE "Discount :" TO REP-H5
	       MOVE JHD-DSCNT	 TO REP-VAL
	       PERFORM CALL-PRINTUTL.
	   IF JHD-TAX NOT = ZERO
               MOVE "V.A.T.   :" TO REP-H5
	       MOVE JHD-TAX	 TO REP-VAL
	       PERFORM CALL-PRINTUTL.
	     MOVE "TOTAL    :"	 TO REP-H5.
	     MOVE JHD-TOT	 TO REP-VAL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM AY70 THRU AY999.
	     MOVE SPACES	 TO REP-DETAIL1.
             GO TO BC05.

023520 BC115.
028770	     DISPLAY (LIN, 1) "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3
			       WS-JOB78 WITH FOREGROUND-COLOR 3
			      "³" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3.
023540	     ADD 1		 TO LIN.
       BC599.
             EXIT.
      /
      *    ****    J O B - C A R D S
      *
018500 BD000	     SECTION 51.
018570 BD05.
	     MOVE LS-SLS-JOBNO	 TO W90-REF W10-JCST.
             PERFORM GET-JOBCST.
           IF WS-ERROR NOT = ZERO
	       MOVE "Database corrupted"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD599.
       BD10.
      *
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   
      *                          J O B C A R D
      *
       BD65.
025200      DISPLAY "Printing Jobcard" AT 2502 WITH
025210		     BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
025220       MOVE ZERO           TO WS-SALKEY.
025230       PERFORM AY60 THRU AY999.
026210 BD70.
      *
      *    ****    R E A D   J O B C S T   F I L E   A N D   P R I N T
      *
026330       MOVE W90-REF        TO JOB-REF.
             MOVE ZERO           TO JOB-SQN.
             PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
           IF WS-F-ERROR = 13
	       MOVE "Database corrupted"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD599.
	     ADD 1		 TO W90-PICK.
	     PERFORM BB050.

       BD75.
             PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
           IF WS-F-ERROR = 13
               GO TO BD100.
           IF JOB-REF NOT = W90-REF
               GO TO BD100.
	   IF JOB-TYPE < 1
	       GO TO BD75.
	   IF JOB-TYPE NOT = 1
               IF JOB-TYPE = 2
                   GO TO BD75
               ELSE
		   MOVE JOB-RMKS TO REP-REMK
		   PERFORM BD80
                   GO TO BD75.
	     MOVE JOB-EXT-ITEM	  TO PRT-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE JOB-ITEM	 TO REP-ITM1
	       MOVE "/" 	 TO REP-SL
	       MOVE JOB-ITM	 TO REP-ITM
	   ELSE
	       MOVE JOB-EXT-ITEM TO REP-EXT-ITEM.
	     MOVE JOB-DESC	 TO REP-DESC.
	     MOVE JOB-QNT	 TO REP-QNT.
           IF JOB-SELL = ZERO
               MOVE "  N/C"      TO REP-NC
           ELSE
               MOVE JOB-SELL     TO REP-PRICE.
             MOVE JOB-VAT        TO REP-VAT.
	     MOVE JOB-VAL	 TO REP-VAL.
       BD80.
039710	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       MOVE "JOB-CARD - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X"		 TO W02-PRN-TYPE
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       PERFORM BB60 THRU BB65.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BD85.
021370       MOVE 1              TO WS-S6.
021420 BD90.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440	       ADD 1		 TO WS-S6
021450         GO TO BD95.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE PRT-CODE(WS-S6)
				 TO REP-ITM1
	       MOVE "/" 	 TO REP-SL
	       MOVE PRT-PITM(WS-S6)
				 TO REP-ITM
	   ELSE
	       MOVE PRT-EXT-CODE(WS-S6)
				 TO REP-EXT-ITEM.
021480	     MULTIPLY PRT-QUANT(WS-S6) BY JOB-QNT
                                 GIVING REP-QNT.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BD95.
021540       MOVE STK-DESC       TO REP-DESC.
             PERFORM BD80
021600	     ADD 1		 TO WS-S6.
021660 BD95.
021670     IF WS-S6 < 31
021680         GO TO BD90.
             GO TO BD75.
      *
      *    ****    U P D A T E   O R D E R   F I L E 
      *
       BD100.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "="	 TO REP-DETAIL2.
039710	   IF W02-LINAGE < (W02-PRN-LENGTH - 8)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       MOVE "JOBCARD - CONTINUED ..."
                                 TO REP-H1
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X"		 TO W02-PRN-TYPE
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "H"		 TO W02-PRN-TYPE
	       PERFORM BB60 THRU BB65
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE SPACES	 TO REP-DETAIL1
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	   IF LS-RET = "N"
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       GO TO BD105.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017790	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Sub Total:"	 TO REP-H5.
	     MOVE JHD-SUBTOT	 TO REP-VAL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF JHD-DSCNT NOT = ZERO
               MOVE "Discount :" TO REP-H5
	       MOVE JHD-DSCNT	 TO REP-VAL
	       PERFORM CALL-PRINTUTL.
	   IF JHD-TAX NOT = ZERO
               MOVE "V.A.T.   :" TO REP-H5
	       MOVE JHD-TAX	 TO REP-VAL
	       PERFORM CALL-PRINTUTL.
	     MOVE "TOTAL    :"	 TO REP-H5.
	     MOVE JHD-TOT	 TO REP-VAL.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO BD500.

       BD105.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Dispatch:____________________  Arrive  :_____________
      -		  "_______  Mileage :____________________"
				 TO REP-DETAIL1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Start   :____________________  End     :_____________
      - 	  "_______  Support :____________________"
				 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Details of work done:"
				 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE "_____________________________________________________
      -		  "_______________________________________"
				 TO REP-DETAIL1.
	     MOVE 1		 TO WS-S9.

       BD110.
	     PERFORM CALL-PRINTUTL.
	   IF WS-S9 < 11
	       ADD 1		 TO WS-S9
	       GO TO BD110.
	     MOVE W30-RMKS (1)	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE W30-RMKS (2)	 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE W30-RMKS (3)	 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE W30-RMKS (4)	 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE 4		 TO WS-ADVANCE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM CALL-PRINTUTL.
	     MOVE "CUSTOMER NAME____________________________________   S
      - 	  "IGN___________________________________"
				 TO REP-DETAIL1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "DATE RECEIVED :"
				 TO REP-DETAIL1.
	     MOVE W12-TODAY	 TO W10-EDTE.
	     MOVE W10-DATE	 TO REP-DESC.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
       BD500.
             PERFORM AY70 THRU AY999.
       BD599.
             EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SALEJ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       ZA01B.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-VAT-REG	 TO W05-VATNO.

       ZA02.
050280       OPEN OUTPUT SALE.
050290       CLOSE SALE.
050300       OPEN I-O SALE.
050490       MOVE 9              TO WS-PARKEY.
050500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
050750	     MOVE PAR-POST-CDE	 TO W95-POST.
	     MOVE SPACES	 TO W95-ADDRESS.
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
	     MOVE 1		 TO WS-S1.
	   IF W95-ADD1 = SPACES
	       GO TO ZA35.
       ZA05.
	     MOVE 30		 TO WS-S3.
	     MOVE 1		 TO WS-S2.
       ZA10.
	   IF W95-CHR1 (WS-S3) = SPACE
	       SUBTRACT 1	 FROM WS-S3
	       GO TO ZA10.
       ZA15.
	     MOVE W95-CHR1 (WS-S2)
				 TO W95-CHAR (WS-S1).
	   IF WS-S2 < WS-S3
	       ADD 1		 TO WS-S1 WS-S2
	       GO TO ZA15.
	     ADD 1		 TO WS-S1.
       ZA20.
	   IF W95-ADD2 = SPACES
	       GO TO ZA35.
	     MOVE W95-ADD2	 TO W95-ADD1.
       ZA25.
	     MOVE ","		 TO W95-CHAR (WS-S1).
	     ADD 1		 TO WS-S1.
	     PERFORM ZA05 THRU ZA15.
       ZA30.
	   IF W95-ADD3 = SPACES
	       GO TO ZA35.
	     MOVE W95-ADD3	 TO W95-ADD1.
	     PERFORM ZA25.
	   IF W95-ADD4 = SPACES
	       GO TO ZA35.
	     MOVE W95-ADD4	 TO W95-ADD1.
	     PERFORM ZA25.
       ZA35.
050890	     MOVE 7		 TO WS-PARKEY.
050900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050910       MOVE PAR-CASH-AC    TO WS-CASH-AC.
	     MOVE 281		 TO WS-PARKEY.
	     MOVE 1		 TO WS-S1.

       ZA40.
008700	     READ PARAM IGNORE LOCK.
008710	   IF NOT (WS-STATUS = "00")
	       MOVE SPACES	 TO PAR-JOB-RMKS
008720	       GO TO ZA50.
	     MOVE PAR-JOB-RMKS	 TO W30-RMKS (WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
				    WS-PARKEY
	       GO TO ZA40.
	     GO TO ZA999.

       ZA50.
050900	     WRITE PAR-RECORD1.
	     MOVE PAR-JOB-RMKS	 TO W30-RMKS (WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
				    WS-PARKEY
	       GO TO ZA50.

051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	91.
051650 OPEN-ERROR.
051660       DISPLAY CLR-SCREEN.
051670       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
051680       PERFORM DISPLAY-FILE-NAME.
051690
051700 READ-ERROR.
051710       DISPLAY CLR-SCREEN.
051720       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051740
051750 WRITE-ERROR.
051760       DISPLAY CLR-SCREEN.
051770       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051780

051790 DISPLAY-FILE-NAME.
051840     IF WS-F-ERROR = 2
051850         MOVE W02-NETWORK  TO WS-FILE
051860         MOVE WS-NETKEY    TO WS-KEY
051910     ELSE
051920     IF WS-F-ERROR = 6
051930	       MOVE W02-DEBTOR	 TO WS-FILE
051940	       MOVE ZERO	 TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070	   ELSE
041080     IF WS-F-ERROR = 7
041090         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
052190	   ELSE
052200     IF WS-F-ERROR = 13
052210         MOVE W02-JOBCST   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
041110	   ELSE
041240	   IF WS-F-ERROR = 14
041250	       MOVE W02-JOBHDR	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE JHD-KEY2	 TO WS-KEYX
052070	   ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170         MOVE W02-SALE TO WS-FILE
052180         MOVE WS-SALKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
052190     ELSE
052200     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 33
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
052310     IF WS-STATUS = "10"
052320         MOVE "End of FILE" TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380         MOVE "Invalid record number" TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410         MOVE "DISK full" TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470         MOVE "FILE locked" TO WS-STAT-MESSAGE.
052480       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
052490       DISPLAY "Status " AT 1212 
052500                WS-STATUS WITH FOREGROUND-COLOR 11
052510               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
052520     IF WS-STATUS NOT = "94" 
052530         DISPLAY "Key    " AT 1412 
052540                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
