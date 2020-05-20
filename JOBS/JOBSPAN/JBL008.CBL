      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   ********  *******   **	  ****	    ****     ******    *
      *      **     **	  **  **	 **  **    **  **   **	  **   *
      *      **     **	  **  **	**    **  **	**  **	  **   *
      *      **     *******   **	**    **  **	**   ******    *
      *      **     **	  **  **	**    **  **	**  **	  **   *
      *   ** **     **	  **  **	 **  **    **  **   **	  **   *
      *    ***	    *******   ********	  ****	    ****     ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *    T I M E   S H E E T S   P E R   T E C H N I C I A N	       *
      *                                                                *
      *     VERSION 8.00 - August 2000				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP008.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY JOBTECH.SL.

000270 COPY PARAM.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY JOBTECH.FDE.

000560 COPY PARAM.FDE.

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
002300 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002400 77  WS-PRN1         PIC  9(01) VALUE 0.  
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002420 77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
002430 77  WS-START        PIC  9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2         PIC  9(08).
002470 77  WS-PORT	   PIC	9(05) VALUE 1004.
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
002590 77  WS-OPTION	   PIC	X(01).
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002640 77  WS-DEM          PIC  9(01).
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP	   PIC	9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002830 77  WS-BATCH	   PIC	9(05).
002880 77  WS-PER	   PIC	Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC S9(07)V9(08) COMP-3.
       77  WS-REPHD	   PIC	X(22).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002910 01  WS-DB-LINE.
002920     03  WS-TOP-LINE.
002930         05  WS-TCHAR PIC X(01) OCCURS 80.
002940     03  WS-TOP-LNE2.
002950         05  WS-TCR  PIC X(01) OCCURS 80.
002960     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
002970         05  FILLER      PIC  X(01).
002980         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003000	   03  WS-MID-LINE.
003010         05  WS-MCHAR PIC X(01) OCCURS 80.
003020     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
003040	   03  WS-BOT-LINE.
003050         05  WS-BCHAR PIC X(01) OCCURS 80.
003060     03  WS-BOT-LNE2.
003070         05  WS-BCR  PIC X(01) OCCURS 80.
003080     03  WS-TOP-LNE.
003090         05  WS-TCHR PIC X(01) OCCURS 80.
003100     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003110         05  FILLER  PIC X(01).
003120         05  WS-H-LINE
003130                     PIC X(78).
003140         05  FILLER  PIC X(01).
003150     03  WS-MID-LNE.
003160         05  WS-MCHR PIC X(01) OCCURS 80.
003170     03  WS-BOT-LNE.
003180         05  WS-BCHR PIC X(01) OCCURS 80.
003190     03  WS-TP-LNE.
003200         05  WS-TCH  PIC X(01) OCCURS 80.
003210     03  WS-MD-LNE.
003220         05  WS-MCH  PIC X(01) OCCURS 80.
003230     03  WS-BT-LNE.
003240         05  WS-BCH  PIC X(01) OCCURS 80.
003190     03  WS-TP-LINE.
003200         05  WS-TCA  PIC X(01) OCCURS 80.
003210     03  WS-MD-LINE.
003220         05  WS-MCA  PIC X(01) OCCURS 80.
	   03  WS-JB78-L REDEFINES WS-MD-LINE.
	       05  WS-MJB1     PIC  X(01).
	       05  WS-MJB78    PIC  X(78).
	       05  WS-MJB80    PIC  X(01).
003230	   03  WS-BT-LINE.
003240         05  WS-BCA  PIC X(01) OCCURS 80.

003340 COPY WS.WS.

       01  WS-SLS-JOB.
           03  WS-SLS-JOBNO    PIC  X(08) VALUE SPACES.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003530 01  W02-FID.

       COPY APACFIDS.ID.

       COPY JOBTECH.ID.

       COPY PARAM.ID.

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
000390	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000400	       05  REP-DETAIL1 PIC  X(140).
000410	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000420	       05  REP-DETAIL2 PIC  X(80).

	   03  REP-L1 REDEFINES W02-PRN-LINE.
	       05  REP-DET1	   PIC X(96).

	   03  REP-L3 REDEFINES W02-PRN-LINE.
	       05  REP-DATE	   PIC	X(06).
	       05  REP-DTE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(03).
	       05  REP-COMPANY	   PIC	X(40).
	       05  FILLER	   PIC	X(02).
	       05  REP-HEAD	   PIC	X(22).
	       05  FILLER	   PIC	X(03).
	       05  REP-PAGE	   PIC	X(06).
	       05  REP-P-NO	   PIC	Z(03)9.

	   03  REP-L3A REDEFINES W02-PRN-LINE.
	       05  REP-CO	   PIC X(40).

	   03  REP-L4 REDEFINES W02-PRN-LINE.
	       05  REP-H1	   PIC	X(12).
	       05  REP-TECH-NO	   PIC	ZZ9.
	       05  FILLER	   PIC	X(01).
	       05  REP-TECH-NAME   PIC	X(40).
	       05  FILLER	   PIC	X(76).

	   03  REP-L5 REDEFINES W02-PRN-LINE.
	       05  REP-H2	   PIC X(20).
	       05  REP-H3	   PIC X(22).
	       05  REP-H4	   PIC X(07).
	       05  REP-H5	   PIC X(41).
	       05  REP-H6	   PIC X(09).
	       05  REP-H7	   PIC X(33).

	   03  REP-L5 REDEFINES W02-PRN-LINE.
	       05  REP-SDTE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  REP-STME	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  REP-EDTE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  REP-ETME	   PIC	X(08).
	       05  REP-TIME	   PIC	X(09).
	       05  FILLER	   PIC	X(01).
	       05  REP-ITEM	   PIC	X(40).
	       05  FILLER	   PIC	X(01).
	       05  REP-REF	   PIC	9(08).
	       05  FILLER	   PIC	X(01).
	       05  REP-NAME	   PIC	X(33).

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

003120 01  W11-PARTS.
003130     03  W11-PART-DETAILS OCCURS 30.
003140         05  W11-CODE   PIC X(14).
003150         05  W11-DESC   PIC X(30).
003160         05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY W12.WS.

       COPY W15.WS.

       COPY W20.WS.

004520 01  W25-CALCS.
004730	   03  W25-RESULT      PIC  9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750	       05  W25-WHOLE   PIC  9(03).
004760	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
004570	   03  W25-TECH-TIME   PIC  9(08)    COMP-3.
004580	   03  W25-TOTAL       PIC  9(10)    COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690	       05  W25-HH      PIC  9(02).
001690	       05  W25-MM      PIC  9(02).
001690	       05  W25-SS      PIC  9(02).
001680	       05  FILLER      PIC  9(02).
	   03  W25-HMS REDEFINES W25-TIME.
	       05  W25-HHMMSS  PIC  9(06).
	       05  FILLER      PIC  9(02).
	   03  W25-D-TIME.
	       05  W25-H       PIC  ZZ9.
	       05  FILLER      PIC  X(01) VALUE ":".
	       05  W25-M       PIC  9(02).
	       05  FILLER      PIC  X(01) VALUE ":".
	       05  W25-S       PIC  9(02).
	   03  W25-D-RTIME REDEFINES W25-D-TIME.
	       05  FILLER      PIC  X(01).
	       05  W25-D-HMS   PIC  X(08).

       01  W30-TIME-CONVERT.
	   03  W30-TIME        PIC  9(06).
	   03  W30-RTIME REDEFINES W30-TIME.
	       05  W30-HH      PIC  9(02).
	       05  W30-MM      PIC  9(02).
	       05  W30-SS      PIC  9(02).
	   03  W30-SECONDS     PIC  9(06) COMP-3.
	   03  W30-START       PIC  9(06) COMP-3.
	   03  W30-END	       PIC  9(06) COMP-3.
	   03  W30-T-TIME      PIC  9(07).
	   03  W30-T-RTIME REDEFINES W30-T-TIME.
	       05  W30-T-HH    PIC  9(03).
	       05  W30-T-MM    PIC  9(02).
	       05  W30-T-SS    PIC  9(02).

       01  W35-TECHNICIAN.
	   03  W35-START       PIC  9(02) COMP-0.
	   03  W35-END	       PIC  9(02) COMP-0.
	   03  W35-TECH1       PIC  X(30).
	   03  W35-TECH2       PIC  X(30).
	   03  W35-TCHNO       PIC  9(02).

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
004790	   03  W75-DNO	 OCCURS 18.
	       05  W75-TECH   PIC  9(02) COMP-5.
	       05  FILLER     PIC  X(05).

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

007420 01  S-PROG.
007430     03  LINE 2  COLUMN 30 FOREGROUND-COLOR 15
007440				 VALUE "TECHNICIAN TIME SHEETS".

       01  S03.
	   03  LINE  4 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
	   03  LINE  5 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   ".
	   03	       COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		       "T E C H N I C I A N   S E L E C T I O N".
	   03	       COLUMN 60 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				 "    �".
	   03  LINE  6 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�  ".
	   03	       COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		       "Keyin the Technician number range required".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				" �" .
	   03  LINE  7 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "쿞tart with:                                   �".
	   03  LINE  8 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "쿐nd with  :                                   �".
	   03  LINE  9 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".

       01  S04.
	   02  S04A.
	   03  LINE  7 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9(02) USING W35-START AUTO.
	   02  S04B.
	   03  LINE  7 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W35-TECH1.
	   02  S04C.
	   03  LINE  8 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9(02) USING W35-END AUTO.
	   02  S04D.
	   03  LINE  8 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W35-TECH2.

009340 01  S05D.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   Job Time Sheet for Period.   �".
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   Starting Date :              �".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   Ending Date   :              �".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쿏ates to be keyed in as ".
	   03	       COLUMN 49 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "�".
	   03  LINE 11 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".

       01  S05A.
	   03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S08.
           03  LINE 4  COLUMN 12 VALUE "Do you wish to continue (".
           03          COLUMN 37 FOREGROUND-COLOR 11 VALUE "Y".
           03          COLUMN 38 VALUE "/".
           03          COLUMN 39 FOREGROUND-COLOR 11 VALUE "N".
           03          COLUMN 40 VALUE ")? > <".

      *COPY JOBLUP.CRT.

       COPY TECHNIC.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-JBLEV < 3
	       DISPLAY CLEAR-L25
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA05.
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
	     PERFORM BE000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE JOBTECH.

       AA05.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY DATE.CHK.

       COPY PRINTUTL.AP9.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000           SECTION.

011290 COPY PARAM.RD.

011270 COPY JOBTECH.RD.

       COPY TECHNIC.LUP.

      /
       AE000           SECTION.
       AE00.
      *
      *    ****   1 2	C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
009860	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     ADD 1		 TO WS-PAGE.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
             MOVE W95-COMP       TO REP-COMPANY.
	     MOVE WS-REPHD	 TO REP-HEAD.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 7	C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "________From_______"  TO REP-H2.
	     MOVE "_________To________"	 TO REP-H3.
	     MOVE "Time"		 TO REP-H4.
	     MOVE "Item Description"	 TO REP-H5.
	     MOVE "Job. No."		 TO REP-H6.
	     MOVE "Client Name"		 TO REP-H7.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       AE999.
             EXIT.
      /
      *    ****    T E C H I C I A N   T I M E	 S H E E T S
      *
       BE000	       SECTION 50.
       BE00.
             DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
             DISPLAY S08.
             MOVE SPACE          TO WS-OPTION.

       BE05.
             ACCEPT WS-OPTION AT 0444 
                    WITH FOREGROUND-COLOR 15 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO BE05.
           IF WS-OPTION = "N"
               GO TO BE999.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       BE10.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BE10
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BE10.
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE10.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE10.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE10.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE W15-S-YMD	 TO TIM-DATE.
	     MOVE ZERO		 TO TIM-TECH W35-START
				    TIM-TIME W35-END
				    W25-TOTAL W25-TECH-TIME.
014600       MOVE ZERO           TO WS-ERROR.
	     PERFORM START-AT-TIM-KEY THRU READ-JOBTECH-EXIT.
014620	   IF WS-F-ERROR = 61
	       MOVE "No data for period"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE00.
	     PERFORM SAVE-SCREEN-3.

       BE15.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 46		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY S03.
	     PERFORM SCREEN-SHADOW.
	     MOVE 01		 TO W35-START.
	     MOVE 50		 TO W35-END.
	     MOVE "'First'"	 TO W35-TECH1.
	     MOVE "'Last'"	 TO W35-TECH2.
	     DISPLAY S04.

       BE20.
	     DISPLAY "F2" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=View Technician Codes, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S04A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE999
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TECHNICIAN-LOOKUP
		     MOVE WS-S7  TO W35-START
011320		     PERFORM RESTORE-SCREEN
		     GO TO BE20
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE20
	       END-EVALUATE.
	   IF W35-START > 50
	       MOVE "Invalid Technician code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE20.
	   IF W35-START = ZERO
	       GO TO BE999.
	     ADD 200 W35-START GIVING WS-PARKEY
010860	     PERFORM READ-PARAM THRU READ-PARAM-EXIT
	   IF PAR-TECHNICIAN = SPACES
	       IF W35-START > 1
		   MOVE "Invalid Technician start code"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BE20
	       END-IF
	   ELSE
	      MOVE PAR-TECHNICIAN
				 TO W35-TECH1.
	     DISPLAY S04B.

       BE25.
	     ACCEPT S04C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BE20
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TECHNICIAN-LOOKUP
		     MOVE WS-S7  TO W35-END
011320		     PERFORM RESTORE-SCREEN
		     GO TO BE25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE25
	       END-EVALUATE.
	   IF W35-END > 50
	       MOVE "Invalid Technician end code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE25.
	   IF W35-END < W35-START
	       MOVE "End code before Start code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE25.
	   IF W35-END = ZERO
	       GO TO BE20.
	     ADD 200 W35-END	 GIVING WS-PARKEY
010860	     PERFORM READ-PARAM THRU READ-PARAM-EXIT
	   IF PAR-TECHNICIAN = SPACES
	       IF W35-END < 50
		   MOVE "Invalid Technician code"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BE25
	       END-IF
	   ELSE
	      MOVE PAR-TECHNICIAN
				 TO W35-TECH2.
	     DISPLAY S04D.

       BE30.
	     MOVE W35-START	 TO TIM-TECH.
	     PERFORM START-AT-TIM-KEY THRU READ-JOBTECH-EXIT.
014620	   IF WS-F-ERROR = 61
	       MOVE "No data for period"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE00.
             MOVE ZERO           TO WS-PAGE.

       BE35.
	     PERFORM READ-JOBTECH-NEXT THRU READ-JOBTECH-EXIT.
014620	   IF WS-F-ERROR = 61
	       PERFORM BE40
	       PERFORM BE45
	       GO TO BE50.
	   IF TIM-TECH > W35-END
	       IF WS-PAGE = ZERO
		   MOVE "No data for period"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BE00
	       ELSE
		   PERFORM BE40
		   PERFORM BE45
		   GO TO BE50.
	   IF WS-PAGE = ZERO
      *
      * 			  R E P O R T	H E A D I N G
      *
	       PERFORM AE000
      *
      * 			  T E C H N I C I A N	H E A D I N G
      *
	       PERFORM BE55.
	   IF TIM-TECH > W35-TCHNO
      *
      * 			  T E C H N I C I A N	T O T A L
      *
	       PERFORM BE40
      *
      * 			  F I R S T   R E C O R D  F O R
      * 			  N E X T   T E C H N I C I A N
      *
	       PERFORM BE45
	       IF WS-ERROR = 1
		   GO TO BE50
	       END-IF
      *
      * 			  T E C H N I C I A N	H E A D I N G
      *
	       PERFORM BE55.
	   IF TIM-DATE > W15-E-YMD
	       GO TO BE35.
	   IF TIM-E-TIME = ZERO
012150	       ACCEPT W25-TIME FROM TIME
	       MOVE W25-HHMMSS	 TO TIM-E-TIME
	       MOVE W12-TODAY	 TO TIM-E-DATE.
      *
      *    ****    C A L C U L A T E   T I M E	 W O R K E D
      *
	     MOVE TIM-TIME	 TO W30-TIME.
	     MOVE W30-SS	 TO W30-SECONDS.
	     COMPUTE W30-SECONDS = W30-SECONDS + (W30-MM * 60).
	     COMPUTE W30-SECONDS = W30-SECONDS + (W30-HH * 3600).
	     MOVE W30-SECONDS	 TO W30-START.
	     MOVE TIM-E-TIME	 TO W30-TIME.
      *
      *    ****    C H E C K   F O R   M I D N I G H T
      *
      * 	  (ASSUME 1 DAY - LABOUR LAWS & BREAKS ETC WOULD NOT
      * 	   ALLOW FOR MORE)
      *
	   IF TIM-TIME > TIM-E-TIME
	       ADD 24		 TO W30-HH.
	     MOVE W30-SS	 TO W30-SECONDS.
	     COMPUTE W30-SECONDS = W30-SECONDS + (W30-MM * 60).
	     COMPUTE W30-SECONDS = W30-SECONDS + (W30-HH * 3600).
	     MOVE W30-SECONDS	 TO W30-END.
	     SUBTRACT W30-START FROM W30-END
				 GIVING W30-SECONDS.
	     ADD W30-SECONDS	 TO W25-TECH-TIME.
	     MOVE ZERO		 TO W30-TIME.
	   IF W30-SECONDS > 3600
	       DIVIDE W30-SECONDS BY 3600
				 GIVING W30-HH
				 REMAINDER W30-SECONDS.
	   IF W30-SECONDS > 60
	       DIVIDE W30-SECONDS BY 60
				 GIVING W30-MM
				 REMAINDER W30-SECONDS.
	     MOVE W30-SECONDS	 TO W30-SS.
	     MOVE TIM-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO REP-SDTE.
      *
      *    ****    S T A R T   T I M E
      *
	     MOVE TIM-TIME	 TO W25-HHMMSS.
	     MOVE W25-HH	 TO W25-H.
	     MOVE W25-MM	 TO W25-M.
	     MOVE W25-SS	 TO W25-S.
	     MOVE W25-D-HMS	 TO REP-STME.
	     MOVE TIM-E-DATE	 TO REP-EDTE.
      *
      *    ****    E N D   T I M E
      *
	     MOVE TIM-E-TIME	 TO W25-HHMMSS.
	     MOVE W25-HH	 TO W25-H.
	     MOVE W25-MM	 TO W25-M.
	     MOVE W25-SS	 TO W25-S.
	     MOVE W25-D-HMS	 TO REP-ETME.
      *
      *    ****    T O T A L   T I M E
      *
	     MOVE W30-TIME	 TO W25-HHMMSS.
	     MOVE W25-HH	 TO W25-H.
	     MOVE W25-MM	 TO W25-M.
	     MOVE W25-SS	 TO W25-S.
	     MOVE W25-D-TIME	 TO REP-TIME.
	     MOVE TIM-ITEM	 TO REP-ITEM.
	     MOVE TIM-JOB	 TO REP-REF.
	     MOVE TIM-NAME	 TO REP-NAME.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
      *
      * 			  R E P O R T	H E A D I N G
      *
	       PERFORM AE000
      *
      * 			  T E C H N I C I A N	H E A D I N G
      *
	       PERFORM BE55.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "X" 	 TO WS-OPTION
014990	       GO TO BE999.
	     GO TO BE35.
      *
      *    ****    P R I N T   T E C H N I C I A N   T O T A L S
      *
       BE40.
	     MOVE "      **** TECHNICIAN TOTAL"
				 TO REP-DETAIL1.
	     ADD W25-TECH-TIME	 TO W25-TOTAL.
	     MOVE ZERO		 TO W30-T-TIME.
	   IF W25-TECH-TIME > 3600
	       DIVIDE W25-TECH-TIME BY 3600
				 GIVING W30-T-HH
				 REMAINDER W25-TECH-TIME.
	   IF W25-TECH-TIME > 60
	       DIVIDE W25-TECH-TIME BY 60
				 GIVING W30-T-MM
				 REMAINDER W25-TECH-TIME.
	     MOVE W25-TECH-TIME	 TO W30-T-SS.
	     MOVE W30-T-HH	 TO W25-H.
	     MOVE W30-T-MM	 TO W25-M.
	     MOVE W30-T-SS	 TO W25-S.
	     MOVE W25-D-TIME	 TO REP-TIME.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 5)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
      *
      * 			  R E P O R T	H E A D I N G
      *
	       PERFORM AE000.
	     MOVE SPACES	 TO REP-DETAIL1.

       BE40-E.
	     EXIT.
      *
      *    ****    G E T   T H E   N E X T   T E C H N I C I A N
      *
       BE45.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE W15-S-YMD	 TO TIM-DATE.
	     MOVE ZERO		 TO W25-TECH-TIME.
	     PERFORM START-AT-TIM-KEY THRU READ-JOBTECH-EXIT.
014620	   IF WS-F-ERROR = 61
	       MOVE 1		 TO WS-ERROR
	   ELSE
	       PERFORM READ-JOBTECH-NEXT THRU READ-JOBTECH-EXIT.

       BE45-E.
	     EXIT.
      *
      *    ****    P R I N T   G R A N D   T O T A L S
      *
       BE50.
	     MOVE "      ****  TOTAL FOR ALL TECHNICIANS"
				 TO REP-DETAIL1.
	     MOVE ZERO		 TO W30-T-TIME.
	   IF W25-TOTAL > 3600
	       DIVIDE W25-TOTAL BY 3600
				 GIVING W30-T-HH
				 REMAINDER W25-TOTAL.
	   IF W25-TOTAL > 60
	       DIVIDE W25-TOTAL BY 60
				 GIVING W30-T-MM
				 REMAINDER W25-TOTAL.
	     MOVE W25-TOTAL	 TO W30-T-SS.
	     MOVE W30-T-HH	 TO W25-H.
	     MOVE W30-T-MM	 TO W25-M.
	     MOVE W30-T-SS	 TO W25-S.
	     MOVE W25-D-TIME	 TO REP-TIME.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO BE999.
      *
      *    ****    P R I N T   T E C H N I C I A N   H E A D I N G S
      *
       BE55.
	     MOVE TIM-TECH	 TO W35-TCHNO.
026150	     ADD 200 W35-TCHNO	 GIVING WS-PARKEY.
026160	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE "Technician:"  TO REP-H1.
	     MOVE PAR-TECHNICIAN TO REP-TECH-NAME.
	     MOVE W35-TCHNO	 TO REP-TECH-NO.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.

       BE999.
             EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
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
	     MOVE "JOBTECH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBTECH.
	     OPEN I-O JOBTECH.
	   IF WS-STATUS = "05"
	       CLOSE JOBTECH
	       OPEN OUTPUT JOBTECH
	       CLOSE JOBTECH
	       OPEN I-O JOBTECH.
038980	   IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050	       IF IDX-CORRUPT
039060		   MOVE 61	 TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090	       MOVE 61		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
039100		   PERFORM OPEN-ERROR.

       ZA02.
050580       MOVE 1              TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
050640       MOVE PAR-TELEPHONE  TO W95-TEL.
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
051530       GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
039860	   IF WS-F-ERROR = 61
039870	       MOVE "TIME LOG file"
039880				 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
051090       MOVE 1 TO WS-S1.
051100       MOVE SPACES         TO WS-MID-LINE WS-MID-LNE 
				    WS-MD-LINE
                                    WS-MD-LNE.
051110 ZA60.
051120       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
051130       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
051140       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
051140       MOVE WS-G1          TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
051150       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
051160     IF WS-S1 < 80
051170         ADD 1 TO WS-S1
051180         GO TO ZA60.
051190       MOVE WS-G9          TO WS-TCR   (1).
051200       MOVE WS-G10         TO WS-TCR   (80).
051210       MOVE WS-G11         TO WS-BCR   (1).
051220       MOVE WS-G12         TO WS-BCR   (80).
051230       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
051240                              WS-TCHR  (1)  WS-BCHR  (1)
051240                              WS-TCA   (1)  WS-BCA   (1)
051250                              WS-TCH   (1)  WS-BCH   (1).
051260       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
051270                              WS-TCHR  (80) WS-BCHR  (80)
051270                              WS-TCA   (80) WS-BCA   (80)
051280                              WS-TCH   (80) WS-BCH   (80).
051290       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
051300                              WS-TCHAR (50) WS-TCHAR (59) 
051310                              WS-TCHAR (69)
051320                              WS-TCHR  (18) WS-TCHR  (49)
051330                              WS-TCHR  (60) WS-TCHR  (70)
051340                              WS-TCA   (10) WS-TCA   (17)
051340                              WS-TCA   (47) WS-TCA   (59)
051340                              WS-TCA   (68)
051340                              WS-TCH   (8)  WS-TCH   (49)
051350                              WS-TCH   (61) WS-TCH   (72).
051360       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
051370                              WS-MCHAR (50) WS-MCHAR (59) 
051380                              WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
051390                              WS-MCH   (8)  WS-MCH   (49)
051400                              WS-MCH   (61) WS-MCH   (72)
                                    WS-MCA   (1)  WS-MCA   (80)
051340                              WS-MCA   (10) WS-MCA   (17)
051340                              WS-MCA   (47) WS-MCA   (59)
051340                              WS-MCA   (68)
051420				    WS-MCHR  (18) WS-MCHR  (49)
051430                              WS-MCHR  (60) WS-MCHR  (70)
051440                              WS-MCHR  (1)  WS-MCHR  (80) 
051450                              WS-MCH   (1)  WS-MCH   (80).
051460       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
051470                              WS-BCHAR (50) WS-BCHAR (59) 
051480                              WS-BCHAR (69)
051490                              WS-BCHR  (18) WS-BCHR  (49)
051500                              WS-BCHR  (60) WS-BCHR  (70)
051340                              WS-BCA   (10) WS-BCA   (17)
051340                              WS-BCA   (47) WS-BCA   (59)
051340                              WS-BCA   (68)
051510                              WS-BCH   (8)  WS-BCH   (49)
051520                              WS-BCH   (61) WS-BCH   (72).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = WS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
051540 ZA200.
051550       DISPLAY "Files locked - Try later" AT 2312
051560                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
051570       DISPLAY "Press any key" AT 2512 
051580                WITH FOREGROUND-COLOR 14.
051590       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
051600       EXIT PROGRAM.
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
052080	   IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100	       MOVE WS-PARKEY	 TO WS-KEY
041110	   ELSE
041240	   IF WS-F-ERROR = 61
041250	       MOVE W02-JOBTECH	 TO WS-FILE
041260         MOVE ZERO         TO WS-KEY
	       MOVE TIM-KEY	 TO WS-KEYX.
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
