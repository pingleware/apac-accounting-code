      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******      **      *
      *   **	**     **     **    **	 **  **   **	**    ***      *
      *   **	       **     **    **	**    **	**     **      *
      *    ******      **     *******	**    **      ***      **      *
      * 	**     **     **	**    **	**     **      *
      *   **	**     **     **	 **  **   **	**     **      *
      *    ******      **     **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     T R A N S F E R   S T O C K   I N	 (BRANCH TO BRANCH)    *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP031.
000030 AUTHOR.	       J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2010
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

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000140 COPY STOCK.SL.

       COPY STKDEX.SL.

       COPY STKTRF.SL.

       COPY STKWRK.SL.

       COPY TRFRIN.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000380 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000300 COPY STOCK.FDE.

       COPY STKDEX.FDE.

       COPY STKTRF.FD.

       COPY STKWRK.FD.

       COPY TRFRIN.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	    PIC 999V99	  COMP-3.
001710 77  WS-SUB           PIC 9(04)     COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001840 77  WS-TRFKEY        PIC 9(06)     COMP-5.
001840 77  WS-PRCKEY	    PIC 9(06)	  COMP-5.
001860 77  WS-PARKEY        PIC 9(06)     COMP-5.
001870 77  WS-NETKEY        PIC 9(06)     COMP-5.
001890 77  WS-RECKEY        PIC 9(06)     COMP-5.
001900 77  WS-RECOVER       PIC 9(06)     COMP-5.
001900 77  WS-RECORDS       PIC 9(06)     COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	    PIC 9(04)	  COMP-5.
001940 77  WS-MONTHS	    PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001980 77  WS-ITM	   PIC	X(18).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-BRNCH	   PIC	X(03).
       77  WS-BRANCH	   PIC	X(38).
       77  WS-ETYPE         PIC X(01).
002010 77  WS-PAGE          PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-HND-FEE	   PIC	X(01).
       77  WS-SELL	   PIC	X(01).
       77  WS-NEW	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-TOTAL	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01)	 VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-OPASSWORD    PIC	X(06).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-LABCDE	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC		X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-HFEE	   PIC	9(03)V999 COMP-3.
       77  WS-BR-TO	   PIC	X(03).
       77  WS-BR-CODE	   PIC	X(03).
       77  WS-REF	   PIC	X(07).
       77  WS-BR-NAME	   PIC	X(38).
       77  WS-BR-NTO	   PIC	X(38).
       77  WS-DATE	   PIC	9(08)	  COMP.
       77  WS-VALUE	   PIC S9(10)V999 COMP-3.
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
002460         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
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

002330 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000300 COPY STOCK.ID.

       COPY STKDEX.ID.

       COPY STKTRF.ID.

       COPY STKWRK.ID.

       COPY TRFRIN.ID.

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
000420	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000430	       05  REP-DETAIL1	   PIC X(96).
000440	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000450	       05  REP-DETAIL2	   PIC X(96).
000460	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000520	       05  REP-DATE	   PIC X(06).
000530	       05  REP-DTE	   PIC Z9/99/9999.
000490	       05  FILLER	   PIC X(12).
000500	       05  REP-COMPANY	   PIC X(40).
000510	       05  FILLER	   PIC X(18).
000470	       05  REP-PAGE	   PIC X(06).
000480	       05  REP-P-NO	   PIC Z(03)9.
000810	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
000820	       05  FILLER	   PIC X(40).
000860	       05  REP-HEAD10	   PIC X(16).
000870	       05  FILLER	   PIC X(40).
	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(14).
	       05  REP-TRF-HD	   PIC	X(15).
	       05  REP-BRANCH	   PIC	X(40).
	       05  FILLER	   PIC	X(27).
001490	   03  ORD-LINE1 REDEFINES W02-PRN-LINE.
001500	       05  ORD-H1	   PIC	X(19).
001510	       05  ORD-H2	   PIC	X(35).
001520	       05  ORD-H3	   PIC	X(11).
001530	       05  ORD-H4	   PIC	X(11).
001540	       05  ORD-H5	   PIC	X(10).
001550	       05  ORD-H6	   PIC	X(10).
001560	   03  ORD-LINE2 REDEFINES W02-PRN-LINE.
001570	       05  ORD-ITEM	   PIC	X(18).
	       05  ORD-ITM REDEFINES ORD-ITEM
				   PIC	X(14)/X(03).
001580	       05  FILLER	   PIC	X(01).
001590	       05  ORD-DESC	   PIC	X(30).
001600	       05  FILLER	   PIC	X(01).
001610	       05  ORD-QUANT	   PIC	Z(06)9.9999.
001620	       05  FILLER	   PIC	X(01).
001630	       05  ORD-COST	   PIC	Z(06)9.999.
001640	       05  FILLER	   PIC	X(01).
001650	       05  ORD-SELL	   PIC	Z(06)9.99.
001660	       05  FILLER	   PIC	X(01).
001670	       05  ORD-VALUE	   PIC	Z(06)9.99.
	   03  TOT-LINE1 REDEFINES W02-PRN-LINE.
	       05  TOT-H1	   PIC	X(14).
	       05  TOT-H2	   PIC	X(32).
	       05  TOT-H3	   PIC	X(12).
	       05  TOT-H4	   PIC	X(38).
	   03  TOT-LINE2 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(04).
	       05  TOT-SL-NO	   PIC	9(03).
	       05  FILLER	   PIC	X(07).
	       05  TOT-SL-NAME	   PIC	X(30).
	       05  FILLER	   PIC	X(03).
	       05  TOT-STK-GL	   PIC	9(04)/99.
	       05  FILLER	   PIC	X(03).
	       05  TOT-SL-VALUE    PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(28).

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
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

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
	   03  W25-HND-FEE    PIC S9(09)V99 COMP-3.
	   03  W25-TH-FEE     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

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

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S10.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.999 USING WS-HFEE AUTO.

003880 01  S20.
	   03  LINE 11 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		    VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003890	   03  LINE 12 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		    VALUE "³ Enter Branch Override Password        ³".
	   03  LINE 13 COLUMN 20 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
		    VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
003910	   03  LINE 12 COLUMN 53 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC  X(06)
				 USING WS-OPASSWORD AUTO SECURE.

       COPY OPT.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
	     PERFORM BA000.
007780	     PERFORM BI000.
	     CLOSE RECOVER
		   STKWRK.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       AA49.
	     EXIT PROGRAM.

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

       COPY LOCKED.RC2.

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
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³	       other keys are to be tested on exit)	       ³
      *    ³			     OR 			       ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)					       ³
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
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

       COPY STKTRF.RD.

       COPY TRFRIN.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

       COPY STKDEX.WR.

       COPY STKTRF.WR.

       COPY TRFRIN.WR.

       COPY STOCK.1ST.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011430 AY000           SECTION.
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
	   IF WS-ACTION = 9
	       MOVE 0		 TO REC-TYPE
	   ELSE
	       MOVE WS-ACTION	 TO REC-TYPE.
011520       MOVE STK-RECORD1    TO REC-STOCKF.
011530       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011690 AY46.
011080	     MOVE 46		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE STX-REC1	 TO REC-STKDEX.
011110       GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  End transaction in recovery log	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011120 AY49.
011700       MOVE 99             TO REC-FILE.
011710       MOVE ZERO           TO REC-KEY.
011720       MOVE SPACES         TO REC-DETAIL.
011730       PERFORM AY50 THRU AY59.
011740       ADD 1               TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011750	   IF WS-RECOVER > 95
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
	   IF NOT(PAR-PROG(WS-SUB) = SPACES OR
		  PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT(PAR-PROG(WS-SUB) = SPACES)
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
	   IF NOT(PAR-USR(WS-SUB) = LS-USER)
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
	   IF NOT(PAR-USR(WS-USUB) = LS-USER)
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
	   IF NOT(PAR-PROG(WS-SUB) = SPACES)
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
      *    ****   U S E R   M E S S A G E
      *
       BA000	       SECTION 50.
       BA00.
028050	     DISPLAY CLR-SCREEN.
	     DISPLAY "TRANSFER STOCK IN" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 6		 TO SHADE-COL.
	     MOVE 70		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0404
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "¿" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0504
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " As stock transfers in may be used for either good
      -		     "s received or goods "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0604
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " returned, the system will only update the physica
      -		     "l stock and will    "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0704
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " not process the transfers as invoices or credit n
      - 	     "otes. Totals are    "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0804
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " given at the end of the report and it is the resp
      - 	     "onsibility of the   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0904
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " USER to process the necessary transactions using
      - 	     "the report totals.  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1004
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " The USER may select to use accounts in the debtor
      -		     " or creditor ledger "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1104
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " or to use general ledger accounts to handle the i
      -		     "nter branch stock   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1204
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " transfers. See Options 2/2/6, 2/2/7, 2/4/1 & 2/4/
      -		     "2 for creditors and "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1304
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " Options 3/2/1, 3/2/2 and 5/6/C for debtors or pho
      -		     "ne the help line    "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 1404
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "Press ANY key to continue" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA05.
	     MOVE "N"		 TO WS-TOTAL.

       BA999.
	     EXIT.
      /
      *    ****  T R A N S F E R   S T O C K   I N
      *
028030 BI000	       SECTION 51.
       BI0.
	     MOVE WS-OPTION	 TO WS-TYPE.

028040 BI00.
028050       DISPLAY CLR-SCREEN.
	     DISPLAY "TRANSFER STOCK IN" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Key in the file location and name :"
                      AT 0412.

       BI05.
	     ACCEPT W02-FILE33 AT 0449
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BI999
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BI05
	       END-EVALUATE.
	   IF W02-FILE33 = SPACES
	       GO TO BI05.
	     PERFORM CHECK-CORRECT.
028650	   IF WS-OPTION = "N"
028660         GO TO BI05.
	     OPEN I-O STKTRF.
           IF WS-STATUS NOT = "00"
	       MOVE "Invalid drive Specification"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO BI05.
	     MOVE ZERO		 TO WS-ERROR.
	     PERFORM BI40 THRU BI60.
	   IF WS-ERROR = 9
	       CLOSE STKTRF
	       GO TO BI999.
	     DISPLAY CLEAR-L25.
035980       MOVE ZERO           TO W25-PCOST WS-PAGE   
				    W25-TSELL W25-TOTAL
				    W25-TH-FEE.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
      *
      *    ****   1 2	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT(WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
009860	       MOVE SPACES	 TO REP-DETAIL1.
	     DISPLAY "Transfer from:" AT 0612.
	     DISPLAY WS-BRANCH AT 0627
		    WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      *
      *    ****    M U S T   A	 H A N D L I N G   F E E   B E
      * 	   C H A R G E D
      *
	   IF WS-HND-FEE = "N"
	       GO TO BI09.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1029
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Handling fee         ³" AT 1129
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1229
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

       BI08.
	     ACCEPT S10.
	     PERFORM CHECK-CORRECT.
036560	   IF WS-OPTION = "N"
036570	       GO TO BI08.
	     PERFORM RESTORE-SCREEN-3.

       BI09.
	     DISPLAY "Update Selling prices Y/N" AT 0812.
	     MOVE SPACE 	 TO WS-SELL.

       BI09A.
	     PERFORM WITH TEST AFTER
		     UNTIL WS-SELL = "N" OR "Y"
	       ACCEPT WS-SELL AT 0838
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
			   UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-SELL
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	     DISPLAY "Stock Code:" AT 1012.

036040 BI10.
036050       MOVE SPACES         TO REP-DETAIL2.
036060       MOVE "PAGE:"        TO REP-PAGE.
036070       MOVE "DATE:"        TO REP-DATE.
036080       MOVE W95-COMP       TO REP-COMPANY.
036090       ADD 1               TO WS-PAGE.
036100       MOVE WS-PAGE        TO REP-P-NO.
036110       MOVE W12-TODAY      TO REP-DTE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
036130	     MOVE SPACES	 TO REP-DETAIL2.
036140       MOVE "TRANSFER IN"  TO REP-HEAD10.
	     PERFORM CALL-PRINTUTL.
036160	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "TRANSFER FROM:"
				 TO REP-TRF-HD.
	     MOVE WS-BRANCH	 TO REP-BRANCH.
	     PERFORM CALL-PRINTUTL.
036160	     MOVE SPACES	 TO REP-DETAIL2.
	   IF WS-TOTAL = "N"
036170	       MOVE "ITEM CODE"	 TO ORD-H1
036180	       MOVE "DESCRIPTION"
				 TO ORD-H2
036190	       MOVE "QUANTITY"	 TO ORD-H3
036200	       MOVE "UNIT COST"  TO ORD-H4
036210	       MOVE "UNIT SELL"  TO ORD-H5
036220	       MOVE "TOTAL COST" TO ORD-H6
	   ELSE
	       MOVE "SALES LEDGER"
				 TO TOT-H1
	       MOVE "DESCRIPTION"
				 TO TOT-H2
	       MOVE "STOCK G/L"  TO TOT-H3
	       MOVE "TRF VALUE"  TO TOT-H4.
	     PERFORM CALL-PRINTUTL.
036250	     MOVE SPACES	 TO REP-DETAIL2.

036260 BI15.
036270	     MOVE SPACES	 TO W10-EXT-ITEM.
036280       MOVE 1              TO W10-QUANT.
036280       MOVE ZERO           TO W25-SCOST W10-COST.
	     MOVE 2		 TO WS-TRFKEY.
	     INITIALIZE SIN-RECORD1.
	     MOVE WS-BRNCH	 TO SIN-BR-CODE.
	     MOVE WS-REF	 TO SIN-REF.
	     PERFORM READ-TRFRIN THRU READ-TRFRIN-EXIT.
	     MOVE SPACE 	 TO WS-OPTION.
	   IF WS-STATUS = "00"
	       MOVE "Y" 	 TO WS-OPTION
	       MOVE "Transfer has been processed - Abort Y/N  [ ]"
				 TO WS-ERR-MES
003260	       PERFORM OPT-MESSAGE TEST AFTER
		       UNTIL WS-OPTION = "Y" OR "N"
	       IF WS-OPTION = "Y"
		   GO TO BI35.
	   IF WS-OPTION = "N"
	       DELETE TRFRIN.
	     MOVE TODAY-DDMMYY	 TO SIN-DATE.
	     MOVE LS0-NO	 TO SIN-USER.
	     MOVE LS-USER	 TO SIN-WS.
	     MOVE LS0-NAME	 TO SIN-USER-NAME.
	     MOVE WS-BRANCH	 TO SIN-BR-FROM.
	     MOVE WS-BR-TO	 TO SIN-TO-CDE
	     MOVE WS-BR-NTO	 TO SIN-BR-TO.
	     MOVE ZERO		 TO SIN-LINES
				    SIN-VALUE.
	     PERFORM WRITE-TRFRIN THRU WRITE-TRFRIN-EXIT.

       BI18.
	     PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
	   IF WS-F-ERROR = 33
               GO TO BI65.
	     DISPLAY TRF-EXT-CODE AT 1024
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
037380	     MOVE TRF-EXT-CODE	 TO W10-EXT-ITEM.
	   IF WS-HND-FEE = "Y"
	       COMPUTE W10-ICOST ROUNDED = TRF-COST - ((TRF-COST /
					   (100.000 + WS-HFEE)) *
					    WS-HFEE)
	   ELSE
	       MOVE TRF-COST	 TO W10-ICOST.
	     MOVE W10-ICOST	 TO TRF-AVGE.
	   IF WS-HND-FEE = "Y"
	       COMPUTE W25-HND-FEE = (TRF-COST - W10-ICOST) * TRF-QUANT
	       ADD W25-HND-FEE	 TO W25-TH-FEE.
	     COMPUTE W10-COST ROUNDED = W10-ICOST * TRF-QUANT.
036350	     PERFORM CA000.
036370     IF WS-F-ERROR = 22
               GO TO BI20.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
031890       PERFORM AY05 THRU AY59.
	   IF TRF-QUANT = ZERO
	       MOVE W10-ICOST	 TO STK-COST
	       PERFORM BI21 THRU BI22
	       MOVE TRF-LEDG	 TO STK-LDG
               MOVE TRF-ACODE    TO STK-ACODE
               MOVE TRF-XREF     TO STK-XREF
               PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT
               GO TO BI28.
017590     IF STK-QUANT < 0.01
017600	       MOVE W10-ICOST	 TO STK-AVGE
017610     ELSE
017620         MULTIPLY STK-QUANT BY STK-AVGE GIVING W10-MCOST
017630         ADD W10-COST      TO W10-MCOST
017640         COMPUTE STK-AVGE ROUNDED = W10-MCOST /
                                          (STK-QUANT + TRF-QUANT).
             ADD TRF-QUANT       TO STK-QUANT.
	     ADD TRF-CASES	 TO STK-CASES.
	     MOVE W10-ICOST	 TO STK-COST.
	     PERFORM BI21 THRU BI22.
	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             GO TO BI25.

       BI20.
             PERFORM AY60 THRU AY999.
             INITIALIZE STK-RECORD1.
	     MOVE TRF-EXT-CODE	 TO STK-CODE.
             MOVE TRF-ACODE      TO STK-ACODE.
             MOVE TRF-BIN        TO STK-BIN.
	     MOVE TRF-LEDG	 TO STK-LDG.
             MOVE TRF-DESC       TO STK-DESC.
	     MOVE TRF-DESC2	 TO STK-DESC2.
             MOVE TRF-QUANT      TO STK-QUANT.
             MOVE TRF-LEVEL      TO STK-LEVEL.
	     MOVE W10-ICOST	 TO STK-COST.
	     MOVE W10-ICOST	 TO STK-AVGE.
             MOVE TRF-ORD        TO STK-ORD.
             MOVE TRF-LTIME      TO STK-LTIME.
	     MOVE TRF-SUPP	 TO STK-SUPP.
             MOVE TRF-GUAR       TO STK-GUAR.
             MOVE TRF-TAX        TO STK-TAX.
             MOVE TRF-XREF       TO STK-XREF.
	     MOVE TRF-CASES	 TO STK-CASES.
	     MOVE TRF-USE-CASES  TO STK-USE-CASES.
	     MOVE TRF-USE-PACKS  TO STK-USE-PACKS.
	     MOVE TRF-EXEMPT	 TO STK-EXEMPT.
	     MOVE TRF-FACT	 TO STK-FACT.
	     MOVE "Y"		 TO WS-NEW.

       BI21.
	     MOVE 1		 TO WS-S1.

       BI22.
	     MOVE TRF-UNT(WS-S1) TO STK-UNT(WS-S1).
	     MOVE TRF-PR(WS-S1)  TO STK-PR(WS-S1).
	   IF (WS-NEW = "Y" OR WS-SELL = "Y")
	       MOVE TRF-MKUP(WS-S1)
				 TO STK-MKUP(WS-S1)
	       MOVE TRF-CMKUP(WS-S1)
				 TO STK-CMKUP(WS-S1)
	       MOVE TRF-WMKUP(WS-S1)
				 TO STK-WMKUP(WS-S1)
	       MOVE TRF-SL(WS-S1)
				 TO STK-SL(WS-S1)
	       MOVE TRF-CSH(WS-S1)
				 TO STK-CSH(WS-S1)
	       MOVE TRF-WSL(WS-S1)
				 TO STK-WSL(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BI22.
	     MOVE "N"		 TO WS-NEW.
	   IF (WS-CARDEX = "Y") AND (TRF-QUANT > ZERO)
	       INITIALIZE STX-REC1
	       MOVE STK-CODE	 TO STX-EXT-CODE
	       MOVE W12-T-YMD	 TO STX-DTE
	       MOVE ZERO	 TO STX-SEQ
	       MOVE LS0-NO	 TO STX-USER
	       MOVE "STKTRF"	 TO STX-PROG
	       MOVE LS-USER	 TO STX-WS
	       MOVE "Stock transfers"
				 TO STX-REMARKS
	       MOVE TRF-QUANT	 TO STX-QNT
	       MOVE STK-COST	 TO STX-COST
	       MOVE STK-SELL	 TO STX-SELL
	       PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
	       MOVE 1		 TO WS-ACTION
	       PERFORM AY46 THRU AY59.

       BI23.
017190	     PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
	     MOVE STK-LDG	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = "7"
	       DISPLAY "Missing Department (Sales Ledger) "
			AT 1822 WITH FOREGROUND-COLOR 4 HIGHLIGHT
			STK-LDG WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       DISPLAY "Stock Code: " AT 2022
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			STK-CODE WITH FOREGROUND-COLOR 3 HIGHLIGHT
	       DISPLAY "Press ANY key to continue" AT 2222
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       MOVE 1801	 TO CPOS
	       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
	   IF TRF-QUANT = ZERO
               GO TO BI28.

       BI25.
      *
      *    ****    U P D A T E	 O N T O   W O R K   F I L E
      *
	     MOVE STK-LDG	 TO STW-DEPT.
	     READ STKWRK KEY IS STW-DEPT.
	   IF NOT(WS-STATUS = "00")
	       MOVE ZERO	 TO STW-SL-TOTAL
	       MOVE STK-LDG	 TO STW-DEPT
	       WRITE STW-RECORD
	       CLOSE STKWRK
	       OPEN I-O STKWRK.
	     ADD W10-COST	 TO STW-SL-TOTAL.
	     REWRITE STW-RECORD.
	     CLOSE STKWRK.
	     OPEN I-O STKWRK.
	     ADD TRF-QUANT	 TO W25-TSELL.
036920       MOVE SPACES         TO REP-LINE1.
037210       MOVE W10-COST       TO ORD-VALUE.
037280       MOVE TRF-QUANT      TO ORD-QUANT.
037370       MOVE TRF-DESC       TO ORD-DESC.
037380       MOVE TRF-CODE       TO ORD-ITEM.
	     MOVE TRF-SL(1)	 TO ORD-SELL.
	     MOVE W10-ICOST	 TO ORD-COST.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
037460	       PERFORM BI10.
037470       MOVE SPACES         TO REP-DETAIL2.
	     ADD W10-COST	 TO STK-TOTAL
				    W25-TOTAL.

       BI28.
037500       PERFORM AY70 THRU AY999.

037510 BI30.
             ADD 1               TO WS-TRFKEY.
037520	     GO TO BI18.

       BI35.
	     MOVE "TRANSFER HAS BEEN PROCESSED"
				 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
037580	     MOVE "***** END TRANSFER *****" TO REP-DETAIL2.
	     GO TO BI80.
      *
      *    G E T   T R A N S F E R   D E T A I L S
      *
       BI40.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE 1		 TO WS-TRFKEY.
	     PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
           IF WS-F-ERROR = 33
	       MOVE "No header details"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BI55.
      *
      *    C H E C K   B R A N C H - T O   C O D E
      *
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      *
      *      Temporary bypass - until all procedures have been completed
      *
      *	   IF SIN-TO-CDE = WS-BR-CODE
	       GO TO BI50.
      *      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
	     MOVE "N"		 TO WS-OPTION.

       BI45.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "Override Branch Code " DELIMITED SIZE
		     SIN-TO-CDE DELIMITED SIZE
		     " [N]" DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N".
	   IF WS-OPTION = "N"
	       GO TO BI55.
	     PERFORM SAVE-SCREEN-2.
018770	     DISPLAY S20.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 22		 TO SHADE-COL.
	     MOVE 39		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
018790	     ACCEPT S20A.
018800	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-OPASSWORD
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018860	     MOVE 1		 TO WS-LENGTH.
018870	     MOVE 5		 TO WS-PARKEY.
018880	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018890	     MOVE PAR-PW	 TO W85-PASS.
	     PERFORM RESTORE-SCREEN-2.
018900	   IF NOT(WS-OPASSWORD = W85-SUPER(1) OR
018940				 W85-SUPER(7) OR
018990				 "BRANCH")
048290	       MOVE "Invalid password"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019030	       GO TO BI55.

       BI50.
	     MOVE TRF-BR-CODE	 TO WS-BRNCH.
	     MOVE TRF-BR-FROM	 TO WS-BRANCH.
	     MOVE TRF-TO-CDE	 TO WS-BR-TO.
	     MOVE TRF-BR-TO	 TO WS-BR-NTO.
	     MOVE TRF-REF	 TO WS-REF.
	     MOVE TRF-DATE	 TO WS-DATE.
	     MOVE TRF-VALUE	 TO WS-VALUE.
	     GO TO BI60.

       BI55.
	     MOVE 9		 TO WS-ERROR.

       BI60.
	     EXIT.

037530 BI65.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
037550	     MOVE "TOTAL"	 TO ORD-DESC.
037560       MOVE W25-TSELL      TO ORD-QUANT.
037560       MOVE W25-TOTAL      TO ORD-VALUE.
	     PERFORM CALL-PRINTUTL.
037580	     MOVE "***** END TRANSFER *****" TO REP-DETAIL2.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Y"		 TO WS-TOTAL.
	     MOVE ZERO		 TO WS-PAGE.
037460	     PERFORM BI10.
	     MOVE "!"		 TO STW-DEPT.
	     START STKWRK KEY >= STW-DEPT.
	   IF NOT(WS-STATUS = "00")
	       GO TO BI75.

       BI70.
	     READ STKWRK NEXT.
	   IF NOT(WS-STATUS = "00")
	       CLOSE STKWRK
	       OPEN OUTPUT STKWRK
	       CLOSE STKWRK
	       OPEN I-O STKWRK
	       GO TO BI75.
	     MOVE STW-DEPT	 TO DPT-CODE
				    TOT-SL-NO.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF NOT(DPT-DESC = SPACES)
	       MOVE DPT-DESC	 TO TOT-SL-NAME
	   ELSE
	       MOVE "No Depart/Sales Ledger Name"
				 TO TOT-SL-NAME.
	     MOVE DPT-PUR-GL	 TO TOT-STK-GL.
	     MOVE STW-SL-TOTAL	 TO TOT-SL-VALUE.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
037460	       PERFORM BI10.
037470       MOVE SPACES         TO REP-DETAIL2.
	     DELETE STKWRK.
	     GO TO BI70.

       BI75.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF WS-HND-FEE = "Y"
	       MOVE "HANDLING FEE"
				 TO TOT-SL-NAME
	       MOVE W25-TH-FEE	 TO TOT-SL-VALUE
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
037580	     MOVE "***** END REPORT *****" TO REP-DETAIL2.

       BI80.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF NOT(WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
010010	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE STKTRF
		   TRFRIN.

       BI999.
             EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
044130 ZA000-INIT	       SECTION 90.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
	   IF WS-STAT1 = "9"
	       IF NET-ERROR OR COM-ERROR
		   PERFORM NETWORK-ERROR THRU LOCK-REC-EXIT
		   GO TO ZA00-READ-APACFIDS.
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
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TRFRIN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TRFRIN.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-HND-FEE = "Y"
	       MOVE PAR-HFEE	 TO WS-HFEE
	       MOVE PAR-HND-FEE	 TO WS-HND-FEE
	   ELSE
	       MOVE "N" 	 TO WS-HND-FEE
	       MOVE ZERO	 TO WS-HFEE.
	   IF NOT(PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO W10-STCK
	   ELSE
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
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
044540	     OPEN OUTPUT RECOVER.
044550       CLOSE RECOVER.
044560       OPEN I-O RECOVER.
	     OPEN I-O TRFRIN.
	   IF WS-STATUS = "05"
	       CLOSE TRFRIN
	       OPEN OUTPUT TRFRIN
	       CLOSE TRFRIN
	       OPEN I-O TRFRIN.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
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
	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE WS-MID-LNE2.
041240 ZA60.
041250	     MOVE WS-G1 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(18) WS-TCHR(49)
021940				    WS-TCHR(60) WS-TCHR(70).
021950	     MOVE WS-G3 	 TO WS-MCHR(18) WS-MCHR(49)
021960				    WS-MCHR(60) WS-MCHR(70)
				    WS-MCHR(1)	WS-MCHR(80)
				    WS-MCH(1)	WS-MCH(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(18) WS-BCHR(49)
021980				    WS-BCHR(60) WS-BCHR(70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
044790 ZA200.

       COPY LOCKED.PRO.

       ZA205.
	     EXIT PROGRAM.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 91.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800	   ELSE
041810     IF WS-F-ERROR = 7
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
041840	   ELSE
041850     IF WS-F-ERROR = 33
041860         MOVE W02-STKTRF   TO WS-FILE
041870	       MOVE WS-TRFKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 62
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
