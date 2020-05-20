      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	       **     **    **	**    **	**	 **    *
      *    ******      **     *******	**    **      ***      **      *
      * 	**     **     **	**    **	**   **        *
      *   **	**     **     **	 **  **   **	**  **	       *
      *    ******      **     **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	-   T R A N S F E R S	O U T	P R O G .      *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP032.
000030 AUTHOR.	       J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2011
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

       COPY STKDEX.SL.

       COPY STKTRF.SL.

       COPY STKWRK.SL.

000140 COPY STOCK.SL.

       COPY TRFOUT.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000380 COPY RECOVER.GFD.

000280 COPY PARAM.FDE.

       COPY SHARED.FDE.

       COPY STKDEX.FDE.

       COPY STKTRF.FD.

       COPY STKWRK.FD.

000300 COPY STOCK.FDE.

       COPY TRFOUT.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	 COMP-3.
001710 77  WS-SUB	   PIC	9(04)	 COMP-5.
001710 77  WS-S1	   PIC	9(04)	 COMP-5.
001720 77  WS-S2	   PIC	9(04)	 COMP-5.
001730 77  WS-S3	   PIC	9(04)	 COMP-5.
001740 77  WS-S4	   PIC	9(04)	 COMP-5.
001750 77  WS-S5	   PIC	9(04)	 COMP-5.
001760 77  WS-S6	   PIC	9(04)	 COMP-5.
001770 77  WS-S7	   PIC	9(04)	 COMP-5.
001780 77  WS-S8	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001840 77  WS-TRFKEY	   PIC	9(06)	 COMP-5.
001840 77  WS-PRCKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001900 77  WS-RECORDS	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-BRANCH	   PIC	X(40).
       77  WS-BR-CODE	   PIC	X(03).
       77  WS-BR-NAME	   PIC	X(38).
       77  WS-TRF-REF	   PIC	9(07)	 COMP-3.
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-HND-FEE	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-TOTAL	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002080 77  WS-IND	   PIC	9(01)	 VALUE 0.
002090 77  WS-INDB	   PIC	9(01)	 VALUE 0.
002100 77  WS-INDP	   PIC	9(01)	 VALUE 0.
002110 77  WS-INDS	   PIC	9(01)	 VALUE 0.
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-LABCDE	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  WS-NODEP	   PIC	X(30) VALUE
			   "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  WS-HFEE	   PIC	9(03)V999 COMP-3.
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

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "032".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY STKDEX.ID.

       COPY STKTRF.ID.

       COPY STKWRK.ID.

000300 COPY STOCK.ID.

       COPY TRFOUT.ID.

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
000430	       05  REP-DETAIL1	   PIC	X(96).
000440	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000450	       05  REP-DETAIL2	   PIC	X(96).
000460	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000520	       05  REP-DATE	   PIC	X(06).
000530	       05  REP-DTE	   PIC	Z9/99/9999.
000490	       05  FILLER	   PIC	X(14).
000500	       05  REP-COMPANY	   PIC	X(40).
000510	       05  FILLER	   PIC	X(18).
000470	       05  REP-PAGE	   PIC	X(06).
000480	       05  REP-P-NO	   PIC	Z(03)9.
000810	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
000820	       05  FILLER	   PIC	X(40).
000860	       05  REP-HEAD10	   PIC	X(16).
000870	       05  FILLER	   PIC	X(40).
	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(14).
	       05  REP-TRF-HD	   PIC	X(13).
	       05  REP-BRANCH	   PIC	X(40).
	       05  FILLER	   PIC	X(29).
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
	       05  TOT-SL-NO	   PIC	X(04).
	       05  FILLER	   PIC	X(06).
	       05  TOT-SL-NAME	   PIC	X(30).
	       05  FILLER	   PIC	X(03).
	       05  TOT-STK-GL	   PIC	9(04)/99.
	       05  FILLER	   PIC	X(03).
	       05  TOT-SL-VALUE    PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(28).

       COPY W05.VAT.

       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

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

       COPY W75.STK.

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

       01  S07.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S07-ITM.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC  X(14)/X(03)
				  USING W10-EXT-ITEM AUTO.

       01  S07-EXT.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC  X(18) USING W10-EXT-ITEM AUTO.

005530 01  S07-3.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       01  S10.
	   03  LINE 11 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9.999 USING WS-HFEE AUTO.

       01  S35.
	   03  FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC Z(05)9.9999 USING W10-QUANT AUTO.

       01  S36.
	   03  FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC Z(06)9.99 USING W10-SELL AUTO.

       01  S37.
	   03  FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC Z(05)9.999 USING W10-COST AUTO.

009930 01  S38.
009940	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 0
				  PIC ZZZZ9 USING W10-CASES AUTO.

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
007780	     PERFORM BH000.
	     CLOSE RECOVER
		   STKWRK.

       AA49.
	     EXIT PROGRAM.

       COPY AA50.LUP.

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

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

       COPY AA300.STK.

      /    *************************************************************
      *    ****    READ FILES ROUTINES
      *    *************************************************************
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

       COPY STKTRF.RD.

      /    *************************************************************
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *    *************************************************************
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STKDEX.WR.

       COPY STOCK.WR.

       COPY STKTRF.WR.

       COPY TRFOUT.WR.

       COPY STOCK.1ST.

       COPY APAC.HLP.

      /    *************************************************************
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
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
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011900         STOP RUN.
011930 AY59.
011940       EXIT.
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *    *************************************************************
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 4 HIGHLIGHT.
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
012640	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
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
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *    *************************************************************
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *    *************************************************************
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *    *************************************************************
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *    *************************************************************
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *    *************************************************************
	       IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *    *************************************************************
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
      *    *************************************************************
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
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
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *    *************************************************************
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
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

      /    *************************************************************
      *    ****   U S E R   M E S S A G E
      *    *************************************************************
       BA000	       SECTION 50.
       BA00.
028050	     DISPLAY CLR-SCREEN.
	     DISPLAY "TRANSFER STOCK OUT" AT 0232
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
		     " As stock transfers out may be used for either sup
      -		     "plying or returning "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0604
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " goods, the system will only update the physical s
      -		     "tock and will NOT   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0704
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " generate invoices or debit notes. The totals are
      - 	     "given at the end of "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 0804
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " the report that is generated and it is the respon
      - 	     "sibility of the     "
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
      /    *************************************************************
      *    ****   T R A N S F E R   S T O C K	O U T
      *    *************************************************************
028030 BH000	       SECTION 51.
       BH0.
	     MOVE WS-OPTION	 TO WS-TYPE.

028040 BH00.
028050	     DISPLAY CLR-SCREEN.
	     DISPLAY "TRANSFER STOCK OUT" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Key in the file location and name :"
                      AT 0412.

       BH05.
	     ACCEPT W02-FILE33 AT 0449
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BH999
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BH05
	       END-EVALUATE.
	   IF W02-FILE33 = SPACES
	       GO TO BH05.
	     PERFORM CHECK-CORRECT.
028650	   IF WS-OPTION = "N"
028660         GO TO BH05.
	     DISPLAY "A" AT 0612 WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "dd details to existing or " 
                      WITH FOREGROUND-COLOR 7
		     "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT
                     "reate new TRANSFER file" WITH FOREGROUND-COLOR 7.
             MOVE SPACE          TO WS-OPTION.

       BH10.
	     PERFORM UNTIL WS-OPTION = "A" OR "C" OR "P"
	       ACCEPT WS-OPTION AT 0664
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF WS-OPTION = "C"
               GO TO BH20.

       BH15.
             OPEN I-O STKTRF.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-TRFKEY
	       GO TO BH25.
	   IF WS-STATUS = "05"
	       CLOSE STKTRF.

       BH20.
             OPEN OUTPUT STKTRF.
	   IF WS-STATUS NOT = "00"
	       MOVE "Invalid drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BH00.
	     MOVE 1		 TO WS-TRFKEY.
	     INITIALIZE TRF-RECORD1.
	     MOVE WS-BR-CODE	 TO TRF-BR-CODE
				    SOT-BR-CODE.
	     MOVE WS-TRF-REF	 TO TRF-REF
				    SOT-REF.
	     MOVE W12-T-YMD	 TO TRF-DATE
				    SOT-DATE.
	     MOVE WS-BR-NAME	 TO TRF-BR-FROM
				    SOT-BR-FROM.
	     MOVE SPACES	 TO TRF-TO-CDE
				    TRF-BR-TO
				    SOT-BR-TO
				    SOT-TO-CDE.
	     MOVE ZERO		 TO TRF-VALUE
				    SOT-VALUE
				    SOT-LINES.
             PERFORM WRITE-STKTRF THRU WRITE-STKTRF-EXIT.
	     CLOSE STKTRF.
	     MOVE LS0-NO	 TO SOT-USER.
	     MOVE LS-USER	 TO SOT-WS.
	     MOVE LS0-NAME	 TO SOT-USER-NAME.
	     PERFORM WRITE-TRFOUT THRU WRITE-TRFOUT-EXIT.
	     MOVE 6		 TO WS-PARKEY.
	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	   IF WS-TRF-REF < 9999999
	       ADD 1		 TO WS-TRF-REF
	   ELSE
	       MOVE 1		 TO WS-TRF-REF.
	     MOVE WS-TRF-REF	 TO PAR-TRF-REF.
	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             GO TO BH15.

       BH25.
             ADD 1               TO WS-TRFKEY.
             PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
           IF WS-F-ERROR = ZERO
               GO TO BH25.
035980       MOVE ZERO           TO W25-PCOST WS-PAGE   
				    W25-TSELL W25-TOTAL
				    W25-TH-FEE.
	     MOVE SPACES	 TO WS-BRANCH.
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
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
009860	       MOVE SPACES	 TO REP-DETAIL1.
036020	     DISPLAY CLR-SCREEN.
             DISPLAY "TRANSFER STOCK OUT" AT 0232 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Transfer to:" AT 0412.

       BH26.
	     ACCEPT WS-BRANCH AT 0425
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
028650	   IF WS-OPTION = "N"
028660	       GO TO BH26.
      *    *************************************************************
      *    ****    M U S T   A	 H A N D L I N G   F E E   B E
      * 	   C H A R G E D
      *    *************************************************************
	   IF WS-HND-FEE = "N"
	       GO TO BH30.
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

       BH28.
	     ACCEPT S10.
	     PERFORM CHECK-CORRECT.
036560	   IF WS-OPTION = "N"
036570	       GO TO BH28.
	     PERFORM RESTORE-SCREEN-3.

036010 BH30.
006460	     DISPLAY "Item Code/Description" AT 0602.
006480	     DISPLAY "Quantity" AT 0636.
006490	     DISPLAY "Unit cost" AT 0647.
006500	     DISPLAY "Unit sell" AT 0659.
006510	     DISPLAY "Total cost" AT 0670.

036040 BH35.
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
036140       MOVE "TRANSFER OUT" TO REP-HEAD10.
	     PERFORM CALL-PRINTUTL.
036160	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "TRANSFER TO:" TO REP-TRF-HD.
	     MOVE WS-BRANCH	 TO REP-BRANCH.
	     PERFORM CALL-PRINTUTL.
036160	     MOVE SPACES	 TO REP-DETAIL2.
	   IF WS-TOTAL = "N"
036170	       MOVE "ITEM CODE"	 TO ORD-H1
036180	       MOVE "DESCRIPTION"
				 TO ORD-H2
036190	       MOVE "QUANTITY"	 TO ORD-H3
036200	       MOVE "UNIT COST"	 TO ORD-H4
036210	       MOVE "UNIT SELL"	 TO ORD-H5
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

036260 BH40.
036270	     MOVE SPACES	 TO W10-EXT-ITEM.
036280	     MOVE 1		 TO W10-QUANT.
036280	     MOVE ZERO		 TO W25-SCOST W10-COST W10-CASES.
	     MOVE 0801		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
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

036310 BH45.
             PERFORM AA300.
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S07-EXT AT 0802
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT 0802
	       ELSE
021070		   ACCEPT S07 AT 0802
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT 0802
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT 0802
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT 0802
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT 0802
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT 0802
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT 0802
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT 0802
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT 0802
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT 0802
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT 0802
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT 0802
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT 0802.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BH65
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BH45
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
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BH45
               END-EVALUATE
	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S07-EXT AT 0802
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT 0802
		   ELSE
		       DISPLAY S07 AT 0802
		   END-IF
	       ELSE
		   DISPLAY S07 AT 0802
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO BH45.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
036330	   IF W10-EXT-ITEM = SPACES
036340         GO TO BH65.

       BH50.
036350       PERFORM CA000.
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT 0802
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT 0802
	       ELSE
		   DISPLAY S07 AT 0802
	       END-IF
	   ELSE
	       DISPLAY S07 AT 0802.
020050	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
036370     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S07-EXT AT 0802
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT 0802
		   ELSE
		       DISPLAY S07 AT 0802
		   END-IF
	       ELSE
		   DISPLAY S07 AT 0802
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BH45
               ELSE
                   GO TO BH50.
	     MOVE STK-DESC	 TO W10-DESC.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BH45.
036470	     DISPLAY W10-DESC AT 0902
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
036560	   IF WS-OPTION = "N"
036570         GO TO BH45.

       BH52.
           IF STK-IND = 3
	       ACCEPT W10-DESC AT 0902
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
	       PERFORM CHECK-CORRECT
036560	       IF WS-OPTION = "N"
036570             GO TO BH52.

036580 BH55.
036590	     ACCEPT S35 AT 0933.
036600     IF W10-QUANT = ZERO
036610         GO TO BH45.
	   IF WS-USE-CASES = "Y" AND STK-USE-CASES = "Y"
	       PERFORM JA000
	       IF W10-CASES = ZERO
		   GO TO BH55.
             MOVE STK-COST       TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
           IF STK-IND = 3
	       ACCEPT S37 AT 0946
	       ACCEPT S36 AT 0958
           ELSE
	       DISPLAY S37 AT 0946
	       DISPLAY S36 AT 0958.
             MOVE W10-COST       TO W10-ICOST.
             COMPUTE W10-COST ROUNDED = W10-ICOST * W10-QUANT.
037140	     DISPLAY S37 AT 0970.
	     PERFORM CHECK-CORRECT.
036690	   IF WS-OPTION = "N"
006700         GO TO BH55.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
031890       PERFORM AY05 THRU AY59.
036880       SUBTRACT W10-QUANT  FROM STK-QUANT.
	     SUBTRACT W10-CASES  FROM STK-CASES.
036890       ADD W10-QUANT       TO STK-MTD STK-YTD.
036920       MOVE SPACES         TO REP-LINE1.
	     MOVE STK-CODE	 TO TRF-EXT-CODE.
             MOVE STK-ACODE      TO TRF-ACODE.
             MOVE STK-BIN        TO TRF-BIN.
	     MOVE STK-LDG	 TO TRF-LEDG.
             MOVE W10-DESC       TO TRF-DESC.
	     MOVE W10-QUANT	 TO TRF-QUANT.
	     MOVE W10-CASES	 TO TRF-CASES.
	     MOVE STK-USE-CASES  TO TRF-USE-CASES.
	     MOVE STK-USE-PACKS  TO TRF-USE-PACKS.
             MOVE STK-LEVEL      TO TRF-LEVEL.
             MOVE W10-ICOST      TO TRF-COST.
	     MOVE STK-ORD	 TO TRF-ORD.
             MOVE STK-LTIME      TO TRF-LTIME.
	     MOVE STK-SUPP	 TO TRF-SUPP.
             MOVE STK-GUAR       TO TRF-GUAR.
             MOVE STK-TAX        TO TRF-TAX.
             MOVE STK-XREF       TO TRF-XREF.
	     MOVE STK-EXEMPT	 TO TRF-EXEMPT.
	     MOVE STK-FACT	 TO TRF-FACT.
	     MOVE 1		 TO WS-S1.

       BH58.
	     MOVE STK-UNT(WS-S1) TO TRF-UNT(WS-S1).
	     MOVE STK-PR(WS-S1)  TO TRF-PR(WS-S1).
	     MOVE STK-MKUP(WS-S1)
				 TO TRF-MKUP(WS-S1).
	     MOVE STK-CMKUP(WS-S1)
				 TO TRF-CMKUP(WS-S1).
	     MOVE STK-WMKUP(WS-S1)
				 TO TRF-WMKUP(WS-S1).
	   IF WS-S1 = 1
	       MOVE W10-SELL	 TO TRF-SL(WS-S1)
	   ELSE
	       MOVE STK-SL(WS-S1)
				 TO TRF-SL(WS-S1).
	     MOVE STK-CSH(WS-S1) TO TRF-CSH(WS-S1).
	     MOVE STK-WSL(WS-S1) TO TRF-WSL(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BH58.
      *    *************************************************************
      *    ****    U P D A T E	 O N T O   W O R K   F I L E
      *    *************************************************************
	     MOVE STK-LDG	 TO STW-DEPT.
	     READ STKWRK KEY IS STW-DEPT.
	   IF NOT (WS-STATUS = "00")
	       MOVE ZERO	 TO STW-SL-TOTAL
	       MOVE STK-LDG	 TO STW-DEPT
	       WRITE STW-RECORD
	       CLOSE STKWRK
	       OPEN I-O STKWRK.
	     ADD W10-COST	 TO STW-SL-TOTAL.
	     REWRITE STW-RECORD.
	     CLOSE STKWRK.
	     OPEN I-O STKWRK.
	   IF WS-HND-FEE = "Y"
	       COMPUTE TRF-COST ROUNDED = TRF-COST + (TRF-COST *
					  WS-HFEE / 100.000).
	     MOVE TRF-COST	 TO TRF-AVGE.
	   IF WS-HND-FEE = "Y"
	       COMPUTE W25-HND-FEE = (TRF-COST - W10-ICOST) * W10-QUANT
	       ADD W25-HND-FEE	 TO W25-TH-FEE
				    W25-TOTAL.
037280	     MOVE W10-QUANT	 TO ORD-QUANT.
037370       MOVE W10-DESC       TO ORD-DESC.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-CODE	 TO ORD-ITM
	   ELSE
037380	       MOVE STK-CODE	 TO ORD-ITEM.
	     MOVE W10-SELL	 TO ORD-SELL.
	     MOVE TRF-COST	 TO ORD-COST.
	     COMPUTE ORD-VALUE ROUNDED = TRF-COST * W10-QUANT.
	     ADD W10-QUANT	 TO W25-TSELL.
             ADD W10-COST        TO W25-TOTAL.
036910	     ADD W10-COST	 TO STK-MTDC STK-MTDV STK-YTDC STK-YTDV.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
037460	       PERFORM BH35.
037470       MOVE SPACES         TO REP-DETAIL2.
037480       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
037630       SUBTRACT W10-COST   FROM STK-TOTAL.
             PERFORM WRITE-STKTRF THRU WRITE-STKTRF-EXIT.
             ADD 1               TO WS-TRFKEY.
	   IF WS-CARDEX = "Y"
	       INITIALIZE STX-REC1
	       MOVE STK-CODE	 TO STX-EXT-CODE
	       MOVE W12-T-YMD	 TO STX-DTE
	       MOVE ZERO	 TO STX-SEQ
	       MOVE LS0-NO	 TO STX-USER
	       MOVE "STKTRF"	 TO STX-PROG
	       MOVE LS-USER	 TO STX-WS
	       MOVE "Stock transfers"
				 TO STX-REMARKS
	       MULTIPLY -1 BY W10-QUANT
				 GIVING STX-QNT
	       MULTIPLY -1 BY W10-COST
				 GIVING STX-COST
	       COMPUTE STX-SELL ROUNDED = (W10-COST +
		       ((W10-COST * STK-MARKUP) / 100)) * -1
	       PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
	       MOVE 1		 TO WS-ACTION
	       PERFORM AY46 THRU AY59.
037500       PERFORM AY70 THRU AY999.

037510 BH60.
037520	     GO TO BH40.

037530 BH65.
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
037460	     PERFORM BH35.
	     MOVE "!"		 TO STW-DEPT.
	     START STKWRK KEY >= STW-DEPT.
	   IF NOT (WS-STATUS = "00")
	       GO TO BH75.

       BH70.
	     READ STKWRK NEXT.
	   IF NOT (WS-STATUS = "00")
	       CLOSE STKWRK
	       OPEN OUTPUT STKWRK
	       CLOSE STKWRK
	       OPEN I-O STKWRK
	       GO TO BH75.
	     MOVE STW-DEPT	 TO DPT-CODE
				    TOT-SL-NO.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF NOT (DPT-DESC = SPACES)
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
037460	       PERFORM BH35.
037470       MOVE SPACES         TO REP-DETAIL2.
	     DELETE STKWRK.
	     GO TO BH70.

       BH75.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF WS-HND-FEE = "Y"
	       MOVE "HANDLING FEE"
				 TO TOT-SL-NAME
	       MOVE W25-TH-FEE	 TO TOT-SL-VALUE
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
037580	     MOVE "***** END REPORT *****" TO REP-DETAIL2.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
010010	       MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE STKTRF.

       BH999.
             EXIT.
      /    *************************************************************
      *    ****    C A S E S
      *    *************************************************************
       JA000	       SECTION 66.
       JA00.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0833
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³ NO OF CASES ³" AT 0933
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 1033
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³             ³" AT 1133
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1233
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 35		 TO SHADE-COL.
	     MOVE 13		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE 1		 TO W10-CASES.

       JA05.
	     ACCEPT S38 AT 1138.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-CASES
		     GO TO JA10
	       END-EVALUATE.

       JA10.
	     PERFORM RESTORE-SCREEN.

       JA999.
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
044150	     DISPLAY CLR-SCREEN.
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
	     MOVE "STKWRK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-STKWRK.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TRFOUT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TRFOUT.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-HND-FEE = "Y"
	       MOVE PAR-HFEE	 TO WS-HFEE
	       MOVE PAR-HND-FEE	 TO WS-HND-FEE
	   ELSE
	       MOVE "N" 	 TO WS-HND-FEE
	       MOVE ZERO	 TO WS-HFEE.
	   IF NOT (PAR-STOCK = "A" OR "N")
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
	     MOVE 275		 TO WS-PARKEY.
	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-BR-CDE	 TO WS-BR-CODE.
	     MOVE PAR-BR-NME	 TO WS-BR-NAME.
	     MOVE 6		 TO WS-PARKEY.
	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-TRF-REF	 TO WS-TRF-REF.

       ZA01B.
044540	     OPEN OUTPUT RECOVER.
044550       CLOSE RECOVER.
044560	     OPEN I-O RECOVER.
	     OPEN I-O STKWRK.
	   IF WS-STATUS = "05"
	       CLOSE STKWRK
	       OPEN OUTPUT STKWRK
	       CLOSE STKWRK
	       OPEN I-O STKWRK.
	     OPEN I-O TRFOUT.
	   IF WS-STATUS = "05"
	       CLOSE TRFOUT
	       OPEN OUTPUT TRFOUT
	       CLOSE TRFOUT
	       OPEN I-O TRFOUT.
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
	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(18) WS-TCHR(49)
021940				    WS-TCHR(60) WS-TCHR(70).
021950	     MOVE WS-G3 	 TO WS-MCHR(18) WS-MCHR(49)
021960				    WS-MCHR(60) WS-MCHR(70)
				    WS-MCHR(1)	WS-MCHR(80).
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
041820	       MOVE W02-DEPART	 TO WS-FILE
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
041850	   IF WS-F-ERROR = 63
041860	       MOVE W02-TRFOUT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SOT-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
