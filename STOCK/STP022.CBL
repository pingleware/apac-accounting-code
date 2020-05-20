      $set LINKCOUNT"578"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	       **     **    **	**    **       **	 **    *
      *    ******      **     *******	**    **     **        **      *
      * 	**     **     **	**    **   **	     **        *
      *   **	**     **     **	 **  **   **	    **	       *
      *    ******      **     **	  ****	  ********  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	P R O D U C T I O N   -   R E C O R D E D      *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP022.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
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

000220 COPY AUDIT.SL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000200 COPY SPARTS.SL.

       COPY STKDEX.SL.

000140 COPY STKMEM.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000450 COPY AUDIT.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000380 COPY RECOVER.GFD.

000280 COPY PARAM.FDE.

       COPY SHARED.FDE.

000360 COPY SPARTS.FDE.

       COPY STKDEX.FDE.

000140 COPY STKMEM.FDE.

000300 COPY STOCK.FDE.

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
000730 77  WS-AUDKEY	   PIC	9(04)	 COMP-5.
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
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001980 77  WS-ITM	   PIC	X(18).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002020 77  WS-OPTION	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002120 77  WS-AMEND	   PIC	9(01)	 VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-LABCDE	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08).
001330 77  WS-DESC-H1	   PIC	X(14) VALUE "Description".
       77  WS-DESC-H2	   PIC	X(14) VALUE "Description 2".
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
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
001390	       05  WS-TCH   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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
	   03  WS-PROG	       PIC  X(03) VALUE "022".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

000450 COPY AUDIT.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000360 COPY SPARTS.ID.

       COPY STKDEX.ID.

000140 COPY STKMEM.ID.

000300 COPY STOCK.ID.

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
000430	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000490	       05  REP-DATE    PIC  X(06).
000500	       05  REP-DTE     PIC  Z9/99/9999.
000460	       05  FILLER      PIC  X(03).
	       05  REP-PRC-HD.
		   07  FILLER  PIC  X(01).
000470		   07  REP-COMPANY  PIC	X(40).
		   07  FILLER  PIC  X(02).
000480	       05  FILLER      PIC  X(08).
000440	       05  REP-PAGE    PIC  X(06).
000450	       05  REP-P-NO    PIC  Z(03)9.
000510	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000520	       05  FILLER      PIC  X(30).
000530	       05  REP-HEAD1   PIC  X(20).
000540	       05  FILLER      PIC  X(30).
000550	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
000560	       05  FILLER      PIC  X(09).
000570	       05  REP-DSC     PIC  X(30).
000580	       05  FILLER      PIC  X(93).
000590	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000600	       05  REP-HEAD2   PIC  X(10).
000610	       05  REP-CNO     PIC  X(04).
000620	       05  REP-CATNO REDEFINES REP-CNO PIC X(04).
000630	       05  FILLER      PIC  X(01).
000640	       05  REP-CAT     PIC  X(30).
000650	       05  FILLER      PIC  X(35).
000510	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000520	       05  FILLER      PIC  X(32).
000530	       05  REP-HEAD10  PIC  X(16).
000540	       05  FILLER      PIC  X(32).
000850	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000860	       05  REP-PG      PIC  X(06).
000870	       05  REP-P-N     PIC  Z(03)9.
000880	       05  FILLER      PIC  X(21).
000890	       05  REP-COMP    PIC  X(40).
000900	       05  REP-REPORT  PIC  X(34).
000910	       05  FILLER      PIC  X(11).
000920	       05  REP-DAT     PIC  X(06).
000930	       05  REP-DT      PIC  Z9/99/9999.
001490	   03  ORD-LINE1 REDEFINES W02-PRN-LINE.
001500	       05  ORD-H1	   PIC X(19).
001510	       05  ORD-H2	   PIC X(35).
001520	       05  ORD-H3	   PIC X(11).
001530	       05  ORD-H4	   PIC X(11).
001540	       05  ORD-H5	   PIC X(10).
001550	       05  ORD-H6	   PIC X(10).
001560	   03  ORD-LINE2 REDEFINES W02-PRN-LINE.
001570	       05  ORD-EXT-ITEM.
		   07  ORD-ITEM    PIC	X(14).
		   07  ORD-SL	   PIC	X(01).
		   07  ORD-ITM	   PIC	X(03).
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

       COPY W05.VAT.

       COPY W05.RND.

       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W20.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750	   03  W25-SCOST      PIC S9(08)V999 COMP-3.
003760	   03  W25-PCOST      PIC S9(08)V999 COMP-3.
003760	   03  W25-PCST       PIC S9(08)V999 COMP-3.
003760	   03  W25-PTOT       PIC S9(08)V999 COMP-3.
003770	   03  W25-TSELL      PIC S9(08)V999 COMP-3.
003770	   03  W25-TOTAL      PIC S9(08)V999 COMP-3.
003780	   03  W25-VALUE      PIC S9(08)V999 COMP-3.
003780	   03  W25-ADJUST     PIC S9(08)V999 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

005810 COPY W70DEBT.IWS.

       COPY W75.STK.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000         05  W80-DATE   PIC Z9/99/99.
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

       01  S05.
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S05-ITM.
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(14)/X(03)
				 USING W10-EXT-ITEM AUTO.

       01  S05-EXT.
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(18) USING W10-EXT-ITEM AUTO.

006430 01  S15.
006440     03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
006450                           VALUE "STOCK PRODUCTION".
	   03  LINE  3 COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Use recorded details".
006460     03  LINE  4 COLUMN  2 VALUE "Item Code/Description".
006480     03          COLUMN 36 VALUE "Quantity".
006490     03          COLUMN 47 VALUE "Unit cost".
006500     03          COLUMN 59 VALUE "Unit sell".
006510     03          COLUMN 70 VALUE "Total cost".

       01  S35.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(05)9.9999 USING W10-QUANT AUTO.

       01  S36.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(06)9.999 USING W10-SELL AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
	     MOVE ZERO		 TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
007780	     PERFORM BQ000.
	     PERFORM AB35.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE AUDIT
		   RECOVER.

       AA49.
	     EXIT PROGRAM.

       AB000	       SECTION.
      *
      *    ****   1 2	C P I	( E L I T E   P R I N T )
      *
       AB25.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
009860	     MOVE SPACES	 TO REP-DETAIL1.

       AB30.
             EXIT.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
       AB35.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
010010	     MOVE SPACES	 TO REP-DETAIL1.

       AB40.
             EXIT.

       COPY AA50.LUP.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AS3.

       COPY AA300.STK.

       COPY DATE.CHK.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

011150 COPY AUDIT.RD.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

000140 COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
009850 AD000-WRITE	       SECTION.

011150 COPY AUDIT.WR.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY STKDEX.WR.

000140 COPY STKMEM.WR.

       COPY STOCK.WR.

       COPY SPARTS.WR.

       COPY STOCK.1ST.

       COPY APAC.HLP.

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

011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

       AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     MOVE DPT-RECORD	 TO REC-DEPART.
             GO TO AY50.

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

011120 AY49.
011700       MOVE 99             TO REC-FILE.
011710       MOVE ZERO           TO REC-KEY.
011720       MOVE SPACES         TO REC-DETAIL.
011730       PERFORM AY50 THRU AY59.
011740       ADD 1               TO WS-TRANS.
011750     IF WS-RECOVER > 95
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
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
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
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
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
      *            NETWORK record 3.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990	     MOVE 1		 TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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

      /
007970 BG00-TRAN               SECTION.
008600 BG80.
008610       MOVE W70-AUDIT      TO WS-AUDKEY.
008620       MOVE LOW-VALUES     TO AUD-REC1.
008630       PERFORM AY16 THRU AY59.
008640	     MOVE STK-CODE	 TO AUD-EXT-CODE.
008650       MOVE 2              TO AUD-TYPE.
008660       MOVE WS-DRAW        TO AUD-SUB.
008680       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
008690       ADD 1               TO W70-AUDIT.
008700       MOVE 1              TO WS-AUDKEY.
008710       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
008720       PERFORM AY16 THRU AY59.
008730       MOVE 1              TO AUD-REP2.
008740       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
008750 BG85.
008760       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY ROUND.PRC.

       CB300	     SECTION.
       CB301.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-SL(WS-S1) ROUNDED = (W10-VSL(WS-S1) /
				(100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-WSL(WS-S1) ROUNDED = (W10-VWSL(WS-S1) /
				 (100.00000 + W05-RATE)) * 100.00
	       COMPUTE W10-CSH(WS-S1) ROUNDED = (W10-VCSH(WS-S1) /
				(100.00000 + W05-RATE)) * 100.00
           ELSE
	       MOVE W10-VWSL(WS-S1)
				 TO W10-WSALE
	       MOVE W10-VCSH(WS-S1)
				 TO W10-CASH
	       MOVE W10-VSL(WS-S1)
				 TO W10-SELL.
	   IF W10-TAX NOT = ZERO
	       COMPUTE W10-CHECK ROUNDED
		   = W10-SL(WS-S1) + (W10-SL(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VSL(WS-S1)
		   SUBTRACT 0.01 FROM W10-SL(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VSL(WS-S1)
		   ADD 0.01	 TO W10-SL(WS-S1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-CSH(WS-S1) + (W10-CSH(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VCSH(WS-S1)
		   SUBTRACT 0.01 FROM W10-CSH(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VCSH(WS-S1)
		   ADD 0.01	 TO W10-CSH(WS-S1)
	       END-IF
	       COMPUTE W10-CHECK ROUNDED
		   = W10-WSL(WS-S1) + (W10-WSL(WS-S1) * W05-RTE)
	       IF W10-CHECK > W10-VWSL(WS-S1)
		   SUBTRACT 0.01 FROM W10-WSL(WS-S1)
	       ELSE
	       IF W10-CHECK < W10-VWSL(WS-S1)
		   ADD 0.01	 TO W10-WSL(WS-S1).

       CB399.
             EXIT.

      /
      *    ****    PRODUCTION PROCESSING
      *
035750 BQ000		       SECTION 6.
035760 BQ0.
035770       DISPLAY CLR-SCREEN.
	     DISPLAY "PRODUCTION PROCESSING" AT 0230
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE "S"		 TO WS-TYPE.
	   IF LS0-STLEV < 3
	       GO TO BQ0D.
	     DISPLAY "Print the " AT 0412 WITH FOREGROUND-COLOR 7
                     "C" WITH FOREGROUND-COLOR 14 
                     "ost, " WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14 
                     "elling or " WITH FOREGROUND-COLOR 7
                     "B" WITH FOREGROUND-COLOR 14 
		     "oth prices" WITH FOREGROUND-COLOR 7.

       BQ0C.
	     ACCEPT WS-TYPE AT 0452
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-TYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
035320	   IF NOT (WS-TYPE = "B" OR "C" OR "S")
035330         GO TO BQ0C.

       BQ0D.
035980       MOVE ZERO           TO W25-PCOST WS-PAGE   
                                    W25-TSELL W25-TOTAL.
	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.

036010 BQ00.
036020       DISPLAY CLR-SCREEN.
036030       DISPLAY S15.

036040 BQ05.
	     PERFORM AB35.
	     MOVE "H"		 TO W02-PRN-TYPE.
036050	     MOVE SPACES	 TO REP-DETAIL1.
036060       MOVE "PAGE:"        TO REP-PAGE.
036070       MOVE "DATE:"        TO REP-DATE.
036080       MOVE W95-COMP       TO REP-COMPANY.
036090       ADD 1               TO WS-PAGE.
036100       MOVE WS-PAGE        TO REP-P-NO.
036110       MOVE W12-TODAY      TO REP-DTE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
036130	     MOVE SPACES	 TO REP-DETAIL1.
036140       MOVE "PRODUCTION"   TO REP-HEAD10.
	     PERFORM CALL-PRINTUTL.
036160	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM AB25.
036170	     MOVE "Item Code"	 TO ORD-H1.
036180	     MOVE WS-DESC-H1	 TO ORD-H2.
036190	     MOVE "Quantity"	 TO ORD-H3.
           IF WS-TYPE = "C" OR "B"
036200	       MOVE "Unit Cost"  TO ORD-H4.
           IF WS-TYPE = "S" OR "B"
036210	       MOVE "Unit Sell"  TO ORD-H5.
           IF WS-TYPE = "C" OR "B"
036220	       MOVE "Total Cost" TO ORD-H6
           ELSE
036220	       MOVE "Total Sell" TO ORD-H6.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
036250	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

036260 BQ10.
036270	     MOVE SPACES	 TO W10-EXT-ITEM.
036280       MOVE ZERO           TO W10-QUANT W25-SCOST.
             MOVE 0701           TO CPOS.
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
036310 BQ15.
             PERFORM AA300.
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S05-ITM
	       ELSE
021070		   ACCEPT S05
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
029790	       ACCEPT W10-I3 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 4
029790	       ACCEPT W10-I4 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 5
029790	       ACCEPT W10-I5 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 6
029790	       ACCEPT W10-I6 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 7
029790	       ACCEPT W10-I7 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 8
029790	       ACCEPT W10-I8 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 9
029790	       ACCEPT W10-I9 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 10
029790	       ACCEPT W10-I10 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 11
029790	       ACCEPT W10-I11 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 12
029790	       ACCEPT W10-I12 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 13
029790	       ACCEPT W10-I13 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
	   ELSE
	   IF W10-SLNGTH = 14
029790	       ACCEPT W10-I14 AT 0602
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BQ35
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BQ15
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
		     GO TO BQ15
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
	       IF W10-EXT-ITEM = SPACES
                   GO TO BQ15.
	     DISPLAY CLEAR-L25.
036330	   IF W10-EXT-ITEM = SPACES
036340         GO TO BQ35.

       BQ15A.
036350       PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S05-EXT
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S05-ITM
	       ELSE
018630		   DISPLAY S05
	       END-IF
	   ELSE
	       DISPLAY S05.
036370	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S05-EXT
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S05-ITM
		   ELSE
		       DISPLAY S05
		   END-IF
	       ELSE
		   DISPLAY S05
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BQ15
               ELSE
                   GO TO BQ15A.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BQ15.

036400 BQ16.
	     MOVE STK-CODE	 TO PRT-EXT-ITEM.
             PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
036420	       MOVE "Item not manufactured or assembled"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
036430         GO TO BQ15.
036470	     DISPLAY STK-DESC AT 0702
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF NOT (STK-DESC2 = SPACES)
	       DISPLAY STK-DESC2 AT 0802
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
036560	   IF WS-OPTION = "N"
036570         GO TO BQ15.
      *
      *    ****    Q U A N T I T Y
      *
036580 BQ20.
036590       ACCEPT S35 AT 0733.
036600     IF W10-QUANT = ZERO
036610         GO TO BQ15.
	     PERFORM CHECK-CORRECT.
036690	   IF WS-OPTION = "N"
036700         GO TO BQ15.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
031890       PERFORM AY05 THRU AY59.
      *
      *    ****    S A V E   S T O C K	 R E C O R D
      *
036450       MOVE STK-RECORD1    TO W15-RECORD1.
036720	     MOVE 1		 TO WS-S6.
036730       DISPLAY "Busy calculating cost price" AT 2312
                      WITH FOREGROUND-COLOR 14.
036740       MOVE ZERO           TO W10-MCOST W10-MSELL
                                    W25-PCST  W25-PTOT.
      *
      *    ****    E N D   O F	 P R O D U C T I O N   D E T A I L S ???
      *
036750 BQ25.
036760	   IF PRT-EXT-CODE(WS-S6) = SPACES
036770         GO TO BQ30.
036780	     MOVE PRT-EXT-CODE(WS-S6)
                                 TO STK-CODE.
             MOVE 0              TO WS-ACTION.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE STK-CODE	 TO ORD-EXT-ITEM
	       MOVE "Invalid code"
				 TO ORD-DESC
	       IF W02-LINAGE < W02-PRN-LENGTH
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009780	       ELSE
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
037020		   PERFORM BQ05
	       END-IF
036250	       MOVE SPACES	 TO REP-DETAIL1
	       GO TO BQ28.
036800       PERFORM AY05 THRU AY59.
036810     IF STK-COST > STK-AVGE
036820	       COMPUTE W10-COST = (PRT-QUANT(WS-S6) * STK-COST)
036830     ELSE
036840	       COMPUTE W10-COST = (PRT-QUANT(WS-S6) * STK-AVGE).
	     COMPUTE W10-SELL = (PRT-QUANT(WS-S6) * STK-SELL).
036850	   IF STK-IND = 0 OR 1
	       COMPUTE W25-SCOST = W25-SCOST + (W10-COST * W10-QUANT)
           ELSE
034330	       ADD W10-COST TO W25-SCOST.
	     COMPUTE W10-MQUANT = PRT-QUANT(WS-S6) * W10-QUANT.
036870       COMPUTE W10-MCOST = W10-MCOST + W10-COST.
036850	   IF STK-IND = 0
036880	       SUBTRACT W10-MQUANT
				 FROM STK-QUANT
	       IF WS-CARDEX = "Y"
		   INITIALIZE STX-REC1
		   MOVE STK-CODE TO STX-EXT-CODE
		   MOVE W12-T-YMD
				 TO STX-DTE
		   MOVE ZERO	 TO STX-SEQ
		   MOVE LS0-NO	 TO STX-USER
		   MOVE "PRODCT" TO STX-PROG
		   MOVE LS-USER  TO STX-WS
		   MOVE "Used for Production"
				 TO STX-REMARKS
		   MULTIPLY -1 BY W10-MQUANT
				 GIVING STX-QNT
		   MULTIPLY -1 BY W10-COST
				 GIVING STX-COST
		   COMPUTE STX-SELL ROUNDED = (W10-COST +
			   ((W10-COST * STK-MARKUP) / 100)) * -1
		   PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
		   MOVE 1	 TO WS-ACTION
		   PERFORM AY46 THRU AY59
	       END-IF
013790	       IF NOT (STK-DISC > ZERO)
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
013810		   IF STK-QUANT NOT > STK-LEVEL
013820		       MOVE 1	 TO STK-DISC
013830		       PERFORM BG80.
036890       ADD W10-MQUANT      TO STK-MTD STK-YTD.
036900	     COMPUTE W10-CCOST = (PRT-QUANT(WS-S6) * STK-AVGE).
036910       ADD W10-CCOST       TO STK-MTDC STK-MTDV STK-YTDC STK-YTDV.
036920       MOVE SPACES         TO REP-LINE1.
036930	     MOVE PRT-QUANT(WS-S6)
                                 TO ORD-QUANT.
036940       MOVE STK-DESC       TO ORD-DESC.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO ORD-ITEM
	       MOVE "/" 	 TO ORD-SL
	       MOVE STK-ITM	 TO ORD-ITM
	   ELSE
037380	       MOVE STK-CODE	 TO ORD-EXT-ITEM.
	   IF WS-TYPE = "B" OR "C"
	       COMPUTE ORD-COST = (W10-COST / PRT-QUANT(WS-S6))
               MOVE W10-COST     TO ORD-VALUE.
           IF WS-TYPE = "B" OR "S"
               MOVE STK-SELL     TO ORD-SELL.
           IF WS-TYPE = "S"
               MOVE W10-SELL     TO ORD-VALUE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
037020	       PERFORM BQ05.
036250	     MOVE SPACES	 TO REP-DETAIL1.
	   IF (WS-USE-PACKS = "Y") AND
	      (STK-USE-PACKS = "Y")
	       PERFORM BQ90 THRU BQ105.
037030	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

       BQ28.
037040     IF WS-S6 < 30
037050         ADD 1             TO WS-S6
037060         GO TO BQ25.

037070 BQ30.
037080     IF WS-S6 = 1
037090         GO TO BQ40.
037100       MOVE W15-RECORD1    TO STK-RECORD1.
	     COMPUTE W10-COST ROUNDED = W10-MCOST * 1.
	     MOVE W10-COST	 TO W80-5.

037130 BQ32.
           IF WS-TYPE = "B" OR "C"
037140	       DISPLAY W80-5 AT 0749
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
037150	     COMPUTE W10-SELL ROUNDED = (W10-COST +
037160		     ((W10-COST * STK-MARKUP) / 100)).
             MOVE W10-SELL       TO W10-MSELL.
           IF STK-CMARKUP NOT > ZERO
               MOVE STK-MARKUP   TO STK-CMARKUP.
037170	     COMPUTE W10-CASH ROUNDED = (W10-COST +
037180		     ((W10-COST * STK-CMARKUP) / 100)).
           IF STK-WMARKUP NOT > ZERO
               MOVE STK-CMARKUP  TO STK-WMARKUP.
037170	     COMPUTE W10-WSALE ROUNDED = (W10-COST +
037180		     ((W10-COST * STK-WMARKUP) / 100)).
037190       MULTIPLY W10-COST BY W10-QUANT 
                                 GIVING W25-VALUE.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
           IF W05-ROUND = "E"
               MOVE W10-MSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL W10-MSELL
               MOVE W10-WSALE    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
               MOVE W10-CASH     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
           IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.

       BQ33.
037200       ACCEPT S36 AT 0759.
	   IF W10-SELL < W10-WSALE
	       MOVE "Selling price margin too small"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       MOVE W10-WSALE	 TO W10-SELL
               GO TO BQ33.
           IF WS-TYPE = "B" OR "C"
037210         MOVE W25-VALUE    TO ORD-VALUE
037220	       DISPLAY ORD-VALUE AT 0769
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	       MOVE ZERO	 TO W90-BAL.
	   IF STK-IND = 0
	       IF STK-QUANT > ZERO
037230		   MULTIPLY STK-QUANT BY STK-AVGE
				 GIVING W90-BAL.
037240       ADD W25-SCOST       TO W25-PCOST W25-PCST.
037250       ADD W25-VALUE       TO W25-TOTAL W25-PTOT.
             COMPUTE W25-TSELL = W25-TSELL + (W10-MSELL * W10-QUANT).
037260       ADD W25-VALUE       TO W90-BAL.
	   IF STK-IND = 0
037270	       ADD W10-QUANT	 TO STK-QUANT
	       IF WS-CARDEX = "Y"
		   INITIALIZE STX-REC1
		   MOVE STK-CODE TO STX-EXT-CODE
		   MOVE W12-T-YMD
				 TO STX-DTE
		   MOVE ZERO	 TO STX-SEQ
		   MOVE LS0-NO	 TO STX-USER
		   MOVE "PRODCT" TO STX-PROG
		   MOVE LS-USER  TO STX-WS
		   MOVE "Production"
				 TO STX-REMARKS
		   MOVE W10-QUANT
				 TO STX-QNT
		   MOVE W25-VALUE
				 TO STX-COST
		   COMPUTE STX-SELL ROUNDED = (W25-VALUE +
			   ((W25-VALUE * STK-MARKUP) / 100))
		   PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
		   MOVE 1	 TO WS-ACTION
		   PERFORM AY46 THRU AY59.
037280       MOVE W10-QUANT      TO ORD-QUANT.
	   IF (STK-IND = 0) AND
	      (STK-QUANT > W10-QUANT)
037290	       DIVIDE W90-BAL BY STK-QUANT
				 GIVING STK-AVGE
	   ELSE
	       MOVE W10-COST	 TO STK-AVGE.
037300     IF STK-AVGE < ZERO
037310         COMPUTE STK-AVGE = STK-AVGE * -1.
037320       MOVE W10-COST       TO STK-COST.
           IF WS-TYPE = "B" OR "C"
               MOVE W10-COST     TO ORD-COST.
037330     IF W10-MSELL > STK-SELL
037340         MOVE W10-MSELL    TO STK-SELL.
037350     IF W10-CASH > STK-CASH
037360         MOVE W10-CASH     TO STK-CASH.
037350     IF W10-WSALE > STK-WSALE
037360         MOVE W10-WSALE    TO STK-WSALE.
037370       MOVE STK-DESC       TO ORD-DESC.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO ORD-ITEM
	       MOVE "/" 	 TO ORD-SL
	       MOVE STK-ITM	 TO ORD-ITM
	   ELSE
037380	       MOVE STK-CODE	 TO ORD-EXT-ITEM.
	   IF WS-TYPE = "B" OR "S"
               MOVE W10-MSELL    TO ORD-SELL
               COMPUTE ORD-VALUE = W10-MSELL * W10-QUANT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
037460	       PERFORM BQ05.
037470	     MOVE SPACES	 TO REP-DETAIL1.
	   IF (WS-USE-PACKS = "Y") AND
	      (STK-USE-PACKS = "Y")
	       PERFORM BQ70 THRU BQ85.
037480	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
037630       SUBTRACT W25-PCST   FROM STK-TOTAL.
037640       ADD W25-PTOT        TO STK-TOTAL.
037500       PERFORM AY70 THRU AY999.

037510 BQ34.
037520       GO TO BQ40.

037530 BQ35.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
037550	     MOVE "TOTAL"	 TO ORD-DESC.
           IF WS-TYPE = "S"
037560         MOVE W25-TSELL    TO ORD-VALUE
           ELSE
037560         MOVE W25-TOTAL    TO ORD-VALUE.
	     PERFORM CALL-PRINTUTL.
037580	     MOVE "***** END REPORT *****" TO REP-DETAIL1.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO REP-DETAIL1.
037660	     GO TO BQ999.
037670 BQ40.
	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
037760	   IF WS-OPTION = "E"
037770         GO TO BQ35.
037780       GO TO BQ10.

       BQ70.
	     MOVE 2		 TO WS-S1.

       BQ75.
	   IF STK-UNT(WS-S1) = ZERO
	       GO TO BQ80.
016780	     MULTIPLY STK-UNT(WS-S1) BY STK-COST
				 GIVING W10-CCOST.
016790	     MOVE STK-SL(WS-S1)	 TO W10-CSELL.
016840	   IF STK-SL(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-MKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-MKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-CSH(WS-S1) TO W10-CSELL.
016840	   IF STK-CSH(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-CMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-CMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-WSL(WS-S1) TO W10-CSELL.
016840	   IF STK-WSL(WS-S1) < W10-CCOST
	       MOVE ZERO	 TO STK-WMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-WMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *
           IF W05-ROUND = "E"
	       MOVE STK-SL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-SL(WS-S1)
	       MOVE STK-WSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-WSL(WS-S1)
	       MOVE STK-CSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-CSH(WS-S1).
	   IF STK-TAX NOT = ZERO
	       MOVE STK-TAX	 TO W10-TAX
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
				 (W10-SL(WS-S1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
				 (W10-WSL(WS-S1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
				 (W10-CSH(WS-S1) * W05-RTE)
           ELSE
	       MOVE W10-WSL(WS-S1)
				 TO W10-VWSL(WS-S1)
	       MOVE W10-CSH(WS-S1)
				 TO W10-VCSH(WS-S1)
	       MOVE W10-SL(WS-S1)
				 TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
	       MOVE W10-VSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VSL(WS-S1)
	       MOVE W10-VWSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VWSL(WS-S1)
	       MOVE W10-VCSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VCSH(WS-S1)
	       PERFORM CB300.
	     MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
	     MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
	     MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BQ80.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BQ75.

       BQ85.
	     EXIT.

       BQ90.
	     MOVE 2		 TO WS-S1.

       BQ95.
	   IF STK-UNT(WS-S1) = ZERO
	       GO TO BQ100.
016780	     MULTIPLY STK-UNT(WS-S1) BY STK-COST
				 GIVING W10-CCOST.
016790	     MOVE STK-SL(WS-S1)	 TO W10-CSELL.
016840	   IF STK-SL(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-MKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-MKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-CSH(WS-S1) TO W10-CSELL.
016840	   IF STK-CSH(WS-S1) < W10-CCOST
016850	       MOVE ZERO	 TO STK-CMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-CMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
016790	     MOVE STK-WSL(WS-S1) TO W10-CSELL.
016840	   IF STK-WSL(WS-S1) < W10-CCOST
	       MOVE ZERO	 TO STK-WMKUP(WS-S1)
	   ELSE
016860	       COMPUTE STK-WMKUP(WS-S1) ROUNDED =
		     ((W10-CSELL - W10-CCOST) / W10-CCOST) * 100.
      *
      *    ****    C H E C K   I F   E X C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *
           IF W05-ROUND = "E"
	       MOVE STK-SL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-SL(WS-S1)
	       MOVE STK-WSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-WSL(WS-S1)
	       MOVE STK-CSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-CSH(WS-S1).
	   IF STK-TAX NOT = ZERO
	       MOVE STK-TAX	 TO W10-TAX
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-S1) ROUNDED = W10-SL(WS-S1) +
				 (W10-SL(WS-S1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-S1) ROUNDED = W10-WSL(WS-S1) +
				 (W10-WSL(WS-S1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-S1) ROUNDED = W10-CSH(WS-S1) +
				 (W10-CSH(WS-S1) * W05-RTE)
           ELSE
	       MOVE W10-WSL(WS-S1)
				 TO W10-VWSL(WS-S1)
	       MOVE W10-CSH(WS-S1)
				 TO W10-VCSH(WS-S1)
	       MOVE W10-SL(WS-S1)
				 TO W10-VSL(WS-S1).
           IF W05-ROUND = "I"
	       MOVE W10-VSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VSL(WS-S1)
	       MOVE W10-VWSL(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VWSL(WS-S1)
	       MOVE W10-VCSH(WS-S1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VCSH(WS-S1)
	       PERFORM CB300.
	     MOVE W10-SL(WS-S1)  TO STK-SL(WS-S1).
	     MOVE W10-CSH(WS-S1) TO STK-CSH(WS-S1).
	     MOVE W10-WSL(WS-S1) TO STK-WSL(WS-S1).

       BQ100.
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BQ95.

       BQ105.
	     EXIT.

039190 BQ999.
039200       EXIT.

      /
044130 ZA000-INIT	       SECTION 8.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB
				    WS-DRAW.
044150	     DISPLAY CLR-SCREEN.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
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
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.

       ZA01B.
053630	     OPEN I-O AUDIT.
053640     IF RUNTIME-ERROR
053650         IF FLE-LOCKED
053660             GO TO ZA200
053670         ELSE
053680         IF FLE-LIMIT
053690             GO TO ZA49.
053700     IF WS-STATUS NOT = "00"
	       IF NOT (WS-STATUS = "41")
053710		   MOVE 1	 TO WS-F-ERROR
053720		   PERFORM OPEN-ERROR.
044540	     OPEN OUTPUT RECOVER.
044550       CLOSE RECOVER.
044560       OPEN I-O RECOVER.
044580       MOVE 1              TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
044650       MOVE 3              TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
042520       MOVE 5              TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-DESC-H1 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H1  TO WS-DESC-H1.
	   IF NOT (PAR-DESC-H2 = X"0000000000000000000000000000")
	       MOVE PAR-DESC-H2  TO WS-DESC-H2.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
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
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 8.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
051800	   IF WS-F-ERROR = 1
051810	       MOVE W02-AUDITF	 TO WS-FILE
051820	       MOVE WS-AUDKEY	 TO WS-KEY
051830     ELSE
041610	   IF WS-F-ERROR = 2
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
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 46
041860	       MOVE W02-STKDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STX-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
042170	     STOP RUN.
