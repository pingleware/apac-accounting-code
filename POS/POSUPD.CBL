      $set LINKCOUNT"596"
      ******************************************************************
      *                                                                *
      *   *******    ******    ******	**    **  *******   *******    *
      *   **	**  **	  **  **    **	**    **  **	**  **	  **   *
      *   **	**  **	  **  **	**    **  **	**  **	  **   *
      *   *******   **	  **   ******	**    **  *******   **	  **   *
      *   **	    **	  **	    **	**    **  **	    **	  **   *
      *   **	    **	  **  **    **	**    **  **	    **	  **   *
      *   **	     ******    ******	 ******   **	    *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     U P D A T E   P O S T   P O S   T R A N S A C T I O N S    *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     POSUPD.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JANUARY 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
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

      $SET KEYCOMPRESS "7"
000010	   SELECT BULK	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD BLK-KEY.
      $SET NOKEYCOMPRESS

       COPY CARDEX.SL.

       COPY CONTROL.SL.

000140 COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000240 COPY RECOVER.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT SLIP	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD SLP-KEY
			  ALTERNATE RECORD SLP-TKEY =
			     SLP-TYPE SLP-KEY
			  ALTERNATE RECORD SLP-DKEY =
			     SLP-DRAW SLP-KEY.
      $SET NOKEYCOMPRESS

       COPY SHARED.SL.

000290 COPY SPARTS.SL.

000180 COPY STOCK.SL.

000180 COPY TXTRAN.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000450 COPY AUDIT.FDE.

       COPY CARDEX.FDE.

      /
000010 FD  BULK      LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-BULK.
000030 01  BLK-RECORD1.
	   03  BLK-KEY.
000050	       05  BLK-ITEM    PIC  X(14).
	       05  BLK-ITM     PIC  X(03).
	       05  FILLER      PIC  X(01).
	   03  BLK-EXT-ITEM REDEFINES BLK-KEY
			       PIC  X(18).
000100	   03  BLK-QNT	       PIC S9(09)V9999 COMP-3.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000280 COPY PARAM.FDE.

000470 COPY RECOVER.GFD.

       COPY SHARED.FDE.

       COPY SLIP.FDE.

000360 COPY SPARTS.FDE.

000300 COPY STOCK.FDE.

000180 COPY TXTRAN.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	  COMP-3.
001710 77  WS-SUB	   PIC	9(04)	  COMP-5.
000600 77  WS-SUPER	   PIC	9(02)	  COMP-5.
001710 77  WS-S1	   PIC	9(04)	  COMP-5.
001720 77  WS-S2	   PIC	9(04)	  COMP-5.
001730 77  WS-S3	   PIC	9(04)	  COMP-5.
001740 77  WS-S4	   PIC	9(04)	  COMP-5.
001750 77  WS-S5	   PIC	9(04)	  COMP-5.
001760 77  WS-S6	   PIC	9(04)	  COMP-5.
001770 77  WS-S7	   PIC	9(04)	  COMP-5.
001780 77  WS-S8	   PIC	9(04)	  COMP-5.
001710 77  WS-P1	   PIC	9(04)	  COMP-5.
001790 77  WS-IXS	   PIC	9(04)	  COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	  COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	  COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	  COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	  COMP-5.
       77  WS-AUDKEY	   PIC	9(06)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	  COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	  COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	  COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	  COMP-5.
001900 77  WS-RECORDS	   PIC	9(06)	  COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	   PIC	9(04)	  COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	  COMP-5.
001980 77  WS-ITM	   PIC	X(14).
       77  WS-ETYPE	   PIC	X(01).
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002220 77  WS-PROC	   PIC	X(01).
002260 77  WS-BATCH	   PIC	9(05).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-LABCDE	   PIC	X(08).
002280 77  WS-PASS	   PIC	X(08).
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

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "BLK".

       COPY LEDTRF.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY AUDIT.ID.

       COPY BULK.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

000470 COPY RECOVER.ID.

       COPY SHARED.ID.

000140 COPY SLIP.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

       COPY TXTRAN.ID.

       COPY W05.VAT.

       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.SAL.

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
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.
001660	   03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       COPY W70DEBT.IWS.

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

003230 01  W90-TRAN.
003990	   03  W90-DATE        PIC  9(08)    COMP-3.
004000	   03  W90-DTE	       PIC  99/99/9999.
003270	   03  W90-CODE        PIC  9(02).
003280     03  W90-ORD         PIC  X(12).
003290	   03  W90-REF	       PIC  X(08) VALUE SPACES.
003300     03  W90-REG         PIC  X(09).
003310	   03  W90-DB	       PIC  9(09)V99 COMP-3.
003320	   03  W90-CR	       PIC  9(09)V99 COMP-3.
003330	   03  W90-BDT	       PIC S9(09)V99 COMP-3.
003340	   03  W90-BCR	       PIC S9(09)V99 COMP-3.
003350     03  W90-DESC        PIC  X(12).
003400	   03  W90-DUE	       PIC S9(09)V99 COMP-3.
003410	   03  W90-CASH        PIC S9(09)V99 COMP-3.
003420	   03  W90-CHQ	       PIC S9(09)V99 COMP-3.
003430	   03  W90-CARD        PIC S9(09)V99 COMP-3.
003430	   03  W90-SPEED       PIC S9(09)V99 COMP-3.
003430	   03  W90-VOUCH       PIC S9(09)V99 COMP-3.
003440	   03  W90-CHANGE      PIC S9(09)V99 COMP-3.
003450	   03  W90-PMT	       PIC S9(09)V99 COMP-3.
003470	   03  W90-BAL	       PIC S9(08)V99 COMP-3.
003480     03  W90-TAX         PIC  X(14).
003490	   03  W90-DVAL        PIC  9(09)V99.
003500	   03  W90-SALES       PIC S9(09)V99  COMP-3.
003510	   03  W90-TDISC       PIC S9(09)V99  COMP-3.
003520	   03  W90-ATAX        PIC S9(09)V99  COMP-3.
003520	   03  W90-TTAX        PIC S9(09)V99  COMP-3.
003530	   03  W90-TCOST       PIC S9(09)V99  COMP-3.
003540	   03  W90-TSELL       PIC S9(09)V999 COMP-3.
003550	   03  W90-DISC        PIC S9(09)V99  COMP-3.
003560	   03  W90-CRD	       PIC S9(09)V99  COMP-3.
003570	   03  W90-CDISC       PIC S9(09)V99  COMP-3.
003580	   03  W90-LDISC       PIC S9(09)V99  COMP-3.
	   03  W90-TAXABLE     OCCURS 12.
006790	       05  W90-TAXVAL  PIC S9(09)V99 COMP-3.
	       05  W90-VAT     PIC S9(09)V99 COMP-3.
006550	   03  W90-EXCDIS      PIC S9(09)V99  COMP-3.
006550	   03  W90-EXCVAL      PIC S9(09)V99  COMP-3.
006550	   03  W90-EXCTOT      PIC S9(09)V99  COMP-3.
003600	   03  W90-NONTAX      PIC S9(09)V99  COMP-3.
003610	   03  W90-VALUE       PIC S9(09)V99  COMP-3.
006570	   03  W90-STOTAL      PIC S9(09)V99  COMP-3.
003620	   03  W90-TOTAL       PIC S9(09)V9(06) COMP-3.
	   03  W90-SLIP-TOT    PIC  9(09)V9(02).
	   03  W90-SLIP-RND REDEFINES W90-SLIP-TOT.
	       05  FILLER      PIC  X(06).
	       05  W90-RR      PIC  9(01).
	       05  W90-RC      PIC  9(02).
	   03  W90-ROUND       PIC  9(01)V99.
	   03  W90-ROUNDING REDEFINES W90-ROUND.
	       05  W90-R       PIC  9(01).
	       05  W90-C       PIC  9(02).
	   03  W90-RTVAL       PIC  9(01)V99 COMP-3.
	   03  W90-RTTAX       PIC  9(01)V99 COMP-3.

004120 01  W95-COMPANY.
	   03  W95-COMP	       PIC X(40).
	   03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

005530 01  S05.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550                           PIC X(14) USING W10-ITEM AUTO.

005530 01  S05-3.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I3 AUTO.

005530 01  S05-4.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S05-5.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S05-6.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S05-7.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S05-8.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S05-9.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S05-10.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S05-11.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S05-12.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S05-13.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S05-14.
005540	   03  LINE 21 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY S99.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004790 AA000		  SECTION.
004800 AA000-INIT.
003240	   IF LS0-STLEV < 3 AND LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004810	     PERFORM ZA000-INIT.
	   IF WS-OPTION = "9"
	       GO TO AA49.
	     PERFORM BA000.
	     CLOSE SLIP.
	     OPEN OUTPUT SLIP.
	     CLOSE AUDIT
		   BULK
		   CARDEX
		   LEDTRF
		   RECOVER
		   SLIP
		   SPARTS
		   STOCK.

       AA49.
	     EXIT PROGRAM.

       COPY AA50.LUP.

       COPY CLEAR.CRT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ	       SECTION.

       COPY AUDIT.RD.

000020 READ-BULK.
000030	     READ BULK
	       KEY IS BLK-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF WS-STATUS = "23"
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-BULK	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-BULK.
000050	     GO TO READ-BULK-EXIT.

000020 READ-BULK-NEXT.
000030	     READ BULK NEXT.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
	       MOVE W02-BULK	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
000100         PERFORM LOCKED-RECORD
000110	       GO TO READ-BULK-NEXT.
000050	     GO TO READ-BULK-EXIT.

000020 START-AT-BULK-KEY.
000030	     START BULK
	       KEY >= BLK-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
           IF WS-STATUS = "23"
	       MOVE 59		 TO WS-F-ERROR
000050	       GO TO READ-BULK-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 59		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.
	     GO TO READ-BULK-EXIT.

000150 READ-BULK-EXIT.
000160       EXIT.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

011310 COPY SLIP.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE	       SECTION.

       COPY AUDIT.WR.

010000 REWRITE-BULK.
010010	     REWRITE BLK-RECORD1.
000030     IF WS-STAT1 NOT = "0"
010030	       MOVE 59		 TO WS-F-ERROR
010040         PERFORM WRITE-ERROR.
010050	     GO TO WRITE-BULK-EXIT.

010060 WRITE-BULK.
010070	     WRITE BLK-RECORD1.
000030     IF WS-STAT1 NOT = "0"
010120	       MOVE 59		 TO WS-F-ERROR
010130	       PERFORM WRITE-ERROR.
	     CLOSE BULK.
	     OPEN I-O BULK.
010050	     GO TO WRITE-BULK-EXIT.

000160 DELETE-BULK-REC.
000170	     DELETE BULK.
000180     IF WS-STAT1 NOT = "0"
000190	       MOVE 59		 TO WS-F-ERROR
000200         PERFORM WRITE-ERROR.
000230
010140 WRITE-BULK-EXIT.
010150       EXIT.

       COPY CARDEX.WR.

       COPY CONTROL.WR.

       COPY LEDTRF.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY SLIP.WR.

       COPY STOCK.WR.

       COPY TXTRAN.WR.

       COPY APAC.HLP.

       COPY LEDTRF.UPD.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
       AY35.
	     MOVE 35		 TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
	     MOVE SLP-RECORD1	 TO REC-SLIP.
             GO TO AY50.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
012090 AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
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

012200 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
       AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390	       DISPLAY "Write error Recovery file - Status " AT 2212
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012430       CLOSE RECOVER.
012440       OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.
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
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
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
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
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
      *            NETWORK record 1.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
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
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013080 AY999.
013090	     EXIT.
      /
       BA000	       SECTION 50.
       BA00.
009640	     DISPLAY CLR-SCREEN.
	     DISPLAY "UPDATE POS TRANSACTIONS" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0624
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Reading POS transaction file ³" AT 0724
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³                              ³" AT 0824
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 0924
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE 13		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1227
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ " AT 1327
		     WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "Accumulate Bulk Stock"
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 7
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³                       ³" AT 1427
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1527
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

	     MOVE 1		 TO WS-S7.
       BA02.
018660	     MOVE ZERO		 TO W90-VAT(WS-S7) W90-TAXVAL(WS-S7).
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO BA02.
	     INITIALIZE SLP-RECORD1.
	     MOVE ZERO		 TO SLP-SEQ
013330				    SLP-REF.
	     PERFORM START-AT-SLIP-KEY THRU READ-SLIP-EXIT.
	   IF WS-F-ERROR = 38
	       GO TO BA20.

       BA05.
	     PERFORM READ-SLIP-NEXT THRU READ-SLIP-EXIT.
	   IF WS-F-ERROR = 38
	       GO TO BA20.
	     DISPLAY SLP-REF AT 0833
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     " " WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 5
		     SLP-SEQ WITH FOREGROUND-COLOR 6 HIGHLIGHT
				  BACKGROUND-COLOR 5.
      *
      *    ****    H E A D E R	  R E C O R D
      *
	   IF SLP-TYPE = "H"
012560	       MOVE ZERO	 TO W90-TTAX  W90-ATAX
				    W90-LDISC W90-TCOST
	       MOVE SLP-REF	 TO W90-REF
	       MOVE SLP-SMAN	 TO W10-SMAN
	       MOVE SLP-VATNO	 TO W05-VATNO
	       MOVE SLP-TAX	 TO W90-TAX
	       MOVE SLP-DRAW	 TO WS-DRAW
	       MOVE SLP-DATE	 TO W12-TODAY
	       MOVE W12-DAY	 TO W12-DD
	       MOVE W12-MONTH	 TO W12-MM
	       MOVE W12-YEAR	 TO W12-YY
	       PERFORM AY60 THRU AY999
	       GO TO BA05.
      *
      *    ****    T O T A L   R E C O R D
      *
	   IF SLP-TYPE = "T"
	       IF WS-PROC = "N"
		   GO TO BA05
	       ELSE
		   GO TO BA15.
      *
      *    ****    P R O C E S S E D   I T E M	 R E C O R D
      *
	   IF SLP-TYPE = "J"
	       GO TO BA05.
      *
      *    ****    R E M A R K S   R E C O R D
      *
	   IF SLP-ITEM = SPACES
	       GO TO BA05.
	   IF SLP-REF = W90-REF
	       MOVE "Y"		 TO WS-PROC.
      *
      *    ****    I T E M   R E C O R D
      *
031260	     MOVE SLP-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.

       BA08.
031270	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      *
      *    ****    C H E C K   I F   C O D E   A M E N D E D   O R
      * 	   D E L E T E D   ???
      *
	   IF WS-F-ERROR = 22
	       PERFORM BB000
	       GO TO BA08.
	     MOVE 0		 TO WS-ACTION.
             PERFORM AY05 THRU AY59.
	   IF SLP-3DEC = "Y" OR "3"
	       MOVE SLP-WEIGHT	 TO W10-QUANT
	   ELSE
	       MOVE SLP-QNT	 TO W10-QUANT.
031290	     MOVE SLP-COST	 TO W10-COST.
	     COMPUTE W90-TCOST = W90-TCOST + (W10-COST * W10-QUANT).
031300	     MOVE SLP-SELL	 TO W10-SELL.
031310	     MOVE SLP-DISC	 TO W90-CDISC.
031320	     MOVE SLP-DSC	 TO WS-DISC.
031330	     MOVE SLP-VAL	 TO W25-VALUE.
      *	     MOVE SLP-VAT	 TO W10-VAT.
	     MOVE SLP-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE SLP-TAXVAL	 TO W90-VALUE.
	     ADD SLP-NONTAX	 TO W90-NONTAX.
      *	     MOVE SLP-INCTAX	 TO W10-INCTAX.
	     MOVE SLP-SUPER	 TO WS-SUPER.
023790	     MOVE SLP-MSER	 TO W10-REMKS.
      *	   IF WS-VAT-SUB > 0
022580*	       ADD W90-VALUE	 TO W90-TAXVAL(WS-VAT-SUB).
013670	     SUBTRACT W90-CDISC FROM W25-VALUE GIVING W90-VALUE.
	     ADD W90-CDISC	 TO W90-LDISC.
      *
      *    ****    S E T U P   C A R D E X   E N T R Y
      *
	   IF WS-CARDEX = "Y"
	       MOVE "CASH"	 TO CRD-AC
	       MOVE ZERO	 TO CRD-SEQ
017540	       MOVE W10-SMAN	 TO CRD-SMAN
017550	       MOVE W90-REF	 TO CRD-REF
	       MOVE STK-SUPP	 TO CRD-SUPP
	       MOVE W12-T-YMD	 TO CRD-DTE
	       MOVE "CASH SALE"	 TO CRD-NAME
	       MOVE SPACES	 TO CRD-WHS
013590	       MOVE SLP-EXT-ITEM TO CRD-EXT-CODE
013610	       MOVE W10-QUANT	 TO CRD-QNT
013630	       MOVE SLP-COST	 TO CRD-COST
013640	       MOVE SLP-SELL	 TO CRD-SELL
	       MOVE STK-LDG	 TO CRD-DEPT
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59.
	   IF SLP-VAT-SUB NOT = ZERO
	       MOVE W05-VAT(SLP-VAT-SUB)
                                 TO W05-RATE
	       COMPUTE W90-VALUE ROUNDED = W90-VALUE -
		   (((W90-VALUE / (W05-RATE + 100.00000)) *
		      W05-RATE))
	       COMPUTE W90-ATAX = W90-ATAX + ((W25-VALUE -
				  W90-CDISC) - W90-VALUE)
	       COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
		   (((W90-CDISC / (W05-RATE + 100.00000)) *
		      W05-RATE))
	       ADD W90-VALUE	 TO W90-EXCVAL.
      *
      *    ****    R E A D   D E P A R T M E N T   R E C O R D
      *
026950	     MOVE STK-LDG	 TO DPT-CODE.
	     MOVE ZERO		 TO WS-ACTION.
026960       PERFORM AY38 THRU AY59.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
026970	     ADD W90-VALUE	 TO DPT-SALES-DAY
026980                              DPT-SALES-MTD
026990                              DPT-SALES-YTD.
      *
      *    ****    U P D A T E	 G / L	 I N T E G R A T I O N
      * 	   B A T C H   -   S A L E S
      *
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "CASH SALES" TO WX-NAR
               COMPUTE WX-VALUE = W90-VALUE * -1
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 04           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
027160       COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
027000       ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
027010                              DPT-COST-YTD.
      *
      *    ****    U P D A T E	 G / L	 I N T E G R A T I O N
      * 	   B A T C H   -   C O S T   O F   S A L E S
      *
	   IF WX-INT = "Y"
               IF W90-BCR NOT = ZERO
                   MOVE W90-BCR  TO WX-VALUE
                   MOVE DPT-COST-GL
                                 TO WX-AC
                   MOVE 08       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
                   COMPUTE WX-VALUE = W90-BCR * -1
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE 19       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
027020       ADD W90-EXCDIS      TO DPT-DISC-DAY
027030                              DPT-DISC-MTD
027040                              DPT-DISC-YTD
                                    DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD.
027050       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
013750       MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D    
      *
	   IF WS-PRC-IND = "Y"
027590	       IF STK-IND > 1
027600		   GO TO BA10
	       END-IF
	   ELSE
	   IF STK-IND > ZERO
	       GO TO BA10.
013780       SUBTRACT W10-QUANT  FROM STK-QUANT.
013790	   IF STK-DISC > ZERO
013800	       GO TO BA10.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
013810     IF STK-QUANT NOT > STK-LEVEL
013820         MOVE 1            TO STK-DISC
013830         PERFORM BG80.
      *
      *    ****    U P D A T E	 S T O C K   S T A T S
      *
       BA10.
013860	     ADD W90-BCR	 TO W90-BDT STK-MTDC STK-YTDC.
013870       ADD W10-QUANT       TO STK-MTD STK-YTD.
013880       ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
013950       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    L O G   T R A N S A C T I O N   T O	 T H E
      * 	   A U D I T   F I L E
      *
013960       MOVE W70-AUDIT      TO WS-AUDKEY.
013970       MOVE LOW-VALUES     TO AUD-REC1.
013980       PERFORM AY16 THRU AY59.
013990       MOVE 6              TO AUD-TYPE.
014000       MOVE WS-DRAW        TO AUD-SUB.
014010       MOVE W90-REF        TO AUD-CSH.
014020       MOVE W10-SMAN       TO AUD-SMAN.
014030       MOVE W90-VALUE      TO AUD-SEL.
014040       MOVE W90-BCR        TO AUD-CST.
014050	     MOVE STK-CODE	 TO AUD-EXT-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
014060       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
014070       ADD 1               TO W70-AUDIT.
014080       MOVE 1              TO WS-AUDKEY.
014090       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
014100       PERFORM AY16 THRU AY59.
014110       MOVE 1              TO AUD-REP6.
014120       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
      *
      *      ****    C H E C K	 I F   B U L K	 S T O C K   M U S T
      * 	     B E   U P D A T E D
      *
	     MOVE SLP-ITEM	 TO PRT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510	   IF WS-F-ERROR = 21
	       GO TO BA12.
      *
      *    ****    A C C U M U L A T E	 P R O D U C E D   I T E M S
      * 	   F O R   U P D A T I N G   O F   B U L K   S T O C K
      *
	     MOVE SLP-ITEM	 TO BLK-ITEM.
	     MOVE SPACES	 TO BLK-ITM.
	     MOVE ZERO		 TO BLK-QNT.
	     PERFORM READ-BULK THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       PERFORM WRITE-BULK THRU WRITE-BULK-EXIT.
	     DISPLAY BLK-ITEM AT 1432
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF SLP-3DEC = "Y" OR "3"
	       ADD SLP-WEIGHT	 TO BLK-QNT
	   ELSE
	       ADD SLP-QNT	 TO BLK-QNT.
	     PERFORM REWRITE-BULK THRU WRITE-BULK-EXIT.
      *
      *    ****    F L A G   A S   P R O C E S S E D
      *
       BA12.
	     MOVE "J"		 TO SLP-TYPE.
	     PERFORM REWRITE-SLIP THRU WRITE-SLIP-EXIT.
	     GO TO BA05.
      *
      *    ****    P R O C E S S   T O T A L   R E C O R D
      *
       BA15.
	     MOVE SLP-TIME2	 TO W95-TIME.
	     MOVE SLP-TOTAL	 TO W90-DB W90-TSELL W90-TOTAL.
	     COMPUTE W90-TDISC = SLP-DISCOUNT * -1.
	     MOVE 1		 TO WS-S7.

       BA16.
	     MOVE SLP-TAXABLE(WS-S7)
				 TO W90-TAXVAL(WS-S7).
	     MOVE SLP-VATVAL(WS-S7)
				 TO W90-VAT(WS-S7).
	     ADD SLP-VATVAL(WS-S7)
				 TO W90-TTAX.
	   IF WS-S7 < 6
	       ADD 1		 TO WS-S7
	       GO TO BA16.
	     MOVE SLP-EXCTAX	 TO W90-EXCTOT.
	     MOVE SLP-CASH	 TO W90-CASH.
	     MOVE SLP-CHQ	 TO W90-CHQ.
	     MOVE SLP-CARD	 TO W90-CARD.
	     MOVE SLP-SPEED	 TO W90-SPEED.
	     MOVE SLP-VOUCH	 TO W90-VOUCH.
	     MOVE SLP-CHANGE	 TO W90-CHANGE.
	     MOVE SLP-TAX	 TO W90-TAX.
	     MOVE SLP-ROUND	 TO W90-ROUND.
	     COMPUTE W90-ROUND = W90-ROUND * -1.
	     MOVE SLP-RTTAX	 TO W90-RTTAX.
	     MOVE SLP-RTVAL	 TO W90-RTVAL.
014300	   IF W90-TDISC NOT = ZERO
014310	       IF W90-LDISC = ZERO
027690		   MOVE "YYYY"	 TO DPT-CODE
		   MOVE ZERO	 TO WS-ACTION
027700		   PERFORM AY38 THRU AY59
027710		   ADD W90-TDISC TO DPT-DISC-DAY
027720				    DPT-DISC-MTD
027730				    DPT-DISC-YTD
		   IF WX-INT = "Y"
		       MOVE W90-EXCDIS
				 TO WX-VALUE
		       MOVE DPT-DISC-GL
				 TO WX-AC
		       MOVE DPT-CODE
				 TO WX-LEDG
		       MOVE 15	 TO WX-TYPE
		       PERFORM GL-TRF-UPDATE
		   END-IF
027740		   PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
014480	     MOVE 9		 TO WS-PARKEY.
014500       PERFORM AY01 THRU AY59.
	     MOVE 1		 TO W05-V-RATE.

       BA16-VAT.
	   IF W90-VAT(W05-V-RATE) = ZERO
	       GO TO BA16-CHECK-END.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
027840	     PERFORM AY38 THRU AY59.
      *
      *    ****    V A T   S T A T I S T I C S
      *
027850	     ADD W90-VAT(W05-V-RATE)
				 TO DPT-O-VAT-DAY
027860				    DPT-O-VAT-MTD
027870				    DPT-O-VAT-YTD.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO DPT-V-SALES-DAY
				    DPT-V-SALES-MTD
				    DPT-V-SALES-YTD.
	   IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
		COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
		MOVE DPT-O-VAT-GL
				 TO WX-AC
		MOVE DPT-CODE	 TO WX-LEDG
                MOVE 10          TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.

       BA16-CHECK-END.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO BA16-VAT.
	     ADD W90-NONTAX	 TO PAR-DNGS-DAY
				    PAR-DNGS-MTD
				    PAR-DNGS-YTD.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     MOVE 2		 TO TAX-TYPE.
	     MOVE 2		 TO TAX-ACTYPE.
	     MOVE 01		 TO TAX-CODE.
	     MOVE W12-T-YMD	 TO TAX-DATE.
	     MOVE W90-REF	 TO TAX-REF.
	     MOVE W90-NONTAX	 TO TAX-TAXFREE.
	     COMPUTE TAX-VALUE = (W90-TOTAL - (W90-DISC + W90-ROUND)).
	     MOVE ZERO		 TO TAX-SEQ
				    TAX-AC.
	     MOVE 1		 TO WS-S7.

       BA17-VAT.
	     MOVE W90-VAT(WS-S7) TO TAX-VAT(WS-S7)
	     MOVE W90-TAXVAL(WS-S7)
				 TO TAX-VAL(WS-S7).
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO BA17-VAT.
	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY19 THRU AY59.
014920	     MOVE W70-AUDIT	 TO WS-AUDKEY.
014930	     MOVE LOW-VALUES	 TO AUD-REC1.
014940	     PERFORM AY16 THRU AY59.
014950	     MOVE 5		 TO AUD-TYPE.
014960	     MOVE WS-DRAW	 TO AUD-SUB.
014970	     MOVE W90-REF	 TO AUD-CSH.
	     COMPUTE AUD-SELL = W90-TSELL - W90-TTAX.
014990	     MOVE W90-TCOST	 TO AUD-COST.
015000	     MOVE W10-SMAN	 TO AUD-SMAN.
015010	     PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015020	     ADD 1		 TO W70-AUDIT.
015030	     MOVE 1		 TO WS-AUDKEY.
015040	     PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015050	     PERFORM AY16 THRU AY59.
015060	     MOVE 1		 TO AUD-REP5.
015070	     PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015080	     MOVE W70-AUDIT	 TO WS-AUDKEY.
015090	     MOVE LOW-VALUES	 TO AUD-REC1.
015100	     PERFORM AY16 THRU AY59.
015110	     MOVE 4		 TO AUD-TYPE.
015120	     MOVE WS-DRAW	 TO AUD-SUB.
015130	     MOVE W10-SMAN	 TO AUD-AC.
015140	     MOVE W90-REF	 TO AUD-REF.
015150	     COMPUTE AUD-VAL = (W90-TOTAL - W90-TDISC).
015160	     PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015170	     ADD 1		 TO W70-AUDIT.
015180	     MOVE 1		 TO WS-AUDKEY.
015190	     PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015200	     PERFORM AY16 THRU AY59.
015210	     MOVE 1		 TO AUD-REP4.
015220	     PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015230	   IF W10-SMAN NOT = ZERO
015240	       ADD 100 W10-SMAN  GIVING WS-PARKEY
015260	       PERFORM AY01 THRU AY59
015270	       ADD W90-TCOST	 TO PAR-CT-MTD PAR-CT-YTD
015280	       ADD W90-EXCTOT	 TO PAR-SL-MTD PAR-SL-YTD
015290	       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015330	     SUBTRACT W90-BAL FROM STK-TOTAL.
015340	     ADD W90-SALES	 TO STK-SMTD STK-SYTD.
015480	     MOVE 7		 TO WS-PARKEY.
015500	     PERFORM AY01 THRU AY59.
	     COMPUTE PAR-CASH-DAY  = PAR-CASH-DAY + (W90-CASH
				     - W90-CHANGE) + W90-CHQ
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
	     COMPUTE PAR-CASH-MTH  = PAR-CASH-MTH + (W90-CASH
				     - W90-CHANGE) + W90-CHQ
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
	     COMPUTE PAR-CASH-YEAR = PAR-CASH-YEAR + (W90-CASH
				     - W90-CHANGE) + W90-CHQ
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
	   IF WX-INT = "Y"
	       MOVE W12-T-YMD	 TO WX-DATE
	       MOVE "01PS"	 TO WX-LEDG
	       MOVE "CASH SLIPS" TO WX-NAR
	       COMPUTE WX-VALUE = (W90-TOTAL - W90-TDISC)
	       MOVE PAR-CASH-GL	 TO WX-AC
	       MOVE 26		 TO WX-TYPE
	       IF NOT (WX-VALUE = ZERO)
		   PERFORM GL-TRF-UPDATE.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    R E D U C E	 C A S H   S A L E S   (ROUNDING)
      *
	   IF NOT (W90-ROUND = ZERO)
	       IF WX-INT = "Y"
		   MOVE W12-T-YMD
				 TO WX-DATE
		   MOVE "RSLP"	 TO WX-LEDG
		   MOVE "ROUNDING"
				 TO WX-NAR
		   COMPUTE WX-VALUE = (W90-ROUND * -1)
		   MOVE PAR-CASH-GL  TO WX-AC
		   MOVE 26	 TO WX-TYPE
		   PERFORM GL-TRF-UPDATE
	       END-IF
027690	       MOVE "YYYY"	 TO DPT-CODE
	       MOVE ZERO	 TO WS-ACTION
027700	       PERFORM AY38 THRU AY59
027710	       ADD W90-RTVAL	 TO DPT-RET-DAY
027720				    DPT-RET-MTD
027730				    DPT-RET-YTD
	       IF WX-INT = "Y"
		   MOVE W90-RTVAL
				 TO WX-VALUE
		   MOVE DPT-DISC-GL
                                 TO WX-AC
		   MOVE 15	 TO WX-TYPE
		   PERFORM GL-TRF-UPDATE
	       END-IF
027740	       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT
	       IF NOT (W90-RTTAX = ZERO)
		   MOVE WS-P1	TO W05-V-RATE
		   MOVE W05-VAT-CODE
				TO DPT-CODE
		   MOVE ZERO	TO WS-ACTION
027840		   PERFORM AY38 THRU AY59.
027850		   SUBTRACT W90-RTTAX
				FROM DPT-SALES-DAY
027860				     DPT-SALES-MTD
027870				     DPT-SALES-YTD
		   IF WX-INT = "Y"
		       MOVE W90-RTTAX
				 TO WX-VALUE
		       MOVE DPT-SALE-GL
				 TO WX-AC
		       MOVE 10	 TO WX-TYPE
		       PERFORM GL-TRF-UPDATE
		   END-IF
027880		   PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
015480	     MOVE 8		 TO WS-PARKEY.
015500	     PERFORM AY01 THRU AY59.
015510	     ADD W90-BDT	 TO PAR-CSHT PAR-CMTD PAR-CYTD.
015520	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015530	     PERFORM AY70 THRU AY999.
015540	     CLOSE PARAM STOCK NETWORK DEPART.
015550	     OPEN I-O PARAM STOCK NETWORK DEPART.
	     MOVE "N"		 TO WS-PROC.
	     GO TO BA05.

       BA20.
009640	     DISPLAY CLR-SCREEN.
	     DISPLAY "UPDATE BULK STOCK" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Updating Stock Records" AT 0812
		      WITH HIGHLIGHT.
	     MOVE SPACES	 TO BLK-ITEM BLK-ITM.
	     PERFORM START-AT-BULK-KEY THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       GO TO BA40.

       BA25.
	     PERFORM READ-BULK-NEXT THRU READ-BULK-EXIT.
	   IF WS-F-ERROR = 59
	       GO TO BA40.
	     MOVE BLK-ITEM	 TO STK-CODE W10-ITEM.
	     MOVE BLK-ITM	 TO STK-ITM.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF STK-IND = ZERO OR
	      STK-IND > 1
	       PERFORM DELETE-BULK-REC THRU WRITE-BULK-EXIT
013770	       GO TO BA25.
	     MOVE W10-ITEM	 TO PRT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510	   IF WS-F-ERROR = 21
	       GO TO BA25.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
021370       MOVE 1              TO WS-S6.

021420 BA30.
021430     IF PRT-CODE (WS-S6) = SPACES
021450	       GO TO BA35.
021470       MOVE PRT-CODE (WS-S6) 
                                 TO W10-ITEM.
021490	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
021500     IF WS-F-ERROR = 22
021520	       GO TO BA35.
	     MULTIPLY W10-QUANT BY PRT-QUANT (WS-S6)
				 GIVING W10-PQUANT.
013780	     SUBTRACT W10-PQUANT FROM STK-QUANT.
013870	     ADD W10-PQUANT	 TO STK-MTD STK-YTD.
013950	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.

021660 BA35.
	     ADD 1		 TO WS-S6.
021670     IF WS-S6 < 31
021680	       GO TO BA30.
	     PERFORM DELETE-BULK-REC THRU WRITE-BULK-EXIT.
	     GO TO BA25.

       BA40.
012990	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012700	     MOVE "N"		 TO DEB-POSMODE.
013010	     PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.

       BA999.
	     EXIT.
      /
       BB000		       SECTION 50.
       BB00.
	     MOVE 19		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 49		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY
	     "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	      AT 1815 WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Item " AT 1915
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		      SLP-ITEM WITH FOREGROUND-COLOR 6 HIGHLIGHT
				    BACKGROUND-COLOR 7
		     " " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		      SLP-SDSC WITH FOREGROUND-COLOR 6 HIGHLIGHT
				    BACKGROUND-COLOR 7
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY
	     "³ has been deleted or amended! Please Key in new  ³"
	      AT 2015 WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Item " AT 2115
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "            "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " " WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		      SLP-SDSC WITH FOREGROUND-COLOR 3 HIGHLIGHT
				    BACKGROUND-COLOR 5
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY
	     "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	      AT 2215 WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

       BB05.
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
011080		     " X/Ref Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BB10.
	   IF W10-STCK = "A"
021070	       ACCEPT S05
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S05-3
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S05-4
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S05-5
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S05-6
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S05-7
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S05-8
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S05-9
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S05-10
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S05-11
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S05-12
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S05-13
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S05-14.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
		     GO TO BB10
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BB10
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
		     GO TO BB10
	       END-EVALUATE
	       PERFORM AA50-LOOKUP
               DISPLAY S05
015990	       GO TO BB10.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
015830     IF W10-ITEM = SPACE
015840	       GO TO BB10.
015850	     PERFORM CA000.
	   IF WS-F-ERROR = 22
	       GO TO BB10.
	     MOVE STK-DESC	 TO W10-DESC.
	     DISPLAY W10-SDSC AT 2135
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
	       GO TO BB10.
	     MOVE W10-ITEM	 TO SLP-ITEM.
	     MOVE STK-DESC	 TO SLP-DESC.
	     MOVE 18		 TO CLIN.
             MOVE 01             TO CCOL.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 23.

       BB999.
	     EXIT.
      /
007970 BG00-TRAN	       SECTION 50.
008600 BG80.
008610       MOVE W70-AUDIT      TO WS-AUDKEY.
008620       MOVE LOW-VALUES     TO AUD-REC1.
008630       PERFORM AY16 THRU AY59.
008640       MOVE STK-CODE       TO AUD-CODE.
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

      /
044130 ZA000-INIT	       SECTION 80.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
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
	     MOVE "BULKST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-BULK.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PSLIPS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SLIP.
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
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
036100	     MOVE "Has a backup of Data Files been made  [ ]"
				 TO WS-ERR-MES.
022850	     MOVE SPACE		 TO WS-OPTION.
      *
      *    ****    B A C K U P	 O F   D A T A	 B E E N   M A D E ?
      *
	     PERFORM OPT-MESSAGE.
011900	   IF NOT (WS-OPTION = "Y" OR "N")
011910	       GO TO ZA00A.
	   IF WS-OPTION = "N"
	       MOVE "9"		 TO WS-OPTION
	       GO TO ZA999-EXIT.
044230       MOVE 3              TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.

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
053830	     OPEN I-O CARDEX.
	   IF WS-STATUS = "05"
	       CLOSE CARDEX
	       OPEN OUTPUT CARDEX
	       CLOSE CARDEX
	       OPEN I-O CARDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 43	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 43		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
044300	     OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 40       TO WS-F-ERROR
                   GO TO ZA50.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 40           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054260		   PERFORM OPEN-ERROR.
044480	     OPEN I-O SPARTS.
044490     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
                   MOVE 21       TO WS-F-ERROR
                   GO TO ZA50.
044510     IF WS-STATUS NOT = "00"
044520         MOVE 21           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
044350		   PERFORM OPEN-ERROR.
054300	     OPEN I-O STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400             GO TO ZA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.
021020	     OPEN OUTPUT RECOVER.
021030       CLOSE RECOVER.
021040       OPEN I-O RECOVER.
	     OPEN I-O BULK.
	   IF WS-STATUS = "05"
	       CLOSE BULK
	       OPEN OUTPUT BULK
	       CLOSE BULK
	       GO TO ZA01B.
054310	   IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49.
021300     IF WS-STATUS NOT = "00"
021310	       MOVE 59		 TO WS-F-ERROR
021320         PERFORM OPEN-ERROR.
       ZA01C.
	     OPEN I-O SLIP.
	   IF WS-STATUS = "05"
	       CLOSE SLIP
	       OPEN OUTPUT SLIP
	       CLOSE SLIP
	       GO TO ZA01C.
054310	   IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49.
021300     IF WS-STATUS NOT = "00"
021310	       MOVE 38		 TO WS-F-ERROR
021320         PERFORM OPEN-ERROR.
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
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 21
041860         MOVE "PRODUCTION" TO WS-FILE
           ELSE
           IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
				 TO WS-FILE
           ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
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
044890 ZB000-ERROR	       SECTION 81.
      
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
051830	   ELSE
041610	   IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
052110	   ELSE
052120     IF WS-F-ERROR = 18
052130         MOVE W02-RECOVER  TO WS-FILE
052140         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
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
041850	   IF WS-F-ERROR = 38
041860	       MOVE W02-SLIP	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SLP-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 59
041860	       MOVE W02-BULK	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE BLK-KEY	 TO WS-KEYX.
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
042170	     STOP RUN.
