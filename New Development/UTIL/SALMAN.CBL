      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******      **     **       **     **     **     **	  **   *
      *   **	**    ****    **       ***   ***    ****    ***   **   *
      *   **	     **  **   **       ** * * **   **  **   ** *  **   *
      *    ******   **	  **  **       **  *  **  **	**  ** *  **   *
      * 	**  ********  **       **     **  ********  **	* **   *
      *   **	**  **	  **  **       **     **  **	**  **	 ***   *
      *    ******   **	  **  *******  **     **  **	**  **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S M A N   R E P O R T   -	R E   C A R D E X      *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      * May 2010 - Found an error:				       *
      * 	       The stock code being used in the program, to    *
      * 	       access the stock details was CRD-CODE which is  *
      * 	       only the first 14 characters of the actual code *
      * 	       which is 18 characters.			       *
      * 	       This could result in the incorrect information  *
      * 	       being displayed/printed. 		       *
      * 	       The SALESMAN file also used a definition of 14. *
      * 	       These errors have been rectified as follows.    *	       *
      * 		   SALESMAN file layout amended:	       *
      *			       03  SAL-CODE.			       *
      *				   05  SAL-ITEM    PIC	X(14).	       *
000060*				   05  SAL-ITM	   PIC	X(03).	       *
      *				   05  FILLER	   PIC	X(01).	       *
      * 							       *
      * 		   The code in BA10 has been amended:	       *
      *			       MOVE CRD-EXT-CODE TO SAL-CODE STK-CODE. *
      * 							       *
      * 		   The code in BA40 has been amended:	       *
      * 		       DISPLAY CRD-EXT-CODE AT 1436	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SALMAN.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2010
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
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CARDEX.SL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT SALESMAN ASSIGN DISK
000040			   STATUS WS-STATUS
000050			   ACCESS DYNAMIC
000060			   ORGANIZATION INDEXED
000070			   RECORD SAL-KEY
			   ALTERNATE RECORD SAL-SKEY =
				     SAL-DTE SAL-REF SAL-SEQ
				     DUPLICATES.
      $SET NOKEYCOMPRESS

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000120 COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

      /
000010 FD  SALESMAN  IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-SALESMAN.
000220 01  SAL-REC1.
000230	   03  SAL-KEY.
	       05  SAL-SMAN    PIC  9(02).
	       05  SAL-DTE     PIC  9(08)    COMP.
	       05  SAL-REF     PIC  X(08).
	       05  SAL-SEQ     PIC  9(04)    COMP.
	   03  SAL-AC	       PIC  X(06).
	   03  SAL-NAME.
	       05  SAL-NME     PIC  X(19).
	       05  FILLER      PIC  X(21).
	   03  SAL-CODE.
	       05  SAL-ITEM    PIC  X(14).
000060	       05  SAL-ITM     PIC  X(03).
	       05  FILLER      PIC  X(01).
	   03  SAL-LEDG        PIC  X(04).
	   03  SAL-DESC        PIC  X(30).
000250	   03  SAL-QNT	       PIC S9(07)V99 COMP-3.
	   03  SAL-COST        PIC S9(09)V99 COMP-3.
	   03  SAL-SELL        PIC S9(09)V99 COMP-3.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7	   PIC	9(04)	 COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-CRDX	   PIC	9(04)	 COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-DEPKEY	   PIC	9(06)	 COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
003390 77  WS-LINES	   PIC	9(02)	 VALUE 66.
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003420 77  WS-NORM	   PIC	X(06).
003060 77  WS-COND	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
001820 77  WS-OPTION	   PIC	X(01).
003550 77  WS-LEDG	   PIC	X(04).
001820 77  WS-RTYPE	   PIC	X(01).
001820 77  WS-WRT	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "SAL".
	   03  WS-PROG	       PIC  X(03) VALUE "MAN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

000010 COPY SALESMAN.ID.

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

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DETAIL.
		   07  R-HL	   PIC	X(96).
		   07  FILLER	   PIC	X(36).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-PGE	   PIC X(06).
	       05  R-PNO	   PIC Z(03)9.
	       05  FILLER	   PIC X(09).
	       05  R-CPY	   PIC X(40).
	       05  FILLER	   PIC X(05).
	       05  R-DATE	   PIC X(06).
	       05  R-DTE	   PIC Z9/99/9999.

	   03  SRP-L1 REDEFINES W02-PRN-LINE.
	       05  SRP-H1	   PIC	X(14).
	       05  SRP-NO	   PIC	Z9.
	       05  FILLER	   PIC	X(01).
	       05  SRP-SMAN	   PIC	X(30).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DH	   PIC	X(05).
	       05  SRP-DEP-CODE    PIC	X(04).
	       05  FILLER	   PIC	X(23).

	   03  SRP-L2 REDEFINES W02-PRN-LINE.
	       05  SRP-H2	   PIC	X(51).
	       05  SRP-H3	   PIC	X(29).

	   03  SRP-L3 REDEFINES W02-PRN-LINE.
	       05  SRP-ACT	   PIC	X(06).
	       05  FILLER	   PIC	X(01).
	       05  SRP-NAME	   PIC	X(39).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DATE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  SRP-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(30).

	   03  SRP-L4 REDEFINES W02-PRN-LINE.
	       05  SRP-H4	   PIC	X(15).
	       05  SRP-H5	   PIC	X(33).
	       05  SRP-H6	   PIC	X(11).
	       05  SRP-H7	   PIC	X(13).
	       05  SRP-H8	   PIC	X(15).
	       05  SRP-H9	   PIC	X(10).

	   03  SRP-L5 REDEFINES W02-PRN-LINE.
	       05  SRP-ITEM	   PIC	X(14).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DESC	   PIC	X(30).
	       05  FILLER	   PIC	X(01).
	       05  SRP-QUANT	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-COST	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-SELL	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-EXTEND	   PIC	Z(07)9.99-.

	   03  SRT-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(05).
	       05  SRT-H4	   PIC	X(10).
	       05  SRT-H5	   PIC	X(07).
	       05  SRT-H6	   PIC	X(15).
	       05  SRT-H7	   PIC	X(19).
	       05  SRT-H8	   PIC	X(11).
	       05  SRT-H9	   PIC	X(13).

	   03  SRT-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(05).
	       05  SRT-MONTH	   PIC	X(09).
	       05  FILLER	   PIC	X(02).
	       05  SRT-YEAR	   PIC	X(04).
	       05  FILLER	   PIC	X(02).
	       05  SRT-COST	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(02).
	       05  SRT-SELL	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(02).
	       05  SRT-EXTEND	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(02).
	       05  SRT-GP	   PIC	Z(02)9.99-.
	       05  FILLER	   PIC	X(06).

       01  W05-CARDEX.
	   03  W05-CRDX-ID.
	       05  W05-CID     PIC  X(01) OCCURS 12.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       COPY W15.WS.

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
	       07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
	       05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
	       05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
	       05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
	       05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
	       05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
	       05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
	       05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
	       05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
	       05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
	       05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
	       05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
	       05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
	       05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
	       05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
	       05  W15-REC15   PIC  X(08).

       COPY W20.WS.

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

002850 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.
004380	   03  W25-COST1       PIC S9(09)V99  COMP-3.
004390	   03  W25-COST2       PIC S9(09)V99  COMP-3.
004400	   03  W25-COST3       PIC S9(09)V99  COMP-3.
004410	   03  W25-SELL1       PIC S9(09)V99  COMP-3.
004420	   03  W25-SELL2       PIC S9(09)V99  COMP-3.
004430	   03  W25-SELL3       PIC S9(09)V99  COMP-3.

       01  W30-DISPLAY.
	   03  W30-SP	       PIC  ZZ9	       BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(02).
	   03  W30-CODE        PIC  X(14).
	   03  FILLER	       PIC  X(01).
	   03  W30-DESC        PIC  X(30).
	   03  FILLER	       PIC  X(01).
	   03  W30-QUANT       PIC  Z(06)9.99- BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01).
	   03  W30-VALUE       PIC  Z(07)9.99- BLANK WHEN ZERO.

       01  W30-WORK.
	   03  W30-SMAN        PIC  9(02).
	   03  W30-ACT	       PIC  X(06).
	   03  W30-REF	       PIC  X(08).

       01  W35-SALESMAN.
	   03  W35-START       PIC  9(02) COMP-0.
	   03  W35-END	       PIC  9(02) COMP-0.
	   03  W35-SMAN1       PIC  X(30).
	   03  W35-SMAN2       PIC  X(30).
	   03  W35-SMNO        PIC  9(02).

002920 01  W40-SALESMAN.
	   03  W40-SM-TOTAL    PIC S9(09)V99 COMP-3 OCCURS 51.

004440 01  W40-INDIC.
004490	   03  W40-EDIT.
004500         05  W40-KEY     PIC ZZZZ9.
004510         05  W40-PERC    PIC ZZ9.99-.
004520     03  W40-EDIT2 REDEFINES W40-EDIT.
004530         05  FILLER      PIC X(01).
004540         05  W40-AMOUNT  PIC Z(06)9.99-.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860	   03  W75-DNO	      PIC  X(06) OCCURS 18.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.
      *	   03  W75-ITEMS OCCURS 18.
      *	       05  W75-CODE   PIC  X(18).
      *	       05  W75-SELL   PIC  9(07)V99 COMP-3.
      *	       05  W75-VSELL  PIC  9(07)V99 COMP-3.
      *	       05  W75-CASH   PIC  9(07)V99 COMP-3.
      *	       05  W75-VCASH  PIC  9(07)V99 COMP-3.
      *	       05  W75-WSALE  PIC  9(07)V99 COMP-3.
      *	       05  W75-VWSALE PIC  9(07)V99 COMP-3.
	   03  W75-RNO	      PIC  X(08) OCCURS 16.
	   03  W75-DATE       PIC  9(06) OCCURS 16.
	   03  W75-SM	      PIC  9(02) OCCURS 16.

003750 01  W75-STORE.
003760	   03  W75-SNAME      PIC  X(08).
	   03  W75-SREF       PIC  X(08).
	   03  W75-SACN       PIC  X(06).
	   03  W75-SDTE       PIC  9(06).
	   03  W75-SSM	      PIC  9(02).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500	   03  W100-COST       PIC Z(06)9.99.
004500	   03  W100-SELL       PIC Z(06)9.99.
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
	       05  FILLER      PIC  X(02).
	       05  W100-QNT    PIC Z(04)9.99.
	   03  W100-START      PIC Z9/99/9999.
	   03  W100-END        PIC Z9/99/9999.

005390 01  W200-TRAN.
005400     03  W200-ACCUM.
005410         05  W200-DT     PIC S9(07)V99 COMP-3 OCCURS 300.
005420         05  W200-CR     PIC S9(07)V99 COMP-3 OCCURS 300.
005430         05  W200-DTOT   PIC S9(07)V99 COMP-3.
005440         05  W200-CTOT   PIC S9(07)V99 COMP-3.
005450         05  W200-COST   PIC S9(07)V99 COMP-3.
005460         05  W200-SELL   PIC S9(07)V99 COMP-3.
005470         05  W200-NET    PIC S9(07)V99 COMP-3.
005470	       05  W200-GP     PIC S9(07)V99 COMP-3.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

000850 01  S-PROG.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "TRANSACTIONS PER SALESPERSON".

       01  S03.
	   03  LINE  4 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³  ".
	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		       "S A L E S P E R S O N   S E L E C T I O N".
	   03	       COLUMN 61 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				 "   ³".
	   03  LINE  6 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³  ".
	   03	       COLUMN 20 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		       "Keyin the Salesperson number range required".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				" ³" .
	   03  LINE  7 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "³Start with:                                   ³".
	   03  LINE  8 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "³End with  :                                   ³".
	   03  LINE  9 COLUMN 17 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
	   02  S04A.
	   03  LINE  7 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9(02) USING W35-START AUTO.
	   02  S04B.
	   03  LINE  7 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W35-SMAN1.
	   02  S04C.
	   03  LINE  8 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  9(02) USING W35-END AUTO.
	   02  S04D.
	   03  LINE  8 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM W35-SMAN2.

009340 01  S05D.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "SALES PERFORMANCE REPORTING".
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Transactions for Period.   ³".
	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Starting Date :            ³".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Ending Date   :            ³".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³Dates to be keyed in as ".
	   03	       COLUMN 50 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMYY".
	   03	       COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 11 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S05A.
	   03  LINE  7 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S06.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
		      "Sales Staff transactions for the period:- ".
	   03	       COLUMN 44 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-START.
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE " to ".
	   03	       COLUMN 58 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-END.
	   03  LINE  5 COLUMN  3 FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
	       VALUE "S/P. Item Code      Stock Description
      - 	     "   Quantity        Value ".

003990 01  S24.
	   03  LINE 14 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -				       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE 15 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "³                      ".
	   03	       COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "SALES LEDGER".
	   03	       COLUMN 45 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "                       ³".
	   03  LINE 16 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 17 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2
				 VALUE "Ledger No      Description  ".
	   03	       COLUMN 39 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "                             ³".
	   03  LINE 18 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
      -				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

005040 01  S25.
005050     03  S25A.
005060	       05  LINE 17 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 2
003470				     PIC X(04) USING W10-LEDG AUTO.
005080     03  S25B.
005090	       05  LINE 17 COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 2
003470				     PIC X(30) USING DPT-DESC AUTO.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

       COPY DEPART.CRT.

       COPY SALESMAN.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE SALESMAN.

       AA49.
013150       EXIT PROGRAM.

       COPY FUNCTION.CRT.

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

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY SALESMAN.LUP.

       COPY APAC.HLP.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

011670 COPY CONTROL.RD.

       COPY DEPART.RD.

011750 COPY PARAM.RD.

       COPY STOCK.RD.

000010 READ-SALESMAN.
000020	     READ SALESMAN	 WITH IGNORE LOCK
000030	       KEY IS SAL-KEY.
000040	   IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALESMAN-EXIT.
000070	   IF WS-STATUS = "23"
000080	       MOVE 44		 TO WS-F-ERROR
000090	       GO TO READ-SALESMAN-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110	       MOVE 44		 TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000160	     GO TO READ-SALESMAN-EXIT.

000380 READ-SALESMAN-NEXT.
000390	     READ SALESMAN NEXT	 WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALESMAN-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-SALESMAN-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-SALESMAN-EXIT.

000380 READ-SALESMAN-PREV.
000390	     READ SALESMAN PREVIOUS WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALESMAN-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-SALESMAN-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-SALESMAN-EXIT.

000750 START-AT-SALESMAN-KEY.
000760	     START SALESMAN
000770	       KEY >= SAL-KEY.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800	       GO TO READ-SALESMAN-EXIT.
000810     IF WS-STATUS = "23"
000820	       MOVE 44		 TO WS-F-ERROR
000830	       GO TO READ-SALESMAN-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850	       MOVE 44		 TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870	     GO TO READ-SALESMAN-EXIT.

000890 START-AT-SALESMAN-DATE.
000900	     START SALESMAN
000910	       KEY >= SAL-SKEY.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940	       GO TO READ-SALESMAN-EXIT.
000950     IF WS-STATUS = "23"
000960	       MOVE 44		 TO WS-F-ERROR
000970	       GO TO READ-SALESMAN-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990	       MOVE 44		 TO WS-F-ERROR
001000         PERFORM READ-ERROR.

001580 READ-SALESMAN-EXIT.
001590       EXIT.

      /
011350 AD000-WRITE             SECTION.

010000 REWRITE-SALESMAN.
010010	     REWRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010030	       MOVE 44		 TO WS-F-ERROR
010040         PERFORM WRITE-ERROR.
010050	     GO TO WRITE-SALESMAN-EXIT.

010060 WRITE-SALESMAN.
010070	     WRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010090	       IF WS-STATUS = "22"
		   ADD 1	 TO SAL-SEQ
010100		   GO TO WRITE-SALESMAN
010110         ELSE
010120		   MOVE 44	 TO WS-F-ERROR
010130             PERFORM WRITE-ERROR.

010140 WRITE-SALESMAN-EXIT.
010150       EXIT.

       COPY DEPART.LUP.

      /
      *  ****  E N Q U I R Y   ( S A L E S   P E R S O N )
      *
021000 BA000		       SECTION.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       BA05.
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
		     GO TO BA999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA05.
	     PERFORM CLEAR-L50.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
	     MOVE "Print 'D'etail or 'T'otals (D/T) [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "D" OR "T".
	     MOVE WS-OPTION	 TO WS-RTYPE.
	     MOVE W15-SMM	 TO WS-CRDX.
	     MOVE W15-START	 TO W22-DTE1.
             MOVE W15-END        TO W22-DTE2.
	     MOVE 13		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 28		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1226
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³Extracting details requested³" AT 1326
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³Reading:                    ³" AT 1426
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1526
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SCREEN-SHADOW.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-CODE.
	     PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       MOVE "No data for time period selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA15.
	     MOVE SPACES	 TO SAL-CODE.
	   IF WS-RTYPE = "T"
	       GO TO BA40.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Extract the SalesPersons details from the CARDEX file and ³
      *    ³ write to SALESMAN file.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA10.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA15.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA15.
	     DISPLAY CRD-CODE AT 1436
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE CRD-AC	 TO SAL-AC.
      *
      *    **** Move Item Code to STK-CODE (STOCK FILE) which will be
      * 	used as the key to read the stock description.
      *
	     MOVE CRD-EXT-CODE	 TO SAL-CODE STK-CODE.
	     MOVE CRD-DTE	 TO SAL-DTE.
	     MOVE ZERO		 TO SAL-SEQ.
	     MOVE CRD-REF	 TO SAL-REF.
	     MOVE CRD-SMAN	 TO SAL-SMAN.
	     MOVE CRD-QNT	 TO SAL-QNT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ There are occassions when the Quantity is negative that   ³
      *    ³ either the cost or selling values are positive on the     ³
      *    ³ CARDEX file record and the following has been inserted to ³
      *    ³ correct the error before doing any calculations.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF CRD-QNT < ZERO
	       MOVE "N" 	 TO WS-WRT
	       IF NOT (CRD-SELL < ZERO)
		   MULTIPLY -1	 BY CRD-SELL
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF NOT (CRD-COST < ZERO)
		   MULTIPLY -1	 BY CRD-COST
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF WS-WRT = "Y"
		   REWRITE CRD-REC1.
	   IF CRD-QNT < ZERO
	       COMPUTE SAL-COST = (CRD-QNT * CRD-COST) * -1
	       COMPUTE SAL-SELL = (CRD-QNT * CRD-SELL) * -1
	   ELSE
	       COMPUTE SAL-COST = (CRD-QNT * CRD-COST)
	       COMPUTE SAL-SELL = (CRD-QNT * CRD-SELL).
	     MOVE CRD-NAME	 TO SAL-NAME.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "No Stock Details for this Code"
				 TO STK-DESC
	       MOVE "ZZZZ"	 TO STK-LDG.
	     MOVE STK-DESC	 TO SAL-DESC.
	     MOVE STK-LDG	 TO SAL-LEDG.
	     PERFORM WRITE-SALESMAN THRU WRITE-SALESMAN-EXIT.
	     MOVE SAL-SMAN	 TO WS-S1.
	   IF SAL-SMAN = ZERO
	       MOVE 51		 TO WS-S1.
	     ADD SAL-SELL	 TO W40-SM-TOTAL (WS-S1).
	     GO TO BA10.

       BA15.
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
	     MOVE "'First'"	 TO W35-SMAN1.
	     MOVE "'Last'"	 TO W35-SMAN2.
	     DISPLAY S04.

       BA25.
	     DISPLAY "F2" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=View Salesperson Codes, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S04A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO BA32
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM SALESMAN-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO BA25
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA25
	       END-EVALUATE.
	   IF W35-START > 50
	       MOVE "Invalid Salesperson code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA25.
	   IF W35-START = ZERO
	       MOVE "Sales not allocated"
				 TO W35-SMAN1
	   ELSE
	       ADD 100 W35-START GIVING WS-PARKEY
010860	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       IF PAR-SALESMAN = SPACES
		   IF W35-START > 1
		       MOVE "Invalid Salesperson start code"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       GO TO BA25
		   END-IF
	       ELSE
		   MOVE PAR-SALESMAN TO W35-SMAN1.
	     DISPLAY S04B.

       BA30.
	     ACCEPT S04C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA25
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM SALESMAN-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO BA30
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA30
	       END-EVALUATE.
	   IF W35-END > 50
	       MOVE "Invalid Salesperson end code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA30.
	   IF W35-END < W35-START
	       MOVE "End code before Start code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA30.
	   IF W35-END = ZERO
	       MOVE "Sales not allocated"
				 TO W35-SMAN2
	   ELSE
	       ADD 100 W35-END	 GIVING WS-PARKEY
010860	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       IF PAR-SALESMAN = SPACES
		   IF W35-END < 50
		       MOVE "Invalid Salesperson code"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       GO TO BA30
		   END-IF
	       ELSE
		   MOVE PAR-SALESMAN TO W35-SMAN2.
	     DISPLAY S04D.

       BA32.
	     EXIT.

       BA35.
	   IF WS-ERROR = 9
	       MOVE ZERO	 TO WS-ERROR
	       GO TO BA999.
	     PERFORM BA100 THRU BA199.
050150	     MOVE "'P'rinter or 'S'creen"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 14		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT TEST AFTER
		     UNTIL WS-OPTION = "P" OR "S".
	   IF WS-OPTION = "P"
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	       MOVE 8		 TO W02-PRN-TYPE
	       PERFORM OPEN-PRINTER
	       MOVE "P"		 TO WS-COMMAND
	       PERFORM BB000
      *
      *    ****   6   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	   ELSE
	       PERFORM BC000.
007060	     GO TO BA15.

       BA40.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA45.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA45.
	     DISPLAY CRD-EXT-CODE AT 1436
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ There are occassions when the Quantity is negative while  ³
      *    ³ either the Cost or Selling values are positive on the     ³
      *    ³ CARDEX file record and the following has been inserted to ³
      *    ³ correct the error before doing any calculations.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF CRD-QNT < ZERO
	       MOVE "N" 	 TO WS-WRT
	       IF NOT (CRD-SELL < ZERO)
		   MULTIPLY -1	 BY CRD-SELL
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF NOT (CRD-COST < ZERO)
		   MULTIPLY -1	 BY CRD-COST
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF WS-WRT = "Y"
		   REWRITE CRD-REC1.
	     MOVE CRD-SMAN	 TO SAL-SMAN.
	     MOVE CRD-DTE	 TO W21-DATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As totals are accumulated per month - move zero to day.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO W21-DD.
	     MOVE W21-DATE	 TO SAL-DTE.
	     PERFORM READ-SALESMAN THRU READ-SALESMAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       If no record - create a new record.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-F-ERROR = 44
	       INITIALIZE SAL-REC1
	       MOVE CRD-SMAN	 TO SAL-SMAN
	       MOVE W21-DATE	 TO SAL-DTE
	       MOVE SPACES	 TO SAL-REF
	       MOVE ZERO	 TO SAL-SEQ
	       PERFORM WRITE-SALESMAN THRU WRITE-SALESMAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Accumulate the totals for the sales person for the month  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF CRD-QNT < ZERO
	       COMPUTE SAL-COST = SAL-COST + ((CRD-QNT * CRD-COST) * -1)
	       COMPUTE SAL-SELL = SAL-SELL + ((CRD-QNT * CRD-SELL) * -1)
	   ELSE
	       COMPUTE SAL-COST = SAL-COST + (CRD-QNT * CRD-COST)
	       COMPUTE SAL-SELL = SAL-SELL + (CRD-QNT * CRD-SELL).
	     PERFORM REWRITE-SALESMAN THRU WRITE-SALESMAN-EXIT.
	     GO TO BA40.

       BA45.
	     PERFORM BA15 THRU BA32.
	   IF WS-ERROR = 9
	       MOVE ZERO	 TO WS-ERROR
	       GO TO BA999.

       BA50.
050150	     MOVE "'P'rinter or 'S'creen"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 14		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT TEST AFTER
		     UNTIL WS-OPTION = "P" OR "S".
	   IF WS-OPTION = "P"
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	       MOVE 8		 TO W02-PRN-TYPE
	       PERFORM OPEN-PRINTER
	       MOVE "P"		 TO WS-COMMAND
	       PERFORM BD000
      *
      *    ****   6   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	   ELSE
	       PERFORM BE000.
007060	     GO TO BA15.

       BA100.
050150	     MOVE "Selective per Department 'Y/N'  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 14		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N".
	     MOVE SPACES	 TO W10-LEDG
				    WS-LEDG.
	   IF WS-OPTION = "N"
	       GO TO BA199.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 15		 TO SHADE-ROW.
	     MOVE 12		 TO SHADE-COL.
	     MOVE 57		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY S24.
	     PERFORM SCREEN-SHADOW.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
011020		     "=Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA105.
	     ACCEPT S25A.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-LEDG
		     GO TO BA199
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER
                     CALL X"E5"
               END-EVALUATE
015990	       GO TO BA105.
019250	     PERFORM CLEAR-L50.
009170	   IF W10-LEDG = SPACES
009180	       GO TO BA199.
009240	     MOVE W10-LEDG	 TO DPT-CODE.
009250	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid ledger code"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO BA105.
	     DISPLAY S25.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
	       GO TO BA105.
	     MOVE W10-LEDG	 TO WS-LEDG.
	     PERFORM RESTORE-SCREEN-3.

       BA199.
	     EXIT.

021720 BA999.
021730       EXIT.

       BB000	       SECTION 50.
       BB00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE W15-START	 TO W100-START.
	     MOVE W15-END	 TO W100-END.
	     DISPLAY "Sales Staff transactions for the period:- "
		      AT 0404 WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-START WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " to " WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-END WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 29		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1025
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³ Reading Transaction Details ³" AT 1125
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³                             ³" AT 1225
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1325
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SCREEN-SHADOW.
012510	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
012520				    W25-SELL1 W25-SELL2 W25-SELL3.
012530	     MOVE ZERO		 TO WS-PAGE.
	     INITIALIZE W30-WORK.
	     INITIALIZE SAL-KEY.
	     MOVE W35-START	 TO SAL-SMAN.
	     PERFORM START-AT-SALESMAN-KEY THRU READ-SALESMAN-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO BB05.
	     PERFORM READ-SALESMAN-NEXT THRU READ-SALESMAN-EXIT.
	   IF NOT (SAL-SMAN > W35-END)
	       PERFORM BB40
	       PERFORM BB30
	       GO TO BB20.

       BB05.
	     MOVE "No details for Sales Staff selected"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BB999.

       BB10.
	   IF W30-SMAN = ZERO
	       MOVE "Sales not allocated"
				 TO PAR-SALESMAN
	   ELSE
	       ADD 100 W30-SMAN	 GIVING WS-PARKEY
010860	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       IF PAR-SALESMAN = SPACES
		   MOVE "No name specified for this code"
				 TO PAR-SALESMAN.

       BB15.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
007080	     MOVE SPACES	 TO R-DETAIL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007090	     MOVE "PAGE:"	 TO R-PGE.
007100       MOVE "DATE:"        TO R-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO R-PNO.
007130       MOVE W12-TODAY      TO R-DTE.
007140       MOVE W95-COMP       TO R-CPY.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
012840	     MOVE "S A L E S M A N   R E P O R T"
				 TO R-CPY.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
012870	     MOVE "SALES PERSON:"
				 TO SRP-H1.
012880	     MOVE SAL-SMAN	 TO SRP-NO.
012890	     MOVE PAR-SALESMAN	 TO SRP-SMAN.
	   IF NOT(WS-LEDG = SPACES)
	       MOVE "Dept"	 TO SRP-DH
	       MOVE WS-LEDG	 TO SRP-DEP-CODE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 2	C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Account Name" TO SRP-H2.
	     MOVE "Date   Reference"
				 TO SRP-H3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 2	C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "Item Code"	 TO SRP-H4.
012920	     MOVE "Description"  TO SRP-H5.
	     MOVE "Quantity"	 TO SRP-H6.
	     MOVE "Cost Price"	 TO SRP-H7.
	     MOVE "Sell Price"	 TO SRP-H8.
	     MOVE "Extended"	 TO SRP-H9.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

012580 BB20.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
	   IF NOT(WS-LEDG = SPACES)
	       IF NOT(SAL-LEDG = WS-LEDG)
		   GO TO BB22.
	     MOVE SAL-DTE	 TO W95-DTE.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY SAL-SMAN AT 1227
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY W95-DTE AT 1230
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY SAL-REF AT 1239
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY SAL-AC AT 1248
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF NOT (W30-SMAN = SAL-SMAN)
	       PERFORM BB25
	       PERFORM BB35 THRU BB40
	       PERFORM BB30.
	   IF NOT (W30-REF = SAL-REF)
	       PERFORM BB25 THRU BB30.
002830	     MOVE SAL-ITEM	 TO SRP-ITEM.
002850	     MOVE SAL-DESC	 TO SRP-DESC.
002870	     MOVE SAL-QNT	 TO SRP-QUANT.
	   IF SAL-QNT < ZERO
	       COMPUTE SRP-COST = ((SAL-COST / SAL-QNT) * -1)
	       COMPUTE SRP-SELL = ((SAL-SELL / SAL-QNT) * -1)
	   ELSE
	       COMPUTE SRP-COST = SAL-COST / SAL-QNT
	       COMPUTE SRP-SELL = SAL-SELL / SAL-QNT.
002890	     MOVE SAL-SELL	 TO SRP-EXTEND.
	     ADD SAL-SELL	 TO W25-SELL1.
	     ADD SAL-COST	 TO W25-COST1.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB15.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
012990	     MOVE SPACES	 TO R-DETAIL.

       BB22.
	     PERFORM READ-SALESMAN-NEXT THRU READ-SALESMAN-EXIT.
	   IF NOT (WS-F-ERROR = 44)
	       IF NOT (SAL-SMAN > W35-END)
		   GO TO BB20.
	     PERFORM BB25.
	     GO TO BB35.

       BB25.
	     MOVE "Document Total"
				 TO SRP-DESC.
	     MOVE W30-REF	 TO SRP-H7.
	     MOVE W25-SELL1	 TO SRP-EXTEND.
	     ADD W25-SELL1	 TO W25-SELL2.
	     ADD W25-COST1	 TO W25-COST2.
	     MOVE ZERO		 TO W25-SELL1 W25-COST1.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB15.
012990	     MOVE SPACES	 TO R-DETAIL.

       BB30.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE SAL-AC	 TO SRP-ACT.
	     MOVE SAL-NAME	 TO SRP-NAME.
	     MOVE SAL-DTE	 TO W22-DTE2.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO SRP-DATE.
	     MOVE SAL-REF	 TO W30-REF SRP-REF.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB15.
      *
      *    ****   1 2	C P I
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.

       BB35.
	     MOVE "S.Person Total"
				 TO SRP-ITEM.
	     MOVE PAR-SALESMAN	 TO SRP-DESC.
	     MOVE W25-SELL2	 TO SRP-EXTEND.
	     ADD W25-SELL2	 TO W25-SELL3.
	     ADD W25-COST2	 TO W25-COST3.
	     MOVE ZERO		 TO W25-SELL2 W25-COST2.
	   IF NOT (WS-F-ERROR = 44)
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	   ELSE
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB15.

       BB40.
	     MOVE SAL-SMAN	 TO W30-SMAN.
	   IF NOT (WS-F-ERROR = 44)
009800	       PERFORM BB10 THRU BB15.
012990	     MOVE SPACES	 TO R-DETAIL.

      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
       BB45.
012640	     MOVE "TOTAL - ALL SALESMEN"
				 TO SRP-DESC.
012660	     MOVE W25-SELL3	 TO SRP-EXTEND.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
012730	     MOVE SPACES	 TO R-DETAIL.

       BB999.
	     EXIT.

       BC000	       SECTION 51.
       BC00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S06.
012510	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
012520				    W25-SELL1 W25-SELL2 W25-SELL3.
012530	     MOVE ZERO		 TO WS-PAGE.
	     INITIALIZE W30-WORK.
	     INITIALIZE SAL-KEY.
	     MOVE W35-START	 TO SAL-SMAN.
	     PERFORM START-AT-SALESMAN-KEY THRU READ-SALESMAN-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO BC05.
	     PERFORM READ-SALESMAN-NEXT THRU READ-SALESMAN-EXIT.
	     MOVE "J"		 TO WS-OPTION.
	   IF NOT (SAL-SMAN > W35-END)
	       PERFORM BC15
	       PERFORM BC50 THRU BC59
	       GO TO BC20.

       BC05.
	     MOVE "No details for Sales Staff selected"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE
	     GO TO BC999.

       BC10.
	   IF W30-SMAN = ZERO
	       MOVE "Sales not allocated"
				 TO PAR-SALESMAN
	   ELSE
	       ADD 100 W30-SMAN	 GIVING WS-PARKEY
010860	       PERFORM READ-PARAM THRU READ-PARAM-EXIT
	       IF PAR-SALESMAN = SPACES
		   MOVE "No name specified for this code"
				 TO PAR-SALESMAN.

       BC15.
	     MOVE 0603		 TO CPOS.

       BC20.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO BC25.
	     PERFORM READ-SALESMAN-NEXT THRU READ-SALESMAN-EXIT.
	   IF (WS-F-ERROR = 44) OR
	      (SAL-SMAN > W35-END)
	       PERFORM BC45 THRU BC49
	       DISPLAY "End - Press ANY key" AT 2512
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO BC999.
	   IF NOT (W30-SMAN = SAL-SMAN)
	       PERFORM BC45 THRU BC49
	       PERFORM BC50 THRU BC59.

       BC25.
	   IF NOT(WS-LEDG = SPACES)
	       IF NOT(SAL-LEDG = WS-LEDG)
		   GO TO BC20.
	     MOVE SAL-SMAN	 TO W30-SP.
	     MOVE SAL-ITEM	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-SELL	 TO W25-VALUE.
      *	   IF SAL-QNT < ZERO
      *	       COMPUTE W25-VALUE = (SAL-QNT * SAL-SELL) * -1
      *	   ELSE
      *	       COMPUTE W25-VALUE = (SAL-QNT * SAL-SELL).
	     MOVE W25-VALUE	 TO W30-VALUE.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     ADD W25-VALUE	 TO W25-SELL1.
	     MOVE SAL-COST	 TO W25-VALUE.
      *	   IF SAL-QNT < ZERO
002890*	       COMPUTE W25-VALUE = (SAL-QNT * SAL-COST) * -1
      *	   ELSE
      *	       COMPUTE W25-VALUE = (SAL-QNT * SAL-COST).
	     ADD W25-VALUE	 TO W25-COST1.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC20.
001020	     DISPLAY "PgDn" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape." WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BC30.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC999
		 WHEN PAGE-DOWN
001340		     GO TO BC35
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC30.
	     CALL X"E5".
001280	     GO TO BC30.

       BC35.
001480	   IF CLIN < 23
001490	       GO TO BC30.
001500	     MOVE 0602		 TO CPOS.

       BC40.
	     DISPLAY WS-BLNK78 AT CPOS
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC40.

       BC42.
001560	     GO TO BC15.

       BC45.
	     ADD W25-SELL1	 TO W25-SELL2.
	     ADD W25-COST1	 TO W25-COST2.
	     MOVE ZERO		 TO W25-SELL1 W25-COST1.
	     MOVE "S.Person Total"
				 TO W30-CODE.
	     MOVE PAR-SALESMAN	 TO W30-DESC.
	     MOVE ZERO		 TO W30-QUANT.
	     MOVE W25-SELL2	 TO W30-VALUE.
	     ADD W25-SELL2	 TO W25-SELL3.
	     ADD W25-COST2	 TO W25-COST3.
	     MOVE ZERO		 TO W25-SELL2 W25-COST2.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC49.
001020	     DISPLAY "Press " AT 2502
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "ANY" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     " key to continue"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
001500	     MOVE 0602		 TO CPOS.
	     PERFORM BC40.
	     MOVE 0603		 TO CPOS.

       BC49.
	     EXIT.

       BC50.
	     MOVE SAL-SMAN	 TO W30-SMAN.
	   IF NOT (WS-F-ERROR = 44)
009800	       PERFORM BC10.
	     MOVE W30-SMAN	 TO W30-SP.
	     MOVE SPACES	 TO W30-CODE.
	     MOVE PAR-SALESMAN	 TO W30-DESC.
	     MOVE ZERO		 TO W30-QUANT W30-VALUE.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC59.
001020	     DISPLAY "Press " AT 2502
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "ANY" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     " key to continue"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
001500	     MOVE 0602		 TO CPOS.
	     PERFORM BC40.
	     MOVE 0603		 TO CPOS.

       BC59.
	     EXIT.

       BC999.
	     EXIT.

       BD000	       SECTION 52.
       BD00.

       BE000	       SECTION 53.
       BE00.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 90.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
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
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SALESMAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALESMAN.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.

042170 ZA02.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *	     MOVE 1		 TO W05-V-RATE.
      *
      * ZA05-VAT.
      *	     MOVE W05-VAT-CODE	 TO DPT-CODE.
      *	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      *	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
      *	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
      *	     ADD 6 W05-V-RATE	 GIVING WS-S1.
      *	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
      *	   IF W05-V-RATE < 6
      *	       ADD 1		 TO W05-V-RATE
      *	       GO TO ZA05-VAT.
053830	     OPEN OUTPUT SALESMAN.
053840	   IF RUNTIME-ERROR
053880         IF FLE-LIMIT
053890		   GO TO ZA49.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 44		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
	     CLOSE SALESMAN.
	     OPEN I-O SALESMAN.
	     MOVE 1		 TO WS-S1.

       ZA05.
	     MOVE ZERO		 TO W40-SM-TOTAL (WS-S1).
	   IF WS-S1 < 51
	       ADD 1		 TO WS-S1
	       GO TO ZA05.
044780	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

039850 ZA50.
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990		      WS-FILE WITH FOREGROUND-COLOR 6 HIGHLIGHT.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
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
021970	     MOVE WS-G4 	 TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042800 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 91.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041840	   ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-SALESMAN TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.

       COPY DISPERR.PRO.
