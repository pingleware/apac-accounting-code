      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   **	       ******	   ****       **       ****    *
      *   **	**  **	      **    **	  **  **     ***      **  **   *
      *   **	    **	      **	 **    **     **     **    **  *
      *    ******   **	       ******	 **    **     **     **    **  *
      * 	**  **		    **	 **    **     **     **    **  *
      *   **	**  **	      **    **	  **  **      **      **  **   *
      *    ******   ********   ******	   ****    ********    ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   O U T S T A N D I N G   O R D E R S	R E P O R T   P R O G  *
      *                                                                *
      *     VERSION 8.15.01 - July 2011 			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLS010.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

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
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000270 COPY PARAM.SL.

000250 COPY SORDER.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000560 COPY PARAM.FDE.

000540 COPY SORDER.FDE.

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
002350 77  WS-NETKEY       PIC  9(04)    COMP-5.
002400 77  WS-PRN1         PIC  9(01) VALUE 0.  
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002420 77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
002430 77  WS-START        PIC  9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2         PIC  9(08).
002480 77  WS-SEQ	   PIC	X(01) VALUE SPACE.
002500 77  WS-PAR	   PIC	9(01) VALUE 0.
002510 77  WS-INO	   PIC	9(03) VALUE 0.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002520 77  WS-LINES	   PIC	9(02) VALUE 0.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002590 77  WS-OPTION       PIC  X(01).
002600 77  WS-VAT-GST      PIC  X(01).
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-MULTI        PIC  X(01) VALUE "N".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP	   PIC	9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002880 77  WS-PER	   PIC	Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

      /
002910 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ฦอออัออออออออออออออัออออออ
      -        "อออออออออออออออออออออออัออออออออัอออออออออัออออออออออต".
003080     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -        "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "ณ   ณ              ณ
      -        "                       ณ        ณ         ณ          ณ".
	   03  WS-MINV REDEFINES WS-MID-LINE.
	       05  WS-INV1     PIC  X(01).
	       05  WS-INV78    PIC  X(78).
	       05  WS-INV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "ณ".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ฦอออฯออออออออออออออฯออออออ
      -        "อออออออออออออออออออออออฯออออออออฯอออออออออฯออออออออออต".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -        "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "อ".
003220	   03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "ฦออออออออออออออออัออออออออ
      -        "ออออออออออออออออออออออัออออออออออัอออออออออัอออออออออต".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "ณ                ณ
      -        "                      ณ          ณ         ณ         ณ".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "ฦออออออออออออออออฯออออออออ
      -        "ออออออออออออออออออออออฯออออออออออฯอออออออออฯอออออออออต".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "ฦออออออัออออออออออออออออออ
      -        "ออออออออออออออออออออออัอออออออออออัออออออออออัอออออออต".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "ณ      ณ
      -        "                      ณ           ณ          ณ       ณ".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "ฦออออออฯออออออออออออออออออ
      -        "ออออออออออออออออออออออฯอออออออออออฯออออออออออฯอออออออต".
001690	   03  WS-TP-LINE.
001700         05  WS-TCA  PIC X(01) OCCURS 80.
001710     03  WS-MD-LINE.
001720         05  WS-MCA  PIC X(01) OCCURS 80.
	   03  WS-SO78-L REDEFINES WS-MD-LINE.
	       05  WS-MSO1     PIC  X(01).
	       05  WS-MSO78    PIC  X(78).
	       05  WS-MSO80    PIC  X(01).
001730	   03  WS-BT-LINE.
001740         05  WS-BCA  PIC X(01) OCCURS 80.
003250 01  WS-ENTRY.
003260     03  WS-ENT0     PIC  X(08).
003270     03  WS-TYPE0    PIC  X(01).
003280     03  WS-KEY0     PIC  9(06)    COMP-5.
003290 01  WS-ENTRY1.
003300     03  WS-ENT1     PIC  X(08).
003310     03  WS-TYPE1    PIC  X(01).
003320     03  WS-KEY1     PIC  9(06)    COMP-5.

003340 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

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

003530 01  W02-FID.

       COPY APACFIDS.ID.

003570 COPY CONTROL.ID.

       COPY PARAM.ID.

003670 COPY SORDER.ID.

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

000350	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000360	       05  REP-DETAIL1 PIC  X(136).

000370	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000380	       05  REP-DETAIL2 PIC  X(80).

000390	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000400	       05  REP-PAGE    PIC  X(06).
000410	       05  REP-P-NO    PIC  Z(03)9.
000420	       05  FILLER      PIC  X(09).
000430	       05  REP-COMPANY PIC  X(40).
000440	       05  FILLER      PIC  X(05).
000450	       05  REP-DATE    PIC  X(06).
000460	       05  REP-DTE     PIC  Z9/99/9999.

000470	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000480	       05  FILLER      PIC  X(28).
000510	       05  REP-HEAD1   PIC  X(40).
000560	       05  FILLER      PIC  X(28).

001810	   03  ORD-L1 REDEFINES W02-PRN-LINE.
001820	       05  ORD-H1      PIC  X(07).
001830	       05  ORD-H2      PIC  X(40).
001840	       05  ORD-H3      PIC  X(17).
001850	       05  ORD-H4      PIC  X(13).
001860	       05  ORD-H5      PIC  X(09).
	       05  ORD-H6      PIC  X(10).

001870	   03  ORD-L2 REDEFINES W02-PRN-LINE.
001880	       05  ORD-AC1     PIC  X(06).
001890	       05  FILLER      PIC  X(01).
001900	       05  ORD-NME1    PIC  X(39).
001910	       05  FILLER      PIC  X(01).
	       05  ORD-CONTACT PIC  X(16).
	       05  FILLER      PIC  X(01).
	       05  ORD-TEL1    PIC  X(12).
	       05  FILLER      PIC  X(01).
001920	       05  ORD-REF1    PIC  X(08).
001930	       05  FILLER      PIC  X(01).
001940	       05  ORD-DATE1   PIC  Z9/99/9999.

001810	   03  ORD-L3 REDEFINES W02-PRN-LINE.
001820	       05  ORD-H7      PIC  X(09).
001830	       05  ORD-H8      PIC  X(11).
001840	       05  ORD-H9      PIC  X(07).
001850	       05  ORD-H10     PIC  X(40).
001860	       05  ORD-H11     PIC  X(17).
	       05  ORD-H12     PIC  X(12).

001870	   03  ORD-L4 REDEFINES W02-PRN-LINE.
001920	       05  ORD-REF2    PIC  X(08).
001930	       05  FILLER      PIC  X(01).
001940	       05  ORD-DATE2   PIC  Z9/99/9999.
	       05  FILLER      PIC  X(01).
001880	       05  ORD-AC2     PIC  X(06).
001890	       05  FILLER      PIC  X(01).
001900	       05  ORD-NME2    PIC  X(39).
001910	       05  FILLER      PIC  X(01).
	       05  ORD-CONT2   PIC  X(16).
	       05  FILLER      PIC  X(01).
	       05  ORD-TEL2    PIC  X(12).

	   03  ORD-L5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(11).
	       05  ORD-H13     PIC  X(19).
	       05  ORD-H14     PIC  X(31).
	       05  ORD-H15     PIC  X(10).
	       05  ORD-H16     PIC  X(12).
	       05  ORD-H17     PIC  X(13).

001990	   03  ORD-L6 REDEFINES W02-PRN-LINE.
002000	       05  FILLER      PIC  X(11).
002010	       05  ORD-ITEM    PIC  X(18).
002020	       05  FILLER      PIC  X(01).
002030	       05  ORD-DESC    PIC  X(30).
002040	       05  FILLER      PIC  X(01).
002050	       05  ORD-QNT     PIC  Z(04)9.999.
002060	       05  FILLER      PIC  X(01).
002070	       05  ORD-PRC     PIC  Z(06)9.999.
002080	       05  FILLER      PIC  X(01).
	       05  ORD-EXT     PIC  Z(08)9.999.

001990	   03  ORD-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(11).
	       05  ORD-H18     PIC  X(09).
	       05  ORD-SUBTOT  PIC  Z(08)9.99.
	       05  FILLER      PIC  X(02).
	       05  ORD-H19     PIC  X(09).
	       05  ORD-DISCNT  PIC  Z(08)9.99-.
	       05  FILLER      PIC  X(02).
	       05  ORD-H20     PIC  X(06).
	       05  ORD-VAT     PIC  Z(08)9.99.
	       05  FILLER      PIC  X(02).
	       05  ORD-H21     PIC  X(06).
	       05  ORD-TOTAL   PIC  Z(08)9.99.

       COPY W12.WS.

       COPY W20.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006840 SCREEN SECTION.

       COPY BLANK.CRT.

007420 01  S02.
007430	   03  LINE 2  COLUMN 29 FOREGROUND-COLOR 15
007440				 VALUE "OUTSTANDING SALES ORDERS".

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
015160	     MOVE ZERO		 TO WS-PAGE.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.



016340	     PERFORM BA000.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /
      *    ****    R E P O R T	 H E A D I N G S
      *
       AB000		 SECTION.
       AB00.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO REP-PAGE.
016640	     MOVE "DATE:"	 TO REP-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO REP-DTE.
016670	     MOVE WS-PAGE	 TO REP-P-NO.
016680	     MOVE W95-COMP	 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****    S E T   1 2 C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-SEQ = "A"
	       MOVE "OUTSTANDING ORDERS: ACCOUNT NO. SEQUENCE"
				 TO REP-HEAD1
	   ELSE
	       MOVE "OUTSTANDING ORDERS: ORDER NUMB. SEQUENCE"
				 TO REP-HEAD1.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
016710	     MOVE SPACES	 TO REP-DETAIL1.
	   IF WS-SEQ = "A"
016730	       MOVE "AC-NO."	 TO ORD-H1
016740	       MOVE "NAME"	 TO ORD-H2
016750	       MOVE "CONTACT"	 TO ORD-H3
016760	       MOVE "TELEPHONE"	 TO ORD-H4
	       MOVE "REF NUMB"	 TO ORD-H5
	       MOVE "  DATE"	 TO ORD-H6
	   ELSE
016770	       MOVE "ORD NUMB"	 TO ORD-H7
016780	       MOVE "  DATE"	 TO ORD-H8
016790	       MOVE "AC-NO."	 TO ORD-H9
	       MOVE "NAME"	 TO ORD-H10
	       MOVE "CONTACT"	 TO ORD-H11
016800	       MOVE "TELEPHONE"	 TO ORD-H12.
	     PERFORM CALL-PRINTUTL.
016710	     MOVE SPACES	 TO REP-DETAIL1.
016810	     MOVE "ITEM CODE"	 TO ORD-H13.
016820	     MOVE "DESCRIPTION"	 TO ORD-H14.
016830	     MOVE "QUANTITY"	 TO ORD-H15.
016840	     MOVE "UNIT PRICE"	 TO ORD-H16.
	     MOVE "    EXTENDED" TO ORD-H17.
	     PERFORM CALL-PRINTUTL.
016860	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.

       AB99.
	     EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011170 COPY CONTROL.RD.

011290 COPY PARAM.RD.

011270 COPY SORDER.RD.

       COPY PRINTUTL.AP3.

      /
      *    ****    P R I N T   O U T S T A N D I N G   O R D E R S
      *
018500 BA000	     SECTION 50.
018510 BA00.
      *
      *    ****    S A L E S   O R D E R   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590	     DISPLAY S02.
	     DISPLAY "A" AT 0412 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ccount, " WITH FOREGROUND-COLOR 3
		     "O" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "rder sequence or " WITH FOREGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH FOREGROUND-COLOR 3.
	     MOVE "O"		 TO WS-SEQ.

       BA05.
	     ACCEPT WS-SEQ AT 0446
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05.
	     CALL "CBL_TOUPPER" USING WS-SEQ
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-SEQ = "A" OR "O")
	       GO TO BA05.
	     PERFORM AB000.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Reading :" AT 0612.
	   IF WS-SEQ = "A"
	       GO TO BA30.
      *
      *    ****    O R D E R   N O .   S E Q U E N C E
      *
000740	     MOVE "00000001"	 TO SOR-REF.
	     MOVE ZERO		 TO SOR-SEQ.
000750       PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
000880     IF WS-F-ERROR = 20
	       GO TO BA45.

018710 BA10.
000920	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
	       GO TO BA50.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY SOR-KEY AT 0622
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF SOR-TYPE NOT = ZERO
	       GO TO BA10.

       BA15.
	   IF NOT SOR-ORDER
	       GO TO BA10.
	     MOVE SOR-REF	 TO ORD-REF2.
      *
      *    ****   Switch date from YYYYMMDD to DDMMYYYY
      *
	     MOVE SOR-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
002770	     MOVE W22-YY2	 TO W22-YY3.
002780	     MOVE W22-MM2	 TO W22-MM3.
002790	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO ORD-DATE2.
	     MOVE SOR-AC	 TO ORD-AC2.
	     MOVE SOR-NAME	 TO ORD-NME2.
	     MOVE SOR-CONTACT	 TO ORD-CONT2.
	     MOVE SOR-TEL	 TO ORD-TEL2.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.

       BA20.
	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
	       PERFORM BA60
	       GO TO BA50.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY SOR-KEY AT 0622
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF SOR-TYPE = 0
	       PERFORM BA60
	       IF WS-SEQ = "A"
		   GO TO BA35
	       ELSE
		   GO TO BA10.
	   IF SOR-TYPE = 9
	       GO TO BA25.
	   IF NOT (SOR-TYPE = 1)
	       GO TO BA20.
002010	     MOVE SOR-EXT-ITEM	 TO ORD-ITEM.
002030	     MOVE SOR-DESC	 TO ORD-DESC.
002050	     MOVE SOR-QNT	 TO ORD-QNT.
002070	     MOVE SOR-SELL	 TO ORD-PRC.
	     MOVE SOR-VAL	 TO ORD-EXT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM AB000.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
	     GO TO BA20.

       BA25.
	       MOVE "SUB-TOTAL"  TO ORD-H18.
	       MOVE SOR-SUBTOT	 TO ORD-SUBTOT.
	       MOVE "DISCOUNT"	 TO ORD-H19.
	       MOVE SOR-DSCNT	 TO ORD-DISCNT.
	       MOVE "V A T"	 TO ORD-H20.
	       MOVE SOR-TAX	 TO ORD-VAT.
	       MOVE "TOTAL"	 TO ORD-H21.
	       MOVE SOR-TOT	 TO ORD-TOTAL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM AB000.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
	   IF WS-SEQ = "A"
	       GO TO BA35.
	     GO TO BA10.
      *
      *    ****    A C C O U N T   N U M B E R	 S E Q U E N C E
      *
       BA30.
	     MOVE SPACES	 TO SOR-AC.
000740	     MOVE "00000001"	 TO SOR-REF
             MOVE ZERO           TO SOR-SEQ
000750	     PERFORM START-AT-ORD-DEBT THRU READ-SORDER-EXIT.
000880     IF WS-F-ERROR = 20
	       GO TO BA45.

018710 BA35.
000920	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
	       GO TO BA50.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY SOR-KEY AT 0622
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF SOR-TYPE NOT = ZERO
	       GO TO BA35.

       BA40.
	   IF NOT SOR-ORDER
	       GO TO BA35.
	     MOVE SOR-AC	 TO ORD-AC1.
	     MOVE SOR-NAME	 TO ORD-NME1.
	     MOVE SOR-CONTACT	 TO ORD-CONTACT.
	     MOVE SOR-TEL	 TO ORD-TEL1.
	     MOVE SOR-REF	 TO ORD-REF1.
      *
      *    ****   Switch date from YYYYMMDD to DDMMYYYY
      *
	     MOVE SOR-DTE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
002770	     MOVE W22-YY2	 TO W22-YY3.
002780	     MOVE W22-MM2	 TO W22-MM3.
002790	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO ORD-DATE1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
	     GO TO BA20.

       BA45.
014970	     MOVE "**** NO OUTSTANDING ORDERS ON FILE ****"
				 TO REP-DETAIL1.
	     GO TO BA55.

       BA50.
014970	     MOVE "**** END OF OUTSTANDING ORDERS REPORT ****"
				 TO REP-DETAIL1.

       BA55.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
009980	     MOVE SPACES	 TO REP-DETAIL1.
014990	     GO TO BA999.

       BA60.
014970	     MOVE "**** NO TRAILER RECORD FOR THIS ORDER ****"
				 TO REP-DETAIL1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009980	     MOVE SPACES	 TO REP-DETAIL1.

       BA65.
	     EXIT.

031080 BA999.
031090       EXIT.
      /
049180 ZA000-INIT	       SECTION 9.
049190 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
049200	     DISPLAY CLR-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
             MOVE 1              TO USER-ACTION
                                    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
             MOVE ZERO           TO USER-NUMBER.
             MOVE 11             TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
             MOVE 53             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
             MOVE 64             TO MOUSE-FUNC.
             MOVE 1              TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC
                              MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"          TO MOUSE-ATTACHED
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC
                                MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *            A S   F U N C T I O N   K E Y
      *
             MOVE 3              TO USER-ACTION.
             MOVE 27             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
050580	     MOVE 1		 TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
051530	     GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
040050	     MOVE 1 TO WS-S1.
040060	     MOVE SPACES	 TO WS-MD-LINE.
040090 ZA60.
040130	     MOVE WS-G1 	 TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
040150	   IF WS-S1 < 80
040160         ADD 1 TO WS-S1
040170         GO TO ZA60.
040220	     MOVE WS-G14	 TO WS-TCA   (1)  WS-BCA   (1).
040260	     MOVE WS-G13	 TO WS-TCA   (80) WS-BCA   (80).
040300	     MOVE WS-G2 	 TO WS-TCA   (10) WS-TCA   (17)
040360                              WS-TCA   (47) WS-TCA   (56)
040370				    WS-TCA   (59) WS-TCA   (68).
040400	     MOVE WS-G3 	 TO WS-MCA   (1)  WS-MCA   (80)
040470                              WS-MCA   (10) WS-MCA   (17)
040480                              WS-MCA   (47) WS-MCA   (56)
040490				    WS-MCA   (59) WS-MCA   (68).
040550	     MOVE WS-G4 	 TO WS-BCA   (10) WS-BCA   (17)
040610                              WS-BCA   (47) WS-BCA   (56)
040620				    WS-BCA   (59) WS-BCA   (68).
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorฤ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
051540 ZA200.
051550       DISPLAY "Files locked - Try later" AT 2312
051560                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
051570	     DISPLAY "Press any key" AT 2512
051580                WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
051590       ACCEPT WS-OPTION AT 2526
                    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
051600       EXIT PROGRAM.
051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	9.
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
051850         MOVE W02-NETWORK TO WS-FILE
051860         MOVE WS-NETKEY TO WS-KEY
052070	   ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052150     ELSE
052200	   IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220	       MOVE ZERO	 TO WS-KEY.
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
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
