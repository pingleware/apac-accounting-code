      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	     **     ******     *
      *   **	**     **     **    **	 **  **     ***    **	 **    *
      *   **	**     **     **    **	**    **     **    **	 **    *
      *   **	**     **     *******	**    **     **     *******    *
      *   **	**     **     **	**    **     ** 	 **    *
      *   **	**     **     **	 **  **      **    **	 **    *
      *   *******      **     **	  ****	   ******   ******     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R   R E C E I P T I N G	R E P O R T S	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP019.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   June 2002.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2011
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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000120 COPY PARAM.SL.

000140 COPY RECEIPTS.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY RECEIPTS.FDE.

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
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002790 77  WS-SUB-1	   PIC 9(04)	 COMP-5.
002800 77  WS-SUB-2        PIC 9(04)     COMP-5.
002810 77  WS-SUB-3        PIC 9(04)     COMP-5.
002820 77  WS-SUB-4        PIC 9(04)     COMP-5.
002830 77  WS-SUB-5        PIC 9(04)     COMP-5.
002840 77  WS-SUB-6        PIC 9(04)     COMP-5.
002890 77  WS-MSUB         PIC 9(04)     COMP-5.
002900 77  WS-PARKEY       PIC 9(04)     COMP-5.
002930 77  WS-NETKEY	   PIC 9(06)	 COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
002990 77  WS-OPTION       PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
003020 77  WS-USER	   PIC	X(03).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-STM          PIC 9(01).
003180 77  WS-BDT	   PIC 9(01).
003190 77  WS-BAD	   PIC X(01).
003190 77  WS-TYPE	   PIC X(01).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO	   PIC X(06) VALUE SPACES.
003260 77  WS-S1           PIC 9(04)     COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
003260 77  WS-IXS1	   PIC 9(04)	 COMP-5.
003390 77  WS-A 	   PIC X(19) VALUE "Any key to continue".
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY RECEIPTS.ID.

	   03  RECEIPT-ID-ST   PIC  X(64).

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
001570	       05  REC-PAGE    PIC  X(06).
001580	       05  REC-P-NO    PIC  Z(03)9.
001590	       05  FILLER      PIC  X(24).
001610	       05  REC-COMPANY PIC  X(40).
001620	       05  REC-HEAD    PIC  X(24).
001630	       05  FILLER      PIC  X(17).
001640	       05  REC-DATE    PIC  X(06).
001650	       05  REC-DTE     PIC  Z9/99/9999.

	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
	       05  REP-CO      PIC  X(40).

000800	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(01).
000810	       05  REC-AC      PIC  X(06).
000820	       05  FILLER      PIC  X(02).
000830	       05  REC-NAME    PIC  X(40).
000840	       05  FILLER      PIC  X(02).
000850	       05  REC-DTE2    PIC  Z9/99/9999.
000860	       05  FILLER      PIC  X(02).
000870	       05  REC-REF     PIC  X(08).
000880	       05  FILLER      PIC  X(02).
000890	       05  REC-VAL     PIC  Z(08)9.99- BLANK WHEN ZERO.
000880	       05  FILLER      PIC  X(02).
000890	       05  REC-CSH     PIC  Z(08)9.99- BLANK WHEN ZERO.
000880	       05  FILLER      PIC  X(02).
000890	       05  REC-CHQ     PIC  Z(08)9.99- BLANK WHEN ZERO.
000880	       05  FILLER      PIC  X(02).
000890	       05  REC-TRF     PIC  Z(08)9.99- BLANK WHEN ZERO.
000880	       05  FILLER      PIC  X(01).

001080	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
001090	       05  REC-DET     PIC  X(132).

001100	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(01).
001110	       05  REC-HEAD1   PIC  X(08).
001120	       05  REC-HEAD2   PIC  X(45).
001130	       05  REC-HEAD3   PIC  X(09).
001140	       05  REC-HEAD4   PIC  X(10).
001150	       05  REC-HEAD5   PIC  X(17).
001160	       05  REC-HEAD6   PIC  X(13).
001170	       05  REC-HEAD7   PIC  X(15).
001180	       05  REC-HEAD8   PIC  X(14).

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

       COPY W12.WS.

       COPY W20.WS.

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

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005170 COPY W50.WS.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

005490 01  W80-ACCOUNT.
005500     03  W80-ACNO        PIC X(06).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530         05  W80-SERIAL  PIC 9(06).
005520     03  W80-TYPE        PIC 9(01).

005560 01  W90-TRAN.
005570	   03  W90-DATE        PIC X(10).
005580     03  W90-DTE REDEFINES W90-DATE
005590			       PIC 99/99/9999.
005610	   03  W90-REF	       PIC X(08).
005670	   03  W90-VAL	       PIC S9(09)V99 COMP-3.
005670	   03  W90-CSH	       PIC S9(09)V99 COMP-3.
005680	   03  W90-CHQ	       PIC S9(09)V99 COMP-3.
005690	   03  W90-TRF	       PIC S9(09)V99 COMP-3.

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).

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

006250 01  S07.
006260	   03  LINE  7 COLUMN 12 VALUE "Are you sure (".
	   03	       COLUMN 26 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Y".
	   03	       COLUMN 27 VALUE "/".
	   03	       COLUMN 28 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 29 VALUE ")".

006540 01  S14.
006550	   03  LINE  3 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
006560				 VALUE "DEBTOR RECEIPTING REPORTS".
006570     03  LINE  5 COLUMN 12 VALUE "Printing for".

       COPY S99.CRT.

       COPY OPT.CRT.

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

       AA49.
006950	     EXIT PROGRAM.

       COPY DATE.CNV.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

      /
      *    ****    READ FILES ROUTINES
      *
007340 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY RECEIPTS.RD.

      /
      *    ****    D E B T O R	 R E C E I P T I N G   R E P O R T S
      *
016170 DA000	       SECTION 5.

016180 DA00.
016190	     DISPLAY CLR-SCREEN.
	     MOVE LS-USER	 TO WS-USER.
016200       DISPLAY S14.
	     MOVE ZERO		 TO WS-BDT.
	     MOVE "N"		 TO WS-BAD.

016210 DA05.
016220       DISPLAY W95-COMP AT 0527 WITH FOREGROUND-COLOR 11.
016230       DISPLAY S07.
016240	     ACCEPT WS-OPTION AT 0731
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016310	   IF WS-OPTION = "N"
016320	       GO TO DA999-EXIT.
	     DISPLAY "Print reports for Work Station " AT 0912
		      WS-USER WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	   IF NOT (LS-USER = LS-SYS-ID)
	       GO TO DA15.

       DA10.
	     ACCEPT WS-USER AT 0943
		    WITH FOREGROUND-COLOR 6 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999-EXIT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA10
	       END-EVALUATE.
	   IF WS-USER = SPACES
	       GO TO DA999-EXIT.
	     MOVE 3		 TO WS-LENGTH
	     CALL "CBL_TOUPPER" USING WS-USER
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY WS-USER AT 0943
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE 1		 TO WS-LENGTH.

       DA15.
	     MOVE RECEIPT-ID-ST	 TO W02-RECEIPTS.
	     INSPECT W02-RECEIPTS REPLACING FIRST "XXX"
				  BY WS-USER.

      *	     DISPLAY W02-RECEIPTS AT 2502.
      *	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

	     OPEN INPUT RECEIPTS.
	   IF NOT (WS-STATUS = "00")
	       MOVE "No details for Work Station "
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017130	       GO TO DA10.
	     MOVE "  !" 	 TO RCT-REF.
	     PERFORM START-AT-RECT-REF THRU READ-RECEIPTS-EXIT.
	   IF NOT (WS-STATUS = "00")
	       GO TO DA999-EXIT.
016570	     MOVE 0		 TO WS-PAGE
				    W90-VAL
				    W90-CSH
016580				    W90-CHQ
				    W90-TRF.
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

016610 DA20.
	     MOVE "H"		 TO W02-PRN-TYPE.
016620	     MOVE SPACES	 TO REP-DETAIL1.
016630	     MOVE "PAGE:"	 TO REC-PAGE.
016640	     MOVE "DATE:"	 TO REC-DATE.
016650	     ADD 1		 TO WS-PAGE.
016660	     MOVE W12-TODAY	 TO REC-DTE.
016670	     MOVE WS-PAGE	 TO REC-P-NO.
	     MOVE 0		 TO WS-ADVANCE.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO REP-LINE1
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
016680	     MOVE W95-COMP	 TO REC-COMPANY.
016690	     MOVE " - RECEIPTS LISTING"
				 TO REC-HEAD.
	     MOVE "H"		 TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO REP-LINE1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
012760	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
016710	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 2		 TO WS-ADVANCE.

016720 DA25.
016730	     MOVE "AC.NO."	 TO REC-HEAD1.
016740	     MOVE "NAME"	 TO REC-HEAD2.
016750	     MOVE "DATE"	 TO REC-HEAD3.
016760	     MOVE "REF. NO."	 TO REC-HEAD4.
016770	     MOVE "RECEIPT VAL." TO REC-HEAD5
016780	     MOVE "CASH VALUE"	 TO REC-HEAD6
016790	     MOVE "CHEQUE VALUE" TO REC-HEAD7
016800	     MOVE "TRANSFER VAL" TO REC-HEAD8.
	     PERFORM CALL-PRINTUTL.
016860	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "D"		 TO W02-PRN-TYPE.
      *
      *    ****   T E S T   I F   O U T P U T	T O   S C R E E N
      *
016870 DA30.
	   IF WS-PRINTER = "S"
	       GO TO DA40.

016890 DA35.
	     DISPLAY "Reference Numb:" AT 1112.

017380 DA40.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO DA999-EXIT.
017390	     PERFORM READ-RECEIPTS-NEXT THRU READ-RECEIPTS-EXIT.
	   IF WS-F-ERROR = 90
	       GO TO DA45.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY RCT-REF AT 1128 WITH FOREGROUND-COLOR 11.
000310	     MOVE RCT-AC	 TO REC-AC.
	     MOVE RCT-NAME	 TO REC-NAME.
      *
      *    ****   S W I T C H	D A T E
      * 	  F R O M   YYYYMMDD   T O   DDMMYYYY
      *
	     MOVE RCT-DATE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE3	 TO REC-DTE2.
000320	     MOVE RCT-REF	 TO REC-REF.
000340	     MOVE RCT-VAL	 TO REC-VAL.
000340	     MOVE RCT-CSH	 TO REC-CSH.
000340	     MOVE RCT-CHQ	 TO REC-CHQ.
000340	     MOVE RCT-TRF	 TO REC-TRF.
	     ADD RCT-VAL	 TO W90-VAL.
	     ADD RCT-CSH	 TO W90-CSH.
	     ADD RCT-CHQ	 TO W90-CHQ.
	     ADD RCT-TRF	 TO W90-TRF.
	     MOVE 1		 TO WS-ADVANCE.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017950	       PERFORM DA20 THRU DA25.
017960	     MOVE SPACES	 TO REP-DETAIL1.
017970	     GO TO DA40.

018290 DA45.
	     PERFORM CALL-PRINTUTL.
018380	     MOVE "TOTALS"	 TO REC-REF.
000340	     MOVE W90-VAL	 TO REC-VAL.
000340	     MOVE W90-CSH	 TO REC-CSH.
000340	     MOVE W90-CHQ	 TO REC-CHQ.
000340	     MOVE W90-TRF	 TO REC-TRF.
	     MOVE "E"		 TO WS-COMMAND.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

018660 DA50.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE RECEIPTS.
	     DISPLAY CLR-SCREEN.

       DA55.
	     MOVE "Clear Receipts Report File Y/N  [ ]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030150	   IF NOT (WS-OPTION = "N" OR "Y")
	       GO TO DA55.
	   IF WS-OPTION = "Y"
	       OPEN OUTPUT RECEIPTS
	       CLOSE RECEIPTS.

019000 DA999-EXIT.
019010       EXIT.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECEIPTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO RECEIPT-ID-ST.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECEIPTS.

025620 ZA25.
	     MOVE 4		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-AGE	 TO WS-AGE.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
025830	     GO TO ZA999-EXIT.
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
041720     ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 90
041860	       MOVE W02-RECEIPTS TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE RCT-REF	 TO WS-KEYX.


       COPY DISPERR.PRO.
