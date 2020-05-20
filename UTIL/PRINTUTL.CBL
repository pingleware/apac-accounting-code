      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *      *******	*******	   ******   **	   **	********       *
      *      **	   **	**    **     **	    ***	   **	   **	       *
      *      **	   **	**    **     **	    ** *   **	   **	       *
      *      *******	*******	     **	    **	*  **	   **	       *
      *      **		**    **     **	    **	 * **	   **	       *
      *      **		**    **     **	    **	  ***	   **	       *
      *      **		**    **   ******   **	   **	   **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 	  P R I N T   U T I L I T Y   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRINTUTL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   January 1997.

	   This program is used to print/spool/display report details.

	   The program performs the following functions:

	       Open the printer.
	       Open the spool file.
	       Set up the necessary parameters if the report is to appear
	       on the screen.
	       Print a line and advance the paper as requested.
	       Spool a line and pass on the advance parameters.
	       Display a line of print as requested (allows for left/right).

	   The calling program/s treat all reports as if they were being
	   printed on a printer and are unaware of the Users request to
	   spool or display the report.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2011
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

000120 COPY PARAM.SL.

000200	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000200	   SELECT PRNSPL  ASSIGN W02-SPOOLER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

      /
000330 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
       01  R-SL1.
	   03  R-SLA	       PIC  X(78).
	   03  FILLER	       PIC  X(58).
       01  R-SL2.
	   03  FILLER	       PIC  X(58).
	   03  R-SLB	       PIC  X(78).
000350 01  REP-LINE1.
000360	   03  REP-DETAIL1     PIC  X(136).

000330 FD  PRNSPL.
       01  SPL-L1.
	   03  SPL-CONT.
	       05  SPL-ADV     PIC  9(02).
	       05  SPL-LIN     PIC  9(02).
000360	   03  SPL-DETAIL1     PIC  X(136).

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003260 77  WS-S1	   PIC	9(04)	 COMP-5.
003270 77  WS-S2	   PIC	9(04)	 COMP-5.
003280 77  WS-S3	   PIC	9(04)	 COMP-5.
003290 77  WS-S4	   PIC	9(04)	 COMP-5.
003300 77  WS-S5	   PIC	9(04)	 COMP-5.
003310 77  WS-S6	   PIC	9(04)	 COMP-5.
003320 77  WS-S7	   PIC	9(04)	 COMP-5.
003330 77  WS-S8	   PIC	9(04)	 COMP-5.
003330 77  WS-LEFT	   PIC	9(04)	 COMP-5.
003330 77  WS-RIGHT	   PIC	9(04)	 COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001820 77  WS-OPTION	   PIC	X(01).
001820 77  WS-SINGLE	   PIC	X(01).
       77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
       77  WS-LINES	   PIC	9(02)	 VALUE ZERO.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06)	 VALUE X"000000000000".
003660 77  WS-NORM	   PIC	X(06)	 VALUE X"000000000000".
003070 77  WS-EXPP	   PIC	X(06)	 VALUE X"000000000000".
003080 77  WS-ECAN	   PIC	X(06)	 VALUE X"000000000000".
003090 77  WS-8LPI	   PIC	X(06)	 VALUE X"000000000000".
003100 77  WS-6LPI	   PIC	X(06)	 VALUE X"000000000000".
       77  WS-10CPI	   PIC	X(06)	 VALUE X"000000000000".
       77  WS-12CPI	   PIC	X(06)	 VALUE X"000000000000".
       77  WS-17CPI	   PIC	X(06)	 VALUE X"000000000000".
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
       77  WS-SKIP	   PIC	X(01)	 VALUE "N".

003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250	       05  FILLER      PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270			       PIC X(50).
		  07  FILLER   PIC X(10).
003280	       05  FILLER      PIC X(19).

       COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

	   03  W02-PRINTER.
	       05  W02-PRN     PIC  X(06).

	   03  W02-SPOOLER.
	       05  W02-WS      PIC  X(02).
	       05  W02-US      PIC  X(01).
	       05  W02-TERM    PIC  X(03).
	       05  W02-US2     PIC  X(01).
	       05  W02-REP     PIC  X(01).
	       05  W02-P       PIC  X(01).
	       05  W02-USER    PIC  X(03).

       01  W05-SCREEN.
	   03  W05-PRINT-DETAIL.
	     05  W05-LINE OCCURS 20.
	       07  W05-TYPE    PIC  X(01).
	       07  W05-DETAIL  PIC  X(136).
	   03  W05-PRINT-DETAIL2 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE2 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  W05-SLA     PIC  X(78).
	       07  FILLER      PIC  X(58).
	   03  W05-PRINT-DETAIL2 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE3 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  FILLER      PIC  X(58).
	       07  W05-SLB     PIC  X(78).
	   03  W05-PRINT-DETAIL3 REDEFINES W05-PRINT-DETAIL.
	     05  W05-LINE4 OCCURS 20.
	       07  FILLER      PIC  X(01).
	       07  W05-CHAR    PIC  X(01) OCCURS 136.

       01  W06-SCREEN.
	   03  W06-PRINT-DETAIL.
	     05  W06-LINE OCCURS 20.
	       07  W06-CHAR    PIC  X(01) OCCURS 78.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

003930 01  W25-CALCS.
002860	   03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC 9(04).
	       05  W25-SUB     PIC 9(01).
	       05  FILLER      PIC 9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

       01  LS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      * 	    E = End of report
      *		    O = Open printer
      * 	    P = Print a line
      * 	    S = Skip to new page
      *
	   03  LS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      * 	    S = Screen
      * 	    1 = Lpt1
      * 	    2 = Lpt2
      * 	    3 = Lpt3
      * 	    4 = Com1
      * 	    5 = Com2
      *
	   03  LS-PRINTER.
	       05  LS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      * 	    1-3 = Print advance lines specified
      * 	    4-? = Print advance 1 line, then use a loop
      * 		  to advance the remaining lines.
      * 	     99 = Print and advance to a new page.
      *
	   03  LS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      * 	   2 = Invoices/Credit Notes
      * 	   3 = General Reports (Stock)
      * 	   4 = General Reports (Creditors)
      * 	   5 = General Reports (Debtors)
      * 	   6 = General Reports (G/Ledger)
      * 	   7 = Statements
      * 	   8 = Labels
      * 	   9 = Quotations
      *
	   03  LS-REPORT       PIC  9(01).
	   03  LS-SCREEN       PIC  9(01).
	   03  LS-SCREEN-LIN   PIC  9(02).
	   03  LS-LINAGE       PIC  9(02).
	   03  LS-PAPER        PIC  X(01).
	   03  LS-ORIENTATION  PIC  X(01).

       01  L02-PRINTER-DETAILS.
	   03  L02-PRINTER     PIC  X(12).
002570	   03  L02-PGE-LENGTH  PIC  9(02).
001870	   03  L02-PRN-LENGTH  PIC  9(02).
	   03  L02-LINAGE      PIC  9(02).
	   03  L02-PRN-STATUS  PIC  X(01).
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
	   03  L02-PRN-TYPE    PIC  X(01).
	   03  L02-PRN-LINE    PIC  X(136).

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

       01  SD-LEFT.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLA(20).

       01  SD-RIGHT.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W05-SLB(20).

       01  SD-RANDOM.
	   03  LINE  3 COLUMN  1 FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(1).
	   03  LINE  5 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(2).
	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(3).
	   03  LINE  7 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(4).
	   03  LINE  8 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(5).
	   03  LINE  9 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(6).
	   03  LINE 10 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(7).
	   03  LINE 11 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(8).
	   03  LINE 12 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(9).
	   03  LINE 13 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(10).
	   03  LINE 14 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(11).
	   03  LINE 15 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(12).
	   03  LINE 16 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(13).
	   03  LINE 17 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(14).
	   03  LINE 18 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(15).
	   03  LINE 19 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(16).
	   03  LINE 20 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(17).
	   03  LINE 21 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(18).
	   03  LINE 22 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(19).
	   03  LINE 23 COLUMN  2 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 PIC  X(78) FROM W06-LINE(20).

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS-SCREEN-PRINT
				L02-PRINTER-DETAILS.
010250 AA000-MAIN      SECTION.
010260 AA000-INIT.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-SCREEN-PRINT
				 TO WS-SCREEN-PRINT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
       AA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       AA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA00-READ-APACFIDS-EXIT.
	     EXIT.

       AA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.

       AA10-OPTIONS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Must the Screen be cleared - Already been opened.     ³
      *    ³ If a USER selects the screen as output WS-COMMAND is set  ³
      *    ³ to 'I' and this test will clear the screen to a white     ³
      *    ³ background and then exit.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-COMMAND = "I"
	       PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
	       MOVE WS-SCREEN-PRINT
				 TO LS-SCREEN-PRINT
	       GO TO AA999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Printer/Screen/Disk has not been opened yet.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF NOT(L02-PRN-STATUS = "O")
	       PERFORM SAVE-SCREEN-3.
	     PERFORM PRINT-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		Printer/Screen/Disk has been closed.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF L02-PRN-STATUS = "C"
	       PERFORM RESTORE-SCREEN-3.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Move Printer/Screen/Disk controls to linkage area.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE WS-SCREEN-PRINT
				 TO LS-SCREEN-PRINT.

       AA999.
	     EXIT PROGRAM.

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

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

011750 COPY PARAM.RD.

       PRINT-SCREEN    SECTION.
       PRINT-STATUS-TEST.
	   EVALUATE WS-COMMAND
      *
      *    ****   O P E N   T H E   P R I N T E R   F I L E
      *
	     WHEN "O"
	       IF L02-PRN-STATUS = "C"
		   PERFORM OPEN-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
      *
      *    ****   C L O S E   T H E   P R I N T E R   F I L E
      *
	     WHEN "C"
	       IF L02-PRN-STATUS = "O"
		   PERFORM CLOSE-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
	     WHEN "S"
	       IF L02-PRN-STATUS = "O"
		   PERFORM NEW-PAGE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
	   END-EVALUATE.
	     MOVE L02-PRN-LINE	TO REP-DETAIL1.
      *
      *    ****	  C H E C K   I F   P R I N T E R   F I L E   O P E N
      *
	   IF L02-PRN-STATUS = "C"
	       MOVE "Printer file not open"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PRINT-SCREEN-EXIT.
      *
      *    ****	  C H E C K   I F   P R I N T E R   O R   S C R E E N
      *
	   IF WS-PRINTER = "S"
	       PERFORM SCREEN-PRINT
	   ELSE
	       PERFORM LINE-PRINT-SPOOL.

       PRINT-SCREEN-EXIT.
	     EXIT.
      *
      *    ****   O P E N   P R I N T E R   /	S P O O L   F I L E
      *
       OPEN-PRINT-FILE	SECTION 50.
       OPEN-PRINTER.
	   IF L02-LINAGE = 99
	       OPEN INPUT PARAM.
	     MOVE "P"		 TO WS-OPTION.

       OPEN-PRINTER-NO.
	     MOVE SPACES	 TO WS-ERR-MES.
	   IF LS-PRINTER = "S"
	       STRING "'P'rinter or 'D'isk  ["
		      DELIMITED SIZE
		      WS-OPTION DELIMITED SIZE
		      "]" DELIMITED SIZE
				 INTO WS-ERR-MES
	       PERFORM OPT-MESSAGE TEST AFTER
		       UNTIL WS-OPTION = "P" OR "D"
	   ELSE
	       STRING "'P'rinter,'S'creen or 'D'isk  ["
		      DELIMITED SIZE
		      WS-OPTION DELIMITED SIZE
		      "]" DELIMITED SIZE
				 INTO WS-ERR-MES
	       PERFORM OPT-MESSAGE TEST AFTER
		       UNTIL WS-OPTION = "P" OR "D" OR "S".
	     MOVE WS-OPTION	 TO WS-PRINTER.
	   IF WS-OPTION = "S"
	       PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
	       MOVE 62		 TO L02-PRN-LENGTH
	       MOVE 66		 TO L02-PGE-LENGTH
	       GO TO OPEN-PRINTER-STATUS.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
	       END-IF
	   ELSE
	       MOVE 110		 TO WS-USUB.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   EVALUATE WS-REPORT
	     WHEN 1
	       PERFORM OPEN-REPORT-1
	     WHEN 2
	       PERFORM OPEN-REPORT-2
	     WHEN 3
	     WHEN 4
	     WHEN 5
	     WHEN 6
	       PERFORM OPEN-REPORT-3
	     WHEN 7
	       PERFORM OPEN-REPORT-4
	     WHEN 8
	       PERFORM OPEN-REPORT-5
	     WHEN 9
	       PERFORM OPEN-REPORT-6
	   END-EVALUATE.
	     DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE TRM-CONP(WS-S6)
                                 TO WS-COND.
	     MOVE TRM-NORP(WS-S6)
                                 TO WS-NORM.
	     MOVE TRM-EXPP(WS-S6)
				 TO WS-EXPP W08-EXPD.
	     MOVE TRM-ECAN(WS-S6)
                                 TO WS-ECAN.
	     MOVE TRM-8LPI(WS-S6)
                                 TO WS-8LPI.
	     MOVE TRM-6LPI(WS-S6)
                                 TO WS-6LPI.
	     MOVE TRM-10CPI(WS-S6)
                                 TO WS-10CPI.
	     MOVE TRM-12CPI(WS-S6)
                                 TO WS-12CPI.
	     MOVE TRM-17CPI(WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH(WS-S6)
				 TO WS-PRN-LENGTH
				    L02-PRN-LENGTH.
	     MOVE TRM-PAGE(WS-S6)
				 TO WS-PGE-LENGTH
				    L02-PGE-LENGTH.
	     MOVE TRM-SINGLE(WS-S6)
				 TO WS-PAPER.
	   IF TRM-PRN-TYPE(WS-S6) = "H"
	       MOVE 77		 TO WS-SCREEN-LIN
	       MOVE WS-COND	 TO WS-ECAN
	       MOVE 048		 TO W08-DEC(4)
	       MOVE 056		 TO W08-DEC(5)
	       MOVE W08-EXPD	 TO WS-EXPP.
	   IF WS-PRINTER = "D"
	       MOVE "WS"	 TO W02-WS
	       MOVE "_"		 TO W02-US W02-US2
	       MOVE LS-USER	 TO W02-TERM
	       MOVE WS-REPORT	 TO W02-REP
	       MOVE "."		 TO W02-P
	       MOVE LS-SYS-ID	 TO W02-USER
	       MOVE 9		 TO WS-PRN-NO.
      *
      *      Is the printer LPT1, 2 or 3
      *
	   IF WS-PRN-NO < 4
	       PERFORM CHECK-PRINTER THRU CHECK-PRINTER-EXIT
	       IF WS-ERROR = 1
		   MOVE "D"	TO WS-OPTION
		   GO TO OPEN-PRINTER-NO
	       END-IF
	   ELSE
	       MOVE "Waiting for printer response"
				 TO WS-ERR-MES
	       PERFORM OPT-CURSOR THRU OPT-REPLY.
	   IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
	       COMPUTE WS-PGE-LENGTH = WS-PGE-LENGTH / 6 * 8
	       MOVE WS-PGE-LENGTH
				 TO L02-PGE-LENGTH
	       IF WS-PGE-LENGTH > 88
		   SUBTRACT 7 FROM WS-PGE-LENGTH
				 GIVING WS-PRN-LENGTH
					L02-PRN-LENGTH
	       ELSE
		   SUBTRACT 6 FROM WS-PGE-LENGTH
				 GIVING WS-PRN-LENGTH
					L02-PRN-LENGTH.
	   IF NOT(WS-LINAGE = ZERO OR WS-PGE-LENGTH)
	       COMPUTE WS-PRN-LENGTH = WS-LINAGE -
				      (WS-PGE-LENGTH - WS-PRN-LENGTH)
	       MOVE WS-LINAGE	 TO WS-PGE-LENGTH L02-PGE-LENGTH
	       MOVE WS-PRN-LENGTH
				 TO L02-PRN-LENGTH.
	   IF WS-PRINTER = "D"
	       OPEN EXTEND PRNSPL
	   ELSE
019890	       OPEN OUTPUT PRNREP.
	     PERFORM RESTORE-SCREEN.
	     CALL X"91" USING X91-RES X91-FUN PRNSPL.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	   IF WS-PRINTER = "D"
	       MOVE SPACES	 TO SPL-DETAIL1
	       MOVE WS-PGE-LENGTH
				 TO SPL-LIN
	       MOVE ZERO	 TO SPL-ADV
	       WRITE SPL-L1 BEFORE 0 LINES
	       MOVE ZERO	 TO SPL-LIN.
	   IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
	       MOVE WS-8LPI	 TO REP-DETAIL1
	       IF WS-PRINTER = "D"
		   MOVE ZERO	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
011380		   WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	   IF (WS-PGE-LENGTH > 66 AND < 88) OR
	      (WS-PGE-LENGTH > 88)
	       IF WS-PRINTER = "D"
		   MOVE 2	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
		   WRITE REP-LINE1 BEFORE 2 LINES
	       END-IF
	   ELSE
	       IF WS-PRINTER = "D"
		   MOVE ZERO	 TO SPL-ADV
		   MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		   WRITE SPL-L1
	       ELSE
		   WRITE REP-LINE1 BEFORE 0 LINES.
	     GO TO OPEN-PRINTER-STATUS.

       OPEN-REPORT-1.
	   IF TRM-PICP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN1(WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-2.
	   IF TRM-INVP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-INVP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-INVP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-INVP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN2(WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-3.
	   IF TRM-STDP(WS-S6) = 1 OR 9
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-STDP(WS-S6) = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STDP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN3(WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-4.
	   IF TRM-STMP(WS-S6) = 1
	       MOVE "LPT1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 2
	       MOVE "LPT2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 3
	       MOVE "LPT3"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
	   IF TRM-STMP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN4(WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-5.
	   IF TRM-CSHP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-CSHP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-CSHP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-CSHP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN5(WS-S6)
                                 GIVING W25-RESULT.

       OPEN-REPORT-6.
	   IF TRM-POS(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-POS(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN6(WS-S6)
                                 GIVING W25-RESULT.


       CHECK-PRINTER.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO CHECK-PRINTER-EXIT.

       PRINTER-NOT-READY.
	     MOVE "Check printer! - 'R'etry or 'A'bort  [ ] "
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "R" OR "A".
	   IF WS-OPTION = "R"
	       GO TO CHECK-PRINTER.
	     MOVE 1		 TO WS-ERROR.

       CHECK-PRINTER-EXIT.
	     EXIT.

       OPEN-SCREEN.
	     MOVE 1		 TO WS-S8.

       OPEN-SCREEN-LOOP.
	     MOVE SPACES	 TO W05-LINE(WS-S8).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S8
	       GO TO OPEN-SCREEN-LOOP.
	     MOVE ZERO		 TO WS-SCREEN-LIN.
	     MOVE 1		 TO WS-SCREEN
				    WS-LEFT.
	     MOVE 78		 TO WS-RIGHT.

       OPEN-SCREEN-EXIT.
	     EXIT.

       OPEN-PRINTER-STATUS.
	     MOVE SPACES	 TO L02-PRN-LINE.
	     MOVE "O"		 TO L02-PRN-STATUS.
	   IF L02-LINAGE = 99
	       CLOSE PARAM.

       OPEN-PRINTER-EXIT.
	     EXIT.

       CLOSE-PRINT-FILE  SECTION 51.
       CLOSE-PRINTER.
	   IF WS-PRINTER = "S"
	       GO TO CLOSE-PRINTER-STATUS.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-6LPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
	   IF WS-PRINTER = "D"
	       CLOSE PRNSPL
	   ELSE
	       CLOSE PRNREP.

       CLOSE-PRINTER-STATUS.
	     MOVE "C"		 TO L02-PRN-STATUS.

       CLOSE-PRINTER-EXIT.
	     EXIT.
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
       NEW-PAGE        SECTION 52.
       SKIP-TO-NEW-PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   I G N O R E	S K I P   I F	S C R E E N   I S
      * 	  S E L E C T E D
      *
	   IF WS-PRINTER = "S"
	       GO TO NEW-PAGE-EXIT.
	   IF WS-PRINTER = "D"
	       MOVE "PAGE"	 TO SPL-CONT
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	       MOVE ZERO	 TO SPL-LIN
	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
		   IF WS-PRINTER = "D"
		       MOVE 2	 TO SPL-ADV
		       MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		       WRITE SPL-L1
		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       NEW-PAGE-EXIT.
	    EXIT.

       LINE-PRINT-SPOOL	 SECTION 53.
       PRINT-A-LINE.
	   IF WS-ADVANCE = 0
	       PERFORM PRINT-ZERO
	   ELSE
	   IF WS-ADVANCE = 1
	       PERFORM PRINT-SINGLE
	   ELSE
	   IF WS-ADVANCE = 2
	       PERFORM PRINT-DOUBLE
	   ELSE
	   IF WS-ADVANCE = 99
	       PERFORM PRINT-PAGE
	   ELSE
	       MOVE WS-ADVANCE	 TO WS-LINES
	       PERFORM PRINT-ADVANCE.
	     MOVE LINAGE-COUNTER TO L02-LINAGE.
	     GO TO LINE-PRINT-EXIT.

       PRINT-ZERO.
	   IF L02-PRN-TYPE = "C"
	       MOVE WS-COND	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "E"
	       MOVE WS-EXPP	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "X"
	       MOVE WS-ECAN	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "1"
	       MOVE WS-10CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "2"
	       MOVE WS-12CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "3"
	       MOVE WS-17CPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "6"
	       MOVE WS-6LPI	 TO REP-DETAIL1
	   ELSE
	   IF L02-PRN-TYPE = "8"
	       MOVE WS-8LPI	 TO REP-DETAIL1.
	   IF WS-PRINTER = "D"
	       MOVE ZERO	 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 0 LINES.

       PRINT-SINGLE.
	   IF WS-PRINTER = "D"
	       MOVE 1		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.

       PRINT-DOUBLE.
	   IF WS-PRINTER = "D"
	       MOVE 2		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-PAGE.
	   IF WS-PRINTER = "D"
	       MOVE "PAGE"	 TO SPL-CONT
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	       MOVE ZERO	 TO SPL-LIN
	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
		   MOVE SPACES	 TO REP-DETAIL1
		   IF WS-PRINTER = "D"
		       MOVE 2	 TO SPL-ADV
		       MOVE REP-DETAIL1
				 TO SPL-DETAIL1
		       WRITE SPL-L1
		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-ADVANCE.
	   IF WS-PRINTER = "D"
	       MOVE 1		 TO SPL-ADV
	       MOVE REP-DETAIL1  TO SPL-DETAIL1
	       WRITE SPL-L1
	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     SUBTRACT 1		 FROM WS-LINES.
	   IF WS-LINES > ZERO
	       GO TO PRINT-ADVANCE.

       LINE-PRINT-EXIT.
	     EXIT.

       SCREEN-PRINT    SECTION 54.
       SCREEN-PRINT-INIT.
	     MOVE WS-SCREEN-LIN  TO WS-S8.
	   IF WS-SCREEN-LIN < 20
	       ADD 1		 TO WS-S8
	   ELSE
	       GO TO SCREEN-PRINT-SCROLL.

       DISPLAY-A-PRINT-LINE.
	     MOVE L02-PRN-TYPE	 TO W05-TYPE(WS-S8).
	     MOVE L02-PRN-LINE	 TO W05-DETAIL(WS-S8).
	   IF WS-LEFT > 1 AND < 60
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP
	   ELSE
	   IF WS-SCREEN = 1
	       DISPLAY SD-LEFT
	   ELSE
	       DISPLAY SD-RIGHT.
	     GO TO SCREEN-PRINT-CHECK.

       SCREEN-SHIFT-LEFT.
	   IF WS-LEFT > 1
	       SUBTRACT 1	 FROM WS-LEFT
				      WS-RIGHT.
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-RIGHT.
	   IF WS-RIGHT < 136
	       ADD 1		 TO WS-LEFT
				    WS-RIGHT.
	       PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-DATA.
	     MOVE 1		 TO WS-S1 WS-S2.
	     MOVE WS-LEFT	 TO WS-S3.

       SCREEN-SHIFT-LOOP.
	     MOVE W05-CHAR(WS-S1, WS-S3)
				 TO W06-CHAR(WS-S1, WS-S2).
	   IF WS-S2 < 78
	       ADD 1		 TO WS-S2 WS-S3
	       GO TO SCREEN-SHIFT-LOOP.
	   IF WS-S1 < 20
	       ADD 1		 TO WS-S1
	       MOVE 1		 TO WS-S2
	       MOVE WS-LEFT	 TO WS-S3
	       GO TO SCREEN-SHIFT-LOOP.
	     DISPLAY SD-RANDOM.

       SCREEN-PRINT-SCROLL.
	     MOVE 1		 TO WS-S7.
	     MOVE 2		 TO WS-S8.

       SCREEN-PRINT-LOOP.
	     MOVE W05-LINE(WS-S8)
				 TO W05-LINE(WS-S7).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S7 WS-S8
	       GO TO SCREEN-PRINT-LOOP.
	     GO TO DISPLAY-A-PRINT-LINE.

       SCREEN-PRINT-CHECK.
	     MOVE WS-S8 	 TO WS-SCREEN-LIN.
	     MOVE SPACES	 TO L02-PRN-LINE.
	   IF WS-COMMAND = "E"
	       GO TO SCREEN-CONTROL.
	   IF (WS-S8 = 20) AND (LS-ADVANCE = WS-ADVANCE)
	       PERFORM SCREEN-CONTROL THRU SCREEN-CONTROL-EXIT
	       IF (WS-COMMAND = "Z") OR (WS-ADVANCE = 1)
		   GO TO SCREEN-PRINT-EXIT.
	   IF WS-ADVANCE > 1
	       SUBTRACT 1	 FROM WS-ADVANCE
	       GO TO SCREEN-PRINT-INIT.
	   IF WS-SCREEN-LIN < 20
	       GO TO SCREEN-PRINT-EXIT.

       SCREEN-CONTROL.
	     DISPLAY "L" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "eft, "
		     "R" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "ight, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     X"1B201A" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "ext, " WITH FOREGROUND-COLOR 1
				  BACKGROUND-COLOR 3
		     "P" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 3
		     "age or " WITH FOREGROUND-COLOR 1
				    BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       SCREEN-CONTROL-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "Z"	 TO WS-COMMAND
		     GO TO SCREEN-CONTROL-EXIT
		 WHEN PAGE-DOWN
		     GO TO SCREEN-CLEAR
		 WHEN OTHER	 CALL X"E5"
		     GO TO SCREEN-CONTROL-OPT
	       END-EVALUATE
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN LEFT-KEY
		     PERFORM SCREEN-SHIFT-LEFT
		     GO TO SCREEN-CONTROL-OPT
		 WHEN RIGHT-KEY
		     PERFORM SCREEN-SHIFT-RIGHT
		     GO TO SCREEN-CONTROL-OPT
		 WHEN DOWN-KEY
		     GO TO SCREEN-CONTROL-EXIT
		 WHEN OTHER	 CALL X"E5"
		     GO TO SCREEN-CONTROL-OPT
	   ELSE
	       GO TO SCREEN-CONTROL-OPT.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT(WS-OPTION = "L" OR "N" OR "R" OR "P")
	       GO TO SCREEN-CONTROL-OPT.
	   IF WS-OPTION = "R"
	       MOVE 2		 TO WS-SCREEN
	       MOVE 59		 TO WS-LEFT
	       MOVE 136 	 TO WS-RIGHT
	   ELSE
	   IF WS-OPTION = "L"
	       MOVE 1		 TO WS-SCREEN
				    WS-LEFT
	       MOVE 78		 TO WS-RIGHT
	   ELSE
	   IF WS-OPTION = "P"
	       GO TO SCREEN-CLEAR
	   ELSE
	       DISPLAY CLEAR-L25
	       GO TO SCREEN-CONTROL-EXIT.
	   IF WS-SCREEN = 1
	       DISPLAY SD-LEFT
	   ELSE
	       DISPLAY SD-RIGHT.
	     GO TO SCREEN-CONTROL-OPT.

       SCREEN-CLEAR.
	     MOVE 1		 TO WS-S8.

       SCREEN-CLEAR-LOOP.
	     MOVE SPACES	 TO W05-LINE(WS-S8).
	   IF WS-S8 < 20
	       ADD 1		 TO WS-S8
	       GO TO SCREEN-CLEAR-LOOP.
	     MOVE ZERO		 TO WS-SCREEN-LIN.

       SCREEN-CONTROL-EXIT.
	     EXIT.

       SCREEN-PRINT-EXIT.
	     EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 91.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.

       COPY DISPERR.PRO.
