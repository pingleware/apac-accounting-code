      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   **	  **  *******	*******   **	**  *******    *
      *   **	**  **	  **  **       **     **  **	**  **	       *
      *   **	    **	  **  **       **     **  **	**  **	       *
      *   **	    ********  ****     **     **  **	**  ****       *
      *   **	    **	  **  **       **     **  **	**  **	       *
      *   **	**  **	  **  **       **   ***   **	**  **	       *
      *    ******   **	  **  *******	***** **   ******   *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C H E Q U E   P R I N T   U T I L I T Y   P R O G R A M    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CHQPRN.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   January 1997.

	   This program is used to print/spool Cheque details.

	   The program performs the following functions:

	       Open the printer.
	       Open the spool file.
	       Print a line and advance the paper as requested.
	       Spool a line and pass on the advance parameters.

	   The calling program/s treat all cheques as if they were being
	   printed on a printer and are unaware of the Users request to
	   spool the cheques.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2008
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

       01  CHQ-F1-L1.
	   03  FILLER	       PIC  X(64).
	   03  CHQ-F1-DATE     PIC  Z9/99/9999.
	   03  FILLER	       PIC  X(62).

       01  CHQ-F1-L2.
	   03  FILLER	       PIC  X(06).
	   03  CHQ-F1-PAYEE    PIC  X(50).
	   03  FILLER	       PIC  X(80).

       01  CHQ-F1-L3.
	   03  FILLER	       PIC  X(04).
	   03  CHQ-F1-MIL.
	       05  FILLER      PIC  X(02).
	       05  CHQ-MIL-V   PIC  **9.
	       05  FILLER      PIC  X(01).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-HT       PIC  X(06).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-TT       PIC  X(06).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-TH       PIC  X(06).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-HD       PIC  X(06).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-TN       PIC  X(06).
	   03  FILLER	       PIC  X(01).
	   03  CHQ-F1-UN       PIC  X(06).
	   03  FILLER	       PIC  X(02).
	   03  CHQ-F1-CC       PIC  9(02).
	   03  FILLER	       PIC  X(04).
	   03  CHQ-F1-VALUE    PIC  ***,***,**9.99.
	   03  FILLER	       PIC  X(62).
      *
      *    ****    M O R E   F O R M A T S   T O   B E	 A D D E D
      * 	   L A T E R
      *

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
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴
      -        "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "읕컴컴컴컴컴컴컴컴컴컴컴컴
      -        "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".
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
	       05  W02-CHEQUE  PIC  X(06) VALUE "CHEQUE".
	       05  W02-FORMAT  PIC  X(01) VALUE "1".
	       05  W02-EXT     PIC  X(04) VALUE ".PRN".

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

       01  W45-WORDS.
	   03  W45-VALUES.
	       05  FILLER      PIC  X(06) VALUE "  NIL ".
	       05  FILLER      PIC  X(06) VALUE "  ONE ".
	       05  FILLER      PIC  X(06) VALUE "  TWO ".
	       05  FILLER      PIC  X(06) VALUE " THREE".
	       05  FILLER      PIC  X(06) VALUE " FOUR ".
	       05  FILLER      PIC  X(06) VALUE " FIVE ".
	       05  FILLER      PIC  X(06) VALUE "  SIX ".
	       05  FILLER      PIC  X(06) VALUE " SEVEN".
	       05  FILLER      PIC  X(06) VALUE " EIGHT".
	       05  FILLER      PIC  X(06) VALUE " NINE ".
	       05  FILLER      PIC  X(06) VALUE "  TEN ".
	       05  FILLER      PIC  X(06) VALUE "ELEVEN".
	       05  FILLER      PIC  X(06) VALUE "TWELVE".
	   03  W45-AMT-WORD REDEFINES W45-VALUES.
	       05  W45-VALUE   PIC  X(06) OCCURS 13.
	   03  W45-AMOUNT      PIC  9(09)V99.
	   03  W45-AMT REDEFINES W45-AMOUNT.
	       05  W45-MIL     PIC  9(03).
	       05  W45-HT      PIC  9(01).
	       05  W45-TT      PIC  9(01).
	       05  W45-TH      PIC  9(01).
	       05  W45-HD      PIC  9(01).
	       05  W45-TN      PIC  9(01).
	       05  W45-UN      PIC  9(01).
	       05  W45-CC      PIC  9(02).

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
      *    FORMAT: 1 = Cheques - Format 1
      * 	   2 = Cheques - Format 2
      * 	   3 = Cheques - Format 3
      * 	   4 = Cheques - Format 4
      * 	   5 = Cheques - Format 5
      * 	   6 = Cheques - Format 6
      *
	   03  LS-FORMAT       PIC  9(01).
	   03  LS-SCREEN       PIC  9(01).
	   03  LS-SCREEN-LIN   PIC  9(02).
	   03  LS-LINAGE       PIC  9(02).

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
	   03  L02-PRN-TYPE	PIC  X(01).
	   03  L02-PRN-DET.
	       05  L02-PRN-DATE	PIC  9(08).
	       05  L02-PRN-PAY	PIC  X(40).
	       05  L02-PRN-VAL	PIC  9(09)V99.
	       05  FILLER	PIC  X(77).

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

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
      *
      *    ****    P A R A M E T E R   F I L E
      *
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
      *	   IF WS-COMMAND = "I"
      *	       PERFORM OPEN-PRNT THRU OPEN-PRNT-EXIT
      *	       MOVE WS-SCREEN-PRINT
      *				 TO LS-SCREEN-PRINT
      *	       GO TO AA999.
	   IF NOT (L02-PRN-STATUS = "O")
	       PERFORM SAVE-SCREEN-3.
	     PERFORM PRINT-SCREEN.
	   IF L02-PRN-STATUS = "C"
	       PERFORM RESTORE-SCREEN-3.
	     MOVE WS-SCREEN-PRINT
				 TO LS-SCREEN-PRINT.

       AA999.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

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
      *
      *    ****   O P E N   T H E   P R I N T E R   F I L E
      *
	   IF WS-COMMAND = "O"
	       IF L02-PRN-STATUS = "C"
		   PERFORM OPEN-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
	   ELSE
      *
      *    ****   C L O S E   T H E   P R I N T E R   F I L E
      *
	   IF WS-COMMAND = "C"
	       IF L02-PRN-STATUS = "O"
		   PERFORM CLOSE-PRINT-FILE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT
	   ELSE
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
	   IF WS-COMMAND = "S"
	       IF L02-PRN-STATUS = "O"
		   PERFORM NEW-PAGE
	       END-IF
	       GO TO PRINT-SCREEN-EXIT.
      *
      *    ****	  C H E C K   I F   P R I N T E R   F I L E   O P E N
      *
	   IF L02-PRN-STATUS = "C"
	       MOVE "Printer file not open"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO PRINT-SCREEN-EXIT.
	     PERFORM CHEQ-PRINT-SPOOL.

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
000360	     MOVE "P"		 TO WS-PRINTER.
	     MOVE 3		 TO WS-PARKEY.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      *    ****    A D D    O T H E R	F O R M A T S	H E R E
      *
      *	   IF WS-REPORT = 1
	       MOVE 20		 TO WS-PRN-LENGTH
				    L02-PRN-LENGTH
	       MOVE 22		 TO WS-PGE-LENGTH
				    L02-PGE-LENGTH
	       PERFORM OPEN-REPORT-1.
      *	   ELSE
	     DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
				 TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      *	     MOVE TRM-LENGTH (WS-S6)
      *				 TO WS-PRN-LENGTH
      *				    L02-PRN-LENGTH.
      *	     MOVE TRM-PAGE (WS-S6)
      *				 TO WS-PGE-LENGTH
      *				    L02-PGE-LENGTH.
	   IF TRM-PRN-TYPE (WS-S6) = "H"
	       MOVE 77		 TO WS-SCREEN-LIN
	       MOVE WS-COND	 TO WS-ECAN
	       MOVE 048		 TO W08-DEC (4)
	       MOVE 056		 TO W08-DEC (5)
	       MOVE W08-EXPD	 TO WS-EXPP.
      *    IF WS-PRINTER = "D"
      *        MOVE "WS"	 TO W02-WS
      *        MOVE "_"		 TO W02-US W02-US2
      *        MOVE LS-USER	 TO W02-TERM
      *        MOVE WS-REPORT	 TO W02-REP
      *        MOVE "."		 TO W02-P
      *        MOVE WS-SYS-ID	 TO W02-USER
      *        MOVE 9		 TO WS-PRN-NO.
	   IF WS-PRN-NO < 3
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
	   IF NOT (WS-LINAGE = ZERO OR WS-PGE-LENGTH)
	       COMPUTE WS-PRN-LENGTH = WS-LINAGE -
				      (WS-PGE-LENGTH - WS-PRN-LENGTH)
	       MOVE WS-LINAGE	 TO WS-PGE-LENGTH L02-PGE-LENGTH
	       MOVE WS-PRN-LENGTH
				 TO L02-PRN-LENGTH.
      *	   IF WS-PRINTER = "D"
      *	       OPEN EXTEND PRNSPL
      *	   ELSE
019890	       OPEN OUTPUT PRNREP.
	     PERFORM RESTORE-SCREEN.
      *	     CALL X"91" USING X91-RES X91-FUN PRNSPL.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
      *	   IF WS-PRINTER = "D"
      *	       MOVE SPACES	 TO SPL-DETAIL1
      *	       MOVE WS-PGE-LENGTH
      *				 TO SPL-LIN
      *	       MOVE ZERO	 TO SPL-ADV
      *	       WRITE SPL-L1 BEFORE 0 LINES
      *	       MOVE ZERO	 TO SPL-LIN.
	   IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
	       MOVE WS-8LPI	 TO REP-DETAIL1
      *	       IF WS-PRINTER = "D"
      *		   MOVE ZERO	 TO SPL-ADV
      *		   MOVE REP-DETAIL1
      *				 TO SPL-DETAIL1
      *		   WRITE SPL-L1
      *	       ELSE
011380		   WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	   IF (WS-PGE-LENGTH > 66 AND < 88) OR
	      (WS-PGE-LENGTH > 88)
      *	       IF WS-PRINTER = "D"
      *		   MOVE 2	 TO SPL-ADV
      *		   MOVE REP-DETAIL1
      *				 TO SPL-DETAIL1
      *		   WRITE SPL-L1
      *	       ELSE
		   WRITE REP-LINE1 BEFORE 2 LINES
      *	       END-IF
	   ELSE
      *	       IF WS-PRINTER = "D"
      *		   MOVE ZERO	 TO SPL-ADV
      *		   MOVE REP-DETAIL1
      *				 TO SPL-DETAIL1
      *		   WRITE SPL-L1
      *	       ELSE
		   WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     GO TO OPEN-PRINTER-STATUS.

       OPEN-REPORT-1.
	   IF PAR-CHQ-PRN = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF PAR-CHQ-PRN = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF PAR-CHQ-PRN = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF PAR-CHQ-PRN = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF PAR-CHQ-PRN = 5
	       MOVE "COM2"	 TO W02-PRINTER
	   ELSE
	   IF PAR-CHQ-PRN = 9
	       MOVE "CHEQS1"	 TO W02-PRINTER.
	     ADD 501 PAR-CHQ-SPC GIVING W25-RESULT.

      *
      *    ****    A D D    O T H E R	F O R M A T S	H E R E
      *

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
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "A")
	       GO TO PRINTER-NOT-READY.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO CHECK-PRINTER-EXIT.
	       GO TO CHECK-PRINTER.

       CHECK-PRINTER-EXIT.
	     EXIT.

      * OPEN-PRNT.
      *	     MOVE 1		 TO WS-S8.
      *
      * OPEN-PRNT-LOOP.
      *	     MOVE SPACES	 TO W05-LINE (WS-S8).
      *	   IF WS-S8 < 20
      *	       ADD 1		 TO WS-S8
      *	       GO TO OPEN-PRNT-LOOP.
      *	     MOVE ZERO		 TO WS-SCREEN-LIN.
      *	     MOVE 1		 TO WS-SCREEN
      *				    WS-LEFT.
      *	     MOVE 78		 TO WS-RIGHT.
      *
      * OPEN-PRNT-EXIT.
      *	     EXIT.

       OPEN-PRINTER-STATUS.
	     MOVE SPACES	 TO L02-PRN-DET.
	     MOVE "O"		 TO L02-PRN-STATUS.
	   IF L02-LINAGE = 99
	       CLOSE PARAM.

       OPEN-PRINTER-EXIT.
	     EXIT.

       CLOSE-PRINT-FILE  SECTION 51.
       CLOSE-PRINTER.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
      *	   IF WS-PRINTER = "D"
      *	       MOVE ZERO	 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE WS-6LPI	 TO REP-DETAIL1.
      *	   IF WS-PRINTER = "D"
      *	       MOVE ZERO	 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
	       WRITE REP-LINE1 BEFORE 0 LINES.
      *	   IF WS-PRINTER = "D"
      *	       CLOSE PRNSPL
      *	   ELSE
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
      *	   IF WS-PRINTER = "D"
      *	       MOVE "PAGE"	 TO SPL-CONT
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	       MOVE ZERO	 TO SPL-LIN
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
      *		   IF WS-PRINTER = "D"
      *		       MOVE 2	 TO SPL-ADV
      *		       MOVE REP-DETAIL1
      *				 TO SPL-DETAIL1
      *		       WRITE SPL-L1
      *		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       NEW-PAGE-EXIT.
	    EXIT.

       CHEQ-PRINT-SPOOL	 SECTION 53.
       PRINT-A-CHEQ.
      *	   IF WS-REPORT = 1
	       PERFORM PRINT-FORMAT-1-CHEQ
	       GO TO CHEQ-PRINT-EXIT.


      *	     MOVE LINAGE-COUNTER TO L02-LINAGE.
      *	     GO TO CHEQ-PRINT-EXIT.

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
      *	   IF WS-PRINTER = "D"
      *	       MOVE ZERO	 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE 0 LINES.

       PRINT-SINGLE.
      *	   IF WS-PRINTER = "D"
      *	       MOVE 1		 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.

       PRINT-DOUBLE.
      *	   IF WS-PRINTER = "D"
      *	       MOVE 2		 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-PAGE.
      *	   IF WS-PRINTER = "D"
      *	       MOVE "PAGE"	 TO SPL-CONT
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	       MOVE ZERO	 TO SPL-LIN
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE PAGE.
	   IF WS-COMMAND NOT = "E"
	       IF (WS-PGE-LENGTH > 66 AND < 88) OR
		  (WS-PGE-LENGTH > 88)
		   MOVE SPACES	 TO REP-DETAIL1
      *		   IF WS-PRINTER = "D"
      *		       MOVE 2	 TO SPL-ADV
      *		       MOVE REP-DETAIL1
      *				 TO SPL-DETAIL1
      *		       WRITE SPL-L1
      *		   ELSE
		       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-ADVANCE.
      *	   IF WS-PRINTER = "D"
      *	       MOVE 1		 TO SPL-ADV
      *	       MOVE REP-DETAIL1  TO SPL-DETAIL1
      *	       WRITE SPL-L1
      *	   ELSE
028700	       WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     SUBTRACT 1		 FROM WS-LINES.
	   IF WS-LINES > ZERO
	       GO TO PRINT-ADVANCE.

       CHEQ-PRINT-EXIT.
	     EXIT.

       PRINT-FORMAT-1-CHEQ   SECTION 53.
       PRINT-A-FMT-1-CHEQ.
      *	     MOVE SPACES	 TO REP-DETAIL1.
      *	     MOVE 4		 TO WS-LINES.
      *	     PERFORM PRINT-ADVANCE.
	     MOVE L02-PRN-DATE	 TO CHQ-F1-DATE.
	     MOVE 4		 TO WS-LINES.
	     PERFORM PRINT-ADVANCE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE L02-PRN-PAY	 TO CHQ-F1-PAYEE.
	     MOVE 4		 TO WS-LINES.
	     PERFORM PRINT-ADVANCE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE L02-PRN-VAL	 TO W45-AMOUNT
				    CHQ-F1-VALUE.
	     MOVE W45-CC	 TO CHQ-F1-CC.
	     MOVE W45-UN	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-UN.
	     MOVE W45-TN	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-TN.
	     MOVE W45-HD	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-HD.
	     MOVE W45-TH	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-TH.
	     MOVE W45-TT	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-TT.
	     MOVE W45-HT	 TO WS-S8.
	     ADD 1		 TO WS-S8.
	     MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-HT.
	     MOVE W45-MIL	 TO WS-S8.
	  IF WS-S8 > 12
	      MOVE "******"	 TO CHQ-F1-MIL
	      MOVE W45-MIL	 TO CHQ-MIL-V
	  ELSE
	      ADD 1		 TO WS-S8
	      MOVE W45-VALUE (WS-S8)
				 TO CHQ-F1-MIL.
	     MOVE 4		 TO WS-LINES.
	     PERFORM PRINT-ADVANCE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.

       PRINT-FORMAT-1-EXIT.
	     EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 91.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510	     GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
041490	     DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
042170       STOP RUN.
