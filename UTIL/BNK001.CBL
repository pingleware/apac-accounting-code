      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    *******   **    **  **    **    ****      ****      **      *
      *    **    **  ***   **  **   **    **  **    **  **    ***      *
      *    **    **  ****  **  **  **    **    **  **    **    **      *
      *    *******   ** ** **  *****     **    **  **    **    **      *
      *    **    **  **  ****  **  **    **    **  **    **    **      *
      *    **    **  **   ***  **   **    **  **    **  **     **      *
      *    *******   **    **  **    **    ****      ****    ******    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   B A N K   D E P O S I T   S L I P   &   P D   C H E Q U E S  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     BNK001.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   AUGUST 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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

       COPY CHEQUE.SL.

000120 COPY CONTROL.SL.

000140 COPY PARAM.SL.

000240     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000160 DATA DIVISION.
000170 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CHEQUE.FD.

000120 COPY CONTROL.FDE.

000210 COPY PARAM.FDE.

      /
000410 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
001210 01  REP-LINE1.
001220     03  REP-DETAIL1     PIC  X(80).
001250 01  REP-L1.
001270     03  R-H-R1          PIC  X(42).
001280 01  REP-L2.
001290     03  FILLER          PIC  X(26).
001300     03  R-H-R2          PIC  X(27).
           03  FILLER          PIC  X(27).
001310 01  REP-L3.
001320     03  FILLER          PIC  X(41).
001330     03  R-H-R3          PIC  X(27).
000470 01  R-L3.
000530     03  R-DATE          PIC  X(06).
000540	   03  R-DTE	       PIC  Z9/99/9999.
000500	   03  FILLER	       PIC  X(05).
000770     03  R-H5            PIC  X(40).
000800	   03  FILLER	       PIC  X(09).
000480     03  R-PAGE          PIC  X(06).
000490     03  R-P-NO          PIC  Z(03)9.
001340 01  REP-L4.
001350     03  FILLER          PIC  X(31).
001360     03  R-H-R4          PIC  X(37).
001370 01  REP-L5.
001380     03  FILLER          PIC  X(36).
001390     03  R-H-R5          PIC  X(32).
001400 01  REP-L6.
001410     03  FILLER          PIC  X(36).
001420     03  R-H-R6          PIC  X(32).
001400 01  REP-L8.
001410     03  FILLER          PIC  X(50).
001420     03  R-H-R8          PIC  X(26).
001340 01  REP-L9.
001350     03  FILLER          PIC  X(31).
001360     03  R-H-R9          PIC  X(37).
       01  REP-CD1.
	   03  R-H15.
	       05  FILLER      PIC  X(34).
	       05  R-H15A      PIC  X(08).
	   03  R-H16	       PIC  X(20).
	   03  R-H17	       PIC  X(11).
	   03  R-H18	       PIC  X(07).
       01  REP-CD2.
	   03  R-NAME2.
	       05  FILLER      PIC  X(34).
	       05  R-REF       PIC  ZZZZZ9.
	       05  FILLER      PIC  X(02).
	   03  R-TEL2	       PIC  X(16).
	   03  R-CHQ2	       PIC  Z(06)9.99-.
	   03  FILLER	       PIC  X(01).
	   03  R-DTE2	       PIC  Z9/99/9999.

000230 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000240 77  WS-S1	   PIC	9(04)	 COMP-5.
000250 77  WS-S2	   PIC	9(04)	 COMP-5.
000260 77  WS-S3	   PIC	9(04)	 COMP-5.
000270 77  WS-S4	   PIC	9(04)	 COMP-5.
000270 77  WS-S5	   PIC	9(04)	 COMP-5.
000270 77  WS-S6	   PIC	9(04)	 COMP-5.
000270 77  WS-S7	   PIC	9(04)	 COMP-5.
000270 77  WS-S8	   PIC	9(04)	 COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
000280 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001800 77  WS-PAGE         PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
       77  WS-NUM	   PIC	Z9.
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-TRAN         PIC  Z(05)9.
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
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CHEQUE.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "BNK001.".
               05  W02-USER   PIC X(05) VALUE SPACES.

002480 01  W05-REP-HEADINGS.
002550     03  W05-H-R1.
002570         05  FILLER      PIC  X(17) VALUE 
002580             "DEPOSIT SLIP FOR".
002590	       05  W05-DTE1    PIC  99/99/9999.
002840     03  W05-H-R2.
002850         05  FILLER      PIC  X(01) VALUE X"0E".
002860         05  FILLER      PIC  X(16) VALUE 
002870             "P.D. CHEQUES ON".
002770	       05  W05-DTE2    PIC  99/99/9999.
002880         05  FILLER      PIC  X(01) VALUE X"14".

       01  W10-TOTALS.
003060     03  W10-TOT1        PIC S9(09)V99 COMP-3.
003060     03  W10-TOT2        PIC S9(09)V99 COMP-3.
003060     03  W10-TOT3        PIC S9(09)V99 COMP-3.

002500 01  W10-DATES.
002510	   03  W10-DATE        PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W40-CHEQUE.
001000     03  W40-TR-NUMBER   PIC  9(06)    COMP.
	   03  W40-YMD	       PIC  9(08).
           03  W40-RYMD REDEFINES W40-YMD.
	       05  W40-CN      PIC  9(02).
               05  W40-YR      PIC  9(02).
               05  W40-MT      PIC  9(02).
               05  W40-DY      PIC  9(02).
001050     03  W40-NAME        PIC  X(24).
001060     03  W40-TEL         PIC  X(14).
           03  W40-RECEIVED REDEFINES W40-TEL.
               05  W40-REC.
		07 W40-DTE-REC PIC  9(08).
	       05  FILLER      PIC  X(06).
001070	   03  W40-DTE	       PIC  9(08).
           03  W40-RDTE REDEFINES W40-DTE.
               05  W40-DD      PIC  9(02).
               05  W40-MM      PIC  9(02).
	       05  W40-CC      PIC  9(02).
               05  W40-YY      PIC  9(02).
001080	   03  W40-VALUE       PIC S9(09)V99 COMP-3.
           03  W40-DEL         PIC  X(01).
           03  W40-COMP        PIC  X(40).

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "DEPOSIT SLIP & PD CHEQUES".

       01  S-MENU.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³     ".
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "BANK OPTIONS".
	   03	       COLUMN 47 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "     ³".
	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Print Deposit Slip ³".
	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Print P/D Cheques  ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Amend a Cheque     ³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Delete a Cheque    ³".
001470	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Add a Cheque       ³".
001570	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit               ³".
	   03  LINE 14 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
008460     03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
                                 VALUE "AMEND PD CHEQUE".
008470     03  LINE  4 COLUMN  6 VALUE "Reference Number :".
008520	   03  LINE  5 COLUMN  6 VALUE "Name              ".
008540	   03  LINE  6 COLUMN  6 VALUE "Telephone         ".
008550	   03  LINE  7 COLUMN  6 VALUE "Date Due(DDMMCCYY)".
008510	   03  LINE  8 COLUMN  6 VALUE "Amount            ".

       01  S05.
008520	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(24) USING W40-NAME AUTO.
008540	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(14) USING W40-TEL AUTO.
008550	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  99/99/9999 USING W40-DTE AUTO.
008510	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  Z(06)9.99 USING W40-VALUE AUTO.
           
       01  S06.
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  Z(05)9 USING W40-TR-NUMBER.
       01  S07.
008460     03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
                                 VALUE "DELETE PD CHEQUE".
008470     03  LINE  4 COLUMN  6 VALUE "Reference Number :".
008520	   03  LINE  5 COLUMN  6 VALUE "Name              ".
008540	   03  LINE  6 COLUMN  6 VALUE "Telephone         ".
008550	   03  LINE  7 COLUMN  6 VALUE "Date Due(DDMMCCYY)".
008510	   03  LINE  8 COLUMN  6 VALUE "Amount            ".

       01  S08.
008460     03  LINE  2 COLUMN 33 FOREGROUND-COLOR 15
                                 VALUE "ADD A PD CHEQUE".
008470     03  LINE  4 COLUMN  6 VALUE "Reference Number :".
008520	   03  LINE  5 COLUMN  6 VALUE "Name              ".
008540	   03  LINE  6 COLUMN  6 VALUE "Telephone         ".
008550	   03  LINE  7 COLUMN  6 VALUE "Date Due(DDMMCCYY)".
008510	   03  LINE  8 COLUMN  6 VALUE "Amount            ".

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
008250       PERFORM ZA000-INIT.
008260     IF WS-STATUS NOT = "00"
008270         PERFORM OPEN-ERROR.
	     PERFORM BA000.
008280	     GO TO AZ000-EOJ.

       COPY LOCKED.REC.

       COPY FUNCTION.CRT.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY DATE.CNV.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CHEQUE.RD.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CHEQUE.WR.

       COPY CONTROL.WR.

      /
043590 AZ000-END               SECTION.
043600 AZ000-EOJ.
043610	     CLOSE CHEQUE
		   PRNREP.

       AZ999.
             EXIT PROGRAM.

      /
      *    ****    P R O G R A M   M E N U / C O N T R O L
      *
       BA000           SECTION.
       BA00.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < 0 OR > 5
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	   IF WS-OPTION = "0"
	       GO TO BA999.
	   IF WS-OPTION = "1"
	       PERFORM CB000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM CC000
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM CD000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM CE000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM CF000.
	     GO TO BA00.
       BA999.
	     EXIT.
      /
      *    ****    D A I L Y   C H E Q U E   D E P O S I T   L I S T
      *
       CB000	       SECTION 5.
       CB00.
035100       DISPLAY CLR-SCREEN.
035110       DISPLAY "Busy printing the Cheque Deposit Slip" AT 1012 
035120                WITH FOREGROUND-COLOR 14.
             DISPLAY "Transaction Number :" AT 1212.
             MOVE 7              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO W10-TOT1 W10-TOT2 W10-TOT3.
	     MOVE 1		 TO CHQ-TR-NUMBER.
             PERFORM START-CHEQUE-AT-TR-NO THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CB999.
	     MOVE ZERO		 TO WS-PAGE.
             MOVE W12-TODAY      TO W05-DTE1.
       CB05.
	     ADD 1		 TO WS-PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE W05-H-R1	 TO R-H-R2.
             WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "Page :"       TO R-PAGE.
             MOVE WS-PAGE        TO R-P-NO. 
             WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "NAME"	 TO R-H15.
	     MOVE "TELEPHONE"	 TO R-H16.
	     MOVE "AMOUNT"	 TO R-H17.
             WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE ALL "-"	 TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
       CB10.
             PERFORM READ-CHEQUE-NEXT THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CB15.
	     MOVE CHQ-TR-NUMBER  TO WS-TRAN.
             DISPLAY WS-TRAN AT 1233 WITH FOREGROUND-COLOR 15.
           IF CHQ-VALUE = ZERO
               GO TO CB10.
	   IF PAR-CASH-BYMD < CHQ-YMD
               GO TO CB10.
             MOVE CHQ-NAME       TO R-NAME2.
             MOVE CHQ-TEL        TO R-TEL2.
             MOVE CHQ-VALUE      TO R-CHQ2.
             ADD CHQ-VALUE       TO W10-TOT1 W10-TOT2 W10-TOT3.
	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 4)
               WRITE REP-LINE1 BEFORE 1 LINES
           ELSE
               WRITE REP-LINE1 BEFORE 2 LINES
               PERFORM CB12.
             MOVE SPACES         TO REP-DETAIL1.
             MOVE "D"            TO CHQ-DEL.
             PERFORM REWRITE-CHEQUE THRU WRITE-CHEQUE-EXIT.
             GO TO CB10.
       CB12.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "       Page Total" TO R-NAME2.
	     MOVE W10-TOT1	 TO R-CHQ2.
	     MOVE ZERO		 TO W10-TOT1.
             WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "***    Sub Total    ***" TO R-NAME2.
	     MOVE W10-TOT2	 TO R-CHQ2.
             WRITE REP-LINE1 BEFORE PAGE.
             PERFORM CB05.
       CB15.
             WRITE REP-LINE1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "       Page Total"
				 TO R-NAME2.
	     MOVE W10-TOT1	 TO R-CHQ2.
	     MOVE ZERO		 TO W10-TOT1.
             WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "****** TOTAL DEPOSIT ******" TO R-NAME2.
	     MOVE W10-TOT3	 TO R-CHQ2.
             WRITE REP-LINE1 BEFORE PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.
             DISPLAY "Deleting cheques that have been printed" AT 1412.
             MOVE 1              TO CHQ-TR-NUMBER.
             PERFORM START-CHEQUE-AT-TR-NO THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CB999.
       CB20.
             PERFORM READ-CHEQUE-NEXT THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CB999.
	     MOVE CHQ-TR-NUMBER  TO WS-TRAN.
             DISPLAY WS-TRAN AT 1233 WITH FOREGROUND-COLOR 15.
           IF CHQ-DEL NOT = "D"
               GO TO CB20.
             PERFORM DELETE-CHEQUE-REC THRU WRITE-CHEQUE-EXIT.
             GO TO CB20.
       CB999.
             EXIT.

      /
      *    ****    D A I L Y   P O S T   D A T E D   C H E Q U E S 
      *
       CC000	       SECTION 5.
       CC00.
035100       DISPLAY CLR-SCREEN.
035110       DISPLAY "Busy printing the P.D. Cheques List" AT 1012 
035120                WITH FOREGROUND-COLOR 14.
             DISPLAY "Transaction Number :" AT 1212.
	     MOVE ZERO		 TO W10-TOT1 W10-TOT2 W10-TOT3.
	     MOVE 1		 TO CHQ-TR-NUMBER.
             PERFORM START-CHEQUE-AT-TR-NO THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CC999.
	     MOVE ZERO		 TO WS-PAGE.
             MOVE W12-TODAY      TO W05-DTE2.
       CC05.
             ADD 1               TO WS-PAGE.             
             MOVE SPACES         TO REP-DETAIL1.
             MOVE W05-H-R2       TO R-H-R2.
             WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE SPACES         TO REP-DETAIL1.
             MOVE "Page :"       TO R-PAGE.
             MOVE WS-PAGE        TO R-P-NO. 
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL1.
	     MOVE "NAME"	 TO R-H15.
	     MOVE "REF NO"	 TO R-H15A.
             MOVE "TELEPHONE"    TO R-H16.
             MOVE "AMOUNT"       TO R-H17.
             MOVE "DATE"         TO R-H18.
             WRITE REP-LINE1 BEFORE 1 LINES.
             MOVE SPACES         TO REP-DETAIL1.
             MOVE ALL "-"        TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 1 LINES.
             MOVE SPACES         TO REP-DETAIL1.
       CC10.
             PERFORM READ-CHEQUE-NEXT THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
               GO TO CC15.
             MOVE CHQ-TR-NUMBER  TO WS-TRAN.
             DISPLAY WS-TRAN AT 1233 WITH FOREGROUND-COLOR 15.
	     MOVE CHQ-NAME	 TO R-NAME2.
	     MOVE CHQ-TR-NUMBER  TO R-REF.
	     MOVE CHQ-TEL	 TO R-TEL2.
	     MOVE CHQ-VALUE	 TO R-CHQ2.
	     ADD CHQ-VALUE	 TO W10-TOT1 W10-TOT2 W10-TOT3.
	     MOVE CHQ-DTE	 TO R-DTE2.
	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 4)
               WRITE REP-LINE1 BEFORE 1 LINES
           ELSE
               WRITE REP-LINE1 BEFORE 2 LINES
               PERFORM CC12.
	     MOVE SPACES	 TO REP-DETAIL1.
             GO TO CC10.
       CC12.
	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "       Page Total"
				 TO R-NAME2.
	     MOVE W10-TOT1	 TO R-CHQ2.
	     MOVE ZERO		 TO W10-TOT1.
             WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "***    Sub Total    ***" TO R-NAME2.
	     MOVE W10-TOT2	 TO R-CHQ2.
             WRITE REP-LINE1 BEFORE PAGE.
             PERFORM CC05.
       CC15.
             WRITE REP-LINE1 BEFORE 1 LINES.
             MOVE SPACES TO REP-DETAIL1.
	     MOVE "       Page Total"
				 TO R-NAME2.
	     MOVE W10-TOT1	 TO R-CHQ2.
	     MOVE ZERO		 TO W10-TOT1.
             WRITE REP-LINE1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
             MOVE "****** TOTAL P.D. CHEQUES ******" TO R-NAME2.
	     MOVE W10-TOT3	 TO R-CHQ2.
             WRITE REP-LINE1 BEFORE PAGE.
	     MOVE SPACES	 TO REP-DETAIL1.
       CC999.
             EXIT.

      /
      *    ****    A M E N D   P   D   C H E Q U E 
      *
       CD000	       SECTION 5.
       CD00.
035100       DISPLAY CLR-SCREEN.
             DISPLAY S04.
             INITIALIZE W40-CHEQUE.
	     DISPLAY S05.
       CD05.
             ACCEPT S06.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CD999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CD05
	       END-EVALUATE.
           IF W40-TR-NUMBER = ZERO
               GO TO CD999.
	     DISPLAY CLEAR-L25.
             MOVE W40-TR-NUMBER  TO CHQ-TR-NUMBER.
             PERFORM READ-CHEQUE-LOCK THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CD05.
             MOVE CHQ-RECORD2    TO W40-CHEQUE.
	     MOVE W40-YMD	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT
	     MOVE W22-DTE3	 TO W40-DTE.

       CD10.
             ACCEPT S05.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     UNLOCK CHEQUE
		     GO TO CD00
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CD10
	       END-EVALUATE.
	   IF W40-NAME = SPACES
	       MOVE "Name may not be spaces"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CD10.
           IF W40-TEL = SPACES
	       MOVE "Telephone may not be spaces"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CD10.
             MOVE W40-DTE        TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR NOT = ZERO
	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CD10.
           IF W40-VALUE = ZERO
	       MOVE "Cheque value may not be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CD10.
             MOVE W22-DTE1       TO W40-YMD.
             MOVE W40-CHEQUE     TO CHQ-RECORD2.
             PERFORM REWRITE-CHEQUE-UNLOCK THRU WRITE-CHEQUE-EXIT.
             GO TO CD00.
       CD999.
             EXIT.
      /
      *    ****    D E L E T E   P   D   C H E Q U E 
      *
       CE000	       SECTION 5.
       CE00.
035100       DISPLAY CLR-SCREEN.
             DISPLAY S07.
             INITIALIZE W40-CHEQUE.
	     DISPLAY S05.
       CE05.
             ACCEPT S06.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CE999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CE05
	       END-EVALUATE.
	   IF W40-TR-NUMBER = ZERO
               GO TO CE999.
	     MOVE W40-TR-NUMBER  TO CHQ-TR-NUMBER.
             PERFORM READ-CHEQUE-LOCK THRU READ-CHEQUE-EXIT.
           IF WS-F-ERROR = 36
	       MOVE "No Record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CE05.
             MOVE CHQ-RECORD2    TO W40-CHEQUE.
             MOVE W40-YMD        TO W22-DTE2.
             MOVE W22-DD2        TO W22-DD3.
             MOVE W22-MM2        TO W22-MM3.
             MOVE W22-YY2        TO W22-YY3.
             MOVE W22-DTE3       TO W40-DTE.
             DISPLAY S05.
       CE10.
	     PERFORM CHECK-CORRECT.
024270	   IF WS-OPTION = "N"
               UNLOCK CHEQUE
024290         GO TO CE00.
022680	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
             PERFORM DELETE-CHEQUE-REC THRU WRITE-CHEQUE-EXIT.
             GO TO CE00.
       CE999.
             EXIT.
      /
      *    ****    A D D   A   P   D   C H E Q U E 
      *
       CF000	       SECTION 5.
       CF00.
035100       DISPLAY CLR-SCREEN.
             DISPLAY S08.
             INITIALIZE W40-CHEQUE.
	     DISPLAY S05.
       CF05.
             ACCEPT S05.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CF999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CF05
	       END-EVALUATE.
	   IF W40-NAME = SPACES
	       MOVE "Name may not be spaces"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CF05.
           IF W40-TEL = SPACES
	       MOVE "Telephone may not be spaces"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CF05.
             MOVE W40-DTE        TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR NOT = ZERO
	       MOVE "Invalid date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CF05.
           IF W40-VALUE = ZERO
	       MOVE "Cheque value may not be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO CF05.
             MOVE W22-DTE1       TO W40-YMD.
012010       MOVE 1              TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE ZERO           TO CHQ-TR-NUMBER.
025530     IF DEB-RECEIPT = 99999999
025540         MOVE 1            TO DEB-RECEIPT
025550     ELSE
025560         ADD 1             TO DEB-RECEIPT.
             MOVE DEB-RECEIPT    TO W40-TR-NUMBER.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             MOVE W40-CHEQUE     TO CHQ-RECORD2.
             PERFORM WRITE-CHEQUE THRU WRITE-CHEQUE-EXIT.
             GO TO CF00.
       CF999.
             EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
	   ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
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
                                 TO WS-EXPP.
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
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
040400	     OPEN I-O CHEQUE.
	   IF WS-STATUS = "05"
	       CLOSE CHEQUE
	       OPEN OUTPUT CHEQUE
	       CLOSE CHEQUE
	       OPEN I-O CHEQUE.
040410     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 36       TO WS-F-ERROR
                   GO TO ZA50.
040430     IF WS-STATUS NOT = "00"
040440         MOVE 36           TO WS-F-ERROR
040450         GO TO ZA999-EXIT.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W40-COMP.
021680       MOVE 32             TO WS-S1.
021690 ZA05.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
042780	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
044780	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
041850     IF WS-F-ERROR = 36
041860         MOVE "PD CHEQUE file"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
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
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	     MOVE "00"		 TO WS-STATUS.
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
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 8.
      
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
041510       GO TO DISPLAY-FILE-NAME.

       I-O-ERROR-EXIT.
             EXIT.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041720     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 36
041860         MOVE W02-CHEQUE   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE CHQ-TR-NUMBER
                                 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
