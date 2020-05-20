      $set LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	********  **	 **  *******   *
      *   **	**  **	      **    **	**	   **	**   **	   **  *
      *   **	    **	      **    **	**	    ** **    **	   **  *
      *   **	    **	      *******	*****	     ***     *******   *
      *   **   ***  **	      **	**	    ** **    **	       *
      *   **	**  **	      **	**	   **	**   **	       *
      *    ******   ********  **	********  **	 **  **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G E N E R A L   L E D G E R   E X P O R T	P R O G R A M  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     GLPEXP.
000030 AUTHOR.	       James William Lemmon.
000040 DATE-WRITTEN.   SEPTEMBER 1992.

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
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY PARAM.SL.

000200 COPY GACCNT.SL.

000200	   SELECT GACDET  ASSIGN W02-GACDET
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY PARAM.FDE.

000270 COPY GACCNT.FDE.

       FD  GACDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
	   03  EXP-DATA     PIC  X(1024).

      /
000350 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	    PIC 9(04)	  COMP-5.
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
001860 77  WS-PARKEY	    PIC 9(06)	  COMP-5.
001900 77  WS-RECOVER	    PIC 9(06)	  COMP-5.
001940 77  WS-MONTHS	    PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001970 77  WS-KSTORE        PIC 9(04)     COMP-5.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-ETYPE	    PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002080 77  WS-IND           PIC 9(01)     VALUE 0.
002090 77  WS-INDB          PIC 9(01)     VALUE 0.
002100 77  WS-INDP          PIC 9(01)     VALUE 0.
002110 77  WS-INDG	    PIC 9(01)	  VALUE 0.
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002140 77  WS-CONREC        PIC 9(01) VALUE ZERO.
002200 77  WS-COMP          PIC 9(01).
002210 77  WS-WEEK          PIC 9(01).
002220 77  WS-TYPE          PIC X(01).
002260 77  WS-BATCH         PIC 9(05).
002270 77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
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
	   03  WS-MDGL-LNE REDEFINES WS-MID-LNE.
	       05  WS-MGL1     PIC X(01).
	       05  WS-MGL78    PIC X(78).
	       05  WS-MGL80    PIC X(01).
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

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

000360 COPY GACCNT.ID.

       COPY PARAM.ID.

       01  W02-FILE-IDS.
	   03  W02-GACDET.
	       05  W02-EXPORT PIC X(64) VALUE "GLEDGER.TXT".

       COPY W12.WS.

003280 01  W15-RECORD1.
002670	   03  W15-ACCOUNT PIC	9(06).
002680     03  W15-ACNO1   REDEFINES W15-ACCOUNT.
002690         05  W15-ACNO
002700                     PIC 9(04).
002710         05  W15-SER PIC  9(02).
002720     03  W15-ACNO2   REDEFINES W15-ACCOUNT.
002730         05  W15-GR  PIC 9(02).
002740         05  W15-SGRP
002750                     PIC 9(02).
002760         05  W15-ASER
002770                     PIC 9(02).
002780     03  W15-ACNO3   REDEFINES W15-ACCOUNT.
002790         05  W15-GROUP
002800                     PIC 9(02).
002810         05  W15-REST
002820                     PIC 9(04).
002830     03  W15-ACNO4   REDEFINES W15-ACCOUNT.
002840         05  W15-GRP PIC  9(01).
002850         05  FILLER  PIC  9(05).

       COPY W20.WS.

004310 01  W25-CALCS.
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

003420 01  W25-EDIT.
003430     03  W25-7.
003440         05  W25-1   PIC  ZZZZZZZZ9-.
003450     03  W25-S7V2.
003460         05  W25-V1  PIC  Z(07)9.99CR.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC Z(07)9.99CR.
003470	   03  W25-DATE.
003480	       05  W25-DTE PIC	Z9/99/9999.
003490     03  W25-REF.
003500         05  W25-REFER
003510                     PIC Z(07).
003520     03  W25-RECKEY  PIC  Z(04)9.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       01  W55-EXPORT.
           03  W55-S1          PIC  9(04) COMP-5.
           03  W55-EXP-DATA.
               05  W55-EXP     PIC  X(01) OCCURS 450.

003820 01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
003830     03  W60-FNO         PIC  9(02) COMP-5 OCCURS 49.

003850 01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
003860     03  W65-FLDS.
003870         05  FILLER      PIC X(21) VALUE "Account Number".
003870	       05  FILLER      PIC X(21) VALUE "Account Name".
003870	       05  FILLER      PIC X(21) VALUE "Budget Indicator".
003870	       05  FILLER      PIC X(21) VALUE "Budget Percentage".
003870	       05  FILLER      PIC X(21) VALUE "Jan Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Feb Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Mar Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Apr Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "May Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Jun Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Jul Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Aug Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Sep Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Oct Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Nov Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Dec Budget Amount".
003870	       05  FILLER      PIC X(21) VALUE "Jan Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Feb Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Mar Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Apr Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "May Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Jun Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Jul Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Aug Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Sep Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Oct Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Nov Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Dec Closing Balance".
003870	       05  FILLER      PIC X(21) VALUE "Dt Current Period".
003870	       05  FILLER      PIC X(21) VALUE "Cr Current Period".
003870	       05  FILLER      PIC X(21) VALUE "Account Balance".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Jan".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Feb".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Mar".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Apr".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous May".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Jun".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Jul".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Aug".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Sep".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Oct".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Nov".
003870	       05  FILLER      PIC X(21) VALUE "Balance Previous Dec".
	   03  W65-RFLDS REDEFINES W65-FLDS.
	       05  W65-FLD     PIC  X(21) OCCURS 43.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-GNO	      PIC 9(06) COMP-5
                              OCCURS 18.

003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000	       05  W80-DATE   PIC Z9/99/9999.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
           03  W100-1.
               05  W100-N      PIC  9(01).
           03  W100-2.
               05  W100-N2     PIC  9(02).
           03  W100-3.
               05  W100-NO1    PIC  9(03).
           03  W100-9.
               05  W100-NO2    PIC  Z(08)9.
           03  W100-S9.
	       05  W100-NO3    PIC  -(09)9.
           03  W100-S9M REDEFINES W100-S9.
	       05  W100-BUD    PIC  -(09)9.
           03  W100-PRICE.
               05  W100-S6V2   PIC  Z(05)9.99.
           03  W100-S9V2.
	       05  W100-V1     PIC  -(09)9.99.
           03  W100-9V2.
               05  W100-V2     PIC  Z(08)9.99.
           03  W100-9V3.
               05  W100-V3     PIC  Z(08)9.999.
           03  W100-3V2.
               05  W100-V4     PIC  Z(02)9.99.
           03  W100-DATE.
	       05  W100-DTE    PIC  9(08).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

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
000860	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "LEDGER EXPORT PROGRAM".

       01  S-MENU.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³    ".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "EXPORT OPTIONS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " No, Name & Balance ³".
	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " All Fields 	    ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Selected Fields    ³".
001570	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Field Separator    ³".
001570	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit               ³".
	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S03.
           03  LINE  4 COLUMN  2 VALUE 
		      "Export: Account Number, Name and Balance".
       01  S04.
           03  LINE  4 COLUMN  2 VALUE "Export all the fields".
       01  S05.
           03  LINE  4 COLUMN  2 VALUE "Select the fields required".
           03  LINE  6 COLUMN  2 VALUE "Field :".
       01  S06.
           03  LINE  8 COLUMN  2 VALUE "Fields:".
	   03	       COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (1).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (2).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (3).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (4).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (5).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (6).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (7).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (8).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (9).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (10).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (11).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (12).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (13).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (14).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (15).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (16).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (17).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (18).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (19).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (20).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (21).
	   03	       COLUMN 73 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (22).
	   03  LINE 10 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (23).
	   03	       COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (24).
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (25).
	   03	       COLUMN 19 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (26).
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (27).
	   03	       COLUMN 25 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (28).
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (29).
	   03	       COLUMN 31 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (30).
	   03	       COLUMN 34 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (31).
	   03	       COLUMN 37 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (32).
	   03	       COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (33).
	   03	       COLUMN 43 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (34).
	   03	       COLUMN 46 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (35).
	   03	       COLUMN 49 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (36).
	   03	       COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (37).
	   03	       COLUMN 55 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (38).
	   03	       COLUMN 58 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (39).
	   03	       COLUMN 61 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (40).
	   03	       COLUMN 64 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (41).
	   03	       COLUMN 67 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (42).
	   03	       COLUMN 70 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC ZZ FROM W60-FNO (43).

       01  S07.
           03  LINE 16 COLUMN  2 VALUE "Exporting:".
       01  S08.
	   03  LINE 16 COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(06) FROM G-AC.
	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM G-NAME.
       01  S09.
	   03  LINE  6 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 99 FROM W65-S.
	   03	       COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(21) FROM W65-FIELD.

       01  S10.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
				 VALUE "Change Field Separator".
	   03  LINE  6 COLUMN  2 VALUE "Character Used:".

       COPY LEDLUP.CRT.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA000-INIT.
003240	   IF LS0-GLLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA99.
007760	     PERFORM ZA000-INIT.
007770     IF WS-STATUS NOT = "00"
007780         PERFORM OPEN-ERROR.
	     PERFORM BA000.
007790       GO TO AZ000-EOJ.
       AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

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
      *    ³   PERFORM OPT-MESSAGE.				       ³
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
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY CLEAR.CRT.

      *
      *    ****    G E T   T H E   E X P O R T   F I L E   I D 
      *
       AA100           SECTION.
       AA101.
	     DISPLAY "Keyin the <" AT 2202
		     "pathname" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">/<"
		     "filename" WITH FOREGROUND-COLOR 6 HIGHLIGHT ">".
             DISPLAY "EXPORT file :" AT 2302.
             MOVE ZERO           TO WS-ERROR.
       AA105.
	     ACCEPT W02-EXPORT AT 2316
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA199
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA105
	       END-EVALUATE.
	     MOVE 64		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-EXPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             DISPLAY W02-EXPORT AT 2316
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO AA105.
	     DISPLAY CLEAR-L25.
	     OPEN OUTPUT GACDET.
           IF WS-STATUS = "00"
	       PERFORM AA600
               GO TO AA199.
	     MOVE "Invalid file name"
				 TO WS-ERR-STRING
003260	     PERFORM ERROR-MESSAGE.
             GO TO AA105.
       AA199.
             EXIT.
      *
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA 
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *
       AA205.
           IF W50-CCHAR (W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.     
             MOVE 1              TO W50-S1.
       AA220.
             MOVE W50-CCHAR (W50-S1)
                                 TO W55-EXP (W55-S1).
             ADD 1               TO W50-S1
                                    W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.
       AA225.
             MOVE W50-EXP        TO W55-EXP (W55-S1).
             ADD 1               TO W55-S1.
       AA299.
             EXIT.
      *
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *
       AA300              SECTION.
       AA301.
	   IF W50-BIN < 01 OR > 43
               GO TO AA399.
      *
           IF W50-BIN = 01
	       MOVE G-AC	 TO W50-DATA
	       MOVE 6		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 02
	       MOVE G-NAME	 TO W50-DATA
	       MOVE 30		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 03
	       MOVE G-IND	 TO W50-DATA
	       MOVE 1		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 04
	       MOVE G-BPER	 TO W100-V4
	       MOVE W100-3V2	 TO W50-DATA
	       MOVE 5		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 05
	       MOVE G-BVAL (1)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 06
	       MOVE G-BVAL (2)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 07
	       MOVE G-BVAL (3)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 08
	       MOVE G-BVAL (4)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 09
	       MOVE G-BVAL (5)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 10
	       MOVE G-BVAL (6)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 11
	       MOVE G-BVAL (7)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 12
	       MOVE G-BVAL (8)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 13
	       MOVE G-BVAL (9)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 14
	       MOVE G-BVAL (10)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 15
	       MOVE G-BVAL (11)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 16
	       MOVE G-BVAL (12)	 TO W100-NO3
	       MOVE W100-S9	 TO W50-DATA
	       MOVE 10		 TO W50-LENGTH
               PERFORM AA200
	       GO TO AA399.
      *
           IF W50-BIN = 17
	       MOVE G-VAL (1)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
               PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 18
	       MOVE G-VAL (2)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 19
	       MOVE G-VAL (3)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 20
	       MOVE G-VAL (4)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 21
	       MOVE G-VAL (5)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 22
	       MOVE G-VAL (6)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 23
	       MOVE G-VAL (7)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 24
	       MOVE G-VAL (8)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 25
	       MOVE G-VAL (9)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 26
	       MOVE G-VAL (10)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 27
	       MOVE G-VAL (11)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 28
	       MOVE G-VAL (12)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 29
	       MOVE G-DT	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 30
	       MOVE G-CR	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 31
	       MOVE G-BAL	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 32
	       MOVE G-LVAL (1)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 33
	       MOVE G-LVAL (2)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 34
	       MOVE G-LVAL (3)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 35
	       MOVE G-LVAL (4)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 36
	       MOVE G-LVAL (5)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 37
	       MOVE G-LVAL (6)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 38
	       MOVE G-LVAL (7)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 39
	       MOVE G-LVAL (8)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 40
	       MOVE G-LVAL (9)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 41
	       MOVE G-LVAL (10)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
               GO TO AA399.
      *
           IF W50-BIN = 42
	       MOVE G-LVAL (11)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200
	       GO TO AA399.
      *
	   IF W50-BIN = 43
	       MOVE G-LVAL (12)	 TO W100-V1
	       MOVE W100-S9V2	 TO W50-DATA
	       MOVE 13		 TO W50-LENGTH
	       PERFORM AA200.
       AA399.
               EXIT.
      *
      *    ****    P R O C E S S   T H E   S E L E C T E D   F I E L D S
      *
       AA400           SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA405.
           IF W60-FNO (W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
             PERFORM AA300.
	   IF W60-S < 43
               ADD 1             TO W60-S
               GO TO AA405.
       AA499.
             EXIT.
      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
023190       DISPLAY "Insufficient security" AT 0412
                      WITH FOREGROUND-COLOR 14.
023200       DISPLAY "Press ENTER to continue" AT 0612
                      WITH FOREGROUND-COLOR 14.
023210	     ACCEPT WS-OPTION AT 0636
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
       AA505.
             EXIT.
      *
      *    ****   H E A D E R S
      *
       AA600		  SECTION.
       AA601.
	     MOVE 1		 TO W60-S.
      *
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
       AA605.
           IF W60-FNO (W60-S) = ZERO
	       MOVE W55-EXP-DATA TO EXP-REC1
	       PERFORM WRITE-GACDET THRU WRITE-GACDET-EXIT
	       GO TO AA699.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
	     MOVE 21		 TO W50-LENGTH.
	     MOVE W65-FLD (W50-BIN)
				 TO W50-DATA.
	     PERFORM AA200.
	   IF W60-S < 43
               ADD 1             TO W60-S
	       GO TO AA605.
       AA699.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
       AA700              SECTION.
       AA701.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA705.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY PARAM.RD.

       COPY GACCNT.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       WRITE-GACDET.
000250       WRITE EXP-REC1.
000260     IF WS-STATUS NOT = "00"
000270         MOVE 99           TO WS-F-ERROR
000280         PERFORM WRITE-ERROR.
000310
000320 WRITE-GACDET-EXIT.
000330       EXIT.

       COPY GACCNT.LUP.

      /
012350 AZ000-END               SECTION.
012360 AZ000-EOJ.
	     MOVE ZERO		 TO WS-S1.

012440 AZ010.
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
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	   IF LS0-LEV < 3
	       DISPLAY "(N/A)" AT 1049
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA02.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < 0 OR > 4
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
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
	       MOVE 10		 TO SCREEN-LIN.
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
	       PERFORM DA000
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM EA000
	   ELSE
	   IF WS-OPTION = "3" AND
	      LS0-LEV > 2
	       PERFORM FA000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM GA000.
	     GO TO BA00.
       BA999.
	     EXIT.
      /
      *    ****    E X P O R T :
      *
      * 	       A C C O U N T   N U M B E R
      * 	       N A M E	 O F   A C C O U N T
      * 	       A C C O U N T   B A L A N C E
      *
       DA000	       SECTION 5.
       DA00.
             DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S03.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO DA999.
	     INITIALIZE W60-FIELDS.
             MOVE 01             TO W60-FNO (1).
	     MOVE 02		 TO W60-FNO (2).
	     MOVE 31		 TO W60-FNO (3).
             DISPLAY S06.
	     PERFORM AA100.

       DA01.
	     MOVE "Exclude Headings (Y/N)  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "N" OR "Y")
014930	       GO TO DA01.
	     MOVE WS-OPTION	 TO WS-SKIP.

       DA02.
	     DISPLAY S07.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO DA900.
       DA05.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
013100	   IF WS-F-ERROR = 8
	       GO TO DA900.
	   IF WS-SKIP = "Y"
	       IF G-ASER = ZERO
		   GO TO DA05.
	     DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
	     PERFORM WRITE-GACDET THRU WRITE-GACDET-EXIT.
	     GO TO DA05.
       DA900.
	     CLOSE GACDET.
       DA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                A L L   T H E   F I E L D S
      *
       EA000	       SECTION 5.
       EA00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
             DISPLAY S04.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO EA999.
	     INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
       EA05.
             MOVE W60-S          TO W60-FNO (W60-S).
	   IF W60-S < 43
               ADD 1             TO W60-S
               GO TO EA05.
             DISPLAY S06.
             PERFORM AA100.

       EA08.
	     MOVE "Exclude Headings (Y/N)  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "N" OR "Y")
014930	       GO TO EA08.
	     MOVE WS-OPTION	 TO WS-SKIP.
	     DISPLAY S07.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO EA900.
       EA10.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
013100	   IF WS-F-ERROR = 8
	       GO TO EA900.
	   IF WS-SKIP = "Y"
	       IF G-ASER = ZERO
		   GO TO EA10.
	     DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
	     PERFORM WRITE-GACDET THRU WRITE-GACDET-EXIT.
	     GO TO EA10.
       EA900.
	     CLOSE GACDET.
       EA999.
             EXIT.
      /
      *    ****    E X P O R T :
      *
      *                S E L E C T E D   F I E L D S
      *
       FA000	       SECTION 5.
       FA05.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
             DISPLAY S05.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO FA999.
	     INITIALIZE W60-FIELDS.
             MOVE 1              TO W60-S.
001020	     DISPLAY " " AT 1803 WITH FOREGROUND-COLOR 14
		     " to view the fields available "
                      WITH FOREGROUND-COLOR 7
                     "S" WITH FOREGROUND-COLOR 14
                     "elect/" WITH FOREGROUND-COLOR 7
                     "A" WITH FOREGROUND-COLOR 14
		     "ccept - " WITH FOREGROUND-COLOR 7
                     "Esc" WITH FOREGROUND-COLOR 14
                     "ape" WITH FOREGROUND-COLOR 7.
             MOVE 1              TO W65-S.
       FA10.
             MOVE W65-FLD (W65-S)
                                 TO W65-FIELD.
             DISPLAY S09.
       FA15.
	     DISPLAY " " AT 1851.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2541
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF W65-S = 1
			 GO TO FA15
		     ELSE
			 SUBTRACT 1  FROM W65-S
			 GO TO FA10
		 WHEN DOWN-KEY
		     IF W65-S NOT < 43
			 GO TO FA15
		     ELSE
			 ADD 1	 TO W65-S
			 GO TO FA10
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO FA15
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO FA05
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO FA15
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	       CALL X"35"
	       GO TO FA15.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
	       CALL X"E5"
               GO TO FA15.
           IF WS-OPTION = "A"
               GO TO FA30.
       FA18.
             MOVE 1              TO WS-S1.
       FA20.
           IF WS-S1 = W60-S
               GO TO FA25.
           IF W60-FNO (WS-S1) = W65-S
               GO TO FA15.
             ADD 1               TO WS-S1.
             GO TO FA20.
       FA25.
             MOVE W65-S          TO W60-FNO (W60-S).
             DISPLAY S06.
             ADD 1               TO W60-S.
	   IF W65-S < 43
               ADD 1             TO W65-S.
             GO TO FA10.
       FA30.
             PERFORM AA100.

       FA31.
	     MOVE "Exclude Headings (Y/N)  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "N" OR "Y")
014930	       GO TO FA31.
	     MOVE WS-OPTION	 TO WS-SKIP.
	     DISPLAY S07.
       FA32.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO FA900.
       FA35.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
013100	   IF WS-F-ERROR = 8
	       GO TO FA900.
	   IF WS-SKIP = "Y"
	       IF G-ASER = ZERO
		   GO TO FA35.
	     DISPLAY S08.
             PERFORM AA400.
             MOVE W55-EXP-DATA   TO EXP-REC1.
	     PERFORM WRITE-GACDET THRU WRITE-GACDET-EXIT.
	     GO TO FA35.
       FA900.
	     CLOSE GACDET.
       FA999.
             EXIT.
      /
      *    ****    F I E L D   S E P A R A T O R
      *
       GA000	       SECTION 5.
       GA00.
             DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S10.
       GA05.
	     ACCEPT W50-EXP AT 0618
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
       GA999.
	     EXIT.

      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
042520	     MOVE 5		 TO WS-PARKEY.
019820	     MOVE 1		 TO WS-PARKEY.
019830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019840       MOVE PAR-DMY        TO W12-TODAY.
019850       MOVE PAR-COMPANY TO W95-COMP.
       ZA05.
             MOVE ","            TO W50-EXP.
019900       GO TO ZA999-EXIT.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.
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
021950	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
021960				    WS-MCHR (63)
				    WS-MCHR (1)	 WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512 
019950                WITH FOREGROUND-COLOR 14.
019960       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.
      
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
      
041560 DISPLAY-FILE-NAME.
041810	   IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 99
041860	       MOVE W02-GACDET	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY.
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
