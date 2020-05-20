      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	      **     ******    *
      *   **	**  **	      **    **	 **  **      ***    **	  **   *
      *   **	    **	      **    **	**    **    * **	 **    *
      *   **	    **	      *******	**    **   *  **       **      *
      *   **   ***  **	      **	**    **  *******    **        *
      *   **	**  **	      **	 **  **       **    **	       *
      *    ******   ********  **	  ****	     ****   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  G / L E D G E R   D E L E T E	 R E C U R I N G   E N T R Y   *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP042.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.           NOVEMBER 1984.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2008
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000160 COPY GACCNT.SL.

000370	   SELECT GLINST  ASSIGN DISK
000380                    LOCK EXCLUSIVE
000390                    STATUS WS-STATUS
000400                    ACCESS RANDOM
000410                    ORGANIZATION RELATIVE
000420                    RELATIVE WS-GLINSTKEY.

000270 COPY PARAM.SL.

       COPY SHARED.SL.

000560 DATA DIVISION.
000570 FILE SECTION.

       COPY APACFIDS.FDE.

000710 COPY CONTROL.FDE.

000610 COPY GACCNT.FDE.

000670 COPY GLINST.FD.

000590 COPY PARAM.FDE.

       COPY SHARED.FDE.

      /
001410 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-SUB	   PIC 9(04)	 COMP-5.
001420 77  WS-PARKEY       PIC 9(05)     COMP-0 VALUE 1.
001430 77  WS-NETKEY	   PIC 9(05)	 COMP-0 VALUE 1.
001460 77  WS-GLINSTKEY    PIC 9(05)	 COMP-0.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001490 77  WS-KSTORE	   PIC 9(05)	 COMP-0.
001500 77  WS-ISUB         PIC 9(05)     COMP-0.
001510 77  WS-CHEQUE       PIC 9(05)     COMP-0.
001520 77  WS-PAGE         PIC 9(05)     COMP-0.
001530 77  WS-MONTH        PIC 9(02) VALUE ZERO.
001540 77  WS-LC           PIC 9(02) VALUE ZERO.
001550 77  WS-S1           PIC 9(05)     COMP-0.
001560 77  WS-S2           PIC 9(05)     COMP-0.
001570 77  WS-S3           PIC 9(05)     COMP-0.
001580 77  WS-S4           PIC 9(05)     COMP-0.
001590 77  WS-S5           PIC 9(05)     COMP-0.
001600 77  WS-S6           PIC 9(05)     COMP-0.
001610 77  WS-S7           PIC 9(05)     COMP-0.
001620 77  WS-S8           PIC 9(05)     COMP-0.
001630 77  WS-S9           PIC 9(05)     COMP-0.
001640 77  WS-S10          PIC 9(05)     COMP-0.
001900 77  WS-Y-END        PIC  9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
001650 77  WS-EXS          PIC 9(01) VALUE ZERO.
001660 77  WS-INDG         PIC 9(01) VALUE ZERO.
001670 77  WS-NUM          PIC Z9.
001680 77  WS-OPTION       PIC X(01).
001680 77  WS-SKIP         PIC X(01) VALUE "Y".
001700 77  WS-ERROR        PIC 9(02) VALUE ZERO.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI       PIC  X(04).
003100 77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001750 77  WS-BATCH        PIC 9(05)     COMP-3.
001760 77  WS-TNEXT        PIC 9(05)     COMP-0.
002080 77  WS-PERIOD	   PIC	9(02) VALUE ZERO.
001770 77  WS-SPC	   PIC X(40) VALUE SPACES.
001780 77  WS-H1	   PIC X(05) VALUE "DATE:".
001790 77  WS-H3           PIC X(06) VALUE "BATCH:".
001800 77  WS-H4           PIC X(05) VALUE "PAGE:".
001830 77  WS-ER1	   PIC X(07) VALUE "Account".
001840 77  WS-ER3          PIC X(04) VALUE "Date".
001850 77  WS-ER4          PIC X(05) VALUE "Value".
001860 77  WS-ER6          PIC X(09) VALUE "No Record".
001870 77  WS-ER7          PIC X(13) VALUE "Group Heading".
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

001890 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002010 01  W02-FID.

       COPY APACFIDS.ID.

000710 COPY CONTROL.ID.

000610 COPY GACCNT.ID.

000670 COPY GLINST.ID.

       COPY PARAM.ID.

       COPY SHARED.ID.

       COPY W05.GL.

002370 01  W10-H2.
002380     03  FILLER      PIC X(02) VALUE "-".
002390     03  W10-HEAD    PIC X(25).

       COPY W15.GL.

002720 01  W20-TOTALS.
002730     03  W20-DEBIT       PIC S9(09)V99 COMP-3.
002740     03  W20-CREDIT      PIC S9(09)V99 COMP-3.
002750     03  W20-SDT         PIC S9(07)V99 COMP-3.
002760     03  W20-SCR         PIC S9(07)V99 COMP-3.
002760     03  W20-ADJUST      PIC S9(07)V99 COMP-3.
002770     03  W20-RESULT      PIC S9(06)V99.
002780     03  W20-RES1        REDEFINES W20-RESULT.
002790         05  W20-WHOLE   PIC 9(06).
002800         05  W20-DEC     PIC S9(02).
002810     03  W20-TOTAL       PIC S9(07)V99 COMP-3.
	   03  W20-YMD	   PIC	9(08).
	   03  W20-RYMD REDEFINES W20-YMD.
	       05  W20-CN  PIC	9(02).
	       05  W20-YR  PIC	9(02).
	       05  W20-MT  PIC	9(02).
	       05  W20-DY  PIC	9(02).

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).

002820 01  W25-EDIT.
002830     03  W25-S7V2.
002840         05  W25-V1      PIC Z(06)9.99CR.
003990	   03  W25-S8V2.
004000	       05  W25-VG1     PIC Z(07)9.99CR.
002850	   03  W25-DATE.
002860	       05  W25-DTE     PIC Z9/99/9999.

       01  W30-DEBT-TRN.
004020     03  W30-RESULT      PIC  9(04)    COMP-5.
004020     03  W30-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W30-TS          PIC  9(02)    COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

002870 01  W50-CALC.
002880     05  W50-ACNTS       OCCURS 30.
002890         07  W50-ANO     PIC 9(06)     COMP-3.
002900         07  W50-PER     PIC 9(03)V99  COMP-3.
002910 01  W55-TOTALS.
002920     03  W55-VAL         PIC S9(07)V99 COMP-3 OCCURS 12.
002930 01  W55-TOT             REDEFINES W55-TOTALS.
002940     03  W55-1           PIC S9(07)V99 COMP-3.
002950     03  W55-2           PIC S9(07)V99 COMP-3.
002960     03  W55-3           PIC S9(07)V99 COMP-3.
002970     03  W55-4           PIC S9(07)V99 COMP-3.
002980     03  W55-5           PIC S9(07)V99 COMP-3.
002990     03  W55-6           PIC S9(07)V99 COMP-3.
003000     03  W55-7           PIC S9(07)V99 COMP-3.
003010     03  W55-8           PIC S9(07)V99 COMP-3.
003020     03  W55-9           PIC S9(07)V99 COMP-3.
003030     03  W55-10          PIC S9(07)V99 COMP-3.
003040     03  W55-11          PIC S9(07)V99 COMP-3.
003050     03  W55-12          PIC S9(07)V99 COMP-3.
003060 01  W70-ACTS.
003070     03  W70-BRANCH      PIC 9(05)     COMP-0.

       COPY W70DEBT.WS REPLACING W70-DEBT BY W70-DEBREC.

       COPY W70CRED.WS.

000910 01  W70-RECORD6.
000920     03  W70-PJF         PIC  9(05).
000930     03  W70-PJL         PIC  9(05).
000940     03  W70-RAILD       PIC S9(09)V99 COMP-3.
000950     03  W70-RAILM       PIC S9(09)V99 COMP-3.
000960     03  W70-DISCD       PIC S9(09)V99 COMP-3.
000970     03  W70-DISCM       PIC S9(09)V99 COMP-3.
000980     03  W70-COST-CODE   PIC  X(10).
000990     03  W70-MTHEND      PIC  9(02).                
001000     03  W70-LPG.
001010         05  W70-LPP     PIC  9(02).
001020     03  W70-WHOLESALE   PIC  X(01).
001060	   03  FILLER	       PIC  X(30).
001070	   03  W70-SUSGL       PIC  9(06)    COMP-3.
	   03  W70-INTEGRATE   PIC  X(01).
001040     03  W70-DEBGL       PIC  9(06)    COMP-3.
001050     03  W70-CREGL       PIC  9(06)    COMP-3.
001060     03  W70-INTGL       PIC  9(06)    COMP-3.
001070     03  W70-BNKGL       PIC  9(06)    COMP-3.
001080     03  W70-UNPROF      PIC  9(06)    COMP-3.
001090     03  W70-REDGL       PIC  9(06)    COMP-3.
001100     03  W70-ADJGL       PIC  9(06)    COMP-3.
001110     03  W70-RLGL        PIC  9(06)    COMP-3.
001120     03  W70-DSGL        PIC  9(06)    COMP-3. 
001130	   03  W70-PL          PIC S9(09)V99 COMP-3.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
007070	   03  W75-GNO	      PIC 9(06) COMP-5
			      OCCURS 18.

003400 01  W90-COMP.
003410     03  W90-CNAME       PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003420 SCREEN SECTION.

       COPY BLANK.CRT.

      *
      *    ****   HEADING LINES FOR INSTRUCTION SCREEN
      *
003880 01  S04.
003890	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 7 HIGHLIGHT
003900                           VALUE "DELETE STANDING INSTRUCTION".
      *
      *    ****   INSTRUCTION SCREEN LAYOUT
      *
003910 01  S05.
003920	   03  LINE  4 COLUMN  4 VALUE "Instruction".
	   03	       COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(05) FROM WS-SPC.
003930	   03  LINE  6 COLUMN  4 VALUE "Value      ".
	   03	       COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(10) FROM WS-SPC.
003940	   03  LINE  7 COLUMN  4 VALUE "Narrative  ".
	   03	       COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(36) FROM WS-SPC.
003960	   03  LINE  9 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Debit Details:".
003970	   03  LINE 11 COLUMN  4 VALUE "Account No ".
	   03	       COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPC.
003980	   03  LINE 13 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Credit Details:".
003990	   03  LINE 15 COLUMN  4 VALUE "Account No ".
	   03	       COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(06) FROM WS-SPC.
	   03	       COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(30) FROM WS-SPC.
      *
      *    ****   FIRST INPUT SCREEN FOR INSTRUCTION
      *
004000 01  S06.
	   02  S06A.
004010	   03  LINE  6 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004020                           PIC Z(06)9.99 USING W15-VAL AUTO.
004030	   03  LINE  7 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004040                           PIC X(36) USING W15-NAR AUTO.
	   02  S06B.
004050	   03  LINE 11 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004060                           PIC 9(06) USING W15-ACCOUNT AUTO.
      *
      *    ****   SECOND INPUT SCREEN FOR INSTRUCTION
      *
004070 01  S07.
004080	   03  LINE 15 COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004090                           PIC 9(06) USING W15-BANK AUTO.
      *
      *    ****   INSTRUCTION NUMBER INPUT/DISPLAY SCREEN
      *
004100 01  S08.
004110	   03  LINE  4 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004120                           PIC Z(04)9 USING W15-INSNO AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004630 AA000	       SECTION.
004640 AA00.
003240	   IF LS0-GLLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA25.
004650	     PERFORM ZA000.
	     PERFORM CA000.
	     CLOSE GLINST.

       AA25.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

005690 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD1.

       COPY GLINST.RD.
 
       COPY SHARED.RD.

      /
006840 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GLINST.WR.

      /
      *    ****    LOGGING OF UPDATES
      *
       AY000           SECTION.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *	     MOVE SHR-STOCK	 TO WS-STOCK.
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
012620	     MOVE "99"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 4              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
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
012760		   IF PAR-PROG(WS-SUB) = "CG" OR "GL"
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
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
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
013090	     COMMIT.

011860 AY999.
011870       EXIT.
      /
      *    ****    D E L E T E   S T A N D I N G   I N S T R U C T I O N
      *
008970 CA000	       SECTION 5.
008980 CA00.
008990       DISPLAY CLR-SCREEN.
009000       DISPLAY S04.
009010       DISPLAY S05.
009020	     MOVE ZERO		 TO W15-INSNO.
009030 CA05.
009040       ACCEPT S08.
009050	     DISPLAY CLEAR-L25.
009060     IF W15-INSNO = ZERO
009070         GO TO CA999.
009080	     MOVE W15-INSNO	 TO WS-GLINSTKEY.
009090       PERFORM READ-GLINST-CHECK THRU READ-GLINST-EXIT.
009100     IF WS-INDG = 1
009110	       MOVE ZERO	 TO WS-INDG
009120	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
009130         GO TO CA05.
009140     IF INS-NAR = SPACES
009150	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
009160         GO TO CA05.
009170	     MOVE INS-NAR	 TO W15-NAR.
009180	     MOVE INS-VALUE	 TO W15-VAL.
009190	     MOVE INS-ACCR	 TO W15-BANK G-AC.
	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       MOVE "INVALID ACCOUNT NUMBER"
				 TO W15-ACNME
	   ELSE
	       MOVE G-NAME	 TO W15-ACNME.
009200	     MOVE INS-ACDT	 TO W15-ACCOUNT G-AC.
	     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       MOVE "INVALID ACCOUNT NUMBER"
				 TO G-NAME.
009210	     DISPLAY S06.
009220	     DISPLAY S07.
	     DISPLAY G-NAME AT 1123
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W15-ACNME AT 1523
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
009310	   IF WS-OPTION = "N"
009320         GO TO CA00.
009330       PERFORM AY60 THRU AY999.
009340	     MOVE SPACES	 TO INS-NAR.
009350	     MOVE ZERO		 TO INS-VALUE INS-ACCR.
009360	     MOVE G-GLINST	 TO INS-ACDT.
009370	     MOVE WS-GLINSTKEY	 TO G-GLINST.
009380       PERFORM REWRITE-GLINST THRU WRITE-GLINST-EXIT.
009400       PERFORM AY70 THRU AY999.
009410       GO TO CA00.
009420 CA999.
009430       EXIT.
      /
021700 ZA000	     SECTION 9.
021710 ZA00.
033940       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
049200	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLINST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLINST.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.

042170 ZA02.
022130	     OPEN I-O GLINST.
022140     IF RUNTIME-ERROR
               IF FLE-LOCKED
041880             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
022160     IF WS-STATUS NOT = "00"
022170         MOVE 10           TO WS-F-ERROR
022180         PERFORM OPEN-ERROR.
022190	     MOVE 1 TO WS-PARKEY.
022200       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022210       MOVE PAR-COMPANY TO W90-CNAME.
022220       MOVE PAR-DMY TO W05-TODAY.
022230       MOVE PAR-YMD TO W05-TYMD.
022250       MOVE 6 TO WS-PARKEY.
022260       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022270       MOVE PAR-RECORD6 TO W70-RECORD6.
022440       MOVE ZERO TO WS-ERROR.
022450     IF W70-BNKGL = ZERO
022460         DISPLAY (20, 20) "Parameters not set up - ENTER"
022470                           WITH FOREGROUND-COLOR 14
022480         ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
022490         MOVE 1 TO WS-ERROR.
	     DISPLAY SPACES AT 2525
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
022540	     GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
041850     IF WS-F-ERROR = 40
041860         MOVE "INTEGRATION"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
034710 ZA55.
034720       MOVE 1 TO WS-S1.
034730	     MOVE SPACES TO WS-MID-LNE.
034740 ZA60.
034750       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
034760       MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
034770     IF WS-S1 < 80
034780         ADD 1 TO WS-S1
034790         GO TO ZA60.
034800       MOVE WS-G9  TO WS-TCH  (1).
034810       MOVE WS-G10 TO WS-TCH  (80).
034820       MOVE WS-G11 TO WS-BCH  (1).
034830       MOVE WS-G12 TO WS-BCH  (80).
034840       MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
034850       MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
034860       MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
034870                      WS-TCHR (63).
034880       MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
034890                      WS-MCHR (63)
034900			    WS-MCHR (1)  WS-MCHR (80).
034920       MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
034930                      WS-BCHR (63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
022550 ZA200.
             DISPLAY "This program can only be run when no other worksta
      -      "tions are logged into the" AT 0803
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "accounting system. All other workstations should r
      -      "eturn to the program con-" AT 0903
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "trol menu or exit from the accounting system befor
      -      "e you may continue       " AT 1003
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
022560       DISPLAY "Files locked - Try later" AT 2312
022570                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
022580       DISPLAY "Press any key" AT 2512 
022590		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
022600	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
022630 ZA999.
022640       EXIT.
      /
022650 ZB000	     SECTION 9.
      
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 10
041860	       MOVE W02-GLINST	 TO WS-FILE
041870         MOVE WS-GLINSTKEY TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
036720     IF WS-STATUS = "10"
036730         MOVE "End of FILE" TO WS-STAT-MESSAGE
036740     ELSE
036750     IF WS-STATUS = "22"
036760         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
036770     ELSE
036780     IF WS-STATUS = "23"
036790         MOVE "Invalid record number" TO WS-STAT-MESSAGE
036800     ELSE
036810     IF WS-STATUS = "24"
036820         MOVE "DISK full" TO WS-STAT-MESSAGE
036830     ELSE
036840     IF WS-STATUS = "30"
036850         MOVE "DISK error" TO WS-STAT-MESSAGE
036860     ELSE
036870     IF WS-STATUS = "94"
036880         MOVE "FILE locked" TO WS-STAT-MESSAGE.
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
