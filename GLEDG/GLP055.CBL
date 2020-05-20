      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	  ********  ********   *
      *   **	**  **	      **    **	 **  **   **	    **	       *
      *   **	    **	      **    **	**    **  *******   *******    *
      *   **	    **	      *******	**    **	**	  **   *
      *   **   ***  **	      **	**    **	**	  **   *
      *   **	**  **	      **	 **  **   **	**  **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   Y E A R - E N D	T R A N S F E R S      *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP055.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.	       OCTOBER 1992.

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

000180 COPY GLTRAN.SL.

000270 COPY PARAM.SL.

000550	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

       COPY SHARED.SL.

000560 DATA DIVISION.
000570 FILE SECTION.

       COPY APACFIDS.FDE.

000710 COPY CONTROL.FDE.

000610 COPY GACCNT.FDE.

000630 COPY GLTRAN.FDE.

000590 COPY PARAM.FDE.

000730 FD  PRNREP    LABEL RECORD OMITTED
000430		     LINAGE IS WS-PGE-LENGTH.
000750 01  R-L1.
000760     03  R-DET       PIC X(132).
000770 01  R-L2.
000780     03  R-H1        PIC X(06).
000790	   03  R-DTE1	   PIC Z9/99/9999.
000800	   03  FILLER	   PIC X(08).
000810     03  R-H2        PIC X(40).
000820     03  R-H2A       PIC X(27).
000830     03  FILLER      PIC X(10).
000840     03  R-H3        PIC X(07).
000850     03  R-BTCH      PIC Z(04)9.
000860     03  FILLER      PIC X(10). 
000870     03  R-H4        PIC X(06).
000880     03  R-PGE       PIC ZZ9.
000890 01  R-L3.
000900     03  FILLER      PIC X(03).
000910     03  R-H5        PIC X(11).
000920     03  R-H6        PIC X(05).
000930     03  FILLER      PIC X(01).
000940     03  R-H7        PIC X(13).
000950     03  R-H8        PIC X(05).
000960     03  FILLER      PIC X(06).
000970     03  R-H9        PIC X(09).
000980     03  R-H10       PIC X(12).
000990     03  R-H11       PIC X(13).
001000     03  FILLER      PIC X(01).
001010     03  R-H12       PIC X(09).
001020     03  R-H13       PIC X(13).
001030     03  FILLER      PIC X(08).
001040     03  R-H14       PIC X(14).
001050     03  R-H15       PIC X(09).
001060 01  R-L4.
001070     03  FILLER      PIC X(03).
001080     03  R-ACC       PIC 9(04)/99.
001090     03  FILLER      PIC X(04).
001100     03  R-ANME      PIC X(24).
001110	   03  FILLER	   PIC X(03).
001120	   03  R-DTE2	   PIC Z9/99/9999.
001130	   03  FILLER	   PIC X(03).
001140     03  R-REF       PIC Z(07).
001150     03  FILLER      PIC X(04).
001160     03  R-NAR       PIC X(36).
001170     03  FILLER      PIC X(01).
           03  R-DBRKO     PIC X(01).
001180     03  R-DBT       PIC Z(08)9.99.
           03  R-DBRKC     PIC X(01).
           03  R-CBRKO     PIC X(01).
001200     03  R-CRD       PIC Z(08)9.99-.
           03  R-CBRKC     PIC X(01).
001210     03  FILLER      PIC X(01).
001220 01  R-L5.
001230     03  RI-H1       PIC X(06).
001240     03  RI-H2       PIC X(40).
001250     03  RI-H3       PIC X(39).
001260     03  RI-H4       PIC X(42).
001270     03  RI-H5       PIC X(05).
001280 01  R-L6.
001290     03  RI-NO       PIC Z(04)9.
001300     03  FILLER      PIC X(01).
001310     03  RI-ACDT     PIC 9999/99.
001320     03  FILLER      PIC X(01).
001330     03  RI-DTNAME   PIC X(30).
001340     03  FILLER      PIC X(02).
001350     03  RI-ACCR     PIC 9999/99.
001360     03  FILLER      PIC X(01).
001370     03  RI-CRNAME   PIC X(30).
001380     03  FILLER      PIC X(01).
001390     03  RI-NAR      PIC X(36).
001400     03  RI-VAL      PIC Z(07)9.99.

       COPY SHARED.FDE.

      /
001410 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-SUB	   PIC 9(04)	 COMP-5.
001420 77  WS-PARKEY       PIC 9(05)     COMP-0 VALUE 1.
001430 77  WS-NETKEY	   PIC 9(05)	 COMP-0 VALUE 1.
001430 77  WS-GLAC	   PIC 9(06)	 COMP-0.
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
001750 77  WS-BATCH	   PIC 9(05)	 COMP-3.
001760 77  WS-TNEXT        PIC 9(05)     COMP-0.
002080 77  WS-PERIOD       PIC  9(02)    COMP-5 VALUE ZERO.
001770 77  WS-SELECT       PIC X(13) VALUE "Select Option".
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
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

002240 01  W00-REC.
	   03  W00-REFER.
	       05  W00-REF PIC	9(07).
002260     03  W00-NAR     PIC  X(40).
002280	   03  W00-VAL	   PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002010 01  W02-FID.

       COPY APACFIDS.ID.

000710 COPY CONTROL.ID.

000610 COPY GACCNT.ID.

000630 COPY GLTRAN.ID.

       COPY PARAM.ID.

       COPY SHARED.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "GLP055.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.GL.

002370 01  W10-H2.
002380     03  FILLER      PIC X(02) VALUE "-".
002390     03  W10-HEAD    PIC X(25).

       COPY W15.GL.

002720 01  W20-TOTALS.
002730     03  W20-DEBIT       PIC S9(09)V99 COMP-3.
002740     03  W20-CREDIT      PIC S9(09)V99 COMP-3.
002750	   03  W20-SDT	       PIC S9(09)V99 COMP-3.
002760	   03  W20-SCR	       PIC S9(09)V99 COMP-3.
002760	   03  W20-ADJUST      PIC S9(09)V99 COMP-3.
002770	   03  W20-RESULT      PIC S9(09)V99.
002780     03  W20-RES1        REDEFINES W20-RESULT.
002790	       05  W20-WHOLE   PIC 9(09).
002800         05  W20-DEC     PIC S9(02).
002810	   03  W20-TOTAL       PIC S9(09)V99 COMP-3.
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
002920	   03  W55-VAL	       PIC S9(09)V99 COMP-3 OCCURS 12.

002930 01  W55-TOT             REDEFINES W55-TOTALS.
002940	   03  W55-1	       PIC S9(09)V99 COMP-3.
002950	   03  W55-2	       PIC S9(09)V99 COMP-3.
002960	   03  W55-3	       PIC S9(09)V99 COMP-3.
002970	   03  W55-4	       PIC S9(09)V99 COMP-3.
002980	   03  W55-5	       PIC S9(09)V99 COMP-3.
002990	   03  W55-6	       PIC S9(09)V99 COMP-3.
003000	   03  W55-7	       PIC S9(09)V99 COMP-3.
003010	   03  W55-8	       PIC S9(09)V99 COMP-3.
003020	   03  W55-9	       PIC S9(09)V99 COMP-3.
003030	   03  W55-10	       PIC S9(09)V99 COMP-3.
003040	   03  W55-11	       PIC S9(09)V99 COMP-3.
003050	   03  W55-12	       PIC S9(09)V99 COMP-3.
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
	   03  W75-GNO	      PIC 9(06) COMP-5
                              OCCURS 18.

003400 01  W90-COMP.
003410     03  W90-CNAME       PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003420 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004630 AA000	       SECTION.
004640 AA00.
003240	   IF LS0-GLLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA25.
004650	     PERFORM ZA000.
	     PERFORM GA000.

       AA25.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

005690 AB000           SECTION.

       COPY CONTROL.RD1.

       COPY PARAM.RD.

       COPY GACCNT.RD1.

       COPY GLTRAN.RD.

       COPY SHARED.RD.

      /
006840 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

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

011260 AY65.
011270       MOVE 4              TO WS-NETKEY.
011300       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
011310       MOVE G-BATCH        TO WS-BATCH.
011320     IF G-BATCH = 99999
011330         MOVE 1            TO G-BATCH
011340     ELSE
011350         ADD 1             TO G-BATCH.
011360       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
011380       GO TO AY999.

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
      *    ****    Y E A R - E N D   T R A N S F E R S
      *
017740 GA000	     SECTION 5.
017750 GA00.
017760       DISPLAY CLR-SCREEN.
	     DISPLAY "G/L CLOSING TRANSFERS" AT 0230
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
017770	     DISPLAY "Post Year-End Closing Transfers (" AT 0412
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")?".
017790 GA05.
017800	     ACCEPT WS-OPTION AT 0451
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
017860	   IF WS-OPTION = "N"
017870         GO TO GA999.
017880     IF NOT (WS-OPTION = "Y")
017890         GO TO GA05.
017930	     DISPLAY "Date         :" AT 0616.
017940	     DISPLAY "Account From :" AT 0816.
017950	     DISPLAY "Account To   :" AT 1016.
017960	     DISPLAY "Value	   :" AT 1216.
017970	     MOVE ZERO		 TO WS-PAGE W20-DEBIT W20-CREDIT.
017980	     MOVE "G/L CLOSING JOURNALS"
				 TO W10-HEAD.
017990	     MOVE W05-TODAY	 TO W15-DATE W25-DTE.
	     MOVE W15-CC	 TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
018000	     DISPLAY W25-DATE AT 0631
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018010	     MOVE WS-COND	 TO R-DET.
018020       WRITE R-L1 BEFORE 0 LINES.
018030       PERFORM AY60 THRU AY999.
010900       PERFORM AY65 THRU AY999.
      *
      *    ****   A U D I T   T R A I L   H E A D I N G S
      *
018040 GA10.
005460       MOVE SPACES         TO R-DET.
005470       MOVE WS-H1          TO R-H1.
005480       MOVE W90-COMP       TO R-H2.
005490       MOVE W10-H2         TO R-H2A.
005500       MOVE WS-H3          TO R-H3.
005510       MOVE WS-H4          TO R-H4.
005520       ADD 1               TO WS-PAGE.
005530       MOVE WS-PAGE        TO R-PGE.
005540       MOVE WS-BATCH       TO R-BTCH.
005550       MOVE W05-TODAY      TO R-DTE1.
005560       WRITE R-L1 BEFORE 2 LINES.
005570       MOVE SPACES         TO R-DET.
018060	     MOVE "ACCNT NO"	 TO R-H5.
018070	     MOVE ALL "_"	 TO R-H6.
018080       MOVE "ACCOUNT NAME" TO R-H7.
018090	     MOVE ALL "_"	 TO R-H8.
018100	     MOVE "DATE"	 TO R-H9.
018110	     MOVE "REFERENCE"	 TO R-H10.
018120	     MOVE ALL "_"	 TO R-H11.
018130	     MOVE "NARRATION"	 TO R-H12.
018140	     MOVE ALL "_"	 TO R-H13.
018150	     MOVE "DEBIT"	 TO R-H14.
018160	     MOVE "CREDIT"	 TO R-H15.
018170       WRITE R-L1 BEFORE 2 LINES.
018180	     MOVE SPACES	 TO R-DET.
      *
      *    ****   R E T A I N E D   I N C O M E   A C C O U N T   N O
      *
018190 GA15.
018200	     MOVE W70-UNPROF	 TO W15-ACCOUNT W15-REFER.
018210	     DISPLAY W15-ACCOUNT AT 1031
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018220     IF (W15-ASER = ZERO) OR (W15-ACCOUNT < 410000)
018230         GO TO GA20.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
018260	       GO TO GA20.
018270	     MOVE G-AC		 TO WS-GLAC.
018280	     MOVE G-NAME	 TO W15-NAR.
018290	     DISPLAY G-NAME AT 1040
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018300       GO TO GA25.

018310 GA20.
018320	     MOVE "Invalid account no (To)"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
018350       GO TO GA999.

018360 GA25.
	     MOVE 100000	 TO G-AC.

       GA26.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
018390	       GO TO GA60.
      *
      *    ****   G E T   T H E   N E X T   A C C O U N T
      *
018400 GA30.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
014220	   IF WS-F-ERROR = 8
	       GO TO GA60.
018410     IF G-AC > 399999
018420         GO TO GA60.
	   IF G-AC = W15-ACNT
	       GO TO GA30.
018430	     MOVE G-MOVE	 TO G-LSTY.
018440     IF (G-ASER = ZERO) OR (G-BAL = ZERO)
018450         PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
018460	       GO TO GA30.
018470	     DISPLAY G-AC AT 0831
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018480	     DISPLAY G-NAME AT 0840
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018490	     MOVE G-BAL 	 TO W00-VAL W25-V1.
018500	     DISPLAY W25-S7V2 AT 1231
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018510	     MULTIPLY -1 BY G-BAL
				 GIVING W15-VAL.
018520	     MOVE G-NAME	 TO W00-NAR.
018530	     MOVE G-AC		 TO W00-REF W15-ACNT.
      *
      *    ****    PROCESS ACCOUNT FROM ENTRY
      *
018540 GA35.
018550	     MOVE G-AC		 TO R-ACC.
018560	     MOVE G-NAME	 TO R-ANME.
018570	     MOVE W15-NAR	 TO R-NAR.
018580	     MOVE W15-REFER	 TO R-REF.
018590	     MOVE W15-DATE	 TO R-DTE2.
018600     IF W15-VAL < ZERO
018610	       MOVE W15-VAL	 TO R-CRD
018620     ELSE
018630	       MOVE W15-VAL	 TO R-DBT.
             INITIALIZE T-REC.

018760 GA40.
012680	     MOVE W15-YMD	 TO T-DATE.
018790       MOVE W15-NAR        TO T-NARATIVE.
018800       MOVE W15-REFER      TO T-REF.
018810       MOVE W15-VAL        TO T-VAL.
018840       MOVE G-AC           TO T-AC.
018850     IF W15-VAL < ZERO
018860         ADD W15-VAL       TO W20-CREDIT
018870         ADD W15-VAL       TO G-CR
018880         MOVE 02           TO T-CODE 
018890     ELSE
018900         ADD W15-VAL       TO W20-DEBIT
018910         ADD W15-VAL       TO G-DT
018920         MOVE 01           TO T-CODE.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             ADD WS-VALUE        TO G-VAL (W05-MM).
018930       ADD W15-VAL         TO G-BAL.
018660       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
018960       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

018990 GA45.
019000	   IF LINAGE-COUNTER < WS-PRN-LENGTH
019010         WRITE R-L1 BEFORE 1 LINES
019020     ELSE
019030         WRITE R-L1 BEFORE PAGE
019040         PERFORM GA10.
019050	     MOVE SPACES	 TO R-DET.
      *
      *    ****    PROCESS ACCOUNT TO ENTRY
      *
019060 GA50.
019070	     MOVE WS-GLAC	 TO G-AC.
019080       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
019090	     MOVE W00-VAL	 TO W15-VAL.
019100	     MOVE W00-NAR	 TO W15-NAR.
019110	     MOVE W00-REF	 TO W15-REFER.
	     MOVE G-NAME	 TO W00-NAR.
019120       PERFORM GA35 THRU GA45.
019130	     MOVE W15-ACCOUNT	 TO W15-REFER.
019140	     MOVE W00-NAR	 TO W15-NAR.

019150 GA55.
	     MOVE W15-ACNT	 TO G-AC.
019180	     GO TO GA26.

019200 GA60.
019190       PERFORM AY70 THRU AY999.
019210	   IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
019220         WRITE R-L1 BEFORE 1 LINES
019230     ELSE
019240         WRITE R-L1 BEFORE PAGE
019250         PERFORM GA10.
019260	     MOVE "***** TOTALS *****"
				 TO R-NAR.
019270	     MOVE W20-DEBIT	 TO R-DBT.
019280	     MOVE W20-CREDIT	 TO R-CRD.
019290       WRITE R-L1 BEFORE PAGE.
019300	     MOVE WS-NORM	 TO R-DET.
019310       WRITE R-L1 BEFORE 0 LINES.
019320 GA999.
019330       EXIT.
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
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
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
042170 ZA02.
022190	     MOVE 1		 TO WS-PARKEY.
022200       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022210	     MOVE PAR-COMPANY	 TO W90-CNAME.
022220	     MOVE PAR-DMY	 TO W05-TODAY.
022230	     MOVE PAR-YMD	 TO W05-TYMD.
022250	     MOVE 6		 TO WS-PARKEY.
022260       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022270	     MOVE PAR-RECORD6	 TO W70-RECORD6.
022440	     MOVE ZERO		 TO WS-ERROR.
022450     IF W70-BNKGL = ZERO
022460         DISPLAY (20, 20) "Parameters not set up - ENTER"
022470                           WITH FOREGROUND-COLOR 14
022480         ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
022490	       MOVE 1		 TO WS-ERROR.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
022530	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
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
034720	     MOVE 1		 TO WS-S1.
034730	     MOVE SPACES	 TO WS-MID-LNE.
034740 ZA60.
034750	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
034760	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
034770     IF WS-S1 < 80
034780	       ADD 1		 TO WS-S1
034790         GO TO ZA60.
034800	     MOVE WS-G9 	 TO WS-TCH  (1).
034810	     MOVE WS-G10	 TO WS-TCH  (80).
034820	     MOVE WS-G11	 TO WS-BCH  (1).
034830	     MOVE WS-G12	 TO WS-BCH  (80).
034840	     MOVE WS-G14	 TO WS-TCHR (1)  WS-BCHR (1).
034850	     MOVE WS-G13	 TO WS-TCHR (80) WS-BCHR (80).
034860	     MOVE WS-G2 	 TO WS-TCHR (11) WS-TCHR (45)
034870				    WS-TCHR (63).
034880	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
034890				    WS-MCHR (63)
034900				    WS-MCHR (1)  WS-MCHR (80).
034920	     MOVE WS-G4 	 TO WS-BCHR (11) WS-BCHR (45)
034930				    WS-BCHR (63).
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
       ZA210.
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
041640	   ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.
036720	   IF WS-STATUS = "10"
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
