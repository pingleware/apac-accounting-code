      $set LINKCOUNT"448"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	  ********   ******    *
      *   **	**  **	      **    **	 **  **   **	    **	  **   *
      *   **	    **	      **    **	**    **  *******	  **   *
      *   **	    **	      *******	**    **	**	***    *
      *   **   ***  **	      **	**    **	**	  **   *
      *   **	**  **	      **	 **  **   **	**  **	  **   *
      *    ******   ********  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   G / L E D G E R   P O S T   I N T E G R A T .   B A T C H    *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       GLP053.
000030 AUTHOR.                 J.W. LEMMON (APAC).
000040 DATE-WRITTEN.	       NOVEMBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2011
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

000210 COPY DEPART.SL.

000160 COPY GACCNT.SL.

000180 COPY GLTRAN.SL.

000210	   SELECT LEDTRF  ASSIGN DISK
                          LOCK EXCLUSIVE
000220                    STATUS WS-STATUS
000230                    ACCESS DYNAMIC
000240                    ORGANIZATION INDEXED
000250                    RECORD XFR-KEY.

000270 COPY PARAM.SL.

       COPY SHARED.SL.

000560 DATA DIVISION.
000570 FILE SECTION.

       COPY APACFIDS.FDE.

000710 COPY CONTROL.FDE.

000690 COPY DEPART.FDE.

000610 COPY GACCNT.FDE.

000630 COPY GLTRAN.FDE.

       COPY LEDTRF.FD.

000590 COPY PARAM.FDE.

       COPY SHARED.FDE.

      /
001410 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-SUB	   PIC	9(04)	  COMP-5.
001420 77  WS-PARKEY	   PIC	9(05)	  COMP-0 VALUE 1.
001430 77  WS-NETKEY	   PIC	9(05)	  COMP-0 VALUE 1.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001500 77  WS-ISUB	   PIC	9(05)	  COMP-0.
001510 77  WS-CHEQUE	   PIC	9(05)	  COMP-0.
001520 77  WS-PAGE	   PIC	9(05)	  COMP-0.
001530 77  WS-MONTH	   PIC	9(02) VALUE ZERO.
001540 77  WS-LC	   PIC	9(02) VALUE ZERO.
001550 77  WS-S1	   PIC	9(05)	  COMP-0.
001560 77  WS-S2	   PIC	9(05)	  COMP-0.
001570 77  WS-S3	   PIC	9(05)	  COMP-0.
001580 77  WS-S4	   PIC	9(05)	  COMP-0.
001590 77  WS-S5	   PIC	9(05)	  COMP-0.
001600 77  WS-S6	   PIC	9(05)	  COMP-0.
001610 77  WS-S7	   PIC	9(05)	  COMP-0.
001620 77  WS-S8	   PIC	9(05)	  COMP-0.
001630 77  WS-S9	   PIC	9(05)	  COMP-0.
001640 77  WS-S10	   PIC	9(05)	  COMP-0.
001490 77  WS-ASTORE	   PIC	9(06).
001900 77  WS-Y-END	   PIC	9(02) VALUE ZERO.
001900 77  WS-L-END	   PIC	9(04) VALUE ZERO.
001900 77  WS-OPEN         PIC  9(02) VALUE ZERO.
       77  WS-END	   PIC	9(08).
001910 77  WS-MS1          PIC  9(04)    COMP-5.
001920 77  WS-MS2          PIC  9(04)    COMP-5.
001930 77  WS-VALUE        PIC S9(09)V99 COMP-3.
001650 77  WS-EXS	   PIC	9(01) VALUE ZERO.
001660 77  WS-INDG	   PIC	9(01) VALUE ZERO.
001670 77  WS-NUM	   PIC	Z9.
001680 77  WS-OPTION	   PIC	X(01).
001680 77  WS-SKIP	   PIC	X(01) VALUE "Y".
001700 77  WS-ERROR	   PIC	9(02) VALUE ZERO.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
001750 77  WS-BATCH	   PIC	9(05)	 COMP-3.
001760 77  WS-TNEXT	   PIC	9(05)	 COMP-0.
002080 77  WS-PERIOD	   PIC	9(02)	 COMP-5 VALUE ZERO.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001780 77  WS-H1	   PIC	X(05) VALUE "DATE:".
001790 77  WS-H3	   PIC	X(06) VALUE "BATCH:".
001800 77  WS-H4	   PIC	X(05) VALUE "PAGE:".
001830 77  WS-ER1	   PIC	X(07) VALUE "Account".
001840 77  WS-ER3	   PIC	X(04) VALUE "Date".
001850 77  WS-ER4	   PIC	X(05) VALUE "Value".
001860 77  WS-ER6	   PIC	X(09) VALUE "No Record".
001870 77  WS-ER7	   PIC	X(13) VALUE "Group Heading".
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

000690 COPY DEPART.ID.

000610 COPY GACCNT.ID.

000630 COPY GLTRAN.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY SHARED.ID.

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
	       05  R-DET       PIC  X(136).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-H1        PIC X(06).
	       05  R-DTE1      PIC Z9/99/9999.
	       05  FILLER      PIC X(08).
	       05  R-H2        PIC X(40).
	       05  R-H2A       PIC X(27).
	       05  FILLER      PIC X(10).
	       05  R-H3        PIC X(07).
	       05  R-BTCH      PIC Z(04)9.
	       05  FILLER      PIC X(10).
	       05  R-H4        PIC X(06).
	       05  R-PGE       PIC ZZ9.

	   03  R-L3 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC X(03).
	       05  R-H5        PIC X(11).
	       05  R-H6        PIC X(05).
	       05  FILLER      PIC X(01).
	       05  R-H7        PIC X(13).
	       05  R-H8        PIC X(05).
	       05  FILLER      PIC X(06).
	       05  R-H9        PIC X(09).
	       05  R-H10       PIC X(12).
	       05  R-H11       PIC X(13).
	       05  FILLER      PIC X(01).
	       05  R-H12       PIC X(09).
	       05  R-H13       PIC X(13).
	       05  FILLER      PIC X(08).
	       05  R-H14       PIC X(14).
	       05  R-H15       PIC X(09).

	   03  R-L4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC X(03).
	       05  R-ACC       PIC 9(04)/99.
	       05  FILLER      PIC X(04).
	       05  R-ANME      PIC X(24).
	       05  FILLER      PIC X(03).
	       05  R-DTE2      PIC Z9/99/9999.
	       05  FILLER      PIC X(03).
	       05  R-REF.
		   07  R-REFER PIC Z(07).
	       05  FILLER      PIC X(04).
	       05  R-NAR       PIC X(36).
	       05  FILLER      PIC X(01).
	       05  R-DBRKO     PIC X(01).
	       05  R-DBT       PIC Z(08)9.99.
	       05  R-DBRKC     PIC X(01).
	       05  R-CBRKO     PIC X(01).
	       05  R-CRD       PIC Z(08)9.99-.
	       05  R-CBRKC     PIC X(01).
	       05  FILLER      PIC X(01).

	   03  R-L5 REDEFINES W02-PRN-LINE.
	       05  RI-H1       PIC X(06).
	       05  RI-H2       PIC X(40).
	       05  RI-H3       PIC X(39).
	       05  RI-H4       PIC X(42).
	       05  RI-H5       PIC X(05).

	   03  R-L6 REDEFINES W02-PRN-LINE.
	       05  RI-NO       PIC Z(04)9.
	       05  FILLER      PIC X(01).
	       05  RI-ACDT     PIC 9999/99.
	       05  FILLER      PIC X(01).
	       05  RI-DTNAME   PIC X(30).
	       05  FILLER      PIC X(02).
	       05  RI-ACCR     PIC 9999/99.
	       05  FILLER      PIC X(01).
	       05  RI-CRNAME   PIC X(30).
	       05  FILLER      PIC X(01).
	       05  RI-NAR      PIC X(36).
	       05  RI-VAL      PIC Z(07)9.99.

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
001060	   03  FILLER	       PIC  X(14).
	   03  W70-CONGL       PIC  9(06)    COMP-3.
	   03  W70-DCNGL       PIC  9(06)    COMP-3.
	   03  W70-CSLGL       PIC  9(06)    COMP-3.
001070	   03  W70-WARGL       PIC  9(06)    COMP-3.
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
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.

003400 01  W90-COMP.
003410     03  W90-CNAME       PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003420 SCREEN SECTION.

       COPY BLANK.CRT.

004870 COPY S99.CRT.

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
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM FA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     CLOSE LEDTRF.

       AA25.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY PRINTUTL.AS6.

005690 AB000	       SECTION.

       COPY CONTROL.RD1.

       COPY DEPART.RD1.

       COPY PARAM.RD.

       COPY GACCNT.RD1.

       COPY GLTRAN.RD.

       COPY LEDTRF.RD1.

       COPY SHARED.RD.

      /
006840 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      * 
       AE000           SECTION.
       AE15.
	     MOVE W15-MM	 TO WS-PERIOD.
	   IF W15-CY > W05-CY
	       MOVE W05-CY	 TO W15-CY.
	   IF W15-CY < W05-CY
	       IF W15-CY < WS-L-END
		   GO TO AE20
	       ELSE
	       IF NOT (W15-MM > WS-Y-END)
		   GO TO AE20
	       ELSE
		   GO TO AE999.
	   IF W05-MM = W15-MM
	      GO TO AE999.
	   IF W15-MM > W05-MM
	       MOVE W05-MM	 TO W15-MM.
	   IF W15-MM > WS-Y-END
               GO TO AE999.
	   IF W15-CY > WS-L-END
               GO TO AE999.
	     ADD 12		 TO WS-PERIOD
             GO TO AE999.

       AE20.
	   IF W15-CY > WS-L-END
               GO TO AE999.
	     ADD 12		 TO WS-PERIOD.

010160 AE999.
010170       EXIT.
      /
      *    THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *    DATE UP TO THE CURRENT MONTH
      *
       AH000           SECTION.
       AH00.
           IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
	       SUBTRACT 13 FROM WS-PERIOD
				 GIVING WS-S3
               PERFORM AH10
	       ADD 1 WS-OPEN	 GIVING WS-MS1
           ELSE
	       MOVE WS-PERIOD	 TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
	     ADD WS-VALUE	 TO G-OBAL.
	     MOVE G-AC		 TO WS-ASTORE.
	   IF G-AC < 400000 AND W70-UNPROF > 400000
	       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
	       MOVE W70-UNPROF	 TO W15-ACCOUNT G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
021840	       IF WS-F-ERROR = 8
		   MOVE G-AC	 TO T-AC
	       ELSE
		   MOVE XFR-AC	 TO W15-ACCOUNT
		   MOVE WS-ASTORE
				 TO G-AC
		   PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
       AH15.
             EXIT.
       AH999.
             EXIT.

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
      *    ****    I N T E G R A T I O N   T R A N S F E R S
      *
012880 FA000	     SECTION 5.
012890 FA00.
012900       DISPLAY CLR-SCREEN.
012910       DISPLAY "POST INTEGRATION JOURNALS" AT 0228
012920                         WITH FOREGROUND-COLOR 15.
012930	     DISPLAY "Are you sure (" AT 0410
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
012950	     ACCEPT WS-OPTION AT 0429
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
013010	   IF WS-OPTION = "N"
013020         GO TO FA999.
	     MOVE W05-TYMD	 TO WS-END.
	     DISPLAY "A" AT 0610 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll or "
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elected transactions".
       FA02.
	     ACCEPT WS-OPTION AT 0639
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
013010	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO FA02.
	   IF WS-OPTION = "A"
	       GO TO FA04.
	     DISPLAY "Include transactions up to " AT 0810.
	     DISPLAY "Format DDMMCCYY" AT 0848
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE W05-TODAY	 TO W15-DATE.

       FA03.
	     ACCEPT W15-DATE AT 0837
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530	       GO TO FA03.
	     MOVE W15-DD	 TO W15-DY.
	     MOVE W15-MM	 TO W15-MT.
	     MOVE W15-CC	 TO W15-CN.
	     MOVE W15-YY	 TO W15-YR.
	     MOVE W15-YMD	 TO WS-END.

       FA04.
	     INITIALIZE XFR-REC.
             PERFORM START-AT-LEDTRF-KEY THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
	       MOVE "No entries to be posted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO FA999.
013370 FA05.
013380       PERFORM AY60 THRU AY999.
010900       PERFORM AY65 THRU AY999.
013390	     MOVE ZERO		 TO WS-PAGE W20-DEBIT W20-CREDIT
				    W20-SDT W20-SCR   W20-YMD.
013400       MOVE "G/L INTEGRATION JOURNALS" 
                                 TO W10-HEAD.
013410       MOVE W05-TODAY      TO W15-DATE.
013420       DISPLAY "Date     :" AT 1210.
013430       DISPLAY "Account  :" AT 1410.
013440       DISPLAY "Narrative:" AT 1610.
013440       DISPLAY "Value    :" AT 1810.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD6    TO W70-RECORD6.

013480 FA10.
      *
      *    ****    Print the audit trail HEADING
      *
	     MOVE "H"		 TO W02-PRN-TYPE.
005460	     MOVE SPACES	 TO R-DET.
005470       MOVE WS-H1          TO R-H1.
005480       MOVE W90-COMP       TO R-H2.
005490       MOVE W10-H2         TO R-H2A.
005500       MOVE WS-H3          TO R-H3.
005510       MOVE WS-H4          TO R-H4.
005520       ADD 1               TO WS-PAGE.
005530       MOVE WS-PAGE        TO R-PGE.
005540       MOVE WS-BATCH       TO R-BTCH.
005550       MOVE W05-TODAY      TO R-DTE1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005570	     MOVE SPACES	 TO R-DET.
013500	     MOVE "ACCNT NO"	 TO R-H5.
013510       MOVE ALL "_"        TO R-H6.
013520       MOVE "ACCOUNT NAME" TO R-H7.
013530       MOVE ALL "_"        TO R-H8.
013540       MOVE "DATE"         TO R-H9.
013550       MOVE "REFERENCE"    TO R-H10.
013560       MOVE ALL "_"        TO R-H11.
013570       MOVE "NARRATION"    TO R-H12.
013580       MOVE ALL "_"        TO R-H13.
013590       MOVE "DEBIT"        TO R-H14.
013600       MOVE "CREDIT"       TO R-H15.
	     PERFORM CALL-PRINTUTL.
013620	     MOVE SPACES	 TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

013630 FA15.
             PERFORM READ-LEDTRF-NEXT THRU READ-LEDTRF-EXIT.
	   IF WS-F-ERROR = 40
	       MOVE 99999999	 TO WS-END
               GO TO FA20.  
	   IF W20-YMD = ZERO
	       MOVE XFR-DATE	 TO W20-YMD.
	   IF W20-YMD NOT = XFR-DATE
	       PERFORM FA20
	       MOVE ZERO	 TO W20-SDT W20-SCR
	       MOVE XFR-DATE	 TO W20-YMD.
	   IF XFR-DATE > WS-END
	       GO TO FA22.
      *
      *    ****    C H E C K   G / L   A C C O U N T   N O .
      *
013710     IF XFR-AC NOT > 100000
	       PERFORM FA25.
013750       MOVE XFR-AC         TO W15-ACCOUNT.
013710	   IF (W15-ACCOUNT NOT > 100000) OR
	      (W15-SER = ZERO)
	       MOVE W70-SUSGL	 TO W15-ACCOUNT.
013710	   IF (W15-ACCOUNT NOT > 100000) OR
              (W15-SER = ZERO)
013730         PERFORM FA30 THRU FA40
	       GO TO FA19.
013940       MOVE ZERO           TO WS-ERROR.
013950       DISPLAY W15-ACCOUNT AT 1422 WITH FOREGROUND-COLOR 11.
008970	     MOVE W15-ACCOUNT	 TO G-AC.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
	       PERFORM FA45.
014040     IF WS-ERROR NOT = ZERO
014060         PERFORM FA30 THRU FA40
014070	       GO TO FA19.
      *
      *    ****    G E N E R A T E   R E F E R E N C E   N O .
      *
	   IF XFR-MM > 9
	       IF XFR-MM = 10
		   MOVE "O"	 TO W15-IMM
	       ELSE
	       IF XFR-MM = 11
		   MOVE "N"	 TO W15-IMM
	       ELSE
		   MOVE "D"	 TO W15-IMM
	       END-IF
	   ELSE
	       MOVE XFR-MM	 TO W15-IMN.
	     MOVE XFR-DD	 TO W15-IDD.
013700       MOVE XFR-LEDG       TO W15-ILEDG.
             MOVE XFR-DATE       TO W15-YMD.
	     MOVE W15-CN	 TO W15-CC.
             MOVE W15-YR         TO W15-YY.
             MOVE W15-MT         TO W15-MM.
             MOVE W15-DY         TO W15-DD.
             MOVE XFR-NAR        TO W15-NAR.
             MOVE XFR-VALUE      TO W15-VAL.

       FA18.
             MOVE W15-DATE       TO W25-DTE.
013450       DISPLAY W25-DATE AT 1221 WITH FOREGROUND-COLOR 11.
014000       DISPLAY G-NAME AT 1430 WITH FOREGROUND-COLOR 11.
             DISPLAY W15-NAR AT 1622 WITH FOREGROUND-COLOR 11.
014110       MOVE W15-VAL        TO W25-V1.
014120       DISPLAY W25-S7V2 AT 1822 WITH FOREGROUND-COLOR 11.
      *
      *    ****    PROCESS AND PRINT TRANSACTION
      *
014130       MOVE G-AC           TO R-ACC.
014140       MOVE G-NAME         TO R-ANME.
014150       MOVE W15-NAR        TO R-NAR.
014160	     MOVE W15-REF	 TO R-REF.
014170       MOVE W15-DATE       TO R-DTE2.
014180     IF W15-VAL < ZERO
014190         MOVE W15-VAL      TO R-CRD
014200     ELSE
014210         MOVE W15-VAL      TO R-DBT.
             INITIALIZE T-REC.
012680       MOVE W15-YMD        TO T-DATE.
014370       MOVE W15-NAR        TO T-NARATIVE.
014380	     MOVE W15-REF	 TO T-REF.
014390       MOVE W15-VAL        TO T-VAL.
014420       MOVE G-AC           TO T-AC.
014430     IF W15-VAL < ZERO
014440	       ADD W15-VAL	 TO W20-CREDIT W20-SCR
014450         ADD W15-VAL       TO G-CR
014460         MOVE 02           TO T-CODE 
014470     ELSE
014480	       ADD W15-VAL	 TO W20-DEBIT W20-SDT
014490         ADD W15-VAL       TO G-DT
014500         MOVE 01           TO T-CODE.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
	     MOVE "N"		 TO T-FLAG.
             PERFORM AE000.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
014510       ADD W15-VAL         TO G-BAL.
014240       PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
014540       PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
015750	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014610	       PERFORM FA10.
014620	     MOVE SPACES	 TO R-DET.

       FA19.
	     DELETE LEDTRF.
             GO TO FA15.

016960 FA20.
	     ADD W20-SDT W20-SCR GIVING W15-VAL.
	   IF W15-VAL NOT = ZERO
	       MOVE W70-SUSGL	 TO W15-ACCOUNT G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
021840	       IF WS-F-ERROR = 0
		   MOVE "Balancing entry (INTEGRATION)"
				 TO W15-NAR
		   IF W20-MT > 9
		       IF W20-MT = 10
			   MOVE "O"
				 TO W15-IMM
		       ELSE
		       IF W20-MT = 11
			   MOVE "N"
				 TO W15-IMM
		       ELSE
			   MOVE "D"
				 TO W15-IMM
		       END-IF
		   ELSE
		       MOVE W20-MT
				 TO W15-IMM
		   END-IF
		   MOVE W20-DY	 TO W15-IDD
013700		   MOVE 9999	 TO W15-ILEDG
		   MOVE W20-YMD  TO W15-YMD
		   MOVE W15-CN	 TO W15-CC
		   MOVE W15-YR	 TO W15-YY
		   MOVE W15-MT	 TO W15-MM
		   MOVE W15-DY	 TO W15-DD
		   MULTIPLY -1	 BY W15-VAL
		   PERFORM FA18.
       FA22.
016970       PERFORM AY70 THRU AY999.
             MOVE "Y"            TO WS-SKIP.
015750	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017060	       PERFORM FA10.
017070	     MOVE "*****   T O T A L S   *****"
                                 TO R-NAR.
017080       MOVE W20-DEBIT      TO R-DBT.
017090       MOVE W20-CREDIT     TO R-CRD.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF WS-END = 99999999
	       CLOSE LEDTRF
	       OPEN OUTPUT LEDTRF
	       CLOSE LEDTRF
	       OPEN I-O LEDTRF.
017210       GO TO FA999.

       FA25.
           IF XFR-TYPE = 01
               MOVE W70-DEBGL    TO XFR-AC
           ELSE
           IF XFR-TYPE = 02
               MOVE W70-CREGL    TO XFR-AC
           ELSE
           IF XFR-TYPE = 03
               MOVE W70-INTGL    TO XFR-AC
           ELSE
           IF (XFR-TYPE > 03 AND < 10) OR
              (XFR-TYPE > 14 AND < 17) OR
              (XFR-TYPE > 18 AND < 21) OR
	      (XFR-TYPE = 28) OR
	      (XFR-TYPE > 34 AND < 37) OR
	      (XFR-TYPE > 38 AND < 40)
	       MOVE XFR-LEDG	 TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               IF XFR-TYPE = 04 OR 05
                   MOVE DPT-SALE-GL  TO XFR-AC
               ELSE
	       IF XFR-TYPE = 06 OR 07 OR 19 OR 20 OR 28	OR 35 OR 36
		   MOVE DPT-PUR-GL   TO XFR-AC
               ELSE
               IF XFR-TYPE = 08 OR 09
                   MOVE DPT-COST-GL  TO XFR-AC
               ELSE
               IF XFR-TYPE = 15 OR 16
                   MOVE DPT-DISC-GL  TO XFR-AC
               END-IF
           ELSE
           IF XFR-TYPE > 09 AND < 14 
	       MOVE "VAT1"	 TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               IF XFR-TYPE = 10 OR 12
		   MOVE DPT-O-VAT-GL
				 TO XFR-AC
               ELSE
               IF XFR-TYPE = 11 OR 13
		   MOVE DPT-I-VAT-GL
				 TO XFR-AC
               END-IF
           ELSE
           IF XFR-TYPE = 14
000040	       ADD 21 XFR-TRN	 GIVING W30-RESULT
000050         DIVIDE W30-RESULT BY 2
000060                           GIVING W30-TRNKEY
000070                           REMAINDER W30-TS
000080         ADD 1             TO W30-TS
000110         MOVE W30-TRNKEY   TO WS-PARKEY
000120         PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE PAR-T-GL (W30-TS)
                                 TO XFR-AC
	   ELSE
           IF XFR-TYPE = 21 OR 22
               MOVE W70-RLGL     TO XFR-AC
           ELSE
           IF XFR-TYPE = 23 OR 24
               MOVE W70-DSGL     TO XFR-AC
           ELSE
           IF XFR-TYPE = 25
	       ADD 381 XFR-TRN	 GIVING W30-RESULT
               DIVIDE W30-RESULT BY 2
                                 GIVING W30-TRNKEY
                                 REMAINDER W30-TS
               ADD 1             TO W30-TS
               MOVE W30-TRNKEY   TO WS-PARKEY
000120         PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE PAR-C-GL (W30-TS)
                                 TO XFR-AC
           ELSE
           IF XFR-TYPE = 26 OR 29
               MOVE 7            TO WS-PARKEY
000120         PERFORM READ-PARAM THRU READ-PARAM-EXIT
               MOVE PAR-CASH-GL  TO XFR-AC
           ELSE
           IF XFR-TYPE = 27
               MOVE W70-ADJGL    TO XFR-AC
           ELSE
	   IF XFR-TYPE = 30
	       MOVE W70-DCNGL	 TO XFR-AC
	   ELSE
	   IF XFR-TYPE = 31 OR 32
	       MOVE W70-CSLGL	 TO XFR-AC
	   ELSE
	   IF XFR-TYPE = 33 OR 34
	       MOVE W70-CONGL	 TO XFR-AC
	   ELSE
	       MOVE W70-SUSGL	 TO XFR-AC.
017220 FA30.
           IF XFR-TYPE = 01
017300         MOVE "** INVALID G/L ACCOUNT FOR DEBTORS" TO R-DET
017320         GO TO FA35
017330     ELSE
           IF XFR-TYPE = 02
017400         MOVE "** INVALID G/L ACCOUNT FOR CREDITORS" TO R-DET
017420         GO TO FA35
017330     ELSE
           IF XFR-TYPE = 03
017400         MOVE "** INVALID G/L ACCOUNT FOR BAD DEBTS" TO R-DET
017420         GO TO FA35
017430     ELSE
           IF XFR-TYPE = 04 OR 05
017240         MOVE "** INVALID G/L ACCOUNT FOR SALES - SALES LEDGER:"
017250             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017270         GO TO FA35
           ELSE
           IF XFR-TYPE = 06 OR 07 OR 19 OR 20 OR 28
017240         MOVE "** INVALID G/L ACCOUNT FOR STOCK - SALES LEDGER:"
017250             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017270         GO TO FA35
           ELSE
           IF XFR-TYPE = 08 OR 09
017240         MOVE "** INVALID G/L ACCOUNT FOR COSTS - SALES LEDGER:"
017250             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017270         GO TO FA35
           ELSE
           IF XFR-TYPE = 10 OR 12 
017400         MOVE "** INVALID G/L ACCOUNT FOR OUTPUT VAT" TO R-DET
017420         GO TO FA35
017330     ELSE
           IF XFR-TYPE = 11 OR 13 
017400         MOVE "** INVALID G/L ACCOUNT FOR INPUT VAT" TO R-DET
017420         GO TO FA35
017280     ELSE
           IF XFR-TYPE = 14
017450         MOVE "** INVALID G/L ACCOUNT FOR DEBTOR TRANSACTION:"
017360             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017370         GO TO FA35
           ELSE
           IF XFR-TYPE = 15 OR 16
017240         MOVE "** INVALID G/L ACCOUNT FOR DISCOUNT - SALES LEDGER"
017250             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017270         GO TO FA35
           ELSE
           IF XFR-TYPE = 17 OR 18
017450         MOVE "** INVALID G/L ACCOUNT FOR AD-VALORM TAX" TO R-DET
               GO TO FA35
017380     ELSE
017440     IF XFR-TYPE = 21 OR 22
017450         MOVE "** INVALID G/L ACCOUNT FOR CREDITOR FREIGHT" TO 
                   R-DET
017470         GO TO FA35
017380     ELSE
017440     IF XFR-TYPE = 23 OR 24
017450         MOVE "** INVALID G/L ACCOUNT FOR CREDITOR DISCOUNT" TO
017460             R-DET
017470         GO TO FA35
017430     ELSE
           IF XFR-TYPE = 25
017450         MOVE "** INVALID G/L ACCOUNT FOR CREDITOR TRANSACTION:"
017360             TO R-DET
017260         MOVE XFR-LEDG     TO R-REF
017370         GO TO FA35
017430     ELSE
017440     IF XFR-TYPE = 26
017450         MOVE "** INVALID G/L ACCOUNT FOR CASH SALES" TO R-DET
017470         GO TO FA35
017430     ELSE
017440     IF XFR-TYPE = 27
017450         MOVE "** INVALID G/L ACCOUNT FOR STOCK ADJUSTMENTS" TO 
                   R-DET
017470         GO TO FA35
017430     ELSE
017440	   IF XFR-TYPE = 39 OR 40
017450	       MOVE "** INVALID G/L ACCOUNT FOR REDUNDANT STOCK" TO
017460             R-DET
017470         GO TO FA35
	   ELSE
               MOVE "** UNKNOWN TRANSACTION TYPE:" TO R-DET
               MOVE XFR-TYPE     TO R-REF.
017620 FA35.
014180     IF XFR-VALUE < ZERO
014190         MOVE XFR-VALUE    TO R-CRD
               MOVE "["          TO R-CBRKO
               MOVE "]"          TO R-CBRKC
014200     ELSE
014210         MOVE XFR-VALUE    TO R-DBT
               MOVE "["          TO R-DBRKO
               MOVE "]"          TO R-DBRKC.
	     MOVE XFR-NAR	 TO R-NAR.
015750	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
017670	       PERFORM FA10.
017680	     MOVE SPACES	 TO R-DET.
017690	     MOVE ZERO		 TO WS-ERROR.
017700 FA40.
017710       EXIT.

       FA45.
	   IF XFR-AC NOT = W70-SUSGL
	       MOVE W70-SUSGL	 TO XFR-AC W15-ACCOUNT
	       MOVE ZERO	 TO WS-ERROR
008970	       MOVE W15-ACCOUNT	 TO G-AC
008980	       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
021840	       IF WS-F-ERROR = 8
		   MOVE 1	 TO WS-ERROR
	       END-IF
	   ELSE
	       MOVE 9		 TO WS-ERROR.

017720 FA999.
017730       EXIT.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.

042170 ZA02.
044300	     OPEN I-O LEDTRF.
044310	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 40       TO WS-F-ERROR
                   GO TO ZA50.
050190     IF WS-STATUS NOT = "00"
050200         MOVE 40           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
022190	     MOVE 1		 TO WS-PARKEY.
022200       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022210	     MOVE PAR-COMPANY	 TO W90-CNAME.
022220	     MOVE PAR-DMY	 TO W05-TODAY.
022230	     MOVE PAR-YMD	 TO W05-TYMD.
022250	     MOVE 6		 TO WS-PARKEY.
022260       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022270	     MOVE PAR-RECORD6	 TO W70-RECORD6.
	     MOVE 4		 TO WS-NETKEY.
035770       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
035900     IF G-YEAR-END = ZERO
035910	       MOVE 2		 TO G-YEAR-END
035920	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
035930	     MOVE 1		 TO WS-S1.
035940	     MOVE G-YEAR-END	 TO WS-S2 WS-Y-END.
035160	     MOVE G-LAST-YE	 TO WS-L-END.
	   IF WS-L-END = ZERO
	       SUBTRACT 1 FROM W05-YY
				 GIVING WS-L-END.
	   IF G-OPEN = ZERO
	       MOVE WS-Y-END	 TO G-OPEN.
	     MOVE G-OPEN	 TO WS-OPEN.
022440	     MOVE ZERO		 TO WS-ERROR.
022450     IF W70-BNKGL = ZERO
022460         DISPLAY (20, 20) "Parameters not set up - ENTER"
022470                           WITH FOREGROUND-COLOR 14
022480         ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO
022490	       MOVE 1		 TO WS-ERROR.
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
034750       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
034760       MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
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
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
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
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE XFR-KEY      TO WS-KEYX.
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
