      $SET LINKCOUNT"312"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	      **       **      *
      *   **	**  **	  **  **    **	 **  **      ***      ***      *
      *   **	**  **	  **  **    **	**    **      **       **      *
      *   *******   ******    *******	**    **      **       **      *
      *   **	    **	 **   **	**    **      **       **      *
      *   **	    **	  **  **	 **  **       **       **      *
      *   **	    **	  **  **	  ****	    ******   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   D E B T O R   T R A N S A C T I O N S   *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP001.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2010
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

000120 COPY CONTROL.SL.

000160 COPY GACCNT.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000320 COPY GACCNT.FDE.

000220 COPY PARAM.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000950 77  WS-S1	   PIC	9(04)	 COMP-5.
000960 77  WS-S2	   PIC	9(04)	 COMP-5.
000970 77  WS-S3	   PIC	9(04)	 COMP-5.
000980 77  WS-S4	   PIC	9(04)	 COMP-5.
000990 77  WS-S5	   PIC	9(04)	 COMP-5.
001000 77  WS-S6	   PIC	9(04)	 COMP-5.
001000 77  WS-S7	   PIC	9(04)	 COMP-5.
001000 77  WS-S8	   PIC	9(04)	 COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	 COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001010 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-PRNKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
       77  WS-ADV-RATE	   PIC	S9(03)V99 COMP-3.
001090 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
001900 77  WS-OPEN	   PIC	9(02) VALUE ZERO.
001100 77  WS-OPTION	   PIC	X(01).
001100 77  WS-VAT-GST	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-TYPE	   PIC	X(01).
001220 77  WS-BATCH	   PIC	9(05).
002190 77  WS-ER6	   PIC	X(09) VALUE "No Record".
002200 77  WS-ER7          PIC  X(13) VALUE "Group Heading".
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

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "PRP".
	   03  WS-PROG	       PIC  X(03) VALUE "011".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

       COPY PARAM.ID.

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
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).
000260	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000270	       05  REP-DETAIL1 PIC  X(136).
000280	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000290	       05  REP-DETAIL2 PIC  X(80).
000300	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000310	       05  REP-PAGE    PIC  X(06).
000320	       05  REP-P-NO    PIC  Z(03)9.
000330	       05  FILLER      PIC  X(09).
000340	       05  REP-COMPANY PIC  X(40).
000350	       05  FILLER      PIC  X(05).
000360	       05  REP-DATE    PIC  X(06).
000370	       05  REP-DTE     PIC  Z9/99/9999.
000380	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000390	       05  FILLER      PIC  X(11).
000400	       05  REP-HEAD1   PIC  X(04).
000410	       05  FILLER      PIC  X(06).
000420	       05  REP-HEAD2   PIC  X(15).
000430	       05  FILLER      PIC  X(06).
000440	       05  REP-HEAD3   PIC  X(15).
000450	       05  FILLER      PIC  X(06).
000460	       05  REP-HEAD4   PIC  X(06).
000470	       05  FILLER      PIC  X(11).
000480	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
000490	       05  FILLER      PIC  X(11).
000500	       05  REP-TRAN    PIC  Z99.
000510	       05  FILLER      PIC  X(09).
000520	       05  REP-ENG     PIC  X(12).
000530	       05  FILLER      PIC  X(09).
000540	       05  REP-AFR     PIC  X(12).
000550	       05  FILLER      PIC  X(10).
000560	       05  REP-ACT     PIC  X(01).
000570	       05  FILLER      PIC  X(13).
000580	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
000590	       05  FILLER      PIC  X(06).
000600	       05  REP-HEAD5   PIC  X(07).
000610	       05  REP-BATCH   PIC  Z(04)9.
000620	       05  FILLER      PIC  X(04).
000630	       05  REP-HEAD6   PIC  X(20).
000640	       05  FILLER      PIC  X(38).
000650	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
000660	       05  FILLER      PIC  X(02).
000670	       05  REP-HEAD7   PIC  X(12).
000680	       05  REP-HEAD8   PIC  X(09).
000690	       05  REP-HEAD9   PIC  X(07).
000700	       05  REP-HEAD10  PIC  X(16).
000710	       05  REP-HEAD11  PIC  X(10).
000720	       05  REP-HEAD12  PIC  X(13).
000730	       05  REP-HEAD13  PIC  X(11).
000740	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
000750	       05  FILLER      PIC  X(03).
000760	       05  REP-TAC     PIC  9(05).
000770	       05  FILLER      PIC  X(04).
000780	       05  REP-TDTE    PIC  X(08).
000790	       05  FILLER      PIC  X(04).
000800	       05  REP-TCDE    PIC  9(02).
000810	       05  FILLER      PIC  X(04).
000820	       05  REP-TDESC   PIC  X(12).
000830	       05  FILLER      PIC  X(03).
000840	       05  REP-TREF    PIC  X(06).
000850	       05  FILLER      PIC  X(03).
000860	       05  REP-TDB     PIC  Z(06)9.99.
000870	       05  FILLER      PIC  X(03).
000880	       05  REP-TCR     PIC  Z(06)9.99.
000890	       05  FILLER      PIC  X(03).
000900	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
000910	       05  FILLER      PIC  X(54).
000920	       05  REP-BTOT    PIC  Z(06)9.99-.
000930	       05  FILLER      PIC  X(15).

       COPY W12.WS.

002660 01  W15-INPUT.
002410	   03  W15-ACNT    PIC	9(06).
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

001890 01  W25-CALCS.
001900     03  W25-RESULT     PIC 9(07)V99.
001910     03  W25-RESULT1 REDEFINES W25-RESULT.
001920         05  FILLER      PIC 9(01).
001930         05  W25-WHOLE  PIC 9(06).
001940         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(06).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
001950     03  W25-PASS.
001960         05  W25-PASS1  PIC 9(03).
001970         05  W25-PASS2  PIC 9(03).
001980     03  W25-TIME       PIC 9(08).
001990     03  W25-TRED REDEFINES W25-TIME.
002000         05  FILLER      PIC 9(02).
002010         05  W25-FACT   PIC 9(06).
002020     03  W25-TOTAL      PIC S9(07)V99  COMP-3.
002030     03  W25-VALUE      PIC S9(07)V99  COMP-3.

002150 01  W40-COMPANY.
002160     02  W40-NAME1.
002170         03  W40-NAME   PIC X(40).
002180         03  FILLER      PIC X(32).
002190     02  W40-NAME2 REDEFINES W40-NAME1.
002200         03  W40-SURNAME PIC X(24).
002210         03  W40-FNAME  PIC X(24).
002220         03  W40-SNAME  PIC X(24).
002230     02  W40-NAME3 REDEFINES W40-NAME1.
002240         03  W40-NCHAR OCCURS 72 PIC X(01).
002250     02  W40-ADDRESS1.
002260         03  W40-ADD1   PIC X(30).
002270         03  W40-ADD2   PIC X(30).
002280         03  W40-ADD3   PIC X(30).
002290         03  W40-ADD4   PIC X(30).
002300     02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
002310         03  W40-ACHAR OCCURS 120 PIC X(01).
002320     02  W40-POST       PIC 9(04).
002330     02  W40-TEL        PIC X(14).
002340     02  W40-COST-CODE  PIC X(10).
           02  W40-VATNO      PIC X(14).

       COPY W40.WS.

002370 01  W45-TRAN.
002380     03  W45-CODE       PIC  9(02).
002390     03  W45-ENG        PIC  X(12).
002400     03  W45-AFR        PIC  X(12).
002410     03  W45-ACT        PIC  X(01).
      *    03  W45-E-ABRV     PIC  X(03).
      *    03  W45-A-ABRV     PIC  X(03).
           03  W45-T-GL       PIC  9(06).
002370 01  W46-TRAN.
002380     03  W46-CODE       PIC  9(02).
002390     03  W46-DESC       PIC  X(15).
           03  W46-ABRV       PIC  X(03).
002410     03  W46-ACT        PIC  X(01).
           03  W46-T-GL       PIC  9(06).

       COPY FUNCTION.WS.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02)    COMP-5.
	   03  W75-S1	      PIC  9(02)    COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
	   03  W75-CODE       PIC  Z9.
	   03  W75-GNO	      PIC 9(06) COMP-5
                              OCCURS 18.

002830 01  W80-ACCOUNT.
002840     03  W80-ACNO       PIC X(06).
002850     03  W80-NUMD REDEFINES W80-ACNO.
002860         05  W80-COMP   PIC 9(01).
002870         05  W80-TYPE   PIC 9(01).
002880         05  W80-SERIAL PIC 9(04).
           03  W80-CASH-GL    PIC 9(06).

       01  W90-CALC-KEYS.
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **    **  **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
003150 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY TRNLUP.CRT.

       01  S10.
003530	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
003540				 VALUE "DEBTOR TRANSACTION OPTIONS".
	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "M".
	   03	       COLUMN 13 VALUE "aintain, ".
	   03	       COLUMN 22 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "P".
	   03	       COLUMN 23 VALUE "rint or ".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 32 VALUE "xit ? > <".

004090 01  S13.
004100	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 15
004110				 VALUE "DEBTOR TRANSACTION CODES".
004120	   03  LINE  4 COLUMN 20 VALUE "Code (01 to 99)".
004130	   03  LINE  5 COLUMN 20 VALUE "Eng Description".
004140	   03  LINE  6 COLUMN 20 VALUE "Afr Description".
004150	   03  LINE  7 COLUMN 20 VALUE "Debit / Credit ".
	   03  LINE  8 COLUMN 20 VALUE "G/L Account No.".

005220 01  S26.
005230     03  S26A.
005240	       05  LINE 4 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC Z9
005250                              USING W45-CODE AUTO.
005260	   03  S26AB.
005260	     04	 S26B.
005270	       05  LINE 5 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC X(12)
005280                              USING W45-ENG AUTO.
005290	       05  LINE 6 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC X(12)
005300                              USING W45-AFR AUTO.
005310	       05  LINE 7 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC X(01)
005320                              USING W45-ACT AUTO.
005260	     04	 S26C.
	       05  LINE 8 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC ZZZZZ9
                                    USING W45-T-GL AUTO.

       01  S26D.
005270	       05  LINE 5 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC X(12)
005280                              USING W45-ENG AUTO.
005290	       05  LINE 6 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				    BACKGROUND-COLOR 4 PIC X(12)
005300                              USING W45-AFR AUTO.

       COPY S99.CRT.

       COPY LEDLUP.CRT.

       COPY ERROR.CRT.

      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006220 AA000-MAIN	       SECTION.
006230 AA000-INIT.
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	   IF LS0-LEV > 0
	       CLOSE GACCNT.

       AA49.
             EXIT PROGRAM.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       COPY ABORT.PRN.

       AA200              SECTION.
       AA201.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1 TO CLIN.
       AA205.
             EXIT.

       COPY APAC.HLP.

       AA350-LOOKUP    SECTION.
       AA350.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM GACCNT-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA399.
             EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY GACCNT.RD.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY DBTRAN.LUP.

       COPY GACCNT.LUP.

       BA000	       SECTION.
       BA00.
007330	     DISPLAY CLR-SCREEN.
007340       DISPLAY S10.
	     MOVE "M"		 TO WS-OPTION.
       BA05.
	     ACCEPT WS-OPTION AT 0439
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "M" OR "P")
	       GO TO BA05.
	   IF WS-OPTION = "M"
	       PERFORM YA000
	   ELSE
	   IF WS-OPTION = "P"
	       PERFORM YB000
	   ELSE
	       GO TO BA999.
	     GO TO BA00.
       BA999.
	     EXIT.

       YA000	       SECTION 50.
009360 YA90-TRAN-CODES.
009370       DISPLAY CLR-SCREEN.
009380       DISPLAY S13.
009390 YA95-ACCEPT.
	     DISPLAY "F6" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3
		     "=View Transaction Codes, "
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
009400	     MOVE ZERO		 TO W45-CODE.
009410       ACCEPT S26A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM TRANCDE-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO YA95-ACCEPT
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA95-ACCEPT
	       END-EVALUATE.
	     DISPLAY CLEAR-L25.
009420	   IF W45-CODE = ZERO
009430	       GO TO YA999.
009440     IF W45-CODE < 01 OR > 99
009450	       MOVE "Invalid code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
009470	       GO TO YA95-ACCEPT.
             ADD 21 W45-CODE     GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-TRNKEY
                                 REMAINDER W90-TS.
             ADD 1               TO W90-TS.
             MOVE W90-TRNKEY     TO WS-PARKEY
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009500       MOVE PAR-E-DESC (W90-TS)
                                 TO W45-ENG.
009510       MOVE PAR-A-DESC (W90-TS)
                                 TO W45-AFR.
009520     IF PAR-ACTION (W90-TS) = 1
009530         MOVE "C"          TO W45-ACT
009540     ELSE
009550         MOVE "D"          TO W45-ACT.
             MOVE PAR-T-GL (W90-TS)
                                 TO W45-T-GL.
009560	     DISPLAY S26AB.

009660 YA100-DESC.
019250	     DISPLAY CLEAR-L25.
           IF (W45-CODE > 00 AND < 07) OR 
              (W45-CODE > 12 AND < 21)
	       ACCEPT S26D
           ELSE
009670         ACCEPT S26B.
	     CALL "CBL_TOUPPER" USING W45-ACT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
009730	   IF W45-ACT < "C" OR > "D"
009740	       MOVE "Invalid Action code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
009760	       GO TO YA100-DESC.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Account Lookup - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       YA105.
	     ACCEPT S26C.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE W45-T-GL
				 TO W15-ACCOUNT
		     PERFORM AA350
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA105
	       END-EVALUATE
	       MOVE W15-ACCOUNT  TO W45-T-GL
	       DISPLAY S26C
	       IF W45-T-GL = ZERO
015990		   GO TO YA105
	       END-IF
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO YA100-DESC
	       END-EVALUATE.
019780	   IF W45-T-GL = ZERO
019790	       GO TO YA105.
008970	     MOVE W45-T-GL	 TO W15-ACCOUNT	G-AC.
013830	   IF W15-ASER = ZERO
013840	       MOVE WS-ER7	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013850	       GO TO YA105.
008980       PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
021840	   IF WS-F-ERROR = 8
019820	       MOVE WS-ER6	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019830	       GO TO YA105.
019250	     DISPLAY CLEAR-L25.
	     PERFORM CHECK-CORRECT.
009850	   IF WS-OPTION = "N"
009860         GO TO YA100-DESC.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
009870       MOVE W45-ENG        TO PAR-E-DESC (W90-TS).
009880       MOVE W45-AFR        TO PAR-A-DESC (W90-TS).
009890       MOVE W45-CODE       TO PAR-T-CODE (W90-TS).
009900     IF W45-ACT = "C"
009910         MOVE 1            TO PAR-ACTION (W90-TS)
009920     ELSE
009930         MOVE 0            TO PAR-ACTION (W90-TS).
009870       MOVE W45-T-GL       TO PAR-T-GL (W90-TS).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
009940       GO TO YA90-TRAN-CODES.

       YA999.
	     EXIT.

       YB000	       SECTION 51.
       YB00.
	     DISPLAY "Busy Printing" AT 1212
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT BLINK.

      *
      *    ****   O P E N   P R I N T E R   F I L E
      *
	     MOVE "O"		 TO WS-COMMAND.
	     MOVE "P"		 TO WS-PRINTER.
	     MOVE ZERO		 TO WS-ADVANCE
				    WS-REPORT
				    WS-SCREEN.
	     MOVE 3		 TO WS-REPORT.

       YB10.
      *
      *    ****   C A L L   T H E   P R I N T / S P O O L   U T I L I T Y
      *
	     CALL "UTP\PRINTUTL" USING WS-PARID
				       LS-USER-ID
				       WS-SCREEN-PRINT
				       W02-PRINTER-DETAILS
		  ON EXCEPTION
		      MOVE "Print/Spool Utility problem"
				 TO WS-ERR-STRING
003260		      PERFORM ERROR-MESSAGE
		      GO TO YB999
	     END-CALL.


015260 YB100-TRANS.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "H"		 TO W02-PRN-TYPE.
015280       MOVE SPACES         TO REP-DETAIL1.
015290       MOVE "PAGE:"        TO REP-PAGE.
             ADD  1              TO WS-PAGE.
015300       MOVE WS-PAGE        TO REP-P-NO.
015310       MOVE "DATE:"        TO REP-DATE.
015320       MOVE W12-TODAY      TO REP-DTE.
015330	     MOVE "DEBTOR TRANSACTION CODES"
				 TO REP-COMPANY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM YB10.
015400*      WRITE REP-LINE4 BEFORE 2 LINES.
015350       MOVE SPACES         TO REP-DETAIL2.
015360       MOVE "CODE"         TO REP-HEAD1.
015370       MOVE "ENG DESCRIPTION"
                                 TO REP-HEAD2.
015380       MOVE "AFR DESCRIPTION" 
                                 TO REP-HEAD3.
015390       MOVE "ACTION"       TO REP-HEAD4.
	     PERFORM YB10.
015400*      WRITE REP-LINE4 BEFORE 2 LINES.
015410       MOVE SPACES         TO REP-DETAIL2.
	     MOVE "D"		 TO W02-PRN-TYPE.

       YB105.
             MOVE 11             TO WS-PARKEY.

015420 YB110-LOOP.
014890       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1              TO W90-TS.

       YB112.
015430	   IF PAR-T-CODE (W90-TS) = ZERO OR
	     (PAR-E-DESC (W90-TS) = SPACES AND
	      PAR-A-DESC (W90-TS) = SPACES)
015440         GO TO YB120-CHECK-END.
015450       MOVE PAR-T-CODE (W90-TS) 
                                 TO REP-TRAN.
015460       MOVE PAR-E-DESC (W90-TS)
                                 TO REP-ENG.
015470       MOVE PAR-A-DESC (W90-TS)
                                 TO REP-AFR.
015480     IF PAR-ACTION (W90-TS) = 1
015490         MOVE "C" TO REP-ACT
015500     ELSE
015510         MOVE "D" TO REP-ACT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM YB10
015520* 	WRITE REP-LINE5 BEFORE 2 LINES
           ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM YB10
015520* 	WRITE REP-LINE5 BEFORE PAGE
               PERFORM YB100-TRANS.
015530       MOVE SPACES TO REP-DETAIL2.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
                                 TO REP-DETAIL2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM YB10
014980* 	WRITE REP-LINE1 BEFORE PAGE
014990	       GO TO YB130.

015540 YB120-CHECK-END.
015550     IF W90-TS < 2
015560         ADD 1             TO W90-TS
015570         GO TO YB112.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
015570         GO TO YB110-LOOP.
015580 YB125.
015590       MOVE "**** END REPORT ****" 
                                 TO REP-COMPANY.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM YB10.
015600*       WRITE REP-LINE3 BEFORE PAGE.

       YB130.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM YB10.

       YB999.
	     EXIT.
      /
015920 ZA000-INIT	       SECTION 80.
015930 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
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
      *    UP AND DOWN ARROW KEYS
      *
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE 9		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-GST    TO WS-VAT-GST.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
	     MOVE W12-MONTH	 TO WS-OPEN.
	   IF LS0-LEV > 0
	       OPEN INPUT GACCNT.
044780	     GO TO ZA999-EXIT.
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
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950	     MOVE WS-G3 	 TO WS-MCHR (11) WS-MCHR (45)
021960				    WS-MCHR (63)
				    WS-MCHR (1)	 WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2312
020130                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
020140       DISPLAY "Press any key" AT 2512 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.

016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 81.
      
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
      
041560 DISPLAY-FILE-NAME.
041810	   IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
