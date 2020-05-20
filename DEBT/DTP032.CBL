      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **	**	 **    *
      *   **	**     **     *******	**    **     ****      **      *
      *   **	**     **     **	**    **	**   **        *
      *   **	**     **     **	 **  **   **	**  **	       *
      *   *******      **     **	  ****	   ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R	  C O N T R A C T   R E P O R T S	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      *								       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP032.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   February 1985.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1985 - 2011
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

000140 COPY DEBTOR.ISL.

000160 COPY DBFOTO.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000250 COPY PARAM.FDE.

000270 COPY DEBTOR.FDE.

000290 COPY DBFOTO.FDE.

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
002850 77  WS-IXS1         PIC 9(04)     COMP-5.
002860 77  WS-IXS2         PIC 9(04)     COMP-5.
002870 77  WS-IXS3         PIC 9(04)     COMP-5.
002880 77  WS-IXS4         PIC 9(04)     COMP-5.
002890 77  WS-MSUB         PIC 9(04)     COMP-5.
002900 77  WS-PARKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEPKEY       PIC 9(04)     COMP-5.
002910 77  WS-DEBKEY       PIC 9(06)     COMP-5.
002930 77  WS-NETKEY       PIC 9(06)     COMP-5.
002940 77  WS-WRKEY        PIC 9(04)     COMP-5.
002950 77  WS-GST-RATE     PIC S9(03)V99 COMP-3.
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
002960 77  WS-NTP	   PIC 9(01).
002970 77  WS-LINES        PIC 9(02).
002980 77  WS-PAGE         PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002990 77  WS-OPTION	   PIC X(01).
002990 77  WS-AGE          PIC X(01).
002990 77  WS-SEQ          PIC X(01).
003000 77  WS-CREDIT       PIC X(01).
002990 77  WS-ZERO         PIC X(01).
       77  WS-NUM          PIC Z9.
003020 77  WS-ADDR         PIC X(01).
003030 77  WS-ERROR        PIC 9(01).
003040 77  WS-ERROR1       PIC 9(01).
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003110 77  WS-EXPAND	   PIC X(01)	 VALUE " ".
003180 77  WS-COMP         PIC 9(01).
003180 77  WS-IND	   PIC 9(01).
003190 77  WS-TYPE         PIC X(01).
003200 77  WS-HEAD         PIC X(01).
003240 77  WS-MESS         PIC X(02).
003250 77  WS-ACNO         PIC X(06).
003260 77  WS-S1           PIC 9(04)     COMP-5.
003270 77  WS-S2           PIC 9(04)     COMP-5.
003280 77  WS-S3           PIC 9(04)     COMP-5.
003290 77  WS-S4           PIC 9(04)     COMP-5.
003300 77  WS-S5           PIC 9(04)     COMP-5.
003310 77  WS-S6           PIC 9(04)     COMP-5.
003320 77  WS-S7           PIC 9(04)     COMP-5.
003330 77  WS-S8           PIC 9(04)     COMP-5.
003410 77  WS-O 	   PIC X(08) VALUE "INTERNAL".
003420 77  WS-AC           PIC X(07) VALUE "Account".
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

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

       COPY DBFOTO.ID.

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

000350	   03  R-L1 REDEFINES W02-PRN-LINE.
000360	       05  R-DET       PIC  X(136).

000370	   03  R-L2 REDEFINES W02-PRN-LINE.
000380	       05  R-DET2      PIC  X(80).

	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
	       05  REP-PAGE	   PIC X(06).
	       05  REP-P-NO	   PIC Z(03)9.
	       05  FILLER	   PIC X(24).
	       05  REP-RESTART.
		   07  REP-COMPANY PIC X(40).
		   07  REP-HEAD    PIC X(24).
	       05  FILLER	   PIC X(19).
	       05  REP-DATE	   PIC X(06).
	       05  REP-DTE	   PIC Z9/99/99.

	   03  REP-LINE3A REDEFINES W02-PRN-LINE.
	       05  REP-EXPP	   PIC	X(04).
	       05  REP-CO	   PIC	X(40).

	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(11).
	       05  REP-HEAD1	   PIC X(04).
	       05  FILLER	   PIC X(06).
	       05  REP-HEAD2	   PIC X(15).
	       05  FILLER	   PIC X(06).
	       05  REP-HEAD3	   PIC X(15).
	       05  FILLER	   PIC X(06).
	       05  REP-HEAD4	   PIC X(06).
	       05  FILLER	   PIC X(11).

	   03  REP-LINE5 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(12).
	       05  REP-TRAN	   PIC 9(02).
	       05  FILLER	   PIC X(09).
	       05  REP-ENG	   PIC X(12).
	       05  FILLER	   PIC X(09).
	       05  REP-AFR	   PIC X(12).
	       05  FILLER	   PIC X(10).
	       05  REP-ACT	   PIC X(01).
	       05  FILLER	   PIC X(13).

	   03  REP-LINE6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(06).
	       05  REP-HEAD5	   PIC X(07).
	       05  REP-BATCH	   PIC Z(04)9.
	       05  FILLER	   PIC X(04).
	       05  REP-HEAD6	   PIC X(20).
	       05  FILLER	   PIC X(38).

	   03  REP-LINE7 REDEFINES W02-PRN-LINE.
	       05  REP-HEAD7	   PIC X(07).
	       05  REP-HEAD8	   PIC X(39).
	       05  REP-HEAD9	   PIC X(09).
	       05  REP-HEAD10	   PIC X(30).
	       05  REP-HEAD11	   PIC X(11).
	       05  REP-HEAD12	   PIC X(05).
	       05  REP-HEAD13	   PIC X(07).
	       05  REP-HEAD14	   PIC X(07).
	       05  REP-HEAD15	   PIC X(09).
	       05  REP-HEAD16	   PIC X(10).

	   03  REP-LINE8 REDEFINES W02-PRN-LINE.
	       05  REP-AC	   PIC	X(06).
	       05  FILLER	   PIC	X(01).
	       05  REP-NAME	   PIC	X(38).
	       05  FILLER	   PIC	X(01).
	       05  REP-CONT	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  REP-DESC	   PIC	X(29).
	       05  FILLER	   PIC	X(01).
	       05  REP-INST	   PIC	Z(06)9.99 BLANK ZERO.
	       05  FILLER	   PIC	X(01).
	       05  REP-TYPE	   PIC	X(04).
	       05  FILLER	   PIC	X(01).
	       05  REP-FREQ	   PIC	X(06).
	       05  FILLER	   PIC	X(01).
	       05  REP-ESC	   PIC	ZZ9.99 BLANK ZERO.
	       05  FILLER	   PIC	X(01).
	       05  REP-ADATE	   PIC	99/99/99 BLANK ZERO.
	       05  FILLER	   PIC	X(01).
	       05  REP-RATE	   PIC	Z(05)9.999 BLANK ZERO.

	   03  REP-LINE9 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC X(54).
	       05  REP-BTOT	   PIC Z(06)9.99-.
	       05  FILLER	   PIC X(15).

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).

001270 01  W08-EXPD.
001280	   03  W08-DEC	       PIC 9(03) OCCURS 6.

       COPY W12.WS.

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

005810 01  W95-STM.
005820     03  W95-COMP        PIC X(40).

005950 01  W100-HEAD.
005960	   03  FILLER	       PIC X(36)    VALUE
005970			     "DEBTOR ACCOUNTS: CONTRACT LISTING -".
005980	   03  W100-DESC       PIC X(28).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005990 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
				"RENTAL/SERVICE CONTRACT REPORTS".
	   03  LINE  4 COLUMN 10 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "R".
	   03	       COLUMN 11 VALUE "ental, ".
	   03	       COLUMN 18 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "S".
	   03	       COLUMN 19 VALUE "ervice or ".
	   03	       COLUMN 29 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 30 VALUE "ll contracts".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
003240	   IF LS0-DBLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM DA000.
	     CLOSE DBFOTO.

       AA49.
006950	     EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

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
      
       COPY DEBTOR.RD1.
      
       COPY DBFOTO.RD.

       COPY APAC.HLP.

009120 CA165-NAME	       SECTION 2.
009130 CA165-EXTRACT.
009140	     MOVE ZERO		 TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160				 INTO W40-SURNAME
				      W40-FNAME
				      W40-SNAME
009170         OVERFLOW
009180		 MOVE 1 	 TO WS-ERROR.

009190 CA170-EXIT.
009200       EXIT.

      /
      *    ****    D E B T O R	 R E N T A L   M A I N T E N A N C E
      *
037050 DA000	       SECTION 5.
037060 DA00.
020320       DISPLAY CLR-SCREEN.
020330	     DISPLAY S01.
	     MOVE "A"		 TO WS-OPTION.

020360 DA05.
	     ACCEPT WS-OPTION AT 0443
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA999
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-OPTION = SPACES
	       GO TO DA999.
018520	   IF NOT (WS-OPTION = "A" OR "R" OR "S")
	       GO TO DA05.
	     MOVE WS-OPTION	 TO WS-SEQ.
	   IF WS-SEQ = "A"
015140	       DISPLAY "Busy printing Rental & Service contracts"
			AT 0810
	       MOVE "RENTAL AND SERVICE AGREEMENTS"
				 TO W100-DESC
	   ELSE
	   IF WS-SEQ = "R"
015140	       DISPLAY "Busy printing Rental contracts" AT 0810
	       MOVE "RENTAL AGREEMENTS"
				 TO W100-DESC
	   ELSE
015140	       DISPLAY "Busy printing Service contracts" AT 0810
	       MOVE "SERVICE AGREEMENTS"
				 TO W100-DESC.
	     DISPLAY "Reading :" AT 1010.
	     PERFORM SAVE-SCREEN-3.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-3.

       DA10.
	     MOVE "H"		 TO W02-PRN-TYPE.
015350	     MOVE SPACES	 TO R-DET.
015360	     MOVE "PAGE:"	 TO REP-PAGE.
015370	     MOVE "DATE:"	 TO REP-DATE.
015380	     ADD 1		 TO WS-PAGE.
015390	     MOVE W12-TODAY	 TO REP-DTE.
015400	     MOVE WS-PAGE	 TO REP-P-NO.
015440	     MOVE W100-HEAD	 TO REP-RESTART.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE "E"		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-L1.
	     MOVE "H"		 TO W02-PRN-TYPE.
015410	     MOVE W95-COMP	 TO REP-CO.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-L1.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE "X"		 TO W02-PRN-TYPE
	     PERFORM CALL-PRINTUTL.
015460	     MOVE SPACES	 TO R-DET.
015470	     MOVE "AC-NO"	 TO REP-HEAD7.
	     MOVE "ACCOUNT NAME" TO REP-HEAD8.
	     MOVE "CONT-NO"	 TO REP-HEAD9.
	     MOVE "CONTRACT DESCRIPTION"
				 TO REP-HEAD10.
	     MOVE "   PAYMENT"	 TO REP-HEAD11.
	     MOVE "TYPE"	 TO REP-HEAD12.
	     MOVE "FREQ"	 TO REP-HEAD13.
	     MOVE "ESCAL"	 TO REP-HEAD14.
	     MOVE "ANV-DATE"	 TO REP-HEAD15.
	     MOVE " UNIT RATE"	 TO REP-HEAD16.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015490	     MOVE SPACES TO R-DET.
	     MOVE "D"		 TO W02-PRN-TYPE.

015500 DA15.
	     MOVE " !"		 TO DBF-AC.
	     PERFORM START-AT-DBFT-KEY THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO DA25.

       DA20.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DET
	       GO TO DA30.
	     PERFORM READ-DBFOTO-NEXT THRU READ-DBFOTO-EXIT.
	   IF WS-F-ERROR = 42
	       GO TO DA25.
      *
      *    ****   T E S T   I F   O U T P U T	T O   P R I N T E R
      * 	  O R	S P O O L  F I L E
      *
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY DBF-AC AT 1020
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY DBF-CONT AT 1027
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	   IF WS-SEQ = "R" OR "S"
	       IF NOT (WS-SEQ = DBF-TYPE)
		   GO TO DA20.
	   IF DBF-AC NOT = DEB-ACNO
	       MOVE DBF-AC	 TO DEB-ACNO
	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
	       MOVE DEB-ACNO	 TO REP-AC
015630	       INSPECT DEB-NAME REPLACING FIRST "*" BY ","
015640         INSPECT DEB-NAME REPLACING FIRST "*" BY " "
015650	       INSPECT DEB-NAME REPLACING FIRST "!" BY " "
	       MOVE DEB-NAME	 TO REP-NAME.
	     MOVE DBF-CONT	 TO REP-CONT.
	     MOVE DBF-DESC	 TO REP-DESC.
	     MOVE DBF-RENTAL	 TO REP-INST.
	   IF DBF-TYPE = "R"
	       MOVE "RENT"	 TO REP-TYPE
	   ELSE
	   IF DBF-TYPE = "S"
	       MOVE "SERV"	 TO REP-TYPE
	   ELSE
	       MOVE "OTHR"	 TO REP-TYPE.
	   IF DBF-FREQ = "1"
	       MOVE "Mthly"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "2"
	       MOVE "2 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "3"
	       MOVE "3 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "4"
	       MOVE "4 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "6"
	       MOVE "6 Mnth"	 TO REP-FREQ
	   ELSE
	   IF DBF-FREQ = "A"
	       MOVE "Annual"	 TO REP-FREQ.
	     MOVE DBF-ESC	 TO REP-ESC.
	     MOVE DBF-ADATE	 TO REP-ADATE.
	     MOVE DBF-RATE	 TO REP-RATE.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016060	       PERFORM DA10.
015860	     MOVE SPACES	 TO R-DET.
	     GO TO DA20.

       DA25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "****    E N D   O F   R E P O R T   ****"
				 TO REP-CO.

       DA30.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       DA999.
	     EXIT.

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
	     MOVE "DBFOTO"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBFOTO.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
044300	     OPEN INPUT DBFOTO.
044310	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 42	 TO WS-F-ERROR
                   GO TO ZA50.
       ZA02.
025430	     MOVE 11		 TO WS-PARKEY.
025440	     MOVE 1		 TO WS-SUB-2.

025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

025620 ZA25.
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

039850 ZA50.
041850	   IF WS-F-ERROR = 42
041860	       MOVE "RENTAL file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

040020 ZA51.
040030       EXIT PROGRAM.

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
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 42
041860	       MOVE W02-DBFOTO	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DBF-KEY	 TO WS-KEYX.
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
