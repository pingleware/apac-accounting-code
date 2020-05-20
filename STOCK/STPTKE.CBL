      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	********  **	**  ********   *
      *   **	**     **     **    **	   **	  **   **   **	       *
      *   **	       **     **    **	   **	  **  **    **	       *
      *    ******      **     *******	   **	  *****     ******     *
      * 	**     **     **	   **	  **  **    **	       *
      *   **	**     **     **	   **	  **   **   **	       *
      *    ******      **     **	   **	  **	**  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K	T A K E	  P R O G R A M			       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPTKE.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2008
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
                       CURSOR IS CSTART
000080		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY DEPART.SL.

       COPY PARAM.SL.

000140 COPY STOCK.SL.

	   SELECT STTAKE  ASSIGN DISK
000180			  STATUS WS-STATUS
000190                    ACCESS SEQUENTIAL
000200			  ORGANIZATION LINE SEQUENTIAL.

      $SET KEYCOMPRESS "7"
	   SELECT STKQNT  ASSIGN DISK
000020			  LOCK MANUAL
000030                    WITH LOCK ON MULTIPLE RECORDS
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD QTY-CODE
000140			  ALTERNATE RECORD QTY-XREF DUPLICATES.
      $SET NOKEYCOMPRESS

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000300 COPY STOCK.FDE.

000290 FD  STTAKE    LABEL RECORD STANDARD
000300		     VALUE OF FILE-ID W02-STKTKE.
       01  TKE-REC1.
	   03  TKE-DATA     PIC  X(40).

       FD  STKQNT    IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-STKQNT.
       01  QTY-RECORD.
	   03  QTY-CODE     PIC  X(18).
	   03  QTY-XREF     PIC  X(18).
	   03  QTY-QNT	    PIC S9(07)V9(03).

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
       77  WS-P1	    PIC	 9(02)	   COMP-5.
001710 77  WS-SUB	    PIC  9(04)	   COMP-5.
001710 77  WS-S1	    PIC  9(04)	   COMP-5.
001720 77  WS-S2	    PIC  9(04)	   COMP-5.
001730 77  WS-S3	    PIC  9(04)	   COMP-5.
001740 77  WS-S4	    PIC  9(04)	   COMP-5.
001750 77  WS-S5	    PIC  9(04)	   COMP-5.
001760 77  WS-S6	    PIC  9(04)	   COMP-5.
001770 77  WS-S7	    PIC  9(04)	   COMP-5.
001780 77  WS-S8	    PIC  9(04)	   COMP-5.
001790 77  WS-IXS	    PIC  9(04)	   COMP-5.
001800 77  WS-IXS1	    PIC  9(04)	   COMP-5.
001810 77  WS-IXS2	    PIC  9(04)	   COMP-5.
001820 77  WS-IXS3	    PIC  9(04)	   COMP-5.
001830 77  WS-IXS4	    PIC  9(04)	   COMP-5.
001860 77  WS-PARKEY	    PIC  9(04)	   COMP-5.
001880 77  WS-NETKEY	    PIC  9(04)	   COMP-5.
       77  WS-PRCKEY	    PIC  9(04)	   COMP-5.
002020 77  WS-OPTION	    PIC  X(01).
      *
      *    ****   M = Manual Input. F = Input from File.
      *
002020 77  WS-STTYPE	    PIC  X(01).
      *
      *    ****   C = Comma delimited. F = Fixed length fields.
      *
002020 77  WS-STFILE	    PIC  X(01).
002020 77  WS-SKIP	    PIC  X(01).
002070 77  WS-ERROR	    PIC  9(01).
002080 77  WS-IND	    PIC  9(01)	   VALUE 0.
002100 77  WS-INDP	    PIC  9(01)	   VALUE 0.
002110 77  WS-INDS	    PIC  9(01)	   VALUE 0.
002120 77  WS-AMEND	    PIC  9(01)	   VALUE 0.
002150 77  WS-LEDG	    PIC  X(04).
002160 77  WS-LEDG1	    PIC  X(04).
002200 77  WS-COMP	    PIC  9(01).
002220 77  WS-TYPE	    PIC  X(01).
       77  WS-PRINT	    PIC  9(01).
002280 77  WS-PASS	    PIC  X(08).
002290 77  WS-QUOTE	    PIC  X(01) VALUE "N".
002290 77  WS-OK	    PIC  X(01) VALUE "N".
       77  TODAY-DDMMYY	    PIC	 9(08) COMP-5.
       77  WS-USUB	    PIC	 9(04) COMP-5.
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
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "TKE".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000300 COPY STOCK.ID.

002440	   03  W02-INPUT.
	       05  W02-STKTKE	PIC X(60) VALUE SPACES.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

       COPY W10.STK.

	   03  W10-ACT	       PIC  9(06)    COMP-3.
	   03  W10-YTDV        PIC S9(09)V99 COMP-3.
	   03  W10-DSCA	       PIC  X(01).
	   03  W10-SUPP        PIC  X(06).
	   03  W10-VTAX OCCURS 4.
002850	       05  W10-SVAT    PIC S9(07)V99  COMP-3.
002860	       05  W10-CVAT    PIC S9(07)V99  COMP-3.
002860	       05  W10-WVAT    PIC S9(07)V99  COMP-3.

       COPY W11.STK.

       COPY W12.WS.

       COPY W15.STK.

       COPY W20.WS.

003700 01  W25-CALCS.
003710     03  W25-RESULT     PIC 9(02)V99.
003720     03  W25-RESULT1 REDEFINES W25-RESULT.
003730         05  W25-WHOLE  PIC 9(02).
003740         05  W25-DECIMAL PIC 9(02).
003750     03  W25-SCOST      PIC S9(07)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(07)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(07)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(07)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(07)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(07)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

       01  W50-NUMERIC-FIELDS.
	   03  W50-FIELD-TYPE  PIC  X(01).
	   03  W50-DECIMALS    PIC  9(01).
	   03  W50-S3	       PIC  9(02)  COMP-5.
	   03  W50-SIGN        PIC  X(01).
	   03  W50-NUMB        PIC S9(14)V9(04).
	   03  W50-N-FIELD.
	       05  W50-N-CHAR  PIC  X(01) OCCURS 18.
	   03  W50-N-FLD1 REDEFINES W50-N-FIELD.
	       05  W50-NUMBER  PIC S9(18).
	   03  W50-N-FLD2 REDEFINES W50-N-FIELD.
	       05  W50-1DEC    PIC S9(17)V9.
	   03  W50-N-FLD3 REDEFINES W50-N-FIELD.
	       05  W50-2DEC    PIC S9(16)V99.
	   03  W50-N-FLD4 REDEFINES W50-N-FIELD.
	       05  W50-3DEC    PIC S9(15)V9(03).
	   03  W50-N-FLD5 REDEFINES W50-N-FIELD.
	       05  W50-4DEC    PIC S9(14)V9(04).
	   03  W50-N-FLD6 REDEFINES W50-N-FIELD.
	       05  W50-5DEC    PIC S9(13)V9(05).
	   03  W50-N-FLD7 REDEFINES W50-N-FIELD.
	       05  W50-6DEC    PIC S9(12)V9(06).
	   03  W50-N-FLD8 REDEFINES W50-N-FIELD.
	       05  W50-7DEC    PIC S9(11)V9(07).
	   03  W50-N-FLD9 REDEFINES W50-N-FIELD.
	       05  W50-8DEC    PIC S9(10)V9(08).

       01  W55-IMPORT.
           03  W55-S1          PIC  9(04) COMP-5.
	   03  W55-TKE-DATA.
	       05  W55-TKE     PIC  X(01) OCCURS 40.

003820 01  W60-FIELDS.
           03  W60-S           PIC  9(02) COMP-5.
	   03  W60-LAST	       PIC  9(02) COMP-5.
903830	   03  W60-FNO	       PIC  9(02) COMP-5 OCCURS 2.

003850 01  W65-FIELDS.
           03  W65-S           PIC  9(02) COMP-5.
           03  W65-FIELD       PIC  X(22).
003860     03  W65-FLDS.
      *    Field 01
003870         05  FILLER      PIC X(21) VALUE "Item Code".
      *    Field 02
003870	       05  FILLER      PIC X(21) VALUE "Quantity".
	   03  W65-RFLDS REDEFINES W65-FLDS.
	       05  W65-FLD     PIC  X(21) OCCURS 02.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.
003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000         05  W80-DATE   PIC Z9/99/99.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
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
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

000850 01  S02.
000860	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "STOCK TAKE PROGRAM".

       01  S05.
	   03  LINE  4 COLUMN 10 VALUE
	       "Is stock take Manual input or from a File ".
	   03	       COLUMN 52 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "M".
	   03	       COLUMN 53 VALUE " or ".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "F".

       01  S06.
	   03  LINE  4 COLUMN 10 VALUE
	       "Are the fields COMMA delimited or FIXED length ".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 58 VALUE " or ".
	   03	       COLUMN 62 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "F".
	   03  LINE  6 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
	       "Fixed length fields must be ITEM CODE = 18 characters".
	   03  LINE  7 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
	       "                            QUANTITY  =  7 characters".

       01  S07.
	   03  LINE 16 COLUMN  2 VALUE "Importing:".

       01  S09.
	   03  LINE  8 COLUMN 10 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC 99 FROM W65-S.
	   03	       COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC  X(21) FROM W65-FIELD.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
007730 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
003240	   IF LS0-STLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
007760	     PERFORM ZA000-INIT.
	     DISPLAY S02.
	     DISPLAY S05.

       AA05.
	     ACCEPT WS-STTYPE AT 0459
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
	     CALL "CBL_TOUPPER" USING W02-STTYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY WS-STTYPE AT 0459
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF NOT(WS-STTYPE = "F" OR "M")
	       GO TO AA05.
	   IF WS-STTYPE = "F"
	       PERFORM BA000.
012370	     CLOSE STKTKE.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 G E T	 T H E	 S T O C K   T A K E   F I L E	I D    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA100           SECTION.
       AA101.
	     DISPLAY "Keyin the <pathname>/<filename> " AT 0902.
	     DISPLAY "STOCK TAKE file :" AT 1002.
             MOVE ZERO           TO WS-ERROR.
       AA105.
	     ACCEPT W02-STKTKE AT 1020
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
	     CALL "CBL_TOUPPER" USING W02-IMPORT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     DISPLAY W02-STKTKE AT 2316
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5 UPDATE AUTO.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
               GO TO AA105.
	     OPEN INPUT STKTKE.
           IF WS-STATUS = "00"
	       GO TO AA199.
	     MOVE "Invalid file name"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
             GO TO AA105.

       AA199.
             EXIT.
      *
      *    ****    E X T R A C T   S T O C K - T A K E
      *
      * 	   W50-DATA   : EXTRACTED FIELD FROM STKTKE DATA
      *            W50-LENGTH : LENGTH OF DATA 
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *
       AA200           SECTION.
       AA201.
	     MOVE SPACES	 TO W50-DATA.
             MOVE W50-LENGTH     TO W50-S1.
	     MOVE 1		 TO W50-S2.
	     MOVE "N"		 TO WS-QUOTE.
      *    *************************************************************
      *    ****    I S	 T H E	 F I R S T   C H A R A C T E R	 A   "
      *    *************************************************************
	   IF W55-IMP (W55-S1) = QUOTE
	       MOVE "Y" 	 TO WS-QUOTE
	       ADD 1		 TO W55-S1.
      *    *************************************************************
      *    ****    M O V E   F I E L D	 T O   W O R K	 A R E A
      *    *************************************************************
       AA205.
	   IF W60-S = W60-LAST
	       IF W50-S2 > W50-LENGTH
		   GO TO AA208.
	   IF NOT (W55-TKE (W55-S1) = W50-EXP OR QUOTE)
	       MOVE W55-TKE (W55-S1)
				 TO W50-CCHAR (W50-S2)
	       ADD 1		 TO W50-S2 W55-S1
	       GO TO AA205.
      *    *************************************************************
      *    ***	S O M E   P R O G R A M S   E N C L O S E   A L P H A -
      * 	N U M E R I C	F I E L D S   W I T H	"
      *    *************************************************************
	   IF W55-TKE (W55-S1) = QUOTE
	       MOVE "N" 	 TO WS-QUOTE
	       ADD 1		 TO W55-S1
	       GO TO AA205.
      *    *************************************************************
      *    **  I F   T H E   F I R S T	 C H A R A C T E R   W A S
      *        A   "   T H E N   A L L   C H A R A C T E R S   M U S T
      *        B E   S T O R E D   U P	 T O   B U T   N O T
      *        I N C L U D I N G   T H E   S E C O N D	 "
      *    *************************************************************
	   IF W55-TKE (W55-S1) = W50-EXP
	       IF WS-QUOTE = "Y"
		   MOVE W55-TKE (W55-S1)
				 TO W50-CCHAR (W50-S2)
		   ADD 1	 TO W50-S2 W55-S1
		   GO TO AA205.
	     ADD 1		 TO W55-S1.
      *    *************************************************************
      *    ****    I F	 A L P H A - N U M E R I C   T H E N   E X I T
      *    *************************************************************
       AA208.
	   IF W50-FIELD-TYPE = "A"
	       GO TO AA299.
	     MOVE 18		 TO W50-S3.
	     MOVE 0		 TO WS-S3 W50-DECIMALS W50-NUMBER.
	   IF W50-S2 = 1
	       GO TO AA220.
      *    *************************************************************
      *    ****    D E T E R M I N E   T H E   S I G N	 O F   T H E
      * 	   N U M E R I C   F I E L D
      *    *************************************************************
	     MOVE "+"		 TO W50-SIGN.
      *    *************************************************************
      *    ****    M O V E   D A T A   T O   N U M B E R S
      * 	   W O R K   F I E L D
      *    *************************************************************
       AA210.
	   IF W50-CCHAR (W50-S2) = "-"
	       MOVE "-" 	 TO W50-SIGN
	       GO TO AA215.
	   IF W50-CCHAR (W50-S2) = "."
	       MOVE WS-S3	 TO W50-DECIMALS
	       GO TO AA215.
	   IF W50-CCHAR (W50-S2) NOT NUMERIC
	       GO TO AA215.
	     MOVE W50-CCHAR (W50-S2)
				 TO W50-N-CHAR (W50-S3).
	     SUBTRACT 1 	 FROM W50-S3.
      *    *************************************************************
      *    ****    D E T E R M I N E   T H E   D I G I T S   A F T E R
      * 	   T H E   D E C I M A L   P O I N T
      *    *************************************************************
	   IF W50-DECIMALS = 0
	       ADD 1		 TO WS-S3.
      *    *************************************************************
      *    ****    E N D   O F	 F I E L D   C O N T R O L   L O O P
      *    *************************************************************
       AA215.
	   IF W50-S2 > 1
	       SUBTRACT 1	 FROM W50-S2
	       GO TO AA210.
      *    *************************************************************
      *    ****    S T O R E   N U M B E R   A S   S9(14)V9(04).
      *    *************************************************************
       AA220.
	   IF W50-DECIMALS = 0
	       MOVE W50-NUMBER	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 1
	       MOVE W50-1DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 2
	       MOVE W50-2DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 3
	       MOVE W50-3DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 4
	       MOVE W50-4DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 5
	       MOVE W50-5DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 6
	       MOVE W50-6DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 7
	       MOVE W50-7DEC	 TO W50-NUMB
	   ELSE
	   IF W50-DECIMALS = 8
	       MOVE W50-8DEC	 TO W50-NUMB
	   ELSE
	       MOVE ZERO	 TO W50-NUMB.
      *    *************************************************************
      *    ****    C H E C K   I F   V A L U E	 M U S T   B E
      * 	   S W I T C H E D   T O   N E G A T I V E
      *    *************************************************************
	   IF W50-SIGN = "-"
	       MULTIPLY -1	 BY W50-NUMB.
       AA299.
             EXIT.
      *    *************************************************************
      *    ****    M O V E   I N D V I D U A L   F I E L D S
      *
      *            W50-BIN : FIELD NUMBER TO MOVE
      *    *************************************************************
       AA300              SECTION.
       AA301.
	   IF W50-BIN < 01 OR > 02
               GO TO AA399.
	     MOVE "N"		 TO W50-FIELD-TYPE.
      *
           IF W50-BIN = 01
	       MOVE 18		 TO W50-LENGTH
	       MOVE "A" 	 TO W50-FIELD-TYPE
               PERFORM AA200
	       MOVE W50-DATA	 TO W10-EXT-ITEM
               GO TO AA399.
      *
           IF W50-BIN = 02
	       MOVE 15		 TO W50-LENGTH
               PERFORM AA200
	       MOVE W50-NUMB	 TO W10-QUANT.

       AA399.
               EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   P R O C E S S   T H E   S E L E C T E D	F I E L D S    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA400	       SECTION.
       AA401.
             MOVE 1              TO W60-S.
      *    *************************************************************
      *    ****    S E T U P   I M P O R T   D A T A   A R E A
      *    *************************************************************
	     MOVE IMP-DATA	 TO W55-IMP-DATA.
             MOVE 1              TO W55-S1.

       AA405.
           IF W60-FNO (W60-S) = ZERO
               GO TO AA499.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
             PERFORM AA300.
	   IF W60-S < 2
               ADD 1             TO W60-S
               GO TO AA405.

       AA499.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		S E C U R I T Y   V I O L A T I O N	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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

       COPY DATE.CHK.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
009850 AD000-WRITE	       SECTION.

       COPY STOCK.WR.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ READ STOCK TAKE DETAILS FROM FILE,	CONSOLODATE PER ITEM   ³
      *    ³ AND CREATE A FILE TO BE USED FOR UPDATING THE STOCK FILE. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012510 BA000	       SECTION.
012520 BA00.
             DISPLAY CLR-SCREEN.
             DISPLAY S02.
      *    *************************************************************
      *    ****   ARE THE FIELDS FIXED LENGTH OR COMMA DELIMITED?
      *    *************************************************************
	     DISPLAY S06.
	     MOVE "C"		 TO WS-STFILE.

       BA05.
	     ACCEPT WS-STFILE AT 0464
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING W02-STTYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY WS-STFILE AT 0464
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF NOT(WS-STFILE = "C" OR "F")
	       GO TO BA05.






       BA10.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY SPACE AT 2541
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF W65-S = 1
			 GO TO BA10
		     ELSE
			 SUBTRACT 1  FROM W65-S
			 GO TO BA05
		     END-IF
		 WHEN DOWN-KEY
		     IF W65-S NOT < 90
			 GO TO BA10
		     ELSE
			 ADD 1	 TO W65-S
			 GO TO BA05
		     END-IF
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO BA15
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO BA10
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER
		     CALL X"E5"
	       END-EVALUATE
	       GO TO BA10
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
           IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BA10.
           IF WS-OPTION = "A"
	       GO TO BA30.

       BA15.
             MOVE 1              TO WS-S1.

       BA20.
           IF WS-S1 = W60-S
	       GO TO BA25.
           IF W60-FNO (WS-S1) = W65-S
	       GO TO BA10.
             ADD 1               TO WS-S1.
	     GO TO BA20.

       BA25.
             MOVE W65-S          TO W60-FNO (W60-S).
             DISPLAY S06.
	     MOVE W60-S 	 TO W60-LAST.
             ADD 1               TO W60-S.
	   IF W65-S < 90
               ADD 1             TO W65-S.
	     GO TO BA05.

       BA30.
             PERFORM AA100.
	   IF WS-ERROR = 9
	       GO TO BA999.
	     READ IMPORT.
             DISPLAY S07.
	     MOVE ZERO		 TO WS-PRCKEY WS-RECKEY.

       BA35.
           IF WS-RECKEY = 100
	       MOVE ZERO	 TO WS-RECKEY
012370	       CLOSE STOCK
012370	       OPEN I-O STOCK.
	     READ IMPORT AT END
			 GO TO BA40.
	     MOVE "001" 	 TO W10-BIN.
	     MOVE 1		 TO W10-TAX.
	     MOVE "N"		 TO W10-USE-PACKS
				    W10-USE-CASES
				    W10-EXEMPT.
015760	     MOVE SPACES	 TO W10-DESC W10-DESC2 W10-EXT-ITEM
				    W10-SUPP W10-ACODE W10-EXT-CODE2
				    W10-LEDG W10-DSCA.
015780       MOVE ZERO           TO W10-COST W10-QUANT W10-MARKUP
015790                              W10-AVRG W10-REORD W10-CMARKUP 
015800				    W10-PRC  W10-LTIME W10-WMARKUP
				    W10-IND  W10-PURCH W10-ORDER
				    W10-ODTE W10-GUAR  W10-FACT
				    W10-MTD  W10-MTDC  W10-MTDV
				    W10-MTH1 W10-MTH2  W10-MTH3
				    W10-MAVG W10-YTD   W10-YTDC
				    W10-YTDV W10-ACT.
	     MOVE 1		 TO WS-S1.

       BA36.
	     MOVE ZERO		 TO W10-UNT(WS-S1)
				    W10-MKUP(WS-S1)
				    W10-CMKUP(WS-S1)
				    W10-WMKUP(WS-S1)
				    W10-SL(WS-S1)
				    W10-CSH(WS-S1)
				    W10-WSL(WS-S1)
				    W10-VSL(WS-S1)
				    W10-VCSH(WS-S1)
				    W10-VWSL(WS-S1)
				    W10-SVAT(WS-S1)
				    W10-CVAT(WS-S1)
				    W10-WVAT(WS-S1).
	   IF WS-S1 < 4
	       ADD 1		 TO WS-S1
	       GO TO BA36.
	     MOVE 1		 TO W10-UNIT.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
	     PERFORM AA400.
	   IF W10-EXT-ITEM = SPACES AND
	      W10-EXT-CODE2 = SPACES AND
	      W10-ACODE = SPACES
	       GO TO BA35.
	     ADD 1		 TO WS-PRCKEY WS-RECKEY.
	     MOVE WS-PRCKEY	 TO W80-REC.
	     DISPLAY W80-NO AT 1613 WITH FOREGROUND-COLOR 11.
	   IF NOT (W10-EXT-ITEM = SPACES)
	       PERFORM CA500
	   ELSE
	   IF NOT (W10-CODE2 = SPACES)
	       PERFORM CA200
	       IF NOT (W10-CODE2 = STK-XREF)
		   GO TO BA35
	       ELSE
		   PERFORM DA000
		   GO TO BA35
	       END-IF
	   ELSE
	   IF NOT (W10-ACODE = SPACES)
	       PERFORM CA100
	       IF NOT (W10-ACODE = STK-ACODE)
		   GO TO BA35
	       ELSE
		   PERFORM DA000
		   GO TO BA35.
	   IF WS-F-ERROR = 22
	       PERFORM DA000
	       GO TO BA35.
	   IF ((W10-COST = ZERO) OR (W10-COST = STK-COST))
                   UNLOCK STOCK 
               ELSE
		   PERFORM DB000.
	     GO TO BA35.

       BA40.
	     DISPLAY "Update complete - Press " AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3 "ENTER"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 3.
	     ACCEPT WS-OPTION AT 2542 WITH FOREGROUND-COLOR 15 AUTO.

       BA999.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   
      *           A L T E R N A T E   C O D E
      *
015170 CA100         SECTION.
015180 CA101.
013310       MOVE 10             TO W50-LENGTH.

       CA105.
013320	     MOVE W10-ACODE	 TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9        TO W10-ACODE
               GO TO CA105.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-ACODE	 TO STK-ACODE W10-ACODE.

       CA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA199.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.

       CA199.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
015170 CA200         SECTION.
015180 CA201.
013310	     MOVE 18		 TO W50-LENGTH.
       CA205.
013320	     MOVE W10-EXT-CODE2	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-CODE2
               GO TO CA205.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-CODE2.

       CA206.
013380	     MOVE W50-EXT-ITEM	 TO STK-XREF.

       CA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
             PERFORM CA500.

       CA299.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
015170 CA500         SECTION.
015180 CA501.
013310	     MOVE 18		 TO W50-LENGTH.
       CA505.
013320	     MOVE W10-EXT-ITEM	  TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-17	 TO W10-EXT-ITEM
               GO TO CA505.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-EXT-ITEM	 TO W10-EXT-ITEM.
013380	     MOVE W50-EXT-ITEM	  TO STK-CODE.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.

       CA599.
             EXIT.

       COPY ROUND.PRC.

      /
      *    ****   C R E A T E   N E W   S T O C K
      *
015670 DA000           SECTION 50.
015680 DA00.
016040	   IF W10-LEDG = SPACES
	       MOVE WS-LEDG	 TO W10-LEDG.
	   IF W10-BIN = SPACES
	       MOVE "001"	 TO W10-BIN.
016270	   IF W10-IND > 4
	       IF NOT (W10-IND = 8 OR 9)
		   MOVE 0	 TO W10-IND.
016010	   IF ((W10-IND < 3) OR (W10-IND = 8 OR 9)) AND
               (W10-DESC = SPACES)
	       MOVE "NOT SPECIFIED"
				 TO W10-DESC.
	   IF W10-TAX > 3
	       MOVE 1		 TO W10-TAX.
	   IF NOT (W10-ODTE = ZERO)
	       MOVE W10-ODTE	 TO W22-DTE2
	       PERFORM DA60
	       MOVE W22-DTE1	 TO W10-ODTE.
	   IF NOT (W10-ACT = ZERO)
	       MOVE W10-ACT	 TO W22-DTE2
	       PERFORM DA60
	       MOVE W22-DTE1	 TO W10-ACT.

016260 DA20.
             INITIALIZE STK-RECORD1.
016670	     MOVE W10-EXT-ITEM	 TO STK-CODE.
016680       MOVE W10-BIN        TO STK-BIN.
016690       MOVE W10-DESC       TO STK-DESC.
016690	     MOVE W10-DESC2	 TO STK-DESC2.
016700       MOVE W10-TAX        TO STK-TAX.
016730       PERFORM DA50.
	     MOVE W10-UNIT	 TO STK-UNIT.
016740       MOVE W10-MARKUP     TO STK-MARKUP.
016750       MOVE W10-QUANT      TO STK-QUANT.
016760       MOVE W10-REORD      TO STK-LEVEL.
016770       MOVE W10-LTIME      TO STK-LTIME.
016780       MOVE W10-COST       TO W10-CCOST.
016790       MOVE W10-SELL       TO W10-CSELL.
	     MOVE W10-PRC	 TO STK-PRC.
016800     IF W10-COST = ZERO
016810         IF W10-SELL NOT = ZERO
016820             COMPUTE W10-CCOST ROUNDED = (W10-CSELL / 
016830                     (100 + W10-MARKUP) * 100).
016840     IF W10-SELL = ZERO
016850         IF W10-COST NOT = ZERO
016860             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + 
016870                     ((W10-CCOST * W10-MARKUP) / 100)).
016880       MOVE W10-CCOST      TO W10-COST.
016890       MOVE W10-CSELL      TO W10-SELL.
016900       MOVE W10-COST       TO STK-COST.
016920	     MOVE W10-LEDG	 TO STK-LDG.
             MOVE W10-GUAR       TO STK-GUAR.
016930       MOVE W10-ORDER      TO STK-ORD.
016940       MOVE W10-ODTE       TO STK-DTE.
             MOVE W10-PURCH      TO STK-PUR.
	     MOVE W10-ACT	 TO STK-ACT.
016170	   IF W10-AVRG = ZERO
016180         MOVE W10-COST     TO W10-AVRG.
016980	     MOVE W10-AVRG	 TO STK-AVGE.
016990       MOVE W10-IND        TO STK-IND.
016730       PERFORM DA52.
017000     IF (W10-CMARKUP = ZERO) AND
017010        (W10-CASH = ZERO)
017020	       MOVE W10-MARKUP	 TO W10-CMARKUP
017030	       MOVE W10-SELL	 TO W10-CASH
017040         GO TO DA21.
017050     IF W10-CMARKUP = ZERO
017060	       MOVE W10-CASH	 TO W10-CSELL
017070         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-CASH = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-CMARKUP) / 100))
017120	       MOVE W10-CSELL	 TO W10-CASH.

017130 DA21.
017140	     MOVE W10-CMARKUP	 TO STK-CMARKUP.
             PERFORM DA54.
017000     IF (W10-WMARKUP = ZERO) AND
017010        (W10-WSALE = ZERO)
017020	       MOVE W10-CMARKUP  TO W10-WMARKUP
017030	       MOVE W10-CASH	 TO W10-WSALE
017040         GO TO DA21D.
017050     IF W10-WMARKUP = ZERO
017060         MOVE W10-WSALE TO W10-CSELL
017070         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-WSALE = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-WMARKUP) / 100))
017120         MOVE W10-CSELL TO W10-WSALE.

       DA21D.
017140       MOVE W10-WMARKUP    TO STK-WMARKUP.
017160	     MOVE W10-EXT-CODE2  TO STK-XREF.
017170       MOVE W10-ACODE      TO STK-ACODE.
	   IF W05-ROUND = "E"
               MOVE W10-SELL     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL
	       MOVE W10-WSALE	 TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
	       MOVE W10-CASH	 TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).
016910	     MOVE W10-SELL	 TO STK-SELL.
017150       MOVE W10-CASH       TO STK-CASH.
017150       MOVE W10-WSALE      TO STK-WSALE.
	     MOVE 2		 TO WS-P1.

       DA22.
	   IF W10-UNT(WS-P1) = ZERO
	       GO TO DA22B.
	     MOVE "Y"		 TO W10-USE-PACKS.
016780	     MULTIPLY W10-UNT(WS-P1) BY W10-COST
				 GIVING W10-CCOST.
016720	   IF W10-MKUP(WS-P1) NOT > ZERO
017600	       IF W10-SL(WS-P1) > ZERO AND W10-COST > ZERO
017610		   MOVE W10-SL(WS-P1)
				 TO W10-CSELL
017620		   MOVE W10-COST TO W10-CCOST
017630		   COMPUTE W10-MKUP(WS-P1) ROUNDED =
			   (((W10-CSELL - W10-CCOST) / W10-COST) * 100).
016790	     MOVE W10-SL(WS-P1)	 TO W10-CSELL.
016840	   IF W10-SL(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-MKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-MKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016890	     MOVE W10-CSELL	 TO W10-SL(WS-P1).
016720	   IF W10-CMKUP(WS-P1) NOT > ZERO
017600	       IF W10-CSH(WS-P1) > ZERO AND W10-COST > ZERO
017610		   MOVE W10-CSH(WS-P1)
				 TO W10-CSELL
017620		   MOVE W10-COST TO W10-CCOST
017630		   COMPUTE W10-CMKUP(WS-P1) ROUNDED =
			   (((W10-CSELL - W10-CCOST) / W10-COST) * 100).
016790	     MOVE W10-CSH(WS-P1) TO W10-CSELL.
016840	   IF W10-CSH(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-CMKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-CMKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016890	     MOVE W10-CSELL	 TO W10-CSH(WS-P1).
016720	   IF W10-WMKUP(WS-P1) NOT > ZERO
017600	       IF W10-WSL(WS-P1) > ZERO AND W10-COST > ZERO
017610		   MOVE W10-WSL(WS-P1)
				 TO W10-CSELL
017620		   MOVE W10-COST TO W10-CCOST
017630		   COMPUTE W10-CMKUP(WS-P1) ROUNDED =
			   (((W10-CSELL - W10-CCOST) / W10-COST) * 100).
016790	     MOVE W10-WSL(WS-P1) TO W10-CSELL.
016840	   IF W10-WSL(WS-P1) = ZERO
016850	       IF (W10-CCOST NOT = ZERO) AND
		  (W10-WMKUP(WS-P1) NOT = ZERO)
016860		   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870			   ((W10-CCOST * W10-WMKUP(WS-P1)) / 100)).
	   IF W10-CSELL = ZERO
	       MOVE W10-CCOST	 TO W10-CSELL.
016890	     MOVE W10-CSELL	 TO W10-WSL(WS-P1).
      *    *************************************************************
      *    ****    C H E C K   I F   E X C L U S I V E	 R O U N D I N G
      * 	   R E Q U I R E D
      *    *************************************************************
           IF W05-ROUND = "E"
	       MOVE W10-SL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-SL(WS-P1)
	       MOVE W10-WSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-WSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-CSH(WS-P1).
	   IF W10-TAX NOT = ZERO
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE
	       COMPUTE W10-VSL(WS-P1) ROUNDED = W10-SL(WS-P1) +
				 (W10-SL(WS-P1) * W05-RTE)
	       COMPUTE W10-VWSL(WS-P1) ROUNDED = W10-WSL(WS-P1) +
				 (W10-WSL(WS-P1) * W05-RTE)
	       COMPUTE W10-VCSH(WS-P1) ROUNDED = W10-CSH(WS-P1) +
				 (W10-CSH(WS-P1) * W05-RTE)
           ELSE
	       MOVE W10-WSL(WS-P1)
				 TO W10-VWSL(WS-P1)
	       MOVE W10-CSH(WS-P1)
				 TO W10-VCSH(WS-P1)
	       MOVE W10-SL(WS-P1)
				 TO W10-VSL(WS-P1).
           IF W05-ROUND = "I"
	       MOVE W10-VSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VSL(WS-P1)
	       MOVE W10-VWSL(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VWSL(WS-P1)
	       MOVE W10-VCSH(WS-P1)
				 TO W10-RSELL
               PERFORM CB100
	       MOVE W10-RSELL	 TO W10-VCSH(WS-P1)
               PERFORM CB200.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE.
	     COMPUTE W10-VCSH(WS-P1) ROUNDED
		   = W10-CSH(WS-P1) + (W10-CSH(WS-P1) * W05-RTE).
	     COMPUTE W10-VWSL(WS-P1) ROUNDED
		   = W10-WSL(WS-P1) + (W10-WSL(WS-P1) * W05-RTE).
	     COMPUTE W10-VSL(WS-P1) ROUNDED
		   = W10-SL(WS-P1) + (W10-SL(WS-P1) * W05-RTE).

       DA22A.
	     MOVE W10-UNT(WS-P1) TO STK-UNT(WS-P1).
	     MOVE W10-PR(WS-P1)  TO STK-PR(WS-P1).
016740	     MOVE W10-MKUP(WS-P1)
				 TO STK-MKUP(WS-P1).
017140	     MOVE W10-CMKUP(WS-P1)
				 TO STK-CMKUP(WS-P1).
017140	     MOVE W10-WMKUP(WS-P1)
				 TO STK-WMKUP(WS-P1).
016910	     MOVE W10-SL(WS-P1)	 TO STK-SL(WS-P1).
017150	     MOVE W10-CSH(WS-P1) TO STK-CSH(WS-P1).
017150	     MOVE W10-WSL(WS-P1) TO STK-WSL(WS-P1).

       DA22B.
	   IF WS-P1 < 4
	       ADD 1		 TO WS-P1
	       GO TO DA22.
	     MOVE W10-MTD	 TO STK-MTD.
	     MOVE W10-MTDC	 TO STK-MTDC.
	     MOVE W10-MTDV	 TO STK-MTDV.
	     MOVE W10-YTD	 TO STK-YTD.
	     MOVE W10-YTDC	 TO STK-YTDC.
	     MOVE W10-YTDV	 TO STK-YTDV.
	     MOVE W10-SUPP	 TO STK-SUPP.
	     MOVE W10-MTH1	 TO STK-MTH1.
	     MOVE W10-MTH2	 TO STK-MTH2.
	     MOVE W10-MTH3	 TO STK-MTH3.
	     MOVE W10-MAVG	 TO STK-MAVG.
	     MOVE W10-PER	 TO STK-PER.
	     MOVE W10-EXEMPT	 TO STK-EXEMPT.
	     MOVE W10-CASES	 TO STK-CASES.
	   IF W10-CASES > ZERO
	       MOVE "Y" 	 TO STK-USE-CASES.
	     MOVE W10-USE-PACKS  TO STK-USE-PACKS.
	     MOVE W10-FACT	 TO STK-FACT.
017190       PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
017230	     MOVE STK-RECORD1	 TO W15-RECORD1.
017500       GO TO DA999.
      *    *************************************************************
      *    ****  D E F A U L T   M A R K U P
      *    *************************************************************
017540 DA50.
017600     IF W10-SELL > ZERO AND W10-COST > ZERO
017610         MOVE W10-SELL     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					     / W10-COST) * 100).
       DA51.
             EXIT.
      *    *************************************************************
      *    ****  D E F A U L T   C A S H   M A R K U P
      *    *************************************************************
       DA52.
017600     IF W10-CASH > ZERO AND W10-COST > ZERO
017610         MOVE W10-CASH     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					      / W10-COST) * 100).
       DA53.
017660       EXIT.
      *    *************************************************************
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *    *************************************************************
       DA54.
017600     IF W10-WSALE > ZERO AND W10-COST > ZERO
017610         MOVE W10-WSALE    TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					      / W10-COST) * 100) .
017650 DA55.
017660	     EXIT.
      *    *************************************************************
      *    ****    C H E C K   D A T E	 F O R M A T
      *    *************************************************************
       DA60.
	   IF NOT (W22-CC2 < 19 OR > 20)
	       MOVE W22-DTE2	 TO W10-EDTE
	   ELSE
	       MOVE W22-CC2	 TO W22-CC3
	       MOVE W22-YY2	 TO W22-YY3
	       MOVE W22-MM2	 TO W22-MM3
	       MOVE W22-DD2	 TO W22-DD3
	       MOVE W22-DTE3	 TO W10-EDTE.
016220	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE ZERO	 TO W22-DTE1.

       DA65.
	     EXIT.

019380 DA999.
019390       EXIT.

       DB000	       SECTION 50.
       DB00.
	   IF W10-COST < STK-COST
               GO TO DB05.
	     MOVE W10-COST	 TO STK-COST.
016860	     COMPUTE W10-SELL ROUNDED = (W10-COST +
016870			      ((W10-COST * STK-MARKUP) / 100)).
016860	     COMPUTE W10-CASH ROUNDED = (W10-COST +
016870			      ((W10-COST * STK-CMARKUP) / 100)).
016860	     COMPUTE W10-WSALE ROUNDED = (W10-COST +
016870			       ((W10-COST * STK-WMARKUP) / 100)).
	   IF W05-ROUND = "E"
               MOVE W10-SELL     TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-SELL
	       MOVE W10-WSALE	 TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-WSALE
	       MOVE W10-CASH	 TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-CASH.
	   IF STK-TAX NOT = ZERO
	       MOVE W05-VAT (STK-TAX)
				 TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL +
                                           (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE +
                                            (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH +
                                           (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE    TO W10-VWSALE
               MOVE W10-CASH     TO W10-VCASH
               MOVE W10-SELL     TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VSELL
               MOVE W10-VWSALE   TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VWSALE
               MOVE W10-VCASH    TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL    TO W10-VCASH
               PERFORM CB200.
	     MOVE ZERO		 TO W05-RATE.
           IF W10-TAX > 0
	       MOVE W05-VAT (W10-TAX)
				 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED 
		   = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED 
		   = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED 
		   = W10-SELL + (W10-SELL * W05-RTE).
016910	     MOVE W10-SELL	 TO STK-SELL.
017150       MOVE W10-CASH       TO STK-CASH.
017150       MOVE W10-WSALE      TO STK-WSALE.

       DB05.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       DB999.
             EXIT.

      /
019370 ZA000-INIT	       SECTION 88.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-ROUND	 TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
	     MOVE SPACES	 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.

       ZA05-DEPT.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = "7"
	       MOVE "XXXX"	 TO WS-LEDG
	       GO TO ZA05-COMP.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO ZA05-DEPT.
	     MOVE DPT-CODE	 TO WS-LEDG.

       ZA05-COMP.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    Set up the SCREEN GRAPHICS		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
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
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
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
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 89.
      
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
	   IF WS-F-ERROR = 7
	       MOVE  W02-DEPART  TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
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
