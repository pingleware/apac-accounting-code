      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******      **      *
      *   **	**  **	  **  **    **	 **  **   **	**    ***      *
      *   **	**  **	  **  **    **	**    **       **      **      *
      *   *******   ******    *******	**    **     **        **      *
      *   **	    **	 **   **	**    **   **	       **      *
      *   **	    **	  **  **	 **  **   **	       **      *
      *   **	    **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   S Y S T E M   P A S S W O R D S	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP052.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1993.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2008
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

000120 COPY SYSTEM.SL.

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000200 COPY SYSTEM.FD.

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
001010 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-PRNKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-DEPKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001020 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
       77  WS-ADV-RATE	   PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE	   PIC	9(04)	 COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI	   PIC	X(04).
003100 77  WS-6LPI	   PIC	X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001100 77  WS-OPTION	   PIC	X(01).
001100 77  WS-VAT-GST	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001140 77  WS-CHAR1	   PIC	X(01).
001150 77  WS-CHAR2	   PIC	X(01).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-TYPE	   PIC	X(01).
001200 77  WS-INDEX-STORE  PIC	X(04).
001210 77  WS-INDEX-NAME   PIC	X(04).
001220 77  WS-BATCH	   PIC	9(05).
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
       77  WS-INVPASS	   PIC	X(06).

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

001260 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

           03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "PRP001.".
               05  W02-USER   PIC X(05) VALUE SPACES.
               
       COPY W12.WS.

001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

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

       COPY W40.WS.

       COPY FUNCTION.WS.

       01  W85-JCP-PASS.
	   03  W85-JPW	       PIC  X(06).
	   03  W85-JPWC REDEFINES W85-JPW
			       PIC  X(01) OCCURS 6.
	   03  W85-JPW2        PIC  X(06).
	   03  W85-JPWC2 REDEFINES W85-JPW2
			       PIC  X(01) OCCURS 6.
	   03  W85-ENCRYPT REDEFINES W85-JPW2.
	       05  W85-ENC1    PIC  X(01) COMP-X.
	       05  W85-ENC2    PIC  X(01) COMP-X.
	       05  W85-ENC3    PIC  X(01) COMP-X.
	       05  W85-ENC4    PIC  X(01) COMP-X.
	       05  W85-ENC5    PIC  X(01) COMP-X.
	       05  W85-ENC6    PIC  X(01) COMP-X.

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

004320 01  S17.
004330     03  LINE  3 COLUMN 31 FOREGROUND-COLOR 15
004340                           VALUE "SUPERVISOR PASSWORDS".
004350	   03  LINE  5 COLUMN 24 VALUE "Master     ".
004360	   03  LINE  6 COLUMN 24 VALUE "G-Ledger   ".
004370	   03  LINE  7 COLUMN 24 VALUE "Creditors  ".
004380	   03  LINE  8 COLUMN 24 VALUE "Guarantees ".
004390	   03  LINE  9 COLUMN 24 VALUE "CashDrawer ".
	   03	       COLUMN 44 VALUE "Float/Period End".
004400	   03  LINE 10 COLUMN 24 VALUE "Debtors    ".
	   03	       COLUMN 44 VALUE "Credit Control".
004410	   03  LINE 11 COLUMN 24 VALUE "Stock      ".
	   03	       COLUMN 44 VALUE "Quantities".
004420	   03  LINE 12 COLUMN 24 VALUE "Debtors    ".
	   03	       COLUMN 44 VALUE "Category 80".
004430	   03  LINE 13 COLUMN 24 VALUE "Stock      ".
	   03	       COLUMN 44 VALUE "Margin Sales/Prices & Negative".
	   03  LINE 14 COLUMN 24 VALUE "Copy Inv.  ".
004440	   03  LINE 15 COLUMN 24 VALUE "Job Costing".
004450	   03  LINE 20 COLUMN 24 VALUE "Initialize Password".

       COPY S99.CRT.

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
003240	   IF LS0-PALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.
       AA49.
             EXIT PROGRAM.

       COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

       AA200		  SECTION.
       AA201.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1 TO CLIN.
       AA205.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

      /
      *    *****    SYSTEM PASSWORDS	*****
      *
008640 YA000		       SECTION 5.
012050 YA300-PASS.
012060       DISPLAY CLR-SCREEN.
012070       DISPLAY S17.
012120	     MOVE 3		 TO WS-PARKEY.
012130	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INVPASS	 TO WS-INVPASS.
	     MOVE PAR-JCP	 TO W85-JPW2.
	     SUBTRACT 31	 FROM W85-ENC1.
	     SUBTRACT 13	 FROM W85-ENC2.
	     SUBTRACT 32	 FROM W85-ENC3.
	     SUBTRACT 23	 FROM W85-ENC4.
	     SUBTRACT 34	 FROM W85-ENC5.
	     SUBTRACT 43	 FROM W85-ENC6.
	     MOVE W85-JPWC2 (1)  TO W85-JPWC (6).
	     MOVE W85-JPWC2 (2)  TO W85-JPWC (5).
	     MOVE W85-JPWC2 (3)  TO W85-JPWC (4).
	     MOVE W85-JPWC2 (4)  TO W85-JPWC (3).
	     MOVE W85-JPWC2 (5)  TO W85-JPWC (2).
	     MOVE W85-JPWC2 (6)  TO W85-JPWC (1).
012120	     MOVE 5		 TO WS-PARKEY.
012130       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012080	     ACCEPT W25-PASS AT 2045
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
012090			 BACKGROUND-COLOR 5 NO-ECHO AUTO.
013370     IF NOT (W25-PASS = PAR-SUPER (1) OR
                   W25-PASS = "!@#$%^")
012110	       GO TO YA999.
012140 YA305.
	     MOVE 6		 TO WS-LENGTH.
012150	     ACCEPT PAR-SUPER (1) AT 0536
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(1)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012170	     ACCEPT PAR-SUPER (2) AT 0636
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(2)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012190	     ACCEPT PAR-SUPER (3) AT 0736
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(3)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012210	     ACCEPT PAR-SUPER (4) AT 0836
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(4)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012230	     ACCEPT PAR-SUPER (5) AT 0936
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(5)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012250	     ACCEPT PAR-SUPER (6) AT 1036
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(6)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012270	     ACCEPT PAR-SUPER (7) AT 1136
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(7)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012290	     ACCEPT PAR-SUPER (8) AT 1236
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(8)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012310	     ACCEPT PAR-SUPER (9) AT 1336
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING PAR-SUPER(9)
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     ACCEPT WS-INVPASS AT 1436
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING WS-INVPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     ACCEPT W85-JPW AT 1536
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE NO-ECHO AUTO.
	     CALL "CBL_TOUPPER" USING W85-JPW
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
012180	     PERFORM CHECK-CORRECT.
012410	   IF WS-OPTION = "N"
012420         GO TO YA305.
	     MOVE 1		 TO WS-LENGTH.
012430       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	     MOVE 3		 TO WS-PARKEY.
012130       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     MOVE WS-INVPASS	 TO PAR-INVPASS.
	     MOVE W85-JPWC (1)	 TO W85-JPWC2 (6).
	     MOVE W85-JPWC (2)	 TO W85-JPWC2 (5).
	     MOVE W85-JPWC (3)	 TO W85-JPWC2 (4).
	     MOVE W85-JPWC (4)	 TO W85-JPWC2 (3).
	     MOVE W85-JPWC (5)	 TO W85-JPWC2 (2).
	     MOVE W85-JPWC (6)	 TO W85-JPWC2 (1).
	     ADD 31		 TO W85-ENC1.
	     ADD 13		 TO W85-ENC2.
	     ADD 32		 TO W85-ENC3.
	     ADD 23		 TO W85-ENC4.
	     ADD 34		 TO W85-ENC5.
	     ADD 43		 TO W85-ENC6.
	     MOVE W85-JPW2	 TO PAR-JCP.
012430       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       YA999.
	     EXIT.
      /
015920 ZA000-INIT	       SECTION 8.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.
	     MOVE 9		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-GST    TO WS-VAT-GST.
       ZA27.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
008740	     OPEN INPUT SECUR.
007570	     MOVE 1		 TO WS-SECKEY.
       ZA28.
007580       READ SECUR.
007590     IF RUNTIME-ERROR
007600         IF RECORD-LOCKED
007610             GO TO ZA28.
007620     IF WS-STATUS NOT = "00"
007630         DISPLAY "Disk error - ZA30" AT 2312
                        WITH FOREGROUND-COLOR 14
007640         STOP RUN.
007650       MOVE 1 TO WS-S1.
007660 ZA30.
007670       MOVE SEC-ASCII (WS-S1) TO W15-ASCII (WS-S1).
007680     IF WS-S1 < 256
007690         ADD 1 TO WS-S1
007700         GO TO ZA30.
             CLOSE SECUR.
044780       GO TO ZA999-EXIT.
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
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
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
016280 ZB000-ERROR	     SECTION 8.
      
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
041850     IF WS-F-ERROR = 15
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
