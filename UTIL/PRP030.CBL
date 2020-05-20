      $SET LINKCOUNT"312"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******     ****     *
      *   **	**  **	  **  **    **	 **  **   **	**   **  **    *
      *   **	**  **	  **  **    **	**    **	**  **	  **   *
      *   *******   ******    *******	**    **     ****   **	  **   *
      *   **	    **	 **   **	**    **	**  **	  **   *
      *   **	    **	  **  **	 **  **   **	**   **  **    *
      *   **	    **	  **  **	  ****	   ******     ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   V A T   R A T E S   P R O G R A M       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP030.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
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

000140 COPY DEPART.SL.

       COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000220 COPY DEPART.FDE.

       COPY PARAM.FDE.

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
002320 77  WS-PARKEY       PIC  9(04)    COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
001100 77  WS-OPTION	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
001160 77  WS-COMP	   PIC	9(01).
001170 77  WS-TYPE	   PIC	X(01).
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

       COPY DEPART.ID.

       COPY PARAM.ID.

	   03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "PRP030.".
               05  W02-USER   PIC X(05) VALUE SPACES.
               
       COPY W05.VAT.

001660 01  W10-INPUT.
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

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

       COPY W40.WS.

       COPY FUNCTION.WS.

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

004190 01  S14.
004200	   03  LINE  3 COLUMN 33 FOREGROUND-COLOR 15
004210				 VALUE "AMEND VAT RATES".
	   03  LINE  5 COLUMN 14 VALUE "    Rate 1     Rate 2     Rate 3
      - 			 "     Rate 4     Rate 5     Rate 6".
004220	   03  LINE  6 COLUMN  2 VALUE "New Rate  :".
004220	   03  LINE  7 COLUMN  2 VALUE "Prev Rate :".
	   03  LINE  8 COLUMN  2 VALUE "Date      :".

005330 01  S27.
	   02  S27-VAT-1.
005340	   03  LINE  6 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R1 AUTO.
005340	   03  LINE  7 COLUMN 18 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R1 AUTO.
	   03  LINE  8 COLUMN 14 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(1) AUTO.
	   02  S27-VAT-2.
005340	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R2 AUTO.
005340	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R2 AUTO.
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(2) AUTO.
	   02  S27-VAT-3.
005340	   03  LINE  6 COLUMN 40 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R3 AUTO.
005340	   03  LINE  7 COLUMN 40 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R3 AUTO.
	   03  LINE  8 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(3) AUTO.
	   02  S27-VAT-4.
005340	   03  LINE  6 COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R4 AUTO.
005340	   03  LINE  7 COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R4 AUTO.
	   03  LINE  8 COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(4) AUTO.
	   02  S27-VAT-5.
005340	   03  LINE  6 COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R5 AUTO.
005340	   03  LINE  7 COLUMN 62 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R5 AUTO.
	   03  LINE  8 COLUMN 58 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(5) AUTO.
	   02  S27-VAT-6.
005340	   03  LINE  6 COLUMN 73 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-VAT-R6 AUTO.
005340	   03  LINE  7 COLUMN 73 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC ZZ9.99
005350				 USING W05-PVAT-R6 AUTO.
	   03  LINE  8 COLUMN 69 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC 99/99/9999
				 USING WS-VAT-DATE(6) AUTO.

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

       COPY DATE.CHK.

       COPY DATE.CNV.

      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY DEPART.RD.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY DEPART.WR.

      /
      *    *****    V A T   R A T E S	 *****
      *
008640 YA000		       SECTION 5.
010150 YA00.
010160       DISPLAY CLR-SCREEN.
010170       DISPLAY S14.

010180 YA05.
010190       ACCEPT S27.
012180	     PERFORM CHECK-CORRECT.
010270	   IF WS-OPTION = "N"
010280	       GO TO YA05.

       YA10.
	     MOVE WS-VAT-DATE(1) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 1)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-1
	       GO TO YA10.

       YA15.
	     MOVE WS-VAT-DATE(2) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 2)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-2
	       GO TO YA15.

       YA20.
	     MOVE WS-VAT-DATE(3) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 3)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-3
	       GO TO YA20.

       YA25.
	     MOVE WS-VAT-DATE(4) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 4)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-4
	       GO TO YA25.

       YA30.
	     MOVE WS-VAT-DATE(5) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 5)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-5
	       GO TO YA30.

       YA35.
	     MOVE WS-VAT-DATE(6) TO W10-EDTE.
	     PERFORM CHECK-DATE.
	   IF WS-ERROR = 1
	       MOVE "Invalid date (Rate 6)"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       ACCEPT S27-VAT-6
	       GO TO YA35.
	     MOVE 1		 TO W05-V-RATE.

       YA40.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
	     MOVE WS-VAT-DATE(W05-V-RATE)
				 TO W22-DTE3.
	     PERFORM SWITCH-DATE-TO-CYMD THRU SWITCH-DATE-EXIT.
	     MOVE W22-DTE2	 TO DPT-R-DATE.
	     MOVE W05-VAT(W05-V-RATE)
				 TO DPT-RATE.
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE W05-VAT(WS-S1) TO DPT-P-RATE.
	     PERFORM REWRITE-DEPART-UNLOCK THRU WRITE-DEPART-EXIT.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO YA40.

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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
016210	     MOVE 1		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO W22-DTE2.
	     PERFORM SWITCH-DATE-TO-DMCY.
	     MOVE W22-DTE3	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
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
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
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
045400	   IF WS-STATUS NOT = "94"
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440	       DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
