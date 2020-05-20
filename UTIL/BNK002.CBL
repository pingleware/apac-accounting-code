      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    *******   **    **  **    **    ****      ****     ******   *
      *    **	 **  ***   **  **   **	  **  **    **	**   **    **  *
      *    **	 **  ****  **  **  **	 **    **  **	 **	  **   *
      *    *******   ** ** **  *****	 **    **  **	 **	**     *
      *    **	 **  **  ****  **  **	 **    **  **	 **   **       *
      *    **	 **  **   ***  **   **	  **  **    **	**   **        *
      *    *******   **    **  **    **    ****      ****    ********  *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   B A N K   D E P O S I T   S L I P   &   P D   C H E Q U E S  *
      *                                                                *
      *   D A T E   C O N T R O L				       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     BNK002.
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
0L0060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY PARAM.SL.

000160 DATA DIVISION.
000170 FILE SECTION.

       COPY APACFIDS.FDE.

000210 COPY PARAM.FDE.

      /
000190 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000200 77  WS-PARKEY	       PIC  9(04)  COMP-5.
000210 77  WS-DATE             PIC  9(06)  COMP-3.
000220 77  WS-OPTION           PIC  X(01).
000240 77  WS-ERROR            PIC  9(01).
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN	   PIC	9(02)	 COMP-5.
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
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
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

001840 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY W12.WS.

       COPY W20.WS.

000490 01  W25-CALCS.
000500     03  W25-RESULT      PIC  9(07)V99.
000510     03  W25-RESULT1 REDEFINES W25-RESULT.
000520         05  FILLER      PIC  9(01).
000530         05  W25-WHOLE   PIC  9(06).
000540         05  W25-DECIMAL PIC  9(02).

000550 01  W30-MONTHS.
000560     03  W30-MONTH-NAMES.
000570         05  W30-ENG     PIC  X(36)    VALUE
000580                         "JanFebMarAprMayJunJulAugSepOctNovDec".
000590     03  W30-MONTH REDEFINES W30-MONTH-NAMES
000600                         PIC  X(03)  OCCURS 12.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000750	SCREEN SECTION.

       COPY BLANK.CRT.

002360 01  S50A.
002390     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15
                                 VALUE "BANKING DATE".
002400     03  LINE  5 COLUMN 12 VALUE "Current Date :".
002410     03  LINE  9 COLUMN 12 VALUE "New Date     :".

002420 01  S51A.
002430	   03  LINE  5 COLUMN 27 FOREGROUND-COLOR 15 PIC  Z9/99/9999
                                 USING PAR-CASH-BDMY AUTO.

002450 01  S52.
002460     03  LINE  9 COLUMN 27 FOREGROUND-COLOR 15 
002470				 PIC  Z9/99/9999 USING W22-DTE3 AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000	       SECTION.
       AA00.
	     PERFORM ZA000-INIT.
	     PERFORM AI000.
       AA05.
	     EXIT PROGRAM.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY PARAM.WR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY FUNCTION.CRT.

      *
      *    ****    CHANGE BANKING DATE    ****
      *
005320 AI000           SECTION.
005330 AI00.
005340	     MOVE 7		 TO WS-PARKEY.
005430 AI01.
005440	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
005480       MOVE PAR-CASH-BDMY  TO W20-DTE.
005490 AI02.
005500       DISPLAY CLR-SCREEN.
005510       DISPLAY S50A.
005520       DISPLAY S51A.
005530	     ACCEPT S52.
002820	     MOVE W22-DD3	 TO W20-DAY.
002830	     MOVE W22-MM3	 TO W20-MONTH.
	     MOVE W22-CC3	 TO W20-CENT.
002840	     MOVE W22-YY3	 TO W20-YEAR.

       AI03.
005540	     MOVE ZERO		 TO WS-ERROR.
005550	   IF W20-DAY < 01 OR > 31
005560	       GO TO AI10.
005570	   IF W20-MONTH < 01 OR > 12
005580	       GO TO AI10.
005590	   IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
005600	       GO TO AI15.
005610	   IF W20-MONTH = 02 AND W20-DAY > 28
005620	       GO TO AI05.
005630	   IF W20-DAY > 30
005640	       GO TO AI10.
005650	     GO TO AI15.
005660 AI05.
005670	   IF W20-DAY > 29
005680	       GO TO AI10.
008610	     DIVIDE W20-CNYR BY 4
				 GIVING W25-RESULT.
005700	   IF W25-DECIMAL = 0
005710	       GO TO AI15.
005720 AI10.
005730	     MOVE 1		 TO WS-ERROR.
005740 AI15.
005750	     EXIT.
005760 AI20.
005770     IF WS-ERROR = 1
005780	       MOVE "Invalid Date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
005800         GO TO AI02.
005810       MOVE W20-DAY        TO W21-DD.
005820       MOVE W20-MONTH      TO W21-MM.
005830	     MOVE W20-CCYY	 TO W21-CCYY.
005840       DISPLAY (12, 12) "Banking Date - " W21-DD " "
005850			      W30-MONTH (W21-MM) "," W21-CCYY.
005860       DISPLAY (14, 12) "Is date correct (Y/N) ?".
005870       ACCEPT (14, 36) WS-OPTION WITH FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
005930	   IF WS-OPTION = "N"
005940         GO TO AI02.
005950	     MOVE W22-DTE3	 TO PAR-CASH-BDMY.
005960       MOVE W21-DTE        TO PAR-CASH-BYMD.
005970	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       AI999.
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
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
	     GO TO ZA999-EXIT.
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
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY.
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
