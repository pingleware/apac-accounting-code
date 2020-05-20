      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******     ****     *
      *   **	**     **     **    **	 **  **   **	**   **  **    *
      *   **	       **     **    **	**    **  **	**  **	  **   *
      *    ******      **     *******	**    **   ******   **	  **   *
      * 	**     **     **	**    **  **	**  **	  **   *
      *   **	**     **     **	 **  **   **	**   **  **    *
      *    ******      **     **	  ****	   ******     ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        S T O C K   O P T I O N S   P R O G R A M	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP080.
000030 AUTHOR.         J.W.LEMMON (CSS).
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
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

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
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001020 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
001070 77  WS-NUM	   PIC	Z9.
001070 77  WS-SNO	   PIC	9(02).
001100 77  WS-OPTION	   PIC	X(01).
001120 77  WS-ERROR	   PIC	9(01).
       77  WS-PASS	   PIC	X(06).
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
002430	   03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC	X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
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

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY W12.WS.

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

       01  S34.
	   03  LINE 16 COLUMN 53 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z9 USING PAR-SLNGTH AUTO.

004870 COPY S99.CRT.

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
003240	   IF LS0-STLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM BA000.

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
      /
      *    ****    READ FILES ROUTINES
      *
006630 AC000-READ              SECTION.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY PARAM.WR.

      /
      *    *****    S T O C K	O P T I O N S	*****
      *
008640 BA000		       SECTION 50.
       BA00.
013650	     DISPLAY CLR-SCREEN.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 34		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
005660	     DISPLAY "STOCK OPTIONS" AT 0334
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0422
		      WITH FOREGROUND-COLOR 2 HIGHLIGHT
			   BACKGROUND-COLOR 2
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 0522 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Answer "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "es or "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "o to all options "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 0622 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "other than the first one!       "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 0722 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Stock code - "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "A" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "lpha/" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 2
		     "umeric       "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 0822 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Stock code - Use ITM extension   "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 0922 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Stock code - Use 18 characters   "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 1022 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Allow for packs                  "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "Control cases                    "
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "À" AT 1222 WITH FOREGROUND-COLOR 2 HIGHLIGHT
				       BACKGROUND-COLOR 2
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     PERFORM SCREEN-SHADOW.
013670	     MOVE 3		 TO WS-PARKEY.
013680	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	   IF NOT (PAR-USE-ITM = "N" OR "Y")
	       MOVE "N" 	 TO PAR-USE-ITM.
	   IF NOT (PAR-EXT-STK = "N" OR "Y")
	       MOVE "N" 	 TO PAR-EXT-STK.
	   IF NOT (PAR-PACKS = "N" OR "Y")
	       MOVE "N" 	 TO PAR-PACKS.
	   IF NOT (PAR-CASES = "N" OR "Y")
	       MOVE "N" 	 TO PAR-CASES.
	   IF NOT (PAR-STOCK = "N" OR "A")
	       MOVE "A" 	 TO PAR-STOCK.

       BA05.
013900	     ACCEPT PAR-STOCK AT 0751
013910		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
	       END-EVALUATE
	       GO TO BA05.
	     CALL "CBL_TOUPPER" USING PAR-STOCK
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	  IF NOT (PAR-STOCK = "N" OR "A")
	      GO TO BA05.

       BA10.
013900	     ACCEPT PAR-USE-ITM AT 0855
013910		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
	       END-EVALUATE
	       GO TO BA05.
	     CALL "CBL_TOUPPER" USING PAR-USE-ITM
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	  IF NOT (PAR-USE-ITM = "N" OR "Y")
	      GO TO BA10.
	  IF PAR-USE-ITM = "Y"
	      MOVE "N"		 TO PAR-EXT-STK
	      DISPLAY PAR-EXT-STK AT 0955
		      WITH BACKGROUND-COLOR 5
			   FOREGROUND-COLOR 7 HIGHLIGHT
	      GO TO BA20.

       BA15.
013900	     ACCEPT PAR-EXT-STK AT 0955
013910		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
	       END-EVALUATE
	       GO TO BA15.
	     CALL "CBL_TOUPPER" USING PAR-EXT-STK
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	  IF NOT (PAR-EXT-STK = "N" OR "Y")
	      GO TO BA15.

       BA20.
013900	     ACCEPT PAR-PACKS AT 1040
013910		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
	       END-EVALUATE
	       GO TO BA20.
	     CALL "CBL_TOUPPER" USING PAR-PACKS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	  IF NOT (PAR-PACKS = "N" OR "Y")
	      GO TO BA20.

       BA25.
013900	     ACCEPT PAR-CASES AT 1138
013910		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
	       END-EVALUATE
	       GO TO BA25.
	     CALL "CBL_TOUPPER" USING PAR-CASES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	  IF NOT (PAR-CASES = "N" OR "Y")
	      GO TO BA25.
	  IF PAR-STOCK = "N"
	      PERFORM BA30.
	     MOVE 15		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
013990	   IF WS-OPTION = "N"
014000	       GO TO BA05.
014010	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	     PERFORM RESTORE-SCREEN-3.
	     GO TO BA999.

       BA30.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 16		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 31		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1524
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ " AT 1624 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Item Code Length (3 to 14)    "
		      WITH FOREGROUND-COLOR 5 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1724 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     ACCEPT S34
	   IF PAR-SLNGTH < 3
	       MOVE 3		 TO PAR-SLNGTH
	   ELSE
	   IF PAR-SLNGTH > 14
	       MOVE 14		 TO PAR-SLNGTH.
	     PERFORM RESTORE-SCREEN-2.

       BA999.
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
016210	     MOVE 1		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G11	 TO WS-BCH  (1).
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
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
