      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******   ********   *
      *   **	**  **	      **    **	 **  **   **	**  **	       *
      *   **	**  **	      **    **	**    **  **	    **	       *
      *   **	**  *****     *******	********  **	    *****      *
      *   **	**  **	      **    **	**    **  **   ***  **	       *
      *   **	**  **	      **    **	**    **  **	**  **	       *
      *   *******   ********  *******	**    **   ******   ********   *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   S E T U P   M O N T H	  E N D	  D A T E S   F O R   T H E    *
      *   P R E V I O U S   5	M O N T H S   -   B A S E D   O N      *
      *   T H E   D E B T O R	M O N T H - E N D   D A T E	       *
      *   P A R A M E T E R   I N   T H E   C O N T R O L   F I L E    *
      *                                                                *
      *     VERSION 8.11.06 - October 2006			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DEBAGE.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 2006.

		   COPYRIGHT NOTICE: COPYRIGHT (C) - 2006
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
000080                CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY PARAM.SL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
003290 77  WS-NETKEY	   PIC 9(04)	 COMP-5 VALUE 1.
003340 77  WS-PARKEY       PIC 9(04)     COMP-5 VALUE 1.
000530 77  WS-TRANS	   PIC 9(05)	 COMP-0 VALUE 1.
000540 77  WS-EKEY         PIC Z(04)9.
000560 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG         PIC X(02).
       77  WS-OPTION       PIC X(01).
       77  WS-JUST         PIC X(01) VALUE "N".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
002460         05  WS-MCH  PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

       COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

       COPY PARAM.ID.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

       01  W15-EDIT.
           03  W15-ACNO.
               05  W15-NO      PIC  Z(04)9.
           03  W15-AC REDEFINES W15-ACNO.
               05  W15-1ST     PIC  X(01).
               05  W15-2TO5    PIC  X(04).

       COPY W20.WS.

       01  W30-AGE-DATES.
	   03  W30-120	       PIC  9(08) COMP-3.
	   03  W30-90	       PIC  9(08) COMP-3.
	   03  W30-60	       PIC  9(08) COMP-3.
	   03  W30-30	       PIC  9(08) COMP-3.
	   03  W30-CUR	       PIC  9(08) COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

000590 COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910	   03  LINE  2 COLUMN 25 FOREGROUND-COLOR 15
00920		       VALUE "DEBTOR MONTH-END DATES CONVERT".

001080 01  S04.
001090     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "DEBTOR month-end dates setup".
001110     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Convertion unsuccessfull !".
001160     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION USING LS-PARID LS-USER-ID.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE WS-MID-LNE2.
041240 AA02.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO AA02.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(16) WS-TCHR(41)
021940				    WS-TCHR(64) WS-TCHR(71).
021950	     MOVE WS-G3 	 TO WS-MCHR(16) WS-MCHR(41)
021960				    WS-MCHR(64) WS-MCHR(71)
				    WS-MCHR(1)	WS-MCHR(80)
				    WS-MCH(1)	WS-MCH(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
001250	     DISPLAY CLR-SCREEN.
001270	     PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
044650	     MOVE 1		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE "8.12"	 TO DEB-VER.
	     MOVE W30-120	 TO DEB-120-DTE.
	     MOVE W30-90	 TO DEB-90D-DTE.
	     MOVE W30-60	 TO DEB-60D-DTE.
	     MOVE W30-30	 TO DEB-30D-DTE.
	     MOVE W30-CUR	 TO DEB-CUR-DTE.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
001910	     DISPLAY S04.
             ACCEPT WS-OPTION AT 1639 WITH FOREGROUND-COLOR 15 AUTO.

       AA25.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

       COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
002460 ZA000           SECTION.  
002470 ZA00.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200.
002490	   IF NOT (WS-STATUS = "00" OR "41")
002500	       MOVE 15		 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520	     MOVE 1		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
002540	   IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
	     MOVE PAR-DMY	 TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
042140	     OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
      *
      *    ****    G E T   D E B T O R	 M O N T H - E N D   D A Y
      *
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	   IF DEB-MTH-END < 21 OR > 31 AND < 99
	       MOVE 99		 TO DEB-MTH-END.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
	     MOVE W12-T-YMD	 TO W22-DTE2.
	     MOVE DEB-MTH-END	 TO W22-DD2
	   IF DEB-MTH-END = 99
	       IF W22-MM2 = 02
		   MOVE 28	 TO W22-DD2
	       ELSE
	       IF W22-MM2 = 04 OR 06 OR 09 OR 11
		   MOVE 30	 TO W22-DD2
	       ELSE
		   MOVE 31	 TO W22-DD2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  The following has been included for Promotion Products.  ³
      *    ³  "DEM" included for testing purposes.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W02-SYSID = "PRO" OR "DEM"
	       IF W22-MM2 = 09
		   IF W22-YY2 = 06
		       MOVE 25	 TO W22-DD2.

       ZA47.
           IF W22-MM2 > 01
               SUBTRACT 1        FROM W22-MM2
           ELSE
               MOVE 12           TO W22-MM2
	       SUBTRACT 1	 FROM W22-CY2.
	   IF W22-MM2 = 02
	       IF DEB-MTH-END > 27
		   MOVE 28	 TO W22-DD2
	       END-IF
           ELSE
	   IF W22-MM2 = 04 OR 06 OR 09 OR 11
	       IF DEB-MTH-END > 29
		   MOVE 30	 TO W22-DD2
	       END-IF
           ELSE
	   IF DEB-MTH-END > 30
	       MOVE 31		 TO W22-DD2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  The following has been included for Promotion Products.  ³
      *    ³  "DEM" included for testing purposes.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W02-SYSID = "PRO" OR "DEM"
	       IF W22-MM2 = 09
		   IF W22-YY2 = 06
		       MOVE 25	 TO W22-DD2.

       ZA48.
           IF W12-MM = W22-MM2
               IF W12-DD < W22-DD2
                   MOVE W12-DD   TO W22-DD2.
	     MOVE W22-DTE2	  TO W30-CUR.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-30.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-60.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-90.
	     PERFORM ZA47.
             MOVE W22-DTE2       TO W30-120.
044780	     GO TO ZA999-EXIT.

002960	     GO TO ZA999-EXIT.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150                WITH FOREGROUND-COLOR 14.
020160       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
	     MOVE 9		 TO WS-ERROR.

003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
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
041840	   ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE W02-PARAM	 TO WS-FILE
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
