      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   **	      *******	  ****	   ******      **      *
      *   **	**  **	      **    **	 **  **   **	**    ***      *
      *   **	    **	      **    **	**    **  **	**     **      *
      *   **	    **	      *******	**    **   *******     **      *
      *   **   ***  **	      **	**    **	**     **      *
      *   **	**  **	      **	 **  **   **	**     **      *
      *    ******   ********  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     Z E R O   A L L   A M O U N T S   IN THE G/L ACCOUNT FILE  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     GLP091.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MAY 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2008
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
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

       COPY GACCNT.SL.

000180 COPY GLTRAN.SL.

       COPY GLINST.SL.

000310 DATA DIVISION.
000320 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000300 COPY PARAM.FDE.

       COPY GACCNT.FDE.

000340 COPY GLTRAN.FDE.

       COPY GLINST.FDE.

      /
002780 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002790 77  WS-S1	   PIC	9(05) COMP-0.
002790 77  WS-S2	   PIC	9(05) COMP-0.
002790 77  WS-S3	   PIC	9(05) COMP-0.
002790 77  WS-S4	   PIC	9(05) COMP-0.
001700 77  WS-PARKEY       PIC  9(04) COMP-5 VALUE 1.
002850 77  WS-GLKEY        PIC  9(05) COMP-0 VALUE 1.
001720 77  WS-GTRKEY       PIC  9(06) COMP-5 VALUE 1.
001460 77  WS-GLINSTKEY    PIC  9(05) COMP-0.
001730 77  WS-NETKEY       PIC  9(04) COMP-5 VALUE 1.
002860 77  WS-OPTION       PIC  X(01).
001960 77  WS-INDG         PIC  9(01) VALUE ZERO.
002020 77  WS-DEM          PIC  9(01) VALUE ZERO.
002880 77  WS-ERROR        PIC  9(01) VALUE 0.
002900 77  WS-EKEY         PIC  Z(05).
002910 77  WS-DEBGL        PIC  9(06) COMP-3.
002920 77  WS-CREGL        PIC  9(06) COMP-3.
002930 77  WS-STKGL        PIC  9(06) COMP-3.
002940 77  WS-BNKGL        PIC  9(06) COMP-3.
002950 77  WS-UNPROF       PIC  9(06) COMP-3.
       77  WS-CHECK        PIC  9(05) COMP-0.
       77  WS-ZERO         PIC S9(07)V99 COMP-3 VALUE ZERO.
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

002220 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003060 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000320 COPY GACCNT.ID.

000340 COPY GLTRAN.ID.

       COPY GLINST.ID.

       COPY PARAM.ID.

       COPY W05.GL.

       COPY W40.WS.

       COPY FUNCTION.WS.

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.


       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004560 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
003370 AA000		       SECTION.
003380 AA00.
003240	   IF LS0-GLLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA35.
003390	     PERFORM ZA000-INIT.
006570     IF WS-ERROR NOT = 0
006580	       MOVE ZERO	 TO WS-OPTION
006590         GO TO AA35.

003880 AA05.
003910       DISPLAY "CLEAR THE GENERAL LEDGER ACCOUNTS" AT 0224
                      WITH FOREGROUND-COLOR 15.
	   IF LS-DEM = 4
	       MOVE 9		 TO LS-DEM
	   ELSE
	       PERFORM AA20
	       IF WS-ERROR NOT = ZERO
		   GO TO AA35.
003920       DISPLAY "Reading " AT 0405 W02-GACCNT " :".
003930       DISPLAY "Writing " AT 0505 W02-GACCNT " :".
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO AA35.

004720 AA10.
	     PERFORM READ-GACCNT-NEXT-LOCK THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               GO TO AA16.
004820	     DISPLAY G-AC AT 0434 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5.
002490	     MOVE ZERO TO G-DT G-CR G-BAL G-OBAL.
             MOVE 1 TO WS-S1.
       AA15.
002550       MOVE ZERO TO G-LVAL (WS-S1) G-BVAL (WS-S1) G-VAL (WS-S1).
           IF WS-S1 < 12
               ADD 1 TO WS-S1
               GO TO AA15.
005260	     DISPLAY G-AC AT 0534 WITH FOREGROUND-COLOR 3 HIGHLIGHT
				       BACKGROUND-COLOR 5.
             PERFORM REWRITE-GACCNT-UNLOCK THRU WRITE-GACCNT-EXIT.
             ADD 1               TO WS-GLKEY.
             GO TO AA10.

       AA16.
             MOVE 1              TO G-BATCH 
                                    G-INST 
                                    G-GLINST 
                                    G-GLTRAN.
             MOVE ZERO           TO G-YEAR-END 
                                    G-LAST-YE
                                    G-OPEN.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             CLOSE GLTRAN.
             OPEN OUTPUT GLTRAN.
             CLOSE GLTRAN.
	     OPEN I-O GLTRAN.
             CLOSE GLINST.
             OPEN OUTPUT GLINST.
             CLOSE GLINST.
	     OPEN I-O GLINST.
	     GO TO AA35.

       AA20.
	     PERFORM SAVE-SCREEN.
032000	     MOVE 5		 TO WS-PARKEY.
032010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
032020       MOVE PAR-PW         TO W85-PASS. 
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "³ " AT 1227 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1327 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1244
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM RESTORE-SCREEN.
018370	   IF NOT (W40-CSHPASS = W85-SUPER (1) OR
				 W85-SUPER (2) OR
				 "ZEROGL")
               MOVE 1            TO WS-ERROR
           ELSE
               MOVE ZERO         TO WS-ERROR.
018260       DISPLAY "                       " AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

007240 AA35.
             EXIT PROGRAM.

       COPY LOCKED.REC.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      * COPY CLEAR.CRT.

007600 AB000           SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD.

       COPY GLTRAN.RD.

       COPY GLINST.RD.

      /
008790 AC000           SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY GACCNT.WR.

       COPY GLTRAN.WR.

       COPY GLINST.RD.

      / 
006800 ZA000-INIT              SECTION 8.
006810 ZA00.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
035290	     DISPLAY CLR-SCREEN.
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
	     MOVE "GLINST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLINST.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
035680	     MOVE 1		 TO WS-PARKEY.
035690       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
035710       MOVE PAR-DMY TO W05-TODAY.
035720       MOVE PAR-YMD TO W05-TYMD.
             MOVE 4              TO WS-NETKEY.
035770       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
036250       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA55.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (11) WS-TCHR (45)
021940                      WS-TCHR (63).
021950       MOVE WS-G3  TO WS-MCHR (11) WS-MCHR (45)
021960                      WS-MCHR (63)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (11) WS-BCHR (45)
021980                      WS-BCHR (63).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
036260 ZA200.
036270       DISPLAY "Files locked - Try later" AT 2512
036280                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
036290       DISPLAY "Press any key" AT 2512 
036300                WITH FOREGROUND-COLOR 14.
036310       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
007330 ZA999.
007340       EXIT.
      /
036360 ZB000         SECTION 9.
      
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
041640     ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 10
041860         MOVE W02-GLINST TO WS-FILE
041870         MOVE WS-GLINSTKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 11
041860         MOVE W02-GLTRAN TO WS-FILE
041870         MOVE WS-GTRKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
		      WS-STATUS WITH FOREGROUND-COLOR 3 HIGHLIGHT
				     BACKGROUND-COLOR 5
		     ": " WS-STAT-MESSAGE
			  WITH FOREGROUND-COLOR 7 HIGHLIGHT
			       BACKGROUND-COLOR 4.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
			WS-KEY WITH FOREGROUND-COLOR 3 HIGHLIGHT
				    BACKGROUND-COLOR 5
			" " WS-KEYX WITH FOREGROUND-COLOR 3 HIGHLIGHT
					 BACKGROUND-COLOR 5
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
