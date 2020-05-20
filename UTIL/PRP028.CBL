      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******    ******    *
      *   **	**  **	  **  **    **	 **  **   **	**  **	  **   *
      *   **	**  **	  **  **    **	**    **       **   **	  **   *
      *   *******   ******    *******	**    **     **      ******    *
      *   **	    **	 **   **	**    **   **	    **	  **   *
      *   **	    **	  **  **	 **  **   **	    **	  **   *
      *   **	    **	  **  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    U S B / W I N D O W S   N U M B E R	 O F   D E B T O R     *
      *    S T A T E M E N T   D E T A I L   L I N E S		       *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
       IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP028.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 2009.

	   This program is used to setup the default number of detail
	   lines to be used for Debtor Statements when using the
	   WINDOWS/USB/Network printer. (Printers that do not allow for
	   inline control characters).

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2011
                                     by James William Lemmon.
                                       (Id No. 4412165050082).

                   All rights reserved.

		   e-mail jwlemmon@apacaccounting.com

000050 SECURITY.
                   This program is free software; you can redistribute
                   it and/or modify it under the terms of the GNU General
                   Public License as published by the Free Software
                   Foundation; either version 2 of the License, or (at
                   your option) any later version.

                   This program is distributed in the hope that it will
                   be useful, but WITHOUT ANY WARRANTY; without even the
                   implied warranty of MERCHANTABILITY or FITNESS FOR A
                   PARTICULAR PURPOSE.  See the GNU General Public License
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

000120 COPY PARAM.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

      /

       working-storage section.
       77  WS-CHECK        PIC  X(18)    VALUE
                           "aeWlimemnomLalismJ".
003260 77  WS-S1           PIC  9(04)    COMP-5.
003270 77  WS-S2           PIC  9(04)    COMP-5.
003280 77  WS-S3           PIC  9(04)    COMP-5.
003290 77  WS-S4           PIC  9(04)    COMP-5.
003300 77  WS-S5           PIC  9(04)    COMP-5.
003310 77  WS-S6           PIC  9(04)    COMP-5.
003320 77  WS-S7           PIC  9(04)    COMP-5.
003330 77  WS-S8           PIC  9(04)    COMP-5.
003330 77  WS-LEFT         PIC  9(04)    COMP-5.
003330 77  WS-RIGHT        PIC  9(04)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SINGLE       PIC  X(01).
       77  WS-ERROR        PIC  9(01)    VALUE ZERO.
       77  WS-LINES        PIC  9(02)    VALUE ZERO.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
       77  TODAY-DDMMYY    PIC  9(08) COMP-5.
       77  WS-USUB         PIC  9(04) COMP-5.

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

       COPY PARAM.ID.

       COPY W12.WS.

001510 01  W20-DATE.
001520     03  W20-DAY         PIC 9(02).
001530     03  W20-MONTH       PIC 9(02).
	   03  W20-CENT        PIC 9(02).
001540     03  W20-YEAR        PIC 9(02).
001550 01  W20-DTE1 REDEFINES W20-DATE.
001560	   03  W20-DTE	       PIC 9(08).
001550 01  W20-DTE2 REDEFINES W20-DATE.
	   03  W20-CC	       PIC 9(02).
001520     03  W20-YY          PIC 9(02).
001530     03  W20-MM          PIC 9(02).
001540     03  W20-DD          PIC 9(02).
       01  W20-LEAP REDEFINES W20-DATE.
	   03  FILLER	       PIC 9(04).
	   03  W20-CY	       PIC 9(04).

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  W25-DAYS    PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  FILLER      PIC  9(02).
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
001630	   03  W25-PASS.
001640	       05  W25-PASS1   PIC  9(03).
001650	       05  W25-PASS2   PIC  9(03).
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

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
001230 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				LS0-SECURITY.

005070 AA000-MAIN	       SECTION.
005080 AA00.
007760	     PERFORM ZA000-INIT.
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.

       AA49.
	     EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.RC2.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
006630 AC000-READ	       SECTION.

       COPY PARAM.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
006940 AD000-WRITE	       SECTION.

       COPY PARAM.WR.

      /    **************************************************
      *    ***** USB/WINDOWS PRINTER - DETAIL LINES FOR *****
      *    ***** DEBTOR STATEMENTS			*****
      *    **************************************************
008640 YA000		       SECTION 5.
011760 YA00.
011770	     MOVE 6		 TO WS-PARKEY.
011780       DISPLAY CLR-SCREEN.
011790	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-STM-DET < 25 OR > 38
	       MOVE 34		 TO PAR-STM-DET.
	     MOVE PAR-STM-DET	 TO WS-LINES.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 37		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0921
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "¿" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1021 WITH FOREGROUND-COLOR 0
				      BACKGROUND-COLOR 4
		     "  DETAIL LINES PER DEBTOR STATEMENT  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³" AT 1121
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     " Number of detail lines required     "
		      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 1221
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "Valid lines 25 to 38" AT 2502.

       YA05.
	     ACCEPT WS-LINES AT 1156
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	   IF NOT(WS-LINES > 25 AND < 38)
	       GO TO YA05.
011790	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
	     MOVE WS-LINES	 TO PAR-STM-DET.
012030	     PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       YA999.
	     EXIT.

      /
044130 ZA000-INIT	       SECTION 80.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
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

       ZA01B.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
	     GO TO ZA999-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the Screen Graphics Characters: ³ Ä Ú ¿ À Ù Â Á etc ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
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
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.

044790 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 81.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.

       COPY DISPERR.PRO.
