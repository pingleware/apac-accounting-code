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
      *    P A R A M E T E R   S T M / I N V O I C E   F O R M A T S   *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jan 2012 - Include Format 99 which calls the INVF12W/CRNF12W  *
      * 	    program to produce either an Invoice or Credit Note*
      * 	    via the Windows spooller.			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP025.
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
000950 77  WS-S1	       PIC 9(04)     COMP-5.
000960 77  WS-S2               PIC 9(04)     COMP-5.
000970 77  WS-S3               PIC 9(04)     COMP-5.
000980 77  WS-S4               PIC 9(04)     COMP-5.
000990 77  WS-S5               PIC 9(04)     COMP-5.
001000 77  WS-S6               PIC 9(04)     COMP-5.
001000 77  WS-S7	       PIC 9(04)     COMP-5.
001000 77  WS-S8	       PIC 9(04)     COMP-5.
001010 77  WS-PARKEY           PIC 9(04)     COMP-5.
001010 77  WS-PRNKEY           PIC 9(04)     COMP-5.
001010 77  WS-DEPKEY           PIC 9(04)     COMP-5.
001010 77  WS-NETKEY           PIC 9(04)     COMP-5.
001020 77  WS-SECKEY           PIC 9(04)     COMP-5.
001070 77  WS-NUM              PIC Z9.
001070 77  WS-SNO              PIC 9(02).
       77  WS-ADV-RATE         PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE             PIC 9(04)     COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI       PIC  X(04).
003100 77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001100 77  WS-OPTION           PIC X(01).
001100 77  WS-VAT-GST          PIC X(01).
001120 77  WS-ERROR            PIC 9(01).
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
001140 77  WS-CHAR1	       PIC X(01).
001150 77  WS-CHAR2            PIC X(01).
001160 77  WS-COMP             PIC 9(01).
001170 77  WS-TYPE             PIC X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440	       05  WS-TCHR PIC X(80) VALUE "фммммммммммммммямммммммммммм
      -	       "ммммммммммммяммммммммммммммммммммммяммммммямммммммм╣".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC X(80) VALUE "зддддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддд©".
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR PIC X(80) VALUE "Ё              Ё
      -        "            Ё                      Ё      Ё        Ё".
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "Ё".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC X(80) VALUE "фммммммммммммммомммммммммммм
      -	       "ммммммммммммоммммммммммммммммммммммоммммммомммммммм╣".
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC X(80) VALUE "юддддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддды".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "PRP".
	   03  WS-PROG	       PIC  X(03) VALUE "025".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "PRP025.".
               05  W02-USER   PIC X(05) VALUE SPACES.
               
001340 01  W05-PRINT.
001350     03  W05-ESC        PIC 9(02) COMP-X.
001360     03  W05-ESCR REDEFINES W05-ESC.
001370         05  W05-E      PIC X(01).
001380     03  W05-DBL        PIC 9(02) COMP-X.
001390     03  W05-DBLR REDEFINES W05-DBL.
001400         05  W05-D      PIC X(01).
001410     03  W05-CDBL       PIC 9(02) COMP-X.
001420     03  W05-CDBLR REDEFINES W05-CDBL.
001430         05  W05-CD     PIC X(01).
001440     03  W05-SI         PIC 9(02) COMP-X.
001450     03  W05-SIR REDEFINES W05-SI.
001460         05  W05-I      PIC X(01).
001470     03  W05-CSI        PIC 9(02) COMP-X.
001480     03  W05-CSIR REDEFINES W05-CSI.
001490         05  W05-CI     PIC X(01).
001500     03  W05-SO         PIC 9(02) COMP-X.
001510     03  W05-SOR REDEFINES W05-SO.
001520         05  W05-O      PIC X(01).
001530     03  W05-CSO        PIC 9(02) COMP-X.
001540     03  W05-CSOR REDEFINES W05-CSO.
001550         05  W05-CO     PIC X(01).
001560     03  W05-6LPI       PIC 9(02) COMP-X.
001570     03  W05-6LPIR REDEFINES W05-6LPI.
001580         05  W05-6      PIC X(01).
001590     03  W05-8LPI       PIC 9(02) COMP-X.
001600     03  W05-8LPIR REDEFINES W05-8LPI.
001610         05  W05-8      PIC X(01).
001590     03  W05-NUL        PIC 9(02) COMP-X.
001600     03  W05-NULR REDEFINES W05-NUL.
001610         05  W05-N      PIC X(01).
001620     03  W05-CSHDRW     PIC 9(02).
001230 01  W06-COND.
001240     03  W06-DEC         PIC 9(03) OCCURS 4.
001250 01  W07-NORM.
001260     03  W07-DEC         PIC 9(03) OCCURS 4.
001270 01  W08-EXPD.
001280     03  W08-DEC         PIC 9(03) OCCURS 4.
001290 01  W09-ECAN.
001300     03  W09-DEC         PIC 9(03) OCCURS 4.
001310 01  W10-PGEL48.
001320     03  W10-DEC         PIC 9(03) OCCURS 4.
001330 01  W11-PGEL66.
001340     03  W11-DEC         PIC 9(03) OCCURS 4.
001330 01  W12-10CPI.
001340     03  W12-DEC         PIC 9(03) OCCURS 4.
001330 01  W13-CPI12.
001340     03  W13-DEC         PIC 9(03) OCCURS 4.
001330 01  W14-CPI17.
001340     03  W14-DEC         PIC 9(03) OCCURS 4.
001330 01  W15-DBLP.
001340     03  W15-DEC         PIC 9(03) OCCURS 4.
001330 01  W16-DCAN.
001340     03  W16-DEC         PIC 9(03) OCCURS 4.
001660 01  W10-INPUT.
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

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

       01  S37.
	   03  LINE  4 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9(02) USING PAR-INV AUTO.
	   03  LINE  6 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9(02) USING PAR-SLIP AUTO.
	   03  LINE  8 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9(02) USING PAR-STM AUTO.

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
003240	   IF LS0-PALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.
       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY APAC.HLP.

       AA200              SECTION.
       AA201.
002130	     DISPLAY "Ё" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "Ё" WITH BACKGROUND-COLOR 0
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
      *    *****    STM/INV/CASH SLIP FORMATS	 *****
      *
008640 YA000		       SECTION 5.
       YA360-FORMATS.
             DISPLAY CLR-SCREEN.
             DISPLAY "INVOICE/CASH SLIP/STATEMENT FORMATS" AT 0224
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "INVOICE      " AT 0406
		     "Format (" "1" WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " to " "22" WITH FOREGROUND-COLOR 3 HIGHLIGHT ")"
		     " or (" "99" WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " for USB/WINDOWS)".
	     DISPLAY "CASH SLIP    " AT 0606
		     "Format (" "1" WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     ") Format (" "2" WITH FOREGROUND-COLOR 3
					   HIGHLIGHT ")".
	     DISPLAY "STATEMENT    " AT 0806
		     "Format (" "1" WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " to " "9" WITH FOREGROUND-COLOR 3 HIGHLIGHT ")".
	   IF LS0-LEV < 4
	       DISPLAY "Valid Invoice formats for this level are:"
			AT 1006 WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       IF LS0-LEV = 3
		   DISPLAY "1, 5, 6, 7, 9, 12, 15 and 21" AT 1048
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
	       ELSE
		   DISPLAY "1, 5, 7 and 9" AT 1048
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR 3
				       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
014240       MOVE 3              TO WS-PARKEY.
014250       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       YA365.
014270	     ACCEPT S37.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO YA365
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA365
	       END-EVALUATE.

       YA370.
	     DISPLAY CLEAR-L25.
	   IF LS0-LEV < 4
	       IF LS0-LEV = 3
		   IF NOT (PAR-INV = 1 OR 5 OR 6 OR 7 OR
				     9 OR 12 OR 15 OR 21 OR 99)
		       MOVE "Invoice selection not valid"
				 TO WS-ERR-STRING
003260		       PERFORM ERROR-MESSAGE
		       GO TO YA365
		   END-IF
	       ELSE
	       IF NOT (PAR-INV = 1 OR 5 OR 7 OR 9 OR 99)
		   MOVE "Invoice selection not valid"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO YA365
	       END-IF
	   ELSE
	   IF (PAR-INV < 1 OR > 22) AND (PAR-INV NOT = 99)
	       MOVE "Invoice format MUST be 1 to 22 or 99"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA365.
           IF NOT (PAR-SLIP = 1 OR 2)
	       MOVE "Cash Slip format MUST be 1 or 2"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA365.
	   IF (PAR-STM < 1 OR > 9)
	       MOVE "Statement format MUST be 1 to 9"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA365.
012180	     PERFORM CHECK-CORRECT.
014350	   IF WS-OPTION = "N"
014360         GO TO YA365.
       YA375.
	   IF PAR-INV = 13
	       DISPLAY "INV Header Margin" AT 1006
	       ACCEPT PAR-HMRG AT 1024
		      WITH BACKGROUND-COLOR 4
			   FOREGROUND-COLOR 7 HIGHLIGHT
			   UPDATE AUTO
	       IF PAR-HMRG > 25
		   GO TO YA375.
014370       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
       YA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
015920 ZA000-INIT	       SECTION 8.
015930 ZA000-OPEN.
	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   S e t   u p	 t h e	 F U N C T I O N   k e y s     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       ESC, F1 to F10 keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    PAGE-UP and PAGE-DOWN keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   A C T I V A T E   M O U S E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	     S Y S T E M   F I L E		    ****
      *    *************************************************************
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
007570       MOVE 1 TO WS-SECKEY.
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
041240 ZA60.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2312
020130                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
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
