      ******************************************************************
      *                                                                *
      *    ******   **    **   ******   *******   ********  **    **   *
      *   **    **  **    **  **    **  **    **  **        ***   **   *
      *   **         **  **   **        **    **  **        ****  **   *
      *    ******     ****     ******   *******   *****     ** ** **   *
      *         **     **           **  **    **  **        **  ****   *
      *   **    **     **     **    **  **    **  **        **   ***   *
      *    ******      **      ******   **    **  ********  **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S  S Y S T E M  R E N A M E  ( E X T E N )   *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SYSREN.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2008
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

000120 COPY SYSTEM.SL.

000120 COPY PARAM.SL.

000140 DATA DIVISION.
000150 FILE SECTION.

       COPY APACFIDS.FD.

000170 COPY SYSTEM.FD.

000170 COPY PARAM.FDE.

      /
000190 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000240 77  WS-C1	   PIC 9(04)	 COMP-5.
000250 77  WS-C2           PIC 9(04)     COMP-5.
000260 77  WS-C3           PIC 9(04)     COMP-5.
000270 77  WS-C4           PIC 9(04)     COMP-5.
001880 77  WS-S1	   PIC	9(04)	 COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-DSK15	   PIC	X(01).
000280 77  WS-SECKEY       PIC 9(04)     COMP-5.
000280 77  WS-PARKEY       PIC 9(04)     COMP-5.
000290 77  WS-SERIAL       PIC 9(07)     VALUE ZERO.
000300 77  WS-NUM          PIC Z(04)9.
000310 77  WS-MES          PIC X(11) VALUE "INITIALIZED".
000330 77  WS-ERROR        PIC 9(01)     VALUE ZERO.
000340 77  WS-CHAR         PIC X(01).
000350 77  WS-STOCK        PIC X(01)     VALUE "Y".
000360 77  WS-DEBTOR       PIC X(01)     VALUE "Y".
000370 77  WS-CREDITOR     PIC X(01)     VALUE "Y".
000380 77  WS-GLEDGER      PIC X(01)     VALUE "Y".
000390 77  WS-HPDEBT       PIC X(01)     VALUE "Y".
000400 77  WS-POSALE       PIC X(01)     VALUE "Y".
000410 77  WS-VEHCLE       PIC X(01)     VALUE "Y".
000420 77  WS-OPTION       PIC X(01).
002420 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).

000980 01  W00-ALPHA-SER.
000990     03  W00-SERIAL      PIC X(07).

       COPY WS.WS.

       COPY CHAIN.WS.

000430 01  W00-FILES.
000440     03  W00-FREC            OCCURS 3.
000450         05  W00-FID         OCCURS 5.
000460             07  W00-DISC    PIC X(01).
000470             07  W00-FNME    PIC X(08).
000480             07  W00-SUFF    PIC X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY SYSTEM.ID.

000490 01  W05-PRINT.
000500     03  W05-CONTRACT.
000510         05  W05-CHAR1       PIC X(01) OCCURS 4.
000520     03  W05-NORMAL.
000530         05  W05-CHAR2       PIC X(01) OCCURS 4.
000570 01  W15-HEX.
000580     03  W15-ASCII           PIC X(01) OCCURS 256.
000590 01  W20-COND.
000600     03  W20-DEC             PIC 9(03) OCCURS 4.
000610 01  W25-NORM.
000620     03  W25-DEC             PIC 9(03) OCCURS 4.
000630 01  W30-ASCII.
000640     03  W30-GRP.
000650         05  W30-DEC         PIC 9(05) COMP-0.
000660 01  W30-GRP1           REDEFINES  W30-ASCII.
000670         05  W30-1           PIC X(01).
000680         05  W30-CHAR        PIC X(01).

       COPY W40.WS.

       COPY FUNCTION.WS.

001940 SCREEN SECTION.

      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLR-L1-2.
003860	   05  LINE  1 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004170                           VALUE "APAC Accounting".
	   05	       COLUMN 17 PIC  X(50) FROM WS-BACKGROUND.
004180	   05	       COLUMN 66 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004190				 VALUE "Version - 6.6".
003920	   05	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003930	   03  LINE  3 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003940	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003950	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003960	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003970	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003980	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003990	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004000	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004010	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004020	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004030	   03  LINE  8 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004040	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004050	   03  LINE  9 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004060	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004070	   03  LINE 10 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004080	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004090	   03  LINE 11 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004100	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004110	   03  LINE 12 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004120	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004130	   03  LINE 13 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004140	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004150	   03  LINE 14 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004160	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004170	   03  LINE 15 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004180	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004190	   03  LINE 16 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004200	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004210	   03  LINE 17 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004220	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004230	   03  LINE 18 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004240	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004250	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004260	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004270	   03  LINE 20 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004280	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004290	   03  LINE 21 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004300	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004310	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004320	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004330	   03  LINE 23 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004340	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
006550	   03  LINE 24 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
	   03  CLEAR-L25.
	   05  LINE 25 BLANK LINE BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 1.

004870 01  S99.
004880	   03  LINE 25 COLUMN 29 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
000830				 VALUE "Correct ".
	   03	       COLUMN 37 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
004890				 VALUE "ENTER".
	   03	       COLUMN 42 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " - ".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 46 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " if incorrect".

       COPY ERROR.CRT.

      /
000690 PROCEDURE DIVISION.
000700 AA000-MAIN              SECTION.
000710 AA00.
000720       DISPLAY CLR-SCREEN.
000730	     DISPLAY "AMEND COMPANY IDENTIFICATION CODE" AT 0321
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
003340	     DISPLAY "COMPANY identity code ?°"	AT 0512
003350		      "   " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5.
	     DISPLAY "***" AT 0541 WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     " will EXIT".
       AA05.
003380	     ACCEPT WS-SYS-ID AT 0536
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF WS-SYS-ID = "***"
	       DISPLAY "Program terminated by Operator" AT 2512
		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
			    BACKGROUND-COLOR 3
               STOP RUN.
	     MOVE 3		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-SYS-ID
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     OPEN I-O APACFIDS.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       AA05A.
	     MOVE "PARAM"	 TO AFID-KEY.

       AA05-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA05-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA05-READ-APACFIDS-EXIT.
	     EXIT.

       AA05A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "SYSTEM"	 TO AFID-KEY.
	     PERFORM AA05-READ-APACFIDS THRU AA05-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSTEM.

       AA10.
012180	     DISPLAY S99.
012190	     ACCEPT WS-OPTION AT 2560
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018520	   IF WS-OPTION = "N"
	       GO TO AA05.
	     PERFORM CLEAR-L50.
001060       OPEN I-O SECUR.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210	       DISPLAY "Files locked - try later - Press ENTER" AT 2512
006220			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
006230	       ACCEPT WS-OPTION AT 2551
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
006240         STOP RUN.
006250     IF WS-STATUS = "30"
006260	       DISPLAY "Invalid or file not on disk" AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270         STOP RUN.
001060       OPEN I-O PARAM.
006200     IF RUNTIME-ERROR
               IF FLE-LOCKED
006210	       DISPLAY "Files locked - try later - Press ENTER" AT 2512
006220			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
006230	       ACCEPT WS-OPTION AT 2551
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
006240         STOP RUN.
006250     IF WS-STATUS = "30"
006260	       DISPLAY "Invalid or file not on disk" AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
006270	       STOP RUN.
006280	     MOVE 2		 TO WS-SECKEY.
000860 AA15.
006300       READ SECUR.
006310     IF WS-STATUS = "00"
006320         GO TO AA18.
006330     IF RUNTIME-ERROR
006340         IF RECORD-LOCKED
006350             GO TO AA15.
006360	     DISPLAY "Disk error - AA15" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
006370       STOP RUN.
       AA18.
             MOVE 3              TO WS-PARKEY.
       AA19.
             READ PARAM.
006310     IF WS-STATUS = "00"
006320         GO TO AA25.
006330     IF RUNTIME-ERROR
006340         IF RECORD-LOCKED
006350             GO TO AA19.
006360	     DISPLAY "Disk error - AA15" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3.
006370       STOP RUN.
       AA25.
	     MOVE WS-SYS-ID	 TO SEC-EXT PAR-EXT.
001490	     REWRITE SEC-REC1.
             REWRITE PAR-RECORD1.
001500       CLOSE SECUR PARAM.
001550	     DISPLAY "Extension and Disk drive amended" AT 1212
                      WITH FOREGROUND-COLOR 14.
	     DISPLAY "Ù" AT 2480 WITH BACKGROUND-COLOR 0.
001560       STOP RUN.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.
