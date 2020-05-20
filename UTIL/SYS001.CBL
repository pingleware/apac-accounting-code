      ******************************************************************
      *                                                                *
      *     ******     ********     ******     **    **    *******     *
      *    **    **    **          **    **    **    **    **    **    *
      *    **          **          **          **    **    **    **    *
      *     ******     *****       **          **    **    ******      *
      *          **    **          **          **    **    **   **     *
      *    **    **    **          **    **    **    **    **    **    *
      *     ******     ********     ******      ******     **    **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     L E G A L I S E   P R O G R A M   M E S S A G E            *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SYS001.
000030 AUTHOR.         J W LEMMON  (APAC).
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
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000140 DATA DIVISION.
000150 FILE SECTION.

      /
000190 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000220 77  WS-OPTION	       PIC  X(01).
000280 77  WS-NUM              PIC  Z9.
000280 77  WS-S                PIC  9(02)  COMP-5.
000280 77  WS-S1               PIC  9(02)  COMP-5.
       77  APAC-VERSION        PIC  X(04) VALUE "2001".
       77  APAC-SYSTEM	       PIC  X(12) VALUE "-Sys Utility".


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
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "ฐ".
	       05  FILLER      PIC  X(01) VALUE "ณ".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000300 COPY WS.WS.

000300 COPY CHAIN.WS.

      /
000750 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  4 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "ษ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE
	   "ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "ป".
	   03  LINE  5 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
           "It is illegal to use this software without paying the reg-".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  6 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "istration and  licence fees.  The software is  distributed".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  7 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "internationally by JWL Software CC and nationally by Work-".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  8 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "group Data Solutions and  APAC Distribution. The  software".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  9 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "is the Copyright (C)  of JW Lemmon. If the sofware is used".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 10 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "without the above fees having been paid to one of the dis-".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 11 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "tributors, you are liable for prosecution. If however, you".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 12 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "contact a Distributor and register the package, there will".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 13 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "be no legal action  taken against you. The  system may  be".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 14 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "used for evaluation for a period not exceeding sixty  five".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 15 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "days,  after which it must be registered  or deleted  from".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 16 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "the computer  on which it has been installed. To  register".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 17 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "the system contact the distributor indicated on the regis-".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 18 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "tration card, and the necessary  arrangements will be made".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 19 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "to make your copy of the system legal.                    ".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 20 COLUMN 11 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "ศ".
	   03	       COLUMN 12 BACKGROUND-COLOR 4
                                 FOREGROUND-COLOR 15 VALUE
	   "ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ".
	   03	       COLUMN 70 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 15 VALUE "ผ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 21 COLUMN 13 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT VALUE
	 "ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ".

       01  S02.
	   03  LINE  4 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "ษ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE
	   "ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "ป".
	   03  LINE  5 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "This is a DEMONSTRATION system supplied by JWL Software   ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  6 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "for your convenience and MUST NOT be used as a live system".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  7 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "as there are restrictions which apply to the demonstration".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  8 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "system. This system will also clear values and quantities ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE  9 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "at different random intervals. This has become necessary  ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 10 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "to stop piracy of our software. It has come to our atten- ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 11 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "tion that there are those amongst us who believe that they".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 12 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "are entitled to use the software without paying for it. We".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 13 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "provided the demonstration system as a fully functional   ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 14 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "system to enable our USERS to try a procedure before hav- ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 15 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "ing to process on the live system. This is still possible ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 16 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "with the current demonstration system, but without the    ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 17 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "guarantee that the data will still be available when call-".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 18 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "ing the module the next time. If this is an inconvenience,".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 19 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "บ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 VALUE
	   "we apologise. Thank the unscrupulous!!! for this.         ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "บ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 20 COLUMN 11 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 15 VALUE "ศ".
	   03	       COLUMN 12 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
	   "ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ".
	   03	       COLUMN 70 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "ผ".
	   03	       COLUMN 71 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT
				 VALUE "ฐฐ".
	   03  LINE 21 COLUMN 13 BACKGROUND-COLOR 0
				 FOREGROUND-COLOR 0 HIGHLIGHT VALUE
	 "ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ".

      /
002520 PROCEDURE DIVISION CHAINING WS-PARID WS-USER-ID WS0-PROGRAMS.
002530 AA000-MAIN      SECTION.
       AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 AA02.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO AA02.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
             MOVE WS-COMPANY     TO WS-TOP-COMP.
	   IF WS-USER = WS-SYS-ID
	       MOVE "Supervisorฤ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE WS-USER	   TO WS-WRKST.
	     DISPLAY CLR-SCREEN.
	   IF WS-DEM > 1
	       DISPLAY S02
	   ELSE
	       DISPLAY S01.
             DISPLAY "Press any key to continue" AT 2502.
             ACCEPT WS-OPTION AT 2528 WITH AUTO.
       AA05.
             EXIT PROGRAM.
