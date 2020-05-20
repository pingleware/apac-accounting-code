      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   **	**   ******   *******	  ****	    ****       **      *
      *   **	**  **	  **  **    **	 **  **    **  **     ***      *
      *   **	**  **	      **   **	**    **  **	**     **      *
      *   **	**   ******   *****	**    **  **	**     **      *
      *   **	**	  **  **  **	**    **  **	**     **      *
      *   **	**  **	  **  **   **	 **  **    **  **      **      *
      *    ******    ******   **    **	  ****	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     U S E R   L O G I N   A N D   L O G O U T	P R O G R A M  *
      *                                                                *
      *     VERSION 8.15.00 - January 2011			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     USR001.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   FEBRUARY 1994.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2008
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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY SYSUSE.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000310 COPY SYSUSE.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000610 77  WS-S1	   PIC	9(04)	 COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-SPC	   PIC	X(40) VALUE SPACES.
       77  WS-OPTION	   PIC	X(01).

      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
001380	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
001400	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
001410         05  FILLER      PIC  X(01).
001420         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
001460	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000670 COPY WS.WS.

000670 COPY CHAIN.WS.

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY SYSUSE.ID.

       01  W10-USER-FIELDS.
	   03  W10-USER        PIC  9(03) VALUE ZERO.
	   03  W10-PASS        PIC  X(08) VALUE SPACES.
	   03  W10-ENCRYPT REDEFINES W10-PASS.
	       05  W10-ENC1    PIC  X(01) COMP-X.
	       05  W10-ENC2    PIC  X(01) COMP-X.
	       05  W10-ENC3    PIC  X(01) COMP-X.
	       05  W10-ENC4    PIC  X(01) COMP-X.
	       05  W10-ENC5    PIC  X(01) COMP-X.
	       05  W10-ENC6    PIC  X(01) COMP-X.
	       05  W10-ENC7    PIC  X(01) COMP-X.
	       05  W10-ENC8    PIC  X(01) COMP-X.
	   03  W10-NAME.
	       05  W10-N1-2    PIC  X(02).
	       05  W10-N3-4    PIC  X(02).
	       05  W10-N5-6    PIC  X(02).
	       05  W10-N7-8    PIC  X(02).
	       05  W10-N9-10   PIC  X(02).
	       05  W10-N11-12  PIC  X(02).
	       05  W10-N13-14  PIC  X(02).
	       05  W10-N15-16  PIC  X(02).
	       05  W10-N17-18  PIC  X(02).
	       05  W10-N19-20  PIC  X(02).
	       05  W10-N21-22  PIC  X(02).
	       05  W10-N23-24  PIC  X(02).
	       05  W10-N25-26  PIC  X(02).
	       05  W10-N27-28  PIC  X(02).
	       05  W10-N29-30  PIC  X(02).

       01 W15-SECURITY.
	   03  W15-NO	       PIC  9(03) VALUE ZERO.
	   03  W15-STATION     PIC  X(03) VALUE SPACES.
	   03  W15-NAME        PIC  X(30) VALUE SPACES.
	   03  W15-ENCRYPT REDEFINES W15-NAME.
	       05  W15-N1-2    PIC  X(02).
	       05  W15-N3-4    PIC  X(02).
	       05  W15-N5-6    PIC  X(02).
	       05  W15-N7-8    PIC  X(02).
	       05  W15-N9-10   PIC  X(02).
	       05  W15-N11-12  PIC  X(02).
	       05  W15-N13-14  PIC  X(02).
	       05  W15-N15-16  PIC  X(02).
	       05  W15-N17-18  PIC  X(02).
	       05  W15-N19-20  PIC  X(02).
	       05  W15-N21-22  PIC  X(02).
	       05  W15-N23-24  PIC  X(02).
	       05  W15-N25-26  PIC  X(02).
	       05  W15-N27-28  PIC  X(02).
	       05  W15-N29-30  PIC  X(02).
	   03  W15-PASS        PIC  X(08) VALUE SPACES.
	   03  W15-DATE        PIC  9(06) VALUE ZERO.
	   03  W15-GL	       PIC  X(01) VALUE "N".
	   03  W15-GLLEV       PIC  9(01) VALUE ZERO.
	   03  W15-STOCK       PIC  X(01) VALUE "N".
	   03  W15-STLEV       PIC  9(01) VALUE ZERO.
	   03  W15-DEBT        PIC  X(01) VALUE "N".
	   03  W15-DBLEV       PIC  9(01) VALUE ZERO.
	   03  W15-CRED        PIC  X(01) VALUE "N".
	   03  W15-CRLEV       PIC  9(01) VALUE ZERO.
	   03  W15-PARM        PIC  X(01) VALUE "N".
	   03  W15-PALEV       PIC  9(01) VALUE ZERO.
	   03  W15-SALES       PIC  X(01) VALUE "Y".
	   03  W15-SALEV       PIC  9(01) VALUE ZERO.
	   03  W15-JOBS        PIC  X(01) VALUE "N".
	   03  W15-JBLEV       PIC  9(01) VALUE ZERO.
	   03  W15-WAGES       PIC  X(01) VALUE "N".
	   03  W15-WGLEV       PIC  9(01) VALUE ZERO.
	   03  W15-ADM	       PIC  X(01) VALUE SPACES.
	   03  W15-DIV	       PIC  9(03) VALUE ZERO.
	   03  W15-SP	       PIC  9(02) VALUE ZERO.
	   03  W15-ORD	       PIC  9(06) COMP-5.
	   03  FILLER	       PIC  X(03).

       01  W20-TEMP.
	   03  W20-PASS        PIC  X(08).

       01  BLOCK-DETAIL1.
	   03  ORIGINAL-VID1.
	       05  ORIGINAL-CHAR1 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID1.
	       05  REVERSE-CHAR1  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL1.
	   03  SHAD1-ROW       PIC  9(02) COMP-X.
	   03  SHAD1-COL       PIC  9(02) COMP-X.
	   03  SHAD1-LINES     PIC  9(02) COMP-X.
	   03  SHAD1-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD1-CHAR      PIC  X(01).

       01  CRT-DETAIL1.
	   03  TOP-ROW1       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW1     PIC  9(02) COMP-X.
	   03  STRING-LENGTH1  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION1.
	       05  SCREEN-LIN1 PIC  9(02) COMP-X.
	       05  SCREEN-COL1 PIC  9(02) COMP-X.

       01  BLOCK-DETAIL2.
	   03  ORIGINAL-VID2.
	       05  ORIGINAL-CHAR2 PIC  X(01) OCCURS 80.
	   03  REVERSE-VID2.
	       05  REVERSE-CHAR2  PIC  X(01) OCCURS 80.

       01  SHADOW-DETAIL2.
	   03  SHAD2-ROW       PIC  9(02) COMP-X.
	   03  SHAD2-COL       PIC  9(02) COMP-X.
	   03  SHAD2-LINES     PIC  9(02) COMP-X.
	   03  SHAD2-WIDTH     PIC  9(02) COMP-X.
	   03  SHAD2-CHAR      PIC  X(01).

       01  CRT-DETAIL2.
	   03  TOP-ROW2       PIC  9(02) COMP-X.
	   03  BOTTOM-ROW2     PIC  9(02) COMP-X.
	   03  STRING-LENGTH2  PIC  9(04) COMP-X.
	   03  SCREEN-POSITION2.
	       05  SCREEN-LIN2 PIC  9(02) COMP-X.
	       05  SCREEN-COL2 PIC  9(02) COMP-X.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
001070 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

       01  S01.
001450	   03  LINE  2 COLUMN 28 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
001460				 VALUE "USER SECURITY CONTROL MENU".

       01  S02.
	   03  LINE  4 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 28 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "USER CONTROL".
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  6 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Login            ³".
001470	   03  LINE  7 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Logout           ³".
001470	   03  LINE  8 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Change Password  ³".
001470	   03  LINE  9 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Add User         ³".
001570	   03  LINE 10 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Amend User Rec.  ³".
001570	   03  LINE 11 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Delete User      ³".
001570	   03  LINE 12 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN  9 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 10 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit	          ³".
	   03  LINE 13 COLUMN  7 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN  8 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S10.
	   03  LINE  2 COLUMN 38 BACKGROUND-COLOR 1
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "LOG-IN".
	   03  LINE  4 COLUMN 12 VALUE "User Number".
	   03	       COLUMN 24 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "   ".
	   03  LINE  6 COLUMN 12 VALUE "Password   ".
	   03	       COLUMN 24 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "        ".
	   03  LINE 11 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT VALUE
	   "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 69 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE 12 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " The  USER NUMBER	must be a valid user number  in  the ³".
	   03  LINE 13 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " range  001 to 999.  The PASSWORD must be the  password ³".
	   03  LINE 14 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " that has been assigned to the user number selected.    ³".
	   03  LINE 15 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " D O   N O T   press the enter key after keying in  the ³".
	   03  LINE 16 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " user number. If the user number is less than 100,  key ³".
	   03  LINE 17 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " in the number and press the TAB key. The system is set ³".
	   03  LINE 18 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " up with USER NUMBER 001 and the password will be setup ³".
	   03  LINE 19 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " as PASSWORD and this may be changed by you.	     ³".
	   03  LINE 20 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    " SEE option 'L' on the program control menu.	     ³".
	   03  LINE 21 COLUMN 12 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 13 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0 VALUE
	    "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S10-NO.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9 USING W10-USER AUTO.

       01  S10-PASS.
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC	X(08)
				 USING W10-PASS AUTO SECURE.

       01  S11.
	   03  LINE  2 COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "CREATE USER RECORD".

       01  S11A.
	   03  LINE  2 COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "AMEND USER RECORD".

       01  S12.
	   03  LINE  2 COLUMN 32 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "DELETE USER RECORD".

       01  S13.
	   03  LINE  4 COLUMN  4 VALUE "User Number".
	   03	       COLUMN 16 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(03) FROM WS-SPC.
	   03  LINE  5 COLUMN  4 VALUE "User Name  ".
	   03	       COLUMN 16 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(30) FROM WS-SPC.
	   03  LINE  6 COLUMN  4 VALUE "Password   ".
	   03	       COLUMN 16 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(08) FROM WS-SPC.
	   03	       COLUMN 30 VALUE "Salesperson No.".
	   03	       COLUMN 46 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(02) FROM WS-SPC.
	   03  LINE  7 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Module   Security Level".
	   03  LINE  8 COLUMN  4 VALUE "Sales         ".
	   03  LINE  9 COLUMN  4 VALUE "Debtors       ".
	   03  LINE 10 COLUMN  4 VALUE "Creditors     ".
	   03  LINE 11 COLUMN  4 VALUE "Stock         ".
	   03  LINE 12 COLUMN  4 VALUE "Job Costing   ".
	   03  LINE 13 COLUMN  4 VALUE "General Ledger".
	   03  LINE 14 COLUMN  4 VALUE "Parameters    ".
	   03  LINE 15 COLUMN  4 VALUE "Wages         ".
	   03  LINE 17 COLUMN  4 HIGHLIGHT VALUE
	       "Module must be a Y or N! (Any key other than Y will be c
      -        "hanged to N)    ".
	   03  LINE 18 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Security level 0 through 9. 0=Enquiries, 1=Create, 2=Ame
      -        "nd, 3=Limited   ".
	   03  LINE 19 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Transactions and Reports, 4=All Reports, Transactions, D
      -	       "elete etc       ".
	   03  LINE 20 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "5-8=Reserved, 9=Supervisor (Any level includes lower lev
      -	       "els)            ".
	   03  LINE 21 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
	       "eg. Level 2 includes levels 0 and 1
      -        "                ".
	   03  LINE 22 COLUMN  4 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
	       "NB level 1 includes sales. Level 4 includes period ends
      -	       "etc             ".

       01  S14.
	   02  S14A.
	   03  LINE  4 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC ZZ9 USING W15-NO AUTO.
	   02  S14B.
	   03  LINE  5 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(30) USING W15-NAME AUTO.
	   03  LINE  6 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(08) USING W15-PASS AUTO.
	   03	       COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(02) USING W15-SP AUTO.
	   03  LINE  8 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-SALES AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-SALEV AUTO.
	   03  LINE  9 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-DEBT AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-DBLEV AUTO.
	   03  LINE 10 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-CRED AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-CRLEV AUTO.
	   03  LINE 11 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-STOCK AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-STLEV AUTO.
	   03  LINE 12 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-JOBS AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-JBLEV AUTO.
	   03  LINE 13 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-GL AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-GLLEV AUTO.
	   03  LINE 14 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-PARM AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-PALEV AUTO.
	   03  LINE 15 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(01) USING W15-WAGES AUTO.
	   03	       COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 9(01) USING W15-WGLEV AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001490 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001500 AA000-MAIN              SECTION.
001510 AA00.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-USER-ID	 TO WS-USER-ID.
	     MOVE LS0-SECURITY	 TO WS0-SECURITY.
	     PERFORM ZA000-INIT.
	   IF WS0-NAME = "ANYBODY"
	       PERFORM CA000
	       GO TO AA90.
	     DISPLAY CLR-L1-2.
	     DISPLAY S01.
	     PERFORM BA000.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
       AA90.
	     MOVE WS0-SECURITY	 TO LS0-SECURITY.
	     CLOSE SYSUSER.

013140 AA99.
013150       EXIT PROGRAM.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY SYSUSE.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY SYSUSE.WR.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

011433 COPY CLEAR.CRT.

       COPY LOCKED.REC.

       BA000	       SECTION.
       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 9		 TO SHADE-COL.
	     MOVE 20		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S02.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 9		 TO SCREEN-COL.
	     MOVE 18		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF (WS-OPTION < 0 OR > 6)
		   CALL X"E5"
		   GO TO BA05
	       END-IF
	       GO TO BA16
	   ELSE
	       CALL X"E5"
	       GO TO BA05.

       BA15.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO BA05
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA18.

       BA16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA18.
	   IF WS-OPTION = "0"
	       GO TO BA999.
	     PERFORM SAVE-SCREEN.
	     MOVE BLOCK-DETAIL	 TO BLOCK-DETAIL1.
	     MOVE SHADOW-DETAIL  TO SHADOW-DETAIL1.
	     MOVE CRT-DETAIL	 TO CRT-DETAIL1.
	   IF WS-OPTION = "1"
	       PERFORM CA000
	       GO TO BA999
	   ELSE
	   IF WS-OPTION = "2"
	       PERFORM DA000
	       GO TO BA999
	   ELSE
	   IF WS-OPTION = "3"
	       PERFORM EA000
	   ELSE
	   IF WS-OPTION = "4"
	       PERFORM FA000
	   ELSE
	   IF WS-OPTION = "5"
	       PERFORM GA000
	   ELSE
	   IF WS-OPTION = "6"
	       PERFORM HA000.
	     PERFORM RESTORE-SCREEN.
	     MOVE BLOCK-DETAIL1  TO BLOCK-DETAIL.
	     MOVE SHADOW-DETAIL1 TO SHADOW-DETAIL.
	     MOVE CRT-DETAIL1	 TO CRT-DETAIL.
	     GO TO BA05.

       BA999.
	     EXIT.

       BB000	      SECTION.
       BB00.
	     MOVE W10-N1-2	 TO W15-N29-30.
	     MOVE W10-N3-4	 TO W15-N1-2.
	     MOVE W10-N5-6	 TO W15-N27-28
	     MOVE W10-N7-8	 TO W15-N3-4
	     MOVE W10-N9-10	 TO W15-N25-26.
	     MOVE W10-N11-12	 TO W15-N5-6.
	     MOVE W10-N13-14	 TO W15-N23-24.
	     MOVE W10-N15-16	 TO W15-N7-8.
	     MOVE W10-N17-18	 TO W15-N21-22.
	     MOVE W10-N19-20	 TO W15-N9-10.
	     MOVE W10-N21-22	 TO W15-N19-20.
	     MOVE W10-N23-24	 TO W15-N11-12.
	     MOVE W10-N25-26	 TO W15-N17-18.
	     MOVE W10-N27-28	 TO W15-N13-14.
	     MOVE W10-N29-30	 TO W15-N15-16.

       BB05.
	     EXIT.

       BB010	       SECTION.
       BB10.
	     MOVE W15-N29-30	 TO W10-N1-2.
	     MOVE W15-N1-2	 TO W10-N3-4.
	     MOVE W15-N27-28	 TO W10-N5-6.
	     MOVE W15-N3-4	 TO W10-N7-8.
	     MOVE W15-N25-26	 TO W10-N9-10.
	     MOVE W15-N5-6	 TO W10-N11-12.
	     MOVE W15-N23-24	 TO W10-N13-14.
	     MOVE W15-N7-8	 TO W10-N15-16.
	     MOVE W15-N21-22	 TO W10-N17-18.
	     MOVE W15-N9-10	 TO W10-N19-20.
	     MOVE W15-N19-20	 TO W10-N21-22.
	     MOVE W15-N11-12	 TO W10-N23-24.
	     MOVE W15-N17-18	 TO W10-N25-26.
	     MOVE W15-N13-14	 TO W10-N27-28.
	     MOVE W15-N15-16	 TO W10-N29-30.

       BB15.
	     EXIT.

       BB020	       SECTION.
       BB20.
	     ADD 31		 TO W10-ENC1.
	     ADD 13		 TO W10-ENC2.
	     ADD 32		 TO W10-ENC3.
	     ADD 23		 TO W10-ENC4.
	     ADD 34		 TO W10-ENC5.
	     ADD 43		 TO W10-ENC6.
	     ADD 35		 TO W10-ENC7.
	     ADD 53		 TO W10-ENC8.

       BB25.
	     EXIT.

      *
      *    ****    L O G I N
      *
       CA000	       SECTION.
       CA00.
018150       DISPLAY CLR-SCREEN.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 14		 TO SHADE-COL.
	     MOVE 56		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
	     DISPLAY S10.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO W10-USER.
	     MOVE SPACES	 TO W10-PASS.

       CA05.
	     ACCEPT S10-NO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-USER
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CA05.
	   IF W10-USER = ZERO
	       GO TO CA999.
	     MOVE W10-USER	 TO USE-NO.
	     PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
	     DISPLAY CLEAR-L25.
	   IF WS-F-ERROR = 64
	       MOVE "Invalid USER number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA05.
	     MOVE USE-NAME	 TO W15-NAME.
	     PERFORM BB010.
	     DISPLAY W10-NAME AT 0430
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

       CA10.
	     ACCEPT S10-PASS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-USER
		     GO TO CA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CA10.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE W10-PASS	 TO W20-PASS.
	     PERFORM BB020.
	   IF (W10-PASS NOT = USE-PASS) AND
	      (W20-PASS NOT = "<,>.?/:;")
	       MOVE "Invalid Password"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO CA999.
	     MOVE USE-RECORD	 TO WS0-SECURITY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ WS0-DIV Will be used to pass the G/L division between the ³
      *    ³ programs. Used by consolidated G/L Systems.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO WS0-DIV.
	     MOVE WS-USER	 TO USE-STATION.
	     PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.

       CA999.
	     EXIT.
      *
      *    ****    L O G O U T
      *
       DA000	       SECTION.
       DA00.
	   IF WS0-NO = ZERO
	       GO TO DA999.
	     MOVE WS0-NO	 TO USE-NO.
	     PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
	     MOVE SPACES	 TO USE-STATION.
	     PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.
	     MOVE "ANYBODY"	 TO WS0-NAME.
	     MOVE SPACES	 TO WS0-PASS.
	     MOVE "N"		 TO WS0-GL    WS0-STOCK WS0-DEBT
				    WS0-CRED  WS0-PARM	WS0-JOBS
				    WS0-WAGES.
	     MOVE "Y"		 TO WS0-SALES.
	     MOVE ZERO		 TO WS0-GLLEV WS0-STLEV WS0-DBLEV
				    WS0-CRLEV WS0-PALEV WS0-SALEV
				    WS0-JBLEV WS0-WGLEV.

       DA999.
	     EXIT.
      *
      *    ****    C H A N G E	 P A S S W O R D
      *
       EA000	       SECTION.
       EA00.
	   IF WS0-NO = ZERO
	       GO TO EA999.
018150       DISPLAY CLR-SCREEN.
	     DISPLAY "CHANGE PASSWORD" AT 0233
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Are you sure (" AT 0412
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     ")".
	     MOVE SPACE 	 TO WS-OPTION.

       EA05.
	     ACCEPT WS-OPTION AT 0432
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO EA05.
	   IF WS-OPTION = "N"
	       GO TO EA999.
	     MOVE WS0-NO	 TO USE-NO.
	     PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
	     DISPLAY "Password    " AT 0612.
	     MOVE SPACES	 TO W10-PASS.

       EA10.
	     ACCEPT W10-PASS AT 0625
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO SECURE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     UNLOCK SYSUSER
		     GO TO EA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EA10.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM BB020.
	   IF (W10-PASS NOT = USE-PASS) AND
	      (W20-PASS NOT = "<,>.?/:;")
	       MOVE "Invalid Password"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       UNLOCK SYSUSER
	       GO TO EA999.
	     DISPLAY "New Password" AT 0812.
	     MOVE SPACES	 TO W10-PASS.

       EA15.
	     ACCEPT W10-PASS AT 0825
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO SECURE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     UNLOCK SYSUSER
		     GO TO EA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO EA10.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W10-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM BB020.
	     MOVE W10-PASS	 TO USE-PASS WS0-PASS.
	     PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.

       EA999.
	     EXIT.
      *
      *    ****    A D D   U S E R S
      *
       FA000	       SECTION.
       FA00.
	   IF WS0-NO NOT = 001
	       GO TO FA999.
	     INITIALIZE W15-SECURITY.
	     MOVE 1		 TO W15-ORD.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S11.
	     DISPLAY S13.

       FA05.
	     ACCEPT S14.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO FA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO FA05.
	     DISPLAY CLEAR-L25.
	     MOVE  W15-NO	 TO USE-NO.
	     PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = ZERO
	       MOVE "User number exists"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA05.
	  IF W15-NAME = SPACES OR W15-PASS = SPACES OR
	     W15-GL = SPACES OR W15-STOCK = SPACES OR
	     W15-DEBT = SPACES OR W15-CRED = SPACES OR
	     W15-PARM = SPACES OR W15-SALES = SPACES OR
	     W15-JOBS = SPACES OR W15-WAGES = SPACES
	       MOVE "Data incomplete"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO FA05.
	  IF W15-SP > 40
	      MOVE 40		 TO W15-SP.
	     MOVE 30		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-NAME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-GL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-GL = "N" OR "Y")
	       MOVE "N" 	 TO W15-GL.
	     CALL "CBL_TOUPPER" USING W15-STOCK
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-STOCK = "N" OR "Y")
	       MOVE "N" 	 TO W15-STOCK.
	     CALL "CBL_TOUPPER" USING W15-DEBT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-DEBT = "N" OR "Y")
	       MOVE "N" 	 TO W15-DEBT.
	     CALL "CBL_TOUPPER" USING W15-CRED
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-CRED = "N" OR "Y")
	       MOVE "N" 	 TO W15-CRED.
	     CALL "CBL_TOUPPER" USING W15-PARM
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-PARM = "N" OR "Y")
	       MOVE "N" 	 TO W15-PARM.
	     CALL "CBL_TOUPPER" USING W15-SALES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-SALES = "N" OR "Y")
	       MOVE "N" 	 TO W15-SALES.
	     CALL "CBL_TOUPPER" USING W15-JOBS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-JOBS = "N" OR "Y")
	       MOVE "N" 	 TO W15-JOBS.
	     CALL "CBL_TOUPPER" USING W15-WAGES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-WAGES = "N" OR "Y")
	       MOVE "N" 	 TO W15-WAGES.
	   IF W15-GLLEV > 4 AND < 9
	       MOVE 4		 TO W15-GLLEV.
	   IF W15-STLEV > 4 AND < 9
	       MOVE 4		 TO W15-STLEV.
	   IF W15-DBLEV > 5 AND < 9
	       MOVE 5		 TO W15-DBLEV.
	   IF W15-CRLEV > 4 AND < 9
	       MOVE 4		 TO W15-CRLEV.
	   IF W15-PALEV > 4 AND < 9
	       MOVE 4		 TO W15-PALEV.
	   IF W15-SALEV > 4 AND < 9
	       MOVE 4		 TO W15-SALEV.
	   IF W15-JBLEV > 4 AND < 9
	       MOVE 4		 TO W15-JBLEV.
	   IF W15-WGLEV > 4 AND < 9
	       MOVE 4		 TO W15-WGLEV.
	     MOVE W15-PASS	 TO W10-PASS.
	     PERFORM BB020.
	     MOVE W10-PASS	 TO W15-PASS.
	     MOVE W15-NAME	 TO W10-NAME.
	     PERFORM BB000.
	     MOVE W15-SECURITY	 TO USE-RECORD.
	     PERFORM WRITE-SYSUSER THRU WRITE-SYSUSER-EXIT.
	     MOVE SPACES	 TO W15-NAME W15-PASS.

       FA999.
	     EXIT.
      *
      *    ****    A M E N D   U S E R S
      *
       GA000	       SECTION.
       GA00.
	   IF WS0-NO NOT = 001
	       GO TO GA999.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S11A.
	     DISPLAY S13.

       GA02.
	     ACCEPT S14A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO GA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GA02.
	     DISPLAY CLEAR-L25.
	     MOVE  W15-NO	 TO USE-NO.
	     PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       MOVE "User number invalid"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA02.
	     MOVE USE-RECORD	 TO W15-SECURITY.
	     MOVE W15-PASS	 TO W10-PASS.
	     SUBTRACT 31	 FROM W10-ENC1.
	     SUBTRACT 13	 FROM W10-ENC2.
	     SUBTRACT 32	 FROM W10-ENC3.
	     SUBTRACT 23	 FROM W10-ENC4.
	     SUBTRACT 34	 FROM W10-ENC5.
	     SUBTRACT 43	 FROM W10-ENC6.
	     SUBTRACT 35	 FROM W10-ENC7.
	     SUBTRACT 53	 FROM W10-ENC8.
	     MOVE W10-PASS	 TO W15-PASS.
	     PERFORM BB010.
	     MOVE W10-NAME	 TO W15-NAME.

       GA05.
	     ACCEPT S14B.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     UNLOCK SYSUSER
		     GO TO GA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO GA05.
	     DISPLAY CLEAR-L25.
	   IF W15-NAME = SPACES OR W15-PASS = SPACES OR
	      W15-GL = SPACES OR W15-STOCK = SPACES OR
	      W15-DEBT = SPACES OR W15-CRED = SPACES OR
	      W15-PARM = SPACES OR W15-SALES = SPACES OR
	      W15-JOBS = SPACES OR W15-WAGES = SPACES
	       MOVE "Data incomplete"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO GA05.
	   IF W15-SP > 40
	       MOVE 40		 TO W15-SP.
	     MOVE 30		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-NAME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-GL
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-GL = "N" OR "Y")
	       MOVE "N" 	 TO W15-GL.
	     CALL "CBL_TOUPPER" USING W15-STOCK
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-STOCK = "N" OR "Y")
	       MOVE "N" 	 TO W15-STOCK.
	     CALL "CBL_TOUPPER" USING W15-DEBT
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-DEBT = "N" OR "Y")
	       MOVE "N" 	 TO W15-DEBT.
	     CALL "CBL_TOUPPER" USING W15-CRED
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-CRED = "N" OR "Y")
	       MOVE "N" 	 TO W15-CRED.
	     CALL "CBL_TOUPPER" USING W15-PARM
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-PARM = "N" OR "Y")
	       MOVE "N" 	 TO W15-PARM.
	     CALL "CBL_TOUPPER" USING W15-SALES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-SALES = "N" OR "Y")
	       MOVE "N" 	 TO W15-SALES.
	     CALL "CBL_TOUPPER" USING W15-JOBS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-JOBS = "N" OR "Y")
	       MOVE "N" 	 TO W15-JOBS.
	     CALL "CBL_TOUPPER" USING W15-WAGES
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W15-WAGES = "N" OR "Y")
	       MOVE "N" 	 TO W15-WAGES.
	   IF W15-GLLEV > 4 AND < 9
	       MOVE 4		 TO W15-GLLEV.
	   IF W15-STLEV > 4 AND < 9
	       MOVE 4		 TO W15-STLEV.
	   IF W15-DBLEV > 5 AND < 9
	       MOVE 5		 TO W15-DBLEV.
	   IF W15-CRLEV > 4 AND < 9
	       MOVE 4		 TO W15-CRLEV.
	   IF W15-PALEV > 4 AND < 9
	       MOVE 4		 TO W15-PALEV.
	   IF W15-SALEV > 4 AND < 9
	       MOVE 4		 TO W15-SALEV.
	   IF W15-JBLEV > 4 AND < 9
	       MOVE 4		 TO W15-JBLEV.
	   IF W15-WGLEV > 4 AND < 9
	       MOVE 4		 TO W15-WGLEV.
	     MOVE W15-PASS	 TO W10-PASS.
	     PERFORM BB020.
	     MOVE W10-PASS	 TO W15-PASS.
	     MOVE W15-NAME	 TO W10-NAME.
	     PERFORM BB000.
	     MOVE W15-SECURITY	 TO USE-RECORD.
	     PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.
	     MOVE SPACES	 TO W15-NAME W15-PASS.

       GA999.
	     EXIT.
      *
      *    ****    D E L E T E	 U S E R S
      *
       HA000	       SECTION.
       HA00.
	   IF WS0-NO NOT = 001
	       GO TO HA999.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S12.
	     DISPLAY S13.

       HA05.
	     ACCEPT W15-NO AT 0416
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO HA999
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO HA05.
	     MOVE  W15-NO	 TO USE-NO.
	     PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       MOVE "Invalid user number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO HA05.
	     DISPLAY CLEAR-L25.
	     MOVE USE-RECORD	 TO W15-SECURITY.
	     PERFORM BB010.
	     MOVE W10-NAME	 TO W15-NAME.
	     DISPLAY S14.

       HA10.
	     MOVE "Delete this user (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO HA10.
	   IF WS-OPTION = "N"
	       GO TO HA999.
	     PERFORM DELETE-SYSUSER-REC THRU WRITE-SYSUSER-EXIT.

       HA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION.
038150 ZA000-OPEN.
038160	     PERFORM ZA60.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	   ELSE
	       GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "SYSUSE"	 TO AFID-KEY.

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
	     MOVE AFID-PATH	 TO W02-SYSUSER.

       ZA05.
	     OPEN I-O SYSUSER.
	   IF WS-STATUS = "05"
	       CLOSE SYSUSER
	       OPEN OUTPUT SYSUSER
	       INITIALIZE USE-RECORD
	       MOVE 001		 TO USE-NO
	       MOVE "ADMINISTRATOR"
				 TO W10-NAME
	       PERFORM BB000
	       MOVE W15-NAME	 TO USE-NAME
	       MOVE "oNsjyzuy"	 TO USE-PASS
	       MOVE "Y"		 TO USE-GL    USE-STOCK USE-DEBT
				    USE-CRED  USE-PARM	USE-SALES
				    USE-JOBS  USE-WAGES
	       MOVE 9		 TO USE-GLLEV USE-STLEV USE-DBLEV
				    USE-CRLEV USE-PALEV USE-SALEV
				    USE-JBLEV USE-WGLEV
	       MOVE 1		 TO USE-ORD
006010	       WRITE USE-RECORD
	       CLOSE SYSUSER
	       GO TO ZA05.
039770       GO TO ZA999.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
040090 ZA60.
040650	     MOVE WS-COMPANY	 TO WS-TOP-COMP.
	   IF WS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

       ZA65.
	     EXIT.

040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION.
040790 OPEN-ERROR.
040800       DISPLAY CLR-SCREEN.
040810       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
040820       PERFORM DISPLAY-FILE-NAME.
040830
040840 READ-ERROR.
040850       DISPLAY CLR-SCREEN.
040860       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
040870       PERFORM DISPLAY-FILE-NAME.
040880
040890 WRITE-ERROR.
040900       DISPLAY CLR-SCREEN.
040910       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
040920       PERFORM DISPLAY-FILE-NAME.
040930

040950 DISPLAY-FILE-NAME.
041120	   IF WS-F-ERROR = 64
041130	       MOVE W02-SYSUSER	 TO WS-FILE
041140	       MOVE USE-NO	 TO WS-KEY.
041350	   IF WS-STATUS = "10"
041360         MOVE "End of FILE" TO WS-STAT-MESSAGE
041370     ELSE
041380     IF WS-STATUS = "22"
041390         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041400     ELSE
041410     IF WS-STATUS = "23"
041420         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041430     ELSE
041440     IF WS-STATUS = "24"
041450         MOVE "DISK full" TO WS-STAT-MESSAGE
041460     ELSE
041470     IF WS-STATUS = "30"
041480         MOVE "DISK error" TO WS-STAT-MESSAGE
041490     ELSE
041500     IF WS-STATUS = "94"
041510         MOVE "FILE locked" TO WS-STAT-MESSAGE.
041520       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
041530       DISPLAY "Status " AT 1212 
041540                WS-STATUS WITH FOREGROUND-COLOR 11
041550               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
041560     IF WS-STATUS NOT = "94" 
041570         DISPLAY "Key    " AT 1412 
041580                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
041590         DISPLAY "Reverse backup or contact program Support"
041600                  AT 1612.
041610         DISPLAY "Please make a note of these details" AT 1812.
041620       STOP RUN.
