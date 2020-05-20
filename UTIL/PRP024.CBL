      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******	  ****	   ******	**     *
      *   **	**  **	  **  **    **	 **  **   **	**     ***     *
      *   **	**  **	  **  **    **	**    **       **     * **     *
      *   *******   ******    *******	**    **     **      *	**     *
      *   **	    **	 **   **	**    **   **	    *******    *
      *   **	    **	  **  **	 **  **   **		**     *
      *   **	    **	  **  **	  ****	  ********     ****    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   C A S H	 S L I P   C O N T R O L S     *
      *                                                                *
      *     VERSION 8.15.00 - November 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PRP024.
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

000140 COPY PARAM.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

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
000950 77  WS-S2	   PIC	9(04)	 COMP-5.
000950 77  WS-S3	   PIC	9(04)	 COMP-5.
000950 77  WS-S4	   PIC	9(04)	 COMP-5.
001010 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
001010 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
001020 77  WS-SECKEY	   PIC	9(04)	 COMP-5.
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

       COPY CONTROL.ID.

       COPY PARAM.ID.

001340 01  W05-PRINT.
001350	   03  W05-ESC	      PIC  9(02) COMP-X.
001360     03  W05-ESCR REDEFINES W05-ESC.
001370	       05  W05-E      PIC  X(01).
001380	   03  W05-DBL	      PIC  9(02) COMP-X.
001390     03  W05-DBLR REDEFINES W05-DBL.
001400	       05  W05-D      PIC  X(01).
001410	   03  W05-CDBL       PIC  9(02) COMP-X.
001420     03  W05-CDBLR REDEFINES W05-CDBL.
001430	       05  W05-CD     PIC  X(01).
001440	   03  W05-SI	      PIC  9(02) COMP-X.
001450     03  W05-SIR REDEFINES W05-SI.
001460	       05  W05-I      PIC  X(01).
001470	   03  W05-CSI	      PIC  9(02) COMP-X.
001480     03  W05-CSIR REDEFINES W05-CSI.
001490	       05  W05-CI     PIC  X(01).
001500	   03  W05-SO	      PIC  9(02) COMP-X.
001510     03  W05-SOR REDEFINES W05-SO.
001520	       05  W05-O      PIC  X(01).
001530	   03  W05-CSO	      PIC  9(02) COMP-X.
001540     03  W05-CSOR REDEFINES W05-CSO.
001550	       05  W05-CO     PIC  X(01).
001560	   03  W05-6LPI       PIC  9(02) COMP-X.
001570     03  W05-6LPIR REDEFINES W05-6LPI.
001580	       05  W05-6      PIC  X(01).
001590	   03  W05-8LPI       PIC  9(02) COMP-X.
001600     03  W05-8LPIR REDEFINES W05-8LPI.
001610	       05  W05-8      PIC  X(01).
001590	   03  W05-NUL	      PIC  9(02) COMP-X.
001600     03  W05-NULR REDEFINES W05-NUL.
001610	       05  W05-N      PIC  X(01).
001620	   03  W05-CSHDRW     PIC  9(02).
	   03  W05-DRAWER     PIC  X(01).

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

004460 01  S19.
004470     03  LINE  3 COLUMN 28 FOREGROUND-COLOR 15
004480                           VALUE "CASH SLIP PRINTER CONTROLS".
004600	   03  LINE  4 COLUMN 12 VALUE "Cash Draw No    ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "  ".
004490	   03  LINE  6 COLUMN  2 FOREGROUND-COLOR 3 HIGHLIGHT
004500                           VALUE "Decimal values to be entered".
004510	   03  LINE  8 COLUMN 12 VALUE "Escape          ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004520	   03  LINE  9 COLUMN 12 VALUE "Double Print    ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004530	   03  LINE 10 COLUMN 12 VALUE "Cancel Double   ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004540	   03  LINE 11 COLUMN 12 VALUE "Shift In        ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004550	   03  LINE 12 COLUMN 12 VALUE "Cancel Shift In ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004560	   03  LINE 13 COLUMN 12 VALUE "Shift Out       ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004570	   03  LINE 14 COLUMN 12 VALUE "Cancel Shift Out".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004580	   03  LINE 15 COLUMN 12 VALUE "6 Lines per inch".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
004590	   03  LINE 16 COLUMN 12 VALUE "8 Lines per inch".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "   ".
	   03  LINE 17 COLUMN 12 VALUE "Lines to advance".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE "  ".
	   03  LINE 18 COLUMN 12 VALUE "Printer Number  ".
	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 31 VALUE "1 to 5".
	   03  LINE  8 COLUMN 41 VALUE "Draw type       ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "N".
	   03	       COLUMN 60 VALUE "o, ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "P".
	   03	       COLUMN 64 VALUE "ort or ".
	   03	       COLUMN 71 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "K".
	   03	       COLUMN 72 VALUE "ick".
	   03  LINE  9 COLUMN 41 VALUE "COM port number ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "1 to 4".
	   03  LINE 10 COLUMN 41 VALUE "Customer Dislay ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "Y or N".
	   03  LINE 11 COLUMN 41 VALUE "COM port number ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "1 to 4".
	   03  LINE 12 COLUMN 41 VALUE "Digital Scale   ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "Y or N".
	   03  LINE 13 COLUMN 41 VALUE "COM port number ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "1 to 4".
	   03  LINE 14 COLUMN 41 VALUE "Scale Type      ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 60 VALUE "very/".
	   03	       COLUMN 65 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 66 VALUE "S640/".
	   03	       COLUMN 71 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "O".
	   03	       COLUMN 72 VALUE "ther".
	   03  LINE 15 COLUMN 41 VALUE "Kitchen Printer ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "Y or N".
	   03  LINE 16 COLUMN 41 VALUE "Printer Number  ".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "1 to 5".
	   03  LINE 17 COLUMN 41 VALUE "Qnty: use 3 dec.".
	   03	       COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 VALUE " ".
	   03	       COLUMN 59 VALUE "Y or N".


004610 01  S20.
004620	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004630                           PIC ZZ9 USING W05-ESC AUTO.
004640	   03  LINE  9 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004650                           PIC ZZ9 USING W05-DBL AUTO.
004660	   03  LINE 10 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004670                           PIC ZZ9 USING W05-CDBL AUTO.
004680	   03  LINE 11 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004690                           PIC ZZ9 USING W05-SI AUTO.
004700	   03  LINE 12 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004710                           PIC ZZ9 USING W05-CSI AUTO.
004720	   03  LINE 13 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004730                           PIC ZZ9 USING W05-SO AUTO.
004740	   03  LINE 14 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004750                           PIC ZZ9 USING W05-CSO AUTO.
004760	   03  LINE 15 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004770                           PIC ZZ9 USING W05-6LPI AUTO.
004780	   03  LINE 16 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790                           PIC ZZ9 USING W05-8LPI AUTO.
004780	   03  LINE 17 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790                           PIC Z9 USING PAR-ADVANCE AUTO.
004780	   03  LINE 18 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790                           PIC 9 USING PAR-CPRN AUTO.
004780	   03  LINE  8 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790				 PIC X USING W05-DRAWER AUTO.
	   03  LINE  9 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
                                 PIC 9 USING PAR-PORT AUTO.
004780	   03  LINE 10 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790				 PIC X USING PAR-CUSDISP AUTO.
	   03  LINE 11 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9 USING PAR-CUSPORT AUTO.
004780	   03  LINE 12 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790				 PIC X USING PAR-SCALE AUTO.
	   03  LINE 13 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9 USING PAR-SCALPORT AUTO.
	   03  LINE 14 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC X USING PAR-SCALE-TYPE AUTO.
004780	   03  LINE 15 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
004790				 PIC X USING PAR-KTCHPRN AUTO.
	   03  LINE 16 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC 9 USING PAR-KCHPORT AUTO.
	   03  LINE 17 COLUMN 57 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
				 PIC X USING PAR-USE-3 AUTO.

005720 01  S34.
005730	   03  LINE  4 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4 PIC Z9
005740                           USING W05-CSHDRW AUTO.

       01  S35.
	   03  LINE  4 COLUMN 38 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 999 USING PAR-KC1 AUTO.
	   03	       COLUMN 42 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 999 USING PAR-KC2 AUTO.
	   03	       COLUMN 46 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 999 USING PAR-KC3 AUTO.
	   03	       COLUMN 50 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 999 USING PAR-KC4 AUTO.
	   03	       COLUMN 54 BACKGROUND-COLOR 4
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 999 USING PAR-KC5 AUTO.

       01  S-HELP.
	   03  LINE  4 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "        CASH DRAWER CONTROLS        ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  6 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "Drawer no: In the range 01 to 40    ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  7 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "See the printer reference MANUAL for".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  8 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "details of the control codes reques-".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  9 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "ed. Please note that the decimal    ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 10 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "value of the code must be keyed in. ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 11 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "Codes are usually specified using   ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "three formats:-                     ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 13 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "ASCII/GRAPHIC, HEXIDECIMAL & DECIMAL".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 14 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "EXAMPLE:                            ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 15 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       " ASCII/GRAPHIC - ESC SO  SI  DC2 :  ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 16 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       " HEXIDECIMAL   - 1B  0E  0F  12  3A ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 17 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       " DECIMAL       - 027 014 015 018 058".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 18 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "Lines to advance: Allow for tear off".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 19 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "Draw type: Most drawers are type F  ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 20 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "           Use P if no drawer       ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 21 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7 VALUE
	       "COM port: Port used for cash drawer ".
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 22 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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

       COPY CONTROL.RD.

       COPY PARAM.RD.

      *
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
006940 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

      /
      *    *****    CASH SLIP / DRAWER CONTROLS    *****
      *
008640 YA000		       SECTION 5.
012450 YA310-PRN.
012460       DISPLAY CLR-SCREEN.
012470       DISPLAY S19.
	     DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR 3
				       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
012480	     MOVE ZERO TO W05-CSHDRW.
012490 YA315.
012500       ACCEPT S34.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F1-KEY GO TO YA318
	       END-EVALUATE
	   ELSE
	       GO TO YA318.
	     PERFORM SAVE-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 43		 TO SHADE-COL.
	     MOVE 36		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
019090	     DISPLAY S-HELP.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
021280	     DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN.
	     GO TO YA315.
       YA318.
	     DISPLAY CLEAR-L25.
012510	   IF W05-CSHDRW = ZERO
012520	       GO TO YA999.
012530     IF W05-CSHDRW > 40
012540	       MOVE "Invalid Cash Draw Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012560         GO TO YA315.
	     DISPLAY CLEAR-L25.
012580	     ADD 150 W05-CSHDRW  GIVING WS-PARKEY.
012590       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012600       MOVE ZERO           TO W05-ESC
012610                              W05-DBL
012620                              W05-CDBL
012630                              W05-SI
012640                              W05-CSI
012650                              W05-SO
012660                              W05-CSO
012670                              W05-6LPI
012680                              W05-8LPI
                                    W05-NUL.
012690       MOVE PAR-ESC        TO W05-E.
012700       MOVE PAR-DBL        TO W05-D.
012710       MOVE PAR-CDBL       TO W05-CD.
012720       MOVE PAR-SI         TO W05-I.
012730       MOVE PAR-CSI        TO W05-CI.
012740       MOVE PAR-SO         TO W05-O.
012750       MOVE PAR-CSO        TO W05-CO.
012760       MOVE PAR-6LPI       TO W05-6.
012770       MOVE PAR-8LPI       TO W05-8.
           IF NOT (PAR-PORT = 1 OR 2 OR 3 OR 4)
               MOVE 1            TO PAR-PORT.
	   IF PAR-DRAWER = "F"
	       MOVE "P" 	 TO W05-DRAWER
	   ELSE
	   IF PAR-DRAWER = "P"
	       MOVE "N" 	 TO W05-DRAWER
	   ELSE
	       MOVE PAR-DRAWER	 TO W05-DRAWER.
	     DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR 3
				       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
012780 YA320.
012790	     ACCEPT S20.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F1-KEY GO TO YA325
	       END-EVALUATE
	   ELSE
	       GO TO YA325.
	     PERFORM SAVE-SCREEN.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 43		 TO SHADE-COL.
	     MOVE 36		 TO SHADE-WIDTH.
	     MOVE 18		 TO SHADE-LINES.
019090	     DISPLAY S-HELP.
	     PERFORM SCREEN-SHADOW.
019250	     DISPLAY CLEAR-L25.
021280	     DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN.
	     GO TO YA320.
       YA325.
	     DISPLAY CLEAR-L25.
012800	   IF 255 < W05-ESC OR W05-DBL OR W05-CDBL OR W05-SI OR
012810              W05-CSI OR W05-SO OR W05-CSO OR W05-6LPI OR W05-8LPI
012820	       MOVE "Decimal values may not be > 255"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
012840         GO TO YA320.
	   IF PAR-ADVANCE > 15
	       MOVE 15		 TO PAR-ADVANCE.
           IF PAR-CPRN = 0
               MOVE 1            TO PAR-CPRN
           ELSE
           IF PAR-CPRN NOT = 9
               IF (PAR-CPRN < 1 OR > 5)
		   MOVE "Printer number MUST be in the range 1 to 5"
				 TO WS-ERR-MES
		   PERFORM ERROR-LENGTH THRU ERROR-EXIT
		   GO TO YA320.
	     CALL "CBL_TOUPPER" USING W05-DRAWER
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (W05-DRAWER = "N" OR "P" OR "K")
	       MOVE "Drawer type must be K, N or P"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
           IF NOT (PAR-PORT = 1 OR 2 OR 3 OR 4)
	       MOVE "COM port must be in the range 1 to 4"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	     CALL "CBL_TOUPPER" USING PAR-CUSDISP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-CUSDISP = "Y" OR "N")
	       MOVE "Customer Display must be Y or N"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	   IF NOT (PAR-CUSPORT = 1 OR 2 OR 3 OR 4)
	       MOVE "DISP port must be in the range 1 to 4"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	     CALL "CBL_TOUPPER" USING PAR-SCALE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-SCALE = "Y" OR "N")
	       MOVE "Digital Scale must be Y or N"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	   IF NOT (PAR-SCALPORT = 1 OR 2 OR 3 OR 4)
	       MOVE "SCALE port must be in the range 1 to 4"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	     CALL "CBL_TOUPPER" USING PAR-SCALE-TYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-SCALE-TYPE = "A" OR "D" OR "O")
	       MOVE "Scale type must be A, D, or O"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	     CALL "CBL_TOUPPER" USING PAR-KTCHPRN
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-KTCHPRN = "Y" OR "N")
	       MOVE "Kitchen printer must be Y or N"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
	   IF PAR-KCHPORT = 0
	       MOVE 1		 TO PAR-KCHPORT
           ELSE
	   IF PAR-KCHPORT NOT = 9
	       IF (PAR-KCHPORT < 1 OR > 5)
		   MOVE "Kitch.Prn no. MUST be in the range 1 to 5"
				 TO WS-ERR-MES
		   PERFORM ERROR-LENGTH THRU ERROR-EXIT
		   GO TO YA320.
	     CALL "CBL_TOUPPER" USING PAR-USE-3
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (PAR-USE-3 = "Y" OR "N")
	       MOVE "Use 3 decimals must be Y or N"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO YA320.
012180	     PERFORM CHECK-CORRECT.
012920	   IF WS-OPTION = "N"
012930         GO TO YA320.
012940       MOVE W05-E          TO PAR-ESC.
012950       MOVE W05-D          TO PAR-DBL.
012960       MOVE W05-CD         TO PAR-CDBL.
012970       MOVE W05-I          TO PAR-SI.
012980       MOVE W05-CI         TO PAR-CSI.
012990       MOVE W05-O          TO PAR-SO.
013000       MOVE W05-CO         TO PAR-CSO.
013010       MOVE W05-6          TO PAR-6LPI.
013020       MOVE W05-8          TO PAR-8LPI.
             MOVE W05-N          TO PAR-NUL.
	   IF PAR-DRAWER = "K"
012460	       DISPLAY CLR-SCREEN
	       DISPLAY "PRINTER 'KICK' DRAWER CONTROLS" AT 0226
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	       DISPLAY "Keyin control characters:" AT 0412
	       ACCEPT S35.
	   IF W05-DRAWER = "N"
	       MOVE "P" 	 TO PAR-DRAWER
	   ELSE
	   IF W05-DRAWER = "P"
	       MOVE "F" 	 TO PAR-DRAWER
	   ELSE
	       MOVE W05-DRAWER	 TO PAR-DRAWER.
013030       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
	   IF NOT (PAR-CUSDISP = "Y")
	       GO TO YA310-PRN.
	     MOVE 7		 TO WS-PARKEY.
012590	     PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012460       DISPLAY CLR-SCREEN.
	     DISPLAY "CUSTOMER DISPLAY HEADING" AT 0329
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Line 1" AT 0512.
	     DISPLAY "Line 2" AT 0612.
       YA330.
	     ACCEPT PAR-DSP-L1 AT 0519
		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	     ACCEPT PAR-DSP-L2 AT 0619
		    WITH BACKGROUND-COLOR 4
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
012180	     PERFORM CHECK-CORRECT.
012920	   IF WS-OPTION = "N"
012930	       GO TO YA330.
013030       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
013040	     GO TO YA310-PRN.
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
016210	     MOVE 1		 TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
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
