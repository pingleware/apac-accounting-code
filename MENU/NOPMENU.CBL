      ******************************************************************
      *                                                                *
      *  **	**   ******   *******	**     **  *******  **	   **  *
      *  ***	**  **	  **  **    **	***   ***  **	    ***    **  *
      *  ** *	**  **	  **  **    **	** * * **  **	    ** *   **  *
      *  **  *	**  **	  **  *******	**  *  **  *****    **	*  **  *
      *  **   * **  **	  **  **	**     **  **	    **	 * **  *
      *  **    ***  **	  **  **	**     **  **	    **	  ***  *
      *  **	**   ******   **	**     **  *******  **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     N O T   A V A I L A B L E	I N   T H I S	V E R S I O N  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     NOPMENU.
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

       01  GET-SINGLE-CHAR     PIC  9(02)    COMP-X VALUE 26.

000300 COPY WS.WS.

000300 COPY CHAIN.WS.

      /
002520 PROCEDURE DIVISION CHAINING WS-PARID WS-USER-ID WS0-PROGRAMS.
002530 AA000-MAIN      SECTION.
       AA00.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1425
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "¿" WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 0.
	     DISPLAY "³" AT 1525
		      WITH BACKGROUND-COLOR 4
			   FOREGROUND-COLOR 4 HIGHLIGHT
		     " The Option requested, is not "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "³" AT 1625
		      WITH BACKGROUND-COLOR 4
			   FOREGROUND-COLOR 4 HIGHLIGHT
		     " available in this version.   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³" WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "À" AT 1725
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 0
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			       BACKGROUND-COLOR 0.
	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 1827
		      WITH FOREGROUND-COLOR 0 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA05.
             EXIT PROGRAM.
