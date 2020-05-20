      ******************************************************************
      *                                                                *
      *    ******    ******   *******	 ******    ******   **	   **  *
      *   **	**  **	  **  **    **	**    **  **	**  ***    **  *
      *   **	    **	  **  **    **	**	  **	**  ** *   **  *
      *   **	    **	  **  ******	**	  **	**  **	*  **  *
      *   **	    **	  **  **	**	  **	**  **	 * **  *
      *   **	**  **	  **  **	**    **  **	**  **	  ***  *
      *    ******    ******   **	 ******    ******   **	   **  *
      *                                                                *
      *       ENGLISH						       *
      *                                                                *
      *       C O N V E R T   C O B O L   S O U R C E	C O D E	       *
      *                                                                *
      * QUOTES INSERTED	FOR COPYBOOK STATEMENTS IF QUOTES ARE MISSING  *
      *                                                                *
      *     VERSION OPEN-COBOL COVERTION - April 2011		       *
      * 							       *
      ******************************************************************
      * 							       *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     COPCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   April 2011.

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
