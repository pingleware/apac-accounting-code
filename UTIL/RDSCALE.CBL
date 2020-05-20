      ******************************************************************
      *                                                                *
      * ******	 ******    *****    *****    *****   **       *******  *
      * **   **  **   **  **   **  **	**  **	 **  **       **       *
      * **   **  **   **  **	   **	    **	 **  **       **       *
      * *****	 **   **   *****   **	    *******  **       *****    *
      * **   **  **   **       **  **	    **	 **  **       **       *
      * **   **  **   **  **   **  **	**  **	 **  **       **       *
      * **   **  ******    *****    *****   **	 **  *******  *******  *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   R E A D   I N P U T	F R O M   S C A L E   P R O G R A M    *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     RDSCALE.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2008
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

000170	   SELECT SCALES  ASSIGN W02-SCALES
000300                    ORGANIZATION LINE SEQUENTIAL
000180			  STATUS WS-STATUS
000310			  ACCESS SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

000530 FD  SCALES    LABEL RECORD OMITTED.
000550 01  SCA-REC.
	   03  SCA-LINE        PIC  X(30).
       01  SCA-REC2.
	   03  SCA-CHAR1       PIC  X(01).
	   03  SCA-CHAR2-30    PIC  X(29).
       01  SCA-REC3.
	   03  SCA-KG	       PIC  9(03).
	   03  SCA-DEC	       PIC  X(01).
	   03  SCA-GRAM        PIC  9(03).
	   03  FILLER	       PIC  X(23).

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".

000590 COPY WS.WS.

       COPY FUNCTION.WS.

       01  W02-FILE-IDS.

	   03  W02-SCALES      PIC X(04) VALUE "COM2".

       COPY W12.WS.

       LINKAGE SECTION.

       01  SCALE-QUANTITY.
	   03  SCALE-PORT.
	       05  SCALE-NO	 PIC  X(04).

	   03  SCALE-QUANT.
	       05  SCALE-WEIGHT.
		   07  SCALE-QNT PIC  9(09)V999.
	       05  SCALE-WEIGHT2 REDEFINES SCALE-WEIGHT.
		   07  SCALE-KG	 PIC  9(09).
		   07  SCALE-GR	 PIC  9(03).

      /
001230 PROCEDURE DIVISION USING SCALE-QUANTITY.
001220 AA000	       SECTION.
001230 AA00.
	     MOVE ZERO		TO SCALE-QNT.
	     MOVE SCALE-NO	TO W02-SCALES.
042140	     OPEN I-O SCALES.
003050	   IF NOT (WS-STATUS = "00" OR "41")
	       GO TO AA999.

001340 AA05.
	     READ SCALES.

       AA10.
	   IF NOT (SCA-CHAR1 = "+" OR "-")
	       MOVE SCA-CHAR2-30 TO SCA-REC
	       IF SCA-REC = SPACES
		   GO TO AA999
	       ELSE
		   GO TO AA10.
	     MOVE SCA-CHAR2-30	 TO SCA-REC.
	   IF SCA-REC = SPACES
	       GO TO AA999.
	     MOVE SCA-KG	 TO SCALE-KG.
	     MOVE SCA-GRAM	 TO SCALE-GR.
001830	     CLOSE SCALES.

       AA999.
	     EXIT-PROGRAM.
