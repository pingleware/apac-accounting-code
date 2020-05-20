      ******************************************************************
      *                                                                *
      *    ****    *******    ******   **     **  **	    ********   *
      *   **  **   **	 **  **    **  **     **  **	    **	   **  *
      *  **    **  **	 **  **        **     **  **	    **	   **  *
      *  ********  *******   **	       *********  **	    ********   *
      *  **    **  **	     **        **     **  **	    **	       *
      *  **    **  **	     **    **  **     **  **	    **	       *
      *  **    **  **	      ******   **     **  *******   **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S   S Y S T E M   H E L P   F I L E	       *
      *     M A I N T E N A N C E   P R O G R A M		       *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACHELP.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1994.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       SELECT APCHLP	 ASSIGN DISK
			 LOCK MANUAL
                         WITH LOCK ON MULTIPLE RECORDS
000240                   STATUS WS-STATUS
000250                   ACCESS DYNAMIC
000260                   ORGANIZATION INDEXED
000270			 RECORD HLP-KEY.

       SELECT APCWRK1	 ASSIGN DISK
000040			 STATUS WS-STATUS
000050			 ACCESS RANDOM
000060			 ORGANIZATION RELATIVE
000070			 RELATIVE WS-WRK1KEY.

       SELECT APCWRK2	 ASSIGN DISK
000040			 STATUS WS-STATUS
000050			 ACCESS RANDOM
000060			 ORGANIZATION RELATIVE
000070			 RELATIVE WS-WRK2KEY.

000330 DATA DIVISION.
000340 FILE SECTION.

       FD  APCHLP    IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID "HLP\APACHELP.APC".
       01  HLP-REC.
	   03  HLP-KEY.
	       05  HLP-ID.
		   07  HLP-MODULE  PIC	X(03).
		   07  HLP-PROG	   PIC	X(03).
	       05  HLP-SEQ	   PIC	9(04).
	   03  HLP-DATA 	   PIC	X(36).

       FD  APCWRK1   LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID "APCWORK1.APC".
       01  WK1-REC.
	   03  WK1-KEY.
	       05  WK1-ID.
		   07  WK1-MODULE  PIC	X(03).
		   07  WK1-PROG	   PIC	X(03).
	       05  WK1-SEQ	   PIC	9(04).
	   03  WK1-DATA 	   PIC	X(36).

       FD  APCWRK2   LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID "APCWORK2.APC".
       01  WK2-REC.
	   03  WK2-KEY.
	       05  WK2-ID.
		   07  WK2-MODULE  PIC	X(03).
		   07  WK2-PROG	   PIC	X(03).
	       05  WK2-SEQ	   PIC	9(04).
	   03  WK2-DATA 	   PIC	X(36).

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
       77  WS-S1	   PIC	9(04)	 COMP-5.
       77  WS-S2	   PIC	9(04)	 COMP-5.
       77  WS-S3	   PIC	9(04)	 COMP-5.
       77  WS-S4	   PIC	9(04)	 COMP-5.
       77  WS-S5	   PIC	9(04)	 COMP-5.
       77  WS-S6	   PIC	9(04)	 COMP-5.
003360 77  WS-WRK1KEY	   PIC	9(04)	 COMP-5.
003360 77  WS-WRK2KEY	   PIC	9(04)	 COMP-5.
       77  WS-HSTART	   PIC	9(04)	 COMP-5.
       77  WS-HCUR	   PIC	9(04)	 COMP-5.
       77  WS-HEND	   PIC	9(04)	 COMP-5.
       77  WS-WSTART	   PIC	9(04)	 COMP-5.
       77  WS-WCUR	   PIC	9(04)	 COMP-5.
       77  WS-WEND	   PIC	9(04)	 COMP-5.
       77  WS-NEW	   PIC	X(01).
000950 77  WS-SEQ	   PIC	9(04)	 COMP-5.
       77  WS-SPACES	   PIC	X(36) VALUE SPACES.
       77  WS-WORK1	   PIC	9(01)	 VALUE ZERO.
	   88  WORK1-OPEN  VALUE 1.
       77  WS-WORK2	   PIC	9(01)	 VALUE ZERO.
	   88  WORK2-OPEN  VALUE 1.

       COPY WS.WS.

       COPY FUNCTION.WS.

       01  WS-HELP.
	   03  WS-MODULE   PIC	X(03) VALUE SPACES.
	   03  WS-PROG	   PIC	X(03) VALUE SPACES.

       01  W02-HELP.
	   03  W02-MODULE  PIC	X(03) VALUE SPACES.
	   03  W02-PROG    PIC	X(03) VALUE SPACES.

       01  W05-HELP.
	   03  W05-HLP	   PIC	X(2448).
	   03  W05-LINE	REDEFINES W05-HLP
			   PIC	X(36) OCCURS 68.

       01  W10-HELP.
	   03  W10-HLP.
	       05  W10-L1  PIC	X(36).
	       05  W10-L2  PIC	X(36).
	       05  W10-L3  PIC	X(36).
	       05  W10-L4  PIC	X(36).
	       05  W10-L5  PIC	X(36).
	       05  W10-L6  PIC	X(36).
	       05  W10-L7  PIC	X(36).
	       05  W10-L8  PIC	X(36).
	       05  W10-L9  PIC	X(36).
	       05  W10-L10 PIC	X(36).
	       05  W10-L11 PIC	X(36).
	       05  W10-L12 PIC	X(36).
	       05  W10-L13 PIC	X(36).
	       05  W10-L14 PIC	X(36).
	       05  W10-L15 PIC	X(36).
	       05  W10-L16 PIC	X(36).
	       05  W10-L17 PIC	X(36).
	   03  W10-LINE	REDEFINES W10-HLP
			   PIC	X(36) OCCURS 17.

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
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLR-L1-2.
003860	   05  LINE  1 COLUMN  1 BACKGROUND-COLOR 0
				       VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
004170				 VALUE "APAC Help Maint".
	   05	       COLUMN 17 PIC  X(50) FROM WS-BACKGROUND.
004180	   05	       COLUMN 66 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
004190				 VALUE "Version - 1.0 ".
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

       01  S-HELP.
	   03  LINE  4 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  6 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  7 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  8 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE  9 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 10 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 11 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 12 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 13 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 14 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 15 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 16 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 17 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 18 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 19 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 20 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 21 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 42 FOREGROUND-COLOR 1
				 BACKGROUND-COLOR 7
				 PIC  X(36) FROM WS-SPACES.
	   03	       COLUMN 78 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE "³".
	   03  LINE 22 COLUMN 41 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 7 VALUE
	       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S-PROG.
	   03  LINE  4 COLUMN  4 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN  4 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³ ".
	   03	       COLUMN  6 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 VALUE "Program Id:       ".
	   03	       COLUMN 24 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE " ³".
	   03  LINE  6 COLUMN  4 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S-DATA.
	   03  LINE  5 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L1 AUTO.
	   03  LINE  6 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L2 AUTO.
	   03  LINE  7 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L3 AUTO.
	   03  LINE  8 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L4 AUTO.
	   03  LINE  9 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L5 AUTO.
	   03  LINE 10 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L6 AUTO.
	   03  LINE 11 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L7 AUTO.
	   03  LINE 12 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L8 AUTO.
	   03  LINE 13 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L9 AUTO.
	   03  LINE 14 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L10 AUTO.
	   03  LINE 15 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L11 AUTO.
	   03  LINE 16 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L12 AUTO.
	   03  LINE 17 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L13 AUTO.
	   03  LINE 18 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L14 AUTO.
	   03  LINE 19 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L15 AUTO.
	   03  LINE 20 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L16 AUTO.
	   03  LINE 21 COLUMN 42 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
				 PIC  X(36) USING W10-L17 AUTO.

       01  S-COPY.
	   03  LINE  8 COLUMN  4 FOREGROUND-COLOR 7 BACKGROUND-COLOR 2
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  9 COLUMN  4 FOREGROUND-COLOR 7 BACKGROUND-COLOR 2
				 VALUE "³ ".
	   03	       COLUMN  6 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "Program Id:       ".
	   03	       COLUMN 24 FOREGROUND-COLOR 7 BACKGROUND-COLOR 2
				 VALUE " ³".
	   03  LINE 10 COLUMN  4 FOREGROUND-COLOR 7 BACKGROUND-COLOR 2
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  ERROR-LINE.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
				 FOREGROUND-COLOR WS-FGRND-5
				 HIGHLIGHT
				 PIC  X(48) FROM WS-ERR-MES.
      /
      * ******	******	 *****	 *****	******* ******	**   ** ******	*******
      * **   ** **   ** **   ** **   ** **	**   ** **   ** **   ** **
      * **   ** **  **	**   ** **	**	**   ** **   ** **  **	**
      * ******	*****	**   ** **	*****	**   ** **   ** *****	*****
      * **	**  **	**   ** **	**	**   ** **   ** **  **	**
      * **	**   ** **   ** **   ** **	**   ** **   ** **   ** **
      * **	**   **  *****	 *****	******* ******	 *****	**   ** *******
      *
001230 PROCEDURE DIVISION.
006220 AA000	       SECTION.
006230 AA00.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 2		 TO USER-ACTION.
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	   ELSE
	       GO TO AA02.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
       AA02.
	     DISPLAY CLR-SCREEN.
	     OPEN I-O APCHLP.
	   IF WS-STATUS NOT = "00"
	       CLOSE APCHLP
	       GO TO AA00.
	     PERFORM BA000.
	     CLOSE APCHLP.
       AA999.
	     STOP RUN.

      /
      *    ****    READ FILES ROUTINES
      *
       AB000-READ      SECTION.

008460 READ-APCWRK1.
008470	     READ APCWRK1	 WITH IGNORE LOCK.
008480     IF WS-STATUS = "00"
008490	       GO TO READ-APCWRK1-EXIT.
008500     IF WS-STAT1 = "2" OR "3" OR "4"
008510	       MOVE 2		 TO WS-F-ERROR
008520         PERFORM READ-ERROR.
008560	     MOVE 2		 TO WS-F-ERROR.
008570       PERFORM READ-ERROR.

008740 READ-APCWRK1-EXIT.
008750       EXIT.

008460 READ-APCWRK2.
008470	     READ APCWRK2	 WITH IGNORE LOCK.
008480     IF WS-STATUS = "00"
008490	       GO TO READ-APCWRK2-EXIT.
008500     IF WS-STAT1 = "2" OR "3" OR "4"
008510	       MOVE 3		 TO WS-F-ERROR
008520         PERFORM READ-ERROR.
008560	     MOVE 3		 TO WS-F-ERROR.
008570       PERFORM READ-ERROR.

008740 READ-APCWRK2-EXIT.
008750       EXIT.
      
      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AC000-WRITE     SECTION.

010520 REWRITE-APCWRK1.
010530	     REWRITE WK1-REC.
010540     IF WS-STATUS NOT = "00"
010550	       MOVE 2		 TO WS-F-ERROR
010560         PERFORM WRITE-ERROR.
010570	     GO TO WRITE-APCWRK1-EXIT.
      
010580 WRITE-APCWRK1.
	     MOVE WS-WRK1KEY	 TO WK1-SEQ.
010590	     WRITE WK1-REC.
	   IF WS-STATUS = "22"
	       ADD 1		 TO WS-WRK1KEY
	       GO TO WRITE-APCWRK1.
010600	   IF WS-STATUS NOT = "00"
010610	       MOVE 2		 TO WS-F-ERROR
010620         PERFORM WRITE-ERROR.

010630 WRITE-APCWRK1-EXIT.
010640       EXIT.
      
010520 REWRITE-APCWRK2.
010530	     REWRITE WK2-REC.
010540     IF WS-STATUS NOT = "00"
010550	       MOVE 3		 TO WS-F-ERROR
010560         PERFORM WRITE-ERROR.
010570	     GO TO WRITE-APCWRK2-EXIT.
      
010580 WRITE-APCWRK2.
	     MOVE WS-WRK2KEY	 TO WK2-SEQ.
010590	     WRITE WK2-REC.
	   IF WS-STATUS = "22"
	       ADD 1		 TO WS-WRK2KEY
	       GO TO WRITE-APCWRK2.
010600	   IF WS-STATUS NOT = "00"
010610	       MOVE 3		 TO WS-F-ERROR
010620         PERFORM WRITE-ERROR.

010630 WRITE-APCWRK2-EXIT.
010640       EXIT.

       BA000	       SECTION.
       BA00.
	     DISPLAY S-PROG.
	     MOVE SPACES       TO WS-HELP.
	     ACCEPT WS-HELP AT 0518
		    WITH FOREGROUND-COLOR 4 BACKGROUND-COLOR 7
			 UPDATE AUTO.
	     MOVE 6	       TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-HELP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY WS-HELP AT 0518
		    WITH FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
	   IF WS-HELP = SPACES
	       GO TO BA999.
	     MOVE WS-HELP      TO HLP-ID.
	     MOVE ZERO	       TO HLP-SEQ.
	     DISPLAY S-HELP.
006090	     START APCHLP
	       KEY >= HLP-KEY.
006100	   IF WS-STAT1 = "0"
	       MOVE "N" 	 TO WS-NEW
006110	       GO TO BA05.
	     MOVE "Y"		 TO WS-NEW.

       BA05.
	     MOVE ZERO	       TO WS-HEND.
	     MOVE SPACES       TO W05-HELP.
	     MOVE 1	       TO WS-HCUR.
	   IF WS-NEW = "Y"
	       GO TO BA15.
	   IF WORK1-OPEN
	       CLOSE APCWRK1
	       MOVE ZERO       TO WS-WORK1.
	   IF WORK2-OPEN
	       CLOSE APCWRK2
	       MOVE ZERO       TO WS-WORK2.

       BA10.
	     READ APCHLP NEXT.
	   IF WS-STAT1 NOT = ZERO
	       GO TO BA15.
	   IF WS-HELP NOT = HLP-ID
	       GO TO BA15.
	   IF NOT WORK1-OPEN
	       OPEN OUTPUT APCWRK1
	       CLOSE APCWRK1
	       OPEN I-O APCWRK1
	       MOVE ZERO       TO WS-WRK1KEY
	       MOVE 1	       TO WS-WORK1.
	     ADD 1	       TO WS-HEND WS-WRK1KEY.
	     MOVE HLP-DATA     TO W05-LINE (WS-HEND).
	     MOVE HLP-REC      TO WK1-REC.
	     PERFORM WRITE-APCWRK1 THRU WRITE-APCWRK1-EXIT.
	     GO TO BA10.

       BA15.
	     MOVE 1	       TO WS-S1 WS-S4 WS-HSTART WS-WSTART.
	     MOVE WS-HCUR      TO WS-S2 WS-WCUR.

       BA20.
	     MOVE 1	       TO WS-WEND.
	     MOVE WS-HCUR      TO WS-S6.
	     INITIALIZE W10-HELP.

       BA25.
	     MOVE W05-LINE (WS-S6)
			       TO W10-LINE (WS-WEND).
	   IF WS-S5 < 17
	       ADD 1	       TO WS-S6 WS-WEND
	       GO TO BA25.

       BA30.
	     DISPLAY "PgDn" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     ", " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     ", " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F1" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Copy," WITH BACKGROUND-COLOR 3
				   FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Insert line,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Delete line"
		      WITH BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 1.
	     ACCEPT S-DATA.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *
      *    E X I T
      *
		 WHEN ESC-KEY GO TO BA55
      *
      *	   C O P Y   A N   E X I S T I N G   H E L P   S C R E E N
      *
		 WHEN F1-KEY
		   PERFORM CA000
		   GO TO BA15
      *
      *	   I N S E R T	 A   L I N E
      *
		 WHEN F2-KEY
		   PERFORM DA000
		   GO TO BA15
      *
      *	   D E L E T E	 A   L I N E
      *
		 WHEN F3-KEY
		   PERFORM EA000
		   GO TO BA15
      *
      *    P A G E   U P
      *
		 WHEN PAGE-UP
		     IF WS-S4 = 0 OR 1
			 GO TO BA30
		     ELSE
			 GO TO BA50
		     END-IF
      *
      *    P A G E   D O W N
      *
		 WHEN PAGE-DOWN  GO TO BA85
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA30.

       BA35.
	     MOVE 1	       TO WS-WCUR.
	     MOVE WS-HCUR      TO WS-S6.

       BA40.
	     MOVE W10-LINE (WS-WCUR)
			       TO W05-LINE (WS-S6).
	   IF WS-WCUR < 17
	       ADD 1	       TO WS-S6 WS-WCUR
	       GO TO BA40.

       BA45.
	   IF WS-S6 < 68
	       MOVE WS-HCUR    TO WS-S1
	       ADD 1 WS-S1     GIVING WS-S4
	       MOVE 2142       TO COL-AND-LINE
	       GO TO BA20.
	     GO TO BA30.

       BA50.
	     MOVE WS-S1	       TO WS-S4.
	     PERFORM BA35 THRU BA40.
	   IF WS-S1 > 17
	       SUBTRACT 17     FROM WS-S1
	   ELSE
	       MOVE 1	       TO WS-S1.
	     GO TO BA20.

       BA55.
	     MOVE 68	       TO WS-S2.

       BA60.
	   IF W05-LINE (WS-S2) = SPACES
	       SUBTRACT 1      FROM WS-S2
	       GO TO BA60.
	   IF WS-S2 < WS-S3
	       GO TO BA75.

       BA65.
	     MOVE 1	       TO WS-S1.
	     MOVE WS-HELP      TO HLP-ID.

       BA70.
	     MOVE WS-S1        TO HLP-SEQ.
	     MOVE W05-LINE (WS-S1)
			       TO HLP-DATA.
	   IF NOT (WS-S1 > WS-S3)
	       REWRITE HLP-REC
	   ELSE
	       WRITE HLP-REC.
	   IF WS-S1 < WS-S2
	       ADD 1	       TO WS-S1
	       GO TO BA70.
	     DISPLAY CLR-SCREEN.
	     GO TO BA00.

       BA75.
	     MOVE WS-HELP      TO HLP-ID.

       BA80.
	     MOVE WS-S3	       TO HLP-SEQ.
	     DELETE APCHLP.
	   IF WS-S3 > WS-S2
	       SUBTRACT 1      FROM WS-S3
	       GO TO BA80.
	     GO TO BA65.

       BA85.
	     MOVE 1	       TO WS-S5.
	     MOVE WS-S4        TO WS-S6.

       BA90.
	     MOVE W10-LINE (WS-S5)
			       TO W05-LINE (WS-S6).
	   IF WS-S5 < 17
	       ADD 1	       TO WS-S6 WS-S5
	       GO TO BA90.
	   IF WS-S6 < 52
	       MOVE WS-S4      TO WS-S1
	       ADD 1 WS-S6     GIVING WS-S4
	       GO TO BA20
	   ELSE
	       MOVE 51	       TO WS-S1
	       MOVE 52	       TO WS-S4
	       GO TO BA20.
	     GO TO BA30.

       BA999.
	     EXIT.

       CA000	       SECTION.
       CA00.
	     DISPLAY S-COPY.
	     MOVE SPACES	TO W02-HELP.
	     ACCEPT W02-HELP AT 0918
		    WITH FOREGROUND-COLOR 6 HIGHLIGHT
			 BACKGROUND-COLOR 2
			 UPDATE AUTO.
	     MOVE 6		TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-HELP
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY W02-HELP AT 0918
		    WITH FOREGROUND-COLOR 6 HIGHLIGHT
			 BACKGROUND-COLOR 2.
	   IF W02-HELP = SPACES
	       GO TO CA999.
	     MOVE W02-HELP	 TO HLP-ID.
	     MOVE ZERO		 TO HLP-SEQ.
006090	     START APCHLP
	       KEY >= HLP-KEY.
006100	   IF WS-STAT1 = "0"
006110	       GO TO CA05.
	     MOVE "X"		 TO WS-NEW.

       CA05.
	     MOVE ZERO		 TO WS-S3.
	     INITIALIZE W05-HELP.
	   IF WS-NEW = "X"
	       GO TO CA999.

       CA10.
	     READ APCHLP NEXT.
	   IF WS-STAT1 NOT = ZERO
	       GO TO CA15.
	   IF W02-HELP NOT = HLP-ID
	       GO TO CA15.
	     ADD 1		 TO WS-S3.
	     MOVE HLP-DATA	 TO W05-LINE (WS-S3).
	     GO TO CA10.

       CA15.
	     MOVE ZERO		 TO WS-S3.

       CA999.
	     EXIT.

       DA000	       SECTION.
       DA00.


       DA999.
	     EXIT.

       EA000	       SECTION.
       EA00.


       EA999.
	     EXIT.
