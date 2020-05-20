      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******    ********   *******      ****     **      ******** *
      *   **    **      **      **    **    **  **    **	     **    *
      *   **            **      **    **   **    **   **         **    *
      *    ******       **      *******    ********   **	     **    *
      * 	    **      **      **	       **    **   **	     **    *
      *   **    **      **      **	       **    **   **         **    *
      *    ******       **      **	       **    **   *******    **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   This program is used to keep the Stock Alternate Index up-   *
      *   dated (Individual words in stock Description). Add new items *
      *   update existing entries and delete obsolete entries.         *
      *                                                                *
      *     VERSION 9.00.00 - November 2012                            *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPALT.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   July 2007.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2007 - 2013
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
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000220 COPY "RECOVER.SL".

000140 COPY "STKALT.SL".

000250 DATA DIVISION.
000260 FILE SECTION.
      *
      *   Using External file as the program is called by programs
      *   that are writing recovery details to the RECOVERY file
      *   at the time this program is required to update the entries
      *   for the Alternate Index file.
      *
000380 COPY "RECOVER.FDE".

000140 COPY "STKALT.FDE".

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)	 VALUE "aeWlimemnomLalismJ".
001710 77  WS-S1                   PIC  9(04)    COMP-5.
001720 77  WS-S2                   PIC  9(04)    COMP-5.
001730 77  WS-S3                   PIC  9(04)    COMP-5.
001740 77  WS-S4                   PIC  9(04)    COMP-5.
001890 77  WS-RECKEY               PIC  9(06)    COMP-5.
002020 77  WS-OPTION	           PIC  X(01).

      /

002330 COPY "WS.WS".

       COPY "W12.WS".

       COPY "W20.WS".

003700 01  W25-CALCS.
005040     03  W25-RESULT          PIC  9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE       PIC  9(05).
004350         05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
003750     03  W25-SCOST           PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST           PIC S9(09)V99 COMP-3.
003760     03  W25-PCST            PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT            PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL           PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL           PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE           PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST          PIC S9(09)V99 COMP-3.
001660     03  W25-TIME            PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680         05  FILLER          PIC  9(02).
001690         05  W25-FACT        PIC  9(06).
               05  W25-DATE REDEFINES W25-FACT.
                   07  W25-YY      PIC  9(02).
                   07  W25-MM      PIC  9(02).
                   07  W25-DD      PIC  9(02).
           03  W25-CUR-CC          PIC  9(02) COMP-5.
           03  W25-PRV-CC          PIC  9(02) COMP-5.
           03  W25-NXT-CC          PIC  9(02) COMP-5.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

003800 COPY "W50.WS".

       LINKAGE SECTION.

002070 77  WS-ERROR                  PIC  9(01).
001900 77  WS-RECOVER                PIC  9(06)    COMP-5.
001910 77  WS-TRANS                  PIC  9(04)    COMP-5.
       77  W02-RECOVER               PIC  X(64).
       77  W02-STKALT                PIC  X(64).

       COPY "HEADING.CRT".


       COPY "CHAIN.LS".

       01  STK-CODE.
000040     03  STK-ITEM.
               05  STK-PLU           PIC  9(14).
000060     03  STK-ITM               PIC  X(03).
           03  FILLER                PIC  X(01).

000080 01  STK-DESC.
           03  STK-DSC26.
000090         05  STK-DKEY          PIC  X(14).
               05  FILLER            PIC  X(12).
000100     03  FILLER                PIC  X(04).

       01  STK-DESC2.
000090     03  STK-DKEY2             PIC  X(14).
000100     03  FILLER                PIC  X(16).


      /
004130 SCREEN SECTION.

      /
007730 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY W02-STKALT W02-RECOVER WS-RECOVER WS-TRANS STK-CODE STK-DESC STK-DESC2 WS-ERROR.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
           IF WS-ERROR = 9
               GO TO AA99.
      *
      *      LS-INSTR:
      *               A = Amend entries
      *               C = Create new entries
      *               D = Delete existing entries
      *
             EVALUATE LS-INSTR
               WHEN "A"
                    PERFORM DELETE-STOCK-ALT-INDEX
                    PERFORM CREATE-STOCK-ALT-INDEX
               WHEN "C"
                    PERFORM CREATE-STOCK-ALT-INDEX
               WHEN "D"
                    PERFORM DELETE-STOCK-ALT-INDEX
             END-EVALUATE.

       AA99.
	     EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3    Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box + 1.           Ё
      *    Ё          SHADE-COL   - Left line of box + 2.              Ё
      *    Ё          SHADE-WIDTH - Width of the box - 2.              Ё
      *    Ё          SHADE-LINES - Hight of box - 1.                  Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      CHECK-CORRECT                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё           "Press Y if correct - N if incorrect"           Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.	                             Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      * 	       D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      ERASE SCREEN FROM SPECIFIED LOCATION	       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CPOS (CLIN,CCOL) as starting position and increases  Ё
      *    Ё CLIN by 1 until CLIN reaches the line allocated to exit.  Ё
      *    Ё The screen is cleared with Column 1 and 80 containing "Ё" Ё
      *    Ё and columns 2 to 79 containing spaces.		       Ё
      *    Ё  eg.						       Ё
      *    Ё	  MOVE 0801	 TO CPOS.			       Ё
      *    Ё	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY "STKALT.RD".

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY "STKALT.WR".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
      *            ORIGINAL ACTION 	         RECOVERY
      *        здддддддддддддддддддддд©    зддддддддд©
      *        Ё0 = RECORD CHANGED    Ё    Ё(REWRITE)Ё
      *        Ё1 = RECORD WAS ADDED  Ё    Ё(DELETE) Ё
      *        Ё2 = RECORD WAS DELETEDЁ    Ё(WRITE)  Ё
      *        юдддддддддддддддддддддды    юддддддддды

011430 AY000           SECTION.

010970 AY49S.
010980       MOVE 49                 TO REC-FILE.
008020       MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
011000       MOVE STKA-RECORD1       TO REC-STKALT.

011820 AY50.
011830       ADD 1                   TO WS-RECOVER.
011840       MOVE WS-RECOVER         TO WS-RECKEY.
011850       MOVE WS-TRANS           TO REC-TRAN.
011860       WRITE REC-RECORD.
011870     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
011900         STOP RUN.
011930 AY59.
011940       EXIT.

      /
      *    ****    Generate the Stock Alternate Index entries
      *
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё This routine is  used to  create  an index of the various Ё
      *    Ё words that are present in the Stock description.          Ё
      *    Ё The first record will be used for the first 12 characters Ё
      *    Ё of the Stock description as it is for description index   Ё
      *    Ё of the Stock record. All Stock records types will have an Ё
      *    Ё entry for each word that appears in the description. The  Ё
      *    Ё following procedure will be applied;                      Ё
      *    Ё    Find the next word and move the next 12 characters to  Ё
      *    Ё    index key. Repeat until the end of the description has Ё
      *    Ё    been reached. This procedure allows for words such as; Ё
      *    Ё    the, and, etc to have the word following as part of    Ё
      *    Ё    the key which reduces the duplicate keys and gives a   Ё
      *    Ё    better index spread.                                   Ё
      *    Ё This routine can be altered as required.                  Ё
      *    Ё See also STOCK.DSC (Does not log to recovery file).       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *                                                                *
      *  Aug 2008	- New file (STKALT) - Stock alternate index included *
      * 		  for lookups, using any word contained in the Stock *
      * 		  description.                                       *
      *                                                                *
      ******************************************************************
       CREATE-STOCK-ALT-INDEX   SECTION 6.
       CREATE-ALT-STOCK-INDEX.
019690       DISPLAY "Generating Alternate Description Index" AT 5018 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      *
      *    ****    Initialize the extended alpha index record.
      *
             MOVE STK-CODE           TO STKA-CODE.
             MOVE ZERO               TO STKA-NO.
             MOVE STK-DESC           TO W50-DATA STKA-DESC STKA-DKEY.
      *
      *    ****    Write the first entry of the Alternate Index.
      *            This is the same as the Alpha Index entry for the
      *            Stock record.
      *
             PERFORM WRITE-STKALT THRU WRITE-STKALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
             MOVE 1                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
             MOVE 1                  TO WS-S1.
             MOVE 30                 TO WS-S3.
      *
      *    ****    Find the length of the Stock description (characters).
      *            Look for first non space character - starting with 30
      *            and working back. (WS-S3)
      *
       STOCK-FIND-LENGTH.
           IF W50-CCHAR(WS-S3) = SPACE
               SUBTRACT 1            FROM WS-S3
               GO TO STOCK-FIND-LENGTH.
             MOVE ZERO               TO WS-S4.
      *
      *    ****    Find the next word in the description.
      *
	STOCK-NEXT-WORD.
      *
      *    ****    Check if end of word reached and add 1 to the number
      *            of words found (WS-S4). This is used, together with
      *            the Item code, to provide a unique key for the
      *            STKALT records.
      *
           IF W50-CCHAR(1) = SPACE
      *
      *    ****    Move data, one character to the left and reduce the
      *            number of characters left in the description.
      *
               MOVE W50-D39          TO W50-DATA
               SUBTRACT 1            FROM WS-S3
               ADD 1                 TO WS-S4
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If it is necessary to exclude words from the alternate    Ё
      *    Ё index, such as '(PTY)', 'LTD' etc, then this would be the Ё
      *    Ё the point to perform the routine to exclude the word.     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
               GO TO STOCK-STORE-NEXT.
      *
      *    ****    Move data, one character to the left and reduce the
      *            number of characters left in the description.
      *
             MOVE W50-D39            TO W50-DATA.
             SUBTRACT 1              FROM WS-S3.
           IF WS-S3 > 0
               GO TO STOCK-NEXT-WORD.
      *
      *    ****  No more words left in the description.
      *
             GO TO STOCK-DESC-INDEX-EXIT.

       STOCK-STORE-NEXT.
             MOVE WS-S4              TO STKA-NO.
             MOVE W50-DATA           TO STKA-DKEY.
      *
      *    ****    Write an entry in the Alternate Index.
      *            This is an additional Alpha Index entry for the
      *            Stock Record.
      *
             PERFORM WRITE-STKALT THRU WRITE-STKALT-EXIT.
      *
      *    ****   Log to recovery file.
      *
             MOVE 1                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
           IF WS-S3 > 0
               GO TO STOCK-NEXT-WORD.

       STOCK-DESC-INDEX-EXIT.
	     EXIT.


       DELETE-STOCK-ALT-INDEX   SECTION 6.
       DELETE-ALT-STOCK-INDEX.
      *
      *    ****    C H E C K   T H E   A L T E R N A T E   I N D E X
      *
             MOVE ZERO               TO WS-ERROR.
             MOVE STK-CODE           TO STKA-CODE.
             MOVE ZERO               TO STKA-NO.
             PERFORM READ-STKALT THRU READ-STKALT-EXIT.
           IF WS-F-ERROR = 49
               MOVE 9                TO WS-ERROR
               GO TO DELETE-ALT-STOCK-EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If  either of the two descriptions or Item code has  been Ё
      *    Ё amended, then  the existing Alternate Index  entries will Ё
      *    Ё be deleted and new entries generated.                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       DELETE-ALT-STOCK-LOOP.
             MOVE 2                  TO WS-ACTION.
             PERFORM AY49S THRU AY59.
             PERFORM DELETE-STKALT-REC THRU WRITE-STKALT-EXIT.
             PERFORM READ-STKALT-NEXT THRU READ-STKALT-EXIT.
           IF NOT(WS-F-ERROR = 49)
               IF (STKA-CODE = STK-CODE)
	             GO TO DELETE-ALT-STOCK-LOOP.

       DELETE-ALT-STOCK-EXIT.
             EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

	     COPY "FUNCTION.PRO".

       ZA00A.

044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION.

       COPY "ERRORS.PRO".

041560 DISPLAY-FILE-NAME.
041850     IF WS-F-ERROR = 49
041860         MOVE W02-STKALT       TO WS-FILE
041870         MOVE ZERO             TO WS-KEY
               MOVE STKA-KEY         TO WS-KEYX.

       COPY "DISPERR.PRO".
