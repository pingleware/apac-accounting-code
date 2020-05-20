      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	       **     **    **	**    **  **		  **   *
      *    ******      **     *******	**    **  ********	***    *
      * 	**     **     **	**    **  **	**	  **   *
      *   **	**     **     **	 **  **   **	**  **	  **   *
      *    ******      **     **	  ****	   ******    ******    *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      * S T O C K   L O C A T I O N   W / H O U S E   D E L E T I O N S*
      * 							       *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP063.
000030 AUTHOR.	       J.W.LEMMON (JWL Software).
000040 DATE-WRITTEN.   APRIL 1996.

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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000280 COPY PARAM.FDE.

000380 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001710 77  WS-SUB	    PIC 9(04)	  COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001860 77  WS-PARKEY        PIC 9(06)     COMP-5.
001870 77  WS-NETKEY        PIC 9(06)     COMP-5.
001890 77  WS-RECKEY        PIC 9(06)     COMP-5.
001900 77  WS-RECOVER       PIC 9(06)     COMP-5.
001900 77  WS-RECORDS       PIC 9(06)     COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
       77  WS-ETYPE	    PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002140 77  WS-CONREC        PIC 9(01) VALUE ZERO.
002150 77  WS-LEDG	    PIC X(04).
002200 77  WS-COMP          PIC 9(01).
002200 77  WS-IND	    PIC 9(01) VALUE ZERO.
002220 77  WS-TYPE          PIC X(01).
002270 77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  WS-ERR1	   PIC	X(20) VALUE "Quantity > Available".
002030 77  WS-ERR3         PIC  X(15) VALUE "Data incomplete".
002070 77  WS-ERR7	   PIC	X(23) VALUE
002080                     "Address > 67 characters".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
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
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
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
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "064".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000380 COPY RECOVER.ID.

       COPY SHARED.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

000030 01  W15-RECORD1.
	   03  W15-KEY.
	       05  W15-WAR     PIC  X(06).
	       05  W15-EXT-CODE.
000040		 07  W15-CODE  PIC  X(14).
000060		 07  W15-ITM   PIC  X(03).
		 07  FILLER    PIC  X(01).
	   03  W15-BIN	       PIC  X(06).
000110	   03  W15-QUANT       PIC S9(07)V9(04) COMP-3.
000200	   03  W15-MTD	       PIC S9(09)V99  COMP-3.
000230	   03  W15-YTD	       PIC S9(09)V99  COMP-3.
000220	   03  W15-MTDV        PIC S9(09)V99  COMP-3.
000250	   03  W15-YTDV        PIC S9(09)V99  COMP-3.
	   03  FILLER	       PIC  X(02).

000030 01  W16-RECORD1.
	   03  W16-KEY.
	       05  W16-WAR     PIC  X(06).
	       05  W16-EXT-CODE.
000040		 07  W16-CODE  PIC  X(14).
000060		 07  W16-ITM   PIC  X(03).
		 07  FILLER    PIC  X(01).
	   03  W16-BIN	       PIC  X(06).
000110	   03  W16-QUANT       PIC S9(07)V9(04) COMP-3.
000200	   03  W16-MTD	       PIC S9(09)V99  COMP-3.
000230	   03  W16-YTD	       PIC S9(09)V99  COMP-3.
000220	   03  W16-MTDV        PIC S9(09)V99  COMP-3.
000250	   03  W16-YTDV        PIC S9(09)V99  COMP-3.
	   03  FILLER	       PIC  X(02).

       COPY W40.WS.

002920 01  W40-WAREHOUSE.
002930     03  W40-NAME1.
002940	       05  W40-NAME    PIC  X(40).
003020	   03  W40-NAME2 REDEFINES W40-NAME1.
003030	       05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
	   03  W40-CODE	       PIC  X(06).
	   03  W40-CODE1       PIC  X(06).
	   03  W40-CODE2       PIC  X(06).
003070	   03  W40-ADD.
003080	       05  W40-ADDRESS1.
003090		   07  W40-ADD1	PIC X(30).
003100		   07  W40-ADD2	PIC X(30).
003110		   07  W40-ADD3	PIC X(30).
003120		   07  W40-ADD4	PIC X(30).
003130	       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140		   07  W40-ACHAR OCCURS 120 PIC X(01).
003230     03  W40-POST        PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

003550 01  W60-ADD.
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-WHOUSE OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).

003930 01  W80-EDIT.
003950	   03  W80-Q1.
003960	       05  W80-S7V4   PIC Z(06)9.9999-.

004120 01  W95-COMP            PIC X(40).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S01.
	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "DELETE WAREHOUSE DETAILS".
	   03  LINE  4 COLUMN  5 VALUE "Code     ".
006240	   03  LINE  5 COLUMN  5 VALUE "Name     ".
	   03  LINE  6 COLUMN  5 VALUE "Contact  ".
006290	   03  LINE  7 COLUMN 15 VALUE "Address  ".
006310	   03  LINE  8 COLUMN  5 VALUE "Line 1   ".
006330	   03  LINE  9 COLUMN  5 VALUE "     2   ".
006350	   03  LINE 10 COLUMN  5 VALUE "     3   ".
006370	   03  LINE 11 COLUMN  5 VALUE "     4   ".
006390	   03  LINE 12 COLUMN  5 VALUE "P/Code   ".
006450	   03  LINE 13 COLUMN  5 VALUE "Telephone".
	   03  LINE 14 COLUMN  5 VALUE "Fax      ".

       01  S02.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W40-CODE AUTO.

       01  S03.
006550	   03  LINE  5 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006560                           PIC X(40) USING W40-NAME AUTO.
006570	   03  LINE  6 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W40-CONTACT AUTO.
006610	   03  LINE  8 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006620                           PIC X(30) USING W40-ADD1 AUTO.
006630	   03  LINE  9 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006640                           PIC X(30) USING W40-ADD2 AUTO.
006650	   03  LINE 10 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006660                           PIC X(30) USING W40-ADD3 AUTO.
006670	   03  LINE 11 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006680                           PIC X(30) USING W40-ADD4 AUTO.
006690	   03  LINE 12 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006700                           PIC 9(04) USING W40-POST AUTO.
006810	   03  LINE 13 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006820                           PIC X(14) USING W40-TEL AUTO.
006830	   03  LINE 14 COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
006840                           PIC X(14) USING W40-FAX AUTO.

       COPY WARLUP.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CB000.
013040	     CLOSE RECOVER.
013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM WARHSE-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
	     PERFORM SAVE-SCREEN.
	     MOVE 6		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0526
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "¿"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023190	     DISPLAY "³" AT 0626
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Insufficient security     "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
023200	     DISPLAY "³" AT 0726
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     " Press ENTER to continue   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 0826
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.
023210	     ACCEPT WS-OPTION AT 0752
		    WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7 AUTO.
	     PERFORM RESTORE-SCREEN.

       AA505.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SHARED.WR.

       COPY WARHSE.WR.

       COPY WSTOCK.WR.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.
010970 AY051.
010980	     MOVE 51		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-WARHSE-LOCK THRU READ-WARHSE-EXIT.
011000	     MOVE WAR-RECORD1	 TO REC-WARHSE.
011010       GO TO AY50.
010970 AY052.
010980	     MOVE 52		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-WSTOCK-LOCK THRU READ-WSTOCK-EXIT.
011000	     MOVE WST-RECORD1	 TO REC-WSTOCK.
011010       GO TO AY50.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.
011250 AY50.
011260	     ADD 1		 TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "ST"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

011860 AY999.
011870       EXIT.
      *
014530 CA175-ADDRESS	 SECTION.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560                           INTO W40-ADD1 
                                      W40-ADD2 
                                      W40-ADD3 
                                      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
015170 CA600	     SECTION.
015180 CA601.
013310	     MOVE 6		 TO W50-LENGTH.

       CA605.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
	       GO TO CA605.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.

       CA606.
013380	     MOVE W50-DENT	 TO WAR-CODE.

       CA610.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA699.
             EXIT.
      *
      *    ****    D E L E T E	 W A R E H O U S E   R E C O R D
      *
018130 CB000		 SECTION 50.
018140 CB00.
018150	     DISPLAY CLR-SCREEN.
	     DISPLAY S01.
021780	   IF LS-OK = "N"
               PERFORM AA500
021790	       GO TO CB999.
	     MOVE SPACES	 TO W40-NAME1 W40-CONTACT
				    W40-CODE  W40-ADDRESS1
				    W40-TEL   W40-FAX.
003230	     MOVE ZERO		 TO W40-POST.
	     DISPLAY S03.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Warehouse Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       CB05.
	     ACCEPT S02 AT 0415.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CB999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO CB05
		 WHEN F2-KEY
016560		     PERFORM AA50-LOOKUP
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CB05
	       END-EVALUATE
	       DISPLAY S02 AT 0415
015830	       IF W40-CODE = SPACE
015840		   GO TO CB05.
019250	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
015830	   IF W40-CODE = SPACE
015840	       GO TO CB999.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
019250	     DISPLAY CLEAR-L25.
019490	   IF W40-CODE = SPACE
019500	       GO TO CB999.

       CB10.
	     PERFORM CA600.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50-LOOKUP
018630	       DISPLAY S02 AT 0415
018640	       IF W40-CODE = SPACES
018720		   GO TO CB05
               ELSE
		   GO TO CB10.

       CB15.
	     MOVE WAR-CODE	 TO W40-CODE.
019840	     MOVE WAR-NAME	 TO W40-NAME.
	     MOVE WAR-CONTACT	 TO W40-CONTACT.
019850	     MOVE WAR-ADDRESS	 TO W60-ADDRESS.
020540	     MOVE SPACES	 TO W40-ADDRESS1.
019870	     MOVE WAR-POST	 TO W40-POST.
019900	     MOVE WAR-TEL	 TO W40-TEL.
019900	     MOVE WAR-FAX	 TO W40-FAX.
	     PERFORM CA175-ADDRESS.
	     DISPLAY S03.
	     MOVE 16		 TO SLIN.
	     PERFORM CHECK-SETUP THRU CHECK-EXIT.
015980	   IF WS-OPTION = "N"
	       GO TO CB00.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1621
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1721
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     " Checking for stock at this warehouse "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			      BACKGROUND-COLOR 0.
	     DISPLAY "À" AT 1821
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			      BACKGROUND-COLOR 0.
	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 1923
		      WITH FOREGROUND-COLOR 0 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     INITIALIZE WST-RECORD1.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE "!"		 TO WST-EXT-CODE.
	     PERFORM START-AT-WHOUSE THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB25.

       CB20.
	     PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB25.
	   IF WST-WAR > W40-CODE
	       GO TO CB25.
	   IF WST-QUANT = ZERO
	       GO TO CB20.
	     PERFORM RESTORE-SCREEN.
	     MOVE "May not delete - Existing stock"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO CB00.

       CB25.
	     PERFORM RESTORE-SCREEN.
021960       PERFORM AY60 THRU AY999.
	     MOVE 2		 TO WS-ACTION.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1621
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 1721
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " Deleting linked record                "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			      BACKGROUND-COLOR 0.
	     DISPLAY "À" AT 1821
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "°°" WITH FOREGROUND-COLOR 0 HIGHLIGHT
			      BACKGROUND-COLOR 0.
	     DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 1923
		      WITH FOREGROUND-COLOR 0 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     INITIALIZE WST-RECORD1.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE "!"		 TO WST-EXT-CODE.

       CB30.
	     PERFORM START-AT-WHOUSE THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB35.
	     PERFORM READ-WSTOCK-NEXT-LOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO CB35.
	   IF WST-WAR > W40-CODE
	       GO TO CB35.
	     DISPLAY WST-CODE AT 1746
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM AY052 THRU AY59.
	     PERFORM DELETE-WSTOCK-REC THRU WRITE-WSTOCK-EXIT.
	     GO TO CB30.

       CB35.
	     PERFORM RESTORE-SCREEN.
	     PERFORM AY051 THRU AY59.
	     PERFORM DELETE-WARHSE-REC THRU WRITE-WARHSE-EXIT.
	     MOVE "Warehouse deleted - Press any key or 'E'nd"
				 TO WS-ERR-MES.
	     MOVE 17		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "E")
	       GO TO CB00.

       CB999.
	     EXIT.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
044780	     GO TO ZA999-EXIT.
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
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
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
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041840	   ELSE
	   IF WS-F-ERROR = 7
	       MOVE W02-DEPART	 TO WS-FILE
	       MOVE ZERO	 TO WS-KEY
	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
