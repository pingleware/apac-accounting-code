      ******************************************************************
      *                                                                *
      *    ******   ********  *******	*******    ******    ******    *
      *   **	**     **     **    **	**    **  **	**  **	  **   *
      *   **	       **     **    **	**    **  **	**  **	       *
      *    ******      **     *******	*******   **	**   ******    *
      * 	**     **     **	**    **  **	**	  **   *
      *   **	**     **     **	**    **  **	**  **	  **   *
      *    ******      **     **	*******	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    B O S A L   S T O C K   F I L E   C R E A T E - P R O G     *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPBOS.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
                       CURSOR IS CSTART
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY STOCK.SL.

	   SELECT BOSAL  ASSIGN DISK
000190                    ACCESS SEQUENTIAL
000200                    ORGANIZATION LINE SEQUENTIAL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FD.

000300 COPY STOCK.GFD.

000290 FD  BOSAL     LABEL RECORD STANDARD
000300		     VALUE OF FILE-ID W02-BOSAL.
000310 01  PRC-RECORD1.
000040	   03  PRC-CODE        PIC  X(14).
000040	   03  PRC-XREF        PIC  X(14).
	   03  PRC-DESC        PIC  X(30).
	   03  PRC-LEDG        PIC  9(04).
000110	   03  PRC-COST        PIC S9(05)V99 SIGN TRAILING SEPARATE.
000120	   03  PRC-SELL        PIC S9(05)V99 SIGN TRAILING SEPARATE.
000130	   03  PRC-CASH        PIC S9(05)V99 SIGN TRAILING SEPARATE.

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
001840 77  WS-DEPKEY        PIC 9(04)     COMP-5.
001860 77  WS-PARKEY        PIC 9(04)     COMP-5.
001880 77  WS-NETKEY        PIC 9(04)     COMP-5.
       77  WS-PRCKEY        PIC 9(04)     COMP-5.
       77  WS-RECKEY        PIC 9(04)     COMP-5.
       77  WS-TRFKEY        PIC 9(04)     COMP-5 VALUE 1.
001920 77  WS-KEYSTR        PIC 9(04)     COMP-5.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001970 77  WS-KSTORE        PIC 9(04)     COMP-5.
001980 77  WS-ITM           PIC X(12).
001990 77  WS-ITM1          PIC X(12).
002020 77  WS-OPTION        PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002030 77  WS-LENGTH        PIC 9(04) VALUE 1.
002070 77  WS-ERROR         PIC 9(01).
002080 77  WS-IND           PIC 9(01)     VALUE 0.
002100 77  WS-INDP          PIC 9(01)     VALUE 0.
002110 77  WS-INDS          PIC 9(01)     VALUE 0.
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002150 77  WS-LEDG          PIC 9(03).
002160 77  WS-LEDG1         PIC 9(03).
002170 77  WS-SELECT        PIC X(13)     VALUE "Select Option".
002180 77  WS-CHAR1         PIC X(01).
002190 77  WS-CHAR2         PIC X(01).
002200 77  WS-COMP          PIC 9(01).
002220 77  WS-TYPE          PIC X(01).
002240 77  WS-INDEX-STORE   PIC X(12).
002250 77  WS-INDEX-NAME    PIC X(12).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
002290 77  WS-OK            PIC X(01) VALUE "N".
002300 77  WS-CHK           PIC X(33)      VALUE
002310                    "ENTER if correct - N if incorrect".
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
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

002330 COPY WS.WS.

000010 01  WS-USER-ID.
000020     03  WS-USER.
000030         05  WS-USE      PIC  9(03).
000040     03  WS-COMPANY.
000050         05  WS-COMPCHR  PIC  X(01) OCCURS 40.
000060     03  WS-USUB         PIC  9(04) COMP-5.
000070     03  WS-DRAW         PIC  9(02) VALUE 0.
000080     03  WS-CSS          PIC  X(01) VALUE "N".
000090     03  WS-OK           PIC  X(01) VALUE "N".
000100     03  WS-RET          PIC  X(01) VALUE " ".
000290 01  WS-PARID.
000310     03  FILLER          PIC  X(05) VALUE "PARM_".
000320     03  WS-EXT15        PIC  X(03) VALUE "CSS".
000330     03  FILLER          PIC  X(04) VALUE ".APC".
000340 01  WS0-PROGRAMS.
000350     03  WS0-MODULES.
000360         05  WS0-DTP     PIC  9(01).
000370         05  WS0-STP     PIC  9(01).
000380         05  WS0-HPD     PIC  9(01).
000390         05  WS0-CRP     PIC  9(01).
000400         05  WS0-GLP     PIC  9(01).
000410         05  WS0-POS     PIC  9(01).
000420         05  WS0-JCP     PIC  9(01).
000430         05  WS0-VHP     PIC  9(01).
000440         05  WS0-WAG     PIC  9(01).
000450         05  WS0-SAL     PIC  9(01).
000460         05  FILLER      PIC  X(10).
000470     03  WS0-PRGM  REDEFINES WS0-MODULES.
000480         05  WS0-PMODS   PIC  X(20).
000490                               
002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000120 COPY DEPART.ID.

       COPY STOCK.ID.

       COPY STKTRF.ID.

002440	   03  W02-BOSAL.
	       05  W02-BFILE   PIC X(24) VALUE SPACES.

       COPY W10.STK.

003280 01  W15-RECORD1.
003290	   03  W15-CODE       PIC  X(14).
003600     03  W15-XREF       PIC  X(14).
003540	   03  W15-LEDG       PIC  9(03)    COMP.
003310	   03  W15-DESC.
               05 W15-DKEY    PIC  X(14).
               05 FILLER      PIC  X(16).
	   03  W15-IND	      PIC  X(01).
	   03  W15-UNT	      PIC  X(01).
	   03  W15-ICOST      PIC  X(08).
	   03  W15-ISELL      PIC  X(08).
	   03  W15-ICASH      PIC  X(08).
003340     03  W15-COST       PIC S9(06)V999 COMP-3.
003350     03  W15-SELL       PIC S9(07)V99 COMP-3.
003360     03  W15-CASH       PIC S9(07)V99 COMP-3.
003360     03  W15-WSALE      PIC S9(07)V99 COMP-3.
003370	   03  W15-AVGE       PIC S9(06)V999 COMP-3.

003610 01  W20-DATE.
003620     03  W20-YY.
003630         05  W20-YEAR   PIC 9(02).
003640     03  W20-MM.
003650         05  W20-MONTH  PIC 9(02).
003660     03  W20-DD.
003670         05  W20-DAY    PIC 9(02).
003680 01  W20-DTE1 REDEFINES W20-DATE.
003690     03  W20-DTE        PIC 9(06).
003700 01  W25-CALCS.
003710     03  W25-RESULT     PIC 9(02)V99.
003720     03  W25-RESULT1 REDEFINES W25-RESULT.
003730         05  W25-WHOLE  PIC 9(02).
003740         05  W25-DECIMAL PIC 9(02).
003750     03  W25-SCOST      PIC S9(07)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(07)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(07)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(07)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(07)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(07)V99 COMP-3.
       01  W41-SAVE.
           03  W41-LENGTH      PIC 9(04) COMP-X VALUE 2000.
           03  W41-START       PIC 9(04) COMP-X VALUE 1.
           03  W41-BUFFER      PIC 9(04) COMP-X VALUE 1.
       01  W42-ATTRIB.
           03  FILLER          PIC X(2000).
       01  W42-ATTRIB2.
           03  FILLER          PIC X(2000).
       01  W43-SCREEN.
           03  FILLER          PIC X(2000).
003570 01  W43-SCREEN2.
003580     03  FILLER.
               05  W43-LINE    OCCURS 25.
                   07  W43-DET PIC  X(80).
       01  W44-FUNCTION        PIC 9(02)     COMP-X.

       01  W50-FUNCTIONS.
           03  W50-KEY         PIC 9(02)     COMP-X.
           03  W50-ENTRIES OCCURS 12.
               05  W50-LNGTH   PIC 9(02)     COMP-X.
               05  W50-CODE.
                   07  W50-DEC PIC 9(04)     COMP-0.

003800 COPY W50.WS.

       01  W50-BIT-PAIRS       PIC  9(02)    COMP-X VALUE 1.
       01  W50-PARAMETERS.
           03  W50-SETTING     PIC  9(02)    COMP-X.
           03  FILLER          PIC  X(01)    VALUE "2".
           03  W50-NUMBER      PIC  9(02)    COMP-X.
           03  FILLER          PIC  9(02)    COMP-X VALUE 1.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.
003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000         05  W80-DATE   PIC Z9/99/99.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
      /
004130 SCREEN SECTION.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
004140 01  CLR-SCREEN.
004150     03  BLANK SCREEN FOREGROUND-COLOR 3.
003860     03  LINE  1 COLUMN  1 PIC X(80) USING WS-TOP-LNE2.
003870     03  LINE  2 COLUMN  1 PIC X(01) USING WS-G3.
004160     03          COLUMN  2 FOREGROUND-COLOR 12
004170                           VALUE "APAC Accounting".
004180	   03	       COLUMN 68 FOREGROUND-COLOR 13
004190				 VALUE "Version 6.61".
003920     03          COLUMN 80 PIC X(01) USING WS-G3.
003930     03  LINE  3 COLUMN  1 PIC X(01) USING WS-G3.
003940     03          COLUMN 80 PIC X(01) USING WS-G3.
003950     03  LINE  4 COLUMN  1 PIC X(01) USING WS-G3.
003960     03          COLUMN 80 PIC X(01) USING WS-G3.
003970     03  LINE  5 COLUMN  1 PIC X(01) USING WS-G3.
003980     03          COLUMN 80 PIC X(01) USING WS-G3.
003990     03  LINE  6 COLUMN  1 PIC X(01) USING WS-G3.
004000     03          COLUMN 80 PIC X(01) USING WS-G3.
004010     03  LINE  7 COLUMN  1 PIC X(01) USING WS-G3.
004020     03          COLUMN 80 PIC X(01) USING WS-G3.
004030     03  LINE  8 COLUMN  1 PIC X(01) USING WS-G3.
004040     03          COLUMN 80 PIC X(01) USING WS-G3.
004050     03  LINE  9 COLUMN  1 PIC X(01) USING WS-G3.
004060     03          COLUMN 80 PIC X(01) USING WS-G3.
004070     03  LINE 10 COLUMN  1 PIC X(01) USING WS-G3.
004080     03          COLUMN 80 PIC X(01) USING WS-G3.
004090     03  LINE 11 COLUMN  1 PIC X(01) USING WS-G3.
004100     03          COLUMN 80 PIC X(01) USING WS-G3.
004110     03  LINE 12 COLUMN  1 PIC X(01) USING WS-G3.
004120     03          COLUMN 80 PIC X(01) USING WS-G3.
004130     03  LINE 13 COLUMN  1 PIC X(01) USING WS-G3.
004140     03          COLUMN 80 PIC X(01) USING WS-G3.
004150     03  LINE 14 COLUMN  1 PIC X(01) USING WS-G3.
004160     03          COLUMN 80 PIC X(01) USING WS-G3.
004170     03  LINE 15 COLUMN  1 PIC X(01) USING WS-G3.
004180     03          COLUMN 80 PIC X(01) USING WS-G3.
004190     03  LINE 16 COLUMN  1 PIC X(01) USING WS-G3.
004200     03          COLUMN 80 PIC X(01) USING WS-G3.
004210     03  LINE 17 COLUMN  1 PIC X(01) USING WS-G3.
004220     03          COLUMN 80 PIC X(01) USING WS-G3.
004230     03  LINE 18 COLUMN  1 PIC X(01) USING WS-G3.
004240     03          COLUMN 80 PIC X(01) USING WS-G3.
004250     03  LINE 19 COLUMN  1 PIC X(01) USING WS-G3.
004260     03          COLUMN 80 PIC X(01) USING WS-G3.
004270     03  LINE 20 COLUMN  1 PIC X(01) USING WS-G3.
004280     03          COLUMN 80 PIC X(01) USING WS-G3.
004290     03  LINE 21 COLUMN  1 PIC X(01) USING WS-G3.
004300     03          COLUMN 80 PIC X(01) USING WS-G3.
004310     03  LINE 22 COLUMN  1 PIC X(01) USING WS-G3.
004320     03          COLUMN 80 PIC X(01) USING WS-G3.
004330     03  LINE 23 COLUMN  1 PIC X(01) USING WS-G3.
004340     03          COLUMN 80 PIC X(01) USING WS-G3.
004350     03  LINE 24 COLUMN  1 PIC X(80) USING WS-BOT-LNE2.
      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
             MOVE 1 TO WS-S1.
             MOVE 59 TO WS-STAT2.
       AA02.
             MOVE 2 TO W50-LNGTH (WS-S1).
             MOVE WS-STAT2 TO W50-DEC (WS-S1).
           IF WS-S1 < 10
               ADD 1 TO WS-S1 WS-STAT2
               GO TO AA02.
036630       MOVE 133 TO WS-STAT2.
036650       MOVE 2 TO W50-LNGTH (11).
036660       MOVE WS-STAT2 TO W50-DEC (11).
036630       MOVE 134 TO WS-STAT2.
036650       MOVE 2 TO W50-LNGTH (12).
036660       MOVE WS-STAT2 TO W50-DEC (12).
             CALL X"B0" USING 0, W50-FUNCTIONS.
             PERFORM BA000.
007790       GO TO AZ000-EOJ.

      *
      *    ****    D I S P L A Y   R E C O R D   L O C K E D
      *            M E S S A G E   -   D E L A Y   F O R   
      *            C O U N T   O F   2 5 0 0
      *
000010 LOCKED-RECORD      SECTION.
000020 LOCK-REC.
000030       DISPLAY SPACE AT 2550.
000040       DISPLAY "Record LOCKED - " AT 2551 
000050                WITH FOREGROUND-COLOR 14
000060               "waiting" WITH FOREGROUND-COLOR 14 BLINK.
000070       MOVE ZERO TO WS-WAIT.
000080       GO TO LOCK-REC-LOOP.

000100 LOCK-USERS-REC.
000110       DISPLAY SPACE AT 2550.
000120       DISPLAY "waiting" AT 2551 
000130                WITH FOREGROUND-COLOR 14 BLINK.
000140       MOVE -3771          TO WS-WAIT.
000150
000160 LOCK-REC-LOOP.
000170     IF WS-WAIT < 2500
000180         ADD 1 TO WS-WAIT
000190         GO TO LOCK-REC-LOOP.
000200       DISPLAY SPACE AT 2550.
000210
000220 LOCK-REC-EXIT.
000230       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
000010 READ-STOCK.
000020       READ STOCK
000030         KEY IS STK-CODE.
000040     IF WS-STATUS = "00"
000050         MOVE ZERO         TO WS-F-ERROR
000060         GO TO READ-STOCK-EXIT.
000070     IF WS-STATUS = "23"
000080         MOVE 22           TO WS-F-ERROR
000090         GO TO READ-STOCK-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110         MOVE 22           TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000130     IF RECORD-LOCKED
000140         PERFORM LOCKED-RECORD
000150         GO TO READ-STOCK.
000160       GO TO READ-STOCK-EXIT.

000180 READ-STOCK-LOCK.
000190       READ STOCK WITH KEPT LOCK
000200         KEY IS STK-CODE.
000210     IF WS-STATUS = "00"
000220         MOVE ZERO         TO WS-F-ERROR
000230         GO TO READ-STOCK-EXIT.
000240     IF WS-STATUS = "23"
000250         MOVE 22           TO WS-F-ERROR
000260         GO TO READ-STOCK-EXIT.
000270     IF WS-STAT1 = "2" OR "3" OR "4"
000280         MOVE 22           TO WS-F-ERROR
000290         PERFORM READ-ERROR.
000300     IF RECORD-LOCKED
000310         PERFORM LOCKED-RECORD
000320         GO TO READ-STOCK-LOCK.
000330     IF TOO-MANY-LOCKS
000340         UNLOCK STOCK
000350         GO TO READ-STOCK-LOCK.
000360       GO TO READ-STOCK-EXIT.

000380 READ-STOCK-NEXT.
000390       READ STOCK NEXT.
000400     IF WS-STATUS = "00"
000410         MOVE ZERO         TO WS-F-ERROR
000420         GO TO READ-STOCK-EXIT.
000430     IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450         MOVE 22           TO WS-F-ERROR
000460         GO TO READ-STOCK-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480         MOVE 22           TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000500     IF RECORD-LOCKED
000510         PERFORM LOCKED-RECORD
000520         GO TO READ-STOCK-NEXT.
000530       GO TO READ-STOCK-EXIT.

000550 READ-STOCK-NEXT-LOCK.
000560       READ STOCK NEXT WITH KEPT LOCK.
000570     IF WS-STATUS = "00"
000580         MOVE ZERO         TO WS-F-ERROR
000590         GO TO READ-STOCK-EXIT.
000600     IF (WS-STATUS = "23") OR
000610        (WS-STAT1 = "1")
000620         MOVE 22           TO WS-F-ERROR
000630         GO TO READ-STOCK-EXIT.
000640     IF WS-STAT1 = "2" OR "3" OR "4"
000650         MOVE 22           TO WS-F-ERROR
000660         PERFORM READ-ERROR.
000670     IF RECORD-LOCKED
000680         PERFORM LOCKED-RECORD
000690         GO TO READ-STOCK-NEXT-LOCK.
000700     IF TOO-MANY-LOCKS
000710         UNLOCK STOCK
000720         GO TO READ-STOCK-NEXT-LOCK.
000730       GO TO READ-STOCK-EXIT.

000750 START-AT-STOCK-CODE.
000760       START STOCK 
000770         KEY >= STK-CODE.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800         GO TO READ-STOCK-EXIT.
000810     IF WS-STATUS = "23"
000820         MOVE 22           TO WS-F-ERROR
000830         GO TO READ-STOCK-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850         MOVE 22           TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870       GO TO READ-STOCK-EXIT.

000890 START-AT-ALT-CODE.
000900       START STOCK
000910         KEY >= STK-ACODE.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940         GO TO READ-STOCK-EXIT.
000950     IF WS-STATUS = "23"
000960         MOVE 22           TO WS-F-ERROR
000970         GO TO READ-STOCK-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990         MOVE 22           TO WS-F-ERROR
001000         PERFORM READ-ERROR.
001010       GO TO READ-STOCK-EXIT.

001030 START-AT-STOCK-BIN.
001040       START STOCK
001050         KEY >= STK-BSEQ.
001060     IF WS-STATUS = "00"
001070         MOVE ZERO         TO WS-F-ERROR
001080         GO TO READ-STOCK-EXIT.
001090     IF WS-STATUS = "23"
001100         MOVE 22           TO WS-F-ERROR
001110         GO TO READ-STOCK-EXIT.
001120     IF WS-STAT1 = "2" OR "3" OR "4"
001130         MOVE 22           TO WS-F-ERROR
001140         PERFORM READ-ERROR.
001150       GO TO READ-STOCK-EXIT.

001170 START-AT-STOCK-LEDG.
001180       START STOCK
001190         KEY >= STK-LSEQ.
001200     IF WS-STATUS = "00"
001210         MOVE ZERO         TO WS-F-ERROR
001220         GO TO READ-STOCK-EXIT.
001230     IF WS-STATUS = "23"
001240         MOVE 22           TO WS-F-ERROR
001250         GO TO READ-STOCK-EXIT.
001260     IF WS-STAT1 = "2" OR "3" OR "4"
001270         MOVE 22           TO WS-F-ERROR
001280         PERFORM READ-ERROR.
001290       GO TO READ-STOCK-EXIT.

001310 START-AT-STOCK-DESC.
001320       START STOCK
001330         KEY >= STK-DKEY.
001340     IF WS-STATUS = "00"
001350         MOVE ZERO         TO WS-F-ERROR
001360         GO TO READ-STOCK-EXIT.
001370     IF WS-STATUS = "23"
001380         MOVE 22           TO WS-F-ERROR
001390         GO TO READ-STOCK-EXIT.
001400     IF WS-STAT1 = "2" OR "3" OR "4"
001410         MOVE 22           TO WS-F-ERROR
001420         PERFORM READ-ERROR.
001430       GO TO READ-STOCK-EXIT.

001450 START-AT-STOCK-XREF.
001460       START STOCK
001470         KEY >= STK-XREF.
001480     IF WS-STATUS = "00"
001490         MOVE ZERO         TO WS-F-ERROR
001500         GO TO READ-STOCK-EXIT.
001510     IF WS-STATUS = "23"
001520         MOVE 22           TO WS-F-ERROR
001530         GO TO READ-STOCK-EXIT.
001540     IF WS-STAT1 = "2" OR "3" OR "4"
001550         MOVE 22           TO WS-F-ERROR
001560         PERFORM READ-ERROR.

001580 READ-STOCK-EXIT.
001590       EXIT.
      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
000010 REWRITE-STOCK.
000020       REWRITE STK-RECORD1.
000030     IF WS-STAT1 NOT = "0"
000040         MOVE 22           TO WS-F-ERROR
000050         PERFORM WRITE-ERROR.
000060       GO TO WRITE-STOCK-EXIT.
000070
000080 REWRITE-STOCK-UNLOCK.
000090       REWRITE STK-RECORD1.
000100     IF WS-STAT1 NOT = "0"
000110         MOVE 22           TO WS-F-ERROR
000120         PERFORM WRITE-ERROR.
000130       UNLOCK STOCK.
000140       GO TO WRITE-STOCK-EXIT.
000150
000160 DELETE-STOCK-REC.
000170       DELETE STOCK.
000180     IF WS-STAT1 NOT = "0"
000190         MOVE 22           TO WS-F-ERROR
000200         PERFORM WRITE-ERROR.
000210       UNLOCK STOCK.
000220       GO TO WRITE-STOCK-EXIT.
000230
000240 WRITE-STOCK.
000250       WRITE STK-RECORD1.
000260     IF WS-STAT1 NOT = "0"
000270         MOVE 22           TO WS-F-ERROR
000280         PERFORM WRITE-ERROR.
000290       CLOSE STOCK.
000300       OPEN I-O STOCK.
000310
000320 WRITE-STOCK-EXIT.
000330       EXIT.
      /
012350 AZ000-END               SECTION.
012360 AZ000-EOJ.
012370       CLOSE STOCK
		   BOSAL.
012440 AZ010.
             STOP RUN.

012510 BA000           SECTION 1.
012520 BA00.
012530       DISPLAY CLR-SCREEN. 
             DISPLAY "Updating Stock Prices" AT 0230
                      WITH FOREGROUND-COLOR 9.
             DISPLAY "UPDPRC Record :" AT 0603.
             DISPLAY "Stock Details :" AT 0803.
             MOVE ZERO TO WS-PRCKEY WS-RECKEY.
       BA05.
           IF WS-RECKEY = 100
               MOVE ZERO TO WS-RECKEY
012370	       CLOSE STOCK
012370	       OPEN I-O STOCK.
	     READ BOSAL AT END
                         GO TO BA10.
           IF PRC-CODE = SPACES
               GO TO BA05.
             ADD 1 TO WS-PRCKEY WS-RECKEY.
             MOVE WS-PRCKEY TO W80-REC.
             DISPLAY W80-NO AT 0619 WITH FOREGROUND-COLOR 11.
             DISPLAY PRC-RECORD1 AT 0819 WITH FOREGROUND-COLOR 15.
             MOVE PRC-CODE TO W10-ITEM.
             PERFORM CA500.
           IF WS-F-ERROR = ZERO
               IF ((PRC-COST = ZERO) OR (PRC-COST = STK-COST)) AND 
		   (PRC-LEDG = STK-LEDG) AND (PRC-XREF = STK-XREF)
                   UNLOCK STOCK 
                   GO TO BA05
               ELSE
                   PERFORM DB000
                   GO TO BA05.
             PERFORM DA000.
             GO TO BA05.
       BA10.
             DISPLAY "Update complete - Press " AT 2312 
                      WITH FOREGROUND-COLOR 14 "ENTER" 
                      WITH FOREGROUND-COLOR 15.
             ACCEPT WS-OPTION AT 2342 WITH FOREGROUND-COLOR 15 AUTO.
       BA999.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
015170 CA500         SECTION.
015180 CA501.
013310       MOVE 14             TO W50-LENGTH.
       CA505.
013320       MOVE W10-ITEM       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-ITEM
               GO TO CA505.      
013330       CALL "_MSUPCASE" USING W50-DATA
013340                        LENGTH W50-DATA
013350                               W50-LENGTH
013360                        LENGTH W50-LENGTH
013370                               W50-STATUS.
013380       MOVE W50-ITEM       TO W10-ITEM.
013380       MOVE W50-ITEM       TO STK-CODE.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
       CA599.
             EXIT.
      /
      *    ****   C R E A T E   N E W   S T O C K
      *
015670 DA000           SECTION 50.
015680 DA00.
             MOVE "001"          TO W10-BIN.
             MOVE 1              TO W10-TAX.
	     MOVE PRC-XREF	 TO W10-CODE2.
             MOVE PRC-DESC       TO W10-DESC.
015760	     MOVE SPACES	 TO W10-ACODE.
             MOVE PRC-COST       TO W10-COST W10-AVRG.
	     MOVE PRC-SELL	 TO W10-SELL W10-WSALE.
	     MOVE PRC-CASH	 TO W10-CASH.
             MOVE PRC-LEDG       TO W10-LEDG.
015780       MOVE ZERO           TO W10-QUANT W10-SELL W10-MARKUP 
015790                              W10-ORDER W10-CASH W10-CMARKUP 
                                    W10-PURCH W10-ODTE W10-WMARKUP
                                    W10-REORD W10-GUAR W10-LTIME 
				    W10-IND.
016260 DA20.
             INITIALIZE STK-RECORD1.
016670       MOVE W10-ITEM       TO STK-CODE WS-ITM.
016680       MOVE W10-BIN        TO STK-BIN.
016690       MOVE W10-DESC       TO STK-DESC.
016700       MOVE W10-TAX        TO STK-TAX.
016730       PERFORM DA50.
016740       MOVE W10-MARKUP     TO STK-MARKUP.
016750       MOVE W10-QUANT      TO STK-QUANT.
016760       MOVE W10-REORD      TO STK-LEVEL.
016770       MOVE W10-LTIME      TO STK-LTIME.
016780       MOVE W10-COST       TO W10-CCOST.
016790       MOVE W10-SELL       TO W10-CSELL.
016800     IF W10-COST = ZERO
016810         IF W10-SELL NOT = ZERO
016820             COMPUTE W10-CCOST ROUNDED = (W10-CSELL / 
016830                     (100 + W10-MARKUP) * 100).
016840     IF W10-SELL = ZERO
016850         IF W10-COST NOT = ZERO
016860             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + 
016870                     ((W10-CCOST * W10-MARKUP) / 100)).
016880       MOVE W10-CCOST      TO W10-COST.
016890       MOVE W10-CSELL      TO W10-SELL.
016900       MOVE W10-COST       TO STK-COST.
016910       MOVE W10-SELL       TO STK-SELL.
016920       MOVE W10-LEDG       TO STK-LEDG.
             MOVE W10-GUAR       TO STK-GUAR.
016930       MOVE W10-ORDER      TO STK-ORD.
016940       MOVE W10-ODTE       TO STK-DTE.
             MOVE W10-PURCH      TO STK-PUR.
016980       MOVE W10-AVRG       TO STK-AVGE.
016990       MOVE W10-IND        TO STK-IND.
016730       PERFORM DA52.
017000     IF (W10-CMARKUP = ZERO) AND
017010        (W10-CASH = ZERO)
017020         MOVE W10-MARKUP TO W10-CMARKUP
017030         MOVE W10-SELL TO W10-CASH
017040         GO TO DA21.
017050     IF W10-CMARKUP = ZERO
017060         MOVE W10-CASH TO W10-CSELL
017070         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-CASH = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-CMARKUP) / 100))
017120         MOVE W10-CSELL TO W10-CASH.
017130 DA21.
017140       MOVE W10-CMARKUP TO STK-CMARKUP.
017150       MOVE W10-CASH TO STK-CASH.
             PERFORM DA54.
017000     IF (W10-WMARKUP = ZERO) AND
017010        (W10-WSALE = ZERO)
017020         MOVE W10-CMARKUP TO W10-WMARKUP
017030         MOVE W10-CASH TO W10-WSALE
017040         GO TO DA21D.
017050     IF W10-WMARKUP = ZERO
017060         MOVE W10-WSALE TO W10-CSELL
017070         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-WSALE = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-WMARKUP) / 100))
017120         MOVE W10-CSELL TO W10-WSALE.
       DA21D.
017140       MOVE W10-WMARKUP    TO STK-WMARKUP.
017150       MOVE W10-WSALE      TO STK-WSALE.
017160       MOVE W10-CODE2      TO STK-XREF.
017170       MOVE W10-ACODE      TO STK-ACODE.
017190       PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
017230       MOVE STK-RECORD1 TO W15-RECORD1.
017500       GO TO DA999.
      *
      *    ****  D E F A U L T   M A R K U P
      *  
017540 DA50.
017600     IF W10-SELL > ZERO AND W10-COST > ZERO
017610         MOVE W10-SELL     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					     / W10-COST) * 100).
       DA51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *  
       DA52.
017600     IF W10-CASH > ZERO AND W10-COST > ZERO
017610         MOVE W10-CASH     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					      / W10-COST) * 100).
       DA53.
017660       EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *  
       DA54.
017600     IF W10-WSALE > ZERO AND W10-COST > ZERO
017610         MOVE W10-WSALE    TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640					      / W10-COST) * 100) .
017650 DA55.
017660       EXIT.
019380 DA999.
019390       EXIT.
       DB000           SECTION 50.
       DB00.
           IF PRC-COST = ZERO
               GO TO DB05.
             MOVE PRC-COST TO STK-COST.
016860       COMPUTE STK-SELL ROUNDED = (PRC-COST + 
016870                        ((PRC-COST * STK-MARKUP) / 100)).
016860       COMPUTE STK-CASH ROUNDED = (PRC-COST + 
016870                        ((PRC-COST * STK-CMARKUP) / 100)).
016860       COMPUTE STK-WSALE ROUNDED = (PRC-COST + 
016870                         ((PRC-COST * STK-WMARKUP) / 100)).
       DB05.
             MOVE PRC-LEDG       TO STK-LEDG.  
	     MOVE PRC-XREF	 TO STK-XREF.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
       DB999.
             EXIT.
      /
044130 ZA000-INIT              SECTION 80.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
044150       DISPLAY CLR-SCREEN.


	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.




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






044250	     MOVE "C"		 TO W02-DSK22.
044260	     MOVE "CSS"		 TO W02-EXT22.
	     DISPLAY "Input file name ?" AT 1212.
	     ACCEPT W02-BFILE AT 1230 WITH FOREGROUND-COLOR 15
					   UPDATE AUTO.
044300       OPEN I-O STOCK.
044310	   IF NOT (WS-STATUS = "00")
	       OPEN OUTPUT STOCK
	       CLOSE STOCK
	       OPEN I-O STOCK.
044360	     OPEN INPUT BOSAL.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
041220       MOVE 1 TO WS-S1.
021870       MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
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
                            WS-MCHR (1)  WS-MCHR (80) 
                            WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
             MOVE 44             TO W50-NUMBER.
             MOVE 1              TO W50-SETTING.
             CALL X"AF" USING W50-BIT-PAIRS W50-PARAMETERS.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2412
044810                WITH FOREGROUND-COLOR 14.
044820       DISPLAY "Press any key" AT 2512 
044830                WITH FOREGROUND-COLOR 14.
044840       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR             SECTION 81.
      
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
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
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
