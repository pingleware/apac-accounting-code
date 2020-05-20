      $SET GNT"STPBOS.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******  *******     ******    ******    *
      *   **    **     **     **    ** **    **   **    **  **    **   *
      *   **           **     **    ** **    **   **    **  **         *
      *    ******      **     *******  *******    **    **   ******    *
      *         **     **     **       **    **   **    **        **   *
      *   **    **     **     **       **    **   **    **  **    **   *
      *    ******      **     **       *******     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    B O S A L   S T O C K   F I L E   C R E A T E - P R O G     *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * August 2008   - New file (STKALT) - Stock alternate index      *
      *                 included for lookups, using any word con-      *
      *                 tained in the Stock description.               *
      *                                                                *
      * November 2009 - Include words from Description 2 and from      *
      *                 the Item code (Some item codes are comprised   *
      *                 of individual words and these will be          *
      *                 included in the alternate Index)               *
      *                                                                *
      * Jan 2010  - Allow for two discounts for a Debtor on the same   *
      *             Sales Ledger code. Included two additional Debtor  *
      *             discount codes (11 and 12) each with a field to    *
      *             define which debtor discount code uses this as an  *
      *             alternate discount. The relevant Stock Item will   *
      *             have an indicator field to instruct the system to  *
      *             use the alternate discount. The alternate discount *
      *             is only applicable if the discount code for which  *
      *             this alternate discount is applicable is the code  *
      *             on the Debtor account record.                      *
      *                                                                *
      * Feb 2017 - Increased the number of Memo entries, of 60 bytes,  *
      *            allowed from 12 to 34 per stock record.             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STPBOS.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   DECEMBER 1995.

     COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2017
         by James William Lemmon.

     All rights reserved.

       SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
           James William Lemmon.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                       CURSOR IS CSTART
                       CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "APACFIDS.SL".

       COPY "STOCK.SL".

    SELECT BOSAL  ASSIGN DISK
                          ACCESS SEQUENTIAL
                          ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY "APACFIDS.FD".

       COPY "STOCK.GFD".

       FD  BOSAL     LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-BOSAL.
       01  PRC-RECORD1.
          03  PRC-CODE        PIC  X(14).
          03  PRC-XREF        PIC  X(14).
          03  PRC-DESC        PIC  X(30).
          03  PRC-LEDG        PIC  9(04).
          03  PRC-COST        PIC S9(05)V99 SIGN TRAILING SEPARATE.
          03  PRC-SELL        PIC S9(05)V99 SIGN TRAILING SEPARATE.
          03  PRC-CASH        PIC S9(05)V99 SIGN TRAILING SEPARATE.

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-SUB     PIC 9(04)   COMP-5.
       77  WS-S1            PIC 9(04)     COMP-5.
       77  WS-S2            PIC 9(04)     COMP-5.
       77  WS-S3            PIC 9(04)     COMP-5.
       77  WS-S4            PIC 9(04)     COMP-5.
       77  WS-S5            PIC 9(04)     COMP-5.
       77  WS-S6            PIC 9(04)     COMP-5.
       77  WS-S7            PIC 9(04)     COMP-5.
       77  WS-S8            PIC 9(04)     COMP-5.
       77  WS-IXS           PIC 9(04)     COMP-5.
       77  WS-IXS1          PIC 9(04)     COMP-5.
       77  WS-IXS2          PIC 9(04)     COMP-5.
       77  WS-IXS3          PIC 9(04)     COMP-5.
       77  WS-IXS4          PIC 9(04)     COMP-5.
       77  WS-DEPKEY        PIC 9(04)     COMP-5.
       77  WS-PARKEY        PIC 9(04)     COMP-5.
       77  WS-NETKEY        PIC 9(04)     COMP-5.
       77  WS-PRCKEY        PIC 9(04)     COMP-5.
       77  WS-RECKEY        PIC 9(04)     COMP-5.
       77  WS-TRFKEY        PIC 9(04)     COMP-5 VALUE 1.
       77  WS-KEYSTR        PIC 9(04)     COMP-5.
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-KSTORE        PIC 9(04)     COMP-5.
       77  WS-ITM           PIC X(12).
       77  WS-ITM1          PIC X(12).
       77  WS-OPTION        PIC X(01).
       77  WS-SKIP          PIC X(01).
       77  WS-LENGTH        PIC 9(04) VALUE 1.
       77  WS-ERROR         PIC 9(01).
       77  WS-IND           PIC 9(01)     VALUE 0.
       77  WS-INDP          PIC 9(01)     VALUE 0.
       77  WS-INDS          PIC 9(01)     VALUE 0.
       77  WS-AMEND         PIC 9(01)     VALUE 0.
       77  WS-LEDG          PIC 9(03).
       77  WS-LEDG1         PIC 9(03).
       77  WS-SELECT        PIC X(13)     VALUE "Select Option".
       77  WS-CHAR1         PIC X(01).
       77  WS-CHAR2         PIC X(01).
       77  WS-COMP          PIC 9(01).
       77  WS-TYPE          PIC X(01).
       77  WS-INDEX-STORE   PIC X(12).
       77  WS-INDEX-NAME    PIC X(12).
       77  WS-PRINT         PIC 9(01).
       77  WS-PASS          PIC X(08).
       77  WS-OK            PIC X(01) VALUE "N".
       77  WS-CHK           PIC X(33)      VALUE
                          "ENTER if correct - N if incorrect".

      *    旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
      *    � Used by calling program (WORKING-STORAGE) and called program �
      *    � (LINKAGE SECTION) for Main screen layout and headings.       �
      *    읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP091".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -BOSAL CREATE STOCK FILE".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-USER-ID.
           03  WS-USER.
               05  WS-USE      PIC  9(03).
           03  WS-COMPANY.
               05  WS-COMPCHR  PIC  X(01) OCCURS 40.
           03  WS-USUB         PIC  9(04) COMP-5.
           03  WS-DRAW         PIC  9(02) VALUE 0.
           03  WS-CSS          PIC  X(01) VALUE "N".
           03  WS-OK           PIC  X(01) VALUE "N".
           03  WS-RET          PIC  X(01) VALUE " ".
       01  WS-PARID.
           03  FILLER          PIC  X(05) VALUE "PARM_".
           03  WS-EXT15        PIC  X(03) VALUE "CSS".
           03  FILLER          PIC  X(04) VALUE ".APC".
       01  WS0-PROGRAMS.
           03  WS0-MODULES.
               05  WS0-DTP     PIC  9(01).
               05  WS0-STP     PIC  9(01).
               05  WS0-HPD     PIC  9(01).
               05  WS0-CRP     PIC  9(01).
               05  WS0-GLP     PIC  9(01).
               05  WS0-POS     PIC  9(01).
               05  WS0-JCP     PIC  9(01).
               05  WS0-VHP     PIC  9(01).
               05  WS0-WAG     PIC  9(01).
               05  WS0-SAL     PIC  9(01).
               05  FILLER      PIC  X(10).
           03  WS0-PRGM  REDEFINES WS0-MODULES.
               05  WS0-PMODS   PIC  X(20).
      
       01  W02-FID.

       COPY "APACFIDS.ID".

       COPY "CONTROL.ID".

       COPY DEPART.ID.

       COPY "STOCK.ID".

       COPY STKTRF.ID.

          03  W02-BOSAL.
        05  W02-BFILE   PIC X(24) VALUE SPACES.

       COPY "W10.STK".

       01  W15-RECORD1.
          03  W15-CODE       PIC  X(14).
           03  W15-XREF       PIC  X(14).
          03  W15-LEDG       PIC  9(03)    COMP.
          03  W15-DESC.
               05 W15-DKEY    PIC  X(14).
               05 FILLER      PIC  X(16).
    03  W15-IND       PIC  X(01).
    03  W15-UNT       PIC  X(01).
    03  W15-ICOST      PIC  X(08).
    03  W15-ISELL      PIC  X(08).
    03  W15-ICASH      PIC  X(08).
           03  W15-COST       PIC S9(06)V999 COMP-3.
           03  W15-SELL       PIC S9(07)V99 COMP-3.
           03  W15-CASH       PIC S9(07)V99 COMP-3.
           03  W15-WSALE      PIC S9(07)V99 COMP-3.
          03  W15-AVGE       PIC S9(06)V999 COMP-3.

       01  W20-DATE.
           03  W20-YY.
               05  W20-YEAR   PIC 9(02).
           03  W20-MM.
               05  W20-MONTH  PIC 9(02).
           03  W20-DD.
               05  W20-DAY    PIC 9(02).
       01  W20-DTE1 REDEFINES W20-DATE.
           03  W20-DTE        PIC 9(06).
       01  W25-CALCS.
           03  W25-RESULT     PIC 9(02)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE  PIC 9(02).
               05  W25-DECIMAL PIC 9(02).
           03  W25-SCOST      PIC S9(07)V99 COMP-3.
           03  W25-PCOST      PIC S9(07)V99 COMP-3.
           03  W25-TSELL      PIC S9(07)V99 COMP-3.
           03  W25-TOTAL      PIC S9(07)V99 COMP-3.
           03  W25-VALUE      PIC S9(07)V99 COMP-3.
           03  W25-ADJUST     PIC S9(07)V99 COMP-3.
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
       01  W43-SCREEN2.
           03  FILLER.
               05  W43-LINE    OCCURS 25.
                   07  W43-DET PIC  X(80).
       01  W44-FUNCTION        PIC 9(02)     COMP-X.

       01  W50-FUNCTIONS.
           03  W50-KEY         PIC 9(02)     COMP-X.
           03  W50-ENTRIES OCCURS 12.
               05  W50-LNGTH   PIC 9(02)     COMP-X.
               05  W50-CODE.
                   07  W50-DEC PIC 9(04)     COMP-0.

       COPY "W50.WS".

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
       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE   PIC Z9/99/99.
           03  W80-QNT.
               05  W80-5      PIC Z(04)9.99.
           03  W80-QNT1 REDEFINES W80-QNT.
               05  W80-3V2    PIC Z(02)9.9999.
           03  W80-NO.
               05  W80-5N     PIC 9(05).
               05  W80-REC REDEFINES W80-5N
                              PIC ZZZZ9.
           03  W80-DEL.
               05  W80-2N      PIC 9(02).
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
       01  CLR-SCREEN.
           03  BLANK SCREEN FOREGROUND-COLOR 3.
           03  LINE  1 COLUMN  1 VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
           03  LINE  2 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN  2 FOREGROUND-COLOR 12
                                 VALUE "APAC Accounting".
          03        COLUMN 68 FOREGROUND-COLOR 13
           VALUE "Version 6.61".
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  3 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  4 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  5 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  6 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  7 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE  9 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 10 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 11 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 12 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 13 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 14 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 15 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 16 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 17 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 18 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 19 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 20 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 21 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 22 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 23 COLUMN  1 PIC X(01) USING WS-G3.
           03          COLUMN 80 PIC X(01) USING WS-G3.
           03  LINE 24 COLUMN  1 PIC X(80) USING WS-BOT-LNE2.
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION.
       AA000-MAIN              SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
             MOVE 1 TO WS-S1.
             MOVE 59 TO WS-STAT2.
       AA02.
             MOVE 2 TO W50-LNGTH (WS-S1).
             MOVE WS-STAT2 TO W50-DEC (WS-S1).
           IF WS-S1 < 10
               ADD 1 TO WS-S1 WS-STAT2
               GO TO AA02.
             MOVE 133 TO WS-STAT2.
             MOVE 2 TO W50-LNGTH (11).
             MOVE WS-STAT2 TO W50-DEC (11).
             MOVE 134 TO WS-STAT2.
             MOVE 2 TO W50-LNGTH (12).
             MOVE WS-STAT2 TO W50-DEC (12).
             CALL X"B0" USING 0, W50-FUNCTIONS.
             PERFORM BA000.
             GO TO AZ000-EOJ.

      *
      *    ****    D I S P L A Y   R E C O R D   L O C K E D
      *            M E S S A G E   -   D E L A Y   F O R
      *            C O U N T   O F   2 5 0 0
      *
       LOCKED-RECORD      SECTION.
       LOCK-REC.
             DISPLAY SPACE AT 2550.
             DISPLAY "Record LOCKED - " AT 2551
                      WITH FOREGROUND-COLOR 14
                     "waiting" WITH FOREGROUND-COLOR 14 BLINK.
             MOVE ZERO TO WS-WAIT.
             GO TO LOCK-REC-LOOP.

       LOCK-USERS-REC.
             DISPLAY SPACE AT 2550.
             DISPLAY "waiting" AT 2551
                      WITH FOREGROUND-COLOR 14 BLINK.
             MOVE -3771          TO WS-WAIT.
      
       LOCK-REC-LOOP.
           IF WS-WAIT < 2500
               ADD 1 TO WS-WAIT
               GO TO LOCK-REC-LOOP.
             DISPLAY SPACE AT 2550.
      
       LOCK-REC-EXIT.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
       READ-STOCK.
             READ STOCK
               KEY IS STK-CODE.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-STOCK.
             GO TO READ-STOCK-EXIT.

       READ-STOCK-LOCK.
             READ STOCK WITH KEPT LOCK
               KEY IS STK-CODE.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-STOCK-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK STOCK
               GO TO READ-STOCK-LOCK.
             GO TO READ-STOCK-EXIT.

       READ-STOCK-NEXT.
             READ STOCK NEXT.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-STOCK-NEXT.
             GO TO READ-STOCK-EXIT.

       READ-STOCK-NEXT-LOCK.
             READ STOCK NEXT WITH KEPT LOCK.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
           IF RECORD-LOCKED
               PERFORM LOCKED-RECORD
               GO TO READ-STOCK-NEXT-LOCK.
           IF TOO-MANY-LOCKS
               UNLOCK STOCK
               GO TO READ-STOCK-NEXT-LOCK.
             GO TO READ-STOCK-EXIT.

       START-AT-STOCK-CODE.
             START STOCK
               KEY >= STK-CODE.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-STOCK-EXIT.

       START-AT-ALT-CODE.
             START STOCK
               KEY >= STK-ACODE.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-STOCK-EXIT.

       START-AT-STOCK-BIN.
             START STOCK
               KEY >= STK-BSEQ.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-STOCK-EXIT.

       START-AT-STOCK-LEDG.
             START STOCK
               KEY >= STK-LSEQ.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-STOCK-EXIT.

       START-AT-STOCK-DESC.
             START STOCK
               KEY >= STK-DKEY.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-STOCK-EXIT.

       START-AT-STOCK-XREF.
             START STOCK
               KEY >= STK-XREF.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STATUS = "23"
               MOVE 22           TO WS-F-ERROR
               GO TO READ-STOCK-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 22           TO WS-F-ERROR
               PERFORM READ-ERROR.

       READ-STOCK-EXIT.
             EXIT.
      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
       AD000-WRITE             SECTION.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
       REWRITE-STOCK.
             REWRITE STK-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 22           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             GO TO WRITE-STOCK-EXIT.
      
       REWRITE-STOCK-UNLOCK.
             REWRITE STK-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 22           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             UNLOCK STOCK.
             GO TO WRITE-STOCK-EXIT.
      
       DELETE-STOCK-REC.
             DELETE STOCK.
           IF WS-STAT1 NOT = "0"
               MOVE 22           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             UNLOCK STOCK.
             GO TO WRITE-STOCK-EXIT.
      
       WRITE-STOCK.
             WRITE STK-RECORD1.
           IF WS-STAT1 NOT = "0"
               MOVE 22           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             CLOSE STOCK.
             OPEN I-O STOCK.
      
       WRITE-STOCK-EXIT.
             EXIT.
      /
       AZ000-END               SECTION.
       AZ000-EOJ.
             CLOSE STOCK
     BOSAL.
       AZ010.
             STOP RUN.

       BA000           SECTION 1.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "Updating Stock Prices" AT 0230
                      WITH FOREGROUND-COLOR 9.
             DISPLAY "UPDPRC Record :" AT 0603.
             DISPLAY "Stock Details :" AT 0803.
             MOVE ZERO TO WS-PRCKEY WS-RECKEY.
       BA05.
           IF WS-RECKEY = 100
               MOVE ZERO TO WS-RECKEY
              CLOSE STOCK
              OPEN I-O STOCK.
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
       CA500         SECTION.
       CA501.
             MOVE 14             TO W50-LENGTH.
       CA505.
             MOVE W10-ITEM       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-ITEM
               GO TO CA505.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE W50-LENGTH
    RETURNING WS-STATUS.
            MOVE W50-ITEM  TO W10-ITEM.
             MOVE W50-ITEM       TO STK-CODE.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
       CA599.
             EXIT.
      /
      *    ****   C R E A T E   N E W   S T O C K
      *
       DA000           SECTION 50.
       DA00.
             MOVE "001"          TO W10-BIN.
             MOVE 1              TO W10-TAX.
      MOVE PRC-XREF  TO W10-CODE2.
             MOVE PRC-DESC       TO W10-DESC.
            MOVE SPACES  TO W10-ACODE.
             MOVE PRC-COST       TO W10-COST W10-AVRG.
      MOVE PRC-SELL  TO W10-SELL W10-WSALE.
      MOVE PRC-CASH  TO W10-CASH.
             MOVE PRC-LEDG       TO W10-LEDG.
             MOVE ZERO           TO W10-QUANT W10-SELL W10-MARKUP
                                    W10-ORDER W10-CASH W10-CMARKUP
                                    W10-PURCH W10-ODTE W10-WMARKUP
                                    W10-REORD W10-GUAR W10-LTIME
        W10-IND.
       DA20.
             INITIALIZE STK-RECORD1.
             MOVE W10-ITEM       TO STK-CODE WS-ITM.
             MOVE W10-BIN        TO STK-BIN.
             MOVE W10-DESC       TO STK-DESC.
             MOVE W10-TAX        TO STK-TAX.
             PERFORM DA50.
             MOVE W10-MARKUP     TO STK-MARKUP.
             MOVE W10-QUANT      TO STK-QUANT.
             MOVE W10-REORD      TO STK-LEVEL.
             MOVE W10-LTIME      TO STK-LTIME.
             MOVE W10-COST       TO W10-CCOST.
             MOVE W10-SELL       TO W10-CSELL.
           IF W10-COST = ZERO
               IF W10-SELL NOT = ZERO
                   COMPUTE W10-CCOST ROUNDED = (W10-CSELL /
                           (100 + W10-MARKUP) * 100).
           IF W10-SELL = ZERO
               IF W10-COST NOT = ZERO
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
                           ((W10-CCOST * W10-MARKUP) / 100)).
             MOVE W10-CCOST      TO W10-COST.
             MOVE W10-CSELL      TO W10-SELL.
             MOVE W10-COST       TO STK-COST.
             MOVE W10-SELL       TO STK-SELL.
             MOVE W10-LEDG       TO STK-LEDG.
             MOVE W10-GUAR       TO STK-GUAR.
             MOVE W10-ORDER      TO STK-ORD.
             MOVE W10-ODTE       TO STK-DTE.
             MOVE W10-PURCH      TO STK-PUR.
             MOVE W10-AVRG       TO STK-AVGE.
             MOVE W10-IND        TO STK-IND.
             PERFORM DA52.
           IF (W10-CMARKUP = ZERO) AND
              (W10-CASH = ZERO)
               MOVE W10-MARKUP TO W10-CMARKUP
               MOVE W10-SELL TO W10-CASH
               GO TO DA21.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH TO W10-CSELL
               COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
                                             / W10-CCOST) * 100).
           IF W10-CASH = ZERO
               COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
                                            W10-CMARKUP) / 100))
               MOVE W10-CSELL TO W10-CASH.
       DA21.
             MOVE W10-CMARKUP TO STK-CMARKUP.
             MOVE W10-CASH TO STK-CASH.
             PERFORM DA54.
           IF (W10-WMARKUP = ZERO) AND
              (W10-WSALE = ZERO)
               MOVE W10-CMARKUP TO W10-WMARKUP
               MOVE W10-CASH TO W10-WSALE
               GO TO DA21D.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE TO W10-CSELL
               COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
                                             / W10-CCOST) * 100).
           IF W10-WSALE = ZERO
               COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
                                            W10-WMARKUP) / 100))
               MOVE W10-CSELL TO W10-WSALE.
       DA21D.
             MOVE W10-WMARKUP    TO STK-WMARKUP.
             MOVE W10-WSALE      TO STK-WSALE.
             MOVE W10-CODE2      TO STK-XREF.
             MOVE W10-ACODE      TO STK-ACODE.
             PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE STK-RECORD1 TO W15-RECORD1.
             GO TO DA999.
      *
      *    ****  D E F A U L T   M A R K U P
      *
       DA50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
                / W10-COST) * 100).
       DA51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       DA52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH     TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
                 / W10-COST) * 100).
       DA53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       DA54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE    TO W10-CSELL
               MOVE W10-COST     TO W10-CCOST
               COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
                 / W10-COST) * 100) .
       DA55.
             EXIT.
       DA999.
             EXIT.
       DB000           SECTION 50.
       DB00.
           IF PRC-COST = ZERO
               GO TO DB05.
             MOVE PRC-COST TO STK-COST.
             COMPUTE STK-SELL ROUNDED = (PRC-COST +
                              ((PRC-COST * STK-MARKUP) / 100)).
             COMPUTE STK-CASH ROUNDED = (PRC-COST +
                              ((PRC-COST * STK-CMARKUP) / 100)).
             COMPUTE STK-WSALE ROUNDED = (PRC-COST +
                               ((PRC-COST * STK-WMARKUP) / 100)).
       DB05.
             MOVE PRC-LEDG       TO STK-LEDG.
      MOVE PRC-XREF  TO STK-XREF.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
       DB999.
             EXIT.
      /
       ZA000-INIT              SECTION 80.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
             PERFORM ERASE-SCREEN.


      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.




      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.






            MOVE "C"   TO W02-DSK22.
            MOVE "CSS"   TO W02-EXT22.
      DISPLAY "Input file name ?" AT 1212.
      ACCEPT W02-BFILE AT 1230 WITH FOREGROUND-COLOR 15
        UPDATE AUTO.
             OPEN I-O STOCK.
          IF NOT (WS-STATUS = "00")
        OPEN OUTPUT STOCK
        CLOSE STOCK
        OPEN I-O STOCK.
            OPEN INPUT BOSAL.
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
             MOVE 1 TO WS-S1.
             MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.
       ZA60.
             MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
                            WS-TCHR (64) WS-TCHR (71).
             MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
                            WS-MCHR (64) WS-MCHR (71)
                            WS-MCHR (1)  WS-MCHR (80)
                            WS-MCH  (1)  WS-MCH  (80).
             MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
                            WS-BCHR (64) WS-BCHR (71).
             MOVE 44             TO W50-NUMBER.
             MOVE 1              TO W50-SETTING.
             CALL X"AF" USING W50-BIT-PAIRS W50-PARAMETERS.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2412
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key" AT 2512
                      WITH FOREGROUND-COLOR 14.
             ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /
       ZB000-ERROR             SECTION 81.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
              MOVE ZERO  TO WS-KEY.
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
