      $set LINKCOUNT"468" GNT"STP039.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates|   |
      *                                               ---
      *                                               ___
      *                    Input screen co-ordinates |   |
      *                                               ---
      *                                               ___
      *                    Output screen co-ordinates|   |
      *                                               ---
      *  Procedure Division 
      *                                               ___
      *                    Screen Heading Format     |   |
      *                                               ---
      *                                               ___
      *                    Error Messages            |   |
      *                                               ---
      *                                               ___
      *                    User information messages |   |
      *                                               ---
      *                                               ___
      *                    Accept User options       |   |
      *                                               ---
      *                                               ___
      *                    Display co-ordinates      |   |
      *                                               ---
      *                                               ___
      *                    Accept co-ordinates       |   |
      *                                               ---
      *  Accept and Display of Stk-code allows
      *                                               ___
      *         Display ITM, EXT and 14 Characters   |   |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   |   |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       |   |
      *                                               ---
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****     ******    ******    *
      *   **    **     **     **    **   **  **   **    **  **    **   *
      *   **           **     **    **  **    **        **  **    **   *
      *    ******      **     *******   **    **      ***    *******   *
      *         **     **     **        **    **        **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *    ******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     T R A N S F E R   S T O C K   I N  (BRANCH TO BRANCH)      *
      *               *
      *          M A N U A L          *
      *               *
      *       Version 9.04.03 - September 2017                         *

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
       PROGRAM-ID. STP039.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2018
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                       CURSOR IS CSTART
                       CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "STOCK.SL".

       COPY "STKTRF.SL".

           SELECT PRNREP ASSIGN W02-PRINTER
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

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "RECOVER.FD".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "STOCK.FDE".

       COPY "STKTRF.FD".

      /
       FD  PRNREP    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  REP-LINE1.
           03  REP-DETAIL1     PIC X(96).
       01  REP-LINE2.
           03  REP-DETAIL2     PIC X(96).
       01  REP-LINE3.
           03  REP-DATE        PIC X(06).
          03  REP-DTE        PIC Z9/99/9999.
          03  FILLER        PIC X(12).
           03  REP-COMPANY     PIC X(40).
           03  FILLER          PIC X(18).
           03  REP-PAGE        PIC X(06).
           03  REP-P-NO        PIC Z(03)9.
       01  REP-LINE7.
           03  FILLER          PIC X(40).
           03  REP-HEAD10      PIC X(16).
           03  FILLER          PIC X(40).
       01  ENQ-L1.
           03  ENQ-H1          PIC X(15).
           03  ENQ-H2          PIC X(36).
           03  ENQ-H3          PIC X(11).
           03  ENQ-H4          PIC X(12).
           03  ENQ-H5          PIC X(16).
           03  ENQ-H6          PIC X(06).
       01  ENQ-L2.
           03  ENQ-CODE        PIC X(14).
           03  FILLER          PIC X(01).
           03  ENQ-DESC        PIC X(30).
           03  FILLER          PIC X(02).
           03  ENQ-QUANT       PIC Z(06)9.9999-.
           03  FILLER          PIC X(02).
           03  ENQ-COST        PIC Z(05)9.999 BLANK WHEN ZERO.
           03  FILLER          PIC X(02).
           03  ENQ-WSALE       PIC Z(06)9.99.
           03  FILLER          PIC X(02).
           03  ENQ-SELL        PIC Z(06)9.99.
       01  ORD-LINE1.
           03  ORD-H1          PIC X(15).
           03  ORD-H2          PIC X(36).
           03  ORD-H3          PIC X(12).
           03  ORD-H4          PIC X(12).
           03  ORD-H5          PIC X(11).
           03  ORD-H6          PIC X(10).
       01  ORD-LINE2.
           03  ORD-ITEM        PIC X(14).
           03  FILLER          PIC X(01).
           03  ORD-DESC        PIC X(30).
           03  FILLER          PIC X(02).
           03  ORD-QUANT       PIC Z(06)9.9999.
           03  FILLER          PIC X(02).
           03  ORD-COST        PIC Z(06)9.999.
           03  FILLER          PIC X(02).
           03  ORD-SELL        PIC Z(06)9.99.
           03  FILLER          PIC X(02).
           03  ORD-VALUE       PIC Z(06)9.99.
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
       77  WS-DISC                 PIC  9(03)V99 COMP-3.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-TRFKEY        PIC 9(06)     COMP-5.
       77  WS-PRCKEY     PIC 9(06)   COMP-5.
       77  WS-PARKEY        PIC 9(06)     COMP-5.
       77  WS-NETKEY     PIC 9(06)   COMP-5.
       77  WS-RECKEY        PIC 9(06)     COMP-5.
       77  WS-RECOVER       PIC 9(06)     COMP-5.
       77  WS-RECORDS       PIC 9(06)     COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR     PIC 9(04)   COMP-5.
       77  WS-MONTHS     PIC S9(01)V99 COMP-3.
       77  WS-KEY1          PIC 9(04)     COMP-5.
       77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
       77  WS-PAGE          PIC 9(04)     COMP-5.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-OPTION     PIC X(01).
       77  WS-USE-ITM     PIC  X(01).
       77  WS-EXT-STK     PIC  X(01).
       77  WS-SKIP     PIC X(01).
       77  WS-ADJ           PIC X(01).
       77  WS-ERROR         PIC 9(01).
       77  WS-AMEND     PIC 9(01)   VALUE 0.
       77  WS-CONREC        PIC 9(01) VALUE ZERO.
       77  WS-LEDG     PIC X(04).
       77  WS-LEDG1     PIC X(04).
       77  WS-COMP     PIC 9(01).
       77  WS-WEEK          PIC 9(01).
       77  WS-TYPE          PIC X(01).
       77  WS-BATCH         PIC 9(05).
       77  WS-PASSWORD      PIC X(08).
       77  WS-PRINT         PIC 9(01).
       77  WS-LABCDE        PIC X(08).
       77  WS-PASS          PIC X(08).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-NODEP     PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         4 = Clear line 46 and line 50
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP039".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -TRANSFERS IN".
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

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "STP031.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY "W05.VAT".

       COPY "W05.RND".

       01  W09-STOCK.
           03  W09-MARKUP          PIC S9(03)V9999 COMP-3.
           03  W09-CMARKUP         PIC S9(03)V9999 COMP-3.
           03  W09-WMARKUP         PIC S9(03)V9999 COMP-3.

       COPY "W10.STK".

       COPY "W11.STK".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables.cpy".

       COPY "W15.STK".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

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
       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W90-BAL             PIC S9(07)V99 COMP-3.
       01  W95-COMP            PIC X(40).
       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

      *
      *    **       ******  **    **  **   **    ***     *****   ******
      *    **         **    ***   **  **  **    ** **   **   **  **
      *    **         **    ****  **  ** **    **   **  **       ** 
      *    **         **    ** ** **  ****     *******  **       *****
      *    **         **    **  ****  ** **    **   **  **  ***  **  
      *    **         **    **   ***  **  **   **   **  **   **  **
      *    *******  ******  **    **  **   **  **   **   *****   ******
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

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
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
      *
      *   **** Check the User's security level for Stock/Inventory
      *
          IF LS0-STLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM BI000.
      CLOSE RECOVER.

       AA49.
      EXIT PROGRAM.

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 20 (default); move another value to WS-MES-LINE.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear lines 46 and 50:                                 ³
      *    ³    PERFORM CLEAR-L46-AND-50                               ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STOCK.RD".

       COPY "STKTRF.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "STKTRF.WR".

       COPY "STOCK.1ST".

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.
       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
    IF WS-ACTION = 9
        MOVE 0   TO REC-TYPE
    ELSE
        MOVE WS-ACTION  TO REC-TYPE.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.
      *    *************************************************************
      *    ****       N E T W O R K   (S H A R E D)   F I L E       ****
      *    *************************************************************
       AY37.
             MOVE 37                 TO REC-FILE.
             MOVE WS-SHARED          TO REC-KEY.
             MOVE SHR-RECORD         TO REC-NETWORK.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
            ADD 1   TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "ST"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
    IF WS-STOCK = "Y"
              MOVE 3   TO WS-SHARED
              PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
        PERFORM AY37 THRU AY59
        MOVE SHR-RECORD  TO NET-RECORD
    ELSE
              MOVE 3   TO WS-NETKEY
              PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
              PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        IF WS-USUB < 24
     ADD 1  TO WS-USUB
     GO TO AY72
        ELSE
     GO TO AY85.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
    IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.

       COPY "CA000.STK".

       COPY "CA100.STK".

       COPY "CA200.STK".

       COPY "CA500.STK".

      /
      *    ****  T R A N S F E R   S T O C K   I N
      *
       BI000         SECTION 5.
       BI0.
      MOVE WS-OPTION  TO WS-TYPE.
       BI00.
             PERFORM ERASE-SCREEN.
      DISPLAY "TRANSFER STOCK IN" AT 0232
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Key in the file location and name :"
                      AT 0412.
       BI05.
      ACCEPT W02-FILE33 AT 0449
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO BI999
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO BI05
               END-EVALUATE.
    IF W02-FILE33 = SPACES
        GO TO BI05.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BI05.
      OPEN I-O STKTRF.
           IF WS-STATUS NOT = "00"
        MOVE "Invalid drive Specification"
     TO WS-ERR-MES
        PERFORM ERROR-MESSAGE
               GO TO BI05.
      PERFORM CLEAR-L50.
             MOVE ZERO           TO W25-PCOST WS-PAGE
                                    W25-TSELL W25-TOTAL.
      OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
             MOVE WS-12CPI       TO REP-DETAIL2.
             WRITE REP-LINE1 BEFORE 0 LINES.
             DISPLAY "Stock Code:" AT 0612.
       BI10.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "TRANSFER IN"  TO REP-HEAD10.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "ITEM CODE"    TO ORD-H1.
             MOVE "DESCRIPTION"  TO ORD-H2.
             MOVE "QUANTITY"     TO ORD-H3.
             MOVE "UNIT COST"    TO ORD-H4.
             MOVE "UNIT SELL"    TO ORD-H5.
             MOVE "TOTAL SELL"   TO ORD-H6.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE SPACES         TO REP-DETAIL2.
       BI15.
            MOVE SPACES  TO W10-EXT-ITEM.
             MOVE 1              TO W10-QUANT.
             MOVE ZERO           TO W25-SCOST W10-COST.
             MOVE 1              TO WS-TRFKEY.
       BI18.
             PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
           IF WS-F-ERROR = 33
               GO TO BI65.
      DISPLAY TRF-CODE AT 0624
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            MOVE TRF-CODE  TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               GO TO BI20.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             COMPUTE W10-COST ROUNDED = TRF-COST * TRF-QUANT.
    IF TRF-QUANT = ZERO
        MOVE TRF-COST  TO STK-COST
        MOVE TRF-LEDG  TO STK-LDG
               MOVE TRF-ACODE    TO STK-ACODE
               MOVE TRF-XREF     TO STK-XREF
        MOVE TRF-USE-CASES
     TO STK-USE-CASES
        MOVE TRF-USE-PACKS
     TO STK-USE-PACKS
        MOVE 1   TO WS-S4
        PERFORM BI35 THRU BI40
        PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT
        GO TO BI28.
           IF STK-QUANT < 0.01
               MOVE TRF-COST     TO STK-AVGE
           ELSE
               MULTIPLY STK-QUANT BY STK-AVGE GIVING W10-MCOST
               ADD W10-COST      TO W10-MCOST
               COMPUTE STK-AVGE ROUNDED = W10-MCOST /
                                          (STK-QUANT + TRF-QUANT).
             ADD TRF-QUANT       TO STK-QUANT.
      MOVE TRF-COST  TO STK-COST.
      MOVE 1   TO WS-S4.
      PERFORM BI35 THRU BI40.
      PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             GO TO BI25.

       BI20.
             PERFORM AY60 THRU AY999.
             INITIALIZE STK-RECORD1.
             MOVE TRF-CODE       TO STK-CODE.
             MOVE TRF-ACODE      TO STK-ACODE.
             MOVE TRF-BIN        TO STK-BIN.
      MOVE TRF-LEDG  TO STK-LDG.
             MOVE TRF-DESC       TO STK-DESC.
             MOVE TRF-QUANT      TO STK-QUANT.
             MOVE TRF-LEVEL      TO STK-LEVEL.
             MOVE TRF-COST       TO STK-COST.
      MOVE 1   TO WS-S4.
      PERFORM BI35 THRU BI40.
      MOVE TRF-COST  TO STK-AVGE.
             MOVE TRF-ORD        TO STK-ORD.
             MOVE TRF-LTIME      TO STK-LTIME.
      MOVE 1   TO WS-S4.
      PERFORM BI45 THRU BI50.
      MOVE TRF-SUPP  TO STK-SUPP.
             MOVE TRF-GUAR       TO STK-GUAR.
             MOVE TRF-TAX        TO STK-TAX.
             MOVE TRF-XREF       TO STK-XREF.
      MOVE TRF-USE-PACKS  TO TRF-USE-PACKS.
      MOVE TRF-USE-CASES  TO STK-USE-CASES.
             PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
           IF TRF-QUANT = ZERO
               GO TO BI28.
             COMPUTE W10-COST ROUNDED = TRF-COST * TRF-QUANT.

       BI25.
             ADD W10-QUANT       TO W25-TSELL.
             ADD W10-COST        TO W25-TOTAL.
             MOVE SPACES         TO REP-LINE1.
             MOVE W10-COST       TO ORD-VALUE.
             MOVE TRF-QUANT      TO ORD-QUANT.
             MOVE TRF-DESC       TO ORD-DESC.
             MOVE TRF-CODE       TO ORD-ITEM.
      MOVE TRF-SL(1)  TO ORD-SELL.
             MOVE TRF-COST       TO ORD-COST.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE REP-LINE1 BEFORE 1 LINES
           ELSE
               WRITE REP-LINE1 BEFORE PAGE
               PERFORM BI10.
             MOVE SPACES         TO REP-DETAIL2.
             ADD W10-COST        TO STK-TOTAL.

       BI28.
             PERFORM AY70 THRU AY999.

       BI30.
             ADD 1               TO WS-TRFKEY.
             GO TO BI18.

       BI35.
      MOVE TRF-UNT(WS-S4) TO STK-UNT(WS-S4).
      MOVE TRF-PR(WS-S4)  TO STK-PR(WS-S4).
      MOVE TRF-SL(WS-S4)  TO STK-SL(WS-S4).
      MOVE TRF-CSH(WS-S4) TO STK-CSH(WS-S4).
      MOVE TRF-WSL(WS-S4) TO STK-WSL(WS-S4).
    IF WS-S4 < 4
        ADD 1   TO WS-S4
        GO TO BI35.

       BI40.
      EXIT.

       BI45.
      MOVE TRF-MKUP(WS-S4)
     TO STK-MKUP(WS-S4).
      MOVE TRF-CMKUP(WS-S4)
     TO STK-CMKUP(WS-S4).
      MOVE TRF-WMKUP(WS-S4)
     TO STK-WMKUP(WS-S4).
    IF WS-S4 < 4
        ADD 1   TO WS-S4
        GO TO BI45.

       BI50.
      EXIT.

       BI65.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE "TOTAL"        TO ORD-DESC.
             MOVE W25-TSELL      TO ORD-QUANT.
             MOVE W25-TOTAL      TO ORD-VALUE.
             WRITE REP-LINE1 BEFORE 2 LINES.
             MOVE "***** END REPORT *****" TO REP-DETAIL2.
             WRITE REP-LINE1 BEFORE PAGE.
             MOVE WS-10CPI       TO REP-DETAIL1.
             WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE SPACES         TO REP-DETAIL1.
             CLOSE PRNREP.
             CLOSE STKTRF.
      GO TO BI999.
       BI999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
             MOVE PAR-STOCK          TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
             MOVE 110                TO WS-USUB.
           IF LS-USE > ZERO AND < 110
               MOVE LS-USE           TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             MOVE TRM-STDP (WS-S6)
                                 TO WS-PRINT.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA01B.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
      MOVE 1   TO W05-V-RATE.

       ZA05-VAT.
             MOVE W05-VAT-CODE       TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-R-DATE         TO WS-VAT-DATE(W05-V-RATE).
             MOVE DPT-RATE           TO W05-VAT(W05-V-RATE).
             ADD 6 W05-V-RATE        GIVING WS-S1.
             MOVE DPT-P-RATE         TO W05-VAT(WS-S1).
           IF W05-V-RATE < 6
               ADD 1                 TO W05-V-RATE
               GO TO ZA05-VAT.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      ACCEPT WS-OPTION AT 1238
      WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO.
             GO TO ZA205.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 8.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2     MOVE W02-NETWORK  TO WS-FILE
                          MOVE WS-NETKEY    TO WS-KEY
               WHEN 7     MOVE W02-DEPART   TO WS-FILE
                          MOVE DPT-CODE     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 18    MOVE W02-RECOVER  TO WS-FILE
                          MOVE WS-RECKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 33    MOVE W02-STKTRF   TO WS-FILE
                          MOVE WS-TRFKEY    TO WS-KEY
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP039"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
