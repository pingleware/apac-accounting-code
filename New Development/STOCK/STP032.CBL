      $set LINKCOUNT"512" GNT"STP032.GNT"
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
      *   **           **     **    **  **    **        **       **    *
      *    ******      **     *******   **    **      ***      **      *
      *         **     **     **        **    **        **   **        *
      *   **    **     **     **         **  **   **    **  **         *
      *    ******      **     **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   -   T R A N S F E R S   O U T   P R O G .      *
      *                                                                *
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
       PROGRAM-ID.     STP032.
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

       COPY "STKDEX.SL".

       COPY "STKTRF.SL".

       COPY "STKWRK.SL".

       COPY "STOCK.SL".

       COPY TRFOUT.SL.

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

       COPY "STKDEX.FDE".

       COPY "STKTRF.FD".

       COPY "STKWRK.FD".

       COPY "STOCK.FDE".

       COPY TRFOUT.FDE.

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
       77  WS-TRFKEY    PIC 9(06)  COMP-5.
       77  WS-PRCKEY    PIC 9(06)  COMP-5.
       77  WS-PARKEY    PIC 9(06)  COMP-5.
       77  WS-NETKEY    PIC 9(06)  COMP-5.
       77  WS-RECKEY    PIC 9(06)  COMP-5.
       77  WS-RECOVER    PIC 9(06)  COMP-5.
       77  WS-RECORDS    PIC 9(06)  COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEYSTR    PIC 9(04)  COMP-5.
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-BRANCH    PIC X(40).
       77  WS-BR-CODE    PIC X(03).
       77  WS-BR-NAME    PIC X(38).
       77  WS-TRF-REF    PIC 9(07)  COMP-3.
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-HND-FEE    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-TOTAL    PIC X(01).
       77  WS-ERROR    PIC 9(01).
       77  WS-IND    PIC 9(01)  VALUE 0.
       77  WS-INDB    PIC 9(01)  VALUE 0.
       77  WS-INDP    PIC 9(01)  VALUE 0.
       77  WS-INDS    PIC 9(01)  VALUE 0.
       77  WS-AMEND    PIC 9(01)  VALUE 0.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-LEDG    PIC X(04).
       77  WS-LEDG1    PIC X(04).
       77  WS-COMP    PIC 9(01).
       77  WS-WEEK    PIC 9(01).
       77  WS-TYPE    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-PASSWORD    PIC X(08).
       77  WS-PRINT    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-NODEP    PIC X(30) VALUE
      "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-HFEE    PIC 9(03)V999 COMP-3.

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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP031".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -TRANSFERS OUT".
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

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "STP".
           03  WS-PROG             PIC  X(03) VALUE "032".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)           VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).
    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).
          03  REP-LINE1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1    PIC X(96).
          03  REP-LINE2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2    PIC X(96).
          03  REP-LINE3 REDEFINES W02-PRN-LINE.
              05  REP-DATE    PIC X(06).
              05  REP-DTE    PIC Z9/99/9999.
              05  FILLER    PIC X(14).
              05  REP-COMPANY    PIC X(40).
              05  FILLER    PIC X(18).
              05  REP-PAGE    PIC X(06).
              05  REP-P-NO    PIC Z(03)9.
          03  REP-LINE7 REDEFINES W02-PRN-LINE.
              05  FILLER    PIC X(40).
              05  REP-HEAD10    PIC X(16).
              05  FILLER    PIC X(40).
    03  REP-LINE8 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(14).
        05  REP-TRF-HD    PIC X(13).
        05  REP-BRANCH    PIC X(40).
        05  FILLER    PIC X(29).
          03  ORD-LINE1 REDEFINES W02-PRN-LINE.
              05  ORD-H1    PIC X(19).
              05  ORD-H2    PIC X(35).
              05  ORD-H3    PIC X(11).
              05  ORD-H4    PIC X(11).
              05  ORD-H5    PIC X(10).
              05  ORD-H6    PIC X(10).
          03  ORD-LINE2 REDEFINES W02-PRN-LINE.
              05  ORD-ITEM    PIC X(18).
        05  ORD-ITM REDEFINES ORD-ITEM
       PIC X(14)/X(03).
              05  FILLER    PIC X(01).
              05  ORD-DESC    PIC X(30).
              05  FILLER    PIC X(01).
              05  ORD-QUANT    PIC Z(06)9.9999.
              05  FILLER    PIC X(01).
              05  ORD-COST    PIC Z(06)9.999.
              05  FILLER    PIC X(01).
              05  ORD-SELL    PIC Z(06)9.99.
              05  FILLER    PIC X(01).
              05  ORD-VALUE    PIC Z(06)9.99.
    03  TOT-LINE1 REDEFINES W02-PRN-LINE.
        05  TOT-H1    PIC X(14).
        05  TOT-H2    PIC X(32).
        05  TOT-H3    PIC X(12).
        05  TOT-H4    PIC X(38).
    03  TOT-LINE2 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(04).
        05  TOT-SL-NO    PIC X(04).
        05  FILLER    PIC X(06).
        05  TOT-SL-NAME    PIC X(30).
        05  FILLER    PIC X(03).
        05  TOT-STK-GL    PIC 9(04)/99.
        05  FILLER    PIC X(03).
        05  TOT-SL-VALUE    PIC Z(06)9.99-.
        05  FILLER    PIC X(28).

       COPY "W05.VAT".

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

       COPY "W25.STK".

           03  W25-HND-FEE         PIC S9(09)V99 COMP-3.
           03  W25-TH-FEE          PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W75.STK".

       01  W80-EDIT.
           03  W80-CNO        PIC 9(04).
           03  W80-V1.
               05  W80-S7V2   PIC Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2   PIC Z(04)9.99-.
           03  W80-DTE.
              05  W80-DATE   PIC Z9/99/9999.
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

       01  S07.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC  X(14) USING W10-EXT-ITEM AUTO.

       01  S07-ITM.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC  X(14)/X(03)
      USING W10-EXT-ITEM AUTO.

       01  S07-EXT.
    03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC  X(18) USING W10-EXT-ITEM AUTO.

       01  S07-3.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(03) USING W10-I4 AUTO.

       01  S07-4.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(04) USING W10-I4 AUTO.

       01  S07-5.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(05) USING W10-I5 AUTO.

       01  S07-6.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(06) USING W10-I6 AUTO.

       01  S07-7.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(07) USING W10-I7 AUTO.

       01  S07-8.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(08) USING W10-I8 AUTO.

       01  S07-9.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(09) USING W10-I9 AUTO.

       01  S07-10.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(10) USING W10-I10 AUTO.

       01  S07-11.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(11) USING W10-I11 AUTO.

       01  S07-12.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(12) USING W10-I12 AUTO.

       01  S07-13.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(13) USING W10-I13 AUTO.

       01  S07-14.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
           PIC Z(14) USING W10-I14 AUTO.

       01  S10.
    03  LINE 11 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC ZZ9.999 USING WS-HFEE AUTO.

       01  S35.
    03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC Z(05)9.9999 USING W10-QUANT AUTO.

       01  S36.
    03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC Z(06)9.99 USING W10-SELL AUTO.

       01  S37.
    03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC Z(05)9.999 USING W10-COST AUTO.

       01  S38.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Black
      PIC ZZZZ9 USING W10-CASES AUTO.

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
      PERFORM BA000.
            PERFORM BH000.
      CLOSE RECOVER
     STKWRK.

       AA49.
      EXIT PROGRAM.

       COPY "AA50.LUP".

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

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

       COPY "AA300.STK".

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

       COPY "STKDEX.WR".

       COPY "STOCK.WR".

       COPY "STKTRF.WR".

       COPY TRFOUT.WR.

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

      /    *************************************************************
      *   THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
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

       AY46.
            MOVE 46   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE STX-REC1  TO REC-STKDEX.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    End transaction in recovery log        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1               TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *    *************************************************************
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Red HIGHLIGHT.
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
      *    *************************************************************
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
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
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *    *************************************************************
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *    *************************************************************
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *    *************************************************************
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *    *************************************************************
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *    *************************************************************
        IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
      *    *************************************************************
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
      *    *************************************************************
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
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
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *    *************************************************************
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

      /    *************************************************************
      *    ****   U S E R   M E S S A G E
      *    *************************************************************
       BA000        SECTION 50.
       BA00.
            PERFORM ERASE-SCREEN.
      DISPLAY "TRANSFER STOCK OUT" AT 0232
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE 5   TO SHADE-ROW.
      MOVE 6   TO SHADE-COL.
      MOVE 70   TO SHADE-WIDTH.
      MOVE 10   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0404
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       "¿" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 0504
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " As stock transfers out may be used for either supplying or returning "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 0604
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " goods, the system will only update the physical stock and will NOT   "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 0704
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " generate invoices or debit notes. The totals are given at the end of "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 0804
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " the report that is generated and it is the responsibility of the     "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 0904
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " USER to process the necessary transactions using the report totals.  "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 1004
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " The USER may select to use accounts in the debtor or creditor ledger "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 1104
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " or to use general ledger accounts to handle the inter branch stock   "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 1204
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " transfers. See Options 2/2/6, 2/2/7, 2/4/1 & 2/4/2 for creditors and "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "³" AT 1304
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       " Options 3/2/1, 3/2/2 and 5/6/C for debtors or phone the help line    "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Red HIGHLIGHT
         BACKGROUND-COLOR Red.
      DISPLAY "À" AT 1404
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red.
      PERFORM SCREEN-SHADOW.
      DISPLAY "Press ANY key to continue" AT 5012
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Cyan.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA05.
      MOVE "N"   TO WS-TOTAL.

       BA999.
      EXIT.
      /    *************************************************************
      *    ****   T R A N S F E R   S T O C K O U T
      *    *************************************************************
       BH000        SECTION 51.
       BH0.
      MOVE WS-OPTION  TO WS-TYPE.

       BH00.
            PERFORM ERASE-SCREEN.
      DISPLAY "TRANSFER STOCK OUT" AT 0232
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Key in the file location and name :"
                      AT 0412.

       BH05.
      ACCEPT W02-FILE33 AT 0449
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       GO TO BH999
   WHEN OTHER
                     PERFORM AA900-ALARM
       GO TO BH05
        END-EVALUATE.
    IF W02-FILE33 = SPACES
        GO TO BH05.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BH05.
      DISPLAY "A" AT 0612 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "dd details to existing or "
                      WITH FOREGROUND-COLOR Grey
       "C" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "reate new TRANSFER file" WITH FOREGROUND-COLOR Grey.
             MOVE SPACE          TO WS-OPTION.

       BH10.
      PERFORM UNTIL WS-OPTION = "A" OR "C" OR "P"
        ACCEPT WS-OPTION AT 0664
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
      END-PERFORM.
    IF WS-OPTION = "C"
               GO TO BH20.

       BH15.
             OPEN I-O STKTRF.
           IF WS-STATUS = "00"
               MOVE ZERO         TO WS-TRFKEY
        GO TO BH25.
    IF WS-STATUS = "05"
        CLOSE STKTRF.

       BH20.
             OPEN OUTPUT STKTRF.
    IF WS-STATUS NOT = "00"
        MOVE "Invalid drive"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BH00.
      MOVE 1   TO WS-TRFKEY.
      INITIALIZE TRF-RECORD1.
      MOVE WS-BR-CODE  TO TRF-BR-CODE
        SOT-BR-CODE.
      MOVE WS-TRF-REF  TO TRF-REF
        SOT-REF.
      MOVE W12-T-YMD  TO TRF-DATE
        SOT-DATE.
      MOVE WS-BR-NAME  TO TRF-BR-FROM
        SOT-BR-FROM.
      MOVE SPACES  TO TRF-TO-CDE
        TRF-BR-TO
        SOT-BR-TO
        SOT-TO-CDE.
      MOVE ZERO   TO TRF-VALUE
        SOT-VALUE
        SOT-LINES.
             PERFORM WRITE-STKTRF THRU WRITE-STKTRF-EXIT.
      CLOSE STKTRF.
      MOVE LS0-NO  TO SOT-USER.
      MOVE LS-USER  TO SOT-WS.
      MOVE LS0-NAME  TO SOT-USER-NAME.
      PERFORM WRITE-TRFOUT THRU WRITE-TRFOUT-EXIT.
      MOVE 6   TO WS-PARKEY.
      PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
    IF WS-TRF-REF < 9999999
        ADD 1   TO WS-TRF-REF
    ELSE
        MOVE 1   TO WS-TRF-REF.
      MOVE WS-TRF-REF  TO PAR-TRF-REF.
      PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             GO TO BH15.

       BH25.
             ADD 1               TO WS-TRFKEY.
             PERFORM READ-STKTRF THRU READ-STKTRF-EXIT.
           IF WS-F-ERROR = ZERO
               GO TO BH25.
             MOVE ZERO           TO W25-PCOST WS-PAGE
        W25-TSELL W25-TOTAL
        W25-TH-FEE.
      MOVE SPACES  TO WS-BRANCH.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      *
      *    ****   1 2 C P I ( C O N D E N S E D   P R I N T )
      *
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
              MOVE SPACES  TO REP-DETAIL1.
            PERFORM ERASE-SCREEN.
             DISPLAY "TRANSFER STOCK OUT" AT 0232
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY "Transfer to:" AT 0412.

       BH26.
      ACCEPT WS-BRANCH AT 0425
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BH26.
      *    *************************************************************
      *    ****    M U S T   A  H A N D L I N G   F E E   B E
      *     C H A R G E D
      *    *************************************************************
    IF WS-HND-FEE = "N"
        GO TO BH30.
      PERFORM SAVE-SCREEN-3.
      MOVE 11   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 22   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1029
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ Handling fee         ³" AT 1129
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1229
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.

       BH28.
      ACCEPT S10.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BH28.
      PERFORM RESTORE-SCREEN-3.

       BH30.
            DISPLAY "Item Code/Description" AT 0602.
            DISPLAY "Quantity" AT 0636.
            DISPLAY "Unit cost" AT 0647.
            DISPLAY "Unit sell" AT 0659.
            DISPLAY "Total cost" AT 0670.

       BH35.
             MOVE SPACES         TO REP-DETAIL2.
             MOVE "PAGE:"        TO REP-PAGE.
             MOVE "DATE:"        TO REP-DATE.
             MOVE W95-COMP       TO REP-COMPANY.
             ADD 1               TO WS-PAGE.
             MOVE WS-PAGE        TO REP-P-NO.
             MOVE W12-TODAY      TO REP-DTE.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
             MOVE "TRANSFER OUT" TO REP-HEAD10.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      MOVE "TRANSFER TO:" TO REP-TRF-HD.
      MOVE WS-BRANCH  TO REP-BRANCH.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
    IF WS-TOTAL = "N"
              MOVE "ITEM CODE"  TO ORD-H1
              MOVE "DESCRIPTION"
     TO ORD-H2
              MOVE "QUANTITY"  TO ORD-H3
              MOVE "UNIT COST"  TO ORD-H4
              MOVE "UNIT SELL"  TO ORD-H5
              MOVE "TOTAL COST" TO ORD-H6
    ELSE
        MOVE "SALES LEDGER"
     TO TOT-H1
        MOVE "DESCRIPTION"
     TO TOT-H2
        MOVE "STOCK G/L"  TO TOT-H3
        MOVE "TRF VALUE"  TO TOT-H4.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.

       BH40.
             MOVE SPACES             TO W10-EXT-ITEM.
             MOVE 1                  TO W10-QUANT.
             MOVE ZERO               TO W25-SCOST W10-COST W10-CASES.
             MOVE 0801               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
           IF W10-STCK = "N"
               MOVE ZERO             TO W10-I14.
      *        EVALUATE W10-SLNGTH
      *          WHEN 3   MOVE ZERO  TO W10-I3            
      *          WHEN 4   MOVE ZERO  TO W10-I4            
      *          WHEN 5   MOVE ZERO  TO W10-I5            
      *          WHEN 6   MOVE ZERO  TO W10-I6            
      *          WHEN 7   MOVE ZERO  TO W10-I7            
      *          WHEN 8   MOVE ZERO  TO W10-I8            
      *          WHEN 9   MOVE ZERO  TO W10-I9            
      *          WHEN 10  MOVE ZERO  TO W10-I10            
      *          WHEN 11  MOVE ZERO  TO W10-I11            
      *          WHEN 12  MOVE ZERO  TO W10-I12            
      *          WHEN 13  MOVE ZERO  TO W10-I13            
      *          WHEN 14  MOVE ZERO  TO W10-I14
      *        END-EVALUATE.

       BH45.
             PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S05-EXT
           ELSE
      *   
      *       Is an ALPHA-NUMERIC code being used?
      *    
           IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
               IF WS-USE-ITM = "Y"
                   ACCEPT S05-ITM
               ELSE
                   ACCEPT S05
               END-IF
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
      *    
      *     Check the length of the numeric Stock/Inventory item code 
      *     being used and allow the User to key in the code.         
      *    
           ELSE
               EVALUATE W10-SLNGTH
      *                     3 Digit numeric Stock/Inventory item code
                 WHEN 3         ACCEPT S05-3
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S05-4
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S05-5
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S05-6
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S05-7
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S05-8
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S05-9
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S05-10
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S05-11
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S05-12
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S05-13
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S05-14
               END-EVALUATE.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
       GO TO BH65
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BH45
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
                     PERFORM AA900-ALARM
       GO TO BH45
               END-EVALUATE
        PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT AT 0802
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT 0802
     ELSE
         DISPLAY S07 AT 0802
     END-IF
        ELSE
     DISPLAY S07 AT 0802
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BH45.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BH65.

       BH50.
             PERFORM CA000.
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT 0802
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT 0802
        ELSE
     DISPLAY S07 AT 0802
        END-IF
    ELSE
        DISPLAY S07 AT 0802.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT AT 0802
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT 0802
     ELSE
         DISPLAY S07 AT 0802
     END-IF
        ELSE
     DISPLAY S07 AT 0802
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BH45
               ELSE
                   GO TO BH50.
      MOVE STK-DESC  TO W10-DESC.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BH45.
            DISPLAY W10-DESC AT 0902
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BH45.

       BH52.
           IF STK-IND = 3
        ACCEPT W10-DESC AT 0902
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT
      BACKGROUND-COLOR Magenta
        PERFORM CHECK-CORRECT
              IF WS-OPTION = "N"
                   GO TO BH52.

       BH55.
            ACCEPT S35 AT 0933.
           IF W10-QUANT = ZERO
               GO TO BH45.
    IF WS-USE-CASES = "Y" AND STK-USE-CASES = "Y"
        PERFORM JA000
        IF W10-CASES = ZERO
     GO TO BH55.
             MOVE STK-COST       TO W10-COST.
             MOVE STK-SELL       TO W10-SELL.
           IF STK-IND = 3
        ACCEPT S37 AT 0946
        ACCEPT S36 AT 0958
           ELSE
        DISPLAY S37 AT 0946
        DISPLAY S36 AT 0958.
             MOVE W10-COST       TO W10-ICOST.
             COMPUTE W10-COST ROUNDED = W10-ICOST * W10-QUANT.
            DISPLAY S37 AT 0970.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BH55.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             SUBTRACT W10-QUANT  FROM STK-QUANT.
      SUBTRACT W10-CASES  FROM STK-CASES.
             ADD W10-QUANT       TO STK-MTD STK-YTD.
             MOVE SPACES         TO REP-LINE1.
      MOVE STK-CODE  TO TRF-EXT-CODE.
             MOVE STK-ACODE      TO TRF-ACODE.
             MOVE STK-BIN        TO TRF-BIN.
      MOVE STK-LDG  TO TRF-LEDG.
             MOVE W10-DESC       TO TRF-DESC.
      MOVE W10-QUANT  TO TRF-QUANT.
      MOVE W10-CASES  TO TRF-CASES.
      MOVE STK-USE-CASES  TO TRF-USE-CASES.
      MOVE STK-USE-PACKS  TO TRF-USE-PACKS.
             MOVE STK-LEVEL      TO TRF-LEVEL.
             MOVE W10-ICOST      TO TRF-COST.
      MOVE STK-ORD  TO TRF-ORD.
             MOVE STK-LTIME      TO TRF-LTIME.
      MOVE STK-SUPP  TO TRF-SUPP.
             MOVE STK-GUAR       TO TRF-GUAR.
             MOVE STK-TAX        TO TRF-TAX.
             MOVE STK-XREF       TO TRF-XREF.
      MOVE STK-EXEMPT  TO TRF-EXEMPT.
      MOVE STK-FACT  TO TRF-FACT.
      MOVE 1   TO WS-S1.

       BH58.
      MOVE STK-UNT(WS-S1) TO TRF-UNT(WS-S1).
      MOVE STK-PR(WS-S1)  TO TRF-PR(WS-S1).
      MOVE STK-MKUP(WS-S1)
     TO TRF-MKUP(WS-S1).
      MOVE STK-CMKUP(WS-S1)
     TO TRF-CMKUP(WS-S1).
      MOVE STK-WMKUP(WS-S1)
     TO TRF-WMKUP(WS-S1).
    IF WS-S1 = 1
        MOVE W10-SELL  TO TRF-SL(WS-S1)
    ELSE
        MOVE STK-SL(WS-S1)
     TO TRF-SL(WS-S1).
      MOVE STK-CSH(WS-S1) TO TRF-CSH(WS-S1).
      MOVE STK-WSL(WS-S1) TO TRF-WSL(WS-S1).
    IF WS-S1 < 4
        ADD 1   TO WS-S1
        GO TO BH58.
      *    *************************************************************
      *    ****    U P D A T E  O N T O   W O R K   F I L E
      *    *************************************************************
      MOVE STK-LDG  TO STW-DEPT.
      READ STKWRK KEY IS STW-DEPT.
    IF NOT (WS-STATUS = "00")
        MOVE ZERO  TO STW-SL-TOTAL
        MOVE STK-LDG  TO STW-DEPT
        WRITE STW-RECORD
        CLOSE STKWRK
        OPEN I-O STKWRK.
      ADD W10-COST  TO STW-SL-TOTAL.
      REWRITE STW-RECORD.
      CLOSE STKWRK.
      OPEN I-O STKWRK.
    IF WS-HND-FEE = "Y"
        COMPUTE TRF-COST ROUNDED = TRF-COST + (TRF-COST *
       WS-HFEE / 100.000).
      MOVE TRF-COST  TO TRF-AVGE.
    IF WS-HND-FEE = "Y"
        COMPUTE W25-HND-FEE = (TRF-COST - W10-ICOST) * W10-QUANT
        ADD W25-HND-FEE  TO W25-TH-FEE
        W25-TOTAL.
            MOVE W10-QUANT  TO ORD-QUANT.
             MOVE W10-DESC       TO ORD-DESC.
    IF WS-USE-ITM = "Y"
        MOVE STK-CODE  TO ORD-ITM
    ELSE
              MOVE STK-CODE  TO ORD-ITEM.
      MOVE W10-SELL  TO ORD-SELL.
      MOVE TRF-COST  TO ORD-COST.
      COMPUTE ORD-VALUE ROUNDED = TRF-COST * W10-QUANT.
      ADD W10-QUANT  TO W25-TSELL.
             ADD W10-COST        TO W25-TOTAL.
            ADD W10-COST  TO STK-MTDC STK-MTDV STK-YTDC STK-YTDV.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BH35.
             MOVE SPACES         TO REP-DETAIL2.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             SUBTRACT W10-COST   FROM STK-TOTAL.
             PERFORM WRITE-STKTRF THRU WRITE-STKTRF-EXIT.
             ADD 1               TO WS-TRFKEY.
    IF WS-CARDEX = "Y"
        INITIALIZE STX-REC1
        MOVE STK-CODE  TO STX-EXT-CODE
        MOVE W12-T-YMD  TO STX-DTE
        MOVE ZERO  TO STX-SEQ
        MOVE LS0-NO  TO STX-USER
        MOVE "STKTRF"  TO STX-PROG
        MOVE LS-USER  TO STX-WS
        MOVE "Stock transfers" TO STX-REMARKS
        MOVE "I"          TO STX-TYPE
        MULTIPLY -1 BY W10-QUANT
     GIVING STX-QNT
        MULTIPLY -1 BY W10-COST
     GIVING STX-COST
        COMPUTE STX-SELL ROUNDED = (W10-COST +
         ((W10-COST * STK-MARKUP) / 100)) * -1
        PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
        MOVE 1   TO WS-ACTION
        PERFORM AY46 THRU AY59.
             PERFORM AY70 THRU AY999.

       BH60.
            GO TO BH40.

       BH65.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "TOTAL"  TO ORD-DESC.
             MOVE W25-TSELL      TO ORD-QUANT.
             MOVE W25-TOTAL      TO ORD-VALUE.
      PERFORM CALL-PRINTUTL.
            MOVE "***** END TRANSFER *****" TO REP-DETAIL2.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE "Y"   TO WS-TOTAL.
      MOVE ZERO   TO WS-PAGE.
            PERFORM BH35.
      MOVE "!"   TO STW-DEPT.
      START STKWRK KEY >= STW-DEPT.
    IF NOT (WS-STATUS = "00")
        GO TO BH75.

       BH70.
      READ STKWRK NEXT.
    IF NOT (WS-STATUS = "00")
        CLOSE STKWRK
        OPEN OUTPUT STKWRK
        CLOSE STKWRK
        OPEN I-O STKWRK
        GO TO BH75.
      MOVE STW-DEPT  TO DPT-CODE
        TOT-SL-NO.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
    IF NOT (DPT-DESC = SPACES)
        MOVE DPT-DESC  TO TOT-SL-NAME
    ELSE
        MOVE "No Depart/Sales Ledger Name"
     TO TOT-SL-NAME.
      MOVE DPT-PUR-GL  TO TOT-STK-GL.
      MOVE STW-SL-TOTAL  TO TOT-SL-VALUE.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BH35.
             MOVE SPACES         TO REP-DETAIL2.
      DELETE STKWRK.
      GO TO BH70.

       BH75.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-HND-FEE = "Y"
        MOVE "HANDLING FEE"
     TO TOT-SL-NAME
        MOVE W25-TH-FEE  TO TOT-SL-VALUE
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
            MOVE "***** END REPORT *****" TO REP-DETAIL2.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE 1   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
              MOVE SPACES  TO REP-DETAIL1.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      CLOSE STKTRF.

       BH999.
             EXIT.
      /    *************************************************************
      *    ****    C A S E S
      *    *************************************************************
       JA000        SECTION 66.
       JA00.
      PERFORM SAVE-SCREEN.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0833
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Brown.
      DISPLAY "³ NO OF CASES ³" AT 0933
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Brown.
      DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 1033
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Brown.
      DISPLAY "³             ³" AT 1133
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Brown.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1233
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Brown.
      MOVE 09   TO SHADE-ROW.
      MOVE 35   TO SHADE-COL.
      MOVE 13   TO SHADE-WIDTH.
      MOVE 4   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE 1   TO W10-CASES.

       JA05.
      ACCEPT S38 AT 1138.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE ZERO  TO W10-CASES
       GO TO JA10
        END-EVALUATE.

       JA10.
      PERFORM RESTORE-SCREEN.

       JA999.
      EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
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
           IF PAR-HND-FEE = "Y"
               MOVE PAR-HFEE         TO WS-HFEE
               MOVE PAR-HND-FEE      TO WS-HND-FEE
           ELSE
               MOVE "N"              TO WS-HND-FEE
               MOVE ZERO             TO WS-HFEE.
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
             MOVE PAR-CRDX           TO WS-CARDEX.
             MOVE 1                  TO W05-V-RATE.

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
      MOVE 275   TO WS-PARKEY.
      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-BR-CDE  TO WS-BR-CODE.
      MOVE PAR-BR-NME  TO WS-BR-NAME.
      MOVE 6   TO WS-PARKEY.
      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-TRF-REF  TO WS-TRF-REF.

       ZA01B.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
            OPEN I-O RECOVER.
      OPEN I-O STKWRK.
    IF WS-STATUS = "05"
        CLOSE STKWRK
        OPEN OUTPUT STKWRK
        CLOSE STKWRK
        OPEN I-O STKWRK.
      OPEN I-O TRFOUT.
    IF WS-STATUS = "05"
        CLOSE TRFOUT
        OPEN OUTPUT TRFOUT
        CLOSE TRFOUT
        OPEN I-O TRFOUT.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE 3              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK       TO WS-WEEK.
             MOVE STK-RECORDS    TO WS-RECORDS.
             MOVE STK-PASSWORD   TO WS-PASSWORD.
      GO TO ZA999-EXIT.

       COPY ZA49.PRO.


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
           ELSE
          IF WS-F-ERROR = 46
              MOVE W02-STKDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STX-KEY  TO WS-KEYX
           ELSE
          IF WS-F-ERROR = 63
              MOVE W02-TRFOUT  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE SOT-KEY  TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP032"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
