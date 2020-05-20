      $SET LINKCOUNT"380" GNT"PRP003.GNT"
      ******************************************************************
      *                                                                *
      *   *******   *******   *******     ****      ****     ******    *
      *   **    **  **    **  **    **   **  **    **  **   **    **   *
      *   **    **  **    **  **    **  **    **  **    **        **   *
      *   *******   ******    *******   **    **  **    **     ****    *
      *   **        **   **   **        **    **  **    **        **   *
      *   **        **    **  **         **  **    **  **   **    **   *
      *   **        **    **  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P A R A M E T E R   S A L E S   L E D G E R                 *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRP003.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   JANUARY 1983.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2017
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

       COPY "GACCNT.SL".

       COPY "PARAM.SL".

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

       COPY "GACCNT.FDE".

       COPY "PARAM.FDE".

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
      *77  WS-PRNKEY               PIC  9(04)    COMP-5.
      *77  WS-NETKEY               PIC  9(04)    COMP-5.
      *77  WS-SECKEY               PIC  9(04)    COMP-5.
      *77  WS-NUM                  PIC  Z9.
      *77  WS-SNO                  PIC  9(02).
      *77  WS-ADV-RATE             PIC S9(03)V99 COMP-3.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  PRG-PRINTUTL            PIC  X(12)    VALUE "UTP\PRINTUTL".
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
      *77  WS-COND                 PIC  X(06).
      *77  WS-NORM                 PIC  X(06).
      *77  WS-EXPP                 PIC  X(06).
      *77  WS-ECAN                 PIC  X(06).
      *77  WS-8LPI                 PIC  X(06).
      *77  WS-6LPI                 PIC  X(06).
      *77  WS-10CPI                PIC  X(06).
      *77  WS-12CPI                PIC  X(06).
      *77  WS-17CPI                PIC  X(06).
       77  WS-OPEN                 PIC  9(02)    VALUE ZERO.
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-COMP                 PIC  9(01).
       77  WS-IND                  PIC  9(01)    VALUE 0.
       77  WS-ER6                  PIC  X(09)    VALUE "No Record".
       77  WS-ER7                  PIC  X(13)    VALUE "Group Heading".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02) VALUE 5.
           03  CRT-END               PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "PRP003".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "SALES LEDGER/DEPARTMENT MAINTENANCE".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40) VALUE SPACES.
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)    VALUE "PRP".
           03  WS-PROG             PIC  X(03)    VALUE "003".

       01  WS-PARID.
          03  WS-SYS-ID            PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(02).
           03  W02-PRN-LENGTH      PIC  9(02).
           03  W02-LINAGE          PIC  9(02).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).
           03  REP-LINE1                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
           03  REP-LINE2                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2     PIC  X(80).
           03  REP-LINE3                         REDEFINES W02-PRN-LINE.
              05  REP-PAGE         PIC  X(06).
              05  REP-P-NO         PIC  Z(03)9.
              05  FILLER           PIC  X(09).
              05  REP-COMPANY      PIC  X(40).
              05  FILLER           PIC  X(05).
              05  REP-DATE         PIC  X(06).
              05  REP-DTE          PIC  Z9/99/9999.
          03  REP-LINE4                          REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(11).
              05  REP-HEAD1        PIC  X(04).
              05  FILLER           PIC  X(06).
              05  REP-HEAD2        PIC  X(15).
              05  FILLER           PIC  X(06).
              05  REP-HEAD3        PIC  X(15).
              05  FILLER           PIC  X(06).
              05  REP-HEAD4        PIC  X(06).
              05  FILLER           PIC  X(11).
          03  REP-LINE5                          REDEFINES W02-PRN-LINE.
              05  FILLER           PIC  X(11).
              05  REP-TRAN         PIC  Z99.
              05  FILLER           PIC  X(09).
              05  REP-ENG          PIC  X(12).
              05  FILLER           PIC  X(09).
              05  REP-AFR          PIC  X(12).
              05  FILLER           PIC  X(10).
              05  REP-ACT          PIC  X(01).
              05  FILLER           PIC  X(13).

       01  W10-INPUT.
           03  W10-LEDG.
               05  W10-LEDG-NO     PIC  9(04).
           03  W10-LEDG-RED                      REDEFINES W10-LEDG.
               05  W10-GROUP       PIC  X(03).
               05  W10-D-NO        PIC  9(01).
           03  W10-DATE            PIC  X(10).
           03  W10-EDTE            PIC  99/99/9999 REDEFINES W10-DATE.

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

       01  W15-INPUT.
           03  W15-ACNT            PIC  9(06).
           03  W15-ACCOUNT         PIC  9(06).
           03  W15-ACNO1                         REDEFINES W15-ACCOUNT.
               05  W15-ACNO        PIC  9(04).
               05  W15-SER         PIC  9(02).
           03  W15-ACNO2                         REDEFINES W15-ACCOUNT.
               05  W15-GR          PIC  9(02).
               05  W15-SGRP        PIC  9(02).
               05  W15-ASER        PIC  9(02).
           03  W15-ACNO3                         REDEFINES W15-ACCOUNT.
               05  W15-GROUP       PIC  9(02).
               05  W15-REST        PIC  9(04).
           03  W15-ACNO4                         REDEFINES W15-ACCOUNT.
               05  W15-GRP         PIC  9(01).
               05  FILLER          PIC  9(05).
           03  W15-ACNME           PIC  X(30).

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(07)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  FILLER          PIC  9(01).
               05  W25-WHOLE       PIC  9(06).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(06).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-PASS.
               05  W25-PASS1       PIC  9(03).
               05  W25-PASS2       PIC  9(03).
           03  W25-TIME            PIC  9(08).
           03  W25-TRED                          REDEFINES W25-TIME.
               05  FILLER          PIC  9(02).
               05  W25-FACT        PIC  9(06).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-SLEDG           PIC  9(03)    COMP-3 OCCURS 10.
           03  W75-GNO             PIC  9(06)    COMP-5 OCCURS 18.

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

       01  S10.
           03  LINE  5 COLUMN 31 VALUE "SALES LEDGER OPTIONS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Black.
               05  LINE  7 COLUMN 12 VALUE " aintain,  rint or  xit ? > <".
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE "M".
               05          COLUMN 22 VALUE           "P".
               05          COLUMN 31 VALUE                    "E".

       01  S12.
           03  LINE  6 COLUMN 35 VALUE                              "SALES LEDGER" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  6 VALUE "Ledger No....           Description...                               ".
               05  LINE  9 COLUMN  6 VALUE "                                                                     ".
               05  LINE 10 COLUMN  6 VALUE "General Ledger Numbers:                                              ".
               05  LINE 11 COLUMN  6 VALUE "  Income G/L.           Discount G/L..                               ".
               05  LINE 12 COLUMN  6 VALUE "  Stock G/L..           Cost-Of-Sales.                               ".
               05  LINE 13 COLUMN  6 VALUE "                                                                     ".
               05  LINE 14 COLUMN  6 VALUE "Markup Percentages:                                                  ".
               05  LINE 15 COLUMN  6 VALUE "  Retail.....           Cash..........           Wholesale.....      ".
               05  LINE 16 COLUMN  6 VALUE "                                                                     ".
               05  LINE 17 COLUMN  6 VALUE "Discount Rates (Debtors):                        Alternate Rates     ".
               05  LINE 18 COLUMN  6 VALUE "  Rate  1....           Rate  2.......           For Rate.           ".
               05  LINE 19 COLUMN  6 VALUE "  Rate  3....           Rate  4.......           Alt Rate.           ".
               05  LINE 20 COLUMN  6 VALUE "  Rate  5....           Rate  6.......           For Rate.           ".
               05  LINE 21 COLUMN  6 VALUE "  Rate  7....           Rate  8.......           Alt Rate.           ".
               05  LINE 22 COLUMN  6 VALUE "  Rate  9....           Rate 10.......                               ".
               05  LINE 23 COLUMN  6 VALUE "                                                                     ".
               05  LINE 24 COLUMN  6 VALUE "     Ledger Codes XXXX, YYYY & ZZZZ are reserved for system use      " FOREGROUND-COLOR Blue.
               05  LINE 25 COLUMN  6 VALUE "     Codes starting with VAT are reserved for Value Added Tax        " FOREGROUND-COLOR Blue.

       01  S12A.
           03  LINE  6 COLUMN 33 VALUE                            "VALUE ADDED TAX" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  8 COLUMN  6 VALUE "Ledger No....           Description...                               ".
               05  LINE  9 COLUMN  6 VALUE "                                                                     ".
               05  LINE 10 COLUMN  6 VALUE "General Ledger Numbers:                                              ".
               05  LINE 11 COLUMN  6 VALUE "  Output VAT.                                                        ".
               05  LINE 12 COLUMN  6 VALUE "  Input VAT..                                                        ".

       01  S25.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S25A.
                   07  LINE  8 COLUMN 19 PIC X(04)  USING W10-LEDG AUTO.
               05  S25B.
                   07  LINE  8 COLUMN 44 PIC X(30)  USING DPT-DESC AUTO.
               05  S25C.
                   07  LINE 11 COLUMN 19 PIC Z(05)9 USING DPT-SALE-GL AUTO.
               05  S25D.
                   07  LINE 11 COLUMN 44 PIC Z(05)9 USING DPT-DISC-GL AUTO.
               05  S25E.
                   07  LINE 12 COLUMN 19 PIC Z(05)9 USING DPT-PUR-GL AUTO.
               05  S25F.
                   07  LINE 12 COLUMN 44 PIC Z(05)9 USING DPT-COST-GL AUTO.
               05  S25G.
                   07  LINE 15 COLUMN 19 PIC ZZ9.99 USING DPT-MARKUP AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-CMRKUP AUTO.
                   07          COLUMN 69 PIC ZZ9.99 USING DPT-WMRKUP AUTO.
                   07  LINE 18 COLUMN 19 PIC ZZ9.99 USING DPT-DD1 AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-DD2 AUTO.
                   07  LINE 19 COLUMN 19 PIC ZZ9.99 USING DPT-DD3 AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-DD4 AUTO.
                   07  LINE 20 COLUMN 19 PIC ZZ9.99 USING DPT-DD5 AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-DD6 AUTO.
                   07  LINE 21 COLUMN 19 PIC ZZ9.99 USING DPT-DD7 AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-DD8 AUTO.
                   07  LINE 22 COLUMN 19 PIC ZZ9.99 USING DPT-DD9 AUTO.
                   07          COLUMN 44 PIC ZZ9.99 USING DPT-DD10 AUTO.
               05  S25H.
                   07  LINE 18 COLUMN 64 PIC 99     USING DPT-ALT-DD1 AUTO.
                   07  LINE 19 COLUMN 64 PIC ZZ9.99 USING DPT-DD11 AUTO.
                   07  LINE 20 COLUMN 64 PIC 99     USING DPT-ALT-DD2 AUTO.
                   07  LINE 21 COLUMN 64 PIC ZZ9.99 USING DPT-DD12 AUTO.

       01  S26.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S26A.
                   07  LINE  8 COLUMN 19 PIC X(04)  USING W10-LEDG AUTO.
               05  S26B.
                   07  LINE  8 COLUMN 44 PIC X(30)  USING DPT-DESC AUTO.
               05  S26C.
                   07  LINE 11 COLUMN 19 PIC Z(05)9 USING DPT-SALE-GL AUTO.
               05  S26E.
                   07  LINE 12 COLUMN 19 PIC Z(05)9 USING DPT-PUR-GL AUTO.

       COPY "DEPART.CRT".

       COPY "LEDLUP.CRT".

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
           IF LS0-PALEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM BA000.
           IF LS0-LEV > 0
               CLOSE GACCNT.

       AA49.
             EXIT PROGRAM.

       COPY "APAC.HLP".

       AA350-LOOKUP    SECTION.
       AA350.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             PERFORM GACCNT-LOOKUP.
             PERFORM RESTORE-SCREEN.
       AA399.
             EXIT.

       COPY "AA900.ALM".

       COPY "LOCKED.RC2".

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        OPT-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP3".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "GACCNT.RD".

       COPY "PARAM.RD2".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "DEPART.LUP".

       COPY "GACCNT.LUP".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³          S A L E S   L E D G E R   O P T I O N S          ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA000        SECTION.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S10.
             MOVE "M"                TO WS-OPTION.
             MOVE 7                  TO WS-MES-LINE.
             MOVE 39                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE "d"                TO WS-INSTR.
           IF WS-OPTION = X"EC"
               GO TO BA999.
           IF WS-OPTION = "M"
               PERFORM YA000
           ELSE
           IF WS-OPTION = "P"
               PERFORM YB000
           ELSE
               GO TO BA999.
             GO TO BA00.
       BA999.
             EXIT.

       YA000        SECTION 5.
      *
      *  ***    *   *     *****  ***    *     ***** ****   ***  ***** ****
      * *   *  * *  *     *     *   *   *     *     *   * *   * *     *   *
      * *     *   * *     *     *       *     *     *   * *     *     *   *
      *  ***  ***** *     ***    ***    *     ***   *   * *     ***   ****
      *     * *   * *     *         *   *     *     *   * *  ** *     *   *
      * *   * *   * *     *     *   *   *     *     *   * *   * *     *   *
      *  ***  *   * ***** *****  ***    ***** ***** ****   ***  ***** *   *
      *
       YA75-SALES.

       YA80.
             PERFORM ERASE-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 6                  TO SHADE-COL.
             MOVE 70                 TO SHADE-WIDTH.
             MOVE 18                 TO SHADE-LINES.
             DISPLAY S12.
             PERFORM SCREEN-SHADOW.
             MOVE ZERO               TO WS-IND.
             MOVE SPACES             TO W10-LEDG.
             INITIALIZE DPT-RECORD.
             DISPLAY S25.
             DISPLAY "F2"                     AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Sales Ledger Lookup"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       YA85.
             ACCEPT S25A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F2-KEY     PERFORM SAVE-SCREEN
                                 PERFORM DEPART-VIEW
                                 PERFORM RESTORE-SCREEN
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO YA85.
             PERFORM CLEAR-L50.
           IF W10-LEDG = SPACES
               GO TO YA999.
           IF W10-GROUP = "VAT"
               GO TO YA200.
             DISPLAY S12A.
             MOVE W10-LEDG           TO DPT-CODE.
             PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE 0                TO WS-F-ERROR
               MOVE 1                TO WS-IND.
             DISPLAY S25.
             DISPLAY "F1"                            AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F3"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               "=Delete - "                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                         "Esc"               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                            "ape to exit"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       YA90.
             ACCEPT S25B.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F3-KEY     GO TO YA130
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA90
               END-EVALUATE.
           IF (LS0-LEV = 0) AND (WS-IND = 1)
               MOVE ZERO             TO DPT-SALE-GL DPT-DISC-GL DPT-COST-GL DPT-PUR-GL
               GO TO YA120.

       YA95.
             DISPLAY "F1"                                               AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F2"                                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               " to do Account Lookup - "                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                       "Esc"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                          "ape to exit"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       YA100.
             PERFORM YA95.
             ACCEPT S25C.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-SALE-GL  TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA100
               END-EVALUATE
               MOVE W15-ACCOUNT      TO DPT-SALE-GL
               DISPLAY S25C
               IF W15-ACCOUNT = ZERO
                   GO TO YA100
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA90
                 WHEN DOWN-KEY   GO TO YA105
               END-EVALUATE.
           IF DPT-SALE-GL = ZERO
               GO TO YA105.
             MOVE DPT-SALE-GL        TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA100.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA100.

       YA105.
             PERFORM YA95.
             ACCEPT S25D.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-DISC-GL  TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA105
               END-EVALUATE
               MOVE W15-ACCOUNT      TO DPT-DISC-GL
               DISPLAY S25D
               IF W15-ACCOUNT = ZERO
                   GO TO YA105
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA100
                 WHEN DOWN-KEY   GO TO YA110
               END-EVALUATE.
           IF DPT-DISC-GL = ZERO
               GO TO YA110.
             MOVE DPT-DISC-GL        TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA105.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA105.

       YA110.
               PERFORM YA95.
               ACCEPT S25E.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-PUR-GL   TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA110
               END-EVALUATE
               MOVE W15-ACCOUNT      TO DPT-PUR-GL
               DISPLAY S25E
               IF W15-ACCOUNT = ZERO
                   GO TO YA110
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA105
                 WHEN DOWN-KEY   GO TO YA115
               END-EVALUATE.
           IF DPT-PUR-GL = ZERO
               GO TO YA115.
             MOVE DPT-PUR-GL         TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA110.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO YA110.

       YA115.
             PERFORM YA95.
             ACCEPT S25F.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-COST-GL  TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA115
               END-EVALUATE
               MOVE W15-ACCOUNT  TO DPT-COST-GL
               DISPLAY S25F
               IF W15-ACCOUNT = ZERO
                   GO TO YA115
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA110
                 WHEN DOWN-KEY   GO TO YA120
               END-EVALUATE.
           IF DPT-COST-GL = ZERO
               GO TO YA120.
            MOVE DPT-COST-GL         TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA115.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA115.

       YA120.
             PERFORM CLEAR-L50.
             ACCEPT S25G.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA120
               END-EVALUATE
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     IF LS0-LEV > 0
                                     GO TO YA115
                                 ELSE
                                     GO TO YA90
                                 END-IF
               END-EVALUATE.

       YA125.
             PERFORM CLEAR-L50.
             ACCEPT S25H.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA125
               END-EVALUATE
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     IF LS0-LEV > 0
                                     GO TO YA120
                                 ELSE
                                     GO TO YA90
                                 END-IF
               END-EVALUATE.
           IF DPT-ALT-DD1 > 10 OR DPT-ALT-DD2 > 10
               MOVE "Alternate Discount Rate code > 10"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA125.

       YA127.
             MOVE W10-LEDG           TO DPT-SALES-NO.
           IF WS-IND = 1
               PERFORM WRITE-DEPART THRU WRITE-DEPART-EXIT
           ELSE
               PERFORM REWRITE-DEPART-UNLOCK THRU WRITE-DEPART-EXIT.
           IF WS-F-ERROR = 7
               IF WS-IND = 1
                   MOVE "Error while adding record"    TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
               ELSE
                   MOVE "Error while updating record"  TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
               END-IF
               GO TO YA999.
             GO TO YA80.

       YA130.
             MOVE "Delete department (Y/N)?  [N]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO LS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               GO TO YA80.
             DELETE DEPART.
           IF WS-STAT1 NOT = "0"
               MOVE 7                TO WS-F-ERROR
               PERFORM WRITE-ERROR.
             GO TO YA80.

       YA200.
             PERFORM ERASE-SCREEN.
             MOVE 8                  TO SHADE-ROW.
             MOVE 6                  TO SHADE-COL.
             MOVE 70                 TO SHADE-WIDTH.
             MOVE 5                  TO SHADE-LINES.
             DISPLAY S12A.
             PERFORM SCREEN-SHADOW.
             MOVE W10-LEDG           TO DPT-CODE.
             PERFORM READ-DEPART-LOCK THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE 0                TO WS-F-ERROR
               MOVE 1                TO WS-IND.
             DISPLAY S26.
             DISPLAY "F1"                            AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F3"                            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               "=Delete - "                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                         "Esc"               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                            "ape to exit"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       YA205.
             ACCEPT S26B.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F3-KEY     GO TO YA130
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA205
               END-EVALUATE.
           IF (LS0-LEV = 0) AND (WS-IND = 1)
               MOVE ZERO             TO DPT-SALE-GL DPT-DISC-GL DPT-COST-GL DPT-PUR-GL
               GO TO YA120.

       YA210.
             DISPLAY "F1"                                               AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F2"                                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               " to do Account Lookup - "                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                       "Esc"                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                          "ape to exit"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.



       YA215.
             PERFORM YA210.
             ACCEPT S26C.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-SALE-GL  TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA215
               END-EVALUATE
               MOVE W15-ACCOUNT      TO DPT-SALE-GL
               DISPLAY S26C
               IF W15-ACCOUNT = ZERO
                   GO TO YA215
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA205
                 WHEN DOWN-KEY   GO TO YA220
               END-EVALUATE.
           IF DPT-SALE-GL = ZERO
               GO TO YA220.
             MOVE DPT-SALE-GL        TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA215.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA215.

       YA220.
               PERFORM YA210.
               ACCEPT S26E.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO YA999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                 WHEN F2-KEY     MOVE DPT-PUR-GL   TO W15-ACCOUNT
                                 PERFORM AA350
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO YA220
               END-EVALUATE
               MOVE W15-ACCOUNT      TO DPT-PUR-GL
               DISPLAY S26E
               IF W15-ACCOUNT = ZERO
                   GO TO YA220
               END-IF
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     GO TO YA215
                 WHEN DOWN-KEY   GO TO YA127
               END-EVALUATE.
           IF DPT-PUR-GL = ZERO
               GO TO YA127.
             MOVE DPT-PUR-GL         TO W15-ACCOUNT G-AC.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA220.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO YA220.
             GO TO YA127.

       YA999.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³          S A L E S   L E D G E R   L I S T I N G          ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       YB000        SECTION 5.
       YB00.
             DISPLAY "Busy Printing" AT 1212 WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.

       YB025-SLD.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "PAGE:"            TO REP-PAGE.
             ADD  1                  TO WS-PAGE.
             MOVE WS-PAGE            TO REP-P-NO.
             MOVE "DATE:"            TO REP-DATE.
             MOVE W12-TODAY          TO REP-DTE.
             MOVE "SALES LEDGER CODES" TO REP-COMPANY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "CODE"             TO REP-HEAD1.
             MOVE "DESCRIPTION"      TO REP-COMPANY.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL2.
             MOVE "D"                TO W02-PRN-TYPE.

       YB28.
             MOVE "   !"             TO DPT-CODE.
             PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO YB35.

       YB030.
             PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               GO TO YB35.
             MOVE DPT-CODE           TO REP-HEAD1.
             MOVE DPT-DESC           TO REP-COMPANY.
           IF W02-LINAGE < (WS-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM YB025-SLD.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****"  TO REP-DETAIL2
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               GO TO YB999.
             GO TO YB030.

       YB35.
             MOVE "**** END REPORT ****" TO REP-DETAIL2.
             MOVE 99                 TO WS-ADVANCE.
             MOVE "E"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       YB999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 8.
       ZA000-OPEN.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE W12-MONTH          TO WS-OPEN.
           IF LS0-LEV > 0
               OPEN INPUT GACCNT.
             GO TO ZA999-EXIT.

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
               WHEN 7          MOVE W02-DEPART    TO WS-FILE
                               MOVE DPT-CODE      TO WS-KEYX
               WHEN 8          MOVE W02-GACCNT    TO WS-FILE
                               MOVE G-AC          TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "PRP003"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
