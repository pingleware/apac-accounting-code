      $set LINKCOUNT"468" GNT"STP085.GNT"
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
      *    ******   ********  *******     ****     ******   ********   *
      *   **    **     **     **    **   **  **   **    **  **         *
      *   **           **     **    **  **    **  **    **  *******    *
      *    ******      **     *******   **    **   ******         **   *
      *         **     **     **        **    **  **    **        **   *
      *   **    **     **     **         **  **   **    **  **    **   *
      *    ******      **     **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   P R O M O T I O N S   M A I N T E N A N C E    *
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
       PROGRAM-ID.     STP085.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JUNE 1999.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1999 - 2018
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
       77  WS-DISC    PIC 999V99  COMP-3.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-P1    PIC 9(02)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
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
       77  WS-GST-RATE    PIC S9(03)V99 COMP-3.
       77  WS-MONTHS    PIC S9(01)V99 COMP-3.
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(18).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-ETYPE    PIC X(01).
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-NORM         PIC  X(04).
       77  WS-COND         PIC  X(04).
       77  WS-EXPP         PIC  X(04).
       77  WS-ECAN         PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP    PIC X(01).
       77  WS-ADJ    PIC X(01).
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
       77  WS-PPASSWORD    PIC X(06).
       77  WS-PRINT    PIC 9(01).
       77  WS-LABCDE    PIC X(08).
       77  WS-PASS    PIC X(08).
       77  WS-DESC-H1    PIC X(14) VALUE "Description".
       77  WS-DESC-H2    PIC X(14) VALUE "Description 2".
       77  WS-RT-HD    PIC X(10) VALUE "-Retail   ".
       77  WS-WS-HD    PIC X(10) VALUE "-Wholesale".
       77  WS-CS-HD    PIC X(10) VALUE "-Cash sale".
       77  WS-NODEP    PIC X(30) VALUE
      "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP085".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -STOCK PROMOTIONS MAINT.".
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
    03  WS-MODULE       PIC  X(03) VALUE "STP".
    03  WS-PROG        PIC  X(03) VALUE "011".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       COPY "W05.VAT".

       COPY "W05.RND".

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

       COPY "W15.WS".

       COPY "W25.STK".

       COPY "W40.WS".

       01  W45-ATTRIB4.
    03  FILLER        PIC X(4000).

       01  W45-SCREEN4.
    03  FILLER        PIC X(4000).

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
      PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
          03  LINE  7 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 49 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
           PIC X(03) USING W10-ITM AUTO.

       01  S07-EXT.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
            PIC X(18) USING W10-EXT-ITEM AUTO.

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

       01  S20.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 11 COLUMN 20 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 12 COLUMN 20 VALUE "³ Enter price change - Password        ³".
               05  LINE 13 COLUMN 20 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S20A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN 52 PIC  X(06) USING WS-PPASSWORD AUTO SECURE.

       01  S22.
          03  LINE 12 COLUMN 42 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC  X(06)
     USING WS-PPASSWORD AUTO SECURE.
      *
      *    ****    PROMOTION PRICES DISPLAY SCREEN
      *
       01  S25.
    03  LINE  4 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
        "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "¿".
    03  LINE  5 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
        "³       S T O C K   P R O M O T I O N S        ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  6 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE
        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  7 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "Item Code                                     ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  8 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey PIC X(14)
     FROM WS-DESC-H1.
    03        COLUMN 32 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "           ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE  9 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey PIC X(14)
     FROM WS-DESC-H2.
    03        COLUMN 32 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "           ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 10 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "                   Exclusive        Inclusive ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 11 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "Promotion Price                               ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 12 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "Start Date                                    ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 13 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "Start Time                                    ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 14 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "End Date                                      ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 15 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "³".
    03        COLUMN 18 FOREGROUND-COLOR Magenta
     BACKGROUND-COLOR Grey VALUE
        "End Time                                      ".
    03        COLUMN 64 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE "³".
    03  LINE 16 COLUMN 17 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey VALUE "À".
    03        COLUMN 18 FOREGROUND-COLOR Black
     BACKGROUND-COLOR Grey VALUE
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
      *
      *    ****    PROMOTION PERIOD INPUT SCREEN
      *
       01  S26.
           03  S26A.
              05  LINE  8 COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta
               PIC X(30) FROM STK-DESC.
              05  LINE  9 COLUMN 34 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta
               PIC X(30) FROM STK-DESC2.
    03  S26B.
        05  LINE 11 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC ZZZZZZZ9.999 USING
         W10-PRO-PRICE AUTO.
    03  S26C.
        05  LINE 11 COLUMN 51 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC ZZZZZZZZ9.99 FROM
         W10-PRO-VPRICE AUTO.
    03  S26D.
        05  LINE 12 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC 99/99/9999 USING
         W15-START AUTO.
        05  LINE 13 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC 99,99 USING
         W10-PRO-START-T AUTO.
        05  LINE 14 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC 99/99/9999 USING
         W15-END AUTO.
        05  LINE 15 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta
         PIC 99,99 USING
         W10-PRO-END-T AUTO.

       01  S27.
    03  S27A.
        05  LINE 14 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZ9.999
         FROM STK-UNT(2).
        05    COLUMN 44 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZ9.999
         FROM STK-UNT(3).
        05    COLUMN 57 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZ9.999
         FROM STK-UNT(4).
    03  S27B.
        05  LINE 15 COLUMN 27 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZ9.999
         USING W10-PRO-PRICE-2 AUTO.
    03  S27C.
        05  LINE 15 COLUMN 40 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZ9.999
         USING W10-PRO-PRICE-3 AUTO.
    03  S27D.
        05  LINE 15 COLUMN 53 FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZ9.999
         USING W10-PRO-PRICE-4 AUTO.
    03  S27E.
        05  LINE 16 COLUMN 27 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZZ9.99
         USING W10-PRO-VPRICE-2 AUTO.
        05    COLUMN 40 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZZ9.99
         USING W10-PRO-VPRICE-3 AUTO.
        05    COLUMN 53 FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta PIC ZZZZZZZZ9.99
         USING W10-PRO-VPRICE-4 AUTO.


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
          IF LS-OK = "N"
               PERFORM AA500
        GO TO AA49.
            PERFORM ZA000-INIT.
            PERFORM BL000.
      CLOSE RECOVER.

       AA49.
      EXIT PROGRAM.

       COPY "AA50.LUP".

      *
      *    ****    S E C U R I T Y   V I O L A T I O N
      *
       AA500              SECTION.
       AA501.
      PERFORM SAVE-SCREEN.
      MOVE 6   TO SHADE-ROW.
      MOVE 28   TO SHADE-COL.
      MOVE 27   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0526
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "¿"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
            DISPLAY "³" AT 0626
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       " Insufficient security     "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
            DISPLAY "³" AT 0726
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       " Press ENTER to continue   "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Red
       "³"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      DISPLAY "À" AT 0826
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      PERFORM SCREEN-SHADOW.
            ACCEPT WS-OPTION AT 0752
      WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey AUTO.
      PERFORM RESTORE-SCREEN.

       AA505.
             EXIT.

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
      *    ³   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ³
      *    ³   ³           Print transactions (Y/N) [ ]         ³°°    ³
      *    ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°    ³
      *    ³     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°    ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 20) move the line number to be used to            ³
      *    ³ WS-MES-LINE.                                              ³
      *    ³                                                           ³
      *    ³ If character 48 of the message contains a value of X"FA"  ³
      *    ³ then do not display the message in a window, only get the ³
      *    ³ User reponse at the position specified by WS-MES-LINE and ³
      *    ³ WS-MES-COL.                                               ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

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

       COPY "AA300.STK".

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

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

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

       COPY "ROUND.PRC".

      /
      *    ****  P R O M O T I O N S   &   S P E C I A L S
      *
       BL000        SECTION 50.
       BL0.
             PERFORM ERASE-SCREEN.
      DISPLAY "STOCK SPECIALS & PROMOTIONS" AT 0227
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      PERFORM SAVE-SCREEN-2.
            DISPLAY S20.
      MOVE 12   TO SHADE-ROW.
      MOVE 22   TO SHADE-COL.
      MOVE 38   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
            ACCEPT S20A.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PPASSWORD
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
            MOVE 5   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-PW  TO W85-PASS.
      PERFORM RESTORE-SCREEN-2.
          IF NOT (WS-PPASSWORD = W85-SUPER(1)  OR
            W85-SUPER (9) OR
           "AICEPS")
              MOVE "Invalid password"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BL999.

       BL00.
             PERFORM RESTORE-SCREEN-2.
             DISPLAY S25.
             MOVE 5                  TO SHADE-ROW.
             MOVE 19                 TO SHADE-COL.
             MOVE 46                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             MOVE SPACES             TO W10-EXT-ITEM.
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
       BL05.
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
       GO TO BL999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO BL05
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
       GO TO BL05
        END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT AT 0734
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM
     ELSE
         DISPLAY S07 AT 0734
     END-IF
        ELSE
     DISPLAY S07 AT 0734
        END-IF
        IF W10-EXT-ITEM = SPACES
                   GO TO BL05.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACES
               GO TO BL999.

       BL10.
            PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT 0734
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM
        ELSE
           DISPLAY S07 AT 0734
        END-IF
    ELSE
        DISPLAY S07 AT 0734.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S07-EXT AT 0734
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM
     ELSE
         DISPLAY S07 AT 0734
     END-IF
        ELSE
     DISPLAY S07 AT 0734
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BL05
               ELSE
     GO TO BL10.
      DISPLAY S26A.
    IF STK-IND > 1
              MOVE "Not a Controlled or Priced item"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BL00.
      *
      *    ****    C H E C K   I F   E X I S T I N G   P R O M O
      *
      IF STK-PRO-PRICE = ZERO
   MOVE STK-CASH  TO W10-PRO-PRICE
        STK-PRO-PRICE
      ELSE
   MOVE STK-PRO-PRICE
     TO W10-PRO-PRICE.
      *
      *    ****    C H E C K   F O R   P A C K S
      *
      IF (WS-USE-PACKS = "Y") AND
  (STK-USE-PACKS = "Y")
   IF (STK-PRO-PRICE-2 = ZERO) AND
      (STK-UNT(2) NOT = ZERO)
       MOVE STK-CASH-2
     TO W10-PRO-PRICE-2
        STK-PRO-PRICE-2
   ELSE
       MOVE STK-CASH-2
     TO W10-PRO-PRICE-2
   END-IF
   IF (STK-PRO-PRICE-3 = ZERO) AND
      (STK-UNT(3) NOT = ZERO)
       MOVE STK-CASH-3
     TO W10-PRO-PRICE-3
        STK-PRO-PRICE-3
   ELSE
       MOVE STK-CASH-3
     TO W10-PRO-PRICE-3
   END-IF
   IF (STK-PRO-PRICE-4 = ZERO) AND
      (STK-UNT(4) NOT = ZERO)
       MOVE STK-CASH-4
     TO W10-PRO-PRICE-4
        STK-PRO-PRICE-4
   ELSE
       MOVE STK-CASH-4
     TO W10-PRO-PRICE-4.

       BL15.
    IF STK-TAX NOT = ZERO
        MOVE W05-VAT(STK-TAX)
                                 TO W05-RATE
        COMPUTE W10-PRO-VPRICE ROUNDED = W10-PRO-PRICE +
        (W10-PRO-PRICE * W05-RTE)
        COMPUTE W10-PRO-VPRICE-2 ROUNDED = W10-PRO-PRICE-2 +
        (W10-PRO-PRICE-2 * W05-RTE)
        COMPUTE W10-PRO-VPRICE-3 ROUNDED = W10-PRO-PRICE-3 +
        (W10-PRO-PRICE-3 * W05-RTE)
        COMPUTE W10-PRO-VPRICE-4 ROUNDED = W10-PRO-PRICE-4 +
        (W10-PRO-PRICE-4 * W05-RTE)
    ELSE
        MOVE W10-PRO-PRICE
     TO W10-PRO-VPRICE
        MOVE W10-PRO-PRICE-2
     TO W10-PRO-VPRICE-2
        MOVE W10-PRO-PRICE-3
     TO W10-PRO-VPRICE-3
        MOVE W10-PRO-PRICE-4
     TO W10-PRO-VPRICE-4.

       BL20.
    IF (STK-PRO-END = ZERO) OR
       (STK-PRO-END < W12-T-YMD)
        MOVE W12-T-YMD  TO W10-PRO-START
        W10-PRO-END
        MOVE W12-TODAY  TO W15-START
        W15-END
        MOVE ZERO  TO W10-PRO-START-T
        W10-PRO-END-T
    ELSE
        MOVE STK-PRO-START
     TO W10-PRO-START
        W15-S-YMD
      *
      *    ****    S W I T C H  D A T E   T O  DDMMYYYY
      *
        MOVE W15-SCC  TO W15-SCENT
              MOVE W15-SYY  TO W15-SYEAR
              MOVE W15-SMM  TO W15-SMNTH
              MOVE W15-SDD  TO W15-SDAY
        MOVE STK-PRO-START-T
     TO W10-PRO-START-T
        MOVE STK-PRO-END  TO W10-PRO-END
        W15-E-YMD
      *
      *    ****    S W I T C H  D A T E   T O  DDMMYYYY
      *
        MOVE W15-ECC  TO W15-ECENT
              MOVE W15-EYY  TO W15-EYEAR
              MOVE W15-EMM  TO W15-EMNTH
              MOVE W15-EDD  TO W15-EDAY
        MOVE STK-PRO-END-T
     TO W10-PRO-END-T.

       BL25.
      DISPLAY S26.

       BL30.
      ACCEPT S26B.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BL00
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BL30
        END-EVALUATE.
    IF W10-PRO-PRICE < STK-COST
        PERFORM BM000
        IF WS-ERROR = 9
     GO TO BL30.
    IF STK-TAX NOT = ZERO
        COMPUTE W10-PRO-VPRICE ROUNDED = W10-PRO-PRICE +
        (W10-PRO-PRICE * W05-RTE).
      DISPLAY S26C.
    IF (WS-USE-PACKS = "Y") AND
       (STK-USE-PACKS = "Y")
        PERFORM BL45 THRU BL99.

       BL35.
      DISPLAY "Time HH,MM (2400 Clock)" AT 1928
        WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      ACCEPT S26D.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BL30
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BL35
        END-EVALUATE.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BL35.
      PERFORM CLEAR-L50.
             MOVE W15-START          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Start DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Start DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Start MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BL35.
             MOVE W20-DTE            TO W15-S-YMD.
             MOVE W15-END            TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid End DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid End DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid End MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BL35.
             MOVE W20-DTE        TO W15-E-YMD.
    IF W15-E-YMD < W15-S-YMD
        MOVE "End date is before start date"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BL35.
      MOVE W15-S-YMD  TO W10-PRO-START.
      MOVE W15-E-YMD  TO W10-PRO-END.
    IF W10-PRO-START-T = ZERO OR
       W10-PRO-START-T > 2400
              MOVE "Start time" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BL35.
    IF W10-PRO-END-T = ZERO OR
       W10-PRO-END-T > 2400
              MOVE "End time" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BL35.
    IF W15-E-YMD = W15-S-YMD
        IF NOT (W10-PRO-END-T > W10-PRO-START-T)
     MOVE "End time not after start time"
     TO WS-ERR-MES
           PERFORM ERROR-MESSAGE
     GO TO BL35.
      DISPLAY WS-BLNK78 AT 1902 WITH FOREGROUND-COLOR Cyan.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BL30.
            PERFORM AY60 THRU AY999.
      MOVE 0   TO WS-ACTION.
             PERFORM AY05 THRU AY59.
            MOVE W10-PRO-PRICE  TO STK-PRO-PRICE.
            MOVE W10-PRO-START  TO STK-PRO-START.
            MOVE W10-PRO-START-T
     TO STK-PRO-START-T.
            MOVE W10-PRO-END  TO STK-PRO-END.
            MOVE W10-PRO-END-T  TO STK-PRO-END-T.
            PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             PERFORM AY70 THRU AY999.
      MOVE "N"   TO WS-OPTION.

       BL40.
            MOVE "'N'ext item or 'E'nd  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 17   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "E" OR "N")
        GO TO BL40.
          IF WS-OPTION = "E"
               GO TO BL999.
            GO TO BL00.
      *
      *    ****    P A C K   P R I C E S
      *
       BL45.
      MOVE W43-SCREEN2  TO W45-SCREEN4.
      MOVE W42-ATTRIB2  TO W45-ATTRIB4.
      PERFORM SAVE-SCREEN-2.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1215 WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Magenta
     "¿" WITH FOREGROUND-COLOR Magenta HIGHLIGHT BACKGROUND-COLOR Magenta.
      DISPLAY
     "³" AT 1315 WITH FOREGROUND-COLOR Black
        BACKGROUND-COLOR Magenta
     "                 Pack 1       Pack 2       Pack 3 "
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
     "³" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY
     "³" AT 1415 WITH FOREGROUND-COLOR Black
        BACKGROUND-COLOR Magenta
     " Units                                            "
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
     "³" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY
     "³" AT 1515 WITH FOREGROUND-COLOR Black
        BACKGROUND-COLOR Magenta
     " Exclusive                                        "
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
     "³" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY
     "³" AT 1615 WITH FOREGROUND-COLOR Black
        BACKGROUND-COLOR Magenta
     " Inclusive                                        "
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta
     "³" WITH FOREGROUND-COLOR Magenta HIGHLIGHT
       BACKGROUND-COLOR Magenta.
      DISPLAY
     "À" AT 1715 WITH FOREGROUND-COLOR Black
        BACKGROUND-COLOR Magenta
     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
      WITH FOREGROUND-COLOR Magenta HIGHLIGHT
    BACKGROUND-COLOR Magenta.
      MOVE 13   TO SHADE-ROW.
      MOVE 17   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
            COMPUTE W10-MSELL = STK-COST * STK-UNT(2).
            COMPUTE W10-MCOST = STK-COST * STK-UNT(3).
            COMPUTE W10-MQUANT = STK-COST * STK-UNT(4).
      DISPLAY S27.

       BL50.
    IF (STK-UNT(2) = ZERO)
        GO TO BL65.
      ACCEPT S27B.
    IF W10-PRO-PRICE-2 < W10-MSELL
        PERFORM BM000
        IF WS-ERROR = 9
     GO TO BL50.

       BL55.
    IF (STK-UNT(3) = ZERO)
        GO TO BL65.
      ACCEPT S27C.
    IF W10-PRO-PRICE-3 < W10-MSELL
        PERFORM BM000
        IF WS-ERROR = 9
     GO TO BL55.

       BL60.
    IF (STK-UNT(4) = ZERO)
        GO TO BL65.
      ACCEPT S27D.
    IF W10-PRO-PRICE-3 < W10-MSELL
        PERFORM BM000
        IF WS-ERROR = 9
     GO TO BL60.

       BL65.
      PERFORM BL15.
      DISPLAY S27E.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BL50.
      PERFORM RESTORE-SCREEN-2.
      MOVE W45-SCREEN4  TO W43-SCREEN2.
      MOVE W45-ATTRIB4  TO W42-ATTRIB2.

       BL99.
      EXIT.

       BL999.
            EXIT.
      /
      *    ****    P R I C E   L E S S  T H A N   C O S T
      *
       BM000        SECTION 50.
       BM00.
      PERFORM SAVE-SCREEN-3.

       BM05.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1024
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      DISPLAY "³ Promotion Price less than Cost " AT 1124
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      DISPLAY "³        Password                " AT 1224
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      DISPLAY "À" AT 1324
        WITH FOREGROUND-COLOR Red HIGHLIGHT
      BACKGROUND-COLOR Red
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Red.
      MOVE 11   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 32   TO SHADE-WIDTH.
      MOVE 3   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE SPACES  TO WS-PPASSWORD.
      MOVE ZERO   TO WS-ERROR.

       BM10.
      ACCEPT S22.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING WS-PPASSWORD
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
            MOVE 5   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-PW  TO W85-PASS.
      PERFORM RESTORE-SCREEN-3.
          IF NOT (WS-PPASSWORD = W85-SUPER(1) OR
            W85-SUPER(9) OR
           "AECIRP")
              MOVE "Invalid password"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        MOVE 9   TO WS-ERROR.

       BM999.
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
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
      MOVE PAR-CASES  TO WS-USE-CASES.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.

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
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-ROUND  TO W05-ROUND.
             MOVE PAR-ROUND-VAL  TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO         TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO         TO W05-TQTR-VAL.
             MOVE PAR-PW         TO W85-PASS.
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
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta.
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
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP085"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
