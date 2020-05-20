      $set LINKCOUNT"512" GNT"STP064.GNT"
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
      *    ******   ********  *******     ****     ******       ***    *
      *   **    **     **     **    **   **  **   **    **     ****    *
      *   **           **     **    **  **    **  **          ** **    *
      *    ******      **     *******   **    **  ********   **  **    *
      *         **     **     **        **    **  **    **  ********   *
      *   **    **     **     **         **  **   **    **       **    *
      *    ******      **     **          ****     ******       ****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * S T O C K   L O C A T I O N   W / H O U S E   T R A N S F E R S*
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
       PROGRAM-ID.     STP064.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   APRIL 1996.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2018
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

                You should have received a COPY of the GNU General
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

       COPY "STKMEM.SL".

       COPY "STOCK.SL".

       COPY "WARHSE.SL".

       COPY "WSTOCK.SL".

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

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "STKMEM.FDE".

       COPY "STOCK.FDE".

       COPY "WARHSE.FDE".

       COPY "WSTOCK.FDE".

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
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-RECKEY               PIC  9(06)    COMP-5.
       77  WS-RECOVER              PIC  9(06)    COMP-5.
       77  WS-RECORDS              PIC  9(06)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK                PIC  X(01).
       77  WS-ETYPE                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-AMEND                PIC  9(01)           VALUE 0.
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-LEDG                 PIC  9(03).
       77  WS-COMP                 PIC  9(01).
       77  WS-IND                  PIC  9(01)           VALUE ZERO.
       77  WS-TYPE                 PIC  X(01).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-PASS                 PIC  X(08).
       77  WS-DESC-H1              PIC  X(14)           VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)           VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)           VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)           VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)           VALUE "-Cash sale".
       77  WS-ERR1                 PIC  X(20)           VALUE "Quantity > Available".
       77  WS-ERR3                 PIC  X(15)           VALUE "Data incomplete".
       77  WS-ERR7                 PIC  X(23)           VALUE "Address > 67 characters".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-STOCK-LUP           PIC  X(12)           VALUE "STP\STPLOOK".
       77  PRG-WARHS-LUP           PIC  X(12)           VALUE "STP\WARLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP064".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY ITEM TRANSFERS LOCATION".
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
           03  WS-PROG             PIC  X(03) VALUE "064".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.VAT".

       COPY "W10.STK".

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

       01  W15-RECORD1.
           03  W15-KEY.
               05  W15-WAR         PIC  X(06).
               05  W15-CODE        PIC  X(14).
               05  W15-ITM         PIC  X(03).
           03  W15-BIN             PIC  X(04).
           03  W15-QUANT           PIC S9(07)V9(04) COMP-3.
           03  W15-MTD             PIC S9(09)V99  COMP-3.
           03  W15-YTD             PIC S9(09)V99  COMP-3.
           03  W15-MTDV            PIC S9(09)V99  COMP-3.
           03  W15-YTDV            PIC S9(09)V99  COMP-3.
           03  FILLER              PIC  X(03).

       01  W16-RECORD1.
           03  W16-KEY.
               05  W16-WAR         PIC  X(06).
               05  W16-EXT-CODE.
                   07  W16-CODE    PIC  X(14).
                   07  W16-ITM     PIC  X(03).
                   07  FILLER      PIC  X(01).
           03  W16-BIN             PIC  X(04).
           03  W16-QUANT           PIC S9(07)V9(04) COMP-3.
           03  W16-MTD             PIC S9(09)V99  COMP-3.
           03  W16-YTD             PIC S9(09)V99  COMP-3.
           03  W16-MTDV            PIC S9(09)V99  COMP-3.
           03  W16-YTDV            PIC S9(09)V99  COMP-3.
           03  FILLER              PIC  X(03).

       COPY "W40.WS".

       01  W40-WAREHOUSE.
           03  W40-NAME1.
               05  W40-NAME        PIC  X(40).
           03  W40-NAME2                          REDEFINES W40-NAME1.
               05  W40-NCHAR       PIC  X(01)     OCCURS 40.
           03  W40-CONTACT         PIC  X(30).
           03  W40-CODE            PIC  X(06).
           03  W40-CODE1           PIC  X(06).
           03  W40-CODE2           PIC  X(06).
           03  W40-ADD.
               05  W40-ADDRESS1.
                   07  W40-ADD1    PIC  X(30).
                   07  W40-ADD2    PIC  X(30).
                   07  W40-ADD3    PIC  X(30).
                   07  W40-ADD4    PIC  X(30).
               05  W40-ADDRESS2                   REDEFINES W40-ADDRESS1.
                   07  W40-ACHAR   PIC  X(01)     OCCURS 120.
           03  W40-POST            PIC  9(04).
           03  W40-TEL             PIC  X(14).
           03  W40-FAX             PIC  X(14).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-ADD.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC X(01)      OCCURS 70.
           03  W60-ADDR                           REDEFINES W60-ADDRESS.
               05  W60-ADD         PIC X(60).
               05  FILLER          PIC X(10).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)     COMP-5.
           03  W75-S1              PIC  9(02)     COMP-5.
           03  W75-S2              PIC  9(02)     COMP-5.
           03  W75-WHOUSE                         OCCURS 18.
               05  W75-WHCDE       PIC  X(06).
           03  W75-ITEMS                          OCCURS 18.
               05  W75-CODE        PIC  X(14).
               05  W75-ITM         PIC  X(03).
               05  W75-SELL        PIC  9(07)V99  COMP-3.
               05  W75-VSELL       PIC  9(07)V99  COMP-3.
               05  W75-CASH        PIC  9(07)V99  COMP-3.
               05  W75-VCASH       PIC  9(07)V99  COMP-3.
               05  W75-WSALE       PIC  9(07)V99  COMP-3.
               05  W75-VWSALE      PIC  9(07)V99  COMP-3.
           03  W75-SLEDG           PIC  9(03)     COMP-3 OCCURS 10.

       01  W80-EDIT.
           03  W80-Q1.
               05  W80-S7V4        PIC  Z(06)9.9999-.

       01  W95-COMP                PIC  X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                            REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V5                            REDEFINES W100-V2.
               05  W100-QNT        PIC  Z(05)9.99-.

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

       01  S01.
           03  LINE  2 COLUMN 29 VALUE                         "STOCK WAREHOUSE TRANSFER" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  4 COLUMN  5 VALUE "Item Code   ".
           03  LINE  5 COLUMN  5 VALUE "Description ".
           03  LINE  7 COLUMN  5 VALUE "Transfer Details"                                 FOREGROUND-COLOR Brown HIGHLIGHT.
           03  LINE  8 COLUMN  5 VALUE "Source      ".
           03  LINE  9 COLUMN  5 VALUE "Quantity    ".
           03  LINE 11 COLUMN  5 VALUE "Destination ".
           03  LINE 12 COLUMN  5 VALUE "Quantity    ".
           03  LINE 14 COLUMN  5 VALUE "Transfer Qty".
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Black.
               05  LINE 16 COLUMN  7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE 17 COLUMN  7 VALUE "³ Source and Destination refer to LOCATION CODES and keying spaces ³".
               05  LINE 18 COLUMN  7 VALUE "³ into either of these fields refers to the Database Stock Record. ³".
               05  LINE 19 COLUMN  7 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S01-DATA.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S02.
                   07 PIC X(06) USING W40-CODE AUTO.

               05  S03.
                   07  LINE 11 COLUMN 18 PIC X(06) USING W40-CODE2 AUTO.

               05  S04.
                   07  LINE 14 COLUMN 18 PIC Z(06)9.9999 USING W10-QNT AUTO.

               05  S05.
                   07  LINE  4 COLUMN 18 PIC X(14) USING W10-ITEM AUTO.

               05  S05-ITM.
                   07  LINE  4 COLUMN 18 PIC X(14) USING W10-ITEM AUTO.
                   07          COLUMN 33 PIC X(03) USING W10-ITM AUTO.

               05  S05-EXT.
                   07  LINE  4 COLUMN 18 PIC X(18) USING W10-EXT-ITEM AUTO.

               05  S05-3.
                   07  LINE  4 COLUMN 18 PIC Z(03) USING W10-I3 AUTO.

               05  S05-4.
                   07  LINE  4 COLUMN 18 PIC Z(04) USING W10-I4 AUTO.

               05  S05-5.
                   07  LINE  4 COLUMN 18 PIC Z(05) USING W10-I5 AUTO.

               05  S05-6.
                   07  LINE  4 COLUMN 18 PIC Z(06) USING W10-I6 AUTO.

               05  S05-7.
                   07  LINE  4 COLUMN 18 PIC Z(07) USING W10-I7 AUTO.

               05  S05-8.
                   07  LINE  4 COLUMN 18 PIC Z(08) USING W10-I8 AUTO.

               05  S05-9.
                   07  LINE  4 COLUMN 18 PIC Z(09) USING W10-I9 AUTO.

               05  S05-10.
                   07  LINE  4 COLUMN 18 PIC Z(10) USING W10-I10 AUTO.

               05  S05-11.
                   07  LINE  4 COLUMN 18 PIC Z(11) USING W10-I11 AUTO.

               05  S05-12.
                   07  LINE  4 COLUMN 18 PIC Z(12) USING W10-I12 AUTO.

               05  S05-13.
                   07  LINE  4 COLUMN 18 PIC Z(13) USING W10-I13 AUTO.

               05  S05-14.
                   07  LINE  4 COLUMN 18 PIC Z(14) USING W10-I14 AUTO.

       COPY "WARLUP.CRT".

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
       AA000         SECTION.
       AA00.
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
             PERFORM CB000.
             CLOSE RECOVER.
       AA49.
             EXIT PROGRAM.

       COPY "AA50SW.LUP".

       COPY "AA300.STK".

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

       COPY "LOCKED.REC".

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

       COPY "WARHSE.RD".

       COPY "WSTOCK.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000             SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STOCK.WR".

       COPY "WARHSE.WR".

       COPY "WSTOCK.WR".

       COPY "WARHSE.LUP".

       COPY "APAC.HLP".

      *
      *       ****   *****  ***    ***   *   *  *****  ****   *   *
      *       *   *  *     *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***   *      *   *  *   *  ***    ****    * *
      *       *   *  *     *   *  *   *   * *   *      *   *    *
      *       *   *  *****  ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED          ÄÄ     (REWRITE)     ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ     (DELETE)      ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)       ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000           SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                TO REC-FILE.
             MOVE WS-PARKEY         TO REC-KEY.
             MOVE PAR-RECORD1       TO REC-PARAM.
             GO TO AY50.
      *    *************************************************************
      *    ****                 S T O C K   F I L E                 ****
      *    *************************************************************
       AY05.
             MOVE 05                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
           IF WS-ACTION = 9
               MOVE 0                TO REC-TYPE
           ELSE
               MOVE WS-ACTION        TO REC-TYPE.
             MOVE STK-RECORD1        TO REC-STOCKF.
             GO TO AY50.
      *    *************************************************************
      *    ****             W A R E H O U S E   F I L E             ****
      *    *************************************************************
       AY051.
             MOVE 51                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-WARHSE-LOCK THRU READ-WARHSE-EXIT.
               MOVE WAR-RECORD1      TO REC-WARHSE.
             GO TO AY50.
             
      *    *************************************************************
      *    ****       W A R E H O U S E   S T O C K   F I L E       ****
      *    *************************************************************
       AY052.
             MOVE 52                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
             MOVE WST-RECORD1        TO REC-WSTOCK.
             GO TO AY50.
      *    *************************************************************
      *    ****       N E T W O R K   (S H A R E D)   F I L E       ****
      *    *************************************************************
       AY37.
             MOVE 37                 TO REC-FILE.
             MOVE WS-SHARED          TO REC-KEY.
             MOVE SHR-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50 THRU AY59.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO             TO WS-RECOVER.
             GO TO AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         W R I T E   R E C O V E R Y   R E C O R D         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY50.
             ADD 1                   TO WS-RECOVER.
             MOVE WS-RECOVER         TO WS-RECKEY.
             MOVE WS-TRANS           TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *    *************************************************************
      *    ****        Start processing transaction
      *    *************************************************************
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
             MOVE SHR-STOCK          TO WS-STOCK.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *    *************************************************************
      *     Q   F U L L  -  W A I T   F O R   A  C O U N T  O F   400
      *    *************************************************************
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
             MOVE "ST"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the STOCK control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
           IF WS-STOCK = "Y"
               MOVE 3                TO WS-SHARED
               PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
               PERFORM AY37 THRU AY59
               MOVE SHR-RECORD       TO NET-RECORD
           ELSE
               MOVE 3                TO WS-NETKEY
               PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
               PERFORM AY39 THRU AY59.
               GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
      *           B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T   P E R I O D
      *
                       GO TO AY62-WAIT
                   ELSE
                       ADD 1         TO WS-SUB
                       GO TO AY62
                   END-IF
               ELSE
      *
      *    ****   I S   T H I S   P R O G R A M   I N   T H E   Q
      *
               IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
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
             MOVE LS-USER            TO PAR-USR(WS-SUB).
             MOVE WS-SUB             TO PAR-USERS.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
             MOVE 300                TO WS-WAIT.
           IF NOT(PAR-USR(WS-SUB) = LS-USER)
               IF WS-SUB < 24
                   ADD 1             TO WS-SUB
                   GO TO AY62.

       AY62-CHECK.
           IF WS-WAIT > ZERO
               COMMIT
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               ACCEPT WS-STIME FROM TIME
               PERFORM LOCK-REC-LOOP
               DISPLAY "Waiting" AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK
               GO TO AY60.
             DISPLAY SPACE AT 5072 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
             MOVE WS-SUB             TO WS-USUB.
             GO TO AY61.

       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 3.
      *
           IF WS-STOCK = "Y"
               MOVE NET-RECORD       TO SHR-RECORD
               PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
           ELSE
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
               PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               IF WS-USUB < 24
                   ADD 1             TO WS-USUB
                   GO TO AY72
               ELSE
                   GO TO AY85.

       AY75.
             MOVE SPACES             TO PAR-PROG(WS-USUB) PAR-USR(WS-USUB).
           IF WS-USUB < 24
               ADD 1 WS-USUB         GIVING WS-SUB
           ELSE
               GO TO AY80.
           IF NOT(PAR-PROG(WS-SUB) = SPACES)
               MOVE PAR-PROG(WS-SUB) TO PAR-PROG(WS-USUB)
               MOVE PAR-USR(WS-SUB)  TO PAR-USR(WS-USUB)
               ADD 1                 TO WS-USUB
               GO TO AY75.

       AY80.
             SUBTRACT 1 FROM WS-USUB GIVING PAR-USERS.
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

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
       CA600      SECTION.
       CA601.
            MOVE 6   TO W50-LENGTH.

       CA605.
            MOVE W40-CODE  TO W50-DATA.
           IF W50-1 = SPACE
        MOVE W50-5  TO W40-CODE
        GO TO CA605.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-DENT  TO W40-CODE.

       CA606.
            MOVE W50-DENT  TO WAR-CODE.

       CA610.
      PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA699.
             EXIT.
      /
      *    ****   C R E A T E N E W W A R E H O U S E   R E C O R D
      *
       CB000   SECTION 50.
       CB00.
            PERFORM ERASE-SCREEN.
             MOVE 17                 TO SHADE-ROW.
             MOVE 9                  TO SHADE-COL.
             MOVE 66                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S01.
             PERFORM SCREEN-SHADOW.
             MOVE SPACE              TO W10-EXT-ITEM.
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

       CB05.
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
                 WHEN ESC-KEY   MOVE SPACES    TO W10-EXT-ITEM
                                MOVE 9         TO WS-ERROR
                                GO TO CB999
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                                GO TO CB05
                 WHEN F2-KEY    MOVE "I"       TO W10-ETYPE
                 WHEN F3-KEY    MOVE "A"       TO W10-ETYPE
                 WHEN F4-KEY    MOVE "D"       TO W10-ETYPE
                 WHEN F5-KEY    MOVE "2"       TO W10-ETYPE
                 WHEN F6-KEY    MOVE "X"       TO W10-ETYPE
                 WHEN F9-KEY    MOVE "W"       TO W10-ETYPE
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO CB05
               END-EVALUATE
        PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S05-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S05-ITM
     ELSE
         DISPLAY S05
     END-IF
        ELSE
     DISPLAY S05
        END-IF
        IF W10-EXT-ITEM = SPACES
     GO TO CB05.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W10-EXT-ITEM = SPACE
              GO TO CB999.

       CB10.
            PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S05-EXT
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S05-ITM
        ELSE
           DISPLAY S05
        END-IF
    ELSE
        DISPLAY S05.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50-LOOKUP
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S05-EXT
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S05-ITM
     ELSE
         DISPLAY S05
     END-IF
        ELSE
     DISPLAY S05
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO CB05
               ELSE
     GO TO CB10.
      DISPLAY STK-DESC AT 0518
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
            MOVE STK-CODE  TO W10-EXT-ITEM.
            MOVE STK-BIN  TO W10-BIN.
             MOVE STK-DESC       TO W10-DESC.
             MOVE STK-QUANT      TO W10-QUANT.
      MOVE SPACES  TO W40-CODE W40-CODE1 W40-CODE2.
      MOVE ZERO   TO W10-QNT.
      DISPLAY "F6" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=View Quantities"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.

       CB15.
             DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "=Warehouse Lookup,"
       "F6" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=View W/H Quantities"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
      ACCEPT S02.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CB05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO CB15
   WHEN F2-KEY
             PERFORM AA50W THRU AA59
       MOVE W40-CODE
     TO W40-CODE1
   WHEN F6-KEY
       PERFORM CB100 THRU CB199
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CB15
        END-EVALUATE
        DISPLAY S02
              IF W40-CODE1 = SPACE
           GO TO CB15.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
          IF W40-CODE1 = SPACE
        DISPLAY "Stock data base" AT 0825
   WITH HIGHLIGHT BACKGROUND-COLOR Magenta
        MOVE W10-QUANT  TO W80-S7V4
              GO TO CB25.

       CB20.
      MOVE W40-CODE1  TO W40-CODE.
      PERFORM CA600.
          IF WS-F-ERROR = 51
              MOVE "Z"   TO WS-ETYPE
              PERFORM AA50W THRU AA59
        MOVE W40-CODE  TO W40-CODE1
              DISPLAY S02
              IF W40-CODE1 = SPACES
           GO TO CB15
               ELSE
     GO TO CB20.
            MOVE WAR-NAME  TO W40-NAME.
      DISPLAY W40-NAME AT 0825
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
      MOVE W40-CODE1  TO WST-WAR.
      MOVE W10-EXT-ITEM  TO WST-EXT-CODE.
      PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        MOVE "No Record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO CB15.
      MOVE WST-QUANT  TO W80-S7V4.
      MOVE WST-RECORD1  TO W15-RECORD1.

       CB25.
            DISPLAY W80-Q1 AT 0918
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
      MOVE SPACES  TO W40-CODE.

       CB30.
      ACCEPT S03.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CB15
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO CB30
   WHEN F2-KEY
             PERFORM AA50W THRU AA59
       MOVE W40-CODE
     TO W40-CODE2
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CB30
        END-EVALUATE
        DISPLAY S03
              IF W40-CODE2 = SPACE
           GO TO CB30.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
            PERFORM CLEAR-L50.
    IF W40-CODE1 = W40-CODE2
        MOVE "Source = Desctination" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO CB30.
          IF W40-CODE2 = SPACE
        DISPLAY "Stock data base" AT 1125
   WITH HIGHLIGHT BACKGROUND-COLOR Magenta
        MOVE W10-QUANT  TO W80-S7V4
              GO TO CB40.

       CB35.
      MOVE W40-CODE2  TO W40-CODE.
      PERFORM CA600.
          IF WS-F-ERROR = 51
              MOVE "Z"   TO WS-ETYPE
              PERFORM AA50W THRU AA59
        MOVE W40-CODE  TO W40-CODE2
              DISPLAY S03
              IF W40-CODE2 = SPACES
           GO TO CB30
               ELSE
     GO TO CB35.
            MOVE WAR-NAME  TO W40-NAME.
      DISPLAY W40-NAME AT 1125
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.
      INITIALIZE WST-RECORD1.
      MOVE W40-CODE  TO WST-WAR.
      MOVE W10-EXT-ITEM  TO WST-EXT-CODE.
      PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        MOVE 1   TO WS-IND
        MOVE ZERO  TO W80-S7V4
    ELSE
        MOVE ZERO  TO WS-IND
        MOVE WST-QUANT  TO W80-S7V4.
      MOVE WST-RECORD1  TO W16-RECORD1.

       CB40.
            DISPLAY W80-Q1 AT 1218
       WITH HIGHLIGHT BACKGROUND-COLOR Magenta.

       CB45.
      ACCEPT S04.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO CB30
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO CB30
        END-EVALUATE.
    IF W10-QNT = ZERO
        GO TO CB00.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO CB45.
    IF W40-CODE1 = SPACES
        IF W10-QNT > STK-QUANT
     MOVE WS-ERR1  TO WS-ERR-MES
           PERFORM ERROR-MESSAGE
     GO TO CB45
        END-IF
    ELSE
    IF W10-QNT > W15-QUANT
        MOVE WS-ERR1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO CB45.
      PERFORM AY60 THRU AY999.
    IF WS-IND = 1
        INITIALIZE W16-RECORD1
        MOVE W40-CODE  TO W16-WAR
        MOVE W10-EXT-ITEM TO W16-EXT-CODE
        MOVE SPACES  TO W16-BIN
        MOVE ZERO  TO W16-MTD W16-MTDV W16-QUANT
        W16-YTD W16-YTDV
        PERFORM WRITE-WSTOCK THRU WRITE-WSTOCK-EXIT
        MOVE 1   TO WS-ACTION
        PERFORM AY052 THRU AY59.
    IF W40-CODE1 = SPACES
        MOVE 0   TO WS-ACTION
              PERFORM AY05 THRU AY59
        SUBTRACT W10-QNT  FROM STK-QUANT
        PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT
    ELSE
        MOVE W15-RECORD1  TO WST-RECORD1
        MOVE 0   TO WS-ACTION
              PERFORM AY052 THRU AY59
        SUBTRACT W10-QNT  FROM WST-QUANT
        PERFORM REWRITE-WSTOCK-UNLOCK THRU WRITE-WSTOCK-EXIT.
    IF W40-CODE2 = SPACES
        MOVE 0   TO WS-ACTION
              PERFORM AY05 THRU AY59
        ADD W10-QNT  TO STK-QUANT
        PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT
    ELSE
        MOVE W16-RECORD1  TO WST-RECORD1
        MOVE 0   TO WS-ACTION
              PERFORM AY052 THRU AY59
        ADD W10-QNT  TO WST-QUANT
        PERFORM REWRITE-WSTOCK-UNLOCK THRU WRITE-WSTOCK-EXIT.
             PERFORM AY70 THRU AY999.
      GO TO CB00.

       CB100.
      PERFORM SAVE-SCREEN.
      PERFORM CLEAR-L50.
      DISPLAY "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0554
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "³ Code ³  Quantity   ³" AT 0654
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 0754
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1854
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      MOVE 0854   TO CPOS.
      PERFORM CB180 UNTIL CLIN = 18.
      MOVE 6   TO SHADE-ROW.
      MOVE 56   TO SHADE-COL.
      MOVE 20   TO SHADE-WIDTH.
      MOVE 13   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE W10-EXT-ITEM  TO WST-EXT-CODE.
      MOVE SPACES  TO WST-WAR.

       CB105.
      PERFORM START-AT-WH-ITEM THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO CB190.

       CB110.
            MOVE 08   TO CLIN.

       CB115.
      PERFORM READ-WSTOCK-NEXT THRU READ-WSTOCK-EXIT.
    IF WS-F-ERROR = 52
        GO TO CB120.
    IF NOT(W10-EXT-ITEM = WST-EXT-CODE)
        GO TO CB120.
      MOVE WST-QUANT  TO W80-S7V4.
      DISPLAY (CLIN, 55) WST-WAR
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY ( , 62) W80-Q1
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
    IF CLIN < 17
        ADD 1   TO CLIN
        GO TO CB115.

       CB120.
            DISPLAY "PgUp" AT 5027
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
       "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "PgDn" WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
             "Esc" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to quit" WITH BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue HIGHLIGHT.

       CB125.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO CB190
   WHEN PAGE-DOWN
             GO TO CB130
   WHEN PAGE-UP
             GO TO CB140
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO CB125.
      PERFORM AA900-ALARM.
            GO TO CB125.

       CB130.
    IF CLIN < 17
        GO TO CB125.

       CB135.
      MOVE 0854   TO CPOS.
      PERFORM CB180 UNTIL CLIN = 18.
            GO TO CB110.

       CB140.
      MOVE 1   TO WS-IXS1.

       CB145.
      PERFORM READ-WSTOCK-PREV THRU READ-WSTOCK-EXIT.
    IF (WS-F-ERROR = 52) OR
       (W10-EXT-ITEM NOT = WST-EXT-CODE)
        GO TO CB110.
    IF WS-IXS1 < 10
        ADD 1   TO WS-IXS1
        GO TO CB145.
      GO TO CB110.

       CB180.
      DISPLAY "³      ³             ³" AT CPOS
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      ADD 1   TO CLIN.

       CB190.
             PERFORM RESTORE-SCREEN.

       CB199.
      EXIT.

       CB999.
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
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
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
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN 51    MOVE W02-WARHSE   TO WS-FILE
                          MOVE WAR-CODE     TO WS-KEYX
               WHEN 52    MOVE W02-WSTOCK   TO WS-FILE
                          MOVE WST-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP064"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
