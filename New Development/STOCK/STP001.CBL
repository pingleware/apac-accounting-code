      $set LINKCOUNT"512" GNT"STP001.GNT"
      *
      *       ONLY DELETE WHEN ALL BELOW IS COMPLETED  
      *     ===========================================
      *
      *  Screen co-ordinates must be revised for new layout
      *
      *  Screen Section                            Completed  
      *                                               ___
      *                    Layout screen co-ordinates| x |
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
      *         Display ITM, EXT and 14 Characters   | x |
      *                                               ---
      *                                               ___
      *         Accept ITM, EXT, 14 Characters and   | x |
      *            all the Numeric Formats 3 to 14    ---
      *
      *  Data Division.
      *                                               ___
      *         Line up Pic, Comp, Redefines etc.    | x |
      *                                               ---
      *                                               ___
      *         Remove unused variables and insert   | x |
      *            all the New Layouts                ---
      *
      *  Stock file
      *                                               ___       
      *         Use the new layout and W10.STK       |   |
      *                                               ---
      *
      *  Memo File
      *                                               ___ 
      *         Remove STKMEM copies                 | x |
      *                                               ---
      *                                               ___ 
      *         Remove all variables for Memo        |   |
      *                                               ---
      *                                               ___ 
      *         Remove screen layouts for Memo       |   |
      *                                               ---
      *                                               ___ 
      *         Include COPY "STKMEM.PRO" for        | x |
      *                          memo lookups         ---
      *                                               ___ 
      *         Exchange Memo lookup to CALL         |   |
      *                                 STPMEMO       ---
      *                                               ___ 
      *         Exchange Memo maintenance to         |   |
      *                             CALL STP007       ---
      *
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****      ****       **      *
      *   **    **     **     **    **   **  **    **  **     ***      *
      *   **           **     **    **  **    **  **    **     **      *
      *    ******      **     *******   **    **  **    **     **      *
      *         **     **     **        **    **  **    **     **      *
      *   **    **     **     **         **  **    **  **      **      *
      *    ******      **     **          ****      ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        S T O C K   C R E A T E   N E W   R E C O R D           *
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
       PROGRAM-ID.     STP001.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   JANUARY 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2018
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

       COPY "SPARTS.SL".

       COPY "STKDEX.SL".

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

      *
      *   Using External file as the called program STPALT also updates
      *   the Recovery file while generating the entries for the
      *   Alternate Index file.
      *
       COPY "RECOVER.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "SPARTS.FDE".

       COPY "STKDEX.FDE".

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
       77  WS-DISC                 PIC  999V99   COMP-3.
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
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-MONTHS               PIC S9(01)V99 COMP-3.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-ITM                  PIC  X(18).
       77  WS-OPTION               PIC  X(01).
       77  WS-INSTR                PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ADJ                  PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-IND                  PIC  9(01)    VALUE 0.
       77  WS-INDB                 PIC  9(01)    VALUE 0.
       77  WS-INDP                 PIC  9(01)    VALUE 0.
       77  WS-INDS                 PIC  9(01)    VALUE 0.
       77  WS-AMEND                PIC  9(01)    VALUE 0.
       77  WS-CONREC               PIC  9(01)    VALUE ZERO.
       77  WS-LEDG                 PIC  X(04).
       77  WS-LEDG1                PIC  X(04).
       77  WS-USE-CASES            PIC  X(01).
       77  WS-USE-PACKS            PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-CON-FACT             PIC  X(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-WEEK                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-BATCH                PIC  9(05).
       77  WS-PASSWORD             PIC  X(08).
       77  WS-PRINT                PIC  9(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-PASS                 PIC  X(08).
       77  WS-CARDEX               PIC  X(01).
       77  WS-DESC-H1              PIC  X(14)    VALUE "Description".
       77  WS-DESC-H2              PIC  X(14)    VALUE "Description 2".
       77  WS-RT-HD                PIC  X(10)    VALUE "-Retail   ".
       77  WS-WS-HD                PIC  X(10)    VALUE "-Wholesale".
       77  WS-CS-HD                PIC  X(10)    VALUE "-Cash sale".
       77  WS-NODEP                PIC  X(30)    VALUE "NO SUCH   D E P A R T M E N T".
       77  PRG-NAME                PIC  X(12)    VALUE SPACES.
       77  PRG-STOCK-LUP           PIC  X(12)    VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
       77  WS-CODE                 PIC  X(06)    VALUE SPACES.

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)    VALUE "STP".
           03  WS-PROG             PIC  X(03)    VALUE "001".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).
           
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STP001".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY CREATION".
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

       COPY "W20.WS".

       COPY "W25.STK".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-S2              PIC  9(02)    COMP-5.
           03  W75-SLEDG           PIC  X(04)    OCCURS 10.

       01  W80-EDIT.
           03  W80-CNO             PIC  9(04).
           03  W80-V1.
               05  W80-S7V2        PIC  Z(06)9.99-.
           03  W80-V2.
               05  W80-S5V2        PIC  Z(04)9.99-.
           03  W80-DTE.
               05  W80-DATE        PIC  Z9/99/9999.
           03  W80-QNT.
               05  W80-5           PIC  Z(04)9.99.
           03  W80-QNT1                          REDEFINES W80-QNT.
               05  W80-3V2         PIC  Z(02)9.9999.
           03  W80-NO.
               05  W80-5N          PIC  9(05).
               05  W80-REC                       REDEFINES W80-5N    PIC  ZZZZ9.
           03  W80-DEL.
               05  W80-2N          PIC  9(02).

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W90-BAL                 PIC S9(07)V99 COMP-3.

       01  W95-COMP                PIC  X(40).

       01  W100-EDIT.
           03  W100-V2.
               05  W100-S6V2       PIC  Z(05)9.99-.
           03  W100-V3                           REDEFINES W100-V2.
               05  W100-PRICE      PIC  X(09).
               05  FILLER          PIC  X(01).
           03  W100-V5                           REDEFINES W100-V2.
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

       01  S02.
           03  LINE  5 COLUMN  2 VALUE "Logged In"      FOREGROUND-COLOR Cyan HIGHLIGHT BLINK.

       COPY "STOCK.CRT".

       COPY "DEPART.CRT".

      *
      *    ****    PRODUCTION DETAILS DISPLAY SCREEN
      *
       01  S32.
           03  LINE 5 COLUMN 21 VALUE "Details of Manufacture (Include Labour)" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE 6 COLUMN  8 VALUE "Comments".
           03  LINE 7 COLUMN  4 VALUE "Stock code".
           03         COLUMN 19 VALUE "ITM".
           03         COLUMN 28 VALUE "Description".
           03         COLUMN 66 VALUE "Quantity".

       01  S32-ITEM-CODE.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S32A.
                   07  COLUMN  4 PIC  X(14)/X(03) USING W10-EXT-CODE2 AUTO.

               05  S32B.
                   07  COLUMN  4 PIC  X(18) USING W10-EXT-CODE2 AUTO.

               05  S32C.
                   07  COLUMN  4 PIC  X(14) USING W10-EXT-CODE2 AUTO.
                   
               05  S32-3.
                   07  COLUMN  4 PIC Z(03) USING W10-I3 AUTO.
               
               05  S32-4.
                   07  COLUMN  4 PIC Z(04) USING W10-I4 AUTO.
               
               05  S32-5.
                   07  COLUMN  4 PIC Z(05) USING W10-I5 AUTO.
               
               05  S32-6.
                   07  COLUMN  4 PIC Z(06) USING W10-I6 AUTO.
               
               05  S32-7.
                   07  COLUMN  4 PIC Z(07) USING W10-I7 AUTO.
               
               05  S32-8.
                   07  COLUMN  4 PIC Z(08) USING W10-I8 AUTO.
               
               05  S32-9.
                   07  COLUMN  4 PIC Z(09) USING W10-I9 AUTO.
               
               05  S32-10.
                   07  COLUMN  4 PIC Z(10) USING W10-I10 AUTO.
               
               05  S32-11.
                   07  COLUMN  4 PIC Z(11) USING W10-I11 AUTO.
               
               05  S32-12.
                   07  COLUMN  4 PIC Z(12) USING W10-I12 AUTO.
               
               05  S32-13.
                   07  COLUMN  4 PIC Z(13) USING W10-I13 AUTO.
               
               05  S32-14.
                   07  COLUMN  4 PIC Z(14) USING W10-I14 AUTO.

       01  S38.
           03                   PIC ZZZZ9.9999 USING W10-MQNT AUTO FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       01  S-CONV.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black
               05  LINE 10 COLUMN 25 VALUE  "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
               05          COLUMN    VALUE                                 "¿" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 11 COLUMN    VALUE  "³".                               
               05          COLUMN    VALUE   " Convertion Factor "             FOREGROUND-COLOR Red.
               05          COLUMN    VALUE                      "          "   FOREGROUND-COLOR Cyan HIGHLIGHT.
               05          COLUMN    VALUE                                " "  FOREGROUND-COLOR Grey.
               05          COLUMN    VALUE                                 "³" FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 12 COLUMN    VALUE  "À".                               
               05          COLUMN    VALUE   "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" FOREGROUND-COLOR Grey HIGHLIGHT.

       01  SA-FACT.
           03  LINE 11 COLUMN 45 PIC ZZZZZ9.999 USING W10-FACT AUTO BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S-LEDG.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN 02 VALUE "F1"                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 04 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F6"                     FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           "=Sales Ledger Lookup".
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
           IF LS0-STLEV < 1
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.

       AA05.
             PERFORM BB000.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If there was no error while  adding a new Stock/Inventory ³
      *    ³ record, then generate the alternate Index, which contains ³
      *    ³ an entry for each word in the two descriptions and in the ³
      *    ³ Item code.                                                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF NOT(WS-ERROR = 9)
               MOVE "C"              TO LS-INSTR
               CALL "STP\STPALT" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY W02-STKALT W02-RECOVER WS-RECOVER WS-TRANS STK-CODE STK-DESC STK-DESC2 WS-ERROR.
               IF WS-ERROR = 9
                   MOVE "Error with Description WORD index update" TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   STOP RUN
               END-IF
               CANCEL "STP\STPALT"
               PERFORM AY70 THRU AY999
               MOVE "Alternate Index generated for ITEM" TO WS-DSP-MES
               PERFORM DISPLAY-MESSAGE
               GO TO AA05.
             CLOSE RECOVER.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

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

       COPY "AA900.ALM".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                        STOCK-MEMO                         ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ The item code  of  the  Stock  record  for which the MEMO ³
      *    ³ details are to be displayed must be in W10-EXT-ITEM.      ³
      *    ³                    PERFORM STOCK-MEMO.                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *
      *COPY "STKMEM.PRO".
      *
      *
      *    ****    D I S P L A Y   D E P A R T M E N T   N A M E
      *
       AA400              SECTION.
       AA401.
           MOVE W10-LEDG             TO DPT-CODE.
           PERFORM READ-DEPART THRU READ-DEPART-EXIT.
         IF (WS-F-ERROR = 7) OR (DPT-DESC = SPACES)
             MOVE WS-NODEP           TO W10-DEPT
         ELSE
             MOVE DPT-DESC           TO W10-DEPT.
           DISPLAY W10-DEPT AT 1226 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.

       AA405.
             EXIT.

       COPY "DATE.CHK".

       COPY "DATE.CNV".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
        AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "SHARED.RD".

       COPY "STOCK.RD".

       COPY "SPARTS.RD".

      *
      *   R E W R I T E   &    W R I T E   F I L E S   R O U T I N E S
      *
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEPART.WR".

       COPY "PARAM.WR".

       COPY "SHARED.WR".

       COPY "STKDEX.WR".

       COPY "STOCK.WR".

       COPY "SPARTS.WR".

       COPY "DEPART.LUP".

       COPY "STOCK.1ST".

       COPY "APAC.HLP".

      *
      *       ****   *****   ***    ***   *   *  *****  ****   *   *
      *       *   *  *      *   *  *   *  *   *  *      *   *  *   *
      *       ****   ***    *      *   *  *   *  ***    ****    * *
      *       *   *  *      *   *  *   *   * *   *      *   *    *
      *       *   *  *****   ***    ***     *    *****  *   *    *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *           THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
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
      *AY01.
      *      MOVE 01                 TO REC-FILE.
      *      MOVE WS-PARKEY          TO REC-KEY.
      *      MOVE PAR-RECORD1        TO REC-PARAM.
      *      GO TO AY50.
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
      *    ****   S T O C K   P A R T S   F I L E   (MANUFACTURING) ****
      *    *************************************************************
       AY20.
             MOVE 20                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE PRT-REC1           TO REC-SPARTS.
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
      *    *************************************************************
      *    ****          S T O C K   C A R D E X   F I L E          ****
      *    *************************************************************
       AY46.
             MOVE 46                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE STX-REC1           TO REC-STKDEX.
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
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
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
      *    ****   Y E S   -   S E T   W A I T P E R I O D
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
      *    ****   I S   I T   N E X T  I N   L I N E   T O   P R O C E S S
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

       COPY "ROUND.PRC".

       BA000             SECTION.
       BA00.
             MOVE "I"                TO LS-INSTR.
             MOVE SPACE              TO LS0-INSTR.
             CALL "STP\STPREAD" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY STOCK-READ W10-STOCK WS-F-ERROR WS-CODE.
           IF WS-F-ERROR = ZERO
               MOVE W10-EXT-ITEM     TO STK-CODE
               PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       BA99.
             EXIT.

      *
      *    ****   C R E A T E   N E W   S T O C K
      *
       BB000        SECTION 5.
       BB00-NEW-SPARE.
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             DISPLAY S04-N.
           IF WS-USE-ITM = "Y"
               DISPLAY "/ITM" AT 0612 "/" AT 0635.

       BB01-INIT.
             INITIALIZE W10-STOCK.
             MOVE "      1"          TO W10-BIN.
             MOVE 1                  TO W10-TAX.
             MOVE "Each"             TO W10-PR(1).
             MOVE "Six"              TO W10-PR(2).
             MOVE "Dozen"            TO W10-PR(3).
             MOVE "Case"             TO W10-PR(4).
             MOVE "N"                TO W10-USE-PACKS W10-USE-CASES W10-EXEMPT W10-ALT-DISC.
             MOVE SPACES             TO W10-EXT-ITEM  W10-DESC W10-LEDG W10-EXT-CODE2 W10-DESC2 W10-ACODE.
             MOVE ZERO               TO W10-COST W10-QUANT W10-AVRG W10-REORD W10-GUAR W10-LTIME W10-FACT W10-ORDER W10-IND W10-PURCH W10-ODTE W22-DTE2 W10-PRC W10-CASES.
             MOVE 1                  TO WS-S1.

       BB02.
             MOVE ZERO               TO W10-UNT(WS-S1) W10-MKUP(WS-S1) W10-CMKUP(WS-S1) W10-WMKUP(WS-S1) W10-SL(WS-S1) W10-CSH(WS-S1) W10-WSL(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BB02.
             MOVE 1                  TO W10-UNIT.
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

       BB02-EXIT.
             GO TO BB05-ACCEPT.

       BB03.
           IF W10-EXT-ITEM = SPACE
               GO TO BB01-INIT.
             MOVE W10-EXT-ITEM       TO WS-ITM.
             PERFORM BB01-INIT THRU BB02.
             MOVE WS-ITM             TO W10-EXT-ITEM.
             PERFORM BA000.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-TAX            TO W10-TAX.
             MOVE STK-BIN            TO W10-BIN.
             MOVE STK-IND            TO W10-IND.
             MOVE STK-LDG            TO W10-LEDG.
             MOVE STK-LTIME          TO W10-LTIME.
             MOVE STK-ACODE          TO W10-ACODE.
             MOVE STK-GUAR           TO W10-GUAR.
             MOVE STK-USE-CASES      TO W10-USE-CASES.
             MOVE STK-USE-PACKS      TO W10-USE-PACKS.
             MOVE STK-ALT-DISC       TO W10-ALT-DISC.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-COST           TO W10-COST.
             MOVE 1                  TO WS-S1.

       BB04.
             MOVE STK-PR(WS-S1)      TO W10-PR(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BB04.

       BB05-ACCEPT.
             DISPLAY S06A.
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
                                GO TO BB999
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                                GO TO BB05-ACCEPT
                 WHEN F2-KEY    MOVE "I"       TO W10-ETYPE
                 WHEN F3-KEY    MOVE "A"       TO W10-ETYPE
                 WHEN F4-KEY    MOVE "D"       TO W10-ETYPE
                 WHEN F5-KEY    MOVE "2"       TO W10-ETYPE
                 WHEN F6-KEY    MOVE "X"       TO W10-ETYPE
                 WHEN F9-KEY    MOVE "W"       TO W10-ETYPE
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO BB05-ACCEPT
               END-EVALUATE
               PERFORM AA50-LOOKUP
      *    
      *                      E X T E N D E D   C O D E
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S05-EXT
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF (W10-STCK = "A") AND (WS-USE-ITM = "Y")
      *    
      *     A 3 character extension code is being used in addition to 
      *     the 14 character stock/inventory item code.              
      *    
                   DISPLAY S05-ITM
               ELSE
      *    
      *     A 1 TO 14 character Alpha-Numeric or Numeric code is being 
      *     used.               
      *    
                   DISPLAY S05
               END-IF
               GO TO BB03.
             PERFORM CLEAR-L50.
           IF W10-EXT-ITEM = SPACE
               MOVE 9                TO WS-ERROR
               GO TO BB999.
             PERFORM BA000.
           IF WS-F-ERROR = ZERO OR STK-DISC = 9
               GO TO BB25-EXISTS.

       BB15-NEW-DET.
             DISPLAY S-LEDG.

       BB16-NEW-DET.
             ACCEPT S06A.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE SPACES    TO W10-EXT-ITEM
                                 MOVE 9         TO WS-ERROR
                                 GO TO BB999
                 WHEN F1-KEY     PERFORM HELP-ROUTINE
                                 GO TO BB16-NEW-DET
                 WHEN F6-KEY     PERFORM SAVE-SCREEN
                                 PERFORM DEPART-VIEW
                                 PERFORM RESTORE-SCREEN
                                 GO TO BB15-NEW-DET
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO BB16-NEW-DET
               END-EVALUATE
               GO TO BB16-NEW-DET.
             PERFORM CLEAR-L50.
             PERFORM AA400.
             PERFORM BB21D.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB05-ACCEPT.
           IF ((W10-IND < 3) OR (W10-IND = 8 OR 9)) AND (W10-DESC = SPACES)
               MOVE "Invalid description"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
           IF (W10-LEDG = SPACES OR "0000" OR "XXXX" OR "YYYY") OR (DPT-SCDE = "VAT") OR (WS-NODEP = W10-DEPT)
               MOVE "Invalid ledger code"  TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
           IF W10-BIN = SPACES
               MOVE "Invalid bin number"   TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
           IF W10-TAX > 6
               MOVE "Invalid VAT code"     TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
      *
      *    ****    TEMPORARY PATCH - ONLY 1 RATE APPLICABLE
      *
           IF W10-TAX > 1
               MOVE 1                TO W10-TAX.
           IF W10-AVRG = ZERO
               MOVE W10-COST         TO W10-AVRG.
           IF W10-ODTE = ZERO
      *
      *      Zero to date in Stock record in working storage
      *
               MOVE ZERO             TO W10-DTE
               GO TO BB18.
             MOVE W10-ODTE           TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Order DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Order DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Order MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
      *
      *      Date as CCYYMMDD to Stock record in working storage
      *
             MOVE W22-DTE1           TO W10-DTE.

       BB18.
           IF W10-PURCH = ZERO
      *
      *      Zero to date in Stock record in working storage
      *
               MOVE ZERO             TO W10-PUR
               GO TO BB20-CREATE.
             MOVE W10-PURCH          TO W22-DATE.
             PERFORM DATE-CHECK THRU CHECK-DATE-EXIT.
           IF NOT(W22-ERROR = 0)
               EVALUATE W22-ERROR
                 WHEN 1  MOVE "Invalid Received DATE"  TO WS-ERR-MES
                 WHEN 2  MOVE "Invalid Received DAY"   TO WS-ERR-MES
                 WHEN 3  MOVE "Invalid Received MONTH" TO WS-ERR-MES
               END-EVALUATE
               PERFORM ERROR-MESSAGE
               GO TO BB15-NEW-DET.
      *
      *      Date as CCYYMMDD to Stock record in working storage
      *
             MOVE W22-DTE1           TO W10-PUR.

       BB20-CREATE.
           IF W10-IND > 4
               IF NOT(W10-IND = 8 OR 9)
                   MOVE "Invalid indicator"  TO WS-ERR-MES
                   PERFORM ERROR-MESSAGE
                   GO TO BB15-NEW-DET.
           IF W10-PER = SPACES
               MOVE "Each"           TO W10-PER.
           IF W10-ART11 = "N"
               GO TO BB20A.

       BB20-ART11.
             PERFORM SAVE-SCREEN-3.
             MOVE 12                 TO SHADE-ROW.
             MOVE 15                 TO SHADE-COL.
             MOVE 51                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S-ART11.
             PERFORM SCREEN-SHADOW.

       BB20-EXEMPT.
             ACCEPT W10-EXEMPT AT 1263 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING W10-EXEMPT BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(W10-EXEMPT = "N" OR "Y")
               GO TO BB20-EXEMPT.
             PERFORM RESTORE-SCREEN-3.

       BB20A.
             INITIALIZE STK-RECORD1.

       BB21.
             MOVE W10-EXT-ITEM       TO STK-CODE WS-ITM.
             MOVE W10-BIN            TO STK-BIN.
           IF W10-DESC = SPACES
               GO TO BB21AB.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-DESC BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BB21A.
             MOVE W10-DESC           TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-29           TO W10-DESC
               GO TO BB21A.

       BB21AB.
             MOVE W10-DESC           TO STK-DESC.
           IF W10-DESC2 = SPACES
               GO TO BB21BB.
             MOVE 30                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-DESC2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BB21B.
             MOVE W10-DESC2          TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-29           TO W10-DESC2
               GO TO BB21B.

       BB21BB.
             MOVE W10-DESC2          TO STK-DESC2.
             MOVE 10                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-ACODE BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 18                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-EXT-CODE2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             MOVE W10-LEDG           TO STK-LDG.
             MOVE W10-IND            TO STK-IND.
           IF W10-IND = 4
               GO TO BB21E.
           IF W10-IND = 2
               MOVE "-LAB-"          TO STK-SUPP.
             MOVE W10-TAX            TO STK-TAX.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-MARKUP NOT > ZERO
                   PERFORM BB50.
             MOVE W10-MARKUP         TO STK-MARKUP.
             MOVE W10-QUANT          TO STK-QUANT.
             MOVE W10-PRC            TO STK-PRC.
             MOVE W10-UNIT           TO STK-UNIT.
             MOVE W10-PER            TO STK-PER.
             MOVE W10-REORD          TO STK-LEVEL.
             MOVE W10-LTIME          TO STK-LTIME.
             MOVE W10-COST           TO W10-CCOST.
             MOVE W10-SELL           TO W10-CSELL.
           IF W10-COST = ZERO
               IF NOT(W10-SELL = ZERO) 
                   IF NOT(W10-MARKUP = ZERO)
                       COMPUTE W10-CCOST ROUNDED = (W10-CSELL / (100 + W10-MARKUP) * 100).
           IF W10-SELL = ZERO
               IF NOT(W10-COST = ZERO)
                   IF NOT(W10-MARKUP = ZERO)
                       COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-MARKUP) / 100))
                       IF W10-CSELL = ZERO
                           MOVE W10-CCOST TO W10-CSELL.
             MOVE W10-CCOST          TO W10-COST.
             MOVE W10-CSELL          TO W10-SELL.
             MOVE W10-COST           TO STK-COST.
             MOVE W10-GUAR           TO STK-GUAR.
             MOVE W10-ORDER          TO STK-ORD.
             MOVE W22-DTE2           TO STK-DTE.
             MOVE W10-PURCH          TO W22-DTE3.
             PERFORM SWITCH-DATE-TO-CYMD THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE2           TO STK-PUR.
           IF W10-AVRG = ZERO
               MOVE W10-COST         TO STK-AVGE
           ELSE
               MOVE W10-AVRG         TO STK-AVGE.
           IF NOT(W10-ALT-DISC = "Y")
               MOVE "N"              TO W10-ALT-DISC.
             MOVE W10-ALT-DISC       TO STK-ALT-DISC.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-CMARKUP NOT > ZERO
                   PERFORM BB52.
           IF (W10-CMARKUP = ZERO) AND (W10-CASH = ZERO)
               MOVE W10-MARKUP       TO W10-CMARKUP
               MOVE W10-SELL         TO W10-CASH
               GO TO BB21C.
           IF W10-CMARKUP = ZERO
               MOVE W10-CASH         TO W10-CSELL
               COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).
           IF (W10-CASH = ZERO) 
               IF NOT(W10-COST = ZERO)
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-CMARKUP) / 100))
                   MOVE W10-CSELL    TO W10-CASH.

       BB21C.
             MOVE W10-CMARKUP  TO STK-CMARKUP.
           IF (W10-IND < 3) OR (W10-IND = 8 OR 9)
               IF W10-WMARKUP NOT > ZERO
                   PERFORM BB54.
           IF (W10-WMARKUP = ZERO) AND (W10-WSALE = ZERO)
               MOVE W10-CMARKUP      TO W10-WMARKUP
               MOVE W10-CASH         TO W10-WSALE
               GO TO BB21D.
           IF W10-WMARKUP = ZERO
               MOVE W10-WSALE        TO W10-CSELL
               COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-CCOST) * 100).
               IF W10-WSALE = ZERO
                   COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * W10-WMARKUP) / 100))
                   MOVE W10-CSELL    TO W10-WSALE.

       BB21D.
           IF W05-ROUND = "E"
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF NOT(STK-TAX = ZERO)
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE        TO W10-VWSALE
               MOVE W10-CASH         TO W10-VCASH
               MOVE W10-SELL         TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCASH
               PERFORM CB200.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT (W10-TAX)  TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE).

       BB21DA.
             MOVE W10-WMARKUP        TO STK-WMARKUP.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.
             MOVE W10-EXT-CODE2      TO STK-XREF.
             MOVE W10-ACODE          TO STK-ACODE.
             MOVE W10-EXEMPT         TO STK-EXEMPT.

       BB21E.
             DISPLAY S06A.
           IF NOT(W10-IND = 4)
               DISPLAY S06C.

       BB21F.
           IF W10-TAX = ZERO
               MOVE "'C'ontinue or 'R'ekey  [C]" TO WS-OPT-MES
               MOVE 9                TO WS-INSTR
           ELSE
               MOVE "'C'ontinue, amend 'I'nclusive or 'R'ekey  [C]" TO WS-OPT-MES
               MOVE 2                TO WS-INSTR.
             MOVE "C"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "R"
               GO TO BB05-ACCEPT.
           IF NOT(W10-TAX = ZERO) AND (WS-OPTION = "I")
               ACCEPT S06C
               COMPUTE W10-CASH = (W10-VCASH / (100 + W05-RATE)) * 100
               COMPUTE W10-WSALE ROUNDED = (W10-VWSALE / (100 + W05-RATE)) * 100
               COMPUTE W10-SELL ROUNDED = (W10-VSELL / (100 + W05-RATE)) * 100
               DISPLAY S06E
               MOVE W10-SELL         TO STK-SELL
               MOVE W10-CASH         TO STK-CASH
               MOVE W10-WSALE        TO STK-WSALE
               GO TO BB21F.
           IF WS-CON-FACT = "Y"
               PERFORM BD000.
             MOVE W10-FACT           TO STK-FACT.
      *
      *    ****    C H E C K   I F   C A S E S   O R   P A C K S
      *                      O P T I O N   I S   S E T
      *
           IF (LS0-LEV > 3) AND (W10-IND < 2)
               IF (WS-USE-PACKS = "Y") OR (WS-USE-CASES = "Y")
                   PERFORM CLEAR-L50
                   PERFORM SAVE-SCREEN
                   CALL "STP\STPPACKS" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK
                       ON EXCEPTION
                           MOVE "Program- STPPACKS not on disk, press ANY key" TO WS-ERR-MES
                           PERFORM ERROR-MESSAGE
                           GO TO BB22
                   END-CALL
                   CANCEL "STP\STPPACKS"
                   PERFORM RESTORE-SCREEN.
             MOVE W10-USE-CASES      TO STK-USE-CASES.
             MOVE W10-CASES          TO STK-CASES.
             MOVE W10-USE-PACKS      TO STK-USE-PACKS.
             MOVE 2                  TO WS-S1.
             PERFORM BB40 THRU BB45.

       BB22.
             PERFORM AY60 THRU AY999.
           IF STK-IND = 0 OR 1
      *
      *    ****  Check if in-house bar code must be allocated to item.
      *
               IF W50-ALLOC-BC = "Y"
                   PERFORM BC000.
           IF STK-IND = 2
               MOVE ZERO             TO STK-COST STK-MARKUP STK-CMARKUP STK-WMARKUP.
             PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY05 THRU AY59.
             MOVE STK-RECORD1        TO W15-RECORD1.
             PERFORM AY70 THRU AY999.

       BB22A.
           IF LS0-LEV < 3
               GO TO BB24.
             MOVE "N"                TO W10-INSTR.
             CALL "STP\STP007" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W10-STOCK.               

       BB22E.
           IF STK-IND > 0
               GO TO BB24.
             MOVE "Is this Item produced from other Items Y/N  [N]"  TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             MOVE 1                  TO WS-INSTR.

       BB23.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               MOVE ZERO             TO WS-AMEND
               PERFORM BB60 THRU BB99
           ELSE
               PERFORM AY60 THRU AY999.

       BB24.
           IF WS-CARDEX = "Y"
               IF NOT(W10-QUANT = ZERO)
                   INITIALIZE STX-REC1
                   MOVE W10-EXT-ITEM TO STX-EXT-CODE
                   MOVE W12-T-YMD    TO STX-DTE
                   MOVE ZERO         TO STX-SEQ
                   MOVE LS0-NO       TO STX-USER
                   MOVE "CREATE"     TO STX-PROG
                   MOVE "New item quantity"  TO STX-REMARKS
                   MOVE LS-USER      TO STX-WS
                   MOVE W10-QUANT    TO STX-QNT
                   COMPUTE STX-COST ROUNDED = (W10-AVRG * W10-QUANT)
                   COMPUTE STX-SELL ROUNDED = (STX-COST + ((STX-COST * W10-MARKUP) / 100))
                   PERFORM WRITE-STKDEX THRU WRITE-STKDEX-EXIT
                   MOVE 1            TO WS-ACTION
                   PERFORM AY46 THRU AY59.
             MULTIPLY W10-QUANT BY W10-AVRG GIVING W25-TOTAL.
             ADD W25-TOTAL           TO STK-TOTAL.
             ADD 1                   TO STK-RECORDS.
             MOVE STK-RECORDS        TO WS-RECORDS.
             PERFORM AY70 THRU AY999.
             MOVE 0                  TO WS-ERROR.
             GO TO BB999.

       BB25-EXISTS.
           IF STK-DISC = 9
               GO TO BB30.
             MOVE "Item exists"      TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             GO TO BB05-ACCEPT.

       BB30.
             MOVE "Marked for deletion - Reinstate [Y]"  TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             MOVE "Z"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y")
               GO TO BB05-ACCEPT.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
             MOVE 0                  TO STK-DISC.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             GO TO BB00-NEW-SPARE.
      *
      *    ****   P A C K   D E T A I L S
      *
       BB40.
             MOVE W10-UNT(WS-S1)     TO STK-UNT(WS-S1).
             MOVE W10-PR(WS-S1)      TO STK-PR(WS-S1).
             MOVE W10-MKUP(WS-S1)    TO STK-MKUP(WS-S1).
             MOVE W10-CMKUP(WS-S1)   TO STK-CMKUP(WS-S1).
             MOVE W10-WMKUP(WS-S1)   TO STK-WMKUP(WS-S1).
             MOVE W10-SL(WS-S1)      TO STK-SL(WS-S1).
             MOVE W10-CSH(WS-S1)     TO STK-CSH(WS-S1).
             MOVE W10-WSL(WS-S1)     TO STK-WSL(WS-S1).
           IF WS-S1 < 4
               ADD 1                 TO WS-S1
               GO TO BB40.

       BB45.
             EXIT.

      *
      *    ****  D E F A U L T   M A R K U P
      *
       BB50.
           IF W10-SELL > ZERO AND W10-COST > ZERO
               MOVE W10-SELL         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-MARKUP       TO W10-MARKUP.

       BB51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BB52.
           IF W10-CASH > ZERO AND W10-COST > ZERO
               MOVE W10-CASH         TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-CMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-CMRKUP       TO W10-CMARKUP.

       BB53.
             EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BB54.
           IF W10-WSALE > ZERO AND W10-COST > ZERO
               MOVE W10-WSALE        TO W10-CSELL
               MOVE W10-COST         TO W10-CCOST
               COMPUTE W10-WMARKUP = (((W10-CSELL - W10-CCOST) / W10-COST) * 100)
           ELSE
               MOVE DPT-WMRKUP       TO W10-WMARKUP.

       BB55.
             EXIT.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BB60.
             MOVE 1                  TO WS-S6.
             MOVE ZERO               TO W10-MCOST.
             MOVE SPACES             TO W10-COMMENT.

       BB61.
             MOVE SPACES             TO W11-EXT-CODE(WS-S6) W11-DESC(WS-S6).
             MOVE ZERO               TO W11-QUANT(WS-S6).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
               GO TO BB61.

       BB63.
             PERFORM ERASE-SCREEN.
             DISPLAY S32.

       BB64.
             ACCEPT W10-COMMENT AT 0618 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB64.

       BB65.
             MOVE 1                  TO WS-S6 WS-S7.
             MOVE 8                  TO LIN.

       BB70.
             MOVE SPACES             TO W10-EXT-CODE2.
             MOVE ZERO               TO W10-MQNT.
           IF W11-EXT-CODE(WS-S6) = SPACES
               GO TO BB75.
             MOVE W11-EXT-CODE(WS-S6)  TO W10-EXT-CODE2.
             MOVE W11-QUANT(WS-S6)   TO W10-MQNT.

       BB75.
             PERFORM AA300.
      *    *************************************************************
      *                     G E T   S T O C K   C O D E
      *    *************************************************************
      *    
      *      Is an 18 character stock/inventory item code being used?  
      *    
           IF WS-EXT-STK = "Y"
               ACCEPT S32B AT LINE LIN
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
                   ACCEPT S32A AT LINE LIN
               ELSE
                   ACCEPT S32C AT LINE LIN
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
                 WHEN 3         ACCEPT S32-3 AT LINE LIN
      *                     4 Digit numeric Stock/Inventory item code
                 WHEN 4         ACCEPT S32-4 AT LINE LIN
      *                     5 Digit numeric Stock/Inventory item code
                 WHEN 5         ACCEPT S32-5 AT LINE LIN
      *                     6 Digit numeric Stock/Inventory item code
                 WHEN 6         ACCEPT S32-6 AT LINE LIN
      *                     7 Digit numeric Stock/Inventory item code
                 WHEN 7         ACCEPT S32-7 AT LINE LIN
      *                     8 Digit numeric Stock/Inventory item code
                 WHEN 8         ACCEPT S32-8 AT LINE LIN
      *                     9 Digit numeric Stock/Inventory item code
                 WHEN 9         ACCEPT S32-9 AT LINE LIN
      *                    10 Digit numeric Stock/Inventory item code
                 WHEN 10        ACCEPT S32-10 AT LINE LIN
      *                    11 Digit numeric Stock/Inventory item code
                 WHEN 11        ACCEPT S32-11 AT LINE LIN
      *                    12 Digit numeric Stock/Inventory item code
                 WHEN 12        ACCEPT S32-12 AT LINE LIN
      *                    13 Digit numeric Stock/Inventory item code
                 WHEN 13        ACCEPT S32-13 AT LINE LIN
      *                    14 Digit numeric Stock/Inventory item code
                 WHEN 14        ACCEPT S32-14 AT LINE LIN
               END-EVALUATE.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY MOVE SPACES    TO W10-EXT-CODE2
                              GO TO BB75A
                 WHEN F1-KEY  PERFORM HELP-ROUTINE
                              GO TO BB75
                 WHEN F2-KEY  MOVE "I"       TO W10-ETYPE
                 WHEN F3-KEY  MOVE "A"       TO W10-ETYPE
                 WHEN F4-KEY  MOVE "D"       TO W10-ETYPE
                 WHEN F5-KEY  MOVE "2"       TO W10-ETYPE
                 WHEN F6-KEY  MOVE "X"       TO W10-ETYPE
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BB75
               END-EVALUATE
               PERFORM AA50-LOOKUP
               MOVE W10-EXT-ITEM     TO W10-EXT-CODE2
      *    
      *     Is an 18 character stock/inventory item code being used?  
      *    
               IF WS-EXT-STK = "Y"
                   DISPLAY S32B AT LINE LIN
               ELSE
      *   
      *     Is an ALPHA-NUMERIC code being used?
      *    
               IF W10-STCK = "A"
      *    
      *     Is a 3 character extension code being used in addition to 
      *     the 14 character stock/inventory item code?               
      *    
                   IF WS-USE-ITM = "Y"
                       DISPLAY S32A AT LINE LIN
                   ELSE
                       DISPLAY S32C AT LINE LIN
                   END-IF
               ELSE
      *    *************************************************************
      *                         NUMERIC CODES APPLY 
      *    *************************************************************
                   DISPLAY S32C AT LINE LIN
               END-IF
               END-IF    
               IF W10-EXT-CODE2 = SPACES
                   GO TO BB75.
             MOVE 18                 TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W10-EXT-CODE2 BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.

       BB75A.
             PERFORM CLEAR-L50.
           IF W10-EXT-CODE2 = SPACE
               IF WS-AMEND = 1
                   GO TO BB95
               ELSE
                   GO TO BB85.
             MOVE W10-EXT-CODE2  TO W10-EXT-ITEM.

       BB76.
             PERFORM BA000.
           IF WS-F-ERROR = 22
               MOVE "Z"              TO W10-ETYPE
               PERFORM AA50-LOOKUP
               MOVE W10-EXT-ITEM     TO W10-EXT-CODE2
               IF WS-EXT-STK = "Y"
                   DISPLAY S32B AT LINE LIN
               ELSE
               IF WS-USE-ITM = "Y"
                   DISPLAY S32A AT LINE LIN
               ELSE
                   DISPLAY S32C AT LINE LIN
               END-IF
               IF W10-EXT-CODE2 = SPACES
                   GO TO BB75
               ELSE
                   GO TO BB76.
             DISPLAY (LIN, 28) STK-DESC WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB75.

       BB80.
             MOVE 64                 TO COL.
             ACCEPT S38 AT LINE LIN COLUMN COL.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB80.
           IF W10-MQNT = ZERO
               GO TO BB85.
             MOVE W10-EXT-CODE2      TO W11-EXT-CODE(WS-S6).
             MOVE STK-DESC           TO W11-DESC(WS-S6).
             MOVE W10-MQNT           TO W11-QUANT(WS-S6).
             COMPUTE W10-MCOST = W10-MCOST + (W10-MQNT * STK-AVGE).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
           ELSE
               GO TO BB85.
           IF LIN < 22
               ADD 1                 TO LIN
               GO TO BB70.
             ADD 1                   TO WS-S7.
             MOVE 8                  TO LIN.
             MOVE WS-S7              TO WS-S8.

       BB81.
             MOVE W11-EXT-CODE(WS-S8)  TO W10-EXT-CODE2.
           IF WS-EXT-STK = "Y"
               DISPLAY S32B AT LINE LIN
           ELSE
           IF WS-USE-ITM = "Y"
               DISPLAY S32A AT LINE LIN
           ELSE
               DISPLAY S32C AT LINE LIN.
             MOVE W11-DESC(WS-S8)    TO W10-DESC.
             DISPLAY (LIN, 28) W10-DESC WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE W11-QUANT(WS-S8)   TO W10-MQNT.
             DISPLAY S38 AT LINE LIN COLUMN COL.
           IF LIN < 22
               ADD 1                 TO LIN WS-S8
               GO TO BB81.
             DISPLAY WS-BLNK78 AT 2202 WITH FOREGROUND-COLOR Cyan.
             GO TO BB70.

       BB85.
             DISPLAY "Input details complete" AT 4624 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM CHECK-CORRECT.
      *
      *    ****    C L E A R   L I N E   4 6
      *
             MOVE 46                 TO CRT-START CRT-END.
             PERFORM ERASE-SCREEN.
           IF WS-OPTION = "N"
               MOVE ZERO             TO W10-MCOST
               MOVE 1                TO WS-S6
               MOVE 8                TO LIN
               GO TO BB70.
             PERFORM AY60 THRU AY999.
           IF WS-AMEND = 1
               MOVE 0                TO WS-ACTION
               PERFORM AY20 THRU AY59.

       BB88.
             INITIALIZE PRT-REC1.
             MOVE WS-ITM             TO PRT-EXT-ITEM.
             MOVE W15-RECORD1        TO STK-RECORD1.
             COMPUTE STK-COST ROUNDED = W10-MCOST * 1.
             MOVE STK-COST           TO W10-CCOST.
             COMPUTE W10-SELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-MARKUP) / 100)).
             COMPUTE W10-CASH ROUNDED = (W10-CCOST + ((W10-CCOST * STK-CMARKUP) / 100)).
             COMPUTE W10-WSALE ROUNDED = (W10-CCOST + ((W10-CCOST * STK-WMARKUP) / 100)).
           IF W05-ROUND = "E"
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF NOT(STK-TAX = ZERO)
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE        TO W10-VWSALE
               MOVE W10-CASH         TO W10-VCASH
               MOVE W10-SELL         TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCASH
               PERFORM CB200.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.
           IF LS-OK = "Y"
               PERFORM BB100 THRU BB199.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
             MOVE 1                  TO WS-S6.
             MOVE W10-COMMENT        TO PRT-COMMENT.

       BB90.
             MOVE W11-EXT-CODE(WS-S6)  TO PRT-EXT-CODE(WS-S6).
             MOVE W11-QUANT(WS-S6)   TO PRT-QUANT(WS-S6).
           IF WS-S6 < 30
               ADD 1                 TO WS-S6
               GO TO BB90.
           IF WS-AMEND = 0
               PERFORM WRITE-SPARTS THRU WRITE-SPARTS-EXIT
               MOVE 1                TO WS-ACTION
               PERFORM AY20 THRU AY59
           ELSE
               PERFORM REWRITE-SPARTS THRU WRITE-SPARTS-EXIT.
             GO TO BB99.

       BB95.
           IF W11-EXT-CODE(WS-S6) = SPACES
               GO TO BB85.
             ADD 1 WS-S6             GIVING WS-S8.
             MOVE SPACES             TO W11-EXT-CODE(WS-S6) W11-DESC(WS-S6).
             MOVE ZERO               TO W11-QUANT(WS-S6).
           IF WS-S8 > 29
               GO TO BB85.

       BB96.
             MOVE W11-EXT-CODE(WS-S8)  TO W11-EXT-CODE(WS-S6).
             MOVE W11-DESC(WS-S8)    TO W11-DESC(WS-S6).
             MOVE W11-QUANT(WS-S8)   TO W11-QUANT(WS-S6).
             MOVE SPACES             TO W11-EXT-CODE(WS-S8) W11-DESC(WS-S8).
             MOVE ZERO               TO W11-QUANT(WS-S8).
           IF WS-S8 < 30
               ADD 1                 TO WS-S6 WS-S8
               GO TO BB96.
             GO TO BB70.

       BB99.
            EXIT.

       BB100.
             PERFORM ERASE-SCREEN.
             DISPLAY S04-N.
             MOVE STK-CODE           TO W10-EXT-ITEM.
             MOVE STK-XREF           TO W10-EXT-CODE2.
             MOVE STK-BIN            TO W10-BIN.
             MOVE STK-DESC           TO W10-DESC.
             MOVE STK-DESC2          TO W10-DESC2.
             MOVE STK-TAX            TO W10-TAX.
             MOVE STK-MARKUP         TO W10-MARKUP.
             MOVE STK-CMARKUP        TO W10-CMARKUP.
             MOVE STK-WMARKUP        TO W10-WMARKUP.
             MOVE STK-IND            TO W10-IND.
             MOVE STK-QUANT          TO W10-QUANT.
             MOVE STK-LEVEL          TO W10-REORD.
             MOVE STK-LTIME          TO W10-LTIME.
             MOVE STK-COST           TO W10-COST.
             MOVE STK-SELL           TO W10-SELL.
             MOVE STK-CASH           TO W10-CASH.
             MOVE STK-WSALE          TO W10-WSALE.
             MOVE STK-LDG            TO W10-LEDG.
             MOVE STK-GUAR           TO W10-GUAR.
             MOVE STK-ORD            TO W10-ORDER.
             MOVE STK-DTE            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY.
      *      MOVE W22-DD2            TO W22-DD3.
      *      MOVE W22-MM2            TO W22-MM3.
      *      MOVE W22-CC2            TO W22-CC3.
      *      MOVE W22-YY2            TO W22-YY3.
             MOVE W22-DTE3           TO W10-ODTE.
             MOVE STK-PUR            TO W10-PURCH.
             MOVE STK-AVGE           TO W10-AVRG.
             MOVE STK-ACODE          TO W10-ACODE.
             MOVE STK-EXEMPT         TO W10-EXEMPT.
             MOVE W10-ALT-DISC       TO STK-ALT-DISC.

       BB104.
             COMPUTE W09-WMARKUP ROUNDED = ((STK-WSALE - STK-COST) * 100.000) / STK-COST.
             COMPUTE W09-MARKUP ROUNDED = ((STK-SELL - STK-COST) * 100.000) / STK-COST.
             COMPUTE W09-CMARKUP ROUNDED = ((STK-CASH - STK-COST) * 100.000) / STK-COST.
             DISPLAY S06C.
             MOVE STK-GUAR           TO W10-GUAR.
             PERFORM AA400.

       BB105.
             MOVE ZERO               TO W05-RATE.
           IF W10-TAX > 0
               MOVE W05-VAT(W10-TAX) TO W05-RATE.

       BB110.
             COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE).
             DISPLAY S06A.
             DISPLAY S06C.

       BB115.
             DISPLAY "F11"               AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                     " to Amend Markups" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       BB120.
             ACCEPT S06D.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F11-KEY PERFORM BB200
                              GO TO BB104
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO BB120
               END-EVALUATE.
             MOVE W10-COST           TO W10-MCOST W10-CCOST.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-MARKUP) / 100)).
             MOVE W10-CSELL          TO W10-SELL.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-CMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-CASH.
             COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-WMARKUP) / 100)).
             MOVE W10-CSELL          TO W10-WSALE.
             DISPLAY S06E.
             PERFORM BB105 THRU BB110.
             ACCEPT S06E.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO BB115.
             MOVE W10-COST           TO STK-COST.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.

       BB199.
             EXIT.

       BB200.
             ACCEPT S06A-B.
           IF W10-CMARKUP < W10-WMARKUP
               MOVE W10-WMARKUP      TO W10-CMARKUP.
           IF W10-MARKUP < W10-CMARKUP
               MOVE W10-CMARKUP      TO W10-MARKUP.
           IF W10-MARKUP = ZERO
               GO TO BB200.
             MOVE W10-MARKUP         TO STK-MARKUP.
             MOVE W10-CMARKUP        TO STK-CMARKUP.
             MOVE W10-WMARKUP        TO STK-WMARKUP.
             COMPUTE W10-SELL ROUNDED = (W10-CCOST + ((W10-CCOST * STK-MARKUP) / 100)).
             COMPUTE W10-CASH ROUNDED = (W10-CCOST + ((W10-CCOST * STK-CMARKUP) / 100)).
             COMPUTE W10-WSALE ROUNDED = (W10-CCOST + ((W10-CCOST * STK-WMARKUP) / 100)).
           IF W05-ROUND = "E"
               MOVE W10-SELL         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-SELL
               MOVE W10-WSALE        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-WSALE
               MOVE W10-CASH         TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-CASH.
           IF NOT(STK-TAX = ZERO)
               MOVE W05-VAT(STK-TAX) TO W05-RATE
               COMPUTE W10-VSELL ROUNDED = W10-SELL + (W10-SELL * W05-RTE)
               COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE)
               COMPUTE W10-VCASH ROUNDED = W10-CASH + (W10-CASH * W05-RTE)
           ELSE
               MOVE W10-WSALE        TO W10-VWSALE
               MOVE W10-CASH         TO W10-VCASH
               MOVE W10-SELL         TO W10-VSELL.
           IF W05-ROUND = "I"
               MOVE W10-VSELL        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VSELL
               MOVE W10-VWSALE       TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VWSALE
               MOVE W10-VCASH        TO W10-RSELL
               PERFORM CB100
               MOVE W10-RSELL        TO W10-VCASH
               PERFORM CB200.
             MOVE W10-SELL           TO STK-SELL.
             MOVE W10-CASH           TO STK-CASH.
             MOVE W10-WSALE          TO STK-WSALE.

       BB299.
             EXIT.

       BB999.
             EXIT.

       BC000        SECTION 5.
       BC00.
             MOVE W10-EXT-ITEM       TO W10-EXT-ENTRY.
             MOVE STK-BC             TO W10-EXT-ITEM W10-EXT-CODE2 STK-XREF.
             DISPLAY W10-EXT-CODE2 AT 0821 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE 14                 TO WS-S8.

       BC05.
           IF W10-IC(WS-S8) = SPACE
               SUBTRACT 1            FROM WS-S8
               GO TO BC05.
             MOVE WS-S8              TO W10-SLNGTH.
             EVALUATE W10-SLNGTH
               WHEN 6     ADD 1      TO W10-I6
               WHEN 7     ADD 1      TO W10-I7
               WHEN 8     ADD 1      TO W10-I8
               WHEN 9     ADD 1      TO W10-I9
               WHEN 10    ADD 1      TO W10-I10
               WHEN 11    ADD 1      TO W10-I11
               WHEN 12    ADD 1      TO W10-I12
               WHEN 13    ADD 1      TO W10-I13
               WHEN 14    ADD 1      TO W10-I14
             END-EVALUATION.
             MOVE W10-ITEM           TO STK-BC.
             MOVE W10-EXT-ENTRY      TO W10-EXT-ITEM.

       BC999.
             EXIT.
      *
      *    ****    C O N V E R T I O N  F A C T O R
      *
       BD000        SECTION 5.
       BD00.
             PERFORM SAVE-SCREEN-3.
             MOVE 10                 TO SHADE-ROW.
             MOVE 25                 TO SHADE-COL.
             MOVE 32                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S-CONV.
             PERFORM SCREEN-SHADOW.

       BD05.
             ACCEPT SA-FACT.
             PERFORM CHECK-CORRECT.
             PERFORM RESTORE-SCREEN-3.
           IF WS-OPTION = "N"
               GO TO BD00.

       BD999.
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
             MOVE PAR-BAR-CODE       TO W50-BAR-CODE.
             MOVE PAR-ALLOC-BC       TO W50-ALLOC-BC.
             MOVE PAR-CASES          TO WS-USE-CASES.
             MOVE PAR-PACKS          TO WS-USE-PACKS.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
           IF NOT(PAR-ART11 = "Y" OR "N")
               MOVE "N"              TO PAR-ART11.
             MOVE PAR-ART11          TO W10-ART11.
             MOVE PAR-CRDX           TO WS-CARDEX.

       ZA01B.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-WEEK           TO WS-WEEK.
             MOVE STK-RECORDS        TO WS-RECORDS.
             MOVE STK-PASSWORD       TO WS-PASSWORD.
             MOVE STK-CON-FACT       TO WS-CON-FACT.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ROUND          TO W05-ROUND.
             MOVE PAR-ROUND-VAL      TO W05-ROUND-VAL.
           IF W05-ROUND-VAL > 0.01
               COMPUTE W05-HLF-VAL = W05-ROUND-VAL / 2
           ELSE
               MOVE ZERO             TO W05-HLF-VAL.
           IF W05-HLF-VAL > 0.01
               COMPUTE W05-TQTR-VAL = W05-HLF-VAL * 1.5
           ELSE
               MOVE ZERO             TO W05-TQTR-VAL.
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
             MOVE 8                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-DESC-H1 = X"0000000000000000000000000000")
               MOVE PAR-DESC-H1      TO WS-DESC-H1.
           IF NOT(PAR-DESC-H2 = X"0000000000000000000000000000")
               MOVE PAR-DESC-H2      TO WS-DESC-H2.
           IF NOT(PAR-WS-HD = X"00000000000000000000")
               MOVE PAR-WS-HD        TO WS-WS-HD.
           IF NOT(PAR-CS-HD = X"00000000000000000000")
               MOVE PAR-CS-HD        TO WS-CS-HD.
           IF NOT(PAR-RT-HD = X"00000000000000000000")
               MOVE PAR-RT-HD        TO WS-RT-HD.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             GO TO ZA999-EXIT.

      *COPY "ZA49.PRO".
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

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
               WHEN 21    MOVE W02-SPARTS   TO WS-FILE
                          MOVE PRT-ITEM     TO WS-KEYX
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 37    MOVE W02-SHARED   TO WS-FILE
                          MOVE WS-SHARED    TO WS-KEY
               WHEN 46    MOVE W02-STKDEX   TO WS-FILE
                          MOVE STX-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STP001"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
