      $SET LINKCOUNT"384" GNT"STPSEN.GNT"
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
      *    ******   ********  *******    ******   ********  **    **   *
      *   **    **     **     **    **  **    **  **        ***   **   *
      *   **           **     **    **  **        **        ****  **   *
      *    ******      **     *******    ******   *****     ** ** **   *
      *         **     **     **              **  **        **  ****   *
      *   **    **     **     **        **    **  **        **   ***   *
      *    ******      **     **         ******   ********  **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S   T R A C K I N G   P R O G R A M                *
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
       PROGRAM-ID.     STPSEN.
       AUTHOR.         J.W.LEMMON.
       DATE-WRITTEN.   OCTOBER 1995.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2018
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

       COPY "APACFIDS.SL".

       COPY "CARDEX.SL".

      $SET KEYCOMPRESS "7"
          SELECT SALESTRK ASSIGN DISK
            STATUS WS-STATUS
            ACCESS DYNAMIC
            ORGANIZATION INDEXED
            RECORD SAL-KEY.
      $SET NOKEYCOMPRESS

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

       COPY "APACFIDS.FDE".

       COPY "CARDEX.FDE".

       FD  SALESTRK  IS EXTERNAL
       LABEL RECORD STANDARD
             VALUE OF FILE-ID W02-SALESTRK.
       01  SAL-REC1.
          03  SAL-KEY.
        05  SAL-DTE     PIC  9(06)    COMP.
        05  SAL-SEQ     PIC  9(04)    COMP.
    03  SAL-CODE        PIC  X(14).
    03  SAL-SMAN        PIC  9(02).
    03  SAL-REF        PIC  X(08).
    03  SAL-AC        PIC  X(06).
    03  SAL-NAME.
        05  SAL-NME     PIC  X(19).
        05  FILLER      PIC  X(21).
    03  SAL-DESC        PIC  X(30).
          03  SAL-QNT        PIC S9(07)V99 COMP-3.
    03  SAL-COST        PIC S9(07)V99 COMP-3.
    03  SAL-SELL        PIC S9(07)V99 COMP-3.

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
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-S            PIC  9(04)    COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-SAL-STAT    PIC X(01) VALUE "N".
    88 CLOSE-SAL    VALUE "N".
       77  WS-OPTION       PIC  X(01).
       77  WS-MONTH    PIC 9(02).
       77  WS-ETYPE    PIC X(01).
       77  WS-DESC-H1     PIC  X(14) VALUE "Description".
       77  WS-DESC-H2     PIC  X(14) VALUE "Description 2".
       77  WS-RT-HD     PIC  X(10) VALUE "-Retail   ".
       77  WS-WS-HD     PIC  X(10) VALUE "-Wholesale".
       77  WS-CS-HD     PIC  X(10) VALUE "-Cash sale".
       77  WS-ERROR    PIC 9(01) VALUE ZERO.
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.

      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "STPSEN".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "STOCK/INVENTORY -SALES TRACKING".
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
    03  WS-PROG        PIC  X(03) VALUE "SEN".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W10-STOCK.
          03  W10-ITEM       PIC  X(14).
          03  W10-DATE.
        05  W10-EDTE   PIC 99/99/9999.

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

       01  W15-DATES.
          03  W15-START       PIC 9(08).
           03  W15-SDTE-DMY REDEFINES W15-START.
               05  W15-SDAY    PIC 9(02).
               05  W15-SMNTH   PIC 9(02).
        05  W15-SCENT   PIC 9(02).
               05  W15-SYEAR   PIC 9(02).
          03  W15-S-YMD       PIC 9(08).
           03  W15-STRT-YMD REDEFINES W15-S-YMD.
        05  W15-SCC     PIC 9(02).
               05  W15-SYY     PIC 9(02).
               05  W15-SMM     PIC 9(02).
               05  W15-SDD     PIC 9(02).
          03  W15-END        PIC 9(08).
           03  W15-EDTE-DMY REDEFINES W15-END.
               05  W15-EDAY    PIC 9(02).
               05  W15-EMNTH   PIC 9(02).
        05  W15-ECENT   PIC 9(02).
               05  W15-EYEAR   PIC 9(02).
          03  W15-E-YMD       PIC 9(08).
           03  W15-END-YMD REDEFINES W15-E-YMD.
        05  W15-ECC     PIC 9(02).
               05  W15-EYY     PIC 9(02).
               05  W15-EMM     PIC 9(02).
               05  W15-EDD     PIC 9(02).

       COPY "W25.STK".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE        PIC 9(02).
           03  W45-ENG         PIC X(12).
           03  W45-AFR         PIC X(12).
           03  W45-ACT         PIC X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
    03  W75-S2       PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
               05  W95-DTE     PIC Z9/99/99.
           03  W95-POST        PIC 9(04).
           03  W95-TEL         PIC X(08).

       01  W100-EDIT.
          03  W100-COST       PIC Z(06)9.99.
          03  W100-SELL       PIC Z(06)9.99.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99.
           03  W100-CRL        PIC Z(06)9.
           03  W100-KEY        PIC Z(05)9.
    03  W100-SER        PIC ZZ9.
          03  W100-PRC        PIC Z(06)9.99-.
          03  W100-VAL        PIC Z(08)9.99-.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
          03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
          03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
        05  FILLER      PIC  X(02).
        05  W100-QNT    PIC Z(04)9.99.

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

       01  W16-CALL.
    03  W16-ITEM      PIC  X(14).
    03  W16-CRED      PIC  X(05).
    03  W16-COST      PIC S9(07)V99 COMP-3.
    03  W16-CANCEL    PIC  X(01).

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

       01  S05D.
          03  LINE  2 COLUMN 34 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "SALES TRACKING".
    03  LINE  4 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "здддддддддддддддддддддддддддддддд©".
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "Ё    Transactions for Period.    Ё".
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "цдддддддддддддддддддддддддддддддд╢".
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "Ё   Starting Date :              Ё".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "Ё   Ending Date   :              Ё".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "цдддддддддддддддддддддддддддддддд╢".
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "ЁDates to be keyed in as ".
    03        COLUMN 49 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "DDMMCCYY".
    03        COLUMN 57 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE "Ё".
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Brown VALUE
    "юдддддддддддддддддддддддддддддддды".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC 99/99/9999 USING W15-END AUTO.

       01  S10D.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT VALUE
               "SALES TRACKING MOVEMENT/REFERENCE LOOKUP".
    03  LINE  3 COLUMN  3 VALUE "Item Code.".

       01  S36.
          03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
      PIC X(14) USING W10-ITEM AUTO.

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "Ref. No.   Date   Ac/No  SP Cost Price Sell.Price Quanti
      -        "ty Customer Name      ".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR Black VALUE "ф".
          03        COLUMN  2 VALUE "ммммммммяммммммммяммммммяммямммм
      -        "ммммммяммммммммммяммммммммяммммммммммммммммммм".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "╣".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR Black VALUE "ф".
          03        COLUMN  2 VALUE "ммммммммоммммммммоммммммоммомммммммммоммммммммммоммммммммоммммммммммммммммммм".
    03        COLUMN 80 BACKGROUND-COLOR Black VALUE "╣".

      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID
    LS-USER-ID
    LS0-PROGRAMS
    LS0-SECURITY
    W16-CALL.
       AA000-MAIN        SECTION.
       AA000-INIT.
      MOVE "Y"   TO W16-CANCEL.
      MOVE W16-ITEM  TO W10-ITEM.
            PERFORM ZA000-INIT.
      PERFORM BA000.
    IF CLOSE-SAL
        CLOSE CARDEX.

       AA49.
             EXIT PROGRAM.

       COPY "AA50.LUP".

       COPY "AA900.ALM".

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      CHECK-CORRECT                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё           "Press Y if correct - N if incorrect"           Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 20 (default); move another value to WS-MES-LINE.          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Move the message to WS-DSP-MES and the top line of the   Ё
      *    Ё  message box to WS-MES-LINE. If WS-MES-LINE is zero the   Ё
      *    Ё  default will be 20.                                      Ё
      *    Ё                  PERFORM DISPLAY-MESSAGE                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
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
      *    Ё           ERASE SCREEN FROM SPECIFIED LOCATION            Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CRT-START as starting line and increases and CRT-END Ё
      *    Ё as the ending line. The screen is cleared with Column 1   Ё
      *    Ё and 80 containing "Ё" and columns 2 to 79 containing      Ё
      *    Ё spaces.                                                   Ё
      *    Ё                                                           Ё
      *    Ё  eg.                                                      Ё
      *    Ё   MOVE 8         TO CRT-START.                            Ё
      *    Ё   MOVE 28        TO CRT-END.                              Ё
      *    Ё   PERFORM ERASE-SCREEN.                                   Ё
      *    Ё                                                           Ё
      *    Ё To clear the entire screen and Display a new screen with  Ё
      *    Ё headings (program/date/time/company):                     Ё
      *    Ё                                                           Ё
      *    Ё    CRT-PROGRAM (calling program)                          Ё
      *    Ё    CRT-HEADING (Screen heading)                           Ё
      *    Ё    CRT-COMPANY (Company name)                             Ё
      *    Ё    CRT-TERMINAL (consists of two fields:                  Ё
      *    Ё                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    Ё
      *    Ё    These fields should be populated at the start of the   Ё
      *    Ё    program.                                               Ё
      *    Ё    PERFORM HEADING-AND-CLEAR-SCREEN.                      Ё
      *    Ё                                                           Ё
      *    Ё To change the screen heading:                             Ё
      *    Ё    MOVE "The new heading" TO CRT-HEADING.                 Ё
      *    Ё    PERFORM CHANGE-HEADING.                                Ё
      *    Ё                                                           Ё
      *    Ё To change the time on the screen:                         Ё
      *    Ё    PERFORM CHANGE-TIME                                    Ё
      *    Ё                                                           Ё
      *    Ё To clear lines 46 and 50:                                 Ё
      *    Ё    PERFORM CLEAR-L46-AND-50                               Ё
      *    Ё                                                           Ё
      *    Ё To clear line 50:                                         Ё
      *    Ё    PERFORM CLEAR-L50                                      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

       COPY "DATE.CNV".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AC000              SECTION.

       COPY "CARDEX.RD".

      /
      *    ****    M O V E M E N T   L O O K U P
      *
       MOVEMENT-LOOKUP   SECTION.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE CRD-REC1.
      MOVE W10-ITEM  TO CRD-CODE.
      MOVE W15-S-YMD  TO CRD-DTE.

       MOVE-LUP-CLR-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-DNO (W75-S1).
    IF W75-S1 < 18
               ADD 1             TO W75-S1
        GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-CRDX.
      MOVE SPACES  TO WS-OPTION.
            PERFORM START-AT-CRDX-DSKEY THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO MOVE-NO-RECORD.

       MOVE-GET-CARDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO MOVE-SKIP-CRDX-READ.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (CRD-CODE = W10-ITEM)
        GO TO MOVE-FULL-SCREEN.
    IF (CRD-DTE > W15-E-YMD)
        GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-CRDX-READ.
      MOVE CRD-AC  TO W75-DNO (W75-S).
            DISPLAY (CLIN, 2) CRD-REF WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE CRD-DTE            TO W22-DTE2.
             PERFORM SWITCH-DATE-TO-DMCY THRU SWITCH-DATE-EXIT.
             MOVE W22-DTE3           TO W95-DTE.
            DISPLAY ( , 11) W95-DTE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 20) CRD-AC WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY ( , 27) CRD-SMAN WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      MOVE CRD-COST  TO W100-COST.
      MOVE CRD-SELL  TO W100-SELL.
      MOVE CRD-QNT  TO W100-QNT.
            DISPLAY ( , 30) W100-COST WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 41) W100-SELL WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 52) W100-QNT WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      DISPLAY ( , 61) CRD-NME WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
    IF CRD-SELL < ZERO
        DISPLAY ( , 78) " C" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    ELSE
        DISPLAY ( , 78) " I" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-CARDEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR Brown HIGHLIGHT
             " - " WITH FOREGROUND-COLOR Grey
       "PgUp" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "/" WITH FOREGROUND-COLOR Grey
       "PgDn" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       " - " WITH FOREGROUND-COLOR Grey
             "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "ape." WITH FOREGROUND-COLOR Grey.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      PERFORM AA900-ALARM.
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-CARDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-CARDEX-PREV THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-NEW-PAGE.
    IF NOT (CRD-CODE = W10-ITEM)
        PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "ANY" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " key"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CRT-EDGE.
            DISPLAY "Ё" AT CPOS WITH BACKGROUND-COLOR Black
          FOREGROUND-COLOR Cyan
       "        Ё        Ё      Ё  Ё          Ё          Ё        Ё                   "
        WITH FOREGROUND-COLOR Cyan
       "Ё" WITH BACKGROUND-COLOR Black
         FOREGROUND-COLOR Cyan.
            ADD 1   TO CLIN.


       MOVEMENT-LOOKUP-EXIT.
             EXIT.

       COPY STOCK.LUP.

       COPY "APAC.HLP".

      *
      *  ****  E N Q U I R Y   ( S A L E S   T R A C K I N G )
      *
       BA000         SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-TODAY  TO W15-START W15-END.
    IF W15-SMNTH < 3
        ADD 12   TO W15-SMNTH
        SUBTRACT 1  FROM W15-SYEAR.
      SUBTRACT 2   FROM W15-SMNTH.

       BA05.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan
       "Esc" WITH FOREGROUND-COLOR Brown HIGHLIGHT
    BACKGROUND-COLOR Cyan
       "ape to Exit"
        WITH FOREGROUND-COLOR Blue BACKGROUND-COLOR Cyan.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA999
   WHEN OTHER  PERFORM AA900-ALARM
       GO TO BA05
        END-EVALUATE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA05.
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
               GO TO BA05.
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
               GO TO BA05.
             MOVE W20-DTE            TO W15-E-YMD.
           IF W15-E-YMD < W15-S-YMD
               MOVE "End date is before start date" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO BA05.

       BA10.
             PERFORM ERASE-SCREEN.
      DISPLAY WS-BLNK78 AT 0302
       WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
      DISPLAY S10D.

       BA15.
      *
      *    ****    D I S P L A Y   S T O C K   C O D E
      *
            DISPLAY S36 AT 0314.
      PERFORM MOVEMENT-LOOKUP.

       BA999.
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
             MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
            OPEN I-O CARDEX.
    IF WS-STATUS = "05"
        CLOSE CARDEX
        OPEN OUTPUT CARDEX
        CLOSE CARDEX
        OPEN I-O CARDEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT OR
   (WS-STATUS = "39")
     MOVE 43  TO WS-F-ERROR
     GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 43   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 43
              MOVE "CARDEX file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

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
               WHEN 6     MOVE W02-DEBTOR   TO WS-FILE
                          MOVE DEB-ACNO     TO WS-KEYX
               WHEN 15    MOVE WS-PARID     TO WS-FILE
                          MOVE WS-PARKEY    TO WS-KEY
               WHEN 22    MOVE W02-STOCKF   TO WS-FILE
                          MOVE STK-CODE     TO WS-KEYX
               WHEN 25    MOVE W02-STKMEM   TO WS-FILE
                          MOVE SME-KEY      TO WS-KEYX
               WHEN 43    MOVE W02-CARDEX   TO WS-FILE
                          MOVE CRD-KEY      TO WS-KEYX
               WHEN OTHER MOVE "ERROR"      TO WS-FILE
                          MOVE "STPSEN"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
