      $set LINKCOUNT"384" GNT"CRP024.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******       ***    *
      *   **    **  **    **  **    **   **  **   **    **     * **    *
      *   **        **    **  **    **  **    **       **     *  **    *
      *   **        ******    *******   **    **     **      *   **    *
      *   **        **   **   **        **    **    **      ********   *
      *   **    **  **    **  **         **  **    **            **    *
      *    ******   **    **  **          ****    ********       **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   R E M I T T A N C E   A D V I C E S  *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
      *                                                                *
      * June 2018 - Added program CRPRDWR to handle IO for Creditor    *
      *             file and COPY file CA155.CRP to call and handlw IO *
      *             for the CREDITOR file. SEE CA155.CRP and CRPRDWR   *
      *             for details.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CRP024.
       AUTHOR.          J.W. LEMMON (APAC).
       DATE-WRITTEN.    AUGUST, 1983.

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

       COPY "CRDMEM.SL".

       COPY "CREDIT.SL".

       COPY "CRTRAN.SL".

       COPY "PARAM.SL".

       COPY "PDCHEQ.SL".

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

       COPY "CRDMEM.FDE".

       COPY "CREDIT.FDE".

       COPY "CRTRAN.FDE".

       COPY "PARAM.FDE".

       COPY "PDCHEQ.FDE".

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
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-ORD                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-IND                  PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
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
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP024".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - REMITTANCE ADVICES".
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
           03  WS-MODULE           PIC  X(03)           VALUE "CRP".
           03  WS-PROG             PIC  X(03)           VALUE "024".

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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-H1            PIC  X(06).
               05  R-DTE1          PIC  Z9/99/9999.
               05  FILLER          PIC  X(18).
               05  R-H2.
                   07  FILLER      PIC  X(11).
                   07  R-H2B       PIC  X(41).
                   07  FILLER      PIC  X(11).
               05  FILLER          PIC  X(26).
               05  R-H3            PIC  X(06).
               05  R-PGE           PIC  ZZ9.

           03  R-L12                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(31).
               05  ST-H1           PIC  X(70).
               05  FILLER          PIC  X(31).

           03  R-L13                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  ST-NADD         PIC  X(40).
               05  FILLER          PIC  X(10).
               05  ST-H2           PIC  X(12).
               05  ST-ACNO         PIC  X(06).
               05  FILLER          PIC  X(08).
               05  ST-H3           PIC  X(06).
               05  ST-DATE         PIC  Z9/99/9999.
               05  FILLER          PIC  X(08).
               05  ST-H4           PIC  X(06).
               05  ST-PAGE         PIC  Z(03)9.
               05  FILLER          PIC  X(12).

           03  R-L14                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(10).
               05  ST-POST         PIC  9(04).
               05  FILLER          PIC  X(75).
               05  ST-H5           PIC  X(26).
               05  FILLER          PIC  X(17).

           03  R-L15                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  ST-H6           PIC  X(15).
               05  ST-H7           PIC  X(12).
               05  ST-H8           PIC  X(20).
               05  ST-H9           PIC  X(20).
               05  ST-H10          PIC  X(17).
               05  ST-H11          PIC  X(29).
               05  ST-H12          PIC  X(14).

           03  R-L16                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  ST-CODE         PIC  9(02).
               05  FILLER          PIC  X(01).
               05  ST-TRN          PIC  X(08).
               05  FILLER          PIC  X(04).
               05  ST-ORD          PIC  9(05)/99   BLANK WHEN ZERO.
               05  FILLER          PIC  X(04).
               05  ST-GRNN         PIC  Z(05)9     BLANK WHEN ZERO.
               05  FILLER          PIC  X(01).
               05  ST-GRND         PIC  Z9/99/9999 BLANK WHEN ZERO.
               05  FILLER          PIC  X(01).
               05  ST-INVN         PIC  Z(07)9     BLANK WHEN ZERO.
               05  FILLER          PIC  X(01).
               05  ST-INVD         PIC  Z9/99/9999 BLANK WHEN ZERO.
               05  FILLER          PIC  X(03).
               05  ST-COST         PIC  Z(06)9.99DB.
               05  FILLER          PIC  X(05).
               05  ST-NAR.
                   07  ST-30       PIC  X(01).
                   07  FILLER      PIC  X(03).
                   07  ST-60       PIC  X(01).
                   07  FILLER      PIC  X(03).
                   07  ST-90       PIC  X(01).
                   07  FILLER      PIC  X(03).
                   07  ST-120      PIC  X(01).
                   07  FILLER      PIC  X(03).
                   07  ST-150      PIC  X(01).
                   07  FILLER      PIC  X(03).
                   07  ST-180      PIC  X(01).
                   07  FILLER      PIC  X(05).
                   07  ST-BAL      PIC  Z(06)9.99DB.
                   07  FILLER      PIC  X(05).

           03  R-L17                             REDEFINES W02-PRN-LINE.
               05  ST-H13                        OCCURS 12.
                   07  FILLER      PIC  X(05).
                   07  ST-H14      PIC  X(06).

           03  R-L18                             REDEFINES W02-PRN-LINE.
               05  ST-DUE                       OCCURS 12.
                   07  ST-VAL      PIC Z(06)9.99-.

       01  W10-MONTHS.
           03  W10-MONTH-NAMES PIC X(36)     VALUE
                               "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W10-MONTH       REDEFINES W10-MONTH-NAMES
                               PIC X(03)     OCCURS 12.

       01  W10-STOCK.
           03  W10-VSELL      PIC S9(07)V99  COMP-3.
           03  W10-VCASH      PIC S9(07)V99  COMP-3.
           03  W10-VWSALE     PIC S9(07)V99  COMP-3.

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

       COPY "W15.CRP".

    03  W15-YMD      PIC  9(08).
           03  W15-RYMD REDEFINES W15-YMD.
        05  W15-CN    PIC  9(02).
        05  W15-YR    PIC  9(02).
        05  W15-MT    PIC  9(02).
        05  W15-DY    PIC  9(02).

       01  W20-CALCS.
           03  W20-RESULT      PIC 9(06)V99.
           03  W20-RESULT-1    REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(06).
               05  W20-DECIMAL PIC 9(02).
           03  W20-ORD         PIC 9(07)     COMP-3.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).

       01  W25-VALUES.
           03  W25-CFLOW OCCURS 6 PIC S9(07)V99 COMP-3.
          03  W25-V1        PIC S9(09)V99 COMP-3.
          03  W25-V2        PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
               05  W25-1       PIC ZZZZ9.
           03  W25-3V2.
               05  W25-2       PIC ZZ9.99.
           03  W25-7V2.
               05  W25-3       PIC Z(06)9.99.
           03  W25-3N.
               05  W25-4       PIC ZZ9.
           03  W25-DTE.
              05  W25-DATE    PIC 99/99/9999.
           03  W25-6.
               05  W25-NO      PIC Z(05)9.
           03  W25-5V2.
               05  W25-VAL     PIC Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL    PIC Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2    PIC Z(06)9.99DB.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-H2.
           03  W60-COMPANY     PIC X(40).
           03  FILLER          PIC X(02)     VALUE "-".
           03  W60-DESC        PIC X(27).

       01  W65-CREDITOR.
          03  W65-CFLOW       PIC S9(09)V99 COMP-3 OCCURS 6.

       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR OCCURS 2 PIC X(01).

       01  W70-VALUES.
          03  W70-BAL        PIC S9(09)V99 COMP-3.
          03  W70-AREAR       PIC S9(09)V99 COMP-3.
          03  W70-30D        PIC S9(09)V99 COMP-3.
          03  W70-60D        PIC S9(09)V99 COMP-3.
          03  W70-90D        PIC S9(09)V99 COMP-3.
          03  W70-120D        PIC S9(09)V99 COMP-3.
          03  W70-150D        PIC S9(09)V99 COMP-3.
          03  W70-180D        PIC S9(09)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
    03  W75-CNO      PIC  X(06) OCCURS 18.

       01  W90-TABLE.
           03  W90-ENTRIES.
               05  W90-ENTRY    OCCURS 80.
                   07  W90-NAME PIC X(04).
                   07  W90-KEY  PIC 9(04)     COMP-5.
           03  W90-R1           REDEFINES W90-ENTRIES.
               05  W90-F20      PIC X(120).
               05  W90-L60      PIC X(360).
           03  W90-R2           REDEFINES W90-ENTRIES.
               05  W90-F60      PIC X(360).
               05  W90-L20      PIC X(120).
           03  W90-R3           REDEFINES W90-ENTRIES.
               05  W90-20       PIC X(120).
               05  W90-40       PIC X(120).
               05  W90-60       PIC X(120).
               05  W90-80       PIC X(120).

       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

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

       01  S19.
          03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC X(06) USING W15-ACNO AUTO.

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

      *COPY "CRDLUP.CRT".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey VALUE
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E   ".
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black
                                 BACKGROUND-COLOR Grey
                                 PIC X(60) USING W200-MEM12 AUTO.

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
       AA000-START-UP  SECTION.
       AA000.
          IF LS0-CRLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA105.
             PERFORM ZA000-INIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             PERFORM FA000.
             GO TO AA100.

       AA050.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W60-H2             TO R-H2.
             MOVE WS-H3              TO R-H3.
             ADD 1                   TO WS-PAGE.
             MOVE TODAY-DDMMYY       TO R-DTE1.
             MOVE WS-PAGE            TO R-PGE.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       AA100.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             PERFORM ERASE-SCREEN.

       AA105.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
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
      *    *************************************************************
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
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

       COPY "ABORT.PRN".

      *
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *                PRINTER SUCH AS OPEN AND PRINT
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³             PERFORM OPEN-PRINT-FILE                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *
      *         PRINT A LINE, ADVANCE LINE/PAGE, CHANGE STYLE ETC
      *     The following variables need to be set before calling this
      *     routine.
      *     
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
      *       WS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Lpt1
      *             2 = Lpt2
      *             3 = Lpt3
      *             4 = Com1
      *             5 = Com2
      *             8 = USB/WINDOWS
      *
      *       WS-PRINTER.
      *         WS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
      *       WS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
      *      WS-REPORT           PIC  9(01).
      *
      *      See W02-PRN-TYPE for details of Print style.
      *
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³               PERFORM CALL-PRINTUTL                       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´

       COPY "PRINTUTL.AP4".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ SECTION.

       COPY "CONTROL.RD".

       COPY "CRTRAN.RD".

       COPY "CRDMEM.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

       COPY "PDCHEQ.RD".

      /
       AE155-GET-CREDITOR      SECTION.
       AE155-ACCOUNT.
             MOVE W15-ACNO       TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
              MOVE 1   TO WS-ERROR
           ELSE
           IF P-NUMBER NOT = W15-ACNO
              MOVE 2   TO WS-ERROR
           ELSE
              MOVE 0   TO WS-ERROR.
       AE160-EXIT.
             EXIT.

      /
       FA000      SECTION 5.
       FA00.
             PERFORM ERASE-SCREEN.
            DISPLAY "Busy printing remittance advices" AT 0620
              WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      *
      *    ****   1 7 C P I ( C O N D E N S E D   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 3   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            DISPLAY "Selective print (" AT 0820
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")?".
            ACCEPT WS-OPTION AT 0843
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
              MOVE ZERO  TO WS-IND
           ELSE
              MOVE 1   TO WS-IND
        MOVE SPACES  TO W15-ACNO
              GO TO FA80.
            DISPLAY "Print Accounts with ZERO balance (" AT 1020
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT ")?".

       FA02.
            ACCEPT WS-OPTION AT 1060
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
   IF NOT(WS-OPTION = "N" OR "Y")
       GO TO FA02.
      MOVE WS-OPTION  TO WS-ZERO.
      DISPLAY "Account:" AT 1220.

       FA05.
             MOVE "    !"        TO P-NAME.
             PERFORM START-AT-CRED-NAME THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO FA75.

       FA10.
             PERFORM READ-CREDIT-NEXT THRU READ-CREDIT-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO FA75.
      MOVE P-NUMBER  TO CT-CRED.
      MOVE ZERO   TO CT-SDTE
        CT-CODE
        CT-SEQ.
      MOVE SPACES  TO CT-SINV.
    IF P-PBAL = ZERO AND
       WS-ZERO = "N"
        GO TO FA10.
      DISPLAY P-NUMBER AT 1229
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta "°"
       P-NAME WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       FA12.
            MOVE P-PBAL  TO W25-V1.
            MOVE SPACES  TO W15-ADDRESS.
            MOVE P-NAME  TO W15-NAME.
             UNSTRING P-ADDR DELIMITED BY "*"
              INTO W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4.
            MOVE P-PC   TO W15-PCODE.
            MOVE ZERO   TO WS-PAGE.

       FA15.
            MOVE SPACES  TO R-DET.
             MOVE "CREDITORS REMITTANCE ADVICE" TO W60-DESC.
            MOVE W60-H2  TO ST-H1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE ALL "="  TO ST-H1.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W15-NAME  TO ST-NADD.
            MOVE "ACCOUNT NO:"  TO ST-H2.
            MOVE P-NUMBER  TO ST-ACNO.
            MOVE "DATE:"  TO ST-H3.
            MOVE TODAY-DDMMYY  TO ST-DATE.
            MOVE "PAGE:"  TO ST-H4.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO ST-PAGE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W15-ADDR1  TO ST-NADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE 5   TO WS-ERROR.
          IF W15-ADDR2 = SPACES
               GO TO FA20.
            SUBTRACT 1   FROM WS-ERROR.
            MOVE W15-ADDR2  TO ST-NADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
          IF W15-ADDR3 = SPACES
               GO TO FA20.
            SUBTRACT 1   FROM WS-ERROR.
            MOVE W15-ADDR3  TO ST-NADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
          IF W15-ADDR4 = SPACES
               GO TO FA20.
            SUBTRACT 1   FROM WS-ERROR.
            MOVE W15-ADDR4  TO ST-NADD.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       FA20.
            MOVE W15-PCODE  TO ST-POST.
            MOVE WS-ERROR  TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE "TRANSACTION"  TO ST-H6.
            MOVE "ORDER NO"  TO ST-H7.
            MOVE "REF NO  TRN DATE"
     TO ST-H8.
            MOVE "INV NO  INV DATE"
     TO ST-H9.
            MOVE "COST PRICE"  TO ST-H10.
            MOVE "30  60  90  120 150 180"
     TO ST-H11.
            MOVE "BALANCE"  TO ST-H12.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE ZERO   TO W20-WHOLE.
           IF W25-V1 = ZERO
               GO TO FA25.
            MOVE "BALANCE BROUGHT FORWARD"
     TO ST-H5.
            MOVE W25-V1  TO ST-BAL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            ADD 1   TO W20-WHOLE.

       FA25.
             EXIT.

       FA30.
      MOVE P-NUMBER  TO CT-CRED.
      MOVE ZERO   TO CT-SDTE
        CT-CODE
        CT-SEQ.
      MOVE SPACES  TO CT-SINV.
      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
    IF WS-F-ERROR = 4
        GO TO FA50.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF CT-CRED NOT = P-NUMBER
        GO TO FA50.

       FA35.
            MOVE CT-CODE  TO ST-CODE.
          IF CT-CODE = 01 OR 11
              MOVE "INVOICE"  TO ST-TRN
           ELSE
          IF CT-CODE = 03
              MOVE "CHEQUE"  TO ST-TRN
           ELSE
          IF CT-CODE = 04 OR 14
              MOVE "EFT PMT"  TO ST-TRN
           ELSE
           IF CT-CODE = 08
              MOVE "DISCOUNT"  TO ST-TRN
           ELSE
          IF CT-CODE = 02 OR 12
              MOVE "DT NOTE"  TO ST-TRN
           ELSE
           IF CT-CODE = 09 OR 10
              MOVE "JOURNAL"  TO ST-TRN
           ELSE
           IF CT-CODE = 05 OR 06
              MOVE "TRANSFER"  TO ST-TRN
           ELSE
              MOVE "UNKNOWN"  TO ST-TRN.
           IF NOT(CT-ORD = ZERO)
              MOVE CT-ORD  TO ST-ORD.
    IF CT-REF NUMERIC
        MOVE CT-REFNO  TO ST-GRNNO
    ELSE
              MOVE CT-REF  TO ST-GRNN.
            MOVE CT-DTE  TO W15-YMD.
      MOVE W15-DY  TO W15-DD.
      MOVE W15-MT  TO W15-MM.
      MOVE W15-CN  TO W15-CC.
      MOVE W15-YR  TO W15-YY.
      MOVE W15-DATE  TO ST-GRND.
    IF CT-SINV NUMERIC
        MOVE CT-SINN  TO ST-INVNO
    ELSE
              MOVE CT-SINV  TO ST-INVN.
            MOVE CT-SDTE  TO W15-YMD.
      MOVE W15-DY  TO W15-DD.
      MOVE W15-MT  TO W15-MM.
      MOVE W15-CN  TO W15-CC.
      MOVE W15-YR  TO W15-YY.
      MOVE W15-DATE  TO ST-INVD.
            MOVE CT-COST  TO ST-COST.
            ADD CT-COST  TO W25-V1.
            MOVE W25-V1  TO ST-BAL.
           IF CT-AGE = ZERO
               GO TO FA40.
            MOVE CT-AGE  TO WS-S4.
           IF WS-S4 > 31
              SUBTRACT 32  FROM WS-S4
              MOVE "Y"   TO ST-180.
           IF WS-S4 > 15
              SUBTRACT 16  FROM WS-S4
              MOVE "Y"   TO ST-150.
           IF WS-S4 > 7
              SUBTRACT 8  FROM WS-S4
              MOVE "Y"   TO ST-120.
           IF WS-S4 > 3
              SUBTRACT 4  FROM WS-S4
              MOVE "Y"   TO ST-90.
           IF WS-S4 > 1
              SUBTRACT 2  FROM WS-S4
              MOVE "Y"   TO ST-60.
           IF WS-S4 > 0
              SUBTRACT 1  FROM WS-S4
              MOVE "Y"   TO ST-30.

       FA40.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
              GO TO FA75.
            ADD 1   TO W20-WHOLE.
           IF W20-WHOLE < 40
               GO TO FA45.
            MOVE "CARRIED FORWARD"
     TO ST-H5.
            MOVE W25-V1  TO ST-BAL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
              GO TO FA75.
            PERFORM FA15 THRU FA25.

       FA45.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF (WS-F-ERROR = 4) OR
       (CT-CRED NOT = P-NUMBER)
        GO TO FA50.
            GO TO FA35.

       FA50.
            MOVE "CLOSING BALANCE"
     TO ST-H5.
            MOVE P-BAL   TO ST-BAL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
            ADD 2   TO W20-WHOLE.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
              GO TO FA75.
          IF W20-WHOLE > 40
        IF WS-PRINTER = "S"
     MOVE 2  TO WS-ADVANCE
        ELSE
     MOVE 99  TO WS-ADVANCE
        END-IF
        PERFORM CALL-PRINTUTL
              PERFORM FA15 THRU FA25.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
              GO TO FA75.
      PERFORM FA85 THRU FA99.
            SUBTRACT W20-WHOLE FROM 42
     GIVING W20-DECIMAL.
    IF W20-DECIMAL > 2
        IF WS-PRINTER = "S"
     MOVE 2  TO WS-ADVANCE
        ELSE
     MOVE W20-DECIMAL
     TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
              GO TO FA75.
            MOVE SPACES  TO R-DET.
            MOVE 1   TO WS-S.

       FA55.
            MOVE W10-MONTH (WS-S)
     TO ST-H14 (WS-S).
           IF WS-S < 12
              ADD 1   TO WS-S
               GO TO FA55.
            MOVE W12-MONTH  TO WS-S.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE P-OUT   TO ST-VAL (WS-S).

       FA60.
            ADD 1   TO WS-S.
           IF WS-S > 12
              SUBTRACT 12  FROM WS-S.

       FA65.
            MOVE P-30D   TO ST-VAL (WS-S).
             PERFORM FA60.
            MOVE P-60D   TO ST-VAL (WS-S).
             PERFORM FA60.
            MOVE P-90D   TO ST-VAL (WS-S).
             PERFORM FA60.
            MOVE P-120D  TO ST-VAL (WS-S).
             PERFORM FA60.
            MOVE P-150D  TO ST-VAL (WS-S).
             PERFORM FA60.
            MOVE P-180D  TO ST-VAL (WS-S).
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               GO TO FA75.
          IF WS-IND = 1
        MOVE SPACES  TO W15-ACNO
               GO TO FA80.

       FA70.
             GO TO FA10.

       FA75.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
            MOVE 1   TO WS-S.
             GO TO FA999.

       FA80.
      DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
            DISPLAY "Account no:" AT 1420.

      DISPLAY S55.
             ACCEPT S19 AT 1434.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W15-ACNO
       GO TO FA75
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA50-LOOKUP
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA80
        END-EVALUATE
        DISPLAY S19 AT 1434
        IF W15-ACNO = SPACES
           GO TO FA80.
           IF W15-ACNO = SPACES
               GO TO FA75.
             PERFORM AE155-GET-CREDITOR.
           IF NOT(WS-ERROR = ZERO)
        MOVE "Invalid account number" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO FA80.
            DISPLAY P-NAME AT 1439
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA80.
            GO TO FA12.

       FA85.
      INITIALIZE PDT-REC.
      MOVE P-NUMBER  TO PDT-AC.
      PERFORM START-AT-PDCHEQ-AC THRU READ-PDCHEQ-EXIT.
    IF WS-F-ERROR = 44
        GO TO FA99.
      MOVE "POST DATED CHEQUES ISSUED"
     TO ST-NADD.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES TO R-DET.
            ADD 1   TO W20-WHOLE.
          IF W20-WHOLE > 40
        IF WS-PRINTER = "S"
     MOVE 2  TO WS-ADVANCE
        ELSE
     MOVE 99  TO WS-ADVANCE
        END-IF
        PERFORM CALL-PRINTUTL
              PERFORM FA15 THRU FA25.

       FA90.
      PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (PDT-AC NOT = P-NUMBER)
        GO TO FA99.
      MOVE PDT-DATE  TO ST-INVD.
      MOVE PDT-NAR  TO ST-NAR.
      MOVE PDT-REFER  TO ST-INVN.
      MOVE PDT-VAL  TO ST-COST.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
            ADD 1   TO W20-WHOLE.
          IF W20-WHOLE > 40
        IF WS-PRINTER = "S"
     MOVE 2  TO WS-ADVANCE
        ELSE
     MOVE 99  TO WS-ADVANCE
        END-IF
        PERFORM CALL-PRINTUTL
              PERFORM FA15 THRU FA25.
      GO TO FA90.

       FA99.
      EXIT.

       FA999.
             EXIT.

      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W60-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA05.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             PERFORM ERASE-SCREEN.
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
               WHEN 2          MOVE W02-NETWORK TO WS-FILE
                               MOVE WS-NETKEY   TO WS-KEY
               WHEN 3          MOVE W02-CREDIT  TO WS-FILE
                               MOVE P-NUMBER    TO WS-KEYX
               WHEN 4          MOVE W02-CRTRAN  TO WS-FILE
                               MOVE CT-KEY      TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 44         MOVE W02-PDCHEQ  TO WS-FILE
                               MOVE PDT-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
