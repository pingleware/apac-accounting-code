      $set LINKCOUNT"384" GNT"CRP092.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******    ******    *
      *   **    **  **    **  **    **   **  **   **    **  **    **   *
      *   **        **    **  **    **  **    **  **    **       **    *
      *   **        ******    *******   **    **   *******     **      *
      *   **        **   **   **        **    **        **   **        *
      *   **    **  **    **  **         **  **   **    **  **         *
      *    ******   **    **  **          ****     ******   ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   P A T C H   A G I N G   P R O G R A M    *
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
       PROGRAM-ID.      CRP092.
       AUTHOR.          J.W.LEMMON (CSS).
       DATE-WRITTEN.    JUNE, 1983.

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

       COPY "CREDIT.SL".

       COPY "CRTRAN.SL".

       COPY "CRDMEM.SL".

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

       COPY "CREDIT.FDE".

       COPY "CRTRAN.FDE".

       COPY "CRDMEM.FDE".

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
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-FC                   PIC  9(01)           VALUE 0.
       77  WS-SUB                  PIC  9(04)    COMP-5.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-INDS                 PIC  9(01)           VALUE 0.
       77  WS-INDC                 PIC  9(01)           VALUE 0.
       77  WS-SKIP                 PIC  X(01)           VALUE " ".
       77  WS-CHAR1                PIC  X(01).
       77  WS-LIN                  PIC  9(02)           VALUE ZERO.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 66.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-SEASON               PIC  X(01).
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³   (LINKAGE SECTION) for Main screen layout and headings.     ³
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
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15)         VALUE "CRP092".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40)         VALUE "CREDITOR - CHANGE TRANSACTION AGEING".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03) VALUE "CRP".
           03  WS-PROG             PIC  X(03) VALUE "092".

       01  W00-TRAN-CODES.
           03  W00-C-ABR           PIC  X(03).
           03  W00-C-DESC          PIC  X(15).
           03  W00-C-ACT           PIC  9(01).
           03  W00-C-CODE          PIC  9(02)    COMP-5.
           03  W00-C-GL            PIC  9(06)    COMP-3.
           03  W00-C-DAY           PIC  9(06)    COMP-5.
           03  W00-C-VAL           PIC S9(09)V99 COMP-3.
           03  W00-C-MTD           PIC  9(06)    COMP-5.
           03  W00-C-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-C-YTD           PIC  9(06)    COMP-5.
           03  W00-C-YTDV          PIC S9(09)V99 COMP-3.

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       COPY "W05.WS".

       COPY "W15.CRP".

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.
           03  W20-DATE.
               05  W20-CC          PIC  99.
               05  W20-YY          PIC  99.
               05  W20-MM          PIC  99.
               05  W20-DD          PIC  99.
           03  W20-DTE             PIC 9(08)     REDEFINES W20-DATE.

       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W25-OUT             PIC S9(09)V99 COMP-3.
           03  W25-30D             PIC S9(09)V99 COMP-3.
           03  W25-60D             PIC S9(09)V99 COMP-3.
           03  W25-90D             PIC S9(09)V99 COMP-3.
           03  W25-120D            PIC S9(09)V99 COMP-3.
           03  W25-150D            PIC S9(09)V99 COMP-3.
           03  W25-180D            PIC S9(09)V99 COMP-3.
           03  W25-BAL             PIC S9(09)V99 COMP-3.
           03  W25-V1              PIC S9(09)V99 COMP-3.
           03  W25-V2              PIC S9(09)V99 COMP-3.

       01  W25-VALUES-2.
           03  W25-CFLW2           PIC S9(09)V99 COMP-3 OCCURS 6.
           03  W25-V1-2            PIC S9(09)V99 COMP-3.
           03  W25-V2-2            PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-S7V2.
               05  W25-VAL2        PIC Z(06)9.99DB.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-S5V2.
               05  W25-CVAL        PIC  Z(06)9.99-.
           03  W25-DTE.
               05  W25-DATE        PIC  Z9/99/9999.
           03  W25-DEL.
               05  W25-2N          PIC  9(02).
           03  W25-8.
               05  W25-NO          PIC  Z(07)9.

       01  W30-AGE.
           03  W30-CUR             PIC S9(09)V99 COMP-3.
           03  W30-30              PIC S9(09)V99 COMP-3.
           03  W30-60              PIC S9(09)V99 COMP-3.
           03  W30-90              PIC S9(09)V99 COMP-3.
           03  W30-120             PIC S9(09)V99 COMP-3.
           03  W30-150             PIC S9(09)V99 COMP-3.
           03  W30-180             PIC S9(09)V99 COMP-3.

       01  W40-NADD.
           03  W40-ADD.
               05  W40-ACHAR       PIC  X(01) OCCURS 70.
           03  W40-ADDR                       REDEFINES W40-ADD.
               05  W40-ADDRS       PIC  X(60).
               05  FILLER          PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR       PIC  X(01) OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-CNO             PIC  X(06)    OCCURS 18.

       01  W85-PASS.
           03  W85-SUPER           PIC  X(06)    OCCURS 9.
           03  W85-ENTRY           PIC  9(02)    COMP-3.
           03  W85-MARG            PIC S9(02)V99.

       01  W90-TRAN.
           03  W90-CODE            PIC  9(02).
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.

       01  W95-COMPANY.
           03  W95-COMP            PIC  X(40).

       01  W200-MEMO.
           03  W200-MEM-FLDS.
               05  W200-MEM1       PIC  X(60).
               05  W200-MEM2       PIC  X(60).
               05  W200-MEM3       PIC  X(60).
               05  W200-MEM4       PIC  X(60).
               05  W200-MEM5       PIC  X(60).
               05  W200-MEM6       PIC  X(60).
               05  W200-MEM7       PIC  X(60).
               05  W200-MEM8       PIC  X(60).
               05  W200-MEM9       PIC  X(60).
               05  W200-MEM10      PIC  X(60).
               05  W200-MEM11      PIC  X(60).
               05  W200-MEM12      PIC  X(60).
           03  W200-MEM-FOC               REDEFINES W200-MEM-FLDS.
               05  W200-MEM        PIC  X(60) OCCURS 12.

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
           03  PIC X(06) USING W15-ACNO BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       01  S54.
           03  LINE  6 COLUMN  2 VALUE "Ac-No".
           03          COLUMN 15 VALUE              "Name".
           03  LINE  7 COLUMN 15 VALUE              "Contact".
           03          COLUMN 55 VALUE                                                       "Telephone".
           03  LINE  8 COLUMN  2 VALUE "Addr".
           03          COLUMN 39 VALUE                                       "³".
           03          COLUMN 41 VALUE                                         "Op-Bal".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 64 VALUE                                                                "30:".
           03  LINE  9 COLUMN 39 VALUE                                       "³".
           03          COLUMN 41 VALUE                                         "Debits".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 64 VALUE                                                                "60:".
           03  LINE 10 COLUMN 39 VALUE                                       "³".
           03          COLUMN 41 VALUE                                         "Credits".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 64 VALUE                                                                "90:".
           03  LINE 11 COLUMN 39 VALUE                                       "³".
           03          COLUMN 41 VALUE                                         "Cl-Bal".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 63 VALUE                                                               "120:".
           03  LINE 12 COLUMN  2 VALUE "P/C".
           03          COLUMN 39 VALUE                                       "³".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 63 VALUE                                                               "150:".
           03  LINE 13 COLUMN 39 VALUE                                       "³".
           03          COLUMN 45 VALUE                                             "Due".
           03          COLUMN 62 VALUE                                                              "³".
           03          COLUMN 63 VALUE                                                               "180:".
           03  LINE 14 COLUMN  2 VALUE "Terms - :".
           03  LINE 15 COLUMN  2 VALUE "Transactions:".
           03  LINE 16 COLUMN  2 VALUE " Trn Date   Inv/Ref         Type        Cost/Value       Selling   Age 30-180 " BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       01  S54A.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  LINE  6 COLUMN 20 PIC X(40)       USING W15-NAME.
               05  LINE  7 COLUMN 23 PIC X(30)       USING W15-CONTACT.
               05          COLUMN 65 PIC X(14)       USING W15-TEL.
               05  LINE  8 COLUMN  8 PIC X(30)       USING W15-ADDR1.
               05          COLUMN 48 PIC Z(08)9.99DB USING W15-PBAL.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-30D.
               05  LINE  9 COLUMN  8 PIC X(30)       USING W15-ADDR2.
               05          COLUMN 48 PIC Z(08)9.99DB USING W15-DT.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-60D.
               05  LINE 10 COLUMN  8 PIC X(30)       USING W15-ADDR3.
               05          COLUMN 48 PIC Z(08)9.99DB USING W15-CR.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-90D.
               05  LINE 11 COLUMN  8 PIC X(30)       USING W15-ADDR4.
               05          COLUMN 48 PIC Z(08)9.99DB USING W15-BAL.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-120D.
               05  LINE 12 COLUMN  8 PIC 9(04)       USING W15-PC.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-150D.
               05  LINE 13 COLUMN 48 PIC Z(08)9.99DB USING W15-OUT.
               05          COLUMN 67 PIC Z(07)9.99DB USING W15-180D.
               05  LINE 14 COLUMN 14 PIC X(12)       USING W15-PMNT.

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
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    ".
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
           IF LS0-CRLEV < 9
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM KA000.
       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
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
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

       COPY "CRTRAN.RD".

       COPY "CRDMEM.RD".

       COPY "PARAM.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC00-WRITE    SECTION.

       COPY "CONTROL.WR".

       COPY "CREDIT.WR".

       COPY "CRTRAN.WR".

       COPY "PARAM.WR".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

      *
      *    G E T   A   C R E D I T O R   T R A N S A C T I O N
      *    R E C O R D
      *
      *    PERFORM THIS ROUTINE WITH THE TRANSACTION CODE IN W90-CODE
      *
      *    ON EXITING THE TRANSACTION CODE DETAILS ARE IN W00-
      *    WHILE W90-TRNKEY CONTAINS THE KEY TO THE PARAMETER FILE
      *    WHERE THIS TRANSACTION CODE IS FOUND AND W90-TS IS THE
      *    TRANSACTION CODE SUBSCRIPT IN THE RECORD.
      *
       GET-CREDITOR-TRAN-RECORD SECTION.

       GET-CREDITOR-TRAN.
             ADD 381 W90-CODE    GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-TRNKEY
                                 REMAINDER W90-TS.
             ADD 1               TO W90-TS.

       GET-CREDITOR-TRAN-CODE.
             MOVE W90-TRNKEY     TO WS-PARKEY.

       STORE-CREDITOR-TRAN-DESC.
      MOVE PAR-C-ABR(W90-TS)
                                 TO W00-C-ABR.
      MOVE PAR-C-DESC(W90-TS)
                                 TO W00-C-DESC.
      MOVE PAR-C-ACT(W90-TS)
                                 TO W00-C-ACT.
      MOVE PAR-C-CODE(W90-TS)
                                 TO W00-C-CODE.
      MOVE PAR-C-GL(W90-TS)
                                 TO W00-C-GL.

       STORE-CREDITOR-TRAN-STATS.
      MOVE PAR-C-DAY(W90-TS)
                                 TO W00-C-DAY.
      MOVE PAR-C-VAL(W90-TS)
                                 TO W00-C-VAL.
      MOVE PAR-C-MTD(W90-TS)
                                 TO W00-C-MTD.
      MOVE PAR-C-MTDV(W90-TS)
                                 TO W00-C-MTDV.
      MOVE PAR-C-YTD(W90-TS)
                                 TO W00-C-YTD.
      MOVE PAR-C-YTDV(W90-TS)
                                 TO W00-C-YTDV.

       GET-CREDITOR-TRAN-EXIT.
             EXIT.

       GET-CREDITOR-TRAN-DESC SECTION.

       GET-CRED-TRAN-DESC.
             PERFORM GET-CREDITOR-TRAN.
             MOVE W90-TRNKEY     TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-CREDITOR-TRAN-DESC.

       GET-CRED-TRAN-DESC-EXIT.
             EXIT.

      *
      *    ****    Read/Write Creditor record
      *
      *    This routine is performed and the following are the variables
      *    that are required to have been set up.
      *
      *          VARIABLES USED FOR CRPRDWR as instructions and data
      *
      *    03  W15-INSTR             PIC  X(01).
      *
      *        88  ADD-ACCOUNT                     VALUE "A".
      *
      *            W15-CREDREC needs to contain details for the account 
      *                        that must be added
      *
      *        88  DELETE-ACT                      VALUE "D".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account to be deleted
      *
      *        88  GET-ACCOUNT                     VALUE "G".
      *
      *            W15-LINCOL must have the screen location of where the
      *                       User will key in the account number. 
      *                            eg. 0617 = Line 6 Column 17.
      *
      *            W15-BGRND  The Background colour that must be used.
      *            W15-FGRND  The Foreground colour that must be used.
      *                       The folloeing values are available in the
      *                       COPY file "WS.WS" that is included in the
      *                       WORKING-STORAGE SECTION.
      *
      *                           78  Black     value 0.
      *                           78  Blue      value 1.
      *                           78  Green     value 2.
      *                           78  Cyan      value 3.
      *                           78  Red       value 4.
      *                           78  Magenta   value 5.
      *                           78  Brown     value 6.
      *                           78  Grey      value 7.
      *
      *                       If the screen colours for the calling 
      *                       program are Black on White(Grey) then
      *                       that would be setup as follows:
      *
      *                           MOVE Grey  TO W15-BGRND.
      *                           MOVE Black TO W15-FGRND.
      *
      *        88  READ-LOCK                       VALUE "L".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read and locked.
      *
      *        88  READ-RECORD                     VALUE "R".
      *
      *            W15-ACNO must contain the Account Number of the 
      *                     account record to be read.
      *
      *        88  UPDATE-ACT                      VALUE "U".
      *
      *            W15-CREDREC needs to contain the details for the 
      *                        account record to be updated.
      *
      *        88  UPDATE-UNL                      VALUE "X".
      *
      *            W15-CREDREC must contain the the details for the  
      *                        account record to be updated and unlocked.
      *
      *    This variable informs the called program to write details of
      *    the recoosd to the RECOVERY file.
      * 
      *    03  W15-RECOVER.
      *        05  W15-RECV        PIC  X(01)      VALUE "N".
      *            88  ADD-TO-RECV                 VALUE "Y".
      * 
      *    If the record must be written to the RECOVERY file; the 
      *    calling program must MOVE "Y" TO W15-RECV before calling 
      *    CRPRDWR. CRPRDWR will always change it back to "N" before 
      *    returning control to the calling program.
      *
      *    When a read CREDITOR account has been requested WITH W15-RECV
      *    havinge a value of "N" then the program will exit with the
      *    account record in W15-CREDREC. If W15_RECV = "Y" then 
      *    W15-CREDREC remains unchanged.
      *
      *    On exiting W15-ERROR will be 0 if successful or contain and 
      *    error code for unsuccessful requests:
      *               0 = successful
      *               1 = No Record
      *               2 = account number on File not the same as requested
      *               9 = CRPRDWR called with incorrect instruction
      *
              
       COPY "CA155.CRP".

      /
      *    ****    R E A G E   T R A N S A C T I O N
      *
       KA000      SECTION 5.
       KA0.
             DISPLAY "RE-AGE TRANSACTION" AT 0232 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
      MOVE 12   TO SHADE-ROW.
      MOVE 29   TO SHADE-COL.
      MOVE 23   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
            DISPLAY "³ " AT 1227 WITH FOREGROUND-COLOR Grey HIGHLIGHT
           BACKGROUND-COLOR Grey
       "Enter Password        "
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey
       "³" WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1327 WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR Black BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.
            ACCEPT W40-CSHPASS AT 1244
      WITH BACKGROUND-COLOR Magenta
    FOREGROUND-COLOR Cyan HIGHLIGHT AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
           IF NOT(W40-CSHPASS = W85-SUPER (1) OR
     W85-SUPER (3) OR
     "CRTAGE")
        GO TO KA999.

       KA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S54.
             DISPLAY "RE-AGE TRANSACTION" AT 0232
                      WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE SPACES         TO W15-ACNO.
       KA05.
      DISPLAY S55.
            ACCEPT S19 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W15-ACNO
       GO TO KA06
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO KA05
        END-EVALUATE
              DISPLAY S19 AT 0308
              IF W15-ACNO = SPACES
           GO TO KA05.

       KA06.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO KA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM CA155-GET-CREDITOR.
           IF NOT(WS-ERROR = ZERO)
              MOVE WS-ER8  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO KA05.
      PERFORM KA95.
             MOVE P-ADDR         TO W40-ADD.
             UNSTRING W40-ADD DELIMITED BY "*" INTO
               W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4
              OVERFLOW MOVE 1  TO WS-ERROR.
             DISPLAY S54A.
           IF P-TAX = "I"
        DISPLAY "(Inclusive)" AT 1162 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
           IF P-TAX = "E"
        DISPLAY "(Exclusive)" AT 1162 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
        DISPLAY "(No VAT)" AT 1162 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.

      MOVE P-NUMBER  TO CT-CRED.
      MOVE ZERO   TO CT-SDTE
        CT-CODE
        CT-SEQ.
      MOVE SPACES  TO CT-SINV.
      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
    IF WS-F-ERROR = 4
        GO TO KA90.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF (WS-F-ERROR = 4) OR
       (CT-CRED NOT = P-NUMBER)
        GO TO KA90.

       KA10.
             MOVE 1401           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
             MOVE 14             TO LIN.

       KA15.
            MOVE CT-DTE  TO W20-DTE.
      MOVE W20-CC  TO W15-CC.
      MOVE W20-YY  TO W15-YY.
      MOVE W20-MM  TO W15-MM.
      MOVE W20-DD  TO W15-DD.
            DISPLAY W15-DD AT LINE LIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
       W15-MM WITH FOREGROUND-COLOR Grey
     BACKGROUND-COLOR Magenta
       W15-CCYY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
          IF CT-REF NUMERIC
              MOVE CT-REFNO  TO W25-NO
    ELSE
        MOVE CT-REF  TO W25-8.
            DISPLAY W25-8 AT LINE LIN COLUMN 11
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-DESC.
      DISPLAY W00-C-ABR AT LINE LIN COLUMN 20
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
          IF CT-SINV NUMERIC
              MOVE CT-SINN  TO W25-NO
    ELSE
        MOVE CT-SINV  TO W25-8.
            DISPLAY W25-8 AT LINE LIN COLUMN 24
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF NOT(CT-SDTE = ZERO)
              MOVE CT-SDTE  TO W20-DTE
        MOVE W20-CC  TO W15-CC
        MOVE W20-YY  TO W15-YY
        MOVE W20-MM  TO W15-MM
        MOVE W20-DD  TO W15-DD
              DISPLAY W15-DD AT LINE LIN COLUMN 33
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
         W15-MM WITH FOREGROUND-COLOR Grey
       BACKGROUND-COLOR Magenta
         W15-CCYY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
         BACKGROUND-COLOR Magenta.
             MOVE CT-COST        TO W25-VAL2.
            DISPLAY W25-S7V2 AT LINE LIN COLUMN 43
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
           IF NOT(CT-SELL = ZERO)
               MOVE CT-SELL      TO W25-VAL2
              DISPLAY W25-S7V2 AT LINE LIN COLUMN 2
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
           IF CT-AGE = ZERO
               GO TO KA18.
             MOVE ZERO           TO W15-SETT.
             MOVE CT-AGE         TO WS-S4.
           IF WS-S4 > 31
               MOVE 1            TO W15-180
               SUBTRACT 32       FROM WS-S4.
           IF WS-S4 > 15
               MOVE 1            TO W15-150
               SUBTRACT 16       FROM WS-S4.
           IF WS-S4 > 7
               MOVE 1            TO W15-120
               SUBTRACT 8        FROM WS-S4.
           IF WS-S4 > 3
               MOVE 1            TO W15-90
               SUBTRACT 4        FROM WS-S4.
           IF WS-S4 > 1
               MOVE 1            TO W15-60
               SUBTRACT 2        FROM WS-S4.
           IF WS-S4 > 0
               MOVE 1            TO W15-30.

       KA18.
            DISPLAY W15-30 AT LINE LIN COLUMN 69
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-60 AT LINE LIN COLUMN 71
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-90 AT LINE LIN COLUMN 73
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-120 AT LINE LIN COLUMN 75
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-150 AT LINE LIN COLUMN 77
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-180 AT LINE LIN COLUMN 79
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       KA20.
      DISPLAY "A" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "mend / "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ext or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue HIGHLIGHT.
             MOVE "N"            TO WS-OPTION.

       KA25.
      ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO KA00
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO KA25
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(WS-OPTION = "A" OR "N")
               GO TO KA25.
      PERFORM CLEAR-L50.
           IF WS-OPTION = "A"
               GO TO KA30.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
    IF (WS-F-ERROR = 4) OR
       (CT-CRED NOT = P-NUMBER)
              DISPLAY "**** End **** - ENTER" AT 5012
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              ACCEPT WS-OPTION AT 5034
        WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               GO TO KA00.
      DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR Cyan.
             GO TO KA15.

       KA30.
             MOVE CT-COST        TO W20-RESULT.
             MOVE 0              TO WS-S.
             MOVE 1              TO WS-S2.

       KA45.
             MOVE ZERO           TO W25-CFLOW (WS-S2).
           IF W15-MONTH (WS-S2) > 0
               ADD 1 TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO KA45.
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1              TO WS-S3.
           IF WS-S = 0
               MOVE W20-RESULT   TO W25-V2
               GO TO KA55.
           IF WS-S = 1
               MOVE W20-RESULT   TO W25-V2
               GO TO KA50.
             DIVIDE W20-RESULT BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1               TO WS-S.

       KA50.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO KA50.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO KA50.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V2.

       KA55.
             EXIT.

       KA58.
             MOVE W25-VALUES     TO W25-VALUES-2.
           IF CT-COST < ZERO
               GO TO KA65.
             MOVE 1              TO WS-S2.

       KA60.
             MULTIPLY -1         BY W25-CFLW2 (WS-S2).
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO KA60.
             MULTIPLY -1         BY W25-V2-2.

       KA65.
            ACCEPT W15-30 AT 1469
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-60 AT 1471
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-90 AT 1473
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
             ACCEPT W15-120 AT 1475
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
             ACCEPT W15-150 AT 1477
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
             ACCEPT W15-180 AT 1479
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO KA65.
      PERFORM KA30 THRU KA55.
           IF CT-COST > ZERO
               GO TO KA75.
             MOVE 1              TO WS-S2.

       KA70.
             MULTIPLY -1         BY W25-CFLOW (WS-S2).
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO KA70.
             MULTIPLY -1         BY W25-V2.

       KA75.
             PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE 1              TO WS-S2.
             ADD W25-CFLOW (6)   TO P-180D.
             ADD W25-CFLW2 (6)   TO P-180D.
             ADD W25-CFLOW (5)   TO P-150D.
             ADD W25-CFLW2 (5)   TO P-150D.
             ADD W25-CFLOW (4)   TO P-120D.
             ADD W25-CFLW2 (4)   TO P-120D.
             ADD W25-CFLOW (3)   TO P-90D.
             ADD W25-CFLW2 (3)   TO P-90D.
             ADD W25-CFLOW (2)   TO P-60D.
             ADD W25-CFLW2 (2)   TO P-60D.
             ADD W25-CFLOW (1)   TO P-30D.
             ADD W25-CFLW2 (1)   TO P-30D.
             ADD W25-V2          TO P-OUT.
             ADD W25-V2-2        TO P-OUT.
             MOVE 1              TO WS-S WS-S2.
             MOVE ZERO           TO CT-AGE.
           IF NOT(W25-V2 = ZERO)
               GO TO KA85.

       KA80.
           IF NOT(W15-MONTH(WS-S) = ZERO)
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               ADD 1             TO WS-S
               MULTIPLY 2        BY WS-S2
               GO TO KA80.

       KA85.
             PERFORM REWRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
             PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             DISPLAY S54A.
      PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT.
      PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
             GO TO KA15.

       KA90.
      MOVE "No transactions on file" TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
            GO TO KA00.

       KA95.
             COMPUTE W30-CUR = P-BAL - (P-30D + P-120D +
                                        P-60D + P-150D +
                                        P-90D + P-180D).
           IF (P-OUT NOT = W30-CUR) OR
              (P-OUT < ZERO)
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT
               MOVE W30-CUR      TO P-OUT
               IF P-OUT < ZERO
                   ADD P-OUT     TO P-30D
                   MOVE ZERO     TO P-OUT
               END-IF
               IF P-30D < ZERO
                   ADD P-30D     TO P-60D
                   MOVE ZERO     TO P-30D
               END-IF
               IF P-60D < ZERO
                   ADD P-60D     TO P-90D
                   MOVE ZERO     TO P-60D
               END-IF
               IF P-90D < ZERO
                   ADD P-90D     TO P-120D
                   MOVE ZERO     TO P-90D
               END-IF
               IF P-120D < ZERO
                   ADD P-120D    TO P-150D
                   MOVE ZERO     TO P-120D
               END-IF
               IF P-150D < ZERO
                   ADD P-150D    TO P-180D
                   MOVE ZERO     TO P-150D
               END-IF
               IF P-180D < ZERO
                   ADD P-180D    TO P-OUT
                   MOVE ZERO     TO P-180D
               END-IF
               PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.

       KA999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-DATE.
             MOVE LS-TODAY-YYMMDD    TO W05-DATE-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

      MOVE 2   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
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
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
