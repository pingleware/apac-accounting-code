      $set LINKCOUNT"512" GNT"CRPENQ.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******   ********  **    **   ******    *
      *   **    **  **    **  **    **  **        ***   **  **    **   *
      *   **        **    **  **    **  **        ****  **  **    **   *
      *   **        ******    *******   ****      ** ** **  **    **   *
      *   **        **   **   **        **        **  ****  **   ***   *
      *   **    **  **    **  **        **        **   ***   ******    *
      *    ******   **    **  **        ********  **    **        **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   M A I N T E N A N C E   P R O G R A M    *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.         CRPENQ.
       AUTHOR.             J.W.LEMMON (CSS).
       DATE-WRITTEN.       JUNE, 1983.

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

      *
      *    ****    L O C K   M A N U A L
      *
       COPY "CRTRAN.SLM".

       COPY "PARAM.SL".

       COPY "PDCHEQ.SL".

       COPY "PORDER.SL".

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

       COPY "CREDIT.FDE".

       COPY "CRTRAN.FDE".

       COPY "PARAM.FDE".

       COPY "PDCHEQ.FDE".

       COPY "PORDER.FDE".

       COPY "STOCK.FDE".

      *
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)           VALUE "aeWlimemnomLalismJ".
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CREKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
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
       77  WS-ENQPOS               PIC  9(06)    COMP-5.
       77  WS-ENQEND               PIC  9(06)    COMP-5.
       77  WS-ENT                  PIC  X(08).
       77  WS-ENT1                 PIC  X(08).
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-DTE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CHAR1                PIC  X(01).
       77  WS-USE-ITM              PIC  X(01).
       77  WS-EXT-STK              PIC  X(01).
       77  WS-SEASON               PIC  X(01).
       77  WS-ER8                  PIC  X(09)           VALUE "No record".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  TODAY-YYMMDD            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
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
           03  CRT-START           PIC  9(02)    VALUE 5.
           03  CRT-END             PIC  9(02)    VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15)    VALUE "CRPENQ".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)    VALUE "ACCOUNTS PAYABLE/CREDITOR - A/C ENQUIRY".
           03  CRT-HEAD-RED                      REDEFINES CRT-HEADING.
               05  FILLER          PIC  X(28).
               05  CRT-E-TYP       PIC  X(03).
               05  FILLER          PIC  X(09).
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
           03  WS-MODULE           PIC  X(03)    VALUE "CRP".
           03  WS-PROG             PIC  X(03)    VALUE "ENQ".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

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

       COPY "W05.WS".

       01  W10-MONTHS.
           03  W10-MONTH-NAMES     PIC  X(36)    VALUE  "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W10-MNTH-NAME                     REDEFINES W10-MONTH-NAMES.
               05  W10-MONTH       PIC  X(03)    OCCURS 12.

       COPY "W15.CRP".

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)    COMP-3.
           03  W21-DATE.
               05  W21-CC          PIC  99.
               05  W21-YY          PIC  99.
               05  W21-MM          PIC  99.
               05  W21-DD          PIC  99.
           03  W21-DTE             PIC  9(08)    REDEFINES W21-DATE.
           03  W22-DTE             PIC  9(08).
           03  W22-TODAY                         REDEFINES W22-DTE.
               05  W22-DD          PIC  99.
               05  W22-MM          PIC  99.
               05  W22-CC          PIC  99.
               05  W22-YY          PIC  99.

       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.
           03  W25-V1              PIC S9(07)V99 COMP-3.
           03  W25-V2              PIC S9(07)V99 COMP-3.
       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-S7V2.
               05  W25-VAL         PIC  Z(07)9.99-.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-5V2.
               05  W25-7           PIC  Z(05)9.99.
           03  W25-S5V2.
               05  W25-CVAL        PIC  Z(06)9.99-.
           03  W25-DTE.
               05  W25-DATE        PIC  Z9/99/9999.
           03  W25-DEL.
               05  W25-2N          PIC  9(02).
           03  W25-6.
               05  W25-NO          PIC  Z(07)9.

       01  W30-KEY.
           03  W30-CRED            PIC  X(06).
           03  W30-SDTE            PIC  9(08)    COMP.
           03  W30-CODE            PIC  9(02).
           03  W30-SINV.
               05  W30-SINN        PIC  9(08).
           03  W30-SEQ             PIC  9(02)    COMP-X.

       01  W35-DATES.
           03  W35-DOC-DATE        PIC  9(08)    COMP-0 OCCURS 30.
           03  W35-TRN-DATE        PIC  9(08)    COMP-0 OCCURS 30.

       01  W40-NADD.
           03  W40-ADD.
               05  W40-ACHAR       PIC  X(01)    OCCURS 70.
           03  W40-ADDR                          REDEFINES W40-ADD.
               05  W40-ADDRS       PIC  X(60).
               05  FILLER          PIC  X(10).
           03  W40-NAME.
               05  W40-NCHAR       PIC  X(01)    OCCURS 40.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-CNO             PIC  X(06)    OCCURS 18.
           03  W75-POR             PIC  9(10)    OCCURS 35.

       01  W90-TRAN.
           03  W90-CODE            PIC  9(02).
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.

       01  W95-COMPANY.
           03  W95-COMP            PIC  X(40).

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

       01  S09.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta PIC X(06) USING W15-ACNO AUTO.

       01  S14.
           03  LINE  6 COLUMN  2 VALUE "Ac-No".
           03          COLUMN 15 VALUE              "Name".
           03  LINE  7 COLUMN 15 VALUE              "Contact".
           03          COLUMN 55 VALUE                                                       "Telephone".
           03  LINE  8 COLUMN  2 VALUE "Addr".
           03          COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 41 VALUE                                         "Op-Bal".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 64 VALUE                                                                "30:".
           03  LINE  9 COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 41 VALUE                                         "Debits".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 64 VALUE                                                                "60:".
           03  LINE 10 COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 41 VALUE                                         "Credits".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 64 VALUE                                                                "90:".
           03  LINE 11 COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 41 VALUE                                         "Cl-Bal".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 63 VALUE                                                               "120:".
           03  LINE 12 COLUMN  2 VALUE "P/C".
           03          COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 63 VALUE                                                               "150:".
           03  LINE 13 COLUMN 39 VALUE                                       "Ё".
           03          COLUMN 45 VALUE                                             "Due".
           03          COLUMN 62 VALUE                                                              "Ё".
           03          COLUMN 63 VALUE                                                               "180:".
           03  LINE 14 COLUMN  2 VALUE "Terms - :".
           03  LINE 15 COLUMN  2 VALUE "Transactions:".
           03  LINE 16 COLUMN  2 VALUE " Trn Date   Inv/Ref         Type        Cost/Value       Selling   Age 30-180 " BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       01  S14A.
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

       01  S15.
           03  LINE  6 COLUMN  2 VALUE "Account No:".
           03          COLUMN 26 VALUE                         "Name:".
           03  LINE  8 COLUMN  2 VALUE "   Date    Quant Unit-Cost Total-Cost Item code          Del   Qty   MTD M/AVG" BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       01  S16.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE 13 COLUMN 21 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
               05  LINE 14 COLUMN 21 VALUE "Ё               POST DATED CHEQUES ISSUED                Ё".
               05  LINE 15 COLUMN 21 VALUE "Ё   Date    Narrative                             Value  Ё".
               05  LINE 16 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 17 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 18 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 19 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 20 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 21 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 22 COLUMN 21 VALUE "Ё                                                        Ё".
               05  LINE 23 COLUMN 21 VALUE "юдддддддддддддддддддддддддддддддддддддддддддддддддддддддды".

       01  S16A.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Blue.
               05  COLUMN 22 PIC Z9/99/9999   FROM W15-DEL.
               05  COLUMN 33 PIC  X(30)       FROM PDT-NAR.
               05  COLUMN 64 PIC  Z(08)9.99DB FROM PDT-VAL.

       01  S16B.
           03  PIC  X(08) FROM CT-REF BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           
       01  S16C.
           03  PIC  X(08) FROM CT-SINV BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.

       01  S10.
           03  BACKGROUND-COLOR Green FOREGROUND-COLOR Brown HIGHLIGHT.
               05  LINE 12 COLUMN  5 VALUE "здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
               05  LINE 13 COLUMN  5 VALUE "Ё Street Address                                                   Ё".
               05  LINE 14 COLUMN  5 VALUE "Ё                                       Telephone 2                Ё".
               05  LINE 15 COLUMN  5 VALUE "Ё                                       Cell Phone                 Ё".
               05  LINE 16 COLUMN  5 VALUE "Ё                                       Fax                        Ё".
               05  LINE 17 COLUMN  5 VALUE "Ё                                                                  Ё".
               05  LINE 18 COLUMN  5 VALUE "Ё                                                                  Ё".
               05  LINE 19 COLUMN  5 VALUE "Ё eMail                                                            Ё".
               05  LINE 20 COLUMN  5 VALUE "Ё                                                                  Ё".
               05  LINE 21 COLUMN  5 VALUE "юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды".
               05  LINE 14 COLUMN  7          PIC  X(30) USING W15-SADDR1    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 57          PIC  X(14) USING W15-TEL2      FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 15 COLUMN  7          PIC  X(30) USING W15-SADDR2    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 57          PIC  X(14) USING W15-CELL      FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 16 COLUMN  7          PIC  X(30) USING W15-SADDR3    FOREGROUND-COLOR Grey HIGHLIGHT.
               05          COLUMN 57          PIC  X(14) USING W15-FAX       FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 17 COLUMN  7          PIC  X(30) USING W15-SADDR4    FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 18 COLUMN  7          PIC  X(08) USING W15-SPCODE    FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 20 COLUMN  7          PIC  X(64) USING W15-EMAIL     FOREGROUND-COLOR Grey HIGHLIGHT.

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

      *COPY "CRDLUP.CRT".

       01  S-POR.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  9 COLUMN 60 VALUE "здддддддддддддд©".
               05  LINE 10 COLUMN 60 VALUE "Ё Order Number Ё".
               05  LINE 11 COLUMN 60 VALUE "цдддддддддддддд╢".
               05  LINE 12 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 13 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 14 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 15 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 16 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 17 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 18 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 19 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 20 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 21 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 22 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 23 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 24 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 25 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 26 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 27 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 28 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 29 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 30 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 31 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 32 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 33 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 34 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 35 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 36 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 37 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 38 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 39 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 40 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 41 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 42 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 43 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 44 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 45 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 46 COLUMN 60 VALUE "Ё              Ё".
               05  LINE 47 COLUMN 60 VALUE "юдддддддддддддды".

       01  SD-POR.
           03  BACKGROUND-COLOR Grey.
               05  COLUMN 63  PIC Z(07)9 FROM W15-OPREF FOREGROUND-COLOR Magenta.
               05  COLUMN 71  VALUE "/"                 FOREGROUND-COLOR Black.
               05  COLUMN 72  PIC 99     FROM W15-OSUB  FOREGROUND-COLOR Magenta.

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
             PERFORM ZA000-INIT.
             PERFORM GA000.

       AA49.
             EXIT PROGRAM.

       COPY "AA50C.LUP".
       
       COPY "AA900.ALM".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      CREDITOR-MEMO                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё The account number of the Creditor record for which the   Ё
      *    Ё MEMO details are to be displayed must be in W15-ACNO.     Ё
      *    Ё PERFORM CREDITOR-MEMO.                                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CRDMEM.PRO".

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
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.SCR".

      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё                  ERROR-MESSAGE (2 LINES)                  Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё See above for details of the first line and the second    Ё
      *    Ё line of the message must be in WS-ERR-MES-2.              Ё
      *    Ё               PERFORM ERROR-MESSAGE-2.                    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "ERROR.SCR".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      OPT-MESSAGE                          Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'.                                Ё
      *    Ё                                                           Ё
      *    Ё The option request must be placed in WS-OPT-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё                                                           Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.                                    Ё
      *    Ё                                                           Ё
      *    Ё This would be displayed as:                               Ё
      *    Ё      здддддддддддддддддддддддддддддддддддддддддддддддд©   Ё
      *    Ё      Ё          Print transactions (Y/N) [ ]          Ё   Ё
      *    Ё      юдддддддддддддддддддддддддддддддддддддддддддддддды   Ё
      *    Ё                                                           Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё                                                           Ё
      *    Ё The system will display the message box with the top line Ё
      *    Ё at line as the value of WS-MES-LINE. If WS-MES-LINE is    Ё
      *    Ё zero the default will be 20.                              Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

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
             ADD 381 W90-CODE        GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-TRNKEY REMAINDER W90-TS.
             ADD 1                   TO W90-TS.

       GET-CREDITOR-TRAN-CODE.
             MOVE W90-TRNKEY         TO WS-PARKEY.

       STORE-CREDITOR-TRAN-DESC.
             MOVE PAR-C-ABR(W90-TS)  TO W00-C-ABR.
             MOVE PAR-C-DESC(W90-TS) TO W00-C-DESC.
             MOVE PAR-C-ACT(W90-TS)  TO W00-C-ACT.
             MOVE PAR-C-CODE(W90-TS) TO W00-C-CODE.
             MOVE PAR-C-GL(W90-TS)   TO W00-C-GL.

       STORE-CREDITOR-TRAN-STATS.
             MOVE PAR-C-DAY(W90-TS)  TO W00-C-DAY.
             MOVE PAR-C-VAL(W90-TS)  TO W00-C-VAL.
             MOVE PAR-C-MTD(W90-TS)  TO W00-C-MTD.
             MOVE PAR-C-MTDV(W90-TS) TO W00-C-MTDV.
             MOVE PAR-C-YTD(W90-TS)  TO W00-C-YTD.
             MOVE PAR-C-YTDV(W90-TS) TO W00-C-YTDV.

       GET-CREDITOR-TRAN-EXIT.
             EXIT.

       GET-CREDITOR-TRAN-DESC SECTION.

       GET-CRED-TRAN-DESC.
             PERFORM GET-CREDITOR-TRAN.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-CREDITOR-TRAN-DESC.

       GET-CRED-TRAN-DESC-EXIT.
             EXIT.

      /
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ      SECTION.

       COPY "CONTROL.RD".

       COPY "CREDIT.RD".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Special start for displaying transactions descending Seq. Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       START-AT-CTRN-KEY-DESC.
             START CTRANS KEY < CT-KEY.
           IF WS-STATUS = "00"
               MOVE ZERO             TO WS-F-ERROR
               GO TO READ-CRTRAN-EXIT.
           IF WS-STATUS = "23"
               MOVE 4                TO WS-F-ERROR
               GO TO READ-CRTRAN-EXIT.
           IF WS-STAT1 = "2" OR "3" OR "4"
               MOVE 4                TO WS-F-ERROR
               PERFORM READ-ERROR.
             GO TO READ-CRTRAN-EXIT.

       COPY "CRTRAN.RD".

       COPY "PARAM.RD".

       COPY "PDCHEQ.RD".

       COPY "PORDER.RD".

       COPY "STOCK.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC00-WRITE    SECTION.

       COPY "CONTROL.WR".

       COPY "PORDER.WR".

       COPY "STOCK.WR".

      *COPY "CREDIT.LUP".

       COPY "APAC.HLP".

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
      *    ****   C R E D I T O R   E N Q U I R I E S
      *
       GA000      SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             MOVE "T"                TO WS-INSTR.
             MOVE "A"                TO WS-OPTION.
             MOVE "'A'ccount,'O'rder Enquiry,'E'xit or 'Esc' [ ]" TO WS-OPT-MES.  
             PERFORM OPT-MESSAGE.
             EVALUATE WS-OPTION
      *       E X I T   P R O G R A M
               WHEN X"EC"
               WHEN "E"    GO TO GA999
      *       A C C O U N T   E N Q U I R Y
               WHEN "A"    PERFORM GB000
      *       O R D E R   E N Q U I R Y
               WHEN "O"    PERFORM GC000
             END-EVALUATE.
             GO TO GA00.
             
       GA999.
             EXIT.
      /
      *    ****    A C C O U N T   E N Q U I R Y
      *
       GB000      SECTION 5.
       GB00.
             MOVE "A/C"              TO CRT-E-TYP.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY S14.
             MOVE SPACES             TO W15-ACNO.

       GB05.
             DISPLAY S55.
             ACCEPT S09 AT 0608.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE SPACES TO W15-ACNO
                               GO TO GB99
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA50
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO GB05
               END-EVALUATE
               DISPLAY S09 AT 0608
               IF W15-ACNO = SPACES
                   GO TO GB05.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO GB99.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
               MOVE WS-ER8           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB05.
             DISPLAY S14A.
           IF P-TAX = "I"
               DISPLAY "(Inclusive)" AT 1462 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta
           ELSE
           IF P-TAX = "E"
               DISPLAY "(Exclusive)" AT 1462 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta
           ELSE
               DISPLAY "(No VAT)"    AT 1462 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Magenta.
             MOVE ZERO               TO WS-ENQEND WS-ENQPOS.

       GB10.
             MOVE "Ascending/Descending Tran Date seq (A/D)  [A]" TO WS-OPT-MES.
             MOVE "A"                TO WS-OPTION.
             MOVE "J"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
      *
      *    ****  Was the Escape key pressed?
      *
           IF WS-OPTION = X"EC"
               GO TO GB00.
             MOVE WS-OPTION          TO WS-SEQ.
             MOVE 17                 TO CRT-START LIN.
             PERFORM ERASE-SCREEN.
             MOVE P-NUMBER           TO CT-CRED.
           IF WS-SEQ = "A"
               MOVE ZERO             TO CT-SDTE CT-CODE CT-SEQ
               MOVE SPACES           TO CT-SINV
               PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT
           ELSE
               MOVE TODAY-YYMMDD     TO CT-SDTE
               MOVE 99               TO CT-CODE
               CT-SEQ
               MOVE "ZZZZZZZZ"       TO CT-SINV
               PERFORM START-AT-CTRN-KEY-DESC THRU READ-CRTRAN-EXIT.
           IF WS-F-ERROR = 4
               GO TO GB25.
             PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 30.
             MOVE 0                  TO WS-S7.

       GB15.
           IF WS-SEQ = "A"
               PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT
           ELSE
               PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT.
           IF (WS-F-ERROR = 4) OR (CT-CRED NOT = P-NUMBER)
               MOVE WS-ENQPOS        TO WS-ENQEND
               MOVE W30-KEY          TO CT-KEY
               PERFORM START-AT-CTRN-KEY THRU READ-CRTRAN-EXIT
               GO TO GB25.
             MOVE CT-KEY             TO W30-KEY.
             COMMIT.
             ADD 1                   TO WS-ENQPOS WS-S7.
      *
      *    ****    S W I T C H   D A T E   T O   D D M M C C Y Y
      *
       GB18.
             MOVE CT-SDTE            TO W21-DTE.
             MOVE W21-DD             TO W15-DD.
             MOVE W21-MM             TO W15-MM.
             MOVE W21-CC             TO W15-CC.
             MOVE W21-YY             TO W15-YY.
             MOVE W15-DTE            TO W35-TRN-DATE(WS-S7).
             MOVE CT-DTE             TO W21-DTE.
             MOVE W21-DD             TO W15-DD.
             MOVE W21-MM             TO W15-MM.
             MOVE W21-CC             TO W15-CC.
             MOVE W21-YY             TO W15-YY.
             MOVE W15-DTE            TO W35-DOC-DATE(WS-S7).
           IF WS-DTE = "T"
               MOVE W35-TRN-DATE(WS-S7) TO W25-DATE
               DISPLAY W25-DTE AT LINE LIN COLUMN 2 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               MOVE W35-DOC-DATE(WS-S7) TO W25-DATE
               DISPLAY W25-DTE AT LINE LIN COLUMN 2 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
           IF CT-CODE = 01 OR 02
               DISPLAY S16B AT LINE LIN COLUMN 14
           ELSE
               DISPLAY S16C AT LINE LIN COLUMN 14.
             MOVE CT-CODE            TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-DESC.
             DISPLAY W00-C-DESC AT LINE LIN COLUMN 24 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE CT-COST            TO W25-VAL.
             DISPLAY W25-S7V2   AT LINE LIN COLUMN 41 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF CT-SELL NOT = ZERO
               MOVE CT-SELL          TO W25-VAL
               DISPLAY W25-S7V2 AT LINE LIN COLUMN 55 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF CT-AGE = ZERO
               GO TO GB20.
             MOVE ZERO               TO W15-SETT.
             MOVE CT-AGE             TO WS-S4.
           IF WS-S4 > 31
               MOVE 1                TO W15-180
               SUBTRACT 32           FROM WS-S4.
           IF WS-S4 > 15
               MOVE 1                TO W15-150
               SUBTRACT 16           FROM WS-S4.
           IF WS-S4 > 7
               MOVE 1                TO W15-120
               SUBTRACT 8            FROM WS-S4.
           IF WS-S4 > 3
               MOVE 1                TO W15-90
               SUBTRACT 4            FROM WS-S4.
           IF WS-S4 > 1
               MOVE 1                TO W15-60
               SUBTRACT 2            FROM WS-S4.
           IF WS-S4 > 0
               MOVE 1                TO W15-30.
             DISPLAY W15-30  AT LINE LIN COLUMN 69 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W15-60  AT LINE LIN COLUMN 71 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W15-90  AT LINE LIN COLUMN 73 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W15-120 AT LINE LIN COLUMN 75 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W15-150 AT LINE LIN COLUMN 77 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W15-180 AT LINE LIN COLUMN 79 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
       GB20.
           IF LIN < 46
               ADD 1                 TO LIN
               GO TO GB15.

       GB25.
            DISPLAY "PgDn"                                                               AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        ","                                                                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                         "PgUp"                                                                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                             ","                                                                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                              "Esc"                                                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                 ","                                                             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                  "F5"                                                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                    " Screen 2,"                                                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                             "F6"                                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                               " Trn Dates,"                                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                          "F7"                                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                            " PD Chqs,"                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                     "F8"                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                       " Doc Dates,"             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                                  "F9"           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                    " Memo/Rmks" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       GB30.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO GB00
                 WHEN F5-KEY     PERFORM GB35
                 WHEN F6-KEY     PERFORM GB80 THRU GB85
                 WHEN F7-KEY     PERFORM GB100
                 WHEN F8-KEY     PERFORM CREDITOR-MEMO
                 WHEN F9-KEY     MOVE DEB-ACNO TO W15-ACNO
                                 PERFORM CREDITOR-MEMO
                 WHEN PAGE-UP    IF NOT(WS-ENQPOS = 0 OR 1 OR 30)
                                     GO TO GB50
                                 END-IF
                 WHEN PAGE-DOWN  GO TO GB45
                 WHEN OTHER      PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GB25.
             PERFORM AA900-ALARM.
             GO TO GB30.

       GB35.
             PERFORM SAVE-SCREEN.
             MOVE 12                 TO SHADE-ROW.
             MOVE 5                  TO SHADE-COL.
             MOVE 68                 TO SHADE-WIDTH.
             MOVE 10                 TO SHADE-LINES.
             DISPLAY S10.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             DISPLAY "ENTER to continue" AT 5024 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN.
 
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                     P A G E - D O W N                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       GB45.
           IF WS-ENQPOS = WS-ENQEND
               GO TO GB30.
             MOVE 17                 TO CRT-START LIN.
             PERFORM ERASE-SCREEN.
             PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 30.
             MOVE 0                  TO WS-S7.
             GO TO GB15.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      P A G E - U P         Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       GB50.
             MOVE 1                  TO WS-IXS1.

       GB55.
           IF WS-SEQ = "A"
               PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT
           ELSE
               PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT.
           IF WS-F-ERROR = 4
               GO TO GB60.
           IF CT-CRED NOT = P-NUMBER
               IF WS-SEQ = "A"
                   PERFORM READ-CRTRAN-NEXT THRU READ-CRTRAN-EXIT
               ELSE
                   PERFORM READ-CRTRAN-PREV THRU READ-CRTRAN-EXIT
               END-IF
               GO TO GB60.
             SUBTRACT 1              FROM WS-ENQPOS.
             MOVE CT-KEY             TO W30-KEY.
                    ADD 1            TO WS-IXS1.
           IF WS-IXS1 < 20
               IF WS-ENQPOS > 0
                   GO TO GB55.

       GB60.
             MOVE 17                 TO CRT-START LIN.
             PERFORM ERASE-SCREEN.
             ADD 1                   TO WS-ENQPOS.
             PERFORM GB65 VARYING WS-S7 FROM 1 BY 1 UNTIL WS-S7 = 30.
             MOVE 1                  TO WS-S7.
             GO TO GB18.

       GB65.
             MOVE ZERO               TO W35-DOC-DATE(WS-S7).

       GB70.
             MOVE 1                  TO WS-S6.
             MOVE 17                 TO LIN.
             MOVE "D"                TO WS-DTE.
             DISPLAY "Doc" AT 1303 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT.

       GB75.
           IF NOT(WS-S6 > WS-S7)
               MOVE W35-DOC-DATE(WS-S6) TO W25-DATE
               DISPLAY W25-DTE AT LINE LIN COLUMN 2 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta
               ADD 1                 TO WS-S6 LIN
               GO TO GB75.

       GB80.
             MOVE 1                  TO WS-S6.
             MOVE 17                 TO LIN.
             MOVE "T"                TO WS-DTE.
             DISPLAY "Trn" AT 1303 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       GB85.
           IF NOT(WS-S6 > WS-S7)
               MOVE W35-TRN-DATE(WS-S6) TO W25-DATE
               DISPLAY W25-DTE AT LINE LIN COLUMN 2 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
               ADD 1                 TO WS-S6 LIN
               GO TO GB85.

       GB99.
             EXIT.
      *
      *    ****    P D  C H E Q U E S
      *
       GB100        SECTION 5.
       GB105.
             PERFORM SAVE-SCREEN.
             MOVE 14                 TO SHADE-ROW.
             MOVE 21                 TO SHADE-COL.
             MOVE 58                 TO SHADE-WIDTH.
             MOVE 11                 TO SHADE-LINES.
             DISPLAY S16.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             INITIALIZE PDT-REC.
             MOVE W15-ACNO           TO PDT-AC.
             PERFORM START-AT-PDCHEQ-AC THRU READ-PDCHEQ-EXIT.
           IF WS-F-ERROR = 44
      *
      *    ****    N O   P D   C H E Q U E S   F O R   C R E D I T O R
      *
               GO TO GB125.

       GB110.
             MOVE 16                 TO CLIN.

       GB115.
             PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
           IF (WS-F-ERROR = 44) OR (PDT-AC NOT = W15-ACNO)
      *
      *    ****    N O   M O R E   P D   C H E Q U E S   F O R
      *                      C R E D I T O R
      *
               GO TO GB125.
      *
      *    ****    S W I T C H   D A T E   T O   D D M M C C Y Y
      *
             MOVE PDT-DATE           TO W21-DTE.
             MOVE W21-CC             TO W15-CC.
             MOVE W21-YY             TO W15-YY.
             MOVE W21-MM             TO W15-MM.
             MOVE W21-DD             TO W15-DD.
             MOVE W15-DTE            TO W15-DEL.
             DISPLAY S16A AT LINE CLIN.
           IF CLIN < 22
               ADD 1                 TO CLIN
               GO TO GB115.
             DISPLAY "C"          AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                      "ontinue or "       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                 "Esc"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                    "ape" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       GB120.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    GO TO GB130
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO GB120
               END-EVALUATE
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF WS-OPTION NOT = "C"
               GO TO GB120.
             DISPLAY S16.
             GO TO GB110.
       GB125.
             DISPLAY "***** END *****"                     AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                    " - Press "                    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                             "ANY"                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                " key to continue" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       GB130.
            PERFORM RESTORE-SCREEN.

       GB999.
             EXIT.
      /
      *    ****    O U T S T A N D I N G   O R D E R S
      *
       GC000         SECTION 5.
       GC00.
             MOVE "ORD"              TO CRT-E-TYP.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY S15.
             MOVE SPACES             TO W15-ACNO.
             MOVE 10                 TO LIN.

       GC05.
             DISPLAY S55.
             ACCEPT S09 AT 0614.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W15-ACNO
                                GO TO GC999
                 WHEN F1-KEY    PERFORM HELP-ROUTINE
                 WHEN F2-KEY    PERFORM AA50
                 WHEN OTHER     PERFORM AA900-ALARM
                                GO TO GC05
               END-EVALUATE
               DISPLAY S09 AT 0614
               IF W15-ACNO = SPACES
                  GO TO GC05.
             PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO GC999.
             PERFORM CA155-GET-CREDITOR.
           IF WS-ERROR NOT = ZERO
               MOVE WS-ER8           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GC05.
             DISPLAY W15-NAME AT 0432 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       GC07.
             MOVE 1                  TO W75-S W75-S1.

       GC08.
             MOVE ZEROES             TO W75-POR(W75-S1).
           IF W75-S1 < 35
               ADD 1                 TO W75-S1
               GO TO GC08.
             MOVE ZERO               TO W75-S1.

       GC09.
             MOVE P-NUMBER           TO ORD-CRED.
             MOVE SPACES             TO ORD-EXT-ITEM.
             MOVE ZERO               TO ORD-DATE.
             MOVE "'D'ate or 'I'tem code sequence  [I]" TO WS-OPT-MES.
             MOVE "I"                TO WS-OPTION.
             MOVE "Y"                TO WS-INSTR.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION  TO WS-SEQ.
           IF WS-OPTION = "D"
               PERFORM START-AT-ORD-C-DATE THRU READ-PORDER-EXIT
           ELSE
               PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE "No outstanding orders" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GC05.

       GC10.
           IF WS-OPTION = "J"
               MOVE SPACE            TO WS-OPTION
               GO TO GC12.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF (WS-F-ERROR = 16) OR (ORD-CRED NOT = P-NUMBER)
               SUBTRACT 1            FROM W75-S
               GO TO GC15.

       GC12.
             MOVE ORD-KEY            TO W75-POR(W75-S).
             MOVE ORD-COST           TO W25-V1.
             MOVE W25-V1             TO W25-3.
             DIVIDE ORD-COST BY ORD-QUANT GIVING W25-7.
             MOVE ORD-QUANT          TO W25-1.
      *
      *    ****    S W I T C H   D A T E   T O   D D M M C C Y Y
      *
             MOVE ORD-DATE           TO W21-DTE.
             MOVE W21-DD             TO W15-DD.
             MOVE W21-MM             TO W15-MM.
             MOVE W21-CC             TO W15-CC.
             MOVE W21-YY             TO W15-YY.
             MOVE W15-DTE            TO W25-DATE.
             MOVE ORD-DELIV          TO W15-DEL.
             DISPLAY W25-DTE AT LINE LIN COLUMN 2  WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W25-5   AT LINE LIN COLUMN 13 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W25-5V2 AT LINE LIN COLUMN 19 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY W25-7V2 AT LINE LIN COLUMN 29 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE ORD-ITEM  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-EXT-STK = "Y"
               DISPLAY ORD-EXT-ITEM AT LINE LIN COLUMN 40 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
           ELSE
           IF WS-USE-ITM = "Y"
               DISPLAY ORD-ITEM AT LINE LIN COLUMN 40 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
                              "/"                     WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT
                                ORD-ITM               WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY ORD-ITEM              AT LINE LIN COLUMN 40 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W15-DEL-MM = ZERO
               DISPLAY W10-MONTH(W05-MM)     AT LINE LIN COLUMN 59 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT
           ELSE
               DISPLAY W10-MONTH(W15-DEL-MM) AT LINE LIN COLUMN 59 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE STK-QUANT          TO W25-1.
             DISPLAY W25-5                   AT LINE LIN COLUMN 63 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE STK-MTD            TO W25-1.
             DISPLAY W25-5                   AT LINE LIN COLUMN 69 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE STK-MAVG           TO W25-1.
             DISPLAY W25-5                   AT LINE LIN COLUMN 75 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF LIN < 45
               ADD 1                 TO LIN W75-S
               GO TO GC10.

       GC15.
            DISPLAY " "                                                 AT 5003 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        " highlight - "                                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                     "PgUp"                                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                         "/"                                          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                          "PgDn"                                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                              " - "                                   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                 "Esc"                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                    "ape, "                           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                         "F8"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                           "=Order Numbers,"          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                          "F3"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                            "=Delete" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       GC16.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 1001               TO CPOS.

       GC17.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       GC20.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    GO TO GC100
                 WHEN DOWN-KEY  GO TO GC105
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO GC20
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   GO TO GC00
                 WHEN F3-KEY    GO TO GC55
                 WHEN F8-KEY    GO TO GC45
                 WHEN PAGE-UP   GO TO GC35
                 WHEN PAGE-DOWN GO TO GC30
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE.
             PERFORM AA900-ALARM.
             GO TO GC20.

       GC25.
             PERFORM CLEAR-L50.
             MOVE 1001               TO CPOS.
             MOVE 10                 TO CRT-START.
             PERFORM ERASE-SCREEN.
             PERFORM GC07 THRU GC08.
             GO TO GC10.

       GC30.
           IF W75-S < 35
               GO TO GC20.
             GO TO GC25.

       GC35.
             MOVE 1                  TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"                TO WS-OPTION.

       GC40.
             PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE SPACE            TO WS-OPTION
               GO TO GC25.
           IF WS-IXS1 < 70
               ADD 1                 TO WS-IXS1
               GO TO GC40.
             GO TO GC25.

       GC45.
             PERFORM SAVE-SCREEN.
             DISPLAY S-POR.
             MOVE 9                  TO SHADE-ROW.
             MOVE 60                 TO SHADE-COL.
             MOVE 16                 TO SHADE-WIDTH.
             MOVE 39                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             MOVE 10                 TO CLIN.
             MOVE 1                  TO W75-S1.

       GC50.
             MOVE W75-POR(W75-S1)    TO W15-ORD.
             DISPLAY SD-POR AT LINE CLIN.
           IF W75-S1 < W75-S
               ADD 1   TO CLIN W75-S1
               GO TO GC50.
             DISPLAY "Press "            AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                           "ANY"                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                              " key to continue" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM RESTORE-SCREEN.
             GO TO GC20.

       GC55.
             PERFORM CLEAR-L50.
             DISPLAY "Delete highlighted order " AT 5003 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                              "Y"        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                               "/"       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                "N"      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE SPACE              TO WS-OPTION.
             ACCEPT WS-OPTION AT 5032 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y")
               PERFORM GC15
               GO TO GC20.
             MOVE W75-POR(W75-S1)    TO W15-ORD.
             MOVE W15-ONO            TO ORD-KEY.
             PERFORM READ-PORDER-LOCK THRU READ-PORDER-EXIT.
             MOVE ORD-ITEM           TO STK-CODE.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
             SUBTRACT ORD-QUANT      FROM STK-ORD.
           IF NOT(STK-ORD > ZERO)
               MOVE ZERO             TO STK-ORD STK-DTE.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
             PERFORM DELETE-PORDER-REC THRU WRITE-PORDER-EXIT.
           IF W75-S = 1
               GO TO GC00.
           IF W75-S1 = 1
               MOVE 2                TO W75-S1
           ELSE
               MOVE 1                TO W75-S1.
             MOVE W75-POR(W75-S1)    TO W15-ORD.
             MOVE W15-ONO            TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
           IF WS-SEQ = "D"
               PERFORM START-AT-ORD-C-DATE THRU READ-PORDER-EXIT
           ELSE
               PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
             GO TO GC25.

       GC100.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
             GO TO GC110.

       GC105.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO GC110.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       GC110.
             PERFORM GC17.
             DISPLAY W43-DET(CLIN) AT CPOS WITH BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO GC20.

       GC999.
             EXIT.
      /    *************************************************************
      *   I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 9.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-DATE W22-DTE3.
             MOVE LS-TODAY-YYMMDD    TO W05-DATE-YMD.
             MOVE W22-DD3            TO W22-DD2.
             MOVE W22-MM3            TO W22-MM2.
             MOVE W22-CC3            TO W22-CC2.
             MOVE W22-YY3            TO W22-YY2.
             MOVE W22-DTE2           TO TODAY-YYMMDD.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-USE-ITM        TO WS-USE-ITM.
             MOVE PAR-EXT-STK        TO WS-EXT-STK.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
 
       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-I-O-ERROR SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 3          MOVE W02-CREDIT   TO WS-FILE
                               MOVE P-NUMBER     TO WS-KEYX
               WHEN 4          MOVE W02-CRTRAN   TO WS-FILE
                               MOVE CT-KEY       TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 16         MOVE W02-PORDER   TO WS-FILE
                               MOVE ORD-KEY      TO WS-KEYX
               WHEN 44         MOVE W02-PDCHEQ   TO WS-FILE
                               MOVE PDT-KEY      TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
