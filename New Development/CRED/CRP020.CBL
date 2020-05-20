      $set LINKCOUNT"468" GNT"CRP020.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******     ****     *
      *   **    **  **    **  **    **   **  **   **    **   **  **    *
      *   **        **    **  **    **  **    **       **   **    **   *
      *   **        ******    *******   **    **     **     **    **   *
      *   **        **   **   **        **    **   **       **    **   *
      *   **    **  **    **  **         **  **   **         **  **    *
      *    ******   **    **  **          ****   ********     ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       P R I C E   L A B E L S   V I A   I N V O I C I N G      *
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
       PROGRAM-ID.      CRP020.
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

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "PRCLAB.SL".

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

       COPY "PARAM.FDE".

       COPY "PRCLAB.FD".

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
       77  WS-STKKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PRCKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-ERROR                PIC  9(01)           VALUE 0.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-CHAR1                PIC  X(01).
       77  WS-ORD                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-LAB                  PIC  X(01).
       77  WS-PRICE                PIC  X(01).
       77  WS-LABCDE               PIC  X(08).
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-IND                  PIC  9(01).
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "CRP020".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "CREDITOR - PRICE LABELS VIA INVOICING".
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
           03  WS-SYS-ID           PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01) VALUE "C".
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
           03  W02-PRN-TYPE          PIC  X(01).
           03  W02-PRN-LINE          PIC  X(136).
           
           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1       PIC  X(140).
           03  REP-LINE2                         REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2       PIC  X(80).
           03  LAB-L1                            REDEFINES W02-PRN-LINE.
               05  LAB-HEAD          PIC  X(06).
               05  LAB-DESC          PIC  X(74).
           03  LAB-L2                            REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(03).
               05  LAB-ITEM          PIC  X(14).
               05  FILLER            PIC  X(63).
           03  LAB-L3                            REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(06).
               05  LAB-COST          PIC  X(08).
               05  FILLER            PIC  X(66).
           03  LAB-L4                            REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(03).
               05  LAB-SELL          PIC  Z(04)9.99 BLANK WHEN ZERO.
               05  FILLER            PIC  X(69).
           03  LB2-L1                            REDEFINES W02-PRN-LINE.
               05  LB2-DET OCCURS 3.
                   07  LB2-DATA.
                       09  LB2-H     PIC  X(16).
                       09  LB2-PRC   PIC  ZZZZ9.99 BLANK WHEN ZERO.
                   07  FILLER        PIC  X(04).
           03  LB3-L1                            REDEFINES W02-PRN-LINE.
               05  LB3-DET OCCURS 4.
                   07  LB3-DATA.
                       09  LB3-H     PIC  X(10).
                       09  LB3-PRC   PIC  ZZZZZZ9.99 BLANK WHEN ZERO.
                       09  FILLER    PIC  X(01).
                   07  FILLER        PIC  X(04).
           03  LB4-L1                            REDEFINES W02-PRN-LINE.
               05  LB4-DET OCCURS 3.
                   07  LB4-DATA.
                       09  LB4-CODE  PIC  X(14).
                   07  FILLER        PIC  X(03).
           03  LB4-L2                            REDEFINES W02-PRN-LINE.
               05  LB4-DATA2 OCCURS 3.
                   07  LB4-DESC      PIC  X(32).
                   07  FILLER        PIC  X(02).
           03  LB4-L3                            REDEFINES W02-PRN-LINE.
               05  LB4-DATA3 OCCURS 3.
                   07  LB4-H         PIC  X(13).
                   07  LB4-LABCDE.
                       09  LB4-PRC   PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(03).
                   07  LB4-DTE       PIC  9(06).
                   07  FILLER        PIC  X(04).
           03  SAPDC-L1                          REDEFINES W02-PRN-LINE.
               05  SAPDC-DET.
                   07  SAPDC-CDE1    PIC  X(14).
                   07  FILLER        PIC  X(04).
                   07  SAPDC-CDE2    PIC  X(14).
                   07  FILLER        PIC  X(03).
                   07  SAPDC-CDE3    PIC  X(14).
                   07  FILLER        PIC  X(04).
                   07  SAPDC-CDE4    PIC  X(14).
                   07  FILLER        PIC  X(03).
           03  SAPDC-L2                          REDEFINES W02-PRN-LINE.
               05  SAPDC-DATA2 OCCURS 4.
                   07  SAPDC-DESC    PIC  X(27).
                   07  FILLER        PIC  X(08).
           03  SAPDC-L3                          REDEFINES W02-PRN-LINE.
               05  SAPDC-DATA3 OCCURS 4.
                   07  SAPDC-H       PIC  X(18).
                   07  SAPDC-PRC     PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(09).
           03  SAPDC-L4                          REDEFINES W02-PRN-LINE.
               05  SAPDC-DET.
                   07  FILLER        PIC  X(05).
                   07  SAPDC-PRC1    PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(10).
                   07  SAPDC-PRC2    PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(09).
                   07  SAPDC-PRC3    PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(10).
                   07  SAPDC-PRC4    PIC  ZZZZ9.99.
               05  FILLER            PIC  X(04).
           
           03  SAPDC-L5                          REDEFINES W02-PRN-LINE.
               05  SAPDC-DATE-SUPP.
                   07  SAPDC-DT1     PIC  X(06).
                   07  FILLER        PIC  X(03).
                   07  SAPDC-SUP1    PIC  X(05).
                   07  FILLER        PIC  X(21).
                   07  SAPDC-DT2     PIC  X(06).
                   07  FILLER        PIC  X(03).
                   07  SAPDC-SUP2    PIC  X(05).
                   07  FILLER        PIC  X(21).
                   07  SAPDC-DT3     PIC  X(06).
                   07  FILLER        PIC  X(03).
                   07  SAPDC-SUP3    PIC  X(05).
                   07  FILLER        PIC  X(21).
                   07  SAPDC-DT4     PIC  X(06).
                   07  FILLER        PIC  X(03).
                   07  SAPDC-SUP4    PIC  X(05).
                   07  FILLER        PIC  X(02).
           03  LB5-L1                            REDEFINES W02-PRN-LINE.
               05  LB5-DET OCCURS 4.
                   07  LB5-CODE      PIC  X(14).
                   07  FILLER        PIC  X(03).
           03  LB5-L2                            REDEFINES W02-PRN-LINE.
               05  LB5-DATA2 OCCURS 4.
                   07  LB5-DESC.
                       09  FILLER    PIC  X(28).
                       09  LB5-SPC   PIC  X(02).
                   07  FILLER        PIC  X(03).
           03  LB5-L3                            REDEFINES W02-PRN-LINE.
               05  LB5-DATA3 OCCURS 4.
                   07  LB5-H         PIC  X(18).
                   07  LB5-PRC       PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(07).
           03  LB5-L4                            REDEFINES W02-PRN-LINE.
               05  LB5-DATA4 OCCURS 4.
                   07  LB5-COST      PIC  X(08).
                   07  FILLER        PIC  X(06).
                   07  LB5-LAB-SUPP.
                       09  LB5-LABCDE PIC X(08).
                       09  LB5-DTE   PIC  X(06).
                   07  LB5-LBSP                  REDEFINES LB5-LAB-SUPP.
                       09  LB5-H2    PIC  X(06).
                       09  LB5-SUPP  PIC  X(06).
                       09  FILLER    PIC  X(02).
                   07  FILLER        PIC  X(05).
           03  LB6-L1                            REDEFINES W02-PRN-LINE.
               05  LB6-DET OCCURS 4.
                   07  LB6-CODE      PIC  X(14).
                   07  FILLER        PIC  X(04).
           03  LB6-L2                            REDEFINES W02-PRN-LINE.
               05  LB6-DATA2 OCCURS 4.
                   07  LB6-DESC.
                       09  FILLER    PIC  X(28).
                       09  LB6-SPC   PIC  X(02).
                   07  FILLER        PIC  X(06).
           03  LB6-L3                            REDEFINES W02-PRN-LINE.
               05  LB6-DATA3 OCCURS 4.
                   07  LB6-H         PIC  X(18).
                   07  LB6-PRC       PIC  ZZZZ9.99.
                   07  FILLER        PIC  X(10).
           03  LB6-L4                            REDEFINES W02-PRN-LINE.
               05  LB6-DATA4 OCCURS 4.
                   07  LB6-COST      PIC  X(08).
                   07  FILLER        PIC  X(06).
                   07  LB6-LAB-SUPP.
                       09  LB6-LABCDE PIC X(08).
                       09  LB6-DTE   PIC  X(06).
                   07  LB6-LBSP                  REDEFINES LB6-LAB-SUPP.
                       09  LB6-H2    PIC  X(06).
                       09  LB6-SUPP  PIC  X(06).
                       09  FILLER    PIC  X(02).
                   07  FILLER        PIC  X(08).

       COPY "W05.VAT".

       01  W10-STOCK.
           03  W10-SELL            PIC S9(07)V999 COMP-3.
           03  W10-CASH            PIC S9(07)V999 COMP-3.
           03  W10-WSALE           PIC S9(07)V999 COMP-3.
           03  W10-VSELL           PIC S9(09)V99  COMP-3.
           03  W10-VCASH           PIC S9(09)V99  COMP-3.
           03  W10-VWSALE          PIC S9(09)V99  COMP-3.
           03  W10-CHECK           PIC S9(09)V99  COMP-3.
           03  W10-STCK            PIC  X(01).
           03  W10-SLNGTH          PIC  X(01)     COMP-X.
           03  W10-ITEM.
               05  W15-ITEM        PIC  X(14).
           03  W10-ITEM14                        REDEFINES W10-ITEM.
               05  W10-I14         PIC  9(14).
           03  W10-ITEM13                        REDEFINES W10-ITEM.
               05  W10-I13         PIC  9(13).
               05  FILLER          PIC  X(01).
           03  W10-ITEM12                        REDEFINES W10-ITEM.
               05  W10-I12         PIC  9(12).
               05  FILLER          PIC  X(02).
           03  W10-ITEM11                        REDEFINES W10-ITEM.
               05  W10-I11         PIC  9(11).
               05  FILLER          PIC  X(03).
           03  W10-ITEM10                        REDEFINES W10-ITEM.
               05  W10-I10         PIC  9(10).
               05  FILLER          PIC  X(04).
           03  W10-ITEM9                         REDEFINES W10-ITEM.
               05  W10-I9          PIC  9(09).
               05  FILLER          PIC  X(05).
           03  W10-ITEM8                         REDEFINES W10-ITEM.
               05  W10-I8          PIC  9(08).
               05  FILLER          PIC  X(06).
           03  W10-ITEM7                         REDEFINES W10-ITEM.
               05  W10-I7          PIC  9(07).
               05  FILLER          PIC  X(07).
           03  W10-ITEM6                         REDEFINES W10-ITEM.
               05  W10-I6          PIC  9(06).
               05  FILLER          PIC  X(08).
           03  W10-ITEM5                         REDEFINES W10-ITEM.
               05  W10-I5          PIC  9(05).
               05  FILLER          PIC  X(09).
           03  W10-ITEM4                         REDEFINES W10-ITEM.
               05  W10-I4          PIC  9(04).
               05  FILLER          PIC  X(10).
           03  W10-ITEM3                         REDEFINES W10-ITEM.
               05  W10-I3          PIC  9(03).
               05  FILLER          PIC  X(11).
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

       01  W15-LABELS.
           03  W15-DEPTH                         OCCURS 2.
               05  W15-WIDTH                     OCCURS 3.
                   07  W15-ITM     PIC  X(14).
                   07  W15-DSC1    PIC  X(30).
                   07  W15-PRC     PIC  9(05)V99.
                   07  W15-SUPP    PIC  X(05).
       01  W15-LAB-2                             REDEFINES W15-LABELS.
           03  W15-LABS OCCURS 4.
               05  W15-SCDE        PIC  X(14).
               05  W15-DSC         PIC  X(22).
               05  W15-PRICE       PIC  9(05)V99.
               05  W15-SUPP2       PIC  X(05).
       01  W15-LAB-3                             REDEFINES W15-LABELS.
           03  W15-LAB4                          OCCURS 3.
               05  W15-ITM4        PIC  X(14).
               05  W15-DSC4        PIC  X(30).
               05  W15-PRC4        PIC  9(05)V99.
               05  W15-SUPP4       PIC  X(05).
               05  W15-COST4       PIC  X(08).
               05  W15-INV4.
                   07  W15-INVN4   PIC  Z(08).
       01  W15-SAPDC                             REDEFINES W15-LABELS.
           03  W15-LAB5 OCCURS 4.
               05  W15-ITM5        PIC  X(14).
               05  W15-DSC5        PIC  X(30).
               05  W15-PRC5        PIC  9(05)V99.
               05  W15-SUPP5       PIC  X(05).
               05  W15-COST        PIC  X(08).
               05  W15-INV.
                   07  W15-INVN    PIC  Z(08).

       01  W20-LABEL.
           03  W20-CODE            PIC  X(14).
           03  W20-DESC            PIC  X(30).
           03  W20-COST.
               05  W20-CST         PIC  9(06)V99.
           03  W20-SELL            PIC  9(05)V99.
           03  W20-NO              PIC  9(05)    COMP-0.
           03  W20-SUPP            PIC  X(05).
           03  W20-INV.
               05  W20-INVN        PIC  Z(08).

       01  W20-CALCS.
           03  W20-RESULT          PIC  9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC  9(06).
               05  W20-DECIMAL     PIC  9(02).
           03  W20-ORD             PIC  9(07)     COMP-3.
       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.
          03  W25-V1               PIC S9(09)V99 COMP-3.
          03  W25-V2               PIC S9(09)V99 COMP-3.
       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC  ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-DTE.
               05  W25-DATE        PIC  99/99/99.
           03  W25-6.
               05  W25-NO          PIC  Z(05)9.
           03  W25-5V2.
               05  W25-VAL         PIC  Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL        PIC  Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC  Z(06)9.99DB.
       01  W30-DATE.
           03  W30-MM              PIC  X(03).
           03  FILLER              PIC  X(01)    VALUE ",".
           03  W30-YY              PIC  9(02).

       01  W30-COST-CODE.
           03  W30-CODE            PIC  X(01)    OCCURS 10.

       01  W35-COST.
           03  W35-CST             PIC  9(08)V99.
           03  W35-WHOLE                         REDEFINES W35-CST
                                   PIC  9(10).
           03  W35-CPRINT.
               05  W35-CEDIT       PIC  ZZZZZZ9999.
           03  W35-EOC                           REDEFINES W35-CPRINT.
               05  W35-CHAR        PIC  X(01)    OCCURS 10.
           03  W35-EON                           REDEFINES W35-CPRINT.
               05  W35-NUM         PIC  9(01)    OCCURS 10.
           03  W35-CPRN                          REDEFINES W35-CPRINT.
               05  FILLER          PIC  X(02).
               05  W35-LCST        PIC  X(08).
           03  W35-S1              PIC  9(02)    COMP-5.
           03  W35-S2              PIC  9(02)    COMP-5.
           03  W35-S3              PIC  9(02)    COMP-5.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H.
           03  FILLER              PIC  X(01)     VALUE "(".
           03  W50-MONTH           PIC  X(03).
           03  FILLER              PIC  X(01)     VALUE ")".
       01  W55-DEL.
           03  W55-MONTH           PIC  X(03).
           03  FILLER              PIC  X(01)     VALUE ",".
           03  W55-YY              PIC  9(02).
       01  W60-ITMCODE.
           03  W60-ITM.
               05  W60-ICHAR       PIC  X(01)     OCCURS 12.
       01  W60-H2.
           03  W60-COMPANY         PIC  X(40).
           03  FILLER              PIC  X(02)     VALUE "-".
           03  W60-DESC            PIC  X(27).
       01  W65-CREDITOR.
           03  W65-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.

       01  W70-VALUES.
           03  W70-BAL             PIC S9(07)V99 COMP-3.
           03  W70-AREAR           PIC S9(07)V99 COMP-3.
           03  W70-30D             PIC S9(07)V99 COMP-3.
           03  W70-60D             PIC S9(07)V99 COMP-3.
           03  W70-90D             PIC S9(07)V99 COMP-3.
           03  W70-120D            PIC S9(07)V99 COMP-3.
           03  W70-150D            PIC S9(07)V99 COMP-3.
           03  W70-180D            PIC S9(07)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W90-TABLE.
           03  W90-ENTRIES.
               05  W90-ENTRY                     OCCURS 80.
                   07  W90-NAME    PIC  X(04).
                   07  W90-KEY     PIC  9(04)    COMP-5.
           03  W90-R1                            REDEFINES W90-ENTRIES.
               05  W90-F20         PIC  X(120).
               05  W90-L60         PIC  X(360).
           03  W90-R2                            REDEFINES W90-ENTRIES.
               05  W90-F60         PIC  X(360).
               05  W90-L20         PIC  X(120).
           03  W90-R3                            REDEFINES W90-ENTRIES.
               05  W90-20          PIC  X(120).
               05  W90-40          PIC  X(120).
               05  W90-60          PIC  X(120).
               05  W90-80          PIC  X(120).

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

       01  MENU-INSTRUCT.
           03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue VALUE "Use ".
           03          COLUMN  6 HIGHLIGHT BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown VALUE "".
           03          COLUMN  7 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue VALUE " or ".
           03          COLUMN 11 HIGHLIGHT BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown VALUE "".
           03          COLUMN 12 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue VALUE " to point to an item, and ".
           03          COLUMN 38 HIGHLIGHT BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown VALUE "<Enter>".
           03          COLUMN 45 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue VALUE " to Select ".
           03          COLUMN 56 HIGHLIGHT BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown VALUE "<Esc>".
           03          COLUMN 61 BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 31 FOREGROUND-COLOR Grey HIGHLIGHT
                                 VALUE "STOCK LABELS CONTROL".
       01  S-MENU.
           03  LINE  4 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           03  LINE  5 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "PRICE LABEL FORMATS".
           03          COLUMN 50 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " ³".
           03  LINE  6 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "1".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 1          ³".
           03  LINE  7 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "2".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 2          ³".
           03  LINE  8 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "3".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 3          ³".
           03  LINE  9 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "4".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 4          ³".
           03  LINE 10 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "5".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 5          ³".
           03  LINE 11 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "6".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 6          ³".
           03  LINE 12 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "7".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 7          ³".
           03  LINE 13 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "8".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 8 (SAPDC)  ³".
           03  LINE 14 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "9".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Format 9          ³".
           03  LINE 15 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "³ ".
           03          COLUMN 31 BACKGROUND-COLOR Green FOREGROUND-COLOR Grey HIGHLIGHT VALUE "0".
           03          COLUMN 32 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE " Exit              ³".
           03  LINE 16 COLUMN 29 BACKGROUND-COLOR Green FOREGROUND-COLOR Black VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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
             PERFORM KA000.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       AA105.
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
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.SCR".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *    D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "ERROR.SCR".

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

       COPY "PRINTUTL.AP8".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ SECTION.

       COPY "CONTROL.RD".

       COPY "DEPART.RD".

       COPY "STOCK.RD".

       COPY "PARAM.RD".

       COPY "PRCLAB.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000-WRITE   SECTION.

       COPY "PRCLAB.WR".

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000         SECTION.
       CA00.
             MOVE 14                 TO W50-LENGTH.
       CA05.
             MOVE W10-ITEM           TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13           TO W10-ITEM
               GO TO CA05.
             CALL "CBL_TOUPPER" USING W50-DATA BY VALUE W50-LENGTH RETURNING W50-STATUS.
             MOVE W50-ITEM           TO W10-ITEM.
       CA06.
             MOVE W50-ITEM           TO STK-CODE.
       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.
       CA99.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      *     Adjust the average cost for negative
      *     or zero quantity
           IF STK-QUANT < 0.001
               MOVE STK-COST         TO STK-AVGE.
           IF STK-WSALE = ZERO
               IF NOT(STK-CASH = ZERO)
                   MOVE STK-CASH     TO STK-WSALE
               ELSE
                   MOVE STK-SELL     TO STK-WSALE.
           IF STK-CASH = ZERO
               MOVE STK-SELL         TO STK-CASH.
           IF STK-IND < 2
               IF STK-CASH < STK-WSALE
                   MOVE STK-WSALE    TO STK-CASH
               END-IF
               IF STK-SELL < STK-CASH
                   MOVE STK-CASH     TO STK-SELL
               END-IF
               IF STK-CMARKUP < STK-WMARKUP
                   MOVE STK-WMARKUP  TO STK-CMARKUP
               END-IF
               IF STK-MARKUP < STK-CMARKUP
                   MOVE STK-CMARKUP  TO STK-MARKUP
               END-IF.
             MOVE STK-CASH           TO W10-CASH
             MOVE STK-SELL           TO W10-SELL
             MOVE STK-WSALE          TO W10-WSALE.
             MOVE ZERO               TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT(1)       TO W05-RATE.
             COMPUTE W10-VCASH  ROUNDED = W10-CASH  + (W10-CASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED = W10-WSALE + (W10-WSALE * W05-RTE).
             COMPUTE W10-VSELL  ROUNDED = W10-SELL  + (W10-SELL * W05-RTE).
           IF STK-PRC = ZERO
               MOVE "EA"             TO STK-PACK.
       CA599.
             EXIT.

      /
       KA000      SECTION 5.
       KA00.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM ERASE-SCREEN.
             DISPLAY "PRICE LABELS" AT 0235 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
           IF WS-PRCKEY < 3
               DISPLAY "No labels to print - Press any key" AT 1212
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT WS-OPTION AT 1247 WITH AUTO
               GO TO KA999.
             DISPLAY S-PROG.
             MOVE 5                  TO SHADE-ROW.
             MOVE 31                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 12                 TO SHADE-LINES.
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.
       KA01.
             MOVE 5                  TO TOP-ROW.
             MOVE 14                 TO BOTTOM-ROW.
             MOVE 4                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 19                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       KA02.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       KA03.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY     PERFORM MOVE-BLOCK-UP
                 WHEN DOWN-KEY   PERFORM MOVE-BLOCK-DOWN
                 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
                                 IF MOUSE-ENTER = "Y"
                                     MOVE "N"  TO MOUSE-ENTER
                                     GO TO KA04
                                 END-IF
                                 IF MOUSE-Y = 24
                                     IF MOUSE-X = 5
                                         MOVE 5  TO KEY-CODE-1
                                         GO TO KA03
                                     ELSE
                                     IF MOUSE-X = 10
                                         MOVE 6  TO KEY-CODE-1
                                         GO TO KA03
                                     ELSE
                                     IF MOUSE-X > 36 AND < 44
                                         MOVE 0  TO KEY-CODE-1
                                         GO TO KA03
                                     ELSE
                                     IF MOUSE-X > 54 AND < 60
                                         MOVE 0  TO KEY-CODE-1
                                         MOVE 1  TO KEY-TYPE
                                         GO TO KA03
                                     END-IF
                                 END-IF
                 WHEN ENTER-KEY  GO TO KA04
                 WHEN OTHER  PERFORM AA900-ALARM
               END-EVALUATE
               GO TO KA02
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE "0"  TO WS-OPTION
                                GO TO KA06
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO KA02
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < "0" OR > "9")
                   PERFORM AA900-ALARM
                   GO TO KA02
               END-IF
               GO TO KA05
           ELSE
               PERFORM AA900-ALARM
               GO TO KA02.

       KA04.
    IF SCREEN-LIN < 5
        PERFORM AA900-ALARM
        GO TO KA02
    ELSE
    IF SCREEN-LIN = 5
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 6
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 7
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "5"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 10
        MOVE "6"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 11
        MOVE "7"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 12
        MOVE "8"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 13
        MOVE "9"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 14
        MOVE "0"   TO WS-OPTION.
    GO TO KA06.

       KA05.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 14   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 5   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 6   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 8   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "5"
        MOVE 9   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "6"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "7"
        MOVE 11   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "8"
        MOVE 12   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "9"
        MOVE 13   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       KA06.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
               GO TO KA999.
          IF (WS-OPTION < "1" OR > "9")
        GO TO KA02.
             MOVE WS-OPTION      TO WS-LAB.
       KA10.
             PERFORM ERASE-SCREEN.
           IF WS-LAB = "1"
               DISPLAY "FORMAT 1 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
           ELSE
           IF WS-LAB = "2"
               DISPLAY "FORMAT 2 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
           ELSE
           IF WS-LAB = "3"
               DISPLAY "FORMAT 3 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
    ELSE
           IF WS-LAB = "4"
               DISPLAY "FORMAT 4 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
    ELSE
    IF WS-LAB = "5"
        DISPLAY "FORMAT 5 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
    ELSE
    IF WS-LAB = "6"
        DISPLAY "FORMAT 6 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
    ELSE
    IF WS-LAB = "7"
        DISPLAY "FORMAT 7 - PRICE LABELS" AT 0229
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
    ELSE
    IF WS-LAB = "8"
               DISPLAY "SAPDC FORMAT - PRICE LABELS" AT 0227
                        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      *
      *    ****    8   L P I
      *
        MOVE 0   TO WS-ADVANCE
        MOVE 8   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
      *
      *    ****    1 7  C P I
      *
        MOVE 3   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE SPACES  TO R-L1
    ELSE
    IF WS-LAB = "9"
        DISPLAY "FORMAT 9 - PRICE LABELS" AT 0229
   WITH FOREGROUND-COLOR Grey HIGHLIGHT.
    IF WS-LAB = "3" OR "5" OR "6" OR "7"
      *
      *    ****    1 2  C P I
      *
        MOVE 0   TO WS-ADVANCE
        MOVE 2   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE SPACES  TO R-L1.
      DISPLAY "Omit price from label " AT 0812
       "Y" WITH FOREGROUND-COLOR Brown HIGHLIGHT "/"
       "N" WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      MOVE "N"   TO WS-PRICE.
       KA15.
      ACCEPT WS-PRICE AT 0839
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-PRICE
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT(WS-PRICE = "N" OR "Y")
               GO TO KA15.
             DISPLAY "Item code :" AT 1012.
             MOVE WS-PRCKEY      TO WS-KEYSTR.
             MOVE 2              TO WS-PRCKEY.
       KA18.
             MOVE 1              TO W35-S1 W35-S2.
    IF WS-LAB = 1 OR 9
               INITIALIZE W20-LABEL
           ELSE
           IF WS-LAB = 2
               INITIALIZE W15-LABELS
           ELSE
           IF WS-LAB = 3
               INITIALIZE W15-LAB-2
           ELSE
    IF WS-LAB = 4 OR 6
               INITIALIZE W15-LAB-3
           ELSE
    IF WS-LAB = 5 OR 7 OR 8
               INITIALIZE W15-SAPDC.
       KA20.
             PERFORM READ-PRCLAB THRU READ-PRCLAB-EXIT.
             MOVE PLB-ITEM       TO W10-ITEM.
      DISPLAY PLB-ITEM AT 1024
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             MOVE PLB-QUANT      TO W20-NO.
      MOVE PLB-INV  TO W20-INVN.
             PERFORM CA000.
           IF NOT(WS-F-ERROR = ZERO)
        GO TO KA795.
    IF WS-LAB = "1" OR "9"
               GO TO KA25
           ELSE
           IF WS-LAB = "2"
               GO TO KA35
           ELSE
           IF WS-LAB = "3"
        GO TO KA65
           ELSE
           IF WS-LAB = "4"
               GO TO KA90
    ELSE
    IF WS-LAB = "5"
        GO TO KA160
    ELSE
    IF WS-LAB = "6"
        GO TO KA200
    ELSE
    IF WS-LAB = "7"
        GO TO KA260
    ELSE
               GO TO KA125.
      GO TO KA800.
       KA25.
             MOVE STK-CODE       TO W20-CODE.
             MOVE STK-DESC       TO W20-DESC.
             MOVE STK-COST       TO W35-CST.
             MOVE STK-SUPP       TO W20-SUPP.
             MOVE W35-WHOLE      TO W35-CEDIT.
            MOVE 3   TO W35-S3.
       KA26.
    IF W35-S3 < 6
        IF W35-CHAR (W35-S3) = SPACE
     MOVE SPACE  TO W35-CHAR (W35-S3)
     ADD 1  TO W35-S3
     GO TO KA26.
          IF (W35-CHAR (W35-S3) NOT NUMERIC) OR
             (W35-NUM (W35-S3) = 0)
              MOVE 10   TO W35-S2
           ELSE
              MOVE W35-NUM (W35-S3)
     TO W35-S2.
            MOVE W30-CODE (W35-S2)
     TO W35-CHAR (W35-S3).
          IF W35-S3 < 10
              ADD 1   TO W35-S3
               GO TO KA26.
      MOVE W35-LCST  TO W20-COST.
       KA28.
      MOVE W10-VSELL  TO W20-SELL.
       KA30.
           IF W20-NO < 1
        GO TO KA795.
      MOVE SPACES  TO R-L1.
    IF WS-LAB = "9"
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE "Item:"  TO LAB-HEAD.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
      MOVE "E"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE SPACES  TO R-L1.
      MOVE W20-CODE  TO LAB-ITEM.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE "X"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE SPACES  TO R-L1.
           IF WS-PRICE = "N"
        MOVE "Prod:"  TO LAB-HEAD
        MOVE W20-COST  TO LAB-COST.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-L1.
      MOVE "Desc:"  TO LAB-HEAD.
      MOVE W20-DESC  TO LAB-DESC.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-L1.
           IF WS-PRICE = "N"
        MOVE "Price"  TO LAB-HEAD
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
      *
      *      S E T   E X P A N D E D   P R I N T
      *
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE
        MOVE SPACES  TO R-L1
        MOVE W20-SELL  TO LAB-SELL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF WS-PRICE = "N"
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL
        MOVE "D"   TO W02-PRN-TYPE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-L1.
           IF NOT(W20-SUPP = SPACES OR LOW-VALUES)
        MOVE "Supp:"  TO LAB-HEAD
               MOVE W20-SUPP     TO LAB-DESC.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-L1.
    IF WS-LAB = "9"
        PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO KA800.
      SUBTRACT 1   FROM W20-NO.
             GO TO KA30.
       KA35.
           IF W20-NO < 1
        GO TO KA795.
             MOVE STK-CODE       TO W15-ITM (W35-S1, W35-S2).
             MOVE STK-DESC       TO W15-DSC1 (W35-S1, W35-S2).
             MOVE STK-SUPP       TO W15-SUPP (W35-S1, W35-S2).
           IF WS-PRICE = "N"
               MOVE W10-VSELL    TO W15-PRC (W35-S1, W35-S2)
           ELSE
               MOVE ZERO         TO W15-PRC (W35-S1, W35-S2).
             SUBTRACT 1          FROM W20-NO.
           IF W35-S1 < 2
               ADD 1             TO W35-S1
               GO TO KA35.
           IF W35-S2 < 3
               MOVE 1            TO W35-S1
               ADD 1             TO W35-S2
               GO TO KA35.
             PERFORM KA40 THRU KA60.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
              GO TO KA800.
             PERFORM KA18.
             GO TO KA35.
       KA40.
             MOVE 1                  TO W35-S1.
       KA45.
             MOVE 1                  TO W35-S2.
             MOVE SPACES             TO R-L1.

       KA50.
           IF W15-ITM(W35-S1, W35-S2) NOT = SPACES
               MOVE W15-ITM(W35-S1, W35-S2)
                                     TO LB2-DATA(W35-S2).
           IF W35-S2 < 3
               ADD 1                 TO W35-S2
               GO TO KA50.

             MOVE 1   TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA45.

       KA55.
           IF W15-ITM(W35-S1, W35-S2) NOT = SPACES
               MOVE W15-DSC1(W35-S1, W35-S2) TO LB2-DATA(W35-S2).
           IF W35-S2 < 3
               ADD 1                 TO W35-S2
               GO TO KA55.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA45.

       KA58.
           IF W15-ITM(W35-S1, W35-S2) NOT = SPACES
               MOVE "VAT inclusive"  TO LB2-H(W35-S2)
               MOVE W15-PRC(W35-S1, W35-S2) TO LB2-PRC(W35-S2).
           IF W35-S2 < 3
               ADD 1                 TO W35-S2
               GO TO KA58.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA45.

       KA60.
           IF NOT(W15-SUPP(W35-S1, W35-S2) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP(W35-S1, W35-S2) TO LB2-DATA(W35-S2).
           IF W35-S2 < 3
               ADD 1                 TO W35-S2
               GO TO KA60.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA45.
           IF W35-S1 < 2
               ADD 1                 TO W35-S1
               GO TO KA50.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-L1.

       KA65.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-SCDE(W35-S1).
             MOVE STK-DESC           TO W15-DSC(W35-S1).
             MOVE STK-SUPP           TO W15-SUPP2(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRICE(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRICE(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA65.
             PERFORM KA70 THRU KA85.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO KA800.
             PERFORM KA18.
             GO TO KA65.

       KA70.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA75.
           IF W15-SCDE(W35-S1) NOT = SPACES
               MOVE W15-SCDE(W35-S1) TO LB3-DATA(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA75.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA70.

       KA80.
           IF W15-SCDE(W35-S1) NOT = SPACES
               MOVE W15-DSC(W35-S1)  TO LB3-DATA(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA80.
             MOVE 1   TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA70.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       KA84.
           IF W15-SCDE(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO LB3-H(W35-S1)
                   MOVE W15-PRICE(W35-S1) TO LB3-PRC(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA84.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA70.

       KA85.
           IF NOT(W15-SUPP2(W35-S1) = SPACES OR LOW-VALUES)
               MOVE W15-SUPP2(W35-S1) TO LB3-DATA(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA85.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA70.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       KA90.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-ITM4(W35-S1).
             MOVE STK-DESC           TO W15-DSC4(W35-S1).
             MOVE STK-SUPP           TO W15-SUPP4(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRC4(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRC4(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA90.
             PERFORM KA95 THRU KA120.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO KA800.
             PERFORM KA18.
             GO TO KA90.

       KA95.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       KA100.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA105.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-ITM4(W35-S1) TO LB4-CODE(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA105.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA100.

       KA110.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-DSC4(W35-S1) TO LB4-DESC(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA110.
             MOVE 1   TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA100.

       KA112.
           IF W15-ITM4(W35-S1) NOT = SPACES
           IF NOT(W15-SUPP4(W35-S1) = SPACES OR LOW-VALUES)
               MOVE "Supplier:"      TO LB4-H(W35-S1)
               MOVE W15-SUPP4(W35-S1) TO LB4-LABCDE(W35-S1).
           IF W35-S1 < 3
               ADD 1             TO W35-S1
               GO TO KA112.
             MOVE 1              TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA100.

       KA115.
           IF W15-ITM4(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO LB4-H(W35-S1)
                   MOVE W15-PRC4(W35-S1) TO LB4-PRC(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA115.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA100.

       KA120.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE WS-LABCDE        TO LB4-LABCDE(W35-S1)
               MOVE W12-MMYY         TO LB4-DTE(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA120.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA100.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       KA125.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-ITM5(W35-S1).
             MOVE STK-DESC           TO W15-DSC5(W35-S1).
             MOVE STK-SUPP           TO W15-SUPP5(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRC5(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRC5(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA125.
             PERFORM KA130 THRU KA155B.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               GO TO KA800.
             PERFORM KA18.
             GO TO KA125.

       KA130.
             MOVE SPACES             TO R-L1.

       KA135.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       KA140.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA145.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF W35-S1 = 1
                   MOVE W15-ITM5(1)  TO SAPDC-CDE1
               ELSE
               IF W35-S1 = 2
                   MOVE W15-ITM5(2)  TO SAPDC-CDE2
               ELSE
               IF W35-S1 = 3
                   MOVE W15-ITM5(3)  TO SAPDC-CDE3
               ELSE
                   MOVE W15-ITM5(4)  TO SAPDC-CDE4.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA145.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA140.

       KA150.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-DSC5(W35-S1) TO SAPDC-DESC (W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA150.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA140.
             PERFORM CALL-PRINTUTL.

       KA155.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO SAPDC-H (W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA155.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-L1.
             PERFORM KA140.

       KA155A.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   IF W35-S1 = 1
                       MOVE W15-PRC5(1) TO SAPDC-PRC1
                   ELSE
                   IF W35-S1 = 2
                       MOVE W15-PRC5(2) TO SAPDC-PRC2
                   ELSE
                   IF W35-S1 = 3
                       MOVE W15-PRC5(3) TO SAPDC-PRC3
                   ELSE
                       MOVE W15-PRC5(4) TO SAPDC-PRC4.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA155A.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA140.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       KA155B.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF W35-S1 = 1
                   MOVE W15-SUPP5(1) TO SAPDC-SUP1
                   MOVE W12-MMYY     TO SAPDC-DT1
               ELSE
               IF W35-S1 = 2
                   MOVE W15-SUPP5(2) TO SAPDC-SUP2
                   MOVE W12-MMYY     TO SAPDC-DT2
               ELSE
               IF W35-S1 = 3
                   MOVE W15-SUPP5(3) TO SAPDC-SUP3
                   MOVE W12-MMYY     TO SAPDC-DT3
               ELSE
                   MOVE W15-SUPP5(4) TO SAPDC-SUP4
                   MOVE W12-MMYY     TO SAPDC-DT4.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA155B.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA140.
             PERFORM CALL-PRINTUTL.
             PERFORM CALL-PRINTUTL.

       KA160.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-ITM5(W35-S1).
             MOVE STK-DESC           TO W15-DSC5(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRC5(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRC5(W35-S1).
             MOVE STK-COST           TO W35-CST.
             MOVE W35-WHOLE          TO W35-CEDIT.
             MOVE 1                  TO W35-S3.
             PERFORM KA26.
             MOVE W20-COST           TO W15-COST(W35-S1).
             MOVE W20-INV            TO W15-INV(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA160.
             PERFORM KA165 THRU KA192.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               GO TO KA800.
             PERFORM KA18.
             GO TO KA160.

       KA165.
             MOVE SPACES             TO R-L1.

       KA170.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       KA175.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA180.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-ITM5(W35-S1) TO LB5-CODE (W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA180.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA175.

       KA185.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-DSC5(W35-S1) TO LB5-DESC(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA185.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA175.

       KA186.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO LB5-H(W35-S1)
                   MOVE W15-PRC5(W35-S1) TO LB5-PRC(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA186.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA175.

       KA190.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-COST(W35-S1) TO LB5-COST(W35-S1)
               MOVE WS-LABCDE        TO LB5-LABCDE(W35-S1)
               MOVE W12-MMYY         TO LB5-DTE(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA190.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA175.

       KA192.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-INV(W35-S1)  TO LB5-COST(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA192.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA175.
             PERFORM CALL-PRINTUTL.

       KA200.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-ITM4(W35-S1).
             MOVE STK-DESC           TO W15-DSC4(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRC4(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRC4(W35-S1).
             MOVE STK-COST           TO W35-CST.
             MOVE W35-WHOLE          TO W35-CEDIT.
             MOVE 1                  TO W35-S3.
             PERFORM KA26.
             MOVE W20-COST           TO W15-COST4(W35-S1).
             MOVE W20-INV            TO W15-INV4(W35-S1).
             MOVE STK-SUPP           TO W15-SUPP4(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA200.
             PERFORM KA205 THRU KA240.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               GO TO KA800.
             PERFORM KA18.
             GO TO KA200.

       KA205.
             MOVE SPACES             TO R-L1.

       KA210.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       KA215.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA220.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-ITM4(W35-S1) TO LB5-CODE(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA220.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA215.

       KA225.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-DSC4(W35-S1) TO LB5-DESC(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA225.
             MOVE SPACES             TO LB5-SPC(3).
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA215.

       KA230.
           IF W15-ITM4(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO LB5-H (W35-S1)
                   MOVE W15-PRC4(W35-S1) TO LB5-PRC(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA230.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA215.

       KA235.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-COST(W35-S1) TO LB5-COST(W35-S1)
               MOVE WS-LABCDE        TO LB5-LABCDE(W35-S1)
               MOVE W12-MMYY         TO LB5-DTE(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA235.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA215.

       KA240.
           IF W15-ITM4(W35-S1) NOT = SPACES
               MOVE W15-INV4(W35-S1) TO LB5-COST(W35-S1).
           IF NOT(W15-SUPP4(W35-S1)= SPACES OR LOW-VALUES)
               MOVE "Supp:"          TO LB5-H2(W35-S1)
               MOVE W15-SUPP4(W35-S1) TO LB5-SUPP(W35-S1).
           IF W35-S1 < 3
               ADD 1                 TO W35-S1
               GO TO KA240.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA215.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.

       KA260.
           IF W20-NO < 1
               GO TO KA795.
             MOVE STK-CODE           TO W15-ITM5(W35-S1).
             MOVE STK-DESC           TO W15-DSC5(W35-S1).
           IF WS-PRICE = "N"
               MOVE W10-VSELL        TO W15-PRC5(W35-S1)
           ELSE
               MOVE ZERO             TO W15-PRC5(W35-S1).
             MOVE STK-COST           TO W35-CST.
             MOVE W35-WHOLE          TO W35-CEDIT.
             MOVE 1                  TO W35-S3.
             PERFORM KA26.
             MOVE W20-COST           TO W15-COST(W35-S1).
             MOVE W20-INV            TO W15-INV(W35-S1).
             SUBTRACT 1              FROM W20-NO.
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA260.
             PERFORM KA265 THRU KA292.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               GO TO KA800.
             PERFORM KA18.
             GO TO KA260.

       KA265.
             MOVE SPACES             TO R-L1.

       KA270.
      *
      *      S E T   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       KA275.
             MOVE 1                  TO W35-S1.
             MOVE SPACES             TO R-L1.

       KA280.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-ITM5(W35-S1) TO LB6-CODE (W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA280.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-L1.
      *
      *      C A N C E L   E X P A N D E D   P R I N T
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             PERFORM KA275.

       KA285.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-DSC5(W35-S1) TO LB6-DESC(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA285.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA275.

       KA286.
           IF W15-ITM5(W35-S1) NOT = SPACES
               IF WS-PRICE = "N"
                   MOVE "VAT incl"   TO LB6-H(W35-S1)
                   MOVE W15-PRC5(W35-S1) TO LB6-PRC(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA286.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA275.

       KA290.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-COST(W35-S1) TO LB6-COST(W35-S1)
               MOVE WS-LABCDE        TO LB6-LABCDE(W35-S1)
               MOVE W12-MMYY         TO LB6-DTE(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA290.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA275.

       KA292.
           IF W15-ITM5(W35-S1) NOT = SPACES
               MOVE W15-INV(W35-S1)  TO LB6-COST(W35-S1).
           IF W35-S1 < 4
               ADD 1                 TO W35-S1
               GO TO KA292.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             PERFORM KA275.
             PERFORM CALL-PRINTUTL.

       KA795.
             ADD 1                   TO WS-PRCKEY.
           IF WS-PRCKEY < WS-KEYSTR
               GO TO KA20.
           IF WS-LAB = "2"
               IF (W35-S1 > 1) OR (W35-S2 > 1)
                   PERFORM KA40 THRU KA60
               END-IF
           ELSE
           IF WS-LAB = "3"
               IF W35-S1 > 1
                   PERFORM KA70 THRU KA85
               END-IF
           ELSE
           IF WS-LAB = "4"
               IF W35-S1 > 1
                   PERFORM KA95 THRU KA120
               END-IF
           ELSE
           IF WS-LAB = "5"
               IF W35-S1 > 1
                   PERFORM KA165 THRU KA192
               END-IF
      *
      *    ****    1 0  C P I
      *
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
           ELSE
           IF WS-LAB = "6"
               IF W35-S1 > 1
                   PERFORM KA205 THRU KA240
               END-IF
      *
      *    ****    1 0  C P I
      *
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
           ELSE
           IF WS-LAB = "7"
               IF W35-S1 > 1
                   PERFORM KA265 THRU KA292
               END-IF
      *
      *    ****    1 0  C P I
      *
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
           ELSE
           IF WS-LAB = "8"
               IF W35-S1 > 1
                   PERFORM KA130 THRU KA155B
               END-IF
      *
      *    ****    1 0  C P I
      *
               MOVE 0                TO WS-ADVANCE
               MOVE 1                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL
               MOVE "D"              TO W02-PRN-TYPE
      *
      *    ****    6   L P I
      *
               MOVE 0                TO WS-ADVANCE
               MOVE 6                TO W02-PRN-TYPE
               PERFORM CALL-PRINTUTL.
             CLOSE PRCLAB.
             OPEN OUTPUT PRCLAB.
             MOVE 1                  TO WS-PRCKEY.
             INITIALIZE PLB-REC1.
             MOVE 2                  TO PLB-QUANT.
             PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT.
             CLOSE PRCLAB.
             GO TO KA999.

       KA800.
             MOVE WS-KEYSTR          TO WS-PRCKEY.
       KA999.
             EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT    SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W60-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-STOCK = "A" OR "N")
               MOVE "A"              TO PAR-STOCK.
               MOVE PAR-STOCK        TO W10-STCK.
           IF PAR-STOCK = "N"
               MOVE PAR-SLNGTH       TO W10-SLNGTH
           ELSE
               MOVE 14               TO W10-SLNGTH.
           IF W10-SLNGTH < 3
               MOVE 3                TO W10-SLNGTH
           ELSE
           IF W10-SLNGTH > 14
               MOVE 14               TO W10-SLNGTH.
       ZA05.
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
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COST-CODE      TO W30-COST-CODE.
             OPEN I-O PRCLAB.
           IF WS-STATUS = "05"
               CLOSE PRCLAB
               OPEN OUTPUT PRCLAB
               INITIALIZE PLB-REC1
               MOVE 2                TO PLB-QUANT
               PERFORM WRITE-PRCLAB THRU WRITE-PRCLAB-EXIT
               CLOSE PRCLAB
               OPEN I-O PRCLAB.
             PERFORM READ-PRCLAB THRU READ-PRCLAB-EXIT.
             MOVE PLB-QUANT          TO WS-PRCKEY.
             MOVE 3                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE STK-LABCDE         TO WS-LABCDE.
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
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 16         MOVE W02-PORDER  TO WS-FILE
                               MOVE ORD-CRED    TO WS-KEYX
               WHEN 22         MOVE W02-STOCKF  TO WS-FILE
                               MOVE STK-CODE    TO WS-KEYX
               WHEN 37         MOVE W02-PRCLAB  TO WS-FILE
                               MOVE WS-PRCKEY   TO WS-KEY
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
