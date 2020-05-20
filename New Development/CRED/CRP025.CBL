      $set LINKCOUNT"512" GNT"CRP025.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******   ********   *
      *   **    **  **    **  **    **   **  **   **    **  **         *
      *   **        **    **  **    **  **    **       **   *******    *
      *   **        ******    *******   **    **     **           **   *
      *   **        **   **   **        **    **    **            **   *
      *   **    **  **    **  **         **  **    **       **    **   *
      *    ******   **    **  **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R ' S   O U T S T A N D I N G   O R D E R S  *
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
       PROGRAM-ID.      CRP025.
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

       COPY "CREDIT.SL".

       COPY "PARAM.SL".

       COPY "PORDER.SL".

       COPY "STOCK.SL".

       COPY "SYSUSE.SL".

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

       COPY "PARAM.FDE".

       COPY "PORDER.FDE".

       COPY "STOCK.FDE".

       COPY "SYSUSE.FD".

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
       77  WS-ITM1                 PIC  X(12).
       77  WS-ORD                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-PAGE                 PIC  9(04)    COMP-5 VALUE 0.
       77  WS-IND                  PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-H1                   PIC  X(05)           VALUE "DATE:".
       77  WS-H3                   PIC  X(05)           VALUE "PAGE:".
       77  WS-HDNG                 PIC  X(118)          VALUE "ORD ITEM CODE           QUANTITY  X-REF/BAR CODE     DESCRIPTION                     UNIT COST     TOTAL COST CONTACT".
       77  WS-HDNG2                PIC  X(128)          VALUE " ORD NUMBER ITEM CODE          QUANTITY X-REF/BAR CODE     DESCRIPTION                       UNIT COST   TOTAL COST ORDERED BY".
       77  WS-CRD-ORD              PIC  X(36)           VALUE "CREDITOR - OUTSTANDING ORDERS REPORT".
       77  WS-SEL-ORD              PIC  X(33)           VALUE "CREDITOR - PRINT SELECTIVE ORDERS"
       77  WS-ER1                  PIC  X(09)           VALUE "No record".
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.

      /
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP025".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR - OUTSTANDING ORDERS REPORT".
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
           03  WS-MODULE           PIC  X(03)           VALUE "ORD".
           03  WS-PROG             PIC  X(03)           VALUE "PRN".

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

           03  R-L3                              REDEFINES W02-PRN-LINE.
               05  R-H4            PIC  X(06).
               05  R-H5            PIC  X(34).
               05  R-H6            PIC  X(12).
               05  R-H7            PIC  X(12).
               05  R-H8            PIC  X(12).
               05  R-H9            PIC  X(12).
               05  R-H10           PIC  X(11).
               05  R-H11           PIC  X(12).
               05  R-H12           PIC  X(12).
               05  R-H13           PIC  X(09).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(64).
               05  R-H14           PIC  X(12).
               05  R-H15           PIC  X(12).
               05  R-H16           PIC  X(12).
               05  R-H17           PIC  X(12).
               05  R-H18           PIC  X(12).
               05  R-H19           PIC  X(08).

           03  R-L21                             REDEFINES W02-PRN-LINE.
               05  OR-H1           PIC  X(12).
               05  OR-H2.
                   07  FILLER      PIC  X(04).
                   07  OR-H2A      PIC  X(27).
               05  OR-H3           PIC  X(06).
               05  OR-H4           PIC  X(12).
               05  OR-H5           PIC  X(10).
               05  OR-H6           PIC  X(09).
               05  OR-H7           PIC  X(22).
               05  OR-H8           PIC  X(13).
               05  OR-H9           PIC  X(09).
               05  OR-H10          PIC  X(08).

           03  R-L22                             REDEFINES W02-PRN-LINE.
               05  OR-ORD          PIC  Z(07)9/99.
               05  FILLER          PIC  X(01).
               05  OR-DESC.
                   07  OR-ITEM     PIC  X(14).
                   07  OR-SL       PIC  X(01).
                   07  OR-ITM      PIC  X(03).
                   07  FILLER      PIC  X(12).
               05  FILLER          PIC  X(01).
               05  OR-QUANT        PIC  Z(04)9.
               05  FILLER          PIC  X(01).
               05  OR-CURRENCY.
                   07  FILLER      PIC  X(01).
                   07  OR-RATE     PIC  Z(02)9.99.
                   07  FILLER      PIC  X(03).
               05  FILLER          PIC  X(01).
               05  OR-COST         PIC  Z(06)9.99.
               05  FILLER          PIC  X(01).
               05  OR-PMT.
                   07  FILLER      PIC  X(01).
                   07  OR-DISC     PIC  Z9.99    BLANK WHEN ZERO.
                   07  FILLER      PIC  X(02).
               05  FILLER          PIC  X(01).
               05  OR-NAME.
                   07  OR-CONTACT.
                       09  OR-ACNO PIC  X(06).
                       09  FILLER  PIC  X(15).
                   07  FILLER      PIC  X(01).
                   07  OR-TEL      PIC  X(12).
               05  FILLER          PIC  X(01).
               05  OR-DEL          PIC  Z9/99/99.
               05  FILLER          PIC  X(01).
               05  OR-RMKS.
                   07  OR-PSLIP    PIC  Z(08).

           03  R-L23                             REDEFINES W02-PRN-LINE.
               05  REP-PG          PIC  X(06).
               05  REP-P-N         PIC  Z(03)9.
               05  FILLER          PIC  X(16).
               05  REP-COMP        PIC  X(40).
               05  FILLER          PIC  X(02).
               05  REP-REPORT      PIC  X(40).
               05  FILLER          PIC  X(08).
               05  REP-DAT         PIC  X(06).
               05  REP-DT          PIC  Z9/99/9999.

           03  R-L24                             REDEFINES W02-PRN-LINE.
               05  O-HDNG.
                   07  FILLER      PIC X(01).
                   07  O-LINE      PIC 99.
                   07  FILLER      PIC X(01).
                   07  O-EXT-ITEM.
                       09  O-ITEM  PIC  X(14).
                       09  O-SL    PIC  X(01).
                       09  O-ITM   PIC  X(03).
                   07  FILLER      PIC  X(03).
                   07  O-QNT       PIC  Z(05)9.
                   07  FILLER      PIC  X(03).
                   07  O-XREF      PIC  X(18).
                   07  FILLER      PIC  X(01).
                   07  O-DESC      PIC  X(30).
                   07  FILLER      PIC  X(01).
                   07  O-COST      PIC  Z(07)9.999.
                   07  FILLER      PIC  X(01).
                   07  O-EXT       PIC  Z(08)9.99.
                   07  FILLER      PIC  X(01).
                   07  O-CONTACT   PIC  X(22).

           03  R-L25                             REDEFINES W02-PRN-LINE.
               05  REP-EXPNAME     PIC  X(40).
               05  FILLER          PIC  X(92).

           03  R-L26                             REDEFINES W02-PRN-LINE.
               05  REP-NADD        PIC  X(44).
               05  REP-RADD                      REDEFINES REP-NADD.
                   07  REP-PC      PIC  X(04).
                   07  FILLER      PIC  X(40).
               05  FILLER          PIC  X(40).
               05  REP-H2          PIC  X(16).
               05  REP-SMAN.
                   07  REP-TEL     PIC  X(14).
                   07  REP-GNO                   REDEFINES REP-TEL PIC X(14).
                   07  FILLER      PIC  X(09).
                   07  O2-PAGE     PIC  X(06).
                   07  O2-PGNO     PIC  ZZ9.

           03  R-L27                             REDEFINES W02-PRN-LINE.
               05  REP-NAME        PIC  X(40).
               05  REP-DADD                      REDEFINES REP-NAME.
                   07  REP-PC1     PIC  X(04).
                   07  FILLER      PIC  X(36).
               05  FILLER          PIC  X(12).
               05  REP-H3          PIC  X(08).
               05  REP-DADD2.
                   07  REP-PC2     PIC  X(30).
               05  FILLER          PIC  X(02).
               05  REP-H4          PIC  X(17).
               05  REP-STEL.
                   07  REP-ORD.
                       09  REP-TERMS.
                           11  FILLER      PIC  X(05).
                           11  REP-AC.
                               13  FILLER  PIC  X(03).
                               13  REP-PGE PIC  ZZ9.
                       09  FILLER  PIC  X(01).
                   07  FILLER      PIC  X(02).
               05  FILLER          PIC  X(01).

           03  R-L28                             REDEFINES W02-PRN-LINE.
               05  REP-H1          PIC  X(23).
               05  REP-REF         PIC  9(08).
               05  FILLER          PIC  X(49).

           03  R-L29                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(17).
               05  REP-INVOICE.
                   07  FILLER      PIC  X(03).
                   07  REP-CRNOTE.
                       09  FILLER   PIC X(06).
                       09  REP-COPY PIC X(12).
                       09  FILLER   PIC X(06).
                   07  FILLER      PIC  X(03).
               05  FILLER          PIC  X(01).
               05  REP-H14         PIC  X(08).
               05  REP-DTE         PIC  99/99/9999.

           03  R-L30                             REDEFINES W02-PRN-LINE.
               05  O2-HDNG.
                  07  O2-ORD       PIC  Z(07)9.
                  07  O2-SL        PIC  X(01).
                  07  O2-LINE      PIC  9(02).
                  07  FILLER       PIC  X(01).
                  07  O2-EXT-ITEM.
                      09  O2-ITEM  PIC  X(14).
                      09  O2-SL2   PIC  X(01).
                      09  O2-ITM   PIC  X(03).
                  07  FILLER       PIC  X(02).
                  07  O2-QNT       PIC  Z(05)9.
                  07  FILLER       PIC  X(02).
                  07  O2-XREF      PIC  X(18).
                  07  FILLER       PIC  X(01).
                  07  O2-DESC      PIC  X(30).
                  07  FILLER       PIC  X(01).
                  07  O2-HDV.
                      09  O2-COST  PIC  Z(07)9.999.
                  07  FILLER       PIC  X(01).
                  07  O2-EXT       PIC  Z(08)9.99.
                  07  FILLER       PIC  X(01).
                  07  O2-SMAN      PIC  X(20).

       COPY "SYSUSE.ID".

       COPY "W05.VAT".

       01  W10-STOCK.
           03  W10-VSELL           PIC S9(07)V99  COMP-3.
           03  W10-VCASH           PIC S9(07)V99  COMP-3.
           03  W10-VWSALE          PIC S9(07)V99  COMP-3.
           03  W10-NAME.
               05  W10-N1-2        PIC  X(02).
               05  W10-N3-4        PIC  X(02).
               05  W10-N5-6        PIC  X(02).
               05  W10-N7-8        PIC  X(02).
               05  W10-N9-10       PIC  X(02).
               05  W10-N11-12      PIC  X(02).
               05  W10-N13-14      PIC  X(02).
               05  W10-N15-16      PIC  X(02).
               05  W10-N17-18      PIC  X(02).
               05  W10-N19-20      PIC  X(02).
               05  W10-N21-22      PIC  X(02).
               05  W10-N23-24      PIC  X(02).
               05  W10-N25-26      PIC  X(02).
               05  W10-N27-28      PIC  X(02).
               05  W10-N29-30      PIC  X(02).

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

           03  W15-EXT-ITEM.
               05  W15-ITEM        PIC  X(14).
               05  W15-ITM         PIC  X(03).
               05  FILLER          PIC  X(01).
           03  W15-YMD             PIC  9(08).
           03  W15-RYMD REDEFINES W15-YMD.
               05  W15-CN          PIC  9(02).
               05  W15-YR          PIC  9(02).
               05  W15-MT          PIC  9(02).
               05  W15-DY          PIC  9(02).

       01  W15-USER.
           03  W15-ENCRYPT.
               05  W15-N1-2        PIC  X(02).
               05  W15-N3-4        PIC  X(02).
               05  W15-N5-6        PIC  X(02).
               05  W15-N7-8        PIC  X(02).
               05  W15-N9-10       PIC  X(02).
               05  W15-N11-12      PIC  X(02).
               05  W15-N13-14      PIC  X(02).
               05  W15-N15-16      PIC  X(02).
               05  W15-N17-18      PIC  X(02).
               05  W15-N19-20      PIC  X(02).
               05  W15-N21-22      PIC  X(02).
               05  W15-N23-24      PIC  X(02).
               05  W15-N25-26      PIC  X(02).
               05  W15-N27-28      PIC  X(02).
               05  W15-N29-30      PIC  X(02).
               
       01  W20-CALCS.
           03  W20-RESULT          PIC 9(06)V99.
           03  W20-RESULT-1                      REDEFINES W20-RESULT.
               05  W20-WHOLE       PIC 9(06).
               05  W20-DECIMAL     PIC 9(02).
           03  W20-ORD             PIC 9(07)     COMP-3.

       01  W25-CALCS.
           03  W25-RESULT          PIC 9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC 9(05).
               05  W25-DECIMAL     PIC 9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC 9(04).
               05  W25-SUB         PIC 9(01).
               05  FILLER          PIC 9(02).

       01  W25-VALUES.
           03  W25-V1              PIC S9(09)V99 COMP-3.
           03  W25-V2              PIC S9(09)V99 COMP-3.
           03  W25-TV1             PIC S9(09)V99 COMP-3.
           03  W25-TV2             PIC S9(09)V99 COMP-3.

       01  W25-EDIT.
           03  W25-5.
               05  W25-1           PIC ZZZZ9.
           03  W25-3V2.
               05  W25-2           PIC ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC Z(06)9.99.
           03  W25-3N.
               05  W25-4           PIC ZZ9.
           03  W25-DTE.
              05  W25-DATE         PIC 99/99/9999.
           03  W25-6.
               05  W25-NO          PIC Z(05)9.
           03  W25-5V2.
               05  W25-VAL         PIC Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL        PIC Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC Z(06)9.99DB.


       01  W40-COMPANY.
            05  W40-ADDRESS1.
              07  W40-ADD1    PIC  X(30).
              07  W40-ADD2    PIC  X(30).
              07  W40-ADD3    PIC  X(30).
              07  W40-ADD4    PIC  X(30).
             05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
               07  W40-ACHAR OCCURS 120 PIC X(01).
            05  W40-PC1       PIC  X(04).
            05  W40-TEL       PIC  X(14).
            05  W40-FAX       PIC  X(14).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W60-H2.
           03  W60-COMPANY     PIC X(40).
           03  FILLER          PIC X(02)     VALUE "-".
           03  W60-DESC        PIC X(27).

       01  W65-CREDITOR.
           03  W65-CFLOW       PIC S9(07)V99 COMP-3 OCCURS 6.

       01  W70-VALUES.
           03  W70-BAL         PIC S9(07)V99 COMP-3.
           03  W70-AREAR       PIC S9(07)V99 COMP-3.
           03  W70-30D         PIC S9(07)V99 COMP-3.
           03  W70-60D         PIC S9(07)V99 COMP-3.
           03  W70-90D         PIC S9(07)V99 COMP-3.
           03  W70-120D        PIC S9(07)V99 COMP-3.
           03  W70-150D        PIC S9(07)V99 COMP-3.
           03  W70-180D        PIC S9(07)V99 COMP-3.

       COPY "W70CRED.WS".

       01  W75-KEYS.
           03  W75-S               PIC 9(02) COMP-5.
           03  W75-S1              PIC 9(02) COMP-5.
           03  W75-S2              PIC 9(02) COMP-5.
           03  W75-CNO             PIC X(06) OCCURS 32.
           03  W75-ITEMS                     OCCURS 32.
               05  W75-CODE.
                   07  W75-POR     PIC  9(10).
                   07  FILLER      PIC  X(08).

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

       01  W95-STM.
           03  W95-ADD1        PIC X(30).
           03  W95-ADD2        PIC X(30).
           03  W95-ADD3        PIC X(30).
           03  W95-ADD4        PIC X(30).
           03  W95-PC.
               05  W95-POST    PIC 9(04).
           03  W95-TEL         PIC X(14).

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

       01  S21.
          03  PIC Z(07)9 USING W15-OPREF BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

       01  S22.
          03  PIC 99 USING W15-OSUB BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT AUTO.

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
               05  LINE  8 COLUMN 9 VALUE     "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
               05  LINE  9 COLUMN 9 VALUE     "³                     O R D E R   L O O K U P                      ³".
               05  LINE 10 COLUMN 9 VALUE     "ÃÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ´".
               05  S-DET.
                   07  LINE 11 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 12 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 13 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 14 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 15 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 16 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 17 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 18 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 19 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 20 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 21 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 22 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 23 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 24 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 25 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 26 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 27 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 28 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 29 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 30 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 31 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 32 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 33 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 34 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 35 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 36 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 37 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 38 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 39 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 40 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 41 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
                   07  LINE 42 COLUMN 9 VALUE "³           ³              ³                              ³        ³".
               05  LINE 43 COLUMN 9 VALUE     "ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ".

       01  SD-POR.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Magenta.
               05  COLUMN 10 PIC Z(07)9    FROM ORD-ORD.
               05  COLUMN 18 VALUE "/"                   FOREGROUND-COLOR Black.
               05  COLUMN 19 PIC 99        FROM ORD-LINE.
               05  COLUMN 22 PIC X(14)     FROM STK-CODE.
               05  COLUMN 37 PIC X(30)     FROM STK-DESC.
               05  COLUMN 68 PIC Z(04)9.99 FROM ORD-QUANT.

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
             PERFORM GA000.
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
             CLOSE SYSUSER.
             EXIT PROGRAM.

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

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB00-READ SECTION.

       COPY "CONTROL.RD".

       COPY "STOCK.RD".

       COPY "CREDIT.RD".

       COPY "PARAM.RD".

       COPY "PORDER.RD".

       COPY "SYSUSE.RD".

       COPY "CREDIT.ILU".

       COPY "APAC.HLP".

       BB010        SECTION.
       BB10.
             MOVE W15-N29-30  TO W10-N1-2.
             MOVE W15-N1-2  TO W10-N3-4.
             MOVE W15-N27-28  TO W10-N5-6.
             MOVE W15-N3-4  TO W10-N7-8.
             MOVE W15-N25-26  TO W10-N9-10.
             MOVE W15-N5-6  TO W10-N11-12.
             MOVE W15-N23-24  TO W10-N13-14.
             MOVE W15-N7-8  TO W10-N15-16.
             MOVE W15-N21-22  TO W10-N17-18.
             MOVE W15-N9-10  TO W10-N19-20.
             MOVE W15-N19-20  TO W10-N21-22.
             MOVE W15-N11-12  TO W10-N23-24.
             MOVE W15-N17-18  TO W10-N25-26.
             MOVE W15-N13-14  TO W10-N27-28.
             MOVE W15-N15-16  TO W10-N29-30.

       BB15.
             EXIT.

      /
       GA000      SECTION 5.
       GA00.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY "C"                                                AT 0612 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      "reditor or "                                             WITH FOREGROUND-COLOR Grey
                                 "O"                                            WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                  "rder no. sequence or "                       WITH FOREGROUND-COLOR Grey
                                                       "S"                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                        "elective per Creditor" WITH FOREGROUND-COLOR Grey.
             MOVE "S"                TO WS-OPTION.
             DISPLAY "Esc"    AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "ape to Exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       GA00A.
             ACCEPT WS-OPTION AT 0669
             WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO GA999
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO GA00A
               END-EVALUATE.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "C" OR "O" OR "S")
               GO TO GA00A.
           IF WS-OPTION = "C" OR "O"
               DISPLAY "Busy printing outstanding orders" AT 0612.
           IF WS-OPTION = "O"
               DISPLAY "Order No." AT 1012
           ELSE
           IF WS-OPTION = "C"
               PERFORM GC000
               GO TO GA00
           ELSE
               PERFORM GB000
               MOVE WS-CRD-ORD       CRT-HEADING
               PERFORM HEADING-AND-CLEAR-SCREEN
               GO TO GA00.
             MOVE WS-OPTION          TO WS-ORD.
             MOVE ZERO               TO WS-PAGE.
             MOVE ZERO               TO W25-V1 W25-V2.
      *
      *    ****   1 7   C P I   ( C O N D E N S E D   P R I N T )
      *
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.

       GA10.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE "Page:"            TO REP-PG.
             MOVE W60-COMPANY        TO REP-COMP.
             MOVE "Date:"            TO REP-DAT.
             ADD 1                   TO WS-PAGE.
             MOVE TODAY-DDMMYY       TO REP-DT.
             MOVE WS-PAGE            TO REP-P-N.
             MOVE "OUTSTANDING - STOCK PURCHASE ORDERS" TO REP-REPORT.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "ORDER.NO"         TO OR-H1.
             MOVE "ITEM CODE"        TO OR-H2.
             MOVE "QUANT"            TO OR-H3.
             MOVE "CURRENCY"         TO OR-H4.
             MOVE "UNIT COST"        TO OR-H5.
             MOVE "PAYMENT"          TO OR-H6.
             MOVE "CREDITOR"         TO OR-H7.
             MOVE "FAX NO"           TO OR-H8.
             MOVE "DELIVERY"         TO OR-H9.
             MOVE "PCK-SLIP"         TO OR-H10.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "/DESCRIPTION"     TO OR-H2A.
             MOVE " /RATE"           TO OR-H4.
             MOVE "  /RAND"          TO OR-H5.
             MOVE " /DISC"           TO OR-H6.
             MOVE "/NAME/CONTACT"    TO OR-H7.
             MOVE "/TELEPHONE"       TO OR-H8.
             MOVE "/REMARKS"         TO OR-H10.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       GA20.
             MOVE 1                  TO ORD-ORD.
             MOVE ZERO               TO ORD-LINE.
             PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
               GO TO GA70.
             MOVE SPACES             TO W15-ACNO.

       GA30.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
          IF NOT(WS-F-ERROR = ZERO)
               GO TO GA70.
           IF ORD-CRED = W15-ACNO
               GO TO GA40.
           IF W15-ACNO NOT = SPACES
               MOVE SPACES       TO R-DET
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
               GO TO GA75.
      MOVE ORD-CRED  TO P-NUMBER W15-ACNO.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-ORD = "C"
        DISPLAY P-NUMBER AT 1022
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta "°"
         P-NAME WITH FOREGROUND-COLOR Grey HIGHLIGHT
       BACKGROUND-COLOR Magenta.
       GA40.
           IF ORD-QUANT = ZERO
               GO TO GA30.
             MOVE SPACES         TO R-DET.

       GA45.
             MOVE ORD-ORD        TO W15-OPREF.
             MOVE ORD-LINE       TO W15-OSUB.
            MOVE ORD-EXT-ITEM  TO STK-CODE W15-EXT-ITEM.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-DESC       TO W15-DESCR.
            MOVE ORD-QUANT  TO W15-UN.
             MOVE ORD-DELIV      TO W15-DEL.
             MOVE ORD-UCOST      TO W15-COST.
             MOVE ORD-SELL       TO W15-SELL.
             MOVE ORD-OCOST      TO W15-OCOST.
             MOVE ORD-COST       TO W15-TCOST.
      ADD ORD-QUANT  TO W25-V1.
      COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
      MOVE ORD-FAX  TO W15-FAX.
             MOVE ORD-TEL        TO W15-TEL.
             MOVE ORD-DRATE      TO W15-RATE.
             MOVE ORD-CONTACT    TO W15-CONTACT.
             MOVE ORD-PMT        TO W15-PMT.
             MOVE ORD-INST       TO W15-RMKS.
             MOVE ORD-DISC       TO W15-DISC1.
             MOVE ORD-PSLIP      TO W15-PSLIP.
             MOVE ORD-CURRENCY   TO W15-CURRENCY.
             MOVE ORD-TERMS      TO W15-SETT.
            MOVE W15-ORD  TO OR-ORD.
           IF WS-ORD = "O"
        DISPLAY OR-ORD AT 1022
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta.
            MOVE W15-EXT-ITEM  TO OR-DESC.
            MOVE W15-UN  TO OR-QUANT.
      MOVE W15-CURRENCY  TO OR-CURRENCY.
            MOVE W15-OCOST  TO OR-COST.
      MOVE W15-PMT  TO OR-PMT.
            MOVE W15-ACNO  TO OR-ACNO.
             MOVE W15-FAX        TO OR-TEL.
             MOVE W15-DEL        TO OR-DEL.
             MOVE W15-PSLIP      TO OR-PSLIP.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
               GO TO GA75.
            MOVE SPACES  TO R-DET.
             MOVE W15-DESCR      TO OR-DESC.
             MOVE W15-RATE       TO OR-RATE.
             MOVE W15-COST       TO OR-COST.
             MOVE W15-DISC1      TO OR-DISC.
             MOVE P-NAME         TO OR-NAME.
             MOVE W15-RMKS       TO OR-RMKS.
      PERFORM CALL-PRINTUTL.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
               GO TO GA75.
            MOVE SPACES  TO R-DET.
             MOVE W15-CONTACT    TO OR-CONTACT.
             MOVE W15-TEL        TO OR-TEL.
    IF W02-LINAGE < (W02-PRN-LENGTH - 2)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM GA10.
             MOVE SPACES         TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
               GO TO GA75.

       GA50.
             GO TO GA30.

       GA70.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "******  TOTAL ORDERS -   Items"
     TO O2-DESC.
      MOVE "    Value"  TO O2-HDV.
      MOVE W25-V1  TO O2-QNT.
      MOVE W25-V2  TO O2-EXT.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "******  END OF REPORT  ******"
     TO R-DET.

       GA75.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       GA999.
             EXIT.

       GB000      SECTION.
       GB00.
             MOVE WS-SEL-ORD         TO CRT-HEADING.
             PERFORM HEADING-AND-CLEAR-SCREEN.
             DISPLAY "Creditor No." AT 0604.
             MOVE SPACES             TO W15-ACNO.
             DISPLAY S55.
             MOVE ZERO   TO W25-V1 W25-V2.

       GB05.
             ACCEPT W15-ACNO AT 0617 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO GB999
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM SAVE-SCREEN
                               PERFORM SCREEN-CONTENTS
                               PERFORM CREDITOR-LOOKUP
                               PERFORM RESTORE-SCREEN
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO GB05
               END-EVALUATE
               DISPLAY W15-ACNO AT 0617 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT
               IF W15-ACNO = SPACES
                   GO TO GB05.
             MOVE 5                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
           IF W15-ACNO = SPACES
               GO TO GB999.
             PERFORM CLEAR-L50.
             MOVE W15-ACNO           TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB05.
             DISPLAY P-NAME AT 0624 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO GB00.
             DISPLAY "A" AT               0804 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      "ll or "                 WITH FOREGROUND-COLOR Grey
                            "S"                WITH FOREGROUND-COLOR Brown HIGHLIGHT
                             "elective orders" WITH FOREGROUND-COLOR Grey.
             MOVE "A"                TO WS-OPTION.
             DISPLAY "Esc"    AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "ape to Exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.

       GB06.
             ACCEPT WS-OPTION AT 0828 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO GB999
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO GB06
               END-EVALUATE.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "A" OR "S")
               GO TO GB06.
             PERFORM CLEAR-L50.
           IF WS-OPTION = "A"
               MOVE P-NUMBER         TO ORD-CRED
               MOVE SPACES           TO ORD-EXT-ITEM
               GO TO GB30.
             DISPLAY "Order No.   " AT 1004.

       GB10.
             MOVE ZERO   TO W15-OPREF.
             DISPLAY  "F2"                       AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                        "=View Orders or "               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                        "Esc"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                           "ape to exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S21 AT 1017.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE ZERO  TO W15-ORD
                               DISPLAY "        " AT 1017 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT
                 WHEN F2-KEY   PERFORM PORDER-LOOKUP
                               IF W15-ORD = ZERO
                                   GO TO GB10
                               END-IF
                               DISPLAY S21 AT 1017
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO GB10
               END-EVALUATE.
           IF W15-OPREF = ZERO
               GO TO GB00.
             PERFORM CLEAR-L50.
             MOVE W15-OPREF          TO ORD-ORD.
             MOVE ZERO               TO ORD-LINE.
             PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB10.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF NOT(W15-OPREF = ORD-ORD)
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB10.
           IF NOT(P-NUMBER = ORD-CRED)
               MOVE "Invalid order number" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB10.
             UNSTRING P-ADDR DELIMITED BY "*" INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW MOVE 1       TO WS-ERROR.
             MOVE P-TEL              TO W40-TEL.
             MOVE P-FAX              TO W40-FAX.

       GB15.
             MOVE ORD-ORD            TO W15-OPREF.
             MOVE ZERO               TO W15-OSUB.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE "PURCHASE ORDER NUMBER:" TO REP-H1.
             MOVE W15-OPREF          TO REP-REF.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE W60-COMPANY        TO REP-EXPNAME.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD1           TO REP-NADD.
           IF W05-VATNO NOT = SPACES
               MOVE "VAT No/BTW Nr :" TO REP-H2
               MOVE W05-VATNO        TO REP-GNO.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD2           TO REP-NADD.
           IF W95-TEL NOT = SPACES
               MOVE "TELEPHONE/FOON:" TO REP-H2
               MOVE W95-TEL          TO REP-TEL.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD3           TO REP-NADD.
             MOVE "ORD.BY/BEST.DR:"  TO REP-H2.
             MOVE ORD-DRATE          TO USE-NO.
           IF NOT(USE-NO = ZERO)
               PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
               MOVE USE-NAME         TO W15-USER
               PERFORM BB010
               MOVE W10-NAME         TO REP-SMAN.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD4           TO REP-NADD.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-PC             TO REP-PC.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "DATE  :"          TO REP-H14.
             MOVE ORD-DATE           TO W15-YMD.
             MOVE W15-DY             TO W15-DD.
             MOVE W15-MT             TO W15-MM.
             MOVE W15-CN             TO W15-CC.
             MOVE W15-YR             TO W15-YY.
             MOVE W15-DATE           TO REP-DTE.
             PERFORM CALL-PRINTUTL.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE P-NAME             TO REP-NAME.
             MOVE "SUPPLIER NUMBER : " TO REP-H4.
             MOVE ORD-CRED           TO REP-AC.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD1           TO REP-NAME.
             MOVE "SUPPLIER TEL.NO : " TO REP-H4.
             MOVE W40-TEL            TO REP-STEL.
             PERFORM CALL-PRINTUTL.
             MOVE W40-ADD2           TO REP-NAME.
             MOVE "SUPPLIER FAX.NO : " TO REP-H4.
             MOVE W40-FAX            TO REP-STEL.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD3           TO REP-NAME.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD4           TO REP-NAME.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE P-PC               TO REP-PC1.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE WS-HDNG            TO O-HDNG.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE SPACES             TO W15-CONTACT.

       GB20.
             MOVE ORD-LINE           TO O-LINE.
             MOVE ORD-EXT-ITEM       TO O-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-DESC           TO O-DESC.
             MOVE STK-XREF           TO O-XREF.
             MOVE ORD-QUANT          TO O-QNT.
             MOVE ORD-UCOST          TO O-COST.
             COMPUTE O-EXT = ORD-QUANT * ORD-UCOST.
             ADD ORD-QUANT           TO W25-V1.
             COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
           IF NOT(ORD-CONTACT = W15-CONTACT)
               MOVE ORD-CONTACT      TO O-CONTACT W15-CONTACT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE ZERO             TO WS-IND
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE 1                TO WS-IND.
             MOVE SPACES             TO R-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET
               GO TO GB25.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO GB25.
           IF NOT(W15-OPREF = ORD-ORD)
               GO TO GB22.
           IF WS-IND = 1
               PERFORM GB15.
             GO TO GB20.

       GB22.
             MOVE SPACES             TO R-DET.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "**** Total for Order" TO O-DESC.
             MOVE W25-V1             TO O-QNT.
             MOVE W25-V2             TO O-EXT.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE ZERO               TO W25-V1 W25-V2.

       GB25.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO WS-COMMAND.
             GO TO GB00.

       GB30.
             MOVE SPACES  TO W15-CONTACT.
             MOVE ZERO   TO WS-PAGE.
             PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO GB50.
             UNSTRING P-ADDR DELIMITED BY "*" INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
               OVERFLOW MOVE 1       TO WS-ERROR.
             MOVE P-TEL              TO W40-TEL.
             MOVE P-FAX              TO W40-FAX.

       GB35.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO GB50.
           IF NOT(ORD-CRED = P-NUMBER)
               GO TO GB50.
           IF (WS-PAGE = ZERO) OR (WS-IND = 1)
               PERFORM GB45.

       GB40.
             MOVE ORD-ORD            TO O2-ORD.
             MOVE "/"                TO O2-SL.
             MOVE ORD-LINE           TO O2-LINE.
             MOVE ORD-EXT-ITEM       TO O2-EXT-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-XREF           TO O2-XREF.
             MOVE STK-DESC           TO O2-DESC.
             MOVE ORD-QUANT          TO O2-QNT.
             MOVE ORD-UCOST          TO O2-COST.
             COMPUTE O2-EXT = ORD-QUANT * ORD-UCOST.
             MOVE ORD-DRATE          TO USE-NO.
             ADD ORD-QUANT           TO W25-V1.
             COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
           IF NOT(USE-NO = ZERO)
               PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
               MOVE USE-NAME         TO W15-NAME
               PERFORM BB010
               IF NOT(W10-NAME = W15-CONTACT)
             MOVE W10-NAME           TO O2-SMAN W15-CONTACT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               MOVE ZERO             TO WS-IND
           ELSE
             MOVE 99                 TO WS-ADVANCE
             PERFORM CALL-PRINTUTL
             MOVE 1                  TO WS-IND.
             MOVE SPACES             TO R-DET.
             PERFORM PRINT-ABORT.
           IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO R-DET
               MOVE SPACES           TO WS-COMMAND
               GO TO GB55.
             GO TO GB35.

       GB45.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE "OUTSTANDING ORDERS" TO REP-H1.
             PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE W60-COMPANY        TO REP-EXPNAME.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD1           TO REP-NADD.
           IF W05-VATNO NOT = SPACES
               MOVE "VAT No/BTW Nr :" TO REP-H2
               MOVE W05-VATNO        TO REP-GNO.
             MOVE WS-H3              TO O2-PAGE.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO O2-PGNO.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD2           TO REP-NADD.
           IF W95-TEL NOT = SPACES
               MOVE "TELEPHONE/FOON:" TO REP-H2
               MOVE W95-TEL          TO REP-TEL.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD3           TO REP-NADD.
             PERFORM CALL-PRINTUTL.
             MOVE W95-ADD4           TO REP-NADD.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-PC             TO REP-PC.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "DATE  :"          TO REP-H14.
             MOVE ORD-DATE           TO W15-YMD.
             MOVE W15-DY             TO W15-DD.
             MOVE W15-MT             TO W15-MM.
             MOVE W15-CN             TO W15-CC.
             MOVE W15-YR             TO W15-YY.
             MOVE W15-DATE           TO REP-DTE.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE P-NAME             TO REP-NAME.
             MOVE "SUPPLIER NUMBER : " TO REP-H4.
             MOVE ORD-CRED           TO REP-AC.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD1           TO REP-NAME.
             MOVE "SUPPLIER TEL.NO : " TO REP-H4.
             MOVE W40-TEL            TO REP-STEL.
             PERFORM CALL-PRINTUTL.
             MOVE W40-ADD2           TO REP-NAME.
             MOVE "SUPPLIER FAX.NO : " TO REP-H4.
             MOVE W40-FAX            TO REP-STEL.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD3           TO REP-NAME.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W40-ADD4           TO REP-NAME.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE P-PC               TO REP-PC1.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE WS-HDNG2           TO O2-HDNG.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

       GB50.
           IF WS-PAGE = ZERO
               MOVE "No outstanding orders" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GB00.
             MOVE SPACES             TO R-DET.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "**** Total for Supplier" TO O-DESC.
             MOVE W25-V1             TO O-QNT.
             MOVE W25-V2             TO O-EXT.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE ZERO               TO W25-V1 W25-V2.

       GB55.
             MOVE SPACES             TO R-DET
             MOVE 99                 TO WS-ADVANCE
             PERFORM CALL-PRINTUTL
             GO TO GB00.

       GB999.
             EXIT.

      /
       PORDER-LOOKUP  SECTION 5.
       PORDER-LUP-SCREEN.
             PERFORM SAVE-SCREEN.
             DISPLAY S-POR.

       PORDER-LUP-CLR-SCREEN.
             DISPLAY S-DET.
             MOVE 8                  TO SHADE-ROW.
             MOVE 9                  TO SHADE-COL.
             MOVE 68                 TO SHADE-WIDTH.
             MOVE 36                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             PERFORM CLEAR-L50.
             MOVE 11                 TO CLIN.
             MOVE 1                  TO W75-S W75-S1.

       PORDER-LUP-TAB-LOOP.
             MOVE ZEROES  TO W75-POR (W75-S1).
           IF W75-S1 < 32
               ADD 1                 TO W75-S1
               GO TO PORDER-LUP-TAB-LOOP.
             MOVE ZERO               TO W75-S1.
             MOVE P-NUMBER           TO ORD-CRED.
             PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               GO TO PORDER-NO-RECORD.

       PORDER-GET-ORD-RECORD.
           IF WS-OPTION = "J"
               MOVE SPACE            TO WS-OPTION
               GO TO PORDER-SKIP-ORD-READ.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               SUBTRACT 1            FROM W75-S
               GO TO PORDER-FULL-SCREEN.
           IF NOT(ORD-CRED = P-NUMBER)
               SUBTRACT 1            FROM W75-S
               GO TO PORDER-FULL-SCREEN.

       PORDER-SKIP-ORD-READ.
             MOVE ORD-KEY            TO W75-CODE (W75-S).
             MOVE ORD-EXT-ITEM       TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               SUBTRACT 1            FROM W75-S
               GO TO PORDER-GET-ORD-RECORD.
             DISPLAY SD-POR AT LINE CLIN.
           IF CLIN < 42
               ADD 1                 TO CLIN W75-S
               GO TO PORDER-GET-ORD-RECORD.

       PORDER-FULL-SCREEN.
            DISPLAY " "                                     AT 5003 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                         " highlight - "                                  WITH FOREGROUND-COLOR Grey
                                      "PgUp"                              WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                          "/"                             WITH FOREGROUND-COLOR Grey
                                           "PgDn"                         WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                               " - "                      WITH FOREGROUND-COLOR Grey
                                                  "ENTER"                 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                       " select - "       WITH FOREGROUND-COLOR Grey
                                                                 "Esc"    WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                                                    "ape" WITH FOREGROUND-COLOR Grey.

             PORDER-SAVE-FULL-SCREEN.
             PERFORM SAVE-SCREEN-2.
             MOVE 0                  TO W75-S1.
             MOVE 0910               TO CPOS.

       PORDER-CRT-ATTRIB.
             MOVE 3920               TO W41-LENGTH.
             CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS W42-ATTRIB2 W41-LENGTH RETURNING WS-STATUS.
             MOVE 4000               TO W41-LENGTH.

       PORDER-GET-USER-OPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             PERFORM CLEAR-L50.
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY    GO TO PORDER-ADJ-NO-KEYS
                 WHEN DOWN-KEY  GO TO PORDER-CHK-NO-KEYS
                 WHEN ENTER-KEY
                 WHEN CR-KEY    GO TO PORDER-CHK-SELECT
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO PORDER-GET-USER-OPT
           ELSE
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY   MOVE SPACES TO W15-ACNO
                                GO TO PORDER-RESTORE-SCREEN
                 WHEN PAGE-DOWN GO TO PORDER-GET-NEXT-PAGE
                 WHEN PAGE-UP   GO TO PORDER-GET-PREV-PAGE
                 WHEN OTHER     PERFORM AA900-ALARM
               END-EVALUATE
               GO TO PORDER-GET-USER-OPT.
             PERFORM AA900-ALARM.
             GO TO PORDER-GET-USER-OPT.

       PORDER-GET-NEXT-PAGE.
           IF W75-S < 12
               GO TO PORDER-GET-USER-OPT.

       PORDER-NEW-PAGE.
             MOVE 0909               TO CPOS.
             DISPLAY S-DET.
             MOVE 11                 TO CLIN.
             MOVE LOW-VALUES         TO W75-KEYS.
             MOVE 1                  TO W75-S W75-S1.
             PERFORM PORDER-LUP-TAB-LOOP.
             GO TO PORDER-GET-ORD-RECORD.

       PORDER-GET-PREV-PAGE.
             MOVE 1                  TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
             MOVE "J"                TO WS-OPTION.

       PORDER-GET-PREV-LOOP.
             PERFORM READ-PORDER-PREV THRU READ-PORDER-EXIT.
           IF WS-F-ERROR = 16
               MOVE SPACE            TO WS-OPTION
               GO TO PORDER-NEW-PAGE.
           IF WS-IXS1 < 24
               ADD 1                 TO WS-IXS1
               GO TO PORDER-GET-PREV-LOOP.
             GO TO PORDER-NEW-PAGE.

       PORDER-NO-RECORD.
            DISPLAY "No outstandig orders - Press " AT 5012 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                 "ANY"      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                    " key"  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             GO TO PORDER-RESTORE-SCREEN.

       PORDER-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1            FROM W75-S1 CLIN.
               GO TO PORDER-HIGHLIGHT.

       PORDER-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1                TO W75-S1
               GO TO PORDER-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1                 TO W75-S1 CLIN.

       PORDER-HIGHLIGHT.
             PERFORM PORDER-CRT-ATTRIB.
             DISPLAY W43-DET2 (CLIN) AT CPOS WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey.
             GO TO PORDER-GET-USER-OPT.

       PORDER-CHK-SELECT.
           IF W75-S1 = ZERO
               GO TO PORDER-GET-USER-OPT.
             MOVE CSTART             TO WS1-CURSOR.
             MOVE W75-CODE(W75-S1)   TO ORD-KEY.
             PERFORM READ-PORDER THRU READ-PORDER-EXIT.
             MOVE ZERO               TO COL-AND-LINE.
             MOVE ORD-KEY            TO W15-ORD.
             GO TO PORDER-RESTORE-SCREEN.

       PORDER-RESTORE-SCREEN.
            PERFORM RESTORE-SCREEN.

       PORDER-LOOKUP-EXIT.
             EXIT.

       GC000      SECTION 5.
       GC00.
             DISPLAY "Creditor" AT 0812.
             DISPLAY "A"           AT 1012 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                      "ll or " "S"         WITH FOREGROUND-COLOR Brown HIGHLIGHT
                                "elective".
             MOVE "A"                TO WS-SEQ.

       GC02.
             ACCEPT WS-SEQ AT 1029
             WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             CALL "CBL_TOUPPER" USING WS-SEQ BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-SEQ = "A" OR "S")
               GO TO GC02.
             DISPLAY WS-BLNK78 AT 1002 WITH FOREGROUND-COLOR Cyan.
             MOVE ZERO   TO W25-V1 W25-V2 W25-TV1 W25-TV2.
           IF WS-SEQ = "S"
               GO TO GC16.
             MOVE "     !"           TO ORD-CRED.
            PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
          IF NOT(WS-F-ERROR = ZERO)
              GO TO GC20.
             MOVE SPACES             TO W15-ACNO.
             MOVE ZERO               TO WS-IND.

       GC05.
             PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
           IF NOT(WS-F-ERROR = ZERO)
      *    *************************************************************
      *    ****         PERFORM Print the Supplier Totals.          ****
      *    *************************************************************
               PERFORM GC06
               MOVE SPACES           TO R-DET
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     If printing all suppliers, print the grand totals.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF WS-SEQ = "A"
                   PERFORM GC10
                   MOVE SPACES       TO R-DET
                   MOVE WS-HDNG2     TO O2-HDNG
                   MOVE 0            TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL
                   MOVE 2            TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL
                   MOVE SPACES       TO R-DET
                   MOVE "**** Total purchase orders" TO O2-DESC
                   MOVE W25-TV1      TO O2-QNT
                   MOVE W25-TV2      TO O2-EXT
                   MOVE 0            TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL
                   MOVE 99           TO WS-ADVANCE
                   PERFORM CALL-PRINTUTL
               END-IF
               GO TO GC999.
           IF ORD-CRED = W15-ACNO
               GO TO GC15.

       GC06.
    IF NOT(W15-ACNO = SPACES)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If printing all suppliers, accumulate grand totals.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        IF WS-SEQ = "A"
     ADD W25-V1  TO W25-TV1
     ADD W25-V2  TO W25-TV2
        END-IF
      *    *************************************************************
      *    ****       Print the Supplier Totals      ****
      *    *************************************************************
              MOVE SPACES  TO R-DET
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE "**** Supplier Total"
     TO O2-DESC
        MOVE W25-V1  TO O2-QNT
        MOVE W25-V2  TO O2-EXT
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE ZERO  TO W25-V1 W25-V2
    END-IF.

       GC07.
    IF WS-SEQ = "S"
              MOVE SPACES  TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        GO TO GC16.
    IF W15-ACNO NOT = SPACES
              MOVE SPACES  TO R-DET
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.

       GC08.
      MOVE SPACES  TO W15-CONTACT.
      MOVE ZERO   TO WS-PAGE.
             MOVE ORD-CRED       TO P-NUMBER W15-ACNO.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
      DISPLAY P-NUMBER AT 0822
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta "°"
       P-NAME WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            UNSTRING P-ADDR DELIMITED BY "*"
           INTO W40-ADD1
                W40-ADD2
                W40-ADD3
                W40-ADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.

       GC10.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE "OUTSTANDING ORDERS" TO REP-H1.
             PERFORM CALL-PRINTUTL.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE W60-COMPANY        TO REP-EXPNAME.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD1           TO REP-NADD.
           IF W05-VATNO NOT = SPACES
               MOVE "VAT No/BTW Nr :" TO REP-H2
               MOVE W05-VATNO        TO REP-GNO.
             MOVE WS-H3              TO O2-PAGE.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO O2-PGNO.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD2           TO REP-NADD.
           IF W95-TEL NOT = SPACES
               MOVE "TELEPHONE/FOON:" TO REP-H2
               MOVE W95-TEL          TO REP-TEL.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-ADD3           TO REP-NADD.
             PERFORM CALL-PRINTUTL.
             MOVE W95-ADD4           TO REP-NADD.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W95-PC             TO REP-PC.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "E"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE "DATE  :"          TO REP-H14.
             MOVE ORD-DATE           TO W15-YMD.
             MOVE W15-DY             TO W15-DD.
             MOVE W15-MT             TO W15-MM.
             MOVE W15-CN             TO W15-CC.
             MOVE W15-YR             TO W15-YY.
             MOVE W15-DATE           TO REP-DTE.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE 0                  TO WS-ADVANCE.
             MOVE "X"                TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.

       GC12.
            MOVE P-NAME  TO REP-NAME.
      MOVE "SUPPLIER NUMBER : "
     TO REP-H4.
            MOVE ORD-CRED  TO REP-AC.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W40-ADD1  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
            MOVE W40-ADD2  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W40-ADD3  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W40-ADD4  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
            MOVE P-PC   TO REP-PC1.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
      MOVE WS-HDNG2  TO O2-HDNG.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
      MOVE ZERO   TO WS-IND.

       GC15.
    IF WS-IND = 1
        PERFORM GC10 THRU GC12.
      MOVE ORD-ORD  TO O2-ORD.
      MOVE "/"   TO O2-SL.
      MOVE ORD-LINE  TO O2-LINE.
      MOVE ORD-ITEM  TO O2-EXT-ITEM STK-CODE.
      PERFORM READ-STOCK THRU READ-STOCK-EXIT.
      MOVE STK-DESC  TO O2-DESC.
      MOVE ORD-QUANT  TO O2-QNT.
      MOVE ORD-UCOST  TO O2-COST.
      COMPUTE O2-EXT = ORD-QUANT * ORD-UCOST.
      ADD ORD-QUANT  TO W25-V1.
      COMPUTE W25-V2 = W25-V2 + (ORD-QUANT * ORD-UCOST).
      MOVE ORD-DRATE  TO USE-NO.
    IF NOT(USE-NO = ZERO)
        PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
        MOVE USE-NAME  TO W15-NAME
        PERFORM BB010
        IF NOT(W10-NAME = W15-CONTACT)
     MOVE W10-NAME TO O2-SMAN W15-CONTACT.
    IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE ZERO  TO WS-IND
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE 1   TO WS-IND.
      MOVE SPACES  TO R-DET.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DET
        MOVE SPACES  TO WS-COMMAND
              GO TO GC25.
      GO TO GC05.

       GC16.
      MOVE SPACES  TO W15-ACNO.
      DISPLAY "Esc" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to EXIT".

       GC17.
      ACCEPT W15-ACNO AT 0821
      WITH BACKGROUND-COLOR Magenta
    FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO GC999
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO GC17
        END-EVALUATE.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
      MOVE W15-ACNO  TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
    IF NOT(WS-F-ERROR = ZERO)
        MOVE "No account for code selected" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO GC17.
      MOVE W15-ACNO  TO ORD-CRED.
            PERFORM START-AT-ORD-CRED THRU READ-PORDER-EXIT.
          IF NOT(WS-F-ERROR = ZERO)
              GO TO GC18.
            PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
    IF NOT(W15-ACNO = ORD-CRED)
        GO TO GC18.
      MOVE ZERO   TO WS-IND.
      GO TO GC08.

       GC18.
      MOVE "No outstanding orders for account" TO WS-ERR-MES
            PERFORM ERROR-MESSAGE.
      GO TO GC17.

       GC20.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "******  END OF REPORT  ******" TO R-DET.

       GC25.
      MOVE 99   TO WS-ADVANCE.
      MOVE "E"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.

       GC999.
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
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-TELEPHONE      TO W95-TEL.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1       TO W95-ADD1.
             MOVE PAR-ADDRESS2       TO W95-ADD2.
             MOVE PAR-ADDRESS3       TO W95-ADD3.
             MOVE PAR-ADDRESS4       TO W95-ADD4.
             MOVE PAR-POST-CDE       TO W95-POST.
           IF W95-ADD4 = SPACES
               MOVE W95-PC           TO W95-ADD4
               MOVE SPACES           TO W95-PC.
           IF W95-ADD3 = SPACES
               MOVE W95-ADD4         TO W95-ADD3
               MOVE SPACES           TO W95-ADD4.
           IF W95-ADD2 = SPACES
               MOVE W95-ADD3         TO W95-ADD2
               MOVE SPACES           TO W95-ADD3.
           IF W95-ADD1 = SPACES
               MOVE W95-ADD2         TO W95-ADD1
               MOVE SPACES           TO W95-ADD2.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-REG  TO W05-VATNO.
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             PERFORM CHANGE-TIME.
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
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
