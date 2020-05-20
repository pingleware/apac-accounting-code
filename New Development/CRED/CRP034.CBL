      $set LINKCOUNT"384" GNT"CRP034.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******       ***    *
      *   **    **  **    **  **    **   **  **   **    **     * **    *
      *   **        **   **   **    **  **    **        **    *  **    *
      *   **        ******    *******   **    **     ****    *   **    *
      *   **        **   **   **        **    **        **  ********   *
      *   **    **  **    **  **         **  **   **    **       **    *
      *    ******   **    **  **          ****     ******        **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   C R E D I T O R / G L   P O S T   D A T E D   C H E Q U E S  *
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
       PROGRAM-ID.      CRP034.
       AUTHOR.          James William Lemmon.
       DATE-WRITTEN.    MARCH 1984.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2017
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

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       COPY "LEDTRF.SL".

       COPY "PARAM.SL".

       COPY "PDCHEQ.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

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

       COPY "GACCNT.FD".

       COPY "GLTRAN.FD".

       COPY "LEDTRF.FD".

       COPY "PARAM.FDE".

       COPY "PDCHEQ.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

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
       77  WS-IXS                  PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KSTORE               PIC  9(04)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GLJSTR               PIC  9(04)    COMP-5.
       77  WS-GLJKEY               PIC  9(04)    COMP-5.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-CHEQUE               PIC  9(04)    COMP-5.
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-LINES                PIC  9(04)    COMP-5 VALUE 0.
       77  WS-S                    PIC  9(04)    COMP-5.
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-Y-END                PIC  9(02)           VALUE ZERO.
       77  WS-L-END                PIC  9(04)           VALUE ZERO.
       77  WS-OPEN                 PIC  9(02)           VALUE ZERO.
       77  WS-MS1                  PIC  9(04)    COMP-5.
       77  WS-MS2                  PIC  9(04)    COMP-5.
       77  WS-VALUE                PIC S9(09)V99 COMP-3.
       77  WS-PERIOD               PIC  9(02)    COMP-5 VALUE ZERO.
       77  WS-DEL                  PIC  X(01).
       77  WS-XFRC                 PIC  X(01)           VALUE "N".
       77  WS-NUM                  PIC  Z9.
       77  WS-UPDATE               PIC  9(01).
       77  WS-CONREC               PIC  9(01)           VALUE ZERO.
       77  WS-GLCON                PIC  9(01)           VALUE ZERO.
       77  WS-HCRED                PIC  X(09)           VALUE "CREDITOR".
       77  WS-HDTE                 PIC  X(05)           VALUE "DATE".
       77  WS-HACNO                PIC  X(06)           VALUE "A/C NO".
       77  WS-HNAME                PIC  X(04)           VALUE "NAME".
       77  WS-HAGE                 PIC  X(12)           VALUE "AGE 30 - 180".
       77  WS-HCHQ                 PIC  X(06)           VALUE "CHEQUE".
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-LOOK                 PIC  X(01)           VALUE "C".
       77  WS-FIRST                PIC  9(02)           VALUE 0.
       77  WS-LAST                 PIC  9(02)           VALUE 0.
       77  WS-CHAR1                PIC  X(01).
       77  WS-ERROR                PIC  9(01)           VALUE ZERO.
       77  WS-KEY1                 PIC  9(04)    COMP-5.
       77  WS-ENT                  PIC  X(08).
       77  WS-ENT1                 PIC  X(08).
       77  WS-BATCH                PIC  9(05)    COMP-3.
       77  WS-TNEXT                PIC  9(04)    COMP-5.
       77  WS-H1                   PIC  X(06)           VALUE "DATE:".
       77  WS-H3                   PIC  X(07)           VALUE "BATCH:".
       77  WS-H4                   PIC  X(07)           VALUE "PAGE:".
       77  WS-ER1                  PIC  X(08)           VALUE "Account".
       77  WS-ER3                  PIC  X(05)           VALUE "Date".
       77  WS-ER4                  PIC  X(06)           VALUE "Value".
       77  WS-ER5                  PIC  X(07)           VALUE "Cheq no".
       77  WS-ER6                  PIC  X(11)           VALUE "No record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP034".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR/GL - POST DATED CHEQUES".
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
           03  WS-PROG             PIC  X(03)           VALUE "034".

       COPY "LEDTRF.WS".

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

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(03).
          03  W02-PRN-LENGTH  PIC  9(03).
    03  W02-LINAGE      PIC  9(03).
    03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *     C  =  Condensed print
      *     E  =  Expanded print
      *     H  =  Header line
      *     X  =  Cancel expanded print
      *     1  =  10 Characters per inch
      *     2  =  12 Characters per inch
      *     3  =  17 Characters per inch
      *     6  =  6 Lines per inch
      *     8  =  8 Lines per inch
      *
    03  W02-PRN-TYPE    PIC  X(01).
    03  W02-PRN-LINE    PIC  X(136).

    03  R-SL1 REDEFINES W02-PRN-LINE.
        05  R-SLA       PIC  X(78).
        05  FILLER      PIC  X(58).

    03  R-SL2 REDEFINES W02-PRN-LINE.
        05  FILLER      PIC  X(58).
        05  R-SLB       PIC  X(78).

    03  R-L1 REDEFINES W02-PRN-LINE.
        05  R-DET    PIC X(140).

          03  R-L2 REDEFINES W02-PRN-LINE.
              05  R-H1        PIC X(07).
              05  R-DTE1      PIC Z9/99/9999.
              05  FILLER      PIC X(31).
              05  R-H2        PIC X(40).
              05  FILLER      PIC X(33).
              05  R-H3        PIC X(08).
              05  R-PGE       PIC ZZ9.

          03  R-L2A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(56).
              05  R-HEAD      PIC X(20).
              05  FILLER      PIC X(56).

          03  R-L3A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(05).
              05  I-H1        PIC X(11).
              05  I-H2        PIC X(34).
              05  I-H3        PIC X(44).
              05  I-H4        PIC X(11).
              05  I-H5        PIC X(27).

          03  R-L4 REDEFINES W02-PRN-LINE.
              05  G-H1        PIC X(10).
              05  G-SUPP      PIC X(05).
              05  FILLER      PIC X(02).
              05  G-NAMER     PIC X(40).
              05  FILLER      PIC X(23).

          03  R-L4A REDEFINES W02-PRN-LINE.
              05  G-H3        PIC X(05).
              05  G-INVN      PIC Z(07)9.
              05  FILLER      PIC X(01).
              05  G-H4        PIC X(06).
              05  FILLER      PIC X(01).
              05  G-INVD      PIC Z9/99/9999.
              05  G-H5        PIC X(06).
              05  G-RAIL      PIC Z(04)9.99-.
              05  FILLER      PIC X(01).
              05  G-H6        PIC X(06).
              05  G-DISC      PIC Z(04)9.99-.
              05  FILLER      PIC X(01).
              05  G-H7        PIC X(06).
              05  G-TOTAL     PIC Z(06)9.99-.

          03  R-L5A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(05).
              05  I-ACNO      PIC 9(04)/99.
              05  FILLER      PIC X(04).
              05  I-NAME      PIC X(30).
              05  FILLER      PIC X(04).
              05  I-NAR       PIC X(36).
              05  FILLER      PIC X(02).
              05  I-VALUE     PIC Z(08)9.99-.
              05  FILLER      PIC X(04).
              05  I-30        PIC X(04).
              05  I-60        PIC X(04).
              05  I-90        PIC X(04).
              05  I-120       PIC X(04).
              05  I-150       PIC X(04).
              05  I-180       PIC X(07).

          03  R-L6 REDEFINES W02-PRN-LINE.
              05  D-H1        PIC X(06).
              05  D-H2        PIC X(10).
              05  D-H3        PIC X(08).
              05  D-H4        PIC X(10).
              05  D-H5        PIC X(08).
              05  D-H6        PIC X(13).
              05  D-H7        PIC X(13).
              05  D-H8        PIC X(12).

          03  R-L6A REDEFINES W02-PRN-LINE.
              05  D-ACNO      PIC X(06).
              05  FILLER      PIC X(02).
              05  D-NAME      PIC X(72).

          03  R-L6B REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(05).
              05  D-DNO       PIC Z(05)9.
              05  FILLER      PIC X(01).
              05  D-DNDTE     PIC Z9/99/9999.
              05  FILLER      PIC X(01).
              05  D-INV       PIC Z(07)9.
              05  FILLER      PIC X(01).
              05  D-IDTE      PIC Z9/99/9999.
              05  FILLER      PIC X(01).
              05  D-COST      PIC Z(06)9.99-.
              05  FILLER      PIC X(01).
              05  D-SELL      PIC Z(07)9.99-.
              05  FILLER      PIC X(01).
              05  D-30        PIC X(02).
              05  D-60        PIC X(02).
              05  D-90        PIC X(02).
              05  D-120       PIC X(02).
              05  D-150       PIC X(02).
              05  D-180       PIC X(02).

          03  R-L7 REDEFINES W02-PRN-LINE.
              05  CH-H1       PIC X(07).
              05  CH-H2       PIC X(32).
              05  CH-H3       PIC X(08).
              05  CH-H4       PIC X(13).
              05  CH-H5       PIC X(08).
              05  CH-H6       PIC X(12).

          03  R-L7A REDEFINES W02-PRN-LINE.
              05  CH-ACNO     PIC X(06).
              05  FILLER      PIC X(01).
              05  CH-NAME     PIC X(28).
              05  FILLER      PIC X(01).
              05  CH-DATE     PIC Z9/99/9999.
              05  FILLER      PIC X(01).
              05  CH-CHQ      PIC Z(05)9.
              05  FILLER      PIC X(01).
              05  CH-VAL      PIC Z(08)9.99-.
              05  FILLER      PIC X(01).
              05  CH-30       PIC X(02).
              05  CH-60       PIC X(02).
              05  CH-90       PIC X(02).
              05  CH-120      PIC X(02).
              05  CH-150      PIC X(02).
              05  CH-180      PIC X(02).

          03  R-L8 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(03).
              05  R-H5        PIC X(11).
              05  R-H6        PIC X(05).
              05  FILLER      PIC X(01).
              05  R-H7        PIC X(14).
              05  R-H8        PIC X(05).
              05  FILLER      PIC X(05).
              05  R-H9        PIC X(09).
              05  R-H10       PIC X(12).
              05  R-H11       PIC X(13).
              05  R-H12       PIC X(10).
              05  R-H13       PIC X(13).
              05  FILLER      PIC X(08).
              05  R-H14       PIC X(14).
              05  R-H15       PIC X(09).

          03  R-L9 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(03).
              05  R-ACC       PIC 9(04)/99.
              05  FILLER      PIC X(04).
              05  R-ANME      PIC X(24).
              05  FILLER      PIC X(02).
              05  R-DTE2      PIC Z9/99/9999.
              05  FILLER      PIC X(03).
              05  R-REF       PIC Z(08).
              05  FILLER      PIC X(04).
              05  R-NAR       PIC X(36).
              05  FILLER      PIC X(03).
              05  R-DBT       PIC Z(08)9.99.
              05  FILLER      PIC X(02).
              05  R-CRD       PIC Z(08)9.99-.
              05  FILLER      PIC X(01).

          03  R-L10 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC X(65).
              05  R-H16       PIC X(06).
              05  FILLER      PIC X(01).
              05  R-H17       PIC X(23).
              05  R-H18       PIC X(06).
              05  FILLER      PIC X(31).

          03  R2-L2 REDEFINES W02-PRN-LINE.
              05  R2-H1       PIC X(06).
              05  R2-DTE1     PIC Z9/99/9999.
              05  FILLER      PIC X(08).
              05  R2-H2       PIC X(40).
              05  R2-H2A      PIC X(27).
              05  FILLER      PIC X(10).
              05  R2-H3       PIC X(07).
              05  R2-BTCH     PIC Z(04)9.
              05  FILLER      PIC X(10).
              05  R2-H4       PIC X(06).
              05  R2-PGE      PIC ZZ9.

          03  R2-L3 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R2-H5       PIC  X(11).
              05  R2-H6       PIC  X(05).
              05  FILLER      PIC  X(01).
              05  R2-H7       PIC  X(13).
              05  R2-H8       PIC  X(05).
              05  FILLER      PIC  X(06).
              05  R2-H9       PIC  X(09).
              05  R2-H10      PIC  X(12).
              05  R2-H11      PIC  X(13).
              05  FILLER      PIC  X(01).
              05  R2-H12      PIC  X(09).
              05  R2-H13      PIC  X(13).
              05  FILLER      PIC  X(08).
              05  R2-H14      PIC  X(14).
              05  R2-H15      PIC  X(09).

          03  R2-L4 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R2-ACC      PIC  9(04)/99.
              05  FILLER      PIC  X(04).
              05  R2-ANME     PIC  X(24).
              05  FILLER      PIC  X(03).
              05  R2-DTE2     PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
              05  R2-REF      PIC  Z(07).
              05  FILLER      PIC  X(04).
              05  R2-NAR      PIC  X(36).
              05  FILLER      PIC X(01).
        05  R2-DBRKO    PIC X(01).
              05  R2-DBT      PIC Z(08)9.99.
        05  R2-DBRKC    PIC X(01).
        05  R2-CBRKO    PIC X(01).
              05  R2-CRD      PIC Z(08)9.99-.
        05  R2-CBRKC    PIC X(01).
              05  FILLER      PIC X(01).

    03  PD-L1 REDEFINES W02-PRN-LINE.
        05  PD-H1.
     07  PD-H2   PIC  X(07).
     07  PD-H3   PIC  X(41).
        05  PD-H4.
     07  PD-H5   PIC  X(07).
     07  PD-H6   PIC  X(07).
     07  PD-H7.
       09 PD-H8  PIC  X(43).
       09 PD-H9  PIC  X(07).
       09 PD-H10 PIC  X(15).
       09 PD-H11 PIC  X(05).

    03  PD-L2 REDEFINES W02-PRN-LINE.
        05  PD-ACNO     PIC  X(06).
        05  FILLER      PIC  X(01).
        05  PD-NAME     PIC  X(40).
        05  FILLER      PIC  X(01).
        05  PD-BANK     PIC  9(06).
        05  FILLER      PIC  X(01).
        05  PD-CRED     PIC  9(06).
        05  FILLER      PIC  X(01).
        05  PD-NAR      PIC  X(38).
        05  FILLER      PIC  X(01).
        05  PD-DATE     PIC  Z9/99/9999.
        05  FILLER      PIC  X(01).
        05  PD-REF      PIC  Z(06)9.
        05  FILLER      PIC  X(01).
        05  PD-VALUE    PIC  Z(08)9.99.

       COPY "W05.WS".

       01  W10-H2.
           03  FILLER          PIC X(27).
           03  W10-HEAD        PIC X(13).

       COPY "W15.CGL".

       01  W20-TOTALS.
           03  W20-DEBIT       PIC S9(09)V99 COMP-3.
           03  W20-CREDIT      PIC S9(09)V99 COMP-3.
           03  W20-VAT         PIC S9(09)V99 COMP-3.
           03  W20-RESULT      PIC S9(09)V99.
           03  W20-RES1        REDEFINES W20-RESULT.
               05  W20-WHOLE   PIC 9(09).
               05  W20-DEC     PIC S9(02).
           03  W20-TOTAL       PIC S9(09)V99 COMP-3.
           03  W20-VALUE       PIC S9(09)V99 COMP-3.
    03  W20-DATE.
        05  W20-CC      PIC 99.
               05  W20-YY      PIC 99.
               05  W20-MM      PIC 99.
               05  W20-DD      PIC 99.
           03  W20-DTE REDEFINES W20-DATE
          PIC 9(08).
    03  W21-DATE.
        05  W21-CC      PIC 99.
        05  W21-YY      PIC 99.
        05  W21-MM      PIC 99.
        05  W21-DD      PIC 99.
    03  W21-DTE REDEFINES W21-DATE
          PIC 9(08).

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(05).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
       01  W25-EDIT.
           03  W25-3V2.
               05  W25-2       PIC ZZ9.99.
           03  W25-7V2.
               05  W25-3       PIC Z(06)9.99.
           03  W25-3N.
               05  W25-4       PIC ZZ9.
           03  W25-DATE.
              05  W25-DTE     PIC 99/99/9999.
           03  W25-6.
               05  W25-NO      PIC Z(07)9.
           03  W25-5V2.
               05  W25-VAL     PIC Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL    PIC Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2    PIC Z(06)9.99DB.
          03  W25-S8V2.
              05  W25-VG1     PIC Z(07)9.99CR.
          03  W25-72.
               05  W25-VAL3    PIC Z(06)9.99-.
           03  W25-REF.
               05  W25-REFER   PIC Z(07).

       01  W25-VALUES.
           03  W25-CFLOW  OCCURS 6 PIC S9(07)V99  COMP-3.
           03  W25-V1          PIC S9(07)V99  COMP-3.
           03  W25-V2          PIC S9(07)V99  COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
          03  W50-HEAD        PIC X(22).

       01  W55-DEL.
           03  W55-MNTH        PIC X(03).
           03  FILLER          PIC X(01)   VALUE ",".
           03  W55-YY          PIC 9(02).

       01  W60-H2.
           03  W60-DESC        PIC X(21)   VALUE
                                   "CREDITORS AUDIT TRIAL".

       01  W65-CREDITOR.
           03  W65-CFLOW OCCURS 6 PIC 9(07)V99 COMP-3.

       01  W70-ACTS.
           03  W70-BRANCH      PIC 9(04)     COMP-5.

       COPY "W70CRED.WS".

       01  W90-COMP.
           03  W90-CNAME       PIC X(40).

       01  W90-TRAN.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.

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

       01  W300-CHEQUE.
    03  W300-CHQDTE     PIC  9(06)    COMP.
    03  W300-CHQNO      PIC  9(08)    COMP.
    03  W300-PAYEE      PIC  X(36).
    03  W300-NAR        PIC  X(36).
    03  W300-CHQVAL     PIC S9(09)V99 COMP.

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
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Brown
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR Cyan
     FOREGROUND-COLOR Blue
     VALUE " to Exit".

       01  S-PROG.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR Grey
     HIGHLIGHT VALUE
               "CREDITORS G/LEDGER POST DATED CHEQUES".

       01  S-MENU.
    03  LINE  4 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³  ".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "PD CHEQUE OPTIONS".
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "  ³".
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Cheques Issued    ³".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Amend Cheque      ³".
          03  LINE  8 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Cancel Cheque     ³".
          03  LINE  9 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Post Cheques Due  ³".
          03  LINE 10 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE " Exit              ³".
    03  LINE 11 COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Black
     VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S06.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC X(06) USING W15-ACNO AUTO.

       01  S09.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC 9(06) USING W15-ACCOUNT AUTO.

       01  S14.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC Z(06)9 USING W15-GRNNO AUTO.

       01  S15.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC Z9/99/9999 USING W15-GRND AUTO.

       01  S16.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC Z(07)9.99DB
                                       USING W15-TCOST AUTO.

       01  S17.
          03  FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Magenta
      PIC Z(07)9.99DB
                                       USING W15-CHQVAL AUTO.

       01  S20.
    02  S20A.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
           VALUE "ISSUE PD CHEQUE".
    02  S20B.
          03  LINE  4 COLUMN  4 VALUE "Creditor Ac.no.:".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(05) FROM WS-SPC.
    03        COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(40) FROM WS-SPC.
          03  LINE  5 COLUMN  4 VALUE "Cheque Date    :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(08) FROM WS-SPC.
          03  LINE  6 COLUMN  4 VALUE "       Number  :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(06) FROM WS-SPC.
          03  LINE  7 COLUMN  4 VALUE "       Value   :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(13) FROM WS-SPC.
          03  LINE  9 COLUMN  4 VALUE "G/L Bank no.   :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM WS-SPC.
          03  LINE 10 COLUMN  4 VALUE "Narrative      :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(36) FROM WS-SPC.
          03  LINE 11 COLUMN  4 VALUE "G/L Creditors  :".
    03        COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(06) FROM WS-SPC.
    03        COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM WS-SPC.

       01  S21.
    03  LINE  4 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(06) USING W15-ACNO AUTO.
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(40) FROM P-NAME.
    03  LINE  5 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z9/99/9999 USING W15-GRND AUTO.
    03  LINE  6 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z(06)9 USING W15-GRNNO AUTO.
    03  LINE  7 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  Z(07)9.99DB
                USING W15-TCOST AUTO.
    03  LINE  9 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9(06) USING W15-BANK AUTO.
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM W15-BNKNME.
    03  LINE 10 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(36) USING W15-NAR AUTO.
    03  LINE 11 COLUMN 21 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  9(06) USING W15-CRED AUTO.
    03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
     PIC  X(30) FROM W15-ACNME.

       01  S22.
          03  LINE  8 COLUMN  4 VALUE "Allocation - 30:".
    03        COLUMN 24 VALUE "60:".
    03        COLUMN 31 VALUE "90:".
    03        COLUMN 38 VALUE "120:".
    03        COLUMN 46 VALUE "150:".
    03        COLUMN 54 VALUE "180:".

       01  S55.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
               05  LINE 50 COLUMN  2 VALUE "F1"                                           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  4 VALUE   "=Help,".
               05          COLUMN 10 VALUE         "F2"                                   FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 12 VALUE           " to do Creditor Lookup ".
               05          COLUMN 35 VALUE                                  "Esc"         FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 38 VALUE                                     " to exit".

      *COPY "CRDLUP.CRT".

      *COPY "LEDLUP.CRT".

       01  S-CRED.
    03  LINE 23 COLUMN  4 VALUE "Balance :".
    03        COLUMN 14 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
                                 PIC Z(08)9.99DB USING P-BAL.
    03        COLUMN 30 VALUE "Due :".
    03        COLUMN 36 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta
                                 PIC Z(08)9.99DB USING P-OUT.

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
       AA000        SECTION.
       AA00.
          IF LS0-CRLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA11E.
             PERFORM ZA000.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *  *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             PERFORM HA000.

       AA11.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE GACCNT GLTRAN LEDTRF.

       AA11E.
             EXIT PROGRAM.

       COPY "AA20CG.LUP".

       AA50.
      *
      *    ****    Print the audit trail HEADING
      *
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
            MOVE WS-H1   TO R-H1.
            MOVE W90-CNAME  TO R-H2.
            MOVE WS-H4   TO R-H3.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO R-PGE.
            MOVE W05-TODAY  TO R-DTE1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W60-DESC  TO R-H2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACE   TO R-DET.

       AA60.
            MOVE W50-H4  TO R-HEAD.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       AA65.
             EXIT.

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

       COPY "CREDITOR.TRN".

       COPY "PRINTUTL.AS4".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000        SECTION.

       COPY "CONTROL.RD".

       COPY "CRTRAN.RD".

       COPY "CREDIT.RD".

       COPY "CRDMEM.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD2".

       COPY "GLTRAN.RD".

       COPY "LEDTRF.RD1".

       COPY "PDCHEQ.RD".

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "LEDTRF.WR".

       COPY "PDCHEQ.WR".

       COPY "TXTRAN.WR".

      /
       AE155-GET-CREDITOR    SECTION.
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

       COPY "CREDIT.ILU".

       COPY "APAC.HLP".

      /
      *    THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *    DATE UP TO THE CURRENT MONTH
      *
       AH000           SECTION.
       AH00.
           IF WS-PERIOD = W05-MM
               GO TO AH999.
           IF WS-PERIOD > 12
               SUBTRACT 13 FROM WS-PERIOD GIVING WS-S3
               PERFORM AH10
        ADD 1 WS-OPEN  GIVING WS-MS1
           ELSE
        MOVE WS-PERIOD  TO WS-MS1.
       AH05.
             ADD WS-VALUE        TO G-VAL (WS-MS1).
             ADD 1               TO WS-MS1.
           IF WS-MS1 > 12
               MOVE 1            TO WS-MS1.
           IF WS-MS1 = W05-MM
               GO TO AH999.
             GO TO AH05.
       AH10.
             ADD 1               TO WS-S3.
           IF WS-S3 > 12
               MOVE 1            TO WS-S3.
             ADD WS-VALUE        TO G-LVAL (WS-S3).
           IF WS-S3 NOT = WS-OPEN
               GO TO AH10.
       AH15.
             EXIT.
       AH999.
             EXIT.

       COPY "GACCNT.LUP".

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
      *        ³ 0 = RECORD CHANGED          ÄÄ      (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED        ÄÄ      (DELETE)     ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ      (WRITE)      ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY000        SECTION.
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       AY01.
             MOVE 01                 TO REC-FILE.
             MOVE WS-PARKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1        TO REC-PARAM.
             GO TO AY50.
      *    *************************************************************
      *    ****              C R E D I T O R   F I L E              ****
      *    *************************************************************
       AY07.
             MOVE 07                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
             MOVE P-REC              TO REC-CREDIT.
             GO TO AY50.
      *    *************************************************************
      *    ****  C R E D I T O R   T R A N S A C T I O N   F I L E  ****
      *    *************************************************************
       AY08.
             MOVE 08                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE CT-REC             TO REC-CRTRAN.
             GO TO AY50.

       AY12.
             MOVE 12             TO REC-FILE.
            MOVE ZERO   TO REC-KEY REC-TYPE.
      PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
             MOVE G-REC          TO REC-GACCNT.
             GO TO AY50.

       AY13.
             MOVE 13             TO REC-FILE.
            MOVE ZERO   TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
            MOVE T-REC   TO REC-GLTRAN.
             GO TO AY50.
      *    *************************************************************
      *    ****       V A L U E   A D D E D   T A X   F I L E       ****
      *    *************************************************************
       AY19.
             MOVE 19                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE.
             MOVE TAX-RECORD1        TO REC-TXTRAN.
             GO TO AY50.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
       AY39.
             MOVE 39                 TO REC-FILE.
             MOVE WS-NETKEY          TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             MOVE NET-RECORD         TO REC-NETWORK.
             GO TO AY50.
      *    *************************************************************
      *    ****           I N T E G R A T I O N   F I L E           ****
      *    *************************************************************
       AY40.
             MOVE 40                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE WS-ACTION          TO REC-TYPE
             MOVE XFR-REC            TO REC-LEDTRF.
             GO TO AY50.

       AY44.
            MOVE 44   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE PDT-REC  TO REC-PDCHEQ.
             GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              End transaction in recovery log              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AY49.
             MOVE 99                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE SPACES             TO REC-DETAIL.
             PERFORM AY50.
             ADD 1                   TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 195 - Clear (Close and Open    ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-RECOVER > 195
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
               DISPLAY "Write error Recovery file - Status " AT 2212 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               DISPLAY WS-STATUS                             AT 2247 WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
             MOVE 1                  TO WS-COUNT.
             MOVE 5                  TO WS-SHARED.
             PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK          TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1                TO WS-SUB
               MOVE ZERO             TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
      *
             DISPLAY "WAITING" AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
             COMMIT.
             ACCEPT WS-STIME FROM TIME.
             MOVE 400                TO WS-WAIT.
             PERFORM LOCK-REC-LOOP.
             DISPLAY SPACE AT 5051 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
      *
      *    ****    Updating Creditor and General Ledger Files
      *
             MOVE "CG"               TO PAR-PROG(WS-USUB).
             MOVE LS-USER            TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 2                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-CREDITOR       TO W70-CREDCON.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
      *
       AY62.
           IF NOT(PAR-PROG(WS-SUB) = SPACES OR PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   C R E D I T O R   O R   G / L   F I L E S   
      *           B E I N G   U P D A T E D
      *
               IF NOT(PAR-PROG(WS-SUB) = SPACES)
                   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG" OR "GL"
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
      *
      *    ****    End transaction
      *
       AY70.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 2.
      *
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE 2                  TO WS-NETKEY.
             MOVE W70-CREDCON        TO NET-CREDITOR.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
             MOVE 1                  TO WS-USUB.

       AY72.
           IF NOT(PAR-USR(WS-USUB) = LS-USER)
               ADD 1                 TO WS-USUB
               GO TO AY72.

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
            SUBTRACT 1 FROM WS-USUB  GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            COMMIT.

       AY999.
             EXIT.
      /
      *    ****    D A T E   V A L I D A T I O N
      *
       BA000         SECTION.
       BA00.
            MOVE ZERO   TO WS-ERROR.
           IF W15-DD < 01 OR > 31
              GO TO BA10.
           IF W15-MM < 01 OR > 12
              GO TO BA10.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
              GO TO BA15.
           IF W15-MM = 02 AND W15-DD > 28
               GO TO BA05.
           IF W15-DD > 30
              GO TO BA10.
            GO TO BA15.

       BA05.
           IF W15-DD > 29
              GO TO BA10.
            DIVIDE W15-CCYY BY 4
     GIVING W20-RESULT.
          IF W20-DEC > ZERO
              GO TO BA10.
            GO TO BA15.

       BA10.
            MOVE 1   TO WS-ERROR.
      GO TO BA999.
      *
      *   ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *
       BA15.
      MOVE W15-MM  TO WS-PERIOD.
    IF W15-CCYY > W05-CY
        GO TO BA10.
    IF W15-CCYY < W05-CY
        GO TO BA20.
    IF W05-MM = W15-MM
       GO TO BA999.
    IF W15-MM > W05-MM
        GO TO BA10.
    IF W15-MM > WS-Y-END
        GO TO BA999.
    IF W15-CCYY > WS-L-END
        GO TO BA999.
      ADD 12   TO WS-PERIOD
      GO TO BA999.

       BA20.
    IF W15-CCYY > WS-L-END
        GO TO BA999.
      ADD 12   TO WS-PERIOD.

       BA999.
             EXIT.

       CA600         SECTION.
       CA601.
             MOVE CT-CODE        TO W90-CODE.
             PERFORM GET-CREDITOR-TRAN-RECORD.
             SUBTRACT CT-COST    FROM W00-C-VAL
                                      W00-C-MTDV
                                      W00-C-YTDV.
             ADD 1               TO W00-C-DAY
                                    W00-C-MTD
                                    W00-C-YTD.
             PERFORM UPDATE-TRAN-RECORD.
       CA699.
             EXIT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    GL-TRF-UPDATE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ GENERAL LEDGER:            ³
      *    ³ This routine is used to UPDATE the INTEGRATION RECORDS    ³
      *    ³ for posting to the G/L ACCOUNTS.          ³
      *    ³              ³
      *    ³ THE KEY FOR EACH OF THE RECORDS IS:         ³
      *    ³              ³
      *    ³ RUN DATE (CCYYMMDD)  - WX-YMD          ³
      *    ³              ³
      *    ³ TYPE - WX-TYPE            ³
      *    ³  01 : DEBTORS           ³
      *    ³  02 : CREDITORS           ³
      *    ³  03 : BAD DEBTS           ³
      *    ³  04 : SALES           ³
      *    ³  05 : SALES (RETURNS)          ³
      *    ³  06 : PURCHASES           ³
      *    ³  07 : PURCHASES (RETURNS)         ³
      *    ³  08 : COST-OF-SALES          ³
      *    ³  09 : COST-OF-SALES (RETURNS)         ³
      *    ³  10 : OUTPUT VAT (DEBTORS)         ³
      *    ³  11 : INPUT VAT (DEBTORS)         ³
      *    ³  12 : OUTPUT VAT (CREDITORS)         ³
      *    ³  13 : INPUT VAT (CREDITORS)         ³
      *    ³  14 : DEBTOR TRANSACTIONS         ³
      *    ³              ³
      *    ³ TRANSACTION CODE / SALES LEDGER NO - WX-LEDG        ³
      *    ³              ³
      *    ³ OTHER DETAILS ARE:            ³
      *    ³              ³
      *    ³ ACCOUNT NUMBER -  WX-AC           ³
      *    ³              ³
      *    ³ NARRATIVE - WX-NAR           ³
      *    ³              ³
      *    ³ TRANSACTION VALUE - WX-VAL          ³
      *    ³              ³
      *    ³ The routine will first read the file using the DETAILS    ³
      *    ³ ABOVE as the KEY. If the record exists the routine WILL   ³
      *    ³ UPDATE the record. If no record exists a NEW record WILL  ³
      *    ³ be CREATED.            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "LEDTRF.UPD".

      /
      *    ****    P O S T   D A T E D  C H E Q U E S
      *
       HA000        SECTION 5.
       HA00.
             PERFORM ERASE-SCREEN.
       HA01.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
             PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.
      MOVE 5   TO SHADE-ROW.
      MOVE 31   TO SHADE-COL.
      MOVE 21   TO SHADE-WIDTH.
      MOVE 7   TO SHADE-LINES.
      DISPLAY S-MENU.
      DISPLAY MENU-INSTRUCT.
      PERFORM SCREEN-SHADOW.
       HA02.
      MOVE 5   TO TOP-ROW.
      MOVE 10   TO BOTTOM-ROW.
      MOVE 4   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 19   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       HA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       HA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO HA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO HA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO HA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO HA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO HA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO HA15
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO HA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO HA18
   WHEN OTHER  PERFORM AA900-ALARM
        END-EVALUATE
        GO TO HA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        IF WS-OPTION < 0 OR > 4
     PERFORM AA900-ALARM
     GO TO HA05
        END-IF
        GO TO HA16
    ELSE
        PERFORM AA900-ALARM
        GO TO HA05.

       HA15.
    IF SCREEN-LIN < 5
        PERFORM AA900-ALARM
        GO TO HA05
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
        MOVE "0"   TO WS-OPTION.
    GO TO HA18.

       HA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 9   TO SCREEN-LIN
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
        MOVE 8   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       HA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
              GO TO HA99.
           IF WS-OPTION = "1"
              PERFORM HB000
           ELSE
           IF WS-OPTION = "2"
              PERFORM HC000
           ELSE
           IF WS-OPTION = "3"
              PERFORM HD000
           ELSE
           IF WS-OPTION = "4"
              PERFORM HE000.
            GO TO HA01.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Audit trail headings.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HA50.
            MOVE SPACES  TO R-DET.
            MOVE WS-H1   TO R-H1.
            MOVE W90-CNAME  TO R-H2.
            MOVE WS-H4   TO R-H3.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO R-PGE.
            MOVE W05-TODAY  TO R-DTE1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      STRING W60-DESC DELIMITED SIZE
      " - " DELIMITED SIZE
      W50-HEAD DELIMITED SIZE
        INTO PD-H4.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE "CREDITOR ACCOUNT"
     TO PD-H1.
      MOVE "G/L ACCOUNTS" TO PD-H4.
      MOVE "CHEQUE DETAILS"
     TO PD-H7.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE "NUMB."  TO PD-H2.
            MOVE "NAME"  TO PD-H3.
            MOVE "BANK"  TO PD-H5.
            MOVE "CREDIT"  TO PD-H6.
            MOVE "NARRATIVE"  TO PD-H8.
      MOVE "DATE"  TO PD-H9.
            MOVE "CHEQ NO"  TO PD-H10.
            MOVE "VALUE"  TO PD-H11.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       HA99.
      EXIT.
       HA100        SECTION 5.
       HA102.
            MOVE ZERO TO WS-ERROR.
           IF W15-DD < 01 OR > 31
              GO TO HA110.
           IF W15-MM < 01 OR > 12
              GO TO HA110.
           IF W15-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
              GO TO HA999.
           IF W15-MM = 02 AND W15-DD > 28
              GO TO HA105.
           IF W15-DD > 30
              GO TO HA110.
            GO TO HA999.

       HA105.
           IF W15-DD > 29
              GO TO HA110.
            DIVIDE W15-CCYY BY 4
     GIVING W20-RESULT.
          IF W20-DEC > ZERO
              GO TO HA110.
            GO TO HA999.

       HA110.
             MOVE 1 TO WS-ERROR.

       HA999.
             EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Data capture Post dated cheques/checks.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB000        SECTION 5.
       HB00.
            MOVE "PD. CHEQUES ISSUED"
                                 TO W50-HEAD.
             MOVE WX-BNKGL       TO W15-BANK.
             MOVE WX-CREGL       TO W15-CRED.
      *    *************************************************************
      *    ****   C L E A R   T O T A L S   T O   Z E R O
      *    *************************************************************
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Audit trail headings.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB02.
      PERFORM HA50.

       HB05.
            PERFORM ERASE-SCREEN.
            DISPLAY S20.
      *    *************************************************************
      *    ****   I N I T I A L I Z E W O R K   V A R I A B L E S
      *    *************************************************************
            MOVE ZERO   TO W15-GRND W15-TCOST
        W15-GRNNO.
             MOVE SPACES         TO W15-ACNO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB10.
      DISPLAY S55.
            ACCEPT S06 AT 0421.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACE  TO W15-ACNO
       GO TO HB65
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB10
        END-EVALUATE
        DISPLAY S06 AT 0421
               IF W15-ACNO = SPACES
           GO TO HB10.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
              GO TO HB65.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM AE155-GET-CREDITOR.
           IF NOT(WS-ERROR = ZERO)
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HB10.
            MOVE P-NAME  TO W15-NAME.
            DISPLAY P-NAME AT 0428
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             DISPLAY S-CRED.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HB10.

       HB15.
            ACCEPT S15 AT 0521.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HB10
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB15
        END-EVALUATE.
            MOVE W15-GRND  TO W15-DATE.
            PERFORM HA100.
           IF WS-ERROR = 1
        MOVE WS-ER3  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HB15.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
      MOVE W15-DD  TO W20-DD.
             MOVE W15-MM         TO W20-MM.
      MOVE W15-CC  TO W20-CC.
             MOVE W15-YY         TO W20-YY.
    IF W20-DTE NOT > W05-TYMD
        DISPLAY "Not a post dated cheque" AT 2302
         WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Brown HIGHLIGHT
         " Correct date or press "
         WITH BACKGROUND-COLOR Cyan
         "Esc" WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
         "ape" WITH BACKGROUND-COLOR Cyan
              GO TO HB15.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Post dated Cheque/Check number.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB20.
            ACCEPT S14 AT 0621.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HB15
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB20
        END-EVALUATE.
            MOVE W15-GRNNO  TO W15-REFER.
          IF W15-GRNNO = ZERO
              GO TO HB20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Transaction value         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB25.
            ACCEPT S16 AT 0721.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HB20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB25
        END-EVALUATE.
            MULTIPLY -1 BY W15-TCOST
     GIVING W15-CHQVAL.
            DISPLAY S17 AT 0721.
           IF W15-TCOST = ZERO
              GO TO HB25.
            MOVE SPACES  TO W15-NAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         Bank account number         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB30.
             MOVE W15-BANK       TO W15-ACCOUNT.
      DISPLAY S55.
            ACCEPT S09 AT 0921.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HB25
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB30
        END-EVALUATE
        DISPLAY S09 AT 0921
               IF W15-ACCOUNT = ZERO
           GO TO HB30.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HB30.
      *    *************************************************************
      *    ****      Save Bank account number      ****
      *    *************************************************************
            MOVE W15-ACCOUNT  TO W15-BANK.
      *    *************************************************************
      *    ****      Read Bank account record      ****
      *    *************************************************************
       HB35.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HB30.
            DISPLAY G-NAME AT 0928
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       HB40.
            PERFORM CHECK-CORRECT.

       HB45.
          IF WS-OPTION = "N"
              GO TO HB30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Transaction narrative         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HB50.
            ACCEPT W15-NAR AT 1021
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HB30
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB50
        END-EVALUATE.
          IF W15-NAR = SPACES
              GO TO HB50.
            MOVE W15-CRED  TO W15-ACCOUNT.

       HB55.
      DISPLAY S55.
            ACCEPT S09 AT 1121.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HB50
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HB55
        END-EVALUATE
        DISPLAY S09 AT 1121
               IF W15-ACCOUNT = ZERO
           GO TO HB55.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              GO TO HB55.
      *    *************************************************************
      *    ****  Read Creditor control account record     ****
      *    *************************************************************
            MOVE W15-ACCOUNT  TO W15-CRED.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HB55.
            DISPLAY G-NAME AT 1128
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       HB60.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HB55.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Generate the PD Cheque/Check record.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      INITIALIZE PDT-REC.
      MOVE W15-ACNO  TO PD-ACNO PDT-AC.
      MOVE P-NAME  TO PD-NAME.
      MOVE W20-DTE  TO PDT-DATE.
      MOVE W15-GRND  TO PD-DATE.
      MOVE W15-REFER  TO PD-REF PDT-REFER.
      MOVE W15-BANK  TO PD-BANK PDT-BANK.
      MOVE W15-CRED  TO PD-CRED PDT-CRED.
      MOVE W15-NAR  TO PD-NAR PDT-NAR.
      MOVE W15-CHQVAL  TO PD-VALUE PDT-VAL.
      ADD W15-CHQVAL  TO W20-TOTAL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start processing transaction - Lock the control records. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      PERFORM AY60 THRU AY999.
      PERFORM WRITE-PDCHEQ THRU WRITE-PDCHEQ-EXIT.
    IF WS-F-ERROR = 44
        MOVE "Cheque duplicated" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO HB10.
      MOVE 1   TO WS-ACTION.
      PERFORM AY44 THRU AY50.
      PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Print the PD Cheque/Check audit trail.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM HA50.
      MOVE SPACES  TO R-DET.

       HB65.
            MOVE "ENTER to continue - E to exit" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
              GO TO HB05.
      MOVE SPACES  TO R-DET.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****  TOTAL ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PD-VALUE.
      PERFORM CALL-PRINTUTL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *    *************************************************************
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
       HB999.
      EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Amend Post dated cheques/checks.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       HC000        SECTION 5.
       HC00.
            MOVE "PD. CHEQUES AMENDED"
     TO W50-HEAD.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.

       HC02.
      PERFORM HA50.

       HC05.
            PERFORM ERASE-SCREEN.
            DISPLAY S20.
      DISPLAY "AMEND PD CHEQUE" AT 0233
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      *    *************************************************************
      *    ****   I N I T I A L I Z E W O R K   V A R I A B L E S
      *    *************************************************************
            MOVE ZERO   TO W15-GRND
        W15-GRNNO.
      MOVE SPACES  TO W15-ACNO.

       HC10.
      DISPLAY "Key in the cheque number" AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Post dated Cheque/Check number.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            ACCEPT S14 AT 0621.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC70
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC10
        END-EVALUATE.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
            MOVE W15-GRNNO  TO W15-REFER PDT-REFER.
          IF W15-GRNNO = ZERO
              GO TO HC70.
      PERFORM START-AT-PDCHEQ-REF THRU READ-PDCHEQ-EXIT.
    IF WS-F-ERROR = ZERO
        GO TO HC20.
       HC15.
      MOVE "Not on file"  TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO HC10.
       HC20.
      PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (W15-GRNNO < PDT-REFER)
        GO TO HC15.
      *    *************************************************************
      *    ****    Switch date to DDMMCCYY format
      *    *************************************************************
      MOVE PDT-DATE  TO W21-DTE.
      MOVE W21-CC  TO W15-CC.
      MOVE W21-YY  TO W15-YY.
      MOVE W21-MM  TO W15-MM.
      MOVE W21-DD  TO W15-DD.
      *    *************************************************************
      *    ****    Save PD Cheque/Check details.      ****
      *    *************************************************************
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNNO.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-TCOST.
             PERFORM AE155-GET-CREDITOR.
            MOVE W15-CRED  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-ACNME.
            MOVE W15-BANK  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-BNKNME.
      DISPLAY S21.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO HC05.
       HC22.
            ACCEPT S15 AT 0521.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC05
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC22
        END-EVALUATE.
            MOVE W15-GRND  TO W15-DATE.
            PERFORM HA100.
           IF WS-ERROR = 1
        MOVE WS-ER3  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HC22.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
      MOVE W15-DD  TO W20-DD.
             MOVE W15-MM         TO W20-MM.
      MOVE W15-CC  TO W20-CC.
             MOVE W15-YY         TO W20-YY.
    IF W20-DTE NOT > W05-TYMD
        DISPLAY "Not a post dated cheque" AT 2302
         WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Brown HIGHLIGHT
         " Correct date or press "
         WITH BACKGROUND-COLOR Cyan
         "Esc" WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT
         "ape" WITH BACKGROUND-COLOR Cyan
              GO TO HC22.

       HC25.
            ACCEPT S14 AT 0621.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC22
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC25
        END-EVALUATE.
            MOVE W15-GRNNO  TO W15-REFER.
          IF W15-GRNNO = ZERO
              GO TO HC25.

       HC30.
            ACCEPT S16 AT 0721.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC25
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC30
        END-EVALUATE.
    IF NOT(W15-TCOST < ZERO)
              MULTIPLY -1 BY W15-TCOST
     GIVING W15-CHQVAL
    ELSE
        MOVE W15-TCOST  TO W15-CHQVAL.
            DISPLAY S17 AT 0721.
           IF W15-TCOST = ZERO
              GO TO HC30.

       HC35.
             MOVE W15-BANK       TO W15-ACCOUNT.
      DISPLAY S55.
            ACCEPT S09 AT 0921.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HC30
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC35
        END-EVALUATE
        DISPLAY S09 AT 0921
               IF W15-ACCOUNT = ZERO
           GO TO HC35.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HC35.
             MOVE W15-ACCOUNT    TO W15-BANK.
       HC40.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HC35.
            DISPLAY G-NAME AT 0928
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       HC45.
            PERFORM CHECK-CORRECT.
       HC50.
          IF WS-OPTION = "N"
              GO TO HC35.
       HC55.
            ACCEPT W15-NAR AT 1021
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC35
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC55
        END-EVALUATE.
          IF W15-NAR = SPACES
              GO TO HC55.
            MOVE W15-CRED  TO W15-ACCOUNT.
       HC60.
      DISPLAY S55.
            ACCEPT S09 AT 1121.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HC55
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC60
        END-EVALUATE
        DISPLAY S09 AT 1121
               IF W15-ACCOUNT = ZERO
           GO TO HC60.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              GO TO HC60.
            MOVE W15-ACCOUNT  TO W15-CRED.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO HC60.
            DISPLAY G-NAME AT 1128
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
       HC65.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HC60.
      PERFORM AY60 THRU AY999.
    IF (W20-DTE NOT = PDT-DATE) OR
       (W15-ACNO NOT = PDT-AC) OR
       (W15-REFER NOT = PDT-REFER)
        PERFORM DELETE-PDCHEQ-REC THRU WRITE-PDCHEQ-EXIT
        INITIALIZE PDT-REC
        MOVE "Y"   TO WS-DEL
     ELSE
        MOVE "N"   TO WS-DEL.
      MOVE W15-ACNO  TO PD-ACNO PDT-AC.
      MOVE P-NAME  TO PD-NAME.
      MOVE W20-DTE  TO PDT-DATE.
      MOVE W15-GRND  TO PD-DATE.
      MOVE W15-REFER  TO PD-REF PDT-REFER.
      MOVE W15-BANK  TO PD-BANK PDT-BANK.
      MOVE W15-CRED  TO PD-CRED PDT-CRED.
      MOVE W15-NAR  TO PD-NAR PDT-NAR.
      MOVE W15-CHQVAL  TO PD-VALUE PDT-VAL.
      ADD W15-CHQVAL  TO W20-TOTAL.
    IF WS-DEL = "Y"
        PERFORM WRITE-PDCHEQ THRU WRITE-PDCHEQ-EXIT
        MOVE 1   TO WS-ACTION
    ELSE
        PERFORM REWRITE-PDCHEQ THRU WRITE-PDCHEQ-EXIT
        MOVE 0   TO WS-ACTION.
    IF WS-F-ERROR = 44
        MOVE "Cheque duplicated" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO HC22.
      PERFORM AY44 THRU AY50.
      PERFORM AY70 THRU AY999.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM HA50.
      MOVE SPACES  TO R-DET.
       HC70.
            MOVE "ENTER to continue - E to exit" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
              GO TO HC05.
      MOVE SPACES  TO R-DET.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****  TOTAL ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PD-VALUE.
      PERFORM CALL-PRINTUTL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.

       HC999.
      EXIT.
      /
       HD000        SECTION 5.
       HD00.
            MOVE "PD. CHEQUES DELETED"
                                 TO W50-HEAD.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.

       HD02.
      PERFORM HA50.

       HD05.
            PERFORM ERASE-SCREEN.
            DISPLAY S20.
      DISPLAY "DELETE PD CHEQUE" AT 0233
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            MOVE ZERO   TO W15-GRND
        W15-GRNNO.
      MOVE SPACES  TO W15-ACNO.

       HD10.
      DISPLAY "Key in the cheque number" AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT S14 AT 0621.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HD70
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HD10
        END-EVALUATE.
      PERFORM CLEAR-L50.
            MOVE W15-GRNNO  TO W15-REFER PDT-REFER.
          IF W15-GRNNO = ZERO
              GO TO HD70.
      PERFORM START-AT-PDCHEQ-REF THRU READ-PDCHEQ-EXIT.
    IF WS-F-ERROR = ZERO
        GO TO HD20.
       HD15.
      MOVE "Not on file"  TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO HD10.
       HD20.
      PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (W15-GRNNO < PDT-REFER)
        GO TO HD15.
      *
      *    ****    Switch date to DDMMCCYY format
      *
      MOVE PDT-DATE  TO W21-DTE.
      MOVE W21-CC  TO W15-CC.
      MOVE W21-YY  TO W15-YY.
      MOVE W21-MM  TO W15-MM.
      MOVE W21-DD  TO W15-DD.
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNNO.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-TCOST.
            MULTIPLY -1 BY W15-TCOST
     GIVING W15-CHQVAL.
            PERFORM AE155-GET-CREDITOR.
            MOVE W15-CRED  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-ACNME.
            MOVE W15-BANK  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-BNKNME.
      DISPLAY S21.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO HD05.
      MOVE W15-ACNO  TO PD-ACNO.
      MOVE P-NAME  TO PD-NAME.
      MOVE W15-GRND  TO PD-DATE.
      MOVE W15-REFER  TO PD-REF.
      MOVE W15-BANK  TO PD-BANK.
      MOVE W15-CRED  TO PD-CRED.
      MOVE W15-NAR  TO PD-NAR.
      MOVE W15-CHQVAL  TO PD-VALUE.
      ADD W15-CHQVAL  TO W20-TOTAL.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM HA50.
      MOVE SPACES  TO R-DET.
      PERFORM DELETE-PDCHEQ-REC THRU WRITE-PDCHEQ-EXIT.

       HD70.
            MOVE "ENTER to continue - E to exit" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
              GO TO HD05.
      MOVE SPACES  TO R-DET.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****  TOTAL ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PD-VALUE.
      PERFORM CALL-PRINTUTL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.

       HD999.
      EXIT.
      /
       HE000       SECTION 5.
       HE0.
            MOVE "PD. CHEQUES PROCESSED"
                                 TO W50-HEAD.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.
       HE00B.
             PERFORM AA50 THRU AA60.
             MOVE WS-HACNO       TO CH-H1.
             MOVE WS-HNAME       TO CH-H2.
             MOVE WS-HDTE        TO CH-H3.
             MOVE WS-HCHQ        TO CH-H4.
             MOVE "VALUE"        TO CH-H5.
             MOVE WS-HAGE        TO CH-H6.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE WS-HACNO       TO R-H5.
             MOVE ALL "_"        TO R-H6 R-H8 R-H16 R-H18.
             MOVE "ACCOUNT NAME" TO R-H7.
             MOVE WS-HDTE        TO R-H9.
             MOVE "CHEQ NO"      TO R-H10.
             MOVE "PAYEE/NARRATIVE"
                                 TO R-H17.
             MOVE "DEBIT"        TO R-H14.
             MOVE "CREDIT"       TO R-H15.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       HE00.
             PERFORM ERASE-SCREEN.
            DISPLAY S20.
      DISPLAY "POST PD CHEQUES DUE" AT 0231
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY S22.

       HE02.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.
      INITIALIZE PDT-REC.
      PERFORM START-AT-PDCHEQ-KEY THRU READ-PDCHEQ-EXIT.
    IF WS-F-ERROR = 44
        GO TO HE145.

       HE05.
      PERFORM READ-PDCHEQ-NEXT THRU READ-PDCHEQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (PDT-DATE > W05-TYMD)
        GO TO HE135.
      MOVE PDT-DATE  TO W21-DTE W20-DTE.
      *
      *    ****    Switch date to DDMMCCYY format
      *
      MOVE W21-CC  TO W15-CC.
      MOVE W21-YY  TO W15-YY.
      MOVE W21-MM  TO W15-MM.
      MOVE W21-DD  TO W15-DD.
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNNO W15-REFER.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-CHQVAL.
            MULTIPLY -1 BY PDT-VAL
     GIVING W15-TCOST.
             PERFORM AE155-GET-CREDITOR.
            MOVE P-NAME  TO W15-NAME W15-PAYEE.
            MOVE W15-BANK  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-BNKNME.
            MOVE W15-CRED  TO W15-ACCOUNT.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
      MOVE G-NAME  TO W15-ACNME.
      DISPLAY S21.
      MOVE ZERO   TO W15-SETT.
    IF P-OUT NOT > ZERO
        IF P-30 > ZERO
     MOVE 1  TO W15-30
        ELSE
        IF P-60 > ZERO
     MOVE 1  TO W15-60
        ELSE
        IF P-90 > ZERO
     MOVE 1  TO W15-90
        ELSE
        IF P-120 > ZERO
     MOVE 1  TO W15-120
        ELSE
        IF P-150 > ZERO
     MOVE 1  TO W15-150
        ELSE
        IF P-180 > ZERO
     MOVE 1  TO W15-180.
            DISPLAY W15-30 AT 0821
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-60 AT 0828
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-90 AT 0835
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-120 AT 0843
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-150 AT 0851
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            DISPLAY W15-180 AT 0859
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
            PERFORM CLEAR-L50.
             MOVE 1              TO WS-S2.
             MOVE ZERO           TO WS-S.

       HE30.
             MOVE ZERO           TO W25-CFLOW (WS-S2).
           IF W15-MONTH (WS-S2) > ZERO
               ADD 1             TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
              GO TO HE30.
             MOVE ZERO           TO W25-V1 W25-V2.
             MOVE 1 TO WS-S3.
           IF WS-S < 2
               MOVE W15-CHQVAL   TO W25-V2
              GO TO HE35.
             MOVE W15-TCOST      TO W20-RESULT.
             DIVIDE W20-RESULT BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1               TO WS-S.
             MULTIPLY -1         BY W25-V1.
             MULTIPLY -1         BY W25-V2.

       HE35.
            DISPLAY "Processing payment" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
           IF WS-S = ZERO
              GO TO HE45.

       HE40.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
              GO TO HE40.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
              GO TO HE40.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V1 W25-V2.
      DISPLAY "Creditor" AT 5040
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT.

       HE45.
             PERFORM AY60 THRU AY999.
             MOVE 0              TO WS-ACTION.
             PERFORM AY07 THRU AY59.

       HE50.
      INITIALIZE CT-REC.
            MOVE W15-GRND  TO W15-DEL.
      MOVE W15-DEL-DD  TO W21-DD.
      MOVE W15-DEL-MM  TO W21-MM.
      MOVE W15-DEL-CC  TO W21-CC.
      MOVE W15-DEL-YY  TO W21-YY.
            MOVE W21-DTE  TO CT-DTE CT-SDTE.
            MOVE W15-GRNNO  TO CT-REF CT-SINV.
             MOVE P-NUMBER       TO CT-CRED.
             MOVE W15-CHQVAL     TO CT-COST.
             MOVE 03             TO CT-CODE.
      MOVE "U"   TO CT-TYPE.
      MOVE "P"   TO CT-PJ.
            MOVE ZERO   TO CT-PAID
        CT-SEQ
        CT-SELL
        CT-TAXFREE
        CT-TAX.
            MOVE 1   TO WS-S WS-S2.
           IF NOT(W25-V2 = ZERO)
              GO TO HE65.

       HE60.
           IF NOT(W15-MONTH(WS-S) = ZERO)
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               ADD 1             TO WS-S
               MULTIPLY 2        BY WS-S2
              GO TO HE60.

       HE65.
             ADD W15-CHQVAL      TO P-BAL.
             ADD W15-CHQVAL      TO P-DT.
             ADD W25-CFLOW (1)   TO P-30D.
             ADD W25-CFLOW (2)   TO P-60D.
             ADD W25-CFLOW (3)   TO P-90D.
             ADD W25-CFLOW (4)   TO P-120D.
             ADD W25-CFLOW (5)   TO P-150D.
             ADD W25-CFLOW (6)   TO P-180D.
             ADD W25-V2          TO P-OUT.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
      MOVE 1   TO WS-ACTION.
      PERFORM AY08 THRU AY59.
      PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
            ADD W15-CHQVAL  TO W70-CLOSE.
             ADD W15-CHQVAL      TO W70-DEBT.
             ADD W25-CFLOW (6)   TO W70-DUE-180.
             ADD W25-CFLOW (5)   TO W70-DUE-150.
             ADD W25-CFLOW (4)   TO W70-DUE-120.
             ADD W25-CFLOW (3)   TO W70-DUE-90.
             ADD W25-CFLOW (2)   TO W70-DUE-60.
             ADD W25-CFLOW (1)   TO W70-DUE-30.
      *
      *    ****   TRANSACTION STATISTICS
      *
             PERFORM CA600.
            MOVE W15-ACNO  TO CH-ACNO.
             MOVE W15-NAME       TO CH-NAME.
            MOVE W15-GRNNO  TO CH-CHQ.
             MOVE W15-GRND       TO CH-DATE.
             MOVE W15-CHQVAL     TO CH-VAL.
           IF W15-MONTH (1) > ZERO
               MOVE "Y"          TO CH-30.
           IF W15-MONTH (2) > ZERO
               MOVE "Y"          TO CH-60.
           IF W15-MONTH (3) > ZERO
               MOVE "Y"          TO CH-90.
           IF W15-MONTH (4) > ZERO
               MOVE "Y"          TO CH-120.
           IF W15-MONTH (5) > ZERO
               MOVE "Y"          TO CH-150.
           IF W15-MONTH (6) > ZERO
               MOVE "Y"          TO CH-180.
          IF W02-LINAGE < (W02-PRN-LENGTH - 1)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HE00B.
             MOVE SPACES         TO R-DET.
             MOVE W15-TCOST      TO W15-VAL.
             ADD W15-TCOST       TO W20-TOTAL.
             ADD W15-TCOST       TO W20-DEBIT.

       HE75.
            PERFORM CLEAR-L50.
            DISPLAY "Updating Creditor G/L" AT 5020 WITH
              BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       HE105.
      *
      *    ****    Switch date to CCYYMMDD format
      *
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
            MOVE W15-REFER  TO T-REF.
             MOVE G-AC           TO T-AC.
             MOVE 03             TO T-CODE.
             ADD W15-VAL         TO G-DT.
             MOVE W15-VAL        TO T-VAL.
             MOVE W15-VAL        TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W15-VAL         TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             MOVE W15-PAYEE      TO T-NARATIVE.
             MOVE 5              TO T-SUB.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

       HE110.
             MOVE W15-ACCOUNT    TO R-ACC.
             MOVE G-NAME         TO R-ANME.
             MOVE W15-DATE       TO R-DTE2.
             MOVE W15-REFER      TO R-REF.
             MOVE W15-NAR        TO R-NAR.
             MOVE W15-VAL        TO R-DBT.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HE00B.
             MOVE SPACES         TO R-DET.
       HE115.
            DISPLAY "Updating Bank account" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
            MOVE W15-BANK  TO G-AC.
             PERFORM AY12 THRU AY59.
            INITIALIZE T-REC.
       HE120.
      *
      *    ****    Switch date to CCYYMMDD format
      *
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
            MOVE W15-REFER  TO T-REF.
             ADD 1               TO G-GLTRAN.
             MOVE G-AC           TO T-AC.
             MOVE 04             TO T-CODE.
             MULTIPLY -1 BY W15-VAL
                                 GIVING W20-CREDIT.
             ADD W20-CREDIT      TO G-CR.
             MOVE W20-CREDIT     TO T-VAL.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
      ADD WS-VALUE  TO G-VAL(W05-MM).
             ADD W20-CREDIT      TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             MOVE W15-PAYEE      TO T-NARATIVE.
             MOVE 5              TO T-SUB.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.

       HE125.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HE00B.
             MOVE G-AC           TO R-ACC.
             MOVE G-NAME         TO R-ANME.
             MOVE W15-DATE       TO R-DTE2.
             MOVE W15-REFER      TO R-REF.
             MOVE W15-PAYEE      TO R-NAR.
             MOVE W20-CREDIT     TO R-CRD.
             PERFORM AY70 THRU AY999.
      PERFORM DELETE-PDCHEQ-REC THRU WRITE-PDCHEQ-EXIT.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM HE00B.
             MOVE SPACES TO R-DET.
       HE130.
            PERFORM CLEAR-L50.
      PERFORM HE00.
            GO TO HE05.
       HE135.
             MOVE "BATCH TOTAL"  TO R-NAR.
             MOVE W20-TOTAL      TO R-DBT.
             MULTIPLY -1 BY W20-TOTAL
                                 GIVING R-CRD.
       HE140.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
      GO TO HE999.

       HE145.
      MOVE "****    N O   P D   C H E Q U E S   D U E   ****"
     TO R-DET.
      GO TO HE140.
       HE999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000            SECTION 8.
       ZA000-START.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W05-TYMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W90-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

            OPEN I-O GACCNT.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 8 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN I-O GLTRAN.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 11 TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
           IF RUNTIME-ERROR
               IF FLE-LIMIT
     GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 40           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            OPEN OUTPUT RECOVER.
            CLOSE RECOVER.
            OPEN I-O RECOVER.

       ZA05.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
           IF WX-BNKGL = ZERO
              DISPLAY "Parameters not set up - ENTER" AT 2020
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
              ACCEPT WS-OPTION AT 2050
        WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               MOVE 1 TO WS-ERROR.
             MOVE 196            TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-C-ABR (1) = SPACES
               MOVE "Invoice (G/L)"
                                 TO PAR-C-DESC (1)
               MOVE "Inv"        TO PAR-C-ABR (1)
               MOVE 0            TO PAR-C-ACT (1)
               MOVE 11           TO PAR-C-CODE (1)
               MOVE "Debit Note (G/L)"
                                 TO PAR-C-DESC (2)
               MOVE "D/N"        TO PAR-C-ABR (2)
               MOVE 1            TO PAR-C-ACT (2)
               MOVE 12           TO PAR-C-CODE (2)
               PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
               ADD 1             TO WS-PARKEY
               PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT
               MOVE "Payment (G/L)"
                                 TO PAR-C-DESC (2)
               MOVE "Pmt"        TO PAR-C-ABR (2)
               MOVE 0            TO PAR-C-ACT (2)
               MOVE 14           TO PAR-C-CODE (2)
               PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      MOVE 4   TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH TO WS-BATCH.
           IF G-YEAR-END = ZERO
               MOVE 2 TO G-YEAR-END
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *    *************************************************************
      *    ****    S E T   U P  M O N T H S   ( Y E A R   E N D)
      *    *************************************************************
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
    IF WS-L-END = ZERO
        SUBTRACT 1 FROM W05-CY
     GIVING WS-L-END.
    IF G-OPEN = ZERO
        MOVE WS-Y-END  TO G-OPEN.
      MOVE G-OPEN  TO WS-OPEN.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999.
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
               WHEN 8          MOVE W02-GACCNT  TO WS-FILE
                               MOVE WS-GLKEY    TO WS-KEY
               WHEN 11         MOVE W02-GLTRAN  TO WS-FILE
                               MOVE T-KEY       TO WS-KEYX
               WHEN 15         MOVE WS-PARID    TO WS-FILE
                               MOVE WS-PARKEY   TO WS-KEY
               WHEN 18         MOVE W02-RECOVER TO WS-FILE
                               MOVE WS-RECKEY   TO WS-KEY
               WHEN 24         MOVE W02-CRDMEM  TO WS-FILE
                               MOVE CME-KEY     TO WS-KEYX
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN 44         MOVE W02-PDCHEQ  TO WS-FILE
                               MOVE PDT-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
