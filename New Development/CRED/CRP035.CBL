      $set LINKCOUNT"468" GNT"CRP035.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******    ********  *
      *   **    **  **    **  **    **   **  **   **    **   **        *
      *   **        **   **   **    **  **    **        **   **        *
      *   **        ******    *******   **    **     ****    *******   *
      *   **        **   **   **        **    **        **         **  *
      *   **    **  **    **  **         **  **   **    **   **    **  *
      *    ******   **    **  **          ****     ******     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   C R E D I T O R / G L   E L E C T R O N I C   P A Y M E N T  *
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
       PROGRAM-ID.       CRP035.
       AUTHOR.           James William Lemmon.
       DATE-WRITTEN.     MARCH 1984.

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
       77  WS-KSTORE               PIC  9(06)    COMP-5.
       77  WS-SKSTORE              PIC  9(06)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GLJSTR               PIC  9(04)    COMP-5.
       77  WS-GLJKEY               PIC  9(04)    COMP-5.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
       77  WS-EFT                  PIC  9(04)    COMP-5.
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
       77  WS-HEFT                 PIC  X(06)           VALUE "E.F.T.".
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
       77  WS-ER5                  PIC  X(07)           VALUE "Ref.Cde".
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP035".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR/GL - ELECTRONIC PAYMENT".
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
           03  WS-PROG             PIC  X(03)           VALUE "033".

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
        05  R-DET       PIC  X(140).

          03  R-L2 REDEFINES W02-PRN-LINE.
              05  R-H1        PIC  X(07).
              05  R-DTE1      PIC  Z9/99/9999.
              05  FILLER      PIC  X(31).
              05  R-H2        PIC  X(40).
              05  FILLER      PIC  X(33).
              05  R-H3        PIC  X(08).
              05  R-PGE       PIC  ZZ9.

          03  R-L2A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(56).
              05  R-HEAD      PIC  X(20).
              05  FILLER      PIC  X(56).

          03  R-L3A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(05).
              05  I-H1        PIC  X(11).
              05  I-H2        PIC  X(34).
              05  I-H3        PIC  X(44).
              05  I-H4        PIC  X(11).
              05  I-H5        PIC  X(27).

          03  R-L4 REDEFINES W02-PRN-LINE.
              05  G-H1        PIC  X(10).
              05  G-SUPP      PIC  X(05).
              05  FILLER      PIC  X(02).
              05  G-NAMER     PIC  X(40).
              05  FILLER      PIC  X(23).

          03  R-L4A REDEFINES W02-PRN-LINE.
        05  G-H3     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-INVN    PIC X(08).
              05  FILLER    PIC X(01).
        05  G-H4     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-INVD    PIC Z9/99/9999.
        05  FILLER    PIC X(01).
        05  G-H5     PIC X(05).
        05  FILLER    PIC X(01).
              05  G-RAIL    PIC Z(04)9.99-.
              05  FILLER    PIC X(01).
        05  G-H6     PIC X(04).
        05  FILLER    PIC X(01).
        05  G-DISC    PIC Z(04)9.99-.
        05  FILLER    PIC X(01).
        05  G-H6A    PIC X(04).
        05  FILLER    PIC X(01).
        05  G-VAT    PIC Z(05)9.99-.
        05  FILLER    PIC X(01).
        05  G-H7     PIC X(06).
        05  G-TOTAL    PIC Z(07)9.99-.

          03  R-L5A REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(05).
              05  I-ACNO      PIC  9(04)/99.
              05  FILLER      PIC  X(04).
              05  I-NAME      PIC  X(30).
              05  FILLER      PIC  X(04).
              05  I-NAR       PIC  X(36).
              05  FILLER      PIC  X(02).
              05  I-VALUE     PIC  Z(08)9.99-.
              05  FILLER      PIC  X(04).
              05  I-30        PIC  X(04).
              05  I-60        PIC  X(04).
              05  I-90        PIC  X(04).
              05  I-120       PIC  X(04).
              05  I-150       PIC  X(04).
              05  I-180       PIC  X(07).

          03  R-L6 REDEFINES W02-PRN-LINE.
              05  D-H1        PIC  X(06).
              05  D-H2        PIC  X(10).
              05  D-H3        PIC  X(08).
              05  D-H4        PIC  X(10).
              05  D-H5        PIC  X(08).
              05  D-H6        PIC  X(13).
              05  D-H7        PIC  X(13).
              05  D-H8        PIC  X(12).

          03  R-L6A REDEFINES W02-PRN-LINE.
              05  D-ACNO      PIC  X(05).
              05  FILLER      PIC  X(02).
              05  D-NAME      PIC  X(73).

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
              05  CH-H1       PIC  X(07).
              05  CH-H2       PIC  X(32).
              05  CH-H3       PIC  X(08).
              05  CH-H4       PIC  X(13).
              05  CH-H5       PIC  X(08).
              05  CH-H6       PIC  X(12).

          03  R-L7A REDEFINES W02-PRN-LINE.
              05  CH-ACNO     PIC  X(06).
              05  FILLER      PIC  X(01).
              05  CH-NAME     PIC  X(28).
              05  FILLER      PIC  X(01).
              05  CH-DATE     PIC  Z9/99/9999.
              05  FILLER      PIC  X(01).
              05  CH-EFT      PIC  X(07).
              05  FILLER      PIC  X(01).
              05  CH-VAL      PIC  Z(07)9.99-.
              05  FILLER      PIC  X(01).
              05  CH-30       PIC  X(02).
              05  CH-60       PIC  X(02).
              05  CH-90       PIC  X(02).
              05  CH-120      PIC  X(02).
              05  CH-150      PIC  X(02).
              05  CH-180      PIC  X(02).

          03  R-L8 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R-H5        PIC  X(11).
              05  R-H6        PIC  X(05).
              05  FILLER      PIC  X(01).
              05  R-H7        PIC  X(14).
              05  R-H8        PIC  X(05).
              05  FILLER      PIC  X(05).
              05  R-H9        PIC  X(09).
              05  R-H10       PIC  X(12).
              05  R-H11       PIC  X(13).
              05  R-H12       PIC  X(10).
              05  R-H13       PIC  X(13).
              05  FILLER      PIC  X(08).
              05  R-H14       PIC  X(14).
              05  R-H15       PIC  X(09).

          03  R-L9 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(03).
              05  R-ACC       PIC  9(04)/99.
              05  FILLER      PIC  X(04).
              05  R-ANME      PIC  X(24).
              05  FILLER      PIC  X(02).
              05  R-DTE2      PIC  Z9/99/9999.
              05  FILLER      PIC  X(03).
              05  R-REF       PIC  X(08).
              05  FILLER      PIC  X(04).
              05  R-NAR       PIC  X(36).
              05  FILLER      PIC  X(03).
              05  R-DBT       PIC  Z(08)9.99.
              05  FILLER      PIC  X(02).
              05  R-CRD       PIC  Z(08)9.99-.
              05  FILLER      PIC  X(01).

          03  R-L10 REDEFINES W02-PRN-LINE.
              05  FILLER      PIC  X(65).
              05  R-H16       PIC  X(06).
              05  FILLER      PIC  X(01).
              05  R-H17       PIC  X(23).
              05  R-H18       PIC  X(06).
              05  FILLER      PIC  X(31).

          03  R2-L2 REDEFINES W02-PRN-LINE.
              05  R2-H1       PIC  X(06).
              05  R2-DTE1     PIC  Z9/99/9999.
              05  FILLER      PIC  X(08).
              05  R2-H2       PIC  X(40).
              05  R2-H2A      PIC  X(27).
              05  FILLER      PIC  X(10).
              05  R2-H3       PIC  X(07).
              05  R2-BTCH     PIC  Z(04)9.
              05  FILLER      PIC  X(10).
              05  R2-H4       PIC  X(06).
              05  R2-PGE      PIC  ZZ9.

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
              05  R2-REF      PIC  X(07).
              05  FILLER      PIC  X(04).
              05  R2-NAR      PIC  X(36).
              05  FILLER      PIC  X(01).
        05  R2-DBRKO    PIC  X(01).
              05  R2-DBT      PIC  Z(08)9.99.
        05  R2-DBRKC    PIC  X(01).
        05  R2-CBRKO    PIC  X(01).
              05  R2-CRD      PIC  Z(08)9.99-.
        05  R2-CBRKC    PIC  X(01).
              05  FILLER      PIC  X(01).

    03  PD-L1 REDEFINES W02-PRN-LINE.
        05  PD-H1.
     07  PD-H2   PIC  X(06).
     07  PD-H3   PIC  X(41).
        05  PD-H4.
     07  PD-H5   PIC  X(07).
     07  PD-H6   PIC  X(07).
        05  PD-H7.
     07  PD-H8   PIC  X(43).
     07  PD-H9   PIC  X(07).
     07  PD-H10  PIC  X(15).
     07  PD-H11  PIC  X(06).

    03  PD-L2 REDEFINES W02-PRN-LINE.
        05  PD-ACNO     PIC  X(05).
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
        05  PD-REF      PIC  X(07).
        05  FILLER      PIC  X(01).
        05  PD-VALUE    PIC  Z(08)9.99.
        05  FILLER      PIC  X(01).

       COPY "W05.WS".

       01  W10-H2.
          03  FILLER        PIC  X(27).
          03  W10-HEAD        PIC  X(13).

       COPY "W15.CGL".

       01  W20-TOTALS.
           03  W20-DEBIT       PIC S9(09)V99 COMP-3.
           03  W20-CREDIT      PIC S9(09)V99 COMP-3.
           03  W20-VAT         PIC S9(09)V99 COMP-3.
           03  W20-RESULT      PIC S9(09)V99.
           03  W20-RES1        REDEFINES W20-RESULT.
              05  W20-WHOLE   PIC  9(09).
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
          03  W25-RESULT      PIC  9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
              05  W25-WHOLE   PIC  9(05).
              05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
        05  W25-KEY     PIC  9(04).
        05  W25-SUB     PIC  9(01).
        05  FILLER      PIC  9(02).

       01  W25-EDIT.
           03  W25-3V2.
              05  W25-2       PIC  ZZ9.99.
           03  W25-7V2.
              05  W25-3       PIC  Z(06)9.99.
           03  W25-3N.
              05  W25-4       PIC  ZZ9.
           03  W25-DATE.
              05  W25-DTE     PIC  99/99/9999.
           03  W25-6.
              05  W25-NO      PIC  Z(07)9.
           03  W25-5V2.
              05  W25-VAL     PIC  Z(04)9.99.
           03  W25-S5V2.
              05  W25-SVAL    PIC  Z(04)9.99DB.
           03  W25-S7V2.
              05  W25-VAL2    PIC  Z(06)9.99DB.
          03  W25-S8V2.
              05  W25-VG1     PIC  Z(07)9.99CR.
          03  W25-72.
              05  W25-VAL3    PIC  Z(06)9.99-.
           03  W25-REF.
              05  W25-REFER   PIC  Z(07).

       01  W25-VALUES.
           03  W25-CFLOW  OCCURS 6 PIC S9(07)V99  COMP-3.
           03  W25-V1          PIC S9(07)V99  COMP-3.
           03  W25-V2          PIC S9(07)V99  COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
          03  W50-HEAD        PIC  X(20).

       01  W55-DEL.
          03  W55-MNTH        PIC  X(03).
          03  FILLER        PIC  X(01)   VALUE ",".
          03  W55-YY        PIC  9(02).

       01  W60-H2.
          03  W60-DESC        PIC  X(21)   VALUE
                                   "CREDITORS AUDIT TRIAL".

       01  W65-CREDITOR.
           03  W65-CFLOW OCCURS 6 PIC 9(07)V99 COMP-3.

       01  W70-ACTS.
          03  W70-BRANCH      PIC  9(04)     COMP-5.

       COPY "W70CRED.WS".

       01  W90-COMP.
          03  W90-CNAME       PIC  X(40).

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

       01  S05.
          03  LINE  2 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
     VALUE "CREDITORS - ELECTRONIC PMT".
          03  LINE  4 COLUMN  4 VALUE "Creditor Ac.no.:".
          03  LINE  6 COLUMN 14 VALUE "Date".
          03        COLUMN 23 VALUE "Ref.Cde".
          03        COLUMN 35 VALUE "EFT value".
          03        COLUMN 49 VALUE "30".
    03        COLUMN 53 VALUE "60".
    03        COLUMN 57 VALUE "90".
    03        COLUMN 60 VALUE "120".
    03        COLUMN 64 VALUE "150".
    03        COLUMN 68 VALUE "180".

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
      PIC X(07) USING W15-GRNN AUTO.

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
             PERFORM FA000.

       AA11.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE GACCNT GLTRAN
     LEDTRF.

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

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "CRTRAN.WR".

       COPY "CREDIT.WR".

       COPY "LEDTRF.WR".

       COPY "TXTRAN.WR".

      /
       AE155-GET-CREDITOR    SECTION.
       AE155-ACCOUNT.
             MOVE W15-ACNO       TO P-NUMBER.
             PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               MOVE 1 TO WS-ERROR
           ELSE
           IF P-NUMBER NOT = W15-ACNO
               MOVE 2 TO WS-ERROR
           ELSE
               MOVE 0 TO WS-ERROR.
       AE160-EXIT.
             EXIT.

      *COPY "CREDIT.LUP".

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
               ADD 1 WS-OPEN GIVING WS-MS1
           ELSE
               MOVE WS-PERIOD TO WS-MS1.
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
      MOVE G-AC   TO WS-SKSTORE.
    IF G-AC < 400000 AND WX-UNPROF > 400000
        PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT
        MOVE WX-UNPROF  TO W15-ACCOUNT
              MOVE W15-ACCOUNT  TO G-AC
              PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
        IF WS-F-ERROR = 0
     MOVE G-AC  TO T-AC
           PERFORM AY12 THRU AY59
        ELSE
     MOVE WS-SKSTORE
     TO W15-ACCOUNT G-AC
     PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
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
       BA000         SECTION.
       BA00.
             MOVE ZERO TO WS-ERROR.
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
             MOVE 1 TO WS-ERROR.
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
    IF (W15-CCYY = WS-L-END) AND
       (W15-MM > WS-Y-END)
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

       EA000      SECTION 5.
       EA00.
      MOVE "Date in a prior Fin Year -Continue Y/N  [Y]" TO WS-OPT-MES.
      MOVE "Y"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
    IF NOT(WS-OPTION = "Y" OR "N")
        GO TO EA00.
    IF WS-OPTION = "Y"
        MOVE ZERO  TO WS-ERROR
    ELSE
        MOVE 1   TO WS-ERROR.

       EA99.
      EXIT.

      /
       FA000      SECTION 5.
       FA00A.
            MOVE "ELECTRONIC PAYMENTS"
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
       FA00B.
             PERFORM AA50 THRU AA60.
             MOVE WS-HACNO       TO CH-H1.
             MOVE WS-HNAME       TO CH-H2.
             MOVE WS-HDTE        TO CH-H3.
            MOVE WS-HEFT  TO CH-H4.
             MOVE "VALUE"        TO CH-H5.
             MOVE WS-HAGE        TO CH-H6.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
             MOVE WS-HACNO       TO R-H5.
             MOVE ALL "_"        TO R-H6 R-H8 R-H16 R-H18.
             MOVE "ACCOUNT NAME" TO R-H7.
             MOVE WS-HDTE        TO R-H9.
            MOVE "REF.CDE"  TO R-H10.
             MOVE "PAYEE/NARRATIVE"
                                 TO R-H17.
             MOVE "DEBIT"        TO R-H14.
             MOVE "CREDIT"       TO R-H15.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "D"   TO W02-PRN-TYPE.

       FA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S05.
      *    *************************************************************
      *    ****   I N I T I A L I Z E W O R K   V A R I A B L E S
      *    *************************************************************
            MOVE ZERO   TO W15-GRND  W15-TCOST
        W15-SETT.
      MOVE SPACES  TO W15-ACNO W15-GRNN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA05.
      DISPLAY S55.
            ACCEPT S06 AT 0421.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACE  TO W15-ACNO
       GO TO FA130
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA05
        END-EVALUATE
        DISPLAY S06 AT 0421
               IF W15-ACNO = SPACES
           GO TO FA05.
      PERFORM CLEAR-L50.
           IF W15-ACNO = SPACES
               GO TO FA130.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W15-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
            PERFORM AE155-GET-CREDITOR.
           IF NOT(WS-ERROR = ZERO)
              MOVE WS-ER1  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO FA05.
            MOVE P-NAME  TO W15-NAME W15-PAYEE.
            DISPLAY P-NAME AT 0428
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
             DISPLAY S-CRED.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA05.

       FA10.
            ACCEPT S15 AT 0811.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO FA05
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA10
        END-EVALUATE.
            MOVE W15-GRND  TO W15-DATE.
             PERFORM BA000.
           IF WS-ERROR = 1
               GO TO FA10.
    IF WS-PERIOD > 12
        PERFORM EA000
        IF WS-ERROR = 1
     GO TO FA10.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
      MOVE W15-DD  TO W20-DD.
             MOVE W15-MM         TO W20-MM.
      MOVE W15-CC  TO W20-CC.
             MOVE W15-YY         TO W20-YY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Reference code / number         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA15.
             ACCEPT S14 AT 0823.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO FA10
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA10
        END-EVALUATE.
            MOVE W15-GRNN  TO W15-REF.
          IF W15-GRNN = SPACES
               GO TO FA15.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Transaction value         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA20.
            ACCEPT S16 AT 0833.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO FA15
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA10
        END-EVALUATE.
             MULTIPLY -1 BY W15-TCOST
                                 GIVING W15-CHQVAL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Cheque/Check value         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            DISPLAY S17 AT 0833.
           IF W15-TCOST = ZERO
               GO TO FA20.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Age Analysis allocation         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA25.
            ACCEPT W15-30 AT 0849
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-60 AT 0853
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-90 AT 0857
      WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-120 AT 0861
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-150 AT 0865
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            ACCEPT W15-180 AT 0869
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA10.
            MOVE 1   TO WS-S2.
             MOVE ZERO           TO WS-S.

       FA30.
             MOVE ZERO           TO W25-CFLOW (WS-S2).
           IF W15-MONTH (WS-S2) > ZERO
               ADD 1             TO WS-S.
           IF WS-S2 < 6
               ADD 1             TO WS-S2
               GO TO FA30.
             MOVE ZERO           TO W25-V1 W25-V2.
            MOVE 1   TO WS-S3.
           IF WS-S < 2
               MOVE W15-CHQVAL   TO W25-V2
               GO TO FA35.
             MOVE W15-TCOST      TO W20-RESULT.
             DIVIDE W20-RESULT BY WS-S
                                 GIVING W25-V1 ROUNDED.
             SUBTRACT 1          FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1               TO WS-S.
             MULTIPLY -1         BY W25-V1.
             MULTIPLY -1         BY W25-V2.

       FA35.
            DISPLAY "Processing payment" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
           IF WS-S = ZERO
               GO TO FA45.

       FA40.
           IF W15-MONTH (WS-S3) = ZERO
               ADD 1             TO WS-S3
               GO TO FA40.
           IF WS-S > 1
               MOVE W25-V1       TO W25-CFLOW (WS-S3)
               SUBTRACT 1        FROM WS-S
               ADD 1             TO WS-S3
               GO TO FA40.
             MOVE W25-V2         TO W25-CFLOW (WS-S3).
             MOVE ZERO           TO W25-V1 W25-V2.
      DISPLAY "Creditor" AT 5040
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start processing transaction - Lock the control records. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA45.
             PERFORM AY60 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Creditor account record, write it to the recovery³
      *    ³ file and lock the record.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            MOVE 0   TO WS-ACTION.
             PERFORM AY07 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Generate the G/L transaction record.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       FA50.
      INITIALIZE CT-REC.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
            MOVE W15-GRND  TO W15-DEL.
      MOVE W15-DEL-DD  TO W21-DD.
      MOVE W15-DEL-MM  TO W21-MM.
      MOVE W15-DEL-CC  TO W21-CC.
      MOVE W15-DEL-YY  TO W21-YY.
            MOVE W21-DTE  TO CT-DTE CT-SDTE.
            MOVE W15-GRNN  TO CT-REF CT-SINV.
             MOVE P-NUMBER       TO CT-CRED.
             MOVE W15-CHQVAL     TO CT-COST.
            MOVE 04   TO CT-CODE.
      MOVE "U"   TO CT-TYPE.
      MOVE "P"   TO CT-PJ.
            MOVE ZERO   TO CT-PAID
        CT-SEQ
        CT-SELL
        CT-TAXFREE
        CT-TAX.
            MOVE 1   TO WS-S WS-S2.
           IF NOT(W25-V2 = ZERO)
               GO TO FA65.

       FA60.
           IF NOT(W15-MONTH(WS-S) = ZERO)
               ADD WS-S2         TO CT-AGE.
           IF WS-S < 6
               ADD 1             TO WS-S
               MULTIPLY 2        BY WS-S2
               GO TO FA60.

       FA65.
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
             ADD W15-CHQVAL      TO W70-CLOSE.
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
            MOVE W15-GRNN  TO CH-EFT.
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
              PERFORM FA00B.
             MOVE SPACES         TO R-DET.
             MOVE W15-TCOST      TO W15-VAL.
             ADD W15-TCOST       TO W20-TOTAL.
             ADD W15-TCOST       TO W20-DEBIT.
            DISPLAY "G/L Bank no  :" AT 1004.
      DISPLAY "Payee        :" AT 1204.
            DISPLAY "Narrative    :" AT 1404.
            DISPLAY "G/L Creditors:" AT 1604.
             MOVE SPACES         TO W15-NAR.

       FA75.
             MOVE W15-BANK       TO W15-ACCOUNT.
      DISPLAY S55.
            ACCEPT S09 AT 1019.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA75
        END-EVALUATE
              DISPLAY S09 AT 1019
              IF W15-ACCOUNT = ZERO
           GO TO FA75.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO FA75.
             MOVE W15-ACCOUNT    TO W15-BANK.

       FA80.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO FA75.
            DISPLAY G-NAME AT 1026
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       FA85.
            MOVE G-AC   TO WS-KSTORE.
            PERFORM CHECK-CORRECT.

       FA90.
          IF WS-OPTION = "N"
               GO TO FA75.

       FA92.
            ACCEPT W15-PAYEE AT 1219
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
          IF W15-PAYEE = SPACES
              GO TO FA92.

       FA95.
            ACCEPT W15-NAR AT 1419
            WITH FOREGROUND-COLOR Grey HIGHLIGHT
    BACKGROUND-COLOR Magenta UPDATE AUTO.
           IF W15-NAR = SPACES
               GO TO FA95.
            MOVE W15-CRED  TO W15-ACCOUNT.

       FA96.
      DISPLAY S55.
            ACCEPT S09 AT 1619.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO FA95
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO FA96
        END-EVALUATE
              DISPLAY S09 AT 1619
              IF W15-ACCOUNT = ZERO
           GO TO FA96.
      PERFORM CLEAR-L50.
          IF W15-ASER = ZERO
              GO TO FA96.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
               GO TO FA96.
            DISPLAY G-NAME AT 1626
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.

       FA100.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO FA96.
            DISPLAY "Updating Creditor G/L" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.

       FA105.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
      MOVE W15-CC  TO W15-CN.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
            MOVE W15-REFER  TO T-REF.
             MOVE G-AC           TO T-AC.
            MOVE 08   TO T-CODE.
          IF W15-VAL > ZERO
               ADD W15-VAL       TO G-DT
           ELSE
               ADD W15-VAL       TO G-CR.
             MOVE W15-VAL        TO T-VAL
                                    WS-VALUE.
      MOVE WS-BATCH  TO T-BATCH.
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

       FA110.
             MOVE W15-ACCOUNT    TO R-ACC.
             MOVE G-NAME         TO R-ANME.
             MOVE W15-DATE       TO R-DTE2.
             MOVE W15-REFER      TO R-REF.
             MOVE W15-NAR        TO R-NAR.
          IF W15-VAL > ZERO
               MOVE W15-VAL TO R-DBT
           ELSE
               MOVE W15-VAL TO R-CRD.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM FA00B.
             MOVE SPACES         TO R-DET.

       FA115.
            DISPLAY "Updating Bank account" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.

            MOVE WS-KSTORE  TO G-AC.
             PERFORM AY12 THRU AY59.
            INITIALIZE T-REC.

       FA120.
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
            MOVE 08   TO T-CODE.
             MULTIPLY -1 BY W15-VAL
                                 GIVING W20-CREDIT.
          IF W20-CREDIT > ZERO
              ADD W20-CREDIT  TO G-DT
           ELSE
              ADD W20-CREDIT  TO G-CR.
            MOVE W20-CREDIT  TO T-VAL.
             MOVE W20-CREDIT     TO WS-VALUE.
             MOVE WS-BATCH       TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE        TO G-VAL (W05-MM).
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

       FA125.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM FA00B.
             MOVE G-AC           TO R-ACC.
             MOVE G-NAME         TO R-ANME.
             MOVE W15-DATE       TO R-DTE2.
             MOVE W15-REFER      TO R-REF.
             MOVE W15-PAYEE      TO R-NAR.
          IF W20-CREDIT > ZERO
              MOVE W20-CREDIT  TO R-DBT
           ELSE
              MOVE W20-CREDIT  TO R-CRD.
            PERFORM AY70 THRU AY999.
          IF W02-LINAGE < (W02-PRN-LENGTH - 1)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM FA00B.
            MOVE SPACES  TO R-DET.

       FA130.
            MOVE "ENTER to continue - E to exit" TO WS-OPT-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
               GO TO FA00.
             MOVE "BATCH TOTAL"  TO R-NAR.
             MOVE W20-TOTAL      TO R-DBT.
             MULTIPLY -1 BY W20-TOTAL
                                 GIVING R-CRD.
      MOVE 1   TO WS-ADVANCE.
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

       FA999.
             EXIT.
      *    *************************************************************
      *                 I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000         SECTION 8.
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
      *    *************************************************************
      *    I N T E G R A T I O N   F I L E   (C R E D I T O R S   G / L)
      *        File ID LEDTRG
      *    *************************************************************
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
      *    *************************************************************
      *    ****      R E C O V E R Y   F I L E      ****
      *    *************************************************************
            OPEN OUTPUT RECOVER.
            CLOSE RECOVER.
            OPEN I-O RECOVER.
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
              MOVE 1   TO WS-ERROR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if transaction codes and descriptions have been set ³
      *    ³ up for Creditor/GL transactions and if not, then create   ³
      *    ³ the details in the parameter file.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE 196   TO WS-PARKEY.
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
            MOVE G-BATCH  TO WS-BATCH.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if the year-end month has been setup and if not,    ³
      *    ³ then set year-end month to February.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
          IF G-YEAR-END = ZERO
              MOVE 2   TO G-YEAR-END
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
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
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
