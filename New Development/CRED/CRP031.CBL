      $set LINKCOUNT"468" GNT"CRP031.GNT"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****     ******      **      *
      *   **    **  **    **  **    **   **  **   **    **    ***      *
      *   **        **    **  **    **  **    **        **     **      *
      *   **        ******    *******   **    **     ****      **      *
      *   **        **   **   **        **    **        **     **      *
      *   **    **  **    **  **         **  **   **    **     **      *
      *    ******   **    **  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *        C R E D I T O R / G L   T R A N S A C T I O N S         *
      *                                                                *
      *          I N V O I C E                                         *
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
       PROGRAM-ID.      CRP031.
       AUTHOR.          James William Lemmon.
       DATE-WRITTEN.    MARCH 1983.

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

       COPY "DEPART.SL".

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       COPY "GLJRN.SL".

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

       COPY "DEPART.FDE".

       COPY "GACCNT.FD".

       COPY "GLTRAN.FD".

       COPY "GLJRN.FD".

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
       77  WS-KSTORE               PIC  9(04)    COMP-5.
       77  WS-SKSTORE              PIC  9(06)    COMP-5.
       77  WS-KEYSTR               PIC  9(04)    COMP-5.
       77  WS-GLJSTR               PIC  9(04)    COMP-5.
       77  WS-GLJKEY               PIC  9(04)    COMP-5.
       77  WS-RECKEY               PIC  9(04)    COMP-5.
       77  WS-RECOVER              PIC  9(04)    COMP-5.
       77  WS-SHARED               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-TRANS                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-ISUB                 PIC  9(04)    COMP-5.
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
       77  WS-S9                   PIC  9(04)    COMP-5.
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
       77  WS-SCRED                PIC  X(09)           VALUE "Creditor".
       77  WS-OPTION               PIC  X(01).
       77  WS-SKIP                 PIC  X(01)           VALUE "N".
       77  WS-LOOK                 PIC  X(01)           VALUE "C".
       77  WS-FIRST                PIC  9(02)           VALUE 0.
       77  WS-LAST                 PIC  9(02)           VALUE 0.
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
       77  WS-ER6                  PIC  X(11)           VALUE "No record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group heading".
       77  PRG-NAME                PIC  X(12)           VALUE SPACES.
       77  PRG-CRED-LUP            PIC  X(12)           VALUE "CRP\CRPLOOK".
       77  PRG-GLED-LUP            PIC  X(12)           VALUE "GLP\GLPLOOK".
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
           03  CRT-PROGRAM         PIC  X(15)           VALUE "CRP031".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40)           VALUE "CREDITOR/GL - INVOICE".
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
           03  WS-PROG             PIC  X(03)           VALUE "031".

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
           03  W02-PRN-TYPE        PIC  X(01).
           03  W02-PRN-LINE        PIC  X(136).

           03  R-SL1                             REDEFINES W02-PRN-LINE.
               05  R-SLA           PIC  X(78).
               05  FILLER          PIC  X(58).

           03  R-SL2                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(58).
               05  R-SLB           PIC  X(78).

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  R-L2                              REDEFINES W02-PRN-LINE.
               05  R-H1            PIC  X(07).
               05  R-DTE1          PIC  Z9/99/9999.
               05  FILLER          PIC  X(31).
               05  R-H2            PIC  X(40).
               05  FILLER          PIC  X(33).
               05  R-H3            PIC  X(08).
               05  R-PGE           PIC  ZZ9.

           03  R-L2A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(56).
               05  R-HEAD          PIC  X(20).
               05  FILLER          PIC  X(56).

           03  R-L3A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  I-H1            PIC  X(11).
               05  I-H2            PIC  X(34).
               05  I-H3            PIC  X(44).
               05  I-H4            PIC  X(11).
               05  I-H5            PIC  X(27).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  G-H1            PIC  X(10).
               05  G-SUPP          PIC  X(06).
               05  FILLER          PIC  X(01).
               05  G-NAMER         PIC  X(40).
               05  FILLER          PIC  X(23).

           03  R-L4A                             REDEFINES W02-PRN-LINE.
               05  G-H3            PIC  X(05).
               05  G-INVN          PIC  X(09).
               05  FILLER          PIC  X(01).
               05  G-H4            PIC  X(06).
               05  FILLER          PIC  X(01).
               05  G-INVD          PIC  Z9/99/9999.
               05  G-H5            PIC  X(06).
               05  G-RAIL          PIC  Z(04)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H6            PIC  X(06).
               05  G-DISC          PIC  Z(04)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H7            PIC  X(06).
               05  G-TOTAL         PIC  Z(06)9.99-.

           03  R-L5A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  I-ACNO          PIC  9(04)/99.
               05  FILLER          PIC  X(04).
               05  I-NAME          PIC  X(30).
               05  FILLER          PIC  X(04).
               05  I-NAR           PIC  X(36).
               05  FILLER          PIC  X(02).
               05  I-VALUE         PIC  Z(08)9.99-.
               05  FILLER          PIC  X(04).
               05  I-30            PIC  X(04).
               05  I-60            PIC  X(04).
               05  I-90            PIC  X(04).
               05  I-120           PIC  X(04).
               05  I-150           PIC  X(04).
               05  I-180           PIC  X(07).
 
           03  R-L7                              REDEFINES W02-PRN-LINE.
               05  CH-H1           PIC  X(07).
               05  CH-H2           PIC  X(32).
               05  CH-H3           PIC  X(08).
               05  CH-H4           PIC  X(13).
               05  CH-H5           PIC  X(08).
               05  CH-H6           PIC  X(12).
 
           03  R-L7A                             REDEFINES W02-PRN-LINE.
               05  CH-ACNO         PIC  X(06).
               05  FILLER          PIC  X(01).
               05  CH-NAME         PIC  X(28).
               05  FILLER          PIC  X(01).
               05  CH-DATE         PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  CH-CHQ          PIC  Z(05)9.
               05  FILLER          PIC  X(01).
               05  CH-VAL          PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  CH-30           PIC  X(02).
               05  CH-60           PIC  X(02).
               05  CH-90           PIC  X(02).
               05  CH-120          PIC  X(02).
               05  CH-150          PIC  X(02).
               05  CH-180          PIC  X(02).
 
           03  R-L8                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  R-H5            PIC  X(11).
               05  R-H6            PIC  X(05).
               05  FILLER          PIC  X(01).
               05  R-H7            PIC  X(14).
               05  R-H8            PIC  X(05).
               05  FILLER          PIC  X(05).
               05  R-H9            PIC  X(09).
               05  R-H10           PIC  X(12).
               05  R-H11           PIC  X(13).
               05  R-H12           PIC  X(10).
               05  R-H13           PIC  X(13).
               05  FILLER          PIC  X(08).
               05  R-H14           PIC  X(14).
               05  R-H15           PIC  X(09).
 
           03  R-L9                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  R-ACC           PIC  9(04)/99.
               05  FILLER          PIC  X(04).
               05  R-ANME          PIC  X(24).
               05  FILLER          PIC  X(03).
               05  R-DTE2          PIC  Z9/99/9999.
               05  FILLER          PIC  X(03).
               05  R-REF           PIC  X(07).
               05  FILLER          PIC  X(04).
               05  R-NAR           PIC  X(36).
               05  FILLER          PIC  X(03).
               05  R-DBT           PIC  Z(08)9.99.
               05  FILLER          PIC  X(02).
               05  R-CRD           PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
 
           03  R-L10                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(65).
               05  R-H16           PIC  X(06).
               05  FILLER          PIC  X(01).
               05  R-H17           PIC  X(23).
               05  R-H18           PIC  X(06).
               05  FILLER          PIC  X(31).
 
           03  R2-L2                             REDEFINES W02-PRN-LINE.
               05  R2-H1           PIC  X(06).
               05  R2-DTE1         PIC  Z9/99/9999.
               05  FILLER          PIC  X(08).
               05  R2-H2           PIC  X(40).
               05  R2-H2A          PIC  X(27).
               05  FILLER          PIC  X(10).
               05  R2-H3           PIC  X(07).
               05  R2-BTCH         PIC  Z(04)9.
               05  FILLER          PIC  X(10).
               05  R2-H4           PIC  X(06).
               05  R2-PGE          PIC  ZZ9.
 
           03  R2-L3                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  R2-H5           PIC  X(11).
               05  R2-H6           PIC  X(05).
               05  FILLER          PIC  X(01).
               05  R2-H7           PIC  X(13).
               05  R2-H8           PIC  X(05).
               05  FILLER          PIC  X(06).
               05  R2-H9           PIC  X(09).
               05  R2-H10          PIC  X(12).
               05  R2-H11          PIC  X(13).
               05  FILLER          PIC  X(01).
               05  R2-H12          PIC  X(09).
               05  R2-H13          PIC  X(13).
               05  FILLER          PIC  X(08).
               05  R2-H14          PIC  X(14).
               05  R2-H15          PIC  X(09).
 
           03  R2-L4                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  R2-ACC          PIC  9(04)/99.
               05  FILLER          PIC  X(04).
               05  R2-ANME         PIC  X(24).
               05  FILLER          PIC  X(03).
               05  R2-DTE2         PIC  Z9/99/9999.
               05  FILLER          PIC  X(03).
               05  R2-REF          PIC  X(07).
               05  FILLER          PIC  X(04).
               05  R2-NAR          PIC  X(36).
               05  FILLER          PIC  X(01).
               05  R2-DBRKO        PIC  X(01).
               05  R2-DBT          PIC  Z(08)9.99.
               05  R2-DBRKC        PIC  X(01).
               05  R2-CBRKO        PIC  X(01).
               05  R2-CRD          PIC  Z(08)9.99-.
               05  R2-CBRKC        PIC  X(01).
               05  FILLER          PIC  X(01).

           03  PD-L1                             REDEFINES W02-PRN-LINE.
               05  PD-H1.
                   07  PD-H2       PIC  X(06).
                   07  PD-H3       PIC  X(41).
               05  PD-H4.
                   07  PD-H5       PIC  X(07).
                   07  PD-H6       PIC  X(07).
               05  PD-H7.
                   07  PD-H8       PIC  X(43).
                   07  PD-H9       PIC  X(07).
                   07  PD-H10      PIC  X(15).
                   07  PD-H11      PIC  X(06).

       COPY "W05.WS".

       COPY "W05.VAT".

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
               05  W20-CC          PIC  99.
               05  W20-YY          PIC  99.
               05  W20-MM          PIC  99.
               05  W20-DD          PIC  99.
           03  W20-DTE             PIC  9(08)    REDEFINES W20-DATE.
           03  W21-DATE.
               05  W21-CC          PIC  99.
               05  W21-YY          PIC  99.
               05  W21-MM          PIC  99.
               05  W21-DD          PIC  99.
           03  W21-DTE             PIC  9(08)    REDEFINES W21-DATE.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).

       01  W25-EDIT.
           03  W25-3V2.
               05  W25-2           PIC  ZZ9.99.
           03  W25-7V2.
               05  W25-3           PIC  Z(06)9.99.
           03  W25-3N.
               05  W25-4           PIC  ZZ9.
           03  W25-DATE.
              05  W25-DTE          PIC  99/99/9999.
           03  W25-6.
               05  W25-NO          PIC  Z(07)9.
           03  W25-5V2.
               05  W25-VAL         PIC  Z(04)9.99.
           03  W25-S5V2.
               05  W25-SVAL        PIC  Z(04)9.99DB.
           03  W25-S7V2.
               05  W25-VAL2        PIC  Z(06)9.99DB.
          03  W25-S8V2.
              05  W25-VG1          PIC  Z(07)9.99CR.
          03  W25-72.
               05  W25-VAL3        PIC  Z(06)9.99-.
           03  W25-REF.
               05  W25-REFER       PIC  Z(07).

       01  W25-VALUES.
           03  W25-CFLOW           PIC S9(07)V99 COMP-3 OCCURS 6.
           03  W25-V1              PIC S9(07)V99 COMP-3.
           03  W25-V2              PIC S9(07)V99 COMP-3.

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W50-H4.
           03  W50-HEAD            PIC  X(20).

       01  W55-DEL.
           03  W55-MNTH            PIC  X(03).
           03  FILLER              PIC  X(01)           VALUE ",".
           03  W55-YY              PIC  9(02).

       01  W60-H2.
           03  W60-DESC            PIC  X(21)           VALUE "CREDITORS AUDIT TRIAL".

       01  W65-CREDITOR.
           03  W65-CFLOW           PIC  9(07)V99 COMP-3 OCCURS 6.

       01  W70-ACTS.
           03  W70-BRANCH          PIC  9(04)    COMP-5.

       COPY "W70CRED.WS".

       01  W90-COMP.
           03  W90-CNAME           PIC  X(40).

       01  W90-TRAN.
           03  W90-CODE            PIC  9(02).
           03  W90-RESULT          PIC  9(04)    COMP-5.
           03  W90-TRNKEY          PIC  9(04)    COMP-5.
           03  W90-TS              PIC  9(02)    COMP-5.

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
           03  W200-MEM-FOC                      REDEFINES W200-MEM-FLDS.
               05  W200-MEM        PIC  X(60)           OCCURS 12.

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

       01  S03.
           03  LINE  2 COLUMN 26 VALUE "CREDITORS INVOICE" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  4 COLUMN  4 PIC  X(09) USING WS-SCRED.
           03  LINE  5 COLUMN  4 VALUE "Invoice no:".
           03          COLUMN 56 VALUE "Invoice date:".
           03  LINE  6 COLUMN  4 VALUE "Allocation - 30:".
           03          COLUMN 24 VALUE "60:".
           03          COLUMN 31 VALUE "90:".
           03          COLUMN 38 VALUE "120:".
           03          COLUMN 46 VALUE "150:".
           03          COLUMN 54 VALUE "180:".
           03  LINE  7 COLUMN  4 VALUE "G/L Account".
           03          COLUMN 18 VALUE "Narrative".
           03          COLUMN 69 VALUE "Value".
           03  LINE 20 COLUMN 54 VALUE "Sub total :".
           03  LINE 21 COLUMN 54 VALUE "VAT       :".
           03  LINE 22 COLUMN 54 VALUE "Totals    :".

       01  S06-18.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  S06.
                   07  PIC X(06) USING W15-ACNO AUTO.
               05  S07.
                   07  PIC X(08) USING W15-INVN AUTO.
               05  S08.
                   07  PIC Z9/99/9999 USING W15-INVD AUTO.
               05  S09.
                   07  PIC 9(06) USING W15-ACCOUNT AUTO.
               05  S10.
                   07  PIC Z(07)9.99- USING W15-COST AUTO.
               05  S18.
                   07  LINE  6 COLUMN 21 PIC 9(01) USING W15-30 AUTO.
                   07          COLUMN 28 PIC 9(01) USING W15-60 AUTO.
                   07          COLUMN 35 PIC 9(01) USING W15-90 AUTO.
                   07          COLUMN 43 PIC 9(01) USING W15-120 AUTO.
                   07          COLUMN 51 PIC 9(01) USING W15-150 AUTO.
                   07          COLUMN 59 PIC 9(01) USING W15-180 AUTO.

       01  S43-45.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Cyan HIGHLIGHT.
               05  S43.
                   07  PIC Z(07)9.99DB USING W15-VAT AUTO.
               05  S44.
                   07  PIC Z(07)9.99DB USING W15-TCOST AUTO.
               05  S45.
                   07  PIC Z(07)9.99DB USING W15-TRNTOT AUTO.

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
             PERFORM DA000.
           IF WS-XFRC = "Y"
               PERFORM GA000.

       AA11.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.
             CLOSE GACCNT GLTRAN LEDTRF.

       AA11E.
             EXIT PROGRAM.

       COPY "AA20CG.LUP".

       AA50.
      *    *************************************************************
      *    ****    Print the audit trail HEADING
      *    *************************************************************
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             MOVE WS-H1              TO R-H1.
             MOVE W90-CNAME          TO R-H2.
             MOVE WS-H4              TO R-H3.
             ADD 1                   TO WS-PAGE.
             MOVE WS-PAGE            TO R-PGE.
             MOVE W05-TODAY          TO R-DTE1.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE W60-DESC           TO R-H2.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACE              TO R-DET.

       AA60.
             MOVE W50-H4             TO R-HEAD.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.

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

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD2".

       COPY "GLTRAN.RD".

       COPY "GLJRN.RD".

       COPY "LEDTRF.RD1".

       COPY "SHARED.RD".

       COPY "TXTRAN.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "DEPART.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "CRTRAN.WR".

       COPY "GLJRN.WR".

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

      /    *************************************************************
      *      THIS ROUTINE UPDATES THE VARIOUS MONTHS FROM TRANSACTION
      *      DATE UP TO THE CURRENT MONTH
      *    *************************************************************
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
      *    ****            D E P A R T M E N T   F I L E            ****
      *    *************************************************************
       AY38.
             MOVE 38                 TO REC-FILE.
             MOVE ZERO               TO REC-KEY.
             MOVE ZERO               TO REC-TYPE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
           IF WS-F-ERROR = 7
               MOVE "XXXX"           TO DPT-CODE
               GO TO AY38.
             MOVE DPT-RECORD         TO REC-DEPART.
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

       AY65.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             MOVE G-BATCH        TO WS-BATCH.
           IF G-BATCH = 99999
               MOVE 1            TO G-BATCH
           ELSE
               ADD 1             TO G-BATCH.
             PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
             GO TO AY999.
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
            MOVE 1   TO WS-ERROR.
      GO TO BA999.
      *    *************************************************************
      *    ****    CALCULATE THE PERIOD INTO WHICH THIS DATE FALLS
      *    *************************************************************
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
       CA700         SECTION.
       CA701.
             MOVE 3              TO TAX-ACTYPE.
             MOVE CT-CODE        TO TAX-CODE.
             MOVE W20-DTE        TO TAX-DATE.
             MOVE CT-REF         TO TAX-REF.
             MULTIPLY -1 BY CT-COST
                                 GIVING TAX-VALUE.
             MULTIPLY -1 BY CT-TAXFREE
                                 GIVING TAX-TAXFREE.
      MOVE CT-CRED  TO TAX-AC.
    IF W00-C-ACT = 1
               IF CT-CODE = 03
                   MOVE "3"      TO TAX-TYPE
               ELSE
                   MOVE "2"      TO TAX-TYPE
           ELSE
               MOVE "1"          TO TAX-TYPE.
      MOVE ZERO   TO TAX-SEQ.
      MOVE 1   TO WS-S9.

       CA705.
      MULTIPLY -1 BY W15-TAXVAL(WS-S9)
     GIVING TAX-VAL(WS-S9).
      MULTIPLY -1 BY W15-VATVAL(WS-S9)
     GIVING TAX-VAT(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO CA705.
            PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
            MOVE 1   TO WS-ACTION.
            PERFORM AY19 THRU AY59.

       CA799.
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
       DA000      SECTION 5.
       DA00A.
            MOVE "GOODS/SERV RECEIVED"
                                 TO W50-HEAD.
      *    *************************************************************
      *    ****   C L E A R   T O T A L S   T O   Z E R O
      *    *************************************************************
             MOVE ZERO               TO W20-DEBIT W20-VAT.
             MOVE SPACES             TO W15-NAR2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Audit trail headings.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DA00B.
             PERFORM AA50 THRU AA60.
             MOVE "ACC NO"           TO I-H1.
             MOVE "G/LEDGER ACCOUNT" TO I-H2.
             MOVE "NARRATIVE"        TO I-H3.
             MOVE "VALUE"            TO I-H4.
             MOVE "30  60  90 120 150 180" TO I-H5.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       DA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S03.
             MOVE SPACES         TO W15-ACNO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Allow user to key in the Creditor account number.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DA05.
             DISPLAY S55.
             ACCEPT S06 AT 0416.
            IF USER-FUNC
                EVALUATE KEY-CODE-1
                  WHEN ESC-KEY MOVE SPACE  TO W15-ACNO
                               GO TO DA115
                  WHEN F1-KEY  PERFORM HELP-ROUTINE
                  WHEN F2-KEY  PERFORM AA20
                  WHEN OTHER   PERFORM AA900-ALARM
                               GO TO DA05
                END-EVALUATE
                DISPLAY S06 AT 0416
                IF W15-ACNO = SPACES
                    GO TO DA05.
           IF W15-ACNO = SPACES
                GO TO DA115.
             MOVE 6                  TO WS-LENGTH.
             CALL "CBL_TOUPPER" USING W15-ACNO BY VALUE WS-LENGTH RETURNING WS-STATUS.
             MOVE 1                  TO WS-LENGTH.
             PERFORM AE155-GET-CREDITOR.
           IF NOT(WS-ERROR = ZERO)
               MOVE WS-ER1           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA05.
             MOVE P-NAME             TO W15-NAME.
             DISPLAY P-NAME AT 0423 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             DISPLAY S-CRED.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA05.
      *    *************************************************************
      *    ****   I N I T I A L I Z E   W O R K   V A R I A B L E S
      *    *************************************************************
             MOVE SPACES             TO W15-GRNN W15-INVN.
             MOVE ZERO               TO W15-INVD W15-COST.
             MOVE ZERO               TO W65-CFLOW(1) W65-CFLOW(2) W65-CFLOW(3) W65-CFLOW(4) W65-CFLOW(5) W65-CFLOW(6).
             MOVE 1                  TO WS-S9.

       DA06.
             MOVE ZERO               TO W15-VATVAL(WS-S9) W15-TAXVAL(WS-S9).
           IF WS-S9 < 12
               ADD 1                 TO WS-S9
               GO TO DA06.
             MOVE P-NUMBER           TO W15-REFER.

       DA10.
             ACCEPT S07 AT 0516.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  GO TO DA05
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA10
               END-EVALUATE.
           IF W15-INVN = SPACES
               GO TO DA10.

       DA13.
             ACCEPT S08 AT 0570.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY GO TO DA10
                 WHEN OTHER   PERFORM AA900-ALARM
                              GO TO DA13
               END-EVALUATE.
               MOVE W15-INVD         TO W15-DATE.
               PERFORM BA000.
           IF WS-ERROR = 1
                GO TO DA13.
           IF WS-PERIOD > 12
               PERFORM EA000
               IF WS-ERROR = 1
                   GO TO DA13.
            PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA05.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
             MOVE W15-DD             TO W20-DD.
             MOVE W15-MM             TO W20-MM.
             MOVE W15-CC             TO W20-CC.
             MOVE W15-YY             TO W20-YY.
      *    *************************************************************
      *    ****  NB.  AMEND TO ALLOW FOR MULTIPLE VAT RATES!!!
      *    *************************************************************
           IF W20-DTE < WS-VAT-DATE(1)
               MOVE 7                TO WS-VAT-SUB
           ELSE
               MOVE 1                TO WS-VAT-SUB.
             MOVE P-TERMS            TO W15-SETT.
             MOVE W05-TODAY          TO W15-GRND.
             MOVE ZERO               TO W15-TCOST W15-RAIL W15-DISCOUNT.
             MOVE ZERO               TO W15-VAL W15-TXBL W15-TRNTOT W15-VAT W15-EXCL W15-NTXBL W15-LVAT.
             MOVE 7                  TO LIN.
      *    *************************************************************
      *    ****    A G E   A N A L Y S I S
      *    *************************************************************
             ACCEPT S18.
             MOVE 0                  TO WS-LAST.

       DA14.
             MOVE ZERO               TO W15-ACCOUNT W15-COST.
             MOVE W15-NAR2           TO W15-NAR.
             ADD 1                   TO LIN WS-LAST.

       DA15.
             DISPLAY S55.
             ACCEPT S09 AT LINE LIN COLUMN 6.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY  MOVE ZERO  TO W15-ACCOUNT
                               GO TO DA16
                 WHEN F1-KEY   PERFORM HELP-ROUTINE
                 WHEN F2-KEY   PERFORM AA20G THRU AA20
                 WHEN OTHER    PERFORM AA900-ALARM
                               GO TO DA15
               END-EVALUATE
               DISPLAY S09 AT LINE LIN COLUMN 6
               IF W15-ACCOUNT = ZERO
                   GO TO DA15.

       DA16.
           IF W15-ACCOUNT = ZERO
               SUBTRACT 1            FROM WS-LAST LIN
               GO TO DA37.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA15.

       DA20.
             MOVE W15-ACCOUNT        TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA15.

       DA30.
             DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan BACKGROUND-COLOR Blue.
             DISPLAY G-NAME AT 2306 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.

       DA32.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA15.

       DA35.
             ACCEPT W15-NAR AT LINE LIN COLUMN 18 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             MOVE W15-NAR            TO W15-NAR2.
             ACCEPT S10 AT LINE LIN COLUMN 62.
             MOVE W15-COST           TO W15-VAL.
           IF W15-VAL = ZERO
               MOVE WS-ER4           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA35.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA15.

       DA36.
             MOVE "Taxable Y/N  [Y]" TO WS-OPT-MES.
             MOVE "Y"   TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO DA36.
             MOVE ZERO               TO W15-VAT.
           IF WS-OPTION = "N"
               ADD W15-VAL           TO W15-NTXBL
               MOVE W15-VAL          TO GLJ-VAL
               GO TO DA36B.
       DA36A.
             MOVE "'E'xclusive or 'I'nclusive  [E]" TO WS-OPT-MES.
             MOVE "E"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "E" OR "I")
               GO TO DA36A.
             MOVE W05-VAT(WS-VAT-SUB) TO W05-RATE.
           IF WS-OPTION = "E"
               ADD W15-VAL           TO W15-TXBL
               ADD W15-COST          TO W15-TAXVAL(WS-VAT-SUB)
               MOVE W15-VAL          TO GLJ-VAL
               COMPUTE W15-VAT ROUNDED = (W15-VAL * W05-RTE)
           ELSE
               COMPUTE W15-VAT ROUNDED = ((W15-VAL / (100.00000 + W05-RATE)) * W05-RATE)
               COMPUTE GLJ-VAL = W15-VAL - W15-VAT
               COMPUTE W15-TXBL = W15-TXBL + (W15-VAL - W15-VAT)
               ADD GLJ-VAL           TO W15-TAXVAL(WS-VAT-SUB).
               ADD W15-VAT           TO W15-VATVAL(WS-VAT-SUB) W15-LVAT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Write details to the G/L transaction work file.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DA36B.
             MOVE WS-LAST            TO WS-GLJKEY.
             MOVE W15-ACCOUNT        TO GLJ-AC.
             MOVE W15-NAR            TO GLJ-NAR.
             MOVE W15-LVAT           TO W15-VAT.
             PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
             ADD W15-TXBL W15-NTXBL  GIVING W15-TCOST.
             ADD W15-TCOST W15-VAT   GIVING W15-TRNTOT.
      *
      *      ****    C O S T
      *
             DISPLAY S44 AT 2066.
      *
      *      ****    V A T
      *
             DISPLAY S43 AT 2166.
      *
      *      ****    T O T A L
      *
             DISPLAY S45 AT 2266.
           IF LIN < 19
               GO TO DA14.
             MOVE 0801               TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.
             MOVE 7   TO LIN.
             GO TO DA14.

       DA37.
           IF WS-LAST = ZERO
               GO TO DA115.
             PERFORM CLEAR-L50.

       DA37X.
             PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO DA14.

       DA37A.
             MOVE "'P'rocess Invoice or 'C'ancel  [ ]" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "P" OR "C")
               GO TO DA37A.
           IF WS-OPTION = "C"
               GO TO DA115.
           IF NOT(W15-LVAT = ZERO)
               ADD 1                 TO LIN
               PERFORM DA50 THRU DA53.
             DISPLAY "Processing Invoice" AT 5020 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE 1                  TO WS-GLJKEY.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Start processing transaction - Lock the control records. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            PERFORM AY60 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Read the Creditor account record, write it to the recovery³
      *    ³ file and lock the record.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 0                  TO WS-ACTION.
             PERFORM AY07 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Loop through the G/L transaction work file and update.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       DA38.
           IF WS-GLJKEY > WS-LAST
               GO TO DA55.
             PERFORM READ-GLJRN THRU READ-GLJRN-EXIT.
             MOVE GLJ-AC             TO W15-ACCOUNT G-AC.
             MOVE GLJ-NAR            TO W15-NAR.
             MOVE GLJ-VAL            TO W15-VAL.
             PERFORM AY12 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Generate the G/L transaction record.           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
            INITIALIZE T-REC.
      *    *************************************************************
      *    ****    Switch date to CCYYMMDD format
      *    *************************************************************
       DA40.
             MOVE W15-CC             TO W15-CN.
             MOVE W15-YY             TO W15-YR.
             MOVE W15-MM             TO W15-MT.
             MOVE W15-DD             TO W15-DY.
             MOVE W15-YMD            TO T-DATE.
             MOVE W15-NAR            TO T-NARATIVE.
             MOVE W15-INVN           TO T-REFER.
             MOVE W15-VAL            TO T-VAL.
             MOVE G-AC               TO T-AC.
             ADD W15-VAL             TO W20-DEBIT.
             ADD W15-VAL             TO G-DT.
             MOVE 01                 TO T-CODE.
             MOVE W15-VAL            TO WS-VALUE.
             MOVE WS-BATCH           TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE            TO G-VAL(W05-MM).
             ADD W15-VAL             TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
       DA45.
             MOVE W15-ACCOUNT        TO I-ACNO.
             MOVE G-NAME             TO I-NAME.
             MOVE W15-NAR            TO I-NAR.
             MOVE W15-VAL            TO I-VALUE.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM DA00B.
             MOVE SPACES             TO R-DET.
             ADD 1                   TO WS-GLJKEY.
             GO TO DA38.
      *    *************************************************************
      *    ****    VAT Control
      *       MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
       DA50.
             MOVE "VAT1"             TO DPT-CODE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-I-VAT-GL       TO W15-ACCOUNT.
             SUBTRACT W15-LVAT       FROM W15-TRNTOT.

       DA52.
             DISPLAY "Key in the VAT G/L account number "                         AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT " "
                                                       "F1"                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                         "=Help,"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                               "F2"                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                  " to do Account Lookup" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S09 AT LINE LIN COLUMN 6.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
                     PERFORM HELP-ROUTINE
                 WHEN F2-KEY
                     PERFORM AA20G THRU AA20
                 WHEN OTHER
                     PERFORM AA900-ALARM
                     GO TO DA52
               END-EVALUATE
               DISPLAY S09 AT LINE LIN COLUMN 6
               IF W15-ACCOUNT = ZERO
                   GO TO DA52.
             PERFORM CLEAR-L50.
           IF W15-ACCOUNT = ZERO
               GO TO DA52.
           IF W15-ASER = ZERO
               MOVE WS-ER7           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA52.
             MOVE W15-ACCOUNT        TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
           IF WS-F-ERROR = 8
               MOVE WS-ER6           TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO DA52.
             DISPLAY G-NAME AT 2306 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA52.
             MOVE P-NAME             TO W15-NAR.
             ACCEPT W15-NAR AT LINE LIN COLUMN 18 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta UPDATE AUTO.
             ACCEPT S43 AT 2166.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO DA52.
             ADD 1                   TO WS-LAST.
             MOVE WS-LAST            TO WS-GLJKEY.
             MOVE W15-ACCOUNT        TO GLJ-AC.
             MOVE W15-NAR            TO GLJ-NAR.
             MOVE W15-LVAT           TO GLJ-VAL.
             PERFORM WRITE-GLJRN THRU WRITE-GLJRN-EXIT.
             ADD W15-LVAT            TO W15-TRNTOT.

       DA53.
             EXIT.

       DA55.
             MOVE W15-GRND           TO W15-DATE.
             MOVE W15-TRNTOT         TO W20-RESULT.
           IF W20-RESULT = ZERO
               GO TO DA80.
             MOVE ZERO               TO WS-S.
             MOVE 1                  TO WS-S2.

       DA60.
             MOVE ZERO               TO W25-CFLOW (WS-S2).
           IF W15-MONTH(WS-S2) > 0
               ADD 1                 TO WS-S.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO DA60.
             MOVE ZERO               TO W25-V1 W25-V2.
             MOVE 1                  TO WS-S3.
          IF WS-S = ZERO
              MOVE 1                 TO WS-S
              GO TO DA68.
      *        W15-30.
           IF WS-S = 1
               MOVE W20-RESULT       TO W25-V2
               GO TO DA65.
             DIVIDE W20-RESULT BY WS-S GIVING W25-V1 ROUNDED.
             SUBTRACT 1              FROM WS-S.
             COMPUTE W25-V2 = W20-RESULT - (W25-V1 * WS-S).
             ADD 1                   TO WS-S.

       DA65.
           IF W15-MONTH(WS-S3) = ZERO
               ADD 1                 TO WS-S3
               GO TO DA65.
           IF WS-S > 1
               MOVE W25-V1           TO W25-CFLOW(WS-S3)
               SUBTRACT 1            FROM WS-S
               ADD 1                 TO WS-S3
               GO TO DA65.
             MOVE W25-V2             TO W25-CFLOW (WS-S3).

       DA68.
             MOVE 1                  TO WS-S2.

       DA70.
           IF W25-CFLOW (WS-S2) = ZERO
               GO TO DA75.
             ADD W05-MM WS-S2        GIVING WS-S3.
           IF WS-S3 > 12 
               SUBTRACT 12           FROM WS-S3.
       DA75.
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO DA70.

       DA80.
             MOVE SPACES             TO R-DET.

       DA95.
            PERFORM CLEAR-L50.
      *
      *    ****   C R E D I T O R   T R A N S A C T I O N
      *
       DA100.
             INITIALIZE CT-REC.
             MOVE W15-GRND           TO W15-DEL.
             MOVE W15-DEL-DD         TO W21-DD.
             MOVE W15-DEL-MM         TO W21-MM.
             MOVE W15-DEL-CC         TO W21-CC.
             MOVE W15-DEL-YY         TO W21-YY.
             MOVE W21-DTE            TO CT-DTE.
           IF W15-GRNN NOT = SPACES
               MOVE W15-GRNN         TO CT-REF
           ELSE
               MOVE W15-INVN         TO CT-REF.
             MOVE W15-INVN           TO CT-SINV.
             MOVE W15-INVD           TO W15-DEL.
             MOVE W15-DEL-DD         TO W21-DD.
             MOVE W15-DEL-MM         TO W21-MM.
             MOVE W15-DEL-CC         TO W21-CC.
             MOVE W15-DEL-YY         TO W21-YY.
             MOVE W21-DTE            TO CT-SDTE.
             MOVE W15-TRNTOT         TO CT-COST.
             MOVE W15-LVAT           TO CT-TAX.
             MOVE W15-NTXBL          TO CT-TAXFREE.
             MOVE P-NUMBER           TO CT-CRED.
             MOVE 11                 TO CT-CODE.
             MOVE W15-TRNTOT         TO CT-SELL W20-RESULT.
             MOVE "U"                TO CT-TYPE.
             MOVE "I"                TO CT-PJ.
             MOVE ZERO               TO CT-PAID CT-SEQ.
             ADD W15-LVAT            TO W20-VAT.
      *    *************************************************************
      *    ****   TRANSACTION STATISTICS
      *    *************************************************************
             PERFORM CA600.
      *    *************************************************************
      *    ****   VAT REPORT
      *    *************************************************************
             PERFORM CA700.
           IF WX-INT = "Y"
               MOVE "Y"              TO WS-XFRC
               MOVE W20-DTE          TO WX-DATE
               MOVE "  CG"           TO WX-LEDG
               MOVE W90-CODE         TO WX-TRN
               MOVE W00-C-DESC       TO WX-DESC
               MOVE "- CREDITORS"    TO WX-BOOK
               COMPUTE WX-VALUE = CT-COST * -1
               MOVE WX-CREGL         TO WX-AC
               MOVE 02               TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             MOVE ZERO               TO WS-S.
             MOVE 1                  TO WS-S2.
             PERFORM DA60 THRU DA68.
             MOVE 1                  TO WS-S2.

       DA105.
             MOVE W25-CFLOW(WS-S2)   TO W65-CFLOW (WS-S2).
           IF WS-S2 < 6
               ADD 1                 TO WS-S2
               GO TO DA105.
             MOVE 1                  TO WS-S WS-S2.

       DA110.
           IF NOT(W25-CFLOW(WS-S) = ZERO)
               ADD WS-S2             TO CT-AGE.
           IF WS-S < 6
               ADD 1                 TO WS-S
               MULTIPLY 2            BY WS-S2
               GO TO DA110.
             ADD W15-TRNTOT          TO P-BAL.
             ADD W15-TRNTOT          TO P-CR.
             ADD W25-CFLOW(1)        TO P-30D.
             ADD W25-CFLOW(2)        TO P-60D.
             ADD W25-CFLOW(3)        TO P-90D.
             ADD W25-CFLOW(4)        TO P-120D.
             ADD W25-CFLOW(5)        TO P-150D.
             ADD W25-CFLOW(6)        TO P-180D.
           IF CT-AGE = ZERO
               ADD W15-TRNTOT        TO P-OUT W70-DUE-NOW.
             PERFORM WRITE-CRTRAN THRU WRITE-CRTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY08 THRU AY59.
             PERFORM REWRITE-CREDIT THRU WRITE-CREDIT-EXIT.
      *
      *    ****   C O N T R O L   T O T A L S
      *
             ADD W15-TRNTOT          TO W70-CLOSE.
             ADD W15-TRNTOT          TO W70-CRED.
             ADD W25-CFLOW(6)        TO W70-DUE-180.
             ADD W25-CFLOW(5)        TO W70-DUE-150.
             ADD W25-CFLOW(4)        TO W70-DUE-120.
             ADD W25-CFLOW(3)        TO W70-DUE-90.
             ADD W25-CFLOW(2)        TO W70-DUE-60.
             ADD W25-CFLOW(1)        TO W70-DUE-30.
      *    *************************************************************
      *    ****    VAT Control
      *       MULTIPLE RATE TO BE INCLUDED
      *    *************************************************************
           IF W15-LVAT = ZERO
               GO TO DA112.
             MOVE "VAT1"  TO DPT-CODE.
             PERFORM AY38 THRU AY59.
             ADD W15-VATVAL(1)       TO DPT-I-VAT-DAY DPT-I-VAT-MTD DPT-I-VAT-YTD.
             ADD W15-VATVAL(7)       TO DPT-I-VAT-DAY DPT-I-VAT-MTD DPT-I-VAT-YTD.
             ADD W15-TAXVAL(1)       TO DPT-V-PUR-DAY DPT-V-PUR-MTD DPT-V-PUR-YTD.
             ADD W15-TAXVAL(7)       TO DPT-V-PUR-DAY DPT-V-PUR-MTD DPT-V-PUR-YTD.
      *    IF WX-INT = "Y"
      *        MOVE W15-LVAT  TO WX-VALUE
      *        MOVE DPT-I-VAT-GL TO WX-AC
      *        MOVE 13   TO WX-TYPE
      *        PERFORM GL-TRF-UPDATE.
            PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *    *************************************************************
      *    ****    End Recovery Logging.
      *    *************************************************************
       DA112.
             PERFORM AY70 THRU AY999.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM DA00B.
             MOVE SPACES             TO R-DET.
      *    *************************************************************
      *    *****              CREDITOR INVOICE TOTAL               *****
      *    *************************************************************
             MOVE WS-HCRED           TO G-H1.
             MOVE W15-REFER          TO G-SUPP.
             MOVE W15-NAME           TO G-NAMER.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "INV:"             TO G-H3.
             MOVE "DATE:"            TO G-H4.
             MOVE "VAT:"             TO G-H6.
             MOVE "TOTAL:"           TO G-H7.
             MOVE W15-INVN           TO G-INVN.
             MOVE W15-INVD           TO G-INVD.
             MOVE W15-VAT            TO G-DISC.
             MOVE W15-TRNTOT         TO G-TOTAL.
           IF W15-MONTH(1) = 1
               MOVE "Y"              TO I-30
           ELSE
               MOVE "N"              TO I-30.
           IF W15-MONTH(2) = 1
               MOVE "Y"              TO I-60
           ELSE
               MOVE "N"              TO I-60.
           IF W15-MONTH(3) = 1
               MOVE "Y"              TO I-90
           ELSE
               MOVE "N"              TO I-90.
           IF W15-MONTH(4) = 1
               MOVE "Y"              TO I-120
           ELSE
               MOVE "N"              TO I-120.
           IF W15-MONTH(5) = 1
               MOVE "Y"              TO I-150
           ELSE
               MOVE "N"              TO I-150.
           IF W15-MONTH(6) = 1
               MOVE "Y"              TO I-180
           ELSE
               MOVE "N"              TO I-180.
           IF W02-LINAGE < (W02-PRN-LENGTH - 2)
               MOVE 2                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM DA00B.
             MOVE SPACES             TO R-DET.
       DA115.
             MOVE "ENTER to continue - E to exit" TO WS-OPT-MES.
             MOVE SPACE              TO WS-OPTION.
             PERFORM OPT-MESSAGE.
          IF NOT(WS-OPTION = "E")
               GO TO DA00.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE "Total excluding VAT" TO I-NAR.
             SUBTRACT W20-VAT FROM W20-DEBIT GIVING I-VALUE.
             PERFORM CALL-PRINTUTL.
             MOVE "VAT"              TO I-NAR.
             MOVE W20-VAT            TO I-VALUE.
             PERFORM CALL-PRINTUTL.
             MOVE "GENERAL LEDGER TOTAL" TO I-NAR.
             MOVE W20-DEBIT          TO I-VALUE.
             PERFORM CALL-PRINTUTL.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *    *************************************************************
             MOVE 0                  TO WS-ADVANCE.
             MOVE 1                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.

       DA999.
             EXIT.

       EA000      SECTION 5.
       EA00.
             MOVE "Date in a prior Fin Year -Continue Y/N  [Y]" TO WS-OPT-MES.
             MOVE "Y"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "Y" OR "N")
               GO TO EA00.
           IF WS-OPTION = "Y"
               MOVE ZERO             TO WS-ERROR
           ELSE
               MOVE 1                TO WS-ERROR.

       EA99.
             EXIT.

      *    *************************************************************
      *    ****    P R O C E S S   C R E D I T O R   C O N T R O L
      *    *************************************************************
       GA000        SECTION 5.
       GA00.
             PERFORM ERASE-SCREEN.
             DISPLAY "PROCESSING CREDITOR CONTROL" AT 0227
                               WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             INITIALIZE XFR-REC.
             PERFORM START-AT-LEDTRF-KEY THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
               MOVE "No entries to be posted" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO GA999.
       GA05.
             PERFORM AY60 THRU AY999.
             PERFORM AY65 THRU AY999.
             MOVE ZERO               TO WS-PAGE W20-DEBIT W20-CREDIT.
             MOVE "G/L JOURNALS"     TO W10-HEAD.
             MOVE W05-TODAY          TO W15-DATE.
             DISPLAY "Date     :" AT 1210.
             DISPLAY "Account  :" AT 1410.
             DISPLAY "Narrative:" AT 1610.
             DISPLAY "Value    :" AT 1810.
      *    *************************************************************
      *    ****   1 7   C P I   ( C O N D E N S E D   P R I N T )
      *    *************************************************************
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.

       GA10.
             PERFORM AA50.
             MOVE "ACCNT NO"         TO R2-H5.
             MOVE ALL "_"            TO R2-H6.
             MOVE "ACCOUNT NAME"     TO R2-H7.
             MOVE ALL "_"            TO R2-H8.
             MOVE "DATE"             TO R2-H9.
             MOVE "REFERENCE"        TO R2-H10.
             MOVE ALL "_"            TO R2-H11.
             MOVE "NARRATION"        TO R2-H12.
             MOVE ALL "_"            TO R2-H13.
             MOVE "DEBIT"            TO R2-H14.
             MOVE "CREDIT"           TO R2-H15.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO R-DET.
             MOVE "D"                TO W02-PRN-TYPE.

       GA15.
             PERFORM READ-LEDTRF-NEXT THRU READ-LEDTRF-EXIT.
           IF WS-F-ERROR = 40
               GO TO GA20.
      *    *************************************************************
      *    ****    C H E C K   G / L   A C C O U N T   N O .
      *    *************************************************************
             MOVE XFR-AC             TO W15-ACCOUNT.
             MOVE ZERO               TO WS-ERROR.
             DISPLAY W15-ACCOUNT AT 1422 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE W15-ACCOUNT        TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
               PERFORM GA30 THRU GA40
               GO TO GA15.
      *    *************************************************************
      *    ****    G E N E R A T E   R E F E R E N C E   N O .
      *    *************************************************************
           IF XFR-MM > 9
               IF XFR-MM = 10
                   MOVE "O"          TO W15-IMM
               ELSE
               IF XFR-MM = 11
                   MOVE "N"          TO W15-IMM
               ELSE
                   MOVE "D"          TO W15-IMM
               END-IF
           ELSE
               MOVE XFR-MM           TO W15-IMN.
             MOVE XFR-DD             TO W15-IDD.
             MOVE XFR-LEDG           TO W15-ILEDG.
      *    *************************************************************
      *    ****    Switch date to DDMMCCYY format
      *    *************************************************************
             MOVE XFR-DATE           TO W15-YMD.
             MOVE W15-CN             TO W15-CC.
             MOVE W15-YR             TO W15-YY.
             MOVE W15-MT             TO W15-MM.
             MOVE W15-DY             TO W15-DD.
             MOVE XFR-NAR            TO W15-NAR.
             MOVE XFR-VALUE          TO W15-VAL.
             MOVE W15-DATE           TO W25-DTE.
             DISPLAY W25-DATE AT 1221 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY G-NAME   AT 1430 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             DISPLAY W15-NAR  AT 1622 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
             MOVE W15-VAL            TO W25-VAL2.
             DISPLAY W25-S7V2 AT 1822 WITH BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
      *    *************************************************************
      *    ****    PROCESS AND PRINT TRANSACTION
      *    *************************************************************
             MOVE G-AC               TO R2-ACC.
             MOVE G-NAME             TO R2-ANME.
             MOVE W15-NAR            TO R2-NAR.
             MOVE W15-REFER          TO R2-REF.
             MOVE W15-DATE           TO R2-DTE2.
           IF W15-VAL < ZERO
               MOVE W15-VAL          TO R2-CRD
           ELSE
               MOVE W15-VAL          TO R2-DBT.
             PERFORM AY12 THRU AY59.
             INITIALIZE T-REC.
             MOVE W15-YMD            TO T-DATE.
             MOVE W15-NAR            TO T-NARATIVE.
             MOVE W15-REF            TO T-REFER.
             MOVE W15-VAL            TO T-VAL.
             MOVE G-AC               TO T-AC.
           IF W15-VAL < ZERO
               ADD W15-VAL           TO W20-CREDIT
               ADD W15-VAL           TO G-CR
               MOVE 02               TO T-CODE
           ELSE
               ADD W15-VAL           TO W20-DEBIT
               ADD W15-VAL           TO G-DT
               MOVE 01               TO T-CODE.
             MOVE W15-VAL            TO WS-VALUE.
             MOVE WS-BATCH           TO T-BATCH.
             PERFORM AH000.
             ADD WS-VALUE            TO G-VAL (W05-MM).
             ADD W15-VAL             TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1                  TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
           IF W02-LINAGE < W02-PRN-LENGTH
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM GA10.
             MOVE SPACES             TO R-DET.
             GO TO GA15.

       GA20.
             PERFORM AY70 THRU AY999.
           IF W02-LINAGE < (W02-PRN-LENGTH - 1)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM GA10.
             MOVE "*****   T O T A L S   *****" TO R2-NAR.
             MOVE W20-DEBIT          TO R2-DBT.
             MOVE W20-CREDIT         TO R2-CRD.
             MOVE 99                 TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
      *    *************************************************************
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *    *************************************************************
             MOVE 0                  TO WS-ADVANCE.
             MOVE 1                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             MOVE "D"                TO W02-PRN-TYPE.
             MOVE SPACES             TO R-DET.
             CLOSE LEDTRF.
             OPEN OUTPUT LEDTRF.
             CLOSE LEDTRF.
             OPEN I-O LEDTRF.
             GO TO GA999.

       GA30.
             MOVE "** INVALID G/L ACCOUNT NUMBER" TO R-DET.

       GA35.
           IF XFR-VALUE < ZERO
               MOVE XFR-VALUE        TO R2-CRD
               MOVE "["              TO R2-CBRKO
               MOVE "]"              TO R2-CBRKC
           ELSE
               MOVE XFR-VALUE        TO R2-DBT
               MOVE "["              TO R2-DBRKO
               MOVE "]"              TO R2-DBRKC.
           IF W02-LINAGE < (W02-PRN-LENGTH - 1)
               MOVE 1                TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
           ELSE
               MOVE 99               TO WS-ADVANCE
               PERFORM CALL-PRINTUTL
               PERFORM GA10.
             MOVE SPACES             TO R-DET.
             MOVE ZERO               TO WS-ERROR.

       GA40.
             EXIT.

       GA999.
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
               MOVE 8                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN I-O GLTRAN.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 11               TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA01C.
             OPEN I-O LEDTRF.
           IF WS-STATUS = "05"
               CLOSE LEDTRF
               OPEN OUTPUT LEDTRF
               CLOSE LEDTRF
               OPEN I-O LEDTRF.
           IF RUNTIME-ERROR
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STAT1 = "9"
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF IDX-CORRUPT OR (WS-STATUS = "39")
                   CLOSE PARAM NETWORK
                   CALL "UTP\LTFREB" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY
                   CANCEL "UTP\LTFREB"
                   OPEN I-O PARAM NETWORK
                   GO TO ZA01C.
          IF WS-STATUS NOT = "00"
               MOVE 40               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
             MOVE 5                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"              TO WX-INT
           ELSE
               MOVE "Y"              TO WX-INT.
             MOVE PAR-DEBGL          TO WX-DEBGL.
             MOVE PAR-CREGL          TO WX-CREGL.
             MOVE PAR-INTGL          TO WX-INTGL.
             MOVE PAR-BNKGL          TO WX-BNKGL.
             MOVE PAR-UNPROF         TO WX-UNPROF.
             MOVE PAR-REDGL          TO WX-REDGL.
             MOVE PAR-ADJGL          TO WX-ADJGL.
             MOVE PAR-RLGL           TO WX-RLGL.
             MOVE PAR-DSGL           TO WX-DSGL.
           IF WX-BNKGL = ZERO
               DISPLAY "Parameters not set up - ENTER" AT 2020 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT WS-OPTION AT 2050 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               MOVE 1                TO WS-ERROR.
             MOVE 196                TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-C-ABR(1) = SPACES
               MOVE "Invoice (G/L)"  TO PAR-C-DESC(1)
               MOVE "Inv"            TO PAR-C-ABR(1)
               MOVE 0                TO PAR-C-ACT(1)
               MOVE 11               TO PAR-C-CODE(1)
               MOVE "Debit Note (G/L)" TO PAR-C-DESC(2)
               MOVE "D/N"            TO PAR-C-ABR(2)
               MOVE 1                TO PAR-C-ACT(2)
               MOVE 12               TO PAR-C-CODE(2)
               PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
               ADD 1                 TO WS-PARKEY
               PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT
               MOVE "Payment (G/L)"  TO PAR-C-DESC(2)
               MOVE "Pmt"            TO PAR-C-ABR(2)
               MOVE 0                TO PAR-C-ACT(2)
               MOVE 14               TO PAR-C-CODE(2)
               PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH            TO WS-BATCH.
           IF G-YEAR-END = ZERO
               MOVE 2                TO G-YEAR-END
               PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
      *    *************************************************************
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *    *************************************************************
             MOVE 1                  TO WS-S1.
             MOVE G-YEAR-END         TO WS-S2 WS-Y-END.
             MOVE G-LAST-YE          TO WS-L-END.
           IF WS-L-END = ZERO
               SUBTRACT 1 FROM W05-CY GIVING WS-L-END.
           IF G-OPEN = ZERO
               MOVE WS-Y-END         TO G-OPEN.
             MOVE G-OPEN             TO WS-OPEN.
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
               WHEN 7          MOVE W02-DEPART  TO WS-FILE
                               MOVE DPT-CODE    TO WS-KEYX
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
               WHEN 26         MOVE W02-GLJRN   TO WS-FILE
                               MOVE WS-GLJKEY   TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN  TO WS-FILE
                               MOVE TAX-KEY     TO WS-KEYX
               WHEN 37         MOVE W02-SHARED  TO WS-FILE
                               MOVE WS-SHARED   TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF  TO WS-FILE
                               MOVE XFR-KEY     TO WS-KEYX
               WHEN OTHER      MOVE "ERROR"     TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
