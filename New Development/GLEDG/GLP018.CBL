      $set LINKCOUNT"468" GNT"GLP018.GNT"
      ******************************************************************
      *                                                                *
      *    ******   **        *******     ****       **      ******    *
      *   **    **  **        **    **   **  **     ***     **    **   *
      *   **        **        **    **  **    **     **     **    **   *
      *   **        **        *******   **    **     **      ******    *
      *   **   ***  **        **        **    **     **     **    **   *
      *   **    **  **        **         **  **      **     **    **   *
      *    ******   ********  **          ****     ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     P O S T   D A T E D   C H E Q U E S                        *
      *                                                                *
      *       Version 9.04.02 - June 2017                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             GLP018.
       AUTHOR.                 J.W. LEMMON (APAC).
       DATE-WRITTEN.           MARCH 1984.

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

       COPY "GACCNT.SL".

       COPY "GLTRAN.SL".

       COPY "GLJRN.SL".

       COPY "GPDCHQ.SL".

       COPY "PARAM.SL".

       COPY "RECOVER.SL".

       COPY "SHARED.SL".

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

       COPY "GACCNT.FDE".

       COPY "GLTRAN.FDE".

       COPY "GLJRN.FD".

       COPY "GPDCHQ.FDE".

       COPY "PARAM.FDE".

       COPY "RECOVER.FD".

       COPY "SHARED.FDE".

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
       77  WS-DEPKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GLKEY                PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-GINKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CTRKEY               PIC  9(04)    COMP-5 VALUE 1.
       77  WS-CREKEY               PIC  9(04)    COMP-5 VALUE 1.
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
       77  WS-PGE-LENGTH           PIC  9(02)           VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)           VALUE 62.
       77  WS-COND                 PIC  X(06).
       77  WS-NORM                 PIC  X(06).
       77  WS-EXPP                 PIC  X(06).
       77  WS-ECAN                 PIC  X(06).
       77  WS-8LPI                 PIC  X(06).
       77  WS-6LPI                 PIC  X(06).
       77  WS-10CPI                PIC  X(06).
       77  WS-12CPI                PIC  X(06).
       77  WS-17CPI                PIC  X(06).
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  WS-HDTE                 PIC  X(05)           VALUE "DATE".
       77  WS-HACNO                PIC  X(06)           VALUE "A/C NO".
       77  WS-HQNT                 PIC  X(05)           VALUE "QUANT".
       77  WS-HDESC                PIC  X(11)           VALUE "DESCRIPTION".
       77  WS-HCOST                PIC  X(07)           VALUE "C/PRICE".
       77  WS-HNAME                PIC  X(04)           VALUE "NAME".
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
       77  WS-SELECT               PIC  X(13)           VALUE "Select Option".
       77  WS-H1                   PIC  X(06)           VALUE "DATE:".
       77  WS-H3                   PIC  X(07)           VALUE "BATCH:".
       77  WS-H4                   PIC  X(07)           VALUE "PAGE:".
       77  WS-ER1                  PIC  X(08)           VALUE "Account".
       77  WS-ER3                  PIC  X(05)           VALUE "Date".
       77  WS-ER4                  PIC  X(06)           VALUE "Value".
       77  WS-ER5                  PIC  X(07)           VALUE "Cheq no".
       77  WS-ER6                  PIC  X(11)           VALUE "No record".
       77  WS-ER7                  PIC  X(13)           VALUE "Group heading".
       77  TODAY-DDMMYY            PIC  9(08)   COMP-5.
       77  WS-USUB                 PIC  9(04)   COMP-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C =  Clear comment line (50)
      *         E =  Clear lines (any line or lines between 5 and 46)
      *         H =  Change heading
      *         S =  Clear the screen and display headings
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "GLP018".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "GENERAL LEDGER -POST DATED CHEQUES".
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

       COPY "LEDTRF.WS".

       01  W00-TRAN-CODES.
           03  W00-C-ABR   PIC  X(03).
           03  W00-C-DESC  PIC  X(15).
           03  W00-C-ACT   PIC  9(01).
           03  W00-C-CODE  PIC  9(02)    COMP-5.
           03  W00-C-GL    PIC  9(06)    COMP-3.
           03  W00-C-DAY   PIC  9(06)    COMP-5.
           03  W00-C-VAL   PIC S9(09)V99 COMP-3.
           03  W00-C-MTD   PIC  9(06)    COMP-5.
           03  W00-C-MTDV  PIC S9(09)V99 COMP-3.
           03  W00-C-YTD   PIC  9(06)    COMP-5.
           03  W00-C-YTDV  PIC S9(09)V99 COMP-3.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

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

           03  R-L1                              REDEFINES W02-PRN-LINE.
               05  R-DET           PIC  X(140).

           03  RG-L2                             REDEFINES W02-PRN-LINE.
               05  RG-H1           PIC  X(06).
               05  RG-DTE1         PIC  Z9/99/99.
               05  FILLER          PIC  X(25).
               05  RG-H2           PIC  X(40).
               05  FILLER          PIC  X(15).
               05  RG-H3           PIC  X(07).
               05  RG-BTCH         PIC  ZZZZ9.
               05  FILLER          PIC  X(17).
               05  RG-H4           PIC  X(06).
               05  RG-PGE          PIC  ZZ9.

           03  R-L2A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(46).
               05  R-HEAD          PIC  X(40).
               05  FILLER          PIC  X(46).

           03  R-L3A                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(05).
               05  I-H1            PIC  X(11).
               05  I-H2            PIC  X(34).
               05  I-H3            PIC  X(44).
               05  I-H4            PIC  X(11).
               05  I-H5            PIC  X(27).

           03  R-L4                              REDEFINES W02-PRN-LINE.
               05  G-H1            PIC  X(10).
               05  G-SUPP          PIC  X(05).
               05  FILLER          PIC  X(02).
               05  G-NAMER         PIC  X(40).
               05  FILLER          PIC  X(23).

           03  R-L4A                             REDEFINES W02-PRN-LINE.
               05  G-H3            PIC  X(05).
               05  G-INVN          PIC  Z(07)9.
               05  FILLER          PIC  X(01).
               05  G-H4            PIC  X(08).
               05  FILLER          PIC  X(01).
               05  G-INVD          PIC  Z9/99/99.
               05  G-H5            PIC  X(06).
               05  G-RAIL          PIC  Z(04)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H6            PIC  X(06).
               05  G-DISC          PIC  Z(04)9.99-.
               05  FILLER          PIC  X(01).
               05  G-H7            PIC  X(06).
               05  G-TOTAL         PIC  Z(06)9.99-.

           03  RG-L5                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(65).
               05  RG-H16          PIC  X(10).
               05  FILLER          PIC  X(01).
               05  RG-H17          PIC  X(15).
               05  RG-H18          PIC  X(10).
               05  FILLER          PIC  X(31).

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

           03  R-L6                              REDEFINES W02-PRN-LINE.
               05  D-H1            PIC  X(06).
               05  D-H2            PIC  X(10).
               05  D-H3            PIC  X(08).
               05  D-H4            PIC  X(10).
               05  D-H5            PIC  X(08).
               05  D-H6            PIC  X(13).
               05  D-H7            PIC  X(13).
               05  D-H8            PIC  X(12).

           03  R-L6A                             REDEFINES W02-PRN-LINE.
               05  D-ACNO          PIC  X(05).
               05  FILLER          PIC  X(02).
               05  D-NAME          PIC  X(73).

           03  R-L6B                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(06).
               05  D-DNO           PIC  Z(05)9.
               05  FILLER          PIC  X(02).
               05  D-DNDTE         PIC  Z9/99/99.
               05  FILLER          PIC  X(01).
               05  D-INV           PIC  Z(07)9.
               05  FILLER          PIC  X(01).
               05  D-IDTE          PIC  Z9/99/99.
               05  FILLER          PIC  X(01).
               05  D-COST          PIC  Z(07)9.99-.
               05  FILLER          PIC  X(01).
               05  D-SELL          PIC  Z(08)9.99-.
               05  FILLER          PIC  X(01).
               05  D-30            PIC  X(02).
               05  D-60            PIC  X(02).
               05  D-90            PIC  X(02).
               05  D-120           PIC  X(02).
               05  D-150           PIC  X(02).
               05  D-180           PIC  X(02).
									    
           03  R-L7                              REDEFINES W02-PRN-LINE.
               05  CH-H1           PIC  X(07).
               05  CH-H2           PIC  X(32).
               05  CH-H3           PIC  X(08).
               05  CH-H4           PIC  X(13).
               05  CH-H5           PIC  X(08).
               05  CH-H6           PIC  X(12).

           03  R-L7A                             REDEFINES W02-PRN-LINE.
               05  CH-ACNO         PIC  X(05).
               05  FILLER          PIC  X(01).
               05  CH-NAME         PIC  X(29).
               05  FILLER          PIC  X(02).
               05  CH-DATE         PIC  Z9/99/99.
               05  FILLER          PIC  X(02).
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

           03  RG-L8                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  RG-H5           PIC  X(11).
               05  RG-H6           PIC  X(05).
               05  FILLER          PIC  X(01).
               05  RG-H7           PIC  X(13).
               05  RG-H8           PIC  X(05).
               05  FILLER          PIC  X(06).
               05  RG-H9           PIC  X(09).
               05  RG-H10          PIC  X(12).
               05  RG-H11          PIC  X(13).
               05  FILLER          PIC  X(01).
               05  RG-H12          PIC  X(09).
               05  RG-H13          PIC  X(13).
               05  FILLER          PIC  X(08).
               05  RG-H14          PIC  X(14).
               05  RG-H15          PIC  X(09).

           03  R-L9                              REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  R-ACC           PIC  9(04)/99.
               05  FILLER          PIC  X(04).
               05  R-ANME          PIC  X(24).
               05  FILLER          PIC  X(04).
               05  R-DTE2          PIC  Z9/99/99.
               05  FILLER          PIC  X(04).
               05  R-REFX.
                   07  R-REF       PIC  Z(07).
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
               05  R2-DTE1         PIC  Z9/99/99.
               05  FILLER          PIC  X(10).
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
               05  FILLER          PIC  X(04).
               05  R2-DTE2         PIC  Z9/99/99.
               05  FILLER          PIC  X(04).
               05  R2-REF          PIC  Z(07).
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

           03  PD-L2                             REDEFINES W02-PRN-LINE.
               05  PD-ACNO         PIC  X(05).
               05  FILLER          PIC  X(01).
               05  PD-NAME         PIC  X(40).
               05  FILLER          PIC  X(01).
               05  PD-BANK         PIC  9(06).
               05  FILLER          PIC  X(01).
               05  PD-CRED         PIC  9(06).
               05  FILLER          PIC  X(01).
               05  PD-NAR          PIC  X(40).
               05  FILLER          PIC  X(01).
               05  PD-DATE         PIC  Z9/99/99.
               05  FILLER          PIC  X(01).
               05  PD-REF          PIC  Z(06)9.
               05  FILLER          PIC  X(01).
               05  PD-VALUE        PIC  Z(08)9.99.
               05  FILLER          PIC  X(01).

           03  PD-L3                             REDEFINES W02-PRN-LINE.
               05  PG-DATE         PIC  Z9/99/9999.
               05  FILLER          PIC  X(01).
               05  PG-REF          PIC  Z(06)9.
               05  FILLER          PIC  X(01).
               05  PG-D-VALUE      PIC  Z(10)9.99.
               05  FILLER          PIC  X(01).
               05  PG-C-VALUE      PIC  Z(10)9.99-.
               05  FILLER          PIC  X(02).
               05  PG-GL-AC        PIC  9(04)/99.
               05  FILLER          PIC  X(02).
               05  PG-AC-NAME      PIC  X(30).
               05  FILLER          PIC  X(02).
               05  PG-PAYEE        PIC  X(40).

           03  PD-L3                             REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(03).
               05  PG-H1           PIC  X(08).
               05  PG-H2           PIC  X(17).
               05  PG-H3           PIC  X(14).
               05  PG-H4           PIC  X(09).
               05  PG-H5           PIC  X(09).
               05  PG-H6           PIC  X(32).
               05  PG-H7           PIC  X(40).

       COPY "W05.GL".

       COPY "W05.VAT".

       01  W10-H2.
           03  FILLER          PIC X(27).
           03  W10-HEAD        PIC X(13).

       COPY "W15.GL".

          03  W15-COST        PIC S9(09)V99.
           03  W15-TCOST       PIC S9(09)V99 COMP-3.
           03  W15-SETT        PIC 9(06).
           03  W15-SET         REDEFINES W15-SETT.
               05  W15-30      PIC 9(01).
               05  W15-60      PIC 9(01).
               05  W15-90      PIC 9(01).
               05  W15-120     PIC 9(01).
               05  W15-150     PIC 9(01).
               05  W15-180     PIC 9(01).
           03 W15-PAY          REDEFINES W15-SETT.
              05  W15-MONTH    PIC 9(01) OCCURS 6.
           03  W15-INVN        PIC 9(08)    COMP-3.
           03  W15-INVD        PIC 9(06)    COMP-3.
           03  W15-GRNN        PIC 9(06)    COMP-3.
           03  W15-GRND        PIC 9(06)    COMP-3.
           03  W15-CHQVAL      PIC S9(09)V99 COMP-3.
           03  W15-DISCOUNT    PIC S9(09)V99 COMP-3.
           03  W15-RAIL        PIC S9(09)V99 COMP-3.
           03  W15-INCL        PIC S9(09)V99 COMP-3.
           03  W15-EXCL        PIC S9(09)V99 COMP-3.
           03  W15-TXBL        PIC S9(09)V99 COMP-3.
           03  W15-NTXBL       PIC S9(09)V99 COMP-3.
           03  W15-VAT         PIC S9(09)V99 COMP-3.
           03  W15-LVAT        PIC S9(09)V99 COMP-3.
           03  W15-TRNTOT      PIC S9(09)V99 COMP-3.

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

       COPY "W20.WS".

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
          03  W50-HEAD        PIC X(40).
       01  W55-DEL.
           03  W55-MNTH        PIC X(03).
           03  FILLER          PIC X(01)   VALUE ",".
           03  W55-YY          PIC 9(02).

       01  W60-H2.
          03  W60-DESC        PIC X(20)   VALUE
             "G/LEDGER AUDIT TRIAL".

       01  W70-ACTS.
           03  W70-BRANCH          PIC  9(04)    COMP-5.

       01  W90-COMP.
           03  W90-CNAME       PIC X(40).
       01  W90-TRAN.
           03  W90-CODE        PIC  9(02).
           03  W90-RESULT      PIC  9(04)    COMP-5.
           03  W90-TRNKEY      PIC  9(04)    COMP-5.
           03  W90-TS          PIC  9(02)    COMP-5.

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
          03  LINE  2 COLUMN 27 FOREGROUND-COLOR Grey
     HIGHLIGHT
     VALUE "G/LEDGER POST DATED CHEQUES".

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


       01  S-CHEQUE.
      *    03  LINE  2 COLUMN 33 FOREGROUND-COLOR Grey HIGHLIGHT
      *     VALUE "ISSUE PD CHEQUE".
    03  LINE  4 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
    VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 65 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "¿".
    03  LINE  5 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Date    ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "        ".
    03        COLUMN 32 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     VALUE "               ".
    03        COLUMN 47 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Chq.No.".
    03        COLUMN 54 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     VALUE "           ".
    03        COLUMN 65 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "³".
    03  LINE  6 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Payee   ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
       "                                        ".
    03        COLUMN 64 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE " ³".
    03  LINE  7 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Bank Ac ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "      ".
    03        COLUMN 30 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
      ":      ".
    03        COLUMN 61 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "    ³".
    03  LINE  8 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Narrat. ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
       "             ".
    03        COLUMN 64 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE " ³".
    03  LINE  9 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "G/L. Ac ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "      ".
    03        COLUMN 30 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
      ":      ".
    03        COLUMN 61 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "    ³".
    03  LINE 10 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Narrat. ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
       "             ".
    03        COLUMN 64 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE " ³".
    03  LINE 11 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "³ ".
    03        COLUMN 16 FOREGROUND-COLOR Brown HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE "Amount  ".
    03        COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta
     VALUE "              ".
    03        COLUMN 38 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta VALUE
     "             ³".
    03  LINE 12 COLUMN 14 BACKGROUND-COLOR Magenta FOREGROUND-COLOR Black
     VALUE "À".
    03        COLUMN 15 FOREGROUND-COLOR Magenta HIGHLIGHT
     BACKGROUND-COLOR Magenta
    VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  SA-CHEQUE.
    02  SA-CHQ-DTE.
    03  LINE  5 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC 99/99/99
     USING W15-DATE AUTO.
    02  SA-CHQ-NO.
    03  LINE  5 COLUMN 55 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC ZZZZZZZZ
     USING W15-CHQ-NO AUTO.

    02  SA-CHQ-PAY.
    03  LINE  6 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(40)
     USING W15-PAYEE AUTO.

    02  SA-CHQ-BANK.
    03  LINE  7 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC 9(06)
     USING W15-BANK AUTO.

    02  SD-CHQ-BNAME.
    03  LINE  7 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(30)
     FROM W15-BANK-NAME.

    02  SA-CHQ-NAR-1.
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(40)
     USING W15-NARRATIVE-1 AUTO.

    02  SA-CHQ-GLAC.
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC 9(06)
     USING W15-GL-AC AUTO.

    02  SD-CHQ-GNAME.
    03  LINE  9 COLUMN 31 FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(30)
     FROM W15-GLAC-NAME.

    02  SA-CHQ-NAR-2.
    03  LINE 10 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC X(40)
     USING W15-NARRATIVE-2 AUTO.

    02  SA-CHQ-VAL.
    03  LINE 11 COLUMN 24 FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Magenta PIC ZZZ,ZZZ,ZZ9.99
     USING W15-VALUE AUTO.

      *COPY "LEDLUP.CRT".

       01  S-LEDG.
           03  LINE 23 COLUMN  2 VALUE "Balance :".
    03        COLUMN 12 FOREGROUND-COLOR Cyan HIGHLIGHT
     PIC Z(08)9.99CR USING G-BAL.

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
           IF LS0-GLLEV < 3
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA11E.
             PERFORM ZA000.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
             PERFORM OPEN-PRINTER.
             MOVE "P"                TO WS-COMMAND.
             MOVE 0                  TO WS-ADVANCE.
             MOVE 3                  TO W02-PRN-TYPE.
             PERFORM CALL-PRINTUTL.
             PERFORM HA000.
             MOVE "C"                TO WS-COMMAND.
             PERFORM CALL-PRINTUTL.

       AA11E.
             EXIT PROGRAM.

       AA20-LOOKUP     SECTION.
       AA20.
             PERFORM SAVE-SCREEN.
             PERFORM SCREEN-CONTENTS.
             MOVE PRG-GLED-LUP       TO PRG-NAME.
             CALL PRG-NAME USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W15-GLEDGER
                  ON EXCEPTION GO TO AA25
             END-CALL.
             CANCEL PRG-GLED-LUP.
             GO TO AA28.
       
       AA25.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA28.
             PERFORM RESTORE-SCREEN.

       AA29.
             EXIT.

       AA50.
      *
      *    ****    Print the PD Cheques posted audit trail HEADING
      *
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO R-DET.
            MOVE WS-H1   TO RG-H1.
            MOVE W90-CNAME  TO RG-H2.
            MOVE WS-H3   TO RG-H3.
            MOVE WS-H4   TO RG-H4.
            ADD 1   TO WS-PAGE.
            MOVE WS-PAGE  TO RG-PGE.
            MOVE WS-BATCH  TO RG-BTCH.
            MOVE W05-TODAY  TO RG-DTE1.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
            MOVE W50-H4  TO R-HEAD.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.

       AA65.
             EXIT.
      *
      *    ****    Print the G/Ledger PD Cheques HEADING
      *
       AA50GP.
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
            MOVE W50-H4  TO R-HEAD.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO R-DET.
      MOVE "DATE"  TO PG-H1.
      MOVE "CHQ.NO."  TO PG-H2.
      MOVE "DEBIT"  TO PG-H3.
      MOVE "CREDIT"  TO PG-H4.
      MOVE "G/L ACT"  TO PG-H5.
      MOVE "ACCOUNT NAME" TO PG-H6.
      MOVE "PAYEE/NARRATIVE"
     TO PG-H7.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.

       AA55GP.
      EXIT.

       COPY "AA900.ALM".

       COPY "FUNCTION.CRT".

       COPY "LOCKED.REC".

       COPY "CLEAR.CRT".

      *
      *    ****    R E A D   F I L E S   R O U T I N E S
      *
       AB000           SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "GACCNT.RD2".

       COPY "GLTRAN.RD".

       COPY "GLJRN.RD".

       COPY "GPDCHQ.RD".

       COPY "SHARED.RD".

      *                                                                 
      *     R E W R I T E   &   W R I T E   F I L E S   R O U T I N E S 
      *                                                                 
       AC000        SECTION.

       COPY "CONTROL.WR".

       COPY "PARAM.WR".

       COPY "GACCNT.WR".

       COPY "GLTRAN.WR".

       COPY "GLJRN.WR".

       COPY "GPDCHQ.WR".

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
      ADD WS-VALUE  TO G-OBAL.
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
      *AY01.
      *      MOVE 01                 TO REC-FILE.
      *      MOVE WS-PARKEY          TO REC-KEY.
      *      MOVE ZERO               TO REC-TYPE
      *      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-RECORD1        TO REC-PARAM.
      *      GO TO AY50.

       AY12.
             MOVE 12             TO REC-FILE.
             MOVE WS-GLKEY       TO REC-KEY.
             PERFORM READ-GACCNT-LOCK THRU READ-GACCNT-EXIT.
             MOVE G-REC          TO REC-GACCNT.
             GO TO AY50.

       AY13.
             MOVE 13             TO REC-FILE.
             MOVE WS-GTRKEY      TO REC-KEY.
             MOVE T-REC          TO REC-GLTRAN.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

       AY45.
            MOVE 45   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE PDT-REC  TO REC-GPDCHQ.
             GO TO AY50.

       AY49.
             MOVE 99 TO REC-FILE.
             MOVE ZERO TO REC-KEY.
             MOVE SPACES TO REC-DETAIL.
             PERFORM AY50.
             ADD 1 TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO TO WS-RECOVER.
             GO TO AY59.

       AY50.
             ADD 1 TO WS-RECOVER.
             MOVE WS-RECOVER TO WS-RECKEY.
             MOVE WS-TRANS TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY (22, 12) "Write error Recovery file - Status "
                        WITH FOREGROUND-COLOR Brown HIGHLIGHT
                        WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 5051
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 5051
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.
             GO TO AY60.

       AY61.
            MOVE "GL"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the LEDGER control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
             MOVE 4              TO WS-NETKEY.
            PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
            GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "CG" OR "GL"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
     GO TO AY62.

       AY62-CHECK.
    IF WS-WAIT > ZERO
        COMMIT
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 5072
   WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK
        GO TO AY60.
            DISPLAY SPACE AT 5072
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Cyan.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY65.
             MOVE 4              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BATCH        TO WS-BATCH.
           IF G-BATCH = 99999
               MOVE 1            TO G-BATCH
           ELSE
               ADD 1             TO G-BATCH.
             PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             GO TO AY999.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *     NETWORK record 2.
      *
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
            PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
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
             DIVIDE W15-YY BY 4 GIVING W20-RESULT.
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
             MOVE W15-MM TO WS-PERIOD.
    IF W15-YY > W05-YY
        GO TO BA10.
    IF W15-YY < W05-YY
        GO TO BA20.
    IF W05-MM = W15-MM
       GO TO BA999.
    IF W15-MM > W05-MM
        GO TO BA10.
    IF W15-MM > WS-Y-END
        GO TO BA999.
           IF W15-YY > WS-L-END
        GO TO BA999.
             ADD 12 TO WS-PERIOD
      GO TO BA999.
       BA20.
           IF W15-YY > WS-L-END
        GO TO BA999.
             ADD 12 TO WS-PERIOD.
       BA999.
             EXIT.
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
             DIVIDE W15-YY BY 4 GIVING W20-RESULT.
           IF W20-DEC > ZERO
              GO TO HA110.
            GO TO HA999.

       HA110.
             MOVE 1 TO WS-ERROR.

       HA999.
             EXIT.
      /
      *    ****    C A P T U R E   P O S T   D A T E D  C H E Q U E S
      *
       HB000        SECTION 5.
       HB00.
            MOVE "PD. CHEQUES ISSUED (GENERAL LEDGER)"
                                 TO W50-HEAD.
             MOVE WX-BNKGL       TO W15-BANK.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT
        WS-PAGE.

       HB05.
             PERFORM ERASE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 16   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S-CHEQUE.
      PERFORM SCREEN-SHADOW.
      DISPLAY "POST DATED CHEQUES (G/L)" AT 0229
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE SPACES  TO W15-NARRATIVE-1 W15-PAYEE
        W15-NARRATIVE-2 W15-CRED
        W15-BANK-NAME   W15-GLAC-NAME
        W15-CNAME.
      MOVE ZERO   TO W15-VALUE W15-CHQ-NO
        W15-GL-AC.
      MOVE SPACE   TO WS-CRD-CHQ.
             MOVE W70-BNKGL      TO W15-BANK.
      *
      *      ****    ***   *****  *****
      *      *  *  * *    *   *
      *      *  *  *****    *   ***
      *      *  *  * *    *   *
      *      ****   * *    *   *****
      *
       HB10.
      ACCEPT SA-CHQ-DTE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        GO TO HB990
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB10
        END-EVALUATE.
            MOVE W15-DATE  TO W10-EDTE.
             PERFORM CHECK-DATE.
           IF WS-ERROR = 1
              MOVE "Date"  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO HB10.
      *
      *    ****    I N C L U D E   C E N T U R Y   I N  D A T E
      *
      MOVE W22-DTE1  TO W22-YMD
        W22-DTE2.
    IF W22-YY2 > 55
      *
      *    ****   ASSUME 1900
      *
        MOVE 19   TO W22-C
    ELSE
      *
      *    ****   ASSUME 2000
      *
        MOVE 20   TO W22-C.
      *
      *    ****    C H E C K   I F   T H E   C H E Q U E   I S
      *     P O S T   D A T E D
      *
    IF NOT (W22-CYMD > W12-T-CYMD)
        MOVE "Cheque is not Post-Dated"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB10.
      *
      *     ***   *   *  *****  ***   *   *  *****    *   *   ***
      *    *   *  *   *  * *   *  *   *  *        **  *  *   *
      *    *   *****  *** *   *  *   *  ***      * * *  *   *
      *    *   *  *   *  * *  **  *   *  *        *  **  *   *
      *     ***   *   *  *****  **** ***   *****    *   *   ***
      *
       HB15.
      ACCEPT SA-CHQ-NO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        GO TO HB10
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB15
        END-EVALUATE.
    IF W15-CHQ-NO = ZERO
        MOVE "Cheque is not Post-Dated"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB15.
      *
      *    ****    ***  *   * *****  *****
      *    *   *  *   *  *   * *      *
      *    ****   *****   * * ***    ***
      *    *   *   *    * *      *
      *    *   *   *    * *****  *****
      *
       HB20.
      ACCEPT SA-CHQ-PAY.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        GO TO HB15
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB20
        END-EVALUATE.
    IF W15-PAYEE = SPACES
        MOVE "Payee may not be left BLANK"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB20.
    IF NOT (WS-BATCH = "Y")
        DISPLAY WS-NOT-APP AT 0724
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       HBCKGROUND-COLOR Magenta
        DISPLAY WS-NOT-APP AT 0824
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       HBCKGROUND-COLOR Magenta
        DISPLAY WS-NOT-APP AT 0924
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       HBCKGROUND-COLOR Magenta
        DISPLAY WS-NOT-APP AT 1024
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       HBCKGROUND-COLOR Magenta
        GO TO HB55.
    IF NOT (W15-BANK = ZERO)
        MOVE W15-BANK  TO G-AC
              PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
        IF NOT (WS-F-ERROR = 8)
     MOVE G-NAME  TO W15-BANK-NAME
     DISPLAY SD-CHQ-BNAME.
      *
      *    *    ***   *** *   *  *   *  ****
      *    *   *   *  *   * *  *   *   *  *   *
      *    *   *   *  *   * ***    *   *  ****
      *    *   *   *  *   * *  *   *   *  *
      *    *****   ***   *** *   * ***   *
      *
       HB25.
      DISPLAY "F1" AT 5002
        WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "=Help,"
        WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F2" WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape to exit"
        WITH HBCKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      *
      *    ****    ***  *   * *   *
      *    *   *  *   *  **  * *  *
      *    ****   *****  * * * ***
      *    *   *  *   *  *  ** *  *
      *    ****   *   *  *   * *   *
      *
       HB30.
      ACCEPT SA-CHQ-BANK.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        PERFORM CLEAR-L50
        GO TO HB20
   WHEN F1-KEY
        PERFORM HELP-ROUTINE
   WHEN F2-KEY
        MOVE ZERO  TO W15-ACCOUNT
        PERFORM AA350
   WHEN OTHER
        PERFORM AA900-ALARM
        GO TO HB30
        END-EVALUATE
               IF W15-ACCOUNT = ZERO
           GO TO HB30
        END-IF
        MOVE W15-ACCOUNT  TO W15-BANK
        DISPLAY SA-CHQ-BANK.
      PERFORM CLEAR-L50.
          IF W15-BANK = ZERO
              GO TO HB20.
      MOVE W15-BANK  TO W15-ACCOUNT
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO HB30.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO HB25.
      MOVE G-NAME  TO W15-BANK-NAME.
      DISPLAY SD-CHQ-BNAME.
      *
      *    *   *   ***  **** **** ***   *****  ***  *   *  *****
      *    **  *  *   *  *   * *   *  *   * *     *   *   *  *
      *    * * *  *****  **** ****   ***** *     *   *   *  ***
      *    *  **  *   *  *  * *  *   *   * *     *    * *  *
      *    *   *  *   *  *   * *   *  *   * *    ***    *  *****
      *
       HB35.
      ACCEPT SA-CHQ-NAR-1.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        GO TO HB25
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB35
        END-EVALUATE.
    IF W15-NARRATIVE-1 = SPACES
        MOVE "Narrative may not be left BLANK"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB35.
    IF NOT (W15-GL-AC = ZERO)
        MOVE W15-GL-AC  TO G-AC
              PERFORM READ-GACCNT THRU READ-GACCNT-EXIT
        IF NOT (WS-F-ERROR = 8)
     MOVE G-NAME  TO W15-GLAC-NAME
     DISPLAY SD-CHQ-GNAME.

       HB40.
      PERFORM HB25.
      *
      *     ***       *  *       ***    ***    ***   *****
      *    *   *     *  *      *  *  * *  *   *    *
      *    *     *  * ***  *****  *    *     *
      *    *  **   *  *      *  *  * *  *   *    *
      *     ***   *  *****      *  *   ***    ***     *
      *
       HB45.
      ACCEPT SA-CHQ-GLAC.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        PERFORM CLEAR-L50
        GO TO HB35
   WHEN F1-KEY
        PERFORM HELP-ROUTINE
   WHEN F2-KEY
        MOVE ZERO  TO W15-ACCOUNT
        PERFORM AA350
   WHEN OTHER
        PERFORM AA900-ALARM
        GO TO HB45
        END-EVALUATE
               IF W15-ACCOUNT = ZERO
           GO TO HB45
        END-IF
        MOVE W15-ACCOUNT  TO W15-GL-AC
        DISPLAY SA-CHQ-GLAC.
      PERFORM CLEAR-L50.
          IF W15-GL-AC = ZERO
              GO TO HB35.
       MOVE W15-GL-AC  TO W15-ACCOUNT
           IF W15-ASER = ZERO
              MOVE WS-ER7  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO HB40.
            MOVE W15-ACCOUNT  TO G-AC.
             PERFORM READ-GACCNT THRU READ-GACCNT-EXIT.
          IF WS-F-ERROR = 8
              MOVE WS-ER6  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO HB45.
      MOVE G-NAME  TO W15-GLAC-NAME.
      DISPLAY SD-CHQ-GNAME.
      *
      *    *   *   ***  **** **** ***   *****  ***  *   *  *****
      *    **  *  *   *  *   * *   *  *   * *     *   *   *  *
      *    * * *  *****  **** ****   ***** *     *   *   *  ***
      *    *  **  *   *  *  * *  *   *   * *     *    * *  *
      *    *   *  *   *  *   * *   *  *   * *    ***    *  *****
      *
       HB50.
    IF W15-NARRATIVE-2 = SPACES
        MOVE W15-NARRATIVE-1
     TO W15-NARRATIVE-2.
      ACCEPT SA-CHQ-NAR-2.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        GO TO HB40
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB50
        END-EVALUATE.
    IF W15-NARRATIVE-2 = SPACES
        MOVE "Narrative may not be left BLANK"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB50.
      *
      *    *   *   ***  * *   *  *****
      *    *   *  *   *  * *   *  *
      *    *   *  *****  * *   *  ***
      *     * *   *   *  * *   *  *
      *      *   *   *  *****  ***   *****
      *
       HB55.
      ACCEPT SA-CHQ-VAL.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
        IF NOT (WS-BATCH = "Y")
     GO TO HB20
        END-IF
        GO TO HB50
   WHEN OTHER  PERFORM AA900-ALARM
        GO TO HB55
        END-EVALUATE.
    IF W15-VALUE < 2.00
        MOVE "Cheque amount LESS than R2.00"
     TO WS-ERR-STRING
        PERFORM ERROR-SETUP THRU ERROR-POS
        MOVE 15   TO SLIN
              PERFORM ERROR-DISPLAY THRU ERROR-EXIT
        GO TO HB55.
      MOVE 14   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        GO TO HB10.
    IF WS-PAGE = ZERO
        PERFORM AA50GP.
      *
      *    ****    I N C L U D E   C E N T U R Y   I N  D A T E
      *
      MOVE W22-DTE1  TO W22-YMD
        W22-DTE2.
    IF W22-YY2 > 55
      *
      *    ****   ASSUME 1900
      *
        MOVE 19   TO W22-C
    ELSE
      *
      *    ****   ASSUME 2000
      *
        MOVE 20   TO W22-C.
      *
      *    ****    S W I T C H  D A T E   T O  DDMMCCYY
      *
      MOVE W22-CY  TO W22-CCYY
      MOVE W22-M   TO W22-MM
      MOVE W22-D   TO W22-DD
      MOVE W22-DMCY  TO W02-PRN-DATE.
      MOVE W15-PAYEE  TO W02-PRN-PAY.
      MOVE W15-VALUE  TO W02-PRN-VAL.
    IF WS-PAGE = ZERO
        PERFORM AA50GP.
      *
      *    ****    A D D   C H E Q U E  T O   P . D .  C H E Q U E
      *     F I L E
      *
      INITIALIZE GPD-REC.
      MOVE W22-YMD  TO GPD-DATE.
      MOVE W22-DMCY  TO PG-DATE.
      MOVE W15-REFNO  TO PG-REF GPD-REFER.
      MOVE W15-BANK  TO PG-GL-AC GPD-BANK.
      MOVE W15-BNKNME  TO PG-AC-NAME.
      MOVE W15-CRED  TO GPD-GL.
      MOVE W15-NAR  TO GPD-NAR1.
      MOVE W15-NAR2  TO GPD-NAR2.
      MOVE W15-TCOST  TO PG-D-VALUE GPD-VAL.
            MOVE W15-PAYEE  TO PG-PAYEE GPD-PAYEE.
      ADD W15-TCOST  TO W20-TOTAL.
      PERFORM WRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT.
    IF WS-F-ERROR = 45
        MOVE SPACES  TO WS-ERR-STRING
        STRING "Cheque duplicated: "
       DELIMITED SIZE
         W15-REFNO DELIMITED SIZE
     INTO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA35.
      PERFORM AY60 THRU AY999.
      MOVE 1   TO WS-ACTION.
      PERFORM AY45 THRU AY50.
      PERFORM AY70 THRU AY999.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.
    IF W15-NAR  = W15-NAR2
        GO TO HB60.
      MOVE W15-NAR  TO PG-PAYEE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO R-DET.

       HB60.
      MOVE W15-CRED  TO PG-GL-AC.
      MOVE W15-BNKNME  TO PG-AC-NAME.
      MOVE W15-NAR2  TO PG-PAYEE.
      COMPUTE PG-C-VALUE = W15-TCOST * -1.
      PERFORM DELETE-CHQBATCH-REC THRU WRITE-CHQBATCH-EXIT.
          IF W02-LINAGE < (W02-PRN-LENGTH - 5)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        PERFORM HA50.
      MOVE SPACES  TO R-DET.

       HB65.
      MOVE "'C'ontinue or 'E'xit  [ ]"
     TO WS-ERR-MES.
      MOVE 15   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT.
    IF NOT (WS-OPTION = "C" OR "E")
        GO TO HB65.
    IF WS-OPTION = "E"
        GO TO HB990.
      GO TO HB05.

       HB990.
    IF WS-PAGE = ZERO
       GO TO HB999.
      MOVE SPACES  TO R-DET.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE "****  E N D   O F   R E P O R T  ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PG-D-VALUE.
            MULTIPLY -1 BY W20-TOTAL
     GIVING PG-C-VALUE.
      PERFORM CALL-PRINTUTL.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.

       HB999.
      EXIT.

      /
       HC000        SECTION 5.
       HC00.
            MOVE "PD. CHEQUES AMENDED (GENERAL LEDGER)"
                                 TO W50-HEAD.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT
        WS-PAGE.

       HC05.
            PERFORM ERASE-SCREEN.
      MOVE 5   TO SHADE-ROW.
      MOVE 16   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
      DISPLAY S-CHEQUE.
      PERFORM SCREEN-SHADOW.
      DISPLAY "AMEND POST DATED CHEQUES (G/L)" AT 0226
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      MOVE SPACES  TO W15-NARRATIVE-1 W15-PAYEE
        W15-NARRATIVE-2 W15-CRED
        W15-BANK-NAME   W15-GLAC-NAME
        W15-CNAME.
      MOVE ZERO   TO W15-VALUE W15-CHQ-NO
        W15-GL-AC.
      MOVE SPACE   TO WS-CRD-CHQ.

       HC10.
      DISPLAY "Key in the cheque number" AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT S14 AT 0619.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC70
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC10
        END-EVALUATE.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
            MOVE W15-GRNN  TO W15-REFNO PDT-REFER.
           IF W15-GRNN = ZERO
              GO TO HC70.
      PERFORM START-AT-GPDCHQ-REF THRU READ-GPDCHQ-EXIT.
    IF WS-F-ERROR = ZERO
        GO TO HC20.
       HC15.
      DISPLAY "Not on file" AT 2302
        WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      GO TO HC10.
       HC20.
      PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (W15-GRNN < PDT-REFER)
        GO TO HC15.
      MOVE PDT-DATE  TO W21-DTE.
      MOVE W21-YY  TO W15-YY.
      MOVE W21-MM  TO W15-MM.
      MOVE W21-DD  TO W15-DD.
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNN.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-TCOST.

            PERFORM AE155-GET-CREDITOR.

            MOVE W15-CRED  TO W15-ACCOUNT.
             PERFORM AD000.
      MOVE G-NAME  TO W15-ACNME.
            MOVE W15-BANK  TO W15-ACCOUNT.
             PERFORM AD000.
      MOVE G-NAME  TO W15-BNKNME.
      DISPLAY S21.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO HC05.
       HC22.
            ACCEPT S15 AT 0519.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC05
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC22
        END-EVALUATE.
             MOVE W15-GRND TO W15-DATE.
            PERFORM HA100.
           IF WS-ERROR = 1
        DISPLAY WS-ER3 AT 2302
         WITH BACKGROUND-COLOR Cyan
       FOREGROUND-COLOR Brown HIGHLIGHT
              GO TO HC22.
             MOVE W15-DD         TO W20-DD.
             MOVE W15-MM         TO W20-MM.
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
            ACCEPT S14 AT 0619.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC22
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC25
        END-EVALUATE.
            MOVE W15-GRNN  TO W15-REFNO.
           IF W15-GRNN = ZERO
              GO TO HC25.
       HC30.
            ACCEPT S16 AT 0719.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HC25
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC30
        END-EVALUATE.
    IF W15-COST NOT < ZERO
              MULTIPLY -1 BY W15-TCOST
                                 GIVING W15-CHQVAL.
            DISPLAY S17 AT 0719.
           IF W15-TCOST = ZERO
              GO TO HC30.
       HC35.
             MOVE W15-BANK       TO W15-ACCOUNT.
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S09 AT 0919.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HC30
   WHEN F1-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC35
        END-EVALUATE
        DISPLAY S09 AT 0919
               IF W15-ACCOUNT = ZERO
           GO TO HC35.
      PERFORM CLEAR-L50.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF W15-ASER = ZERO
              DISPLAY WS-ER7 AT 2324
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
              GO TO HC35.
             MOVE W15-ACCOUNT    TO W15-BANK.
       HC40.
             PERFORM AD000.
           IF WS-ERROR = 1
              DISPLAY WS-ER6 AT 2324
         WITH FOREGROUND-COLOR Brown HIGHLIGHT
              GO TO HC35.
            DISPLAY G-NAME AT 0926
       WITH FOREGROUND-COLOR Brown HIGHLIGHT.
       HC45.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HC35.
       HC55.
            ACCEPT W15-NAR AT 1019
            WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    UPDATE AUTO.
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
      DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " to do Account Lookup - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Esc" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ape"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT S09 AT 1119.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HC55
   WHEN F1-KEY
       PERFORM AA20G THRU AA20
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HC60
        END-EVALUATE
        DISPLAY S09 AT 1119
               IF W15-ACCOUNT = ZERO
           GO TO HC60.
      PERFORM CLEAR-L50.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
           IF W15-ASER = ZERO
              GO TO HC60.
            MOVE W15-ACCOUNT  TO W15-CRED.
             PERFORM AD000.
           IF WS-ERROR = 1
              DISPLAY WS-ER6 AT 2324 WITH FOREGROUND-COLOR Brown HIGHLIGHT
              GO TO HC60.
            DISPLAY G-NAME AT 1126 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
       HC65.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO HC60.
      PERFORM AY60 THRU AY999.
    IF (W20-DTE NOT = PDT-DATE) OR
       (W15-ACNO NOT = PDT-AC) OR
       (W15-REFNO NOT = PDT-REFER)
        PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT
        INITIALIZE PDT-REC
        MOVE "Y"   TO WS-DEL
     ELSE
        MOVE "N"   TO WS-DEL.
      MOVE W15-ACNO  TO PD-ACNO PDT-AC.
      MOVE P-NAME  TO PD-NAME.
      MOVE W20-DTE  TO PDT-DATE.
      MOVE W15-GRND  TO PD-DATE.
      MOVE W15-REFNO  TO PD-REF PDT-REFER.
      MOVE W15-BANK  TO PD-BANK PDT-BANK.
      MOVE W15-CRED  TO PD-CRED PDT-CRED.
      MOVE W15-NAR  TO PD-NAR PDT-NAR.
      MOVE W15-CHQVAL  TO PD-VALUE PDT-VAL.
      ADD W15-CHQVAL  TO W20-TOTAL.
    IF LINAGE-COUNTER < WS-PRN-LENGTH
        WRITE R-L1 BEFORE 1 LINES
    ELSE
        WRITE R-L1 BEFORE PAGE
        PERFORM HA50.
      MOVE SPACES  TO R-DET.
    IF WS-DEL = "Y"
        PERFORM WRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT
        MOVE 1   TO WS-ACTION
    ELSE
        PERFORM REWRITE-GPDCHQ THRU WRITE-GPDCHQ-EXIT
        MOVE 0   TO WS-ACTION.
      PERFORM AY45 THRU AY50.
      PERFORM AY70 THRU AY999.
       HC70.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue - E to end" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5041
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            PERFORM CLEAR-L50.
           IF NOT (WS-OPTION = "E")
              GO TO HC05.
      MOVE SPACES  TO R-DET.
             WRITE R-L1 BEFORE 1 LINES.
            MOVE "****  TOTAL ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PD-VALUE.
             WRITE R-L1 BEFORE 1 LINES.
            MOVE WS-NORM  TO R-DET.
             WRITE R-L1 BEFORE PAGE.
             MOVE SPACES         TO R-DET.
       HC999.
      EXIT.
      /
       HD000        SECTION 5.
       HD00.
            MOVE "PD. CHEQUES DELETED"
                                 TO W50-HEAD.
            MOVE WS-COND  TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE ZERO           TO W20-TOTAL W20-DEBIT.
       HD02.
      PERFORM HA50.
       HD05.
            PERFORM ERASE-SCREEN.
            DISPLAY S20.
      DISPLAY "DELETE PD CHEQUE" AT 0233
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            MOVE ZERO   TO W15-GRND
        W15-GRNN.
      MOVE SPACES  TO W15-ACNO.
       HD10.
      DISPLAY "Key in the cheque number" AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT S14 AT 0619.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY  GO TO HD70
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO HD10
        END-EVALUATE.
            DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR Cyan.
      PERFORM CLEAR-L50.
            MOVE W15-GRNN  TO W15-REFNO PDT-REFER.
           IF W15-GRNN = ZERO
              GO TO HD70.
      PERFORM START-AT-GPDCHQ-REF THRU READ-GPDCHQ-EXIT.
    IF WS-F-ERROR = ZERO
        GO TO HD20.
       HD15.
      DISPLAY "Not on file" AT 2302
        WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      GO TO HD10.
       HD20.
      PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (W15-GRNN < PDT-REFER)
        GO TO HD15.
      MOVE PDT-DATE  TO W21-DTE.
      MOVE W21-YY  TO W15-YY.
      MOVE W21-MM  TO W15-MM.
      MOVE W21-DD  TO W15-DD.
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNN.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-TCOST.

            PERFORM AE155-GET-CREDITOR.

            MOVE W15-CRED  TO W15-ACCOUNT.
             PERFORM AD000.
      MOVE G-NAME  TO W15-ACNME.
            MOVE W15-BANK  TO W15-ACCOUNT.
             PERFORM AD000.
      MOVE G-NAME  TO W15-BNKNME.
      DISPLAY S21.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO HD05.
      MOVE W15-ACNO  TO PD-ACNO.
      MOVE P-NAME  TO PD-NAME.
      MOVE W15-GRND  TO PD-DATE.
      MOVE W15-REFNO  TO PD-REF.
      MOVE W15-BANK  TO PD-BANK.
      MOVE W15-CRED  TO PD-CRED.
      MOVE W15-NAR  TO PD-NAR.
      MOVE W15-CHQVAL  TO PD-VALUE.
      ADD W15-CHQVAL  TO W20-TOTAL.
    IF LINAGE-COUNTER < WS-PRN-LENGTH
        WRITE R-L1 BEFORE 1 LINES
    ELSE
        WRITE R-L1 BEFORE PAGE
        PERFORM HA50.
      MOVE SPACES  TO R-DET.
      PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT.
       HD70.
            PERFORM CLEAR-L50.
            DISPLAY "ENTER to continue - E to end" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5041
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            PERFORM CLEAR-L50.
           IF NOT (WS-OPTION = "E")
              GO TO HD05.
      MOVE SPACES  TO R-DET.
             WRITE R-L1 BEFORE 1 LINES.
            MOVE "****  TOTAL ****"
     TO PD-NAR.
            MOVE W20-TOTAL  TO PD-VALUE.
             WRITE R-L1 BEFORE 1 LINES.
            MOVE WS-NORM  TO R-DET.
             WRITE R-L1 BEFORE PAGE.
             MOVE SPACES         TO R-DET.
       HD999.
      EXIT.
      /
       HE000       SECTION 5.
       HE0.
            MOVE "PD. CHEQUES PROCESSED"
                                 TO W50-HEAD.
            MOVE WS-COND  TO R-DET.
             WRITE R-L1 BEFORE 0 LINES.
             MOVE ZERO           TO W20-TOTAL W20-DEBIT.
       HE00B.
             PERFORM AA50 THRU AA60.
             MOVE WS-HACNO       TO CH-H1.
             MOVE WS-HNAME       TO CH-H2.
             MOVE WS-HDTE        TO CH-H3.
             MOVE WS-HCHQ        TO CH-H4.
             MOVE "VALUE"        TO CH-H5.
             MOVE WS-HAGE        TO CH-H6.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
             MOVE WS-HACNO       TO R-H5.
             MOVE ALL "_"        TO R-H6 R-H8 R-H16 R-H18.
             MOVE "ACCOUNT NAME" TO R-H7.
             MOVE WS-HDTE        TO R-H9.
             MOVE "CHEQ NO"      TO R-H10.
             MOVE "PAYEE/NARRATIVE"
                                 TO R-H17.
             MOVE "DEBIT"        TO R-H14.
             MOVE "CREDIT"       TO R-H15.
             WRITE R-L1 BEFORE 2 LINES.
             MOVE SPACES         TO R-DET.
       HE00.
             PERFORM ERASE-SCREEN.
            DISPLAY S20.
      DISPLAY "POST PD CHEQUES DUE" AT 0231
        WITH FOREGROUND-COLOR Grey HIGHLIGHT.
      DISPLAY S22.
       HE02.
            MOVE ZERO   TO W20-TOTAL W20-DEBIT.
      INITIALIZE PDT-REC.
      PERFORM START-AT-GPDCHQ-KEY THRU READ-GPDCHQ-EXIT.
    IF WS-F-ERROR = 44
        GO TO HE145.
       HE05.
      PERFORM READ-GPDCHQ-NEXT THRU READ-GPDCHQ-EXIT.
    IF (WS-F-ERROR = 44) OR
       (PDT-DATE > W05-TYMD)
        GO TO HE135.
      MOVE PDT-DATE  TO W21-DTE.
      MOVE W21-YY  TO W15-YY W20-YY.
      MOVE W21-MM  TO W15-MM W20-MM.
      MOVE W21-DD  TO W15-DD W20-DD.
      MOVE W15-DATE  TO W15-GRND.
      MOVE PDT-AC  TO W15-ACNO.
      MOVE PDT-REFER  TO W15-GRNN W15-REFNO.
      MOVE PDT-BANK  TO W15-BANK.
      MOVE PDT-NAR  TO W15-NAR.
      MOVE PDT-CRED  TO W15-CRED.
      MOVE PDT-VAL  TO W15-CHQVAL.
            MULTIPLY -1 BY PDT-VAL
     GIVING W15-TCOST.

            PERFORM AE155-GET-CREDITOR.

            MOVE P-NAME  TO W15-NAME W15-PAYEE.
            MOVE W15-BANK  TO W15-ACCOUNT.
             PERFORM AD000.
             MOVE WS-GLKEY       TO WS-KSTORE.
      MOVE G-NAME  TO W15-BNKNME.
            MOVE W15-CRED  TO W15-ACCOUNT.
             PERFORM AD000.
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
            DISPLAY W15-30 AT 0819 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-60 AT 0826 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-90 AT 0833 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-120 AT 0841 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-150 AT 0849 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY W15-180 AT 0856 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
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
       HE50.
            MOVE W15-ACNO  TO CH-ACNO.
             MOVE W15-NAME       TO CH-NAME.
             MOVE W15-GRNN       TO CH-CHQ.
             MOVE W15-GRND       TO CH-DATE.
             MOVE W15-CHQVAL     TO CH-VAL.
          IF LINAGE-COUNTER < (WS-PRN-LENGTH - 1)
               WRITE R-L1 BEFORE 2 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
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
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
             MOVE W15-REFER      TO T-REFER.
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
             ADD 1               TO T-SEQ.
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
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
              PERFORM HE00B.
             MOVE SPACES         TO R-DET.
       HE115.
            DISPLAY "Updating Bank account" AT 5020 WITH
              BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
             MOVE WS-KSTORE      TO WS-GLKEY.
             PERFORM AY12 THRU AY59.
             MOVE G-GLTRAN       TO WS-GTRKEY.
             INITIALIZE T-REC.
       HE120.
             MOVE W15-YY         TO W15-YR.
             MOVE W15-MM         TO W15-MT.
             MOVE W15-DD         TO W15-DY.
             MOVE W15-YMD        TO T-DATE.
             MOVE W15-NAR        TO T-NARATIVE.
             MOVE W15-REFER      TO T-REFER.
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
             ADD WS-VALUE        TO G-VAL (W05-MM).
             ADD W20-CREDIT      TO G-BAL.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             MOVE W15-PAYEE      TO T-NARATIVE.
             MOVE 5              TO T-SUB.
             ADD 1               TO T-SEQ.
             PERFORM WRITE-GLTRAN THRU WRITE-GLTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY13 THRU AY59.
             PERFORM REWRITE-GACCNT THRU WRITE-GACCNT-EXIT.
       HE125.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 1 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
              PERFORM HE00B.
             MOVE G-AC           TO R-ACC.
             MOVE G-NAME         TO R-ANME.
             MOVE W15-DATE       TO R-DTE2.
             MOVE W15-REFER      TO R-REF.
             MOVE W15-PAYEE      TO R-NAR.
             MOVE W20-CREDIT     TO R-CRD.
             PERFORM AY70 THRU AY999.
      PERFORM DELETE-GPDCHQ-REC THRU WRITE-GPDCHQ-EXIT.
          IF LINAGE-COUNTER < WS-PRN-LENGTH
               WRITE R-L1 BEFORE 2 LINES
           ELSE
               WRITE R-L1 BEFORE PAGE
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
             WRITE R-L1 BEFORE 1 LINES.
            MOVE WS-NORM  TO R-DET.
             WRITE R-L1 BEFORE PAGE.
             MOVE SPACES         TO R-DET.
      GO TO HE999.
       HE145.
      MOVE "****    N O   P D   C H E Q U E S   D U E   ****"
     TO R-DET.
      GO TO HE140.
       HE999.
             EXIT.

      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000      SECTION 9.
       ZA00.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W05-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W05-TYMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W90-CNAME.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *
      *    ****    D O   N O T   D I S P L A Y   E R R O R
      *            M E S S A G E S
      *
      *      MOVE 44   TO USER-NUMBER.
      *      MOVE 1    TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
                   GO TO ZA01A.
             MOVE 110                TO WS-USUB.
       ZA01A.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-STDP(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
        MOVE 2   TO WS-PRN-NO
    ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
        MOVE LS-USER  TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
     TO WS-COND.
             MOVE TRM-NORP (WS-S6)
     TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.
       ZA01B.
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
            OPEN OUTPUT RECOVER GLJRN.
             CLOSE RECOVER GLJRN.
             OPEN I-O RECOVER GLJRN.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W90-COMP.
             MOVE PAR-DMY        TO W05-TODAY.
             MOVE PAR-YMD        TO W05-TYMD.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
      MOVE PAR-RATE-DATE  TO WS-VAT-DATE.
             MOVE W05-VAT-R1     TO W05-RATE.
             MOVE PAR-DISCOUNT   TO W15-DISCOUNT.
             MOVE 6              TO WS-PARKEY.
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
               DISPLAY (20, 20) "Parameters not set up - ENTER"
                                 WITH FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT (20, 50) WS-OPTION WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO
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
      *
      *    ****    S E T   U P   M O N T H S   ( Y E A R   E N D)
      *
            MOVE 1   TO WS-S1.
            MOVE G-YEAR-END  TO WS-S2 WS-Y-END.
            MOVE G-LAST-YE  TO WS-L-END.
    IF G-OPEN = ZERO
        MOVE WS-Y-END  TO G-OPEN.
      MOVE G-OPEN  TO WS-OPEN.
            PERFORM AB70 THRU AB75.
       ZA48.
    IF WS-PRN-NO < 3
        PERFORM ZA210 THRU ZA215
        IF WS-ERROR = 1
     GO TO ZA205
    ELSE
        DISPLAY "Waiting for printer response" AT 1412
   WITH FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
            OPEN OUTPUT PRNREP.
      CALL X"91" USING X91-RES X91-FUN PRNREP.
            GO TO ZA999.

       COPY "ZA49.PRO".

       ZA200.
       
       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.
       ZA210.
      MOVE ZERO   TO WS-ERROR
        WS-S1 WS-S2 WS-S3 WS-S4
        WS-S5 WS-S6 WS-S7 WS-S8.
      CALL "PC_TEST_PRINTER" USING WS-PRN-NO
       WS-PRN-STAT
        RETURNING WS-STATUS.
    IF WS-PRN-STAT > 127
        MOVE 1   TO WS-S1
        SUBTRACT 128  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 63
        MOVE 1   TO WS-S2
        SUBTRACT 64  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 31
        MOVE 1   TO WS-S3
        SUBTRACT 32  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 15
        MOVE 1   TO WS-S4
        SUBTRACT 16  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 7
        MOVE 1   TO WS-S5
        SUBTRACT 8  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 3
        MOVE 1   TO WS-S6
        SUBTRACT 4  FROM WS-PRN-STAT.
    IF WS-PRN-STAT > 1
        MOVE 1   TO WS-S7
        SUBTRACT 2  FROM WS-PRN-STAT.
      MOVE WS-PRN-STAT  TO WS-S8.
    IF WS-S3 = 0 AND
       WS-S5 = 0 AND
       WS-S8 = 0
        GO TO ZA215.
      DISPLAY "Check printer - " AT 2304
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
       "R" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "etry - "
       "A" WITH FOREGROUND-COLOR Grey HIGHLIGHT
       "bort".
       ZA212.
      ACCEPT WS-OPTION AT 2334
      WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      IF NOT (WS-OPTION = "A" OR "R")
   GO TO ZA212.
      IF WS-OPTION = "A"
   MOVE 1   TO WS-ERROR
   GO TO ZA215.
        GO TO ZA210.
       ZA215.
      EXIT.
       ZA999.
             EXIT.
      *
      *     I / O   E R R O R   M E S S A G E S
      *
       ZB000      SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
               WHEN 8          MOVE W02-GACCNT   TO WS-FILE
                               MOVE G-AC         TO WS-KEYX
               WHEN 11         MOVE W02-GLTRAN   TO WS-FILE
                               MOVE T-KEY        TO WS-KEYX
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
               WHEN 18         MOVE W02-RECOVER  TO WS-FILE
                               MOVE WS-RECKEY    TO WS-KEY
               WHEN 26         MOVE W02-GLJRN    TO WS-FILE
                               MOVE WS-GLJKEY    TO WS-KEY
               WHEN 32         MOVE W02-TXTRAN   TO WS-FILE
                               MOVE TAX-KEY      TO WS-KEYX
               WHEN 35         MOVE W02-CRETRN   TO WS-FILE
                               MOVE CRT-KEY      TO WS-KEYX
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
               WHEN 40         MOVE W02-LEDTRF   TO WS-FILE
                               MOVE XFR-KEY      TO WS-KEYX
               WHEN 45         MOVE W02-GPDCHQ   TO WS-FILE
                               MOVE PDT-KEY      TO WS-KEYX.
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
                               MOVE "GLP018"     TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
