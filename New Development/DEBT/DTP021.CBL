      $SET LINKCOUNT"512" GNT"DTP021.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****     ******      **      *
      *   **    **     **     **    **   **  **   **    **    ***      *
      *   **    **     **     **    **  **    **       **      **      *
      *   **    **     **     *******   **    **     **        **      *
      *   **    **     **     **        **    **   **          **      *
      *   **    **     **     **         **  **   **           **      *
      *   *******      **     **          ****    ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  D E B T O R   U N A L L O C A T E D   C R E D I T S   P R O G *
      *                                                                *
      *  This program reports the unallocated credits in the Accounts  *
      *  Receivable transaction file.                                  *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Feb 2007 - New file (DEBDEP/AR_REST.DAT) - Debtor sales       *
      *             restrictions file has been included which will     *
      *             allow for departments to be excluded or only       *
      *             selected departments allowed. This will allow the  *
      *             user the abillity to allow or bar sales of select- *
      *             ed departments.                                    *
      *                                                                *
      *  Jan 2008 - New file (DEBALT) - Debtor alternate index includ- *
      *             ed for lookups, using any word contained in the    *
      *             Debtor name.                                       *
      *                                                                *
      *  July 2012 - Added email address field - DEB-EMAIL             *
      *              (W40-EMAIL) PIC X(64)                             *
      *                                                                *
      *  Nov 2012 - Changed DEB-ADDRESS and DEB-PADDR from group       *
      *             fields with 70 characters to group fields          *
      *             containing DEB-ADD-L1/2/3/4 and DEB-PADD-L1/2/3/4  *
      *             respectively. Each of these new fields are defined *
      *             as PIC X(30). As new PC's and Servers contain huge *
      *             amounts of disk space this has been changed, which *
      *             also results in simplifying the code for handling  *
      *             addresses in Data capture, enquiries, reports etc. *
      *             This also removes the restriction on the number of *
      *             characters that an address may contain.            *
      *                                                                *
      *             Changed DEB-NAME from 40 characters to a group     *
      *             name containing the following fields: DEB-SNAME,   *
      *             DEB-1ST-NAME and DEB-2ND-NAME each defined as PIC  *
      *             X(24) as well as DEB-INIT PIC X(08)(W40-SURNAME,   *
      *             W40-FNAME, W40-FNAME and W40-INITIALS). This allows*
      *             for a name of 80 characters although the screen    *
      *             layout (due to the fact that the system is using   *
      *             an 80 character width screen) for a Company and    *
      *             Internal accounts is providing for only 60 charact-*
      *             ers. This allows an additional 20 characters for   *
      *             Company and Internal account names.                *
      *                                                                *
      *  Jan 2013 - Program DTPMEMO introduced to handle memo lookups  *
      *             reducing code in all programs and removing file    *
      *             DEBMEM from programs. Copy file 'DEBMEM.PRO' now   *
      *             copied into all programs that require lookups for  *
      *             memo/remarks on a Debtor account. Move the Debtor  *
      *             code/account number to W80-ACCOUNT and then        *
      *             PERFORM DEBTOR-MEMO. The Debtor lookup program -   *
      *             DTPLOOK now includes a Memo Lookup so unless there *
      *             is a requirement to lookup memo details after this *
      *             lookup, the MEMO lookup can be removed from all    *
      *             DEBTOR programs.                                   *
      *                                                                *
      *  May 2013 - Added Cell phone number field DEB-C-TEL            *
      *             (W40-C-TEL) PIC X(14).                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTP021.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   October 2004.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2004 - 2016
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

       COPY "PARAM.SL".

       COPY "DEBTOR.SL".

       COPY "DBTRAN.SL".

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

       COPY "PARAM.FDE".

       COPY "DEBTOR.FDE".

       COPY "DBTRAN.FDE".

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
       77  WS-SUB-5                PIC  9(04)    COMP-5.
       77  WS-SUB-6                PIC  9(04)    COMP-5.
       77  WS-IXS1                 PIC  9(04)    COMP-5.
       77  WS-IXS2                 PIC  9(04)    COMP-5.
       77  WS-IXS3                 PIC  9(04)    COMP-5.
       77  WS-IXS4                 PIC  9(04)    COMP-5.
       77  WS-MSUB                 PIC  9(04)    COMP-5.
       77  WS-PARKEY               PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-SEL                  PIC  X(01)           VALUE "N".
       77  WS-LINES                PIC  9(02).
       77  WS-PAGE                 PIC  9(04)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-ACT                  PIC  X(01)           VALUE SPACE.
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ERROR                PIC  9(01).
       77  WS-COMP                 PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  PRG-DEBT-LUP            PIC  X(12)           VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08) COMP-5.
       77  WS-USUB                 PIC  9(04) COMP-5.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         4 = Clear line 46 and line 50
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
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTP021".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE: UNALLOCATED CREDITS".
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
           03  WS-MODULE           PIC  X(03) VALUE "DTP".
           03  WS-PROG             PIC  X(03) VALUE "021".

       01  W00-TRAN-CODES.
           03  W00-T-VAT           PIC  9(09)V99 COMP-3.
           03  W00-E-DESC          PIC  X(12).
           03  W00-A-DESC          PIC  X(12).
           03  W00-ACTION          PIC  9(01).
           03  W00-T-CODE          PIC  9(02)    COMP-5.
           03  W00-T-GL            PIC  9(06)    COMP-3.
           03  W00-T-DAY           PIC  9(06)    COMP-5.
           03  W00-T-VAL           PIC S9(09)V99 COMP-3.
           03  W00-T-MTD           PIC  9(06)    COMP-5.
           03  W00-T-MTDV          PIC S9(09)V99 COMP-3.
           03  W00-T-YTD           PIC  9(06)    COMP-5.
           03  W00-T-YTDV          PIC S9(09)V99 COMP-3.

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER         PIC  X(12).
           03  W02-PGE-LENGTH      PIC  9(03).
           03  W02-PRN-LENGTH      PIC  9(03).
           03  W02-LINAGE          PIC  9(03).
           03  W02-PRN-STATUS      PIC  X(01)    VALUE "C".
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

           03  REP-LINE1                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1     PIC  X(136).
           
           03  REP-LINE2                          REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2     PIC  X(80).
           
           03  REP-LINE3                          REDEFINES W02-PRN-LINE.
               05  REP-PAGE        PIC  X(06).
               05  REP-P-NO        PIC  Z(03)9.
               05  FILLER          PIC  X(10).
               05  REP-COMPANY     PIC  X(40).
               05  FILLER          PIC  X(04).
               05  REP-DATE        PIC  X(06).
               05  REP-DTE         PIC  Z9/99/9999.
           
           03  REP-LINE3A                         REDEFINES W02-PRN-LINE.
               05  REP-CO          PIC  X(40).
           
           03  REP-LINE4                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  REP-HEAD1       PIC  X(08).
               05  REP-HEAD2       PIC  X(07).
               05  REP-HEAD3       PIC  X(09).
               05  REP-HEAD4       PIC  X(11).
               05  REP-HEAD5       PIC  X(06).
               05  REP-HEAD6       PIC  X(21).
               05  REP-HEAD7       PIC  X(16).
           
           03  REP-LINE8                          REDEFINES W02-PRN-LINE.
               05  FILLER          PIC  X(02).
               05  REP-AC          PIC  X(06).
               05  FILLER          PIC  X(02).
               05  REP-NAME.
                   07  FILLER      PIC  X(04).
                   07  REP-TDTE    PIC  Z9/99/9999.
                   07  FILLER      PIC  X(02).
                   07  REP-TREF    PIC  X(08).
                   07  FILLER      PIC  X(04).
                   07  REP-TCDE    PIC  9(02).
                   07  FILLER      PIC  X(03).
                   07  REP-TDESC   PIC  X(12).
                   07  FILLER      PIC  X(02).
                   07  REP-TVAL    PIC  Z(07)9.99CR.
                   07  FILLER      PIC  X(10).
           
       01  W10-TEMP.
           03  W10-DATE            PIC  X(10).
           03  W10-RDATE                          REDEFINES W10-DATE.
              05  W10-DTE          PIC  99/99/9999.

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

       COPY "W15.WS".

       01  W24-MONTHS.
           03  W24-MONTH-NAMES.
               05  FILLER          PIC  X(09)    VALUE " January ".
               05  FILLER          PIC  X(09)    VALUE " February".
               05  FILLER          PIC  X(09)    VALUE "  March  ".
               05  FILLER          PIC  X(09)    VALUE "  April  ".
               05  FILLER          PIC  X(09)    VALUE "   May   ".
               05  FILLER          PIC  X(09)    VALUE "   June  ".
               05  FILLER          PIC  X(09)    VALUE "   July  ".
               05  FILLER          PIC  X(09)    VALUE "  August ".
               05  FILLER          PIC  X(09)    VALUE "September".
               05  FILLER          PIC  X(09)    VALUE " October ".
               05  FILLER          PIC  X(09)    VALUE " November".
               05  FILLER          PIC  X(09)    VALUE " December".
           03  W24-EACH-MONTH                    REDEFINES W24-MONTH-NAMES.
               05  W24-MONTH       PIC  X(09)    OCCURS 12.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC  9(02).
           03  W45-ENG             PIC  X(12).
           03  W45-AFR             PIC  X(12).
           03  W45-ACT             PIC  X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1              PIC  X(09).
           03  W55-H2              PIC  X(09).
           03  W55-H3              PIC  X(09).
           03  W55-H4              PIC  X(09).
           03  W55-H5              PIC  X(09).
           03  W55-H6              PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1        PIC  9(02)    COMP-5.
               05  W55-MTH2        PIC  9(02)    COMP-5.
               05  W55-MTH3        PIC  9(02)    COMP-5.
               05  W55-MTH4        PIC  9(02)    COMP-5.
               05  W55-MTH5        PIC  9(02)    COMP-5.
               05  W55-MTH6        PIC  9(02)    COMP-5.
           03  W55-VALUES.
               05  W55-VAL1        PIC S9(09)V99 COMP-3.
               05  W55-VAL2        PIC S9(09)V99 COMP-3.
               05  W55-VAL3        PIC S9(09)V99 COMP-3.
               05  W55-VAL4        PIC S9(09)V99 COMP-3.
               05  W55-VAL5        PIC S9(09)V99 COMP-3.
               05  W55-VAL6        PIC S9(09)V99 COMP-3.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR       PIC  X(01)    OCCURS 40.
           03  W60-ADDRESS.
               05  W60-ACHAR       PIC  X(01)    OCCURS 60.

       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR        PIC  X(01)    OCCURS 8.
           03  W65-PREFIX                        REDEFINES W65-FOUR.
               05  W65-PREF        PIC  X(02).
               05  FILLER          PIC  X(06).

       01  W65-INDEX.
           03  W65-INPUT.
               05  W65-ICHAR       PIC  X(01)    OCCURS 8.

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       COPY "W75.WS".

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                          REDEFINES W80-ACNO.
               05  W80-SERIAL      PIC  9(06).
           03  W80-TYPE            PIC  9(01).

       COPY "W90.DBT".

       01  W95-STM.
           03  W95-COMP            PIC  X(40).
           03  W95-TEL             PIC  X(14).

       01  W100-HEAD.
          03  FILLER               PIC  X(40)    VALUE "ACCOUNTS RECEIVABLE- UNALLOCATED CREDITS".

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99-.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CUR            PIC  Z(08)9.99-.
           03  W100-30             PIC  Z(08)9.99-.
           03  W100-60             PIC  Z(08)9.99-.
           03  W100-90             PIC  Z(08)9.99-.
           03  W100-120            PIC  Z(08)9.99-.
           03  W100-INT            PIC  Z(08)9.99.
           03  W100-CRL            PIC  Z(06)9.
           03  W100-KEY            PIC  Z(05)9.

      *
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      *
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       COPY "DEBLUP.CRT".

       01  S-PROG.
           03  FOREGROUND-COLOR Grey HIGHLIGHT 
               05  LINE  2 COLUMN 27 VALUE "ACCOUNTS RECEIVABLE REPORTS".
               05  LINE  4 COLUMN 27 VALUE "LIST OF UNALLOCATED CREDITS".

       01  S18.
           03  FOREGROUND-COLOR Cyan HIGHLIGHT 
               05  LINE  8 COLUMN 26 PIC 99/99/9999 USING W15-START AUTO.
               05  LINE  9 COLUMN 26 PIC 99/99/9999 USING W15-END AUTO.

      *
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA00-INIT.
             PERFORM BA000.

       AA49.
            EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
             PERFORM SAVE-SCREEN.
             MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             PERFORM DEBTOR-LOOKUP.
             PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY "AA900.ALM".
       
       COPY "ABORT.PRN".

       COPY "APAC.HLP".

       COPY "CLEAR.CRT".

       COPY "DATE.CHK".

      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ G E T   A   D E B T O R   T R A N S A C T I O N   R E C O R D³
      * ³                                                              ³
      * ³  Perform this routine with the transaction code in W90-CODE  ³
      * ³                                                              ³
      * ³  On exiting, the transaction code details are in W00-......  ³
      * ³  while W90-TRNKEY contains the key to the PARAMETER file     ³
      * ³  where this transaction code is found and W90-TS is the      ³
      * ³  transaction code subscript in the record.                   ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       GET-DEBTOR-TRAN-RECORD SECTION.

       GET-DEBTOR-TRAN.
             ADD 21 W90-CODE         GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2  GIVING W90-TRNKEY REMAINDER W90-TS.
             ADD 1                   TO W90-TS.

       GET-DEBTOR-TRAN-CODE.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.

       STORE-DEBTOR-TRAN-DESC.
             MOVE PAR-E-DESC(W90-TS) TO W00-E-DESC.
             MOVE PAR-A-DESC(W90-TS) TO W00-A-DESC.
             MOVE PAR-ACTION(W90-TS) TO W00-ACTION.
             MOVE PAR-T-CODE(W90-TS) TO W00-T-CODE.
             MOVE PAR-T-GL(W90-TS)   TO W00-T-GL.

       STORE-DEBTOR-TRAN-STATS.
             MOVE PAR-T-VAT(W90-TS)  TO W00-T-VAT.
             MOVE PAR-T-DAY(W90-TS)  TO W00-T-DAY.
             MOVE PAR-T-VAL(W90-TS)  TO W00-T-VAL.
             MOVE PAR-T-MTD(W90-TS)  TO W00-T-MTD.
             MOVE PAR-T-MTDV(W90-TS) TO W00-T-MTDV.
             MOVE PAR-T-YTD(W90-TS)  TO W00-T-YTD.
             MOVE PAR-T-YTDV(W90-TS) TO W00-T-YTDV.

       GET-DEBTOR-TRAN-EXIT.
             EXIT.

       GET-DEBTOR-TRAN-DESC SECTION.

       GET-DEBT-TRAN-DESC.
             PERFORM GET-DEBTOR-TRAN.
             MOVE W90-TRNKEY         TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM STORE-DEBTOR-TRAN-DESC.

       GET-DEBT-TRAN-DESC-EXIT.
             EXIT.
      
      /    *************************************************************
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

       COPY "OPTION.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DEBTOR.RD1".

       COPY "DBTRAN.RD2".

       COPY "DEBTOR.LUP".

       COPY "PRINTUTL.AP5".

      /
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
             MOVE W80-ACNO           TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1                TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR
               MOVE DEB-TYPE         TO W80-TYPE.

       CA160-EXIT.
             EXIT.

       CA165-NAME        SECTION 2.
       CA165-EXTRACT.
             MOVE ZERO               TO WS-ERROR.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!" INTO W40-SURNAME W40-FNAME W40-SNAME
               OVERFLOW MOVE 1       TO WS-ERROR.

       CA170-EXIT.
             EXIT.

      /
       BA000        SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
             DISPLAY S-PROG.

       BA05.
             MOVE "Selective per account (Y/N)  [N]" TO WS-OPT-MES.
             MOVE "N"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
           IF NOT(WS-OPTION = "N" OR "Y")
               GO TO BA05.
           IF WS-OPTION = "Y"
               MOVE "Y"              TO WS-SEL
               GO TO BA65
           ELSE
               MOVE "N"              TO WS-SEL.
             MOVE " !"               TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO BA999.
             MOVE ZERO               TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
             PERFORM OPEN-PRINTER.
           IF NOT(WS-PRINTER = "S")
               DISPLAY "Account No:" AT 1412.

       BA15.
             MOVE "P"                TO WS-COMMAND.
             MOVE "H"                TO W02-PRN-TYPE.
             MOVE SPACES             TO REP-DETAIL1.
             MOVE "Page:"            TO REP-PAGE.
             MOVE "Date:"            TO REP-DATE.
             ADD 1                   TO WS-PAGE.
             MOVE W12-TODAY          TO REP-DTE.
             MOVE WS-PAGE            TO REP-P-NO.
             MOVE W100-HEAD          TO REP-COMPANY.
             MOVE 2                  TO WS-ADVANCE.
             PERFORM CALL-PRINTUTL.
             MOVE SPACES             TO REP-DETAIL1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       I G N O R E   I F   U S I N G   S C R E E N !       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE W95-COMP  TO REP-CO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       I G N O R E   I F   U S I N G   S C R E E N !       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES TO REP-DETAIL1.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     I G N O R E   I F U S I N G   S C R E E N !      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE "Ac.No."  TO REP-HEAD1.
      MOVE "Name"  TO REP-HEAD2.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE 1   TO WS-ADVANCE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       G E T   T H E   N E X T A C C O U N T        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA20.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO BA985.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        E N D   O F   F I L E ?         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-F-ERROR = 6
        GO TO BA980.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     I G N O R E   I F U S I N G   S C R E E N !      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF NOT (WS-PRINTER = "S")
        DISPLAY DEB-ACNO AT 1424 WITH FOREGROUND-COLOR Cyan HIGHLIGHT .
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    No & Name NOT printed on report!        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE SPACE   TO WS-ACT.
            MOVE DEB-ACNO  TO TRA-KEY.

       BA25.
             PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       No transactions on file?         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-F-ERROR = 5
        GO TO BA85.

       BA30.
             PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   End of transaction file or account number has changed?  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF (WS-F-ERROR = 5) OR
       (TRA-AC NOT = DEB-ACNO)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Have details been printed for the previous account?       ³
      *    ³ If there were details printed, then print a blank line.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
        IF WS-ACT = "Y"
     MOVE SPACES  TO  REP-DETAIL1
           IF W02-LINAGE < (W02-PRN-LENGTH - 3)
         MOVE 1  TO WS-ADVANCE
         PERFORM CALL-PRINTUTL
           ELSE
         MOVE 99  TO WS-ADVANCE
         PERFORM CALL-PRINTUTL
               PERFORM BA15
     END-IF
        END-IF
        GO TO BA85.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is this transaction a credit? If not then go and read the ³
      *    ³ next transaction.            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF TRA-VALUE NOT < ZERO
        GO TO BA30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Has the credit value been allocated yet? If yes then go   ³
      *    ³ and read the next transaction.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
     GO TO BA30.
    IF WS-ACT = SPACE
        MOVE "Y"   TO WS-ACT
        MOVE DEB-ACNO  TO REP-AC
        PERFORM BA35 THRU BA60
        MOVE W40-NAME  TO REP-NAME
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO REP-DETAIL1
        MOVE "Date"  TO REP-HEAD3
        MOVE "Reference"  TO REP-HEAD4
        MOVE "Code"  TO REP-HEAD5
        MOVE "Description"
     TO REP-HEAD6
        MOVE "Value"  TO REP-HEAD7
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              GO TO BA985.
      MOVE TRA-CODE  TO REP-TCDE W90-CODE.
             PERFORM GET-DEBTOR-TRAN-DESC.
            MOVE TRA-DATE  TO W22-DTE2.
      MOVE W22-CC2  TO W22-CC3.
      MOVE W22-YY2  TO W22-YY3.
      MOVE W22-MM2  TO W22-MM3.
      MOVE W22-DD2  TO W22-DD3.
      MOVE W22-DTE3  TO REP-TDTE.
      MOVE TRA-REF  TO REP-TREF.
            MOVE W00-E-DESC  TO REP-TDESC.
      COMPUTE REP-TVAL = (TRA-VALUE + TRA-ALLOCATED).
          IF W02-LINAGE < (W02-PRN-LENGTH - 1)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BA15.
            MOVE SPACES  TO REP-DETAIL1.
      GO TO BA30.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the account is for an individusl EXTRACT the initials  ³
      *    ³ and surname and add the title.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA35.
          IF DEB-TYPE NOT = 1
              MOVE DEB-NAME  TO W40-NAME
              GO TO BA60.
            MOVE DEB-NAME  TO W60-NAME.
             PERFORM CA165-NAME.
            MOVE W75-ETITLE (DEB-TITLE)
     TO W60-NAME.
            MOVE 7   TO WS-SUB-6.

       BA40-TITLE.
           IF W60-NCHAR (WS-SUB-6) NOT = SPACE
              GO TO BA45-INIT.
            SUBTRACT 1   FROM WS-SUB-6.
           IF WS-SUB-6 > 0
              GO TO BA40-TITLE.

       BA45-INIT.
            MOVE 1   TO WS-SUB-5.
            ADD 2   TO WS-SUB-6.
             MOVE W40-NCHAR (25) TO W60-NCHAR (WS-SUB-6).
            ADD 1   TO WS-SUB-6.
            MOVE "."   TO W60-NCHAR (WS-SUB-6).
           IF W40-NCHAR (49) = SPACE
              GO TO BA50-SURNAME.
            ADD 1   TO WS-SUB-6.
             MOVE W40-NCHAR (49) TO W60-NCHAR (WS-SUB-6).
            ADD 1   TO WS-SUB-6.
            MOVE "."   TO W60-NCHAR (WS-SUB-6).

       BA50-SURNAME.
            ADD 2   TO WS-SUB-6.

       BA55-LOOP.
            MOVE W40-NCHAR (WS-SUB-5)
     TO W60-NCHAR (WS-SUB-6).
           IF WS-SUB-5 < 24
              ADD 1   TO WS-SUB-5
              ADD 1   TO WS-SUB-6
              GO TO BA55-LOOP.
            MOVE W60-NAME  TO W40-NAME.

       BA60.
      EXIT.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³Account No XXXXXX NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN³°°
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *      °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
       BA65.
            MOVE ZERO   TO WS-PAGE.
      PERFORM OPEN-PRINTER.
      PERFORM BA15.
      MOVE 9   TO SHADE-ROW.
      MOVE 12   TO SHADE-COL.
      MOVE 58   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄ" AT 0810 WITH FOREGROUND-COLOR Black
           BACKGROUND-COLOR Grey
       "¿" WITH FOREGROUND-COLOR Grey HIGHLIGHT
         BACKGROUND-COLOR Grey.
      DISPLAY "³Account No" AT 0910 WITH FOREGROUND-COLOR Black
      BACKGROUND-COLOR Grey
       "                                                ³"
        WITH FOREGROUND-COLOR Grey HIGHLIGHT
      BACKGROUND-COLOR Grey.
      DISPLAY "À" AT 1010 WITH FOREGROUND-COLOR Black
          BACKGROUND-COLOR Grey
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÙ" WITH FOREGROUND-COLOR Grey HIGHLIGHT
          BACKGROUND-COLOR Grey.
      PERFORM SCREEN-SHADOW.
      PERFORM SAVE-SCREEN-3.
      MOVE SPACES  TO W80-ACNO.

       BA70.
      ACCEPT W80-ACNO AT 0922
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    BACKGROUND-COLOR Magenta AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BA75
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50
   WHEN OTHER
       PERFORM AA900-ALARM
       GO TO BA70
        END-EVALUATE
              DISPLAY W80-ACNO AT 0922
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
       BACKGROUND-COLOR Magenta
               IF W80-ACNO = SPACES
           GO TO BA70.
       BA75.
          IF W80-ACNO = SPACES
              GO TO BA980.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BA80.
            PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA70.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA70.
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0929
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT
     BACKGROUND-COLOR Magenta.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM RESTORE-SCREEN-3
              GO TO BA70.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    No & Name NOT printed on report!        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE SPACE   TO WS-ACT.
            MOVE DEB-ACNO  TO TRA-KEY.
            GO TO BA25.

       BA85.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ P R I N T I N G   S E L L E C T I V E   A C C O U N T S ? ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF WS-SEL = "Y"
        PERFORM RESTORE-SCREEN-3
        GO TO BA70.
            GO TO BA20.

       BA980.
            MOVE "****   E n d   o f   R e p o r t   ****"
     TO REP-DETAIL1.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      GO TO BA995.

       BA985.
            MOVE "**** PRINT ABORTED BY OPERATOR ****"
     TO REP-DETAIL1.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      GO TO BA995.

       BA990.
      MOVE "E"   TO WS-COMMAND.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.

       BA995.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BA999.
             EXIT.



      /
       ZA00-INIT              SECTION 9.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO TODAY-YYMMDD W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY W95-COMP.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   d i s p l a y   E R R O R   m e s s a g e s ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *      MOVE 44   TO USER-NUMBER.
      *      MOVE 1   TO USER-SETTING.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA25.
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             GO TO ZA999-EXIT.
      *
      *ZA200.
      *
      *COPY "LOCKED.PRO".
      *
      *ZA205.
      *      EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      /
       ZB000                 SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK   TO WS-FILE
                               MOVE WS-NETKEY     TO WS-KEY
               WHEN 5          MOVE W02-DBTRAN    TO WS-FILE
                               MOVE TRA-KEY       TO WS-KEYX
               WHEN 6          MOVE W02-DEBTOR    TO WS-FILE
                               MOVE DEB-ACNO      TO WS-KEYX
               WHEN 15         MOVE WS-PARID      TO WS-FILE
                               MOVE WS-PARKEY     TO WS-KEY
               WHEN OTHER      MOVE "ERROR"       TO WS-FILE
                               MOVE "DTP021"      TO WS-KEYX
             END-EVALUATE.

       COPY "DISPERR.PRO".
