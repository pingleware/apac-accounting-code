      $SET LINKCOUNT"256" GNT "PRINTUTL.GNT"
      ******************************************************************
      *                                                                *
      *        *******    *******    ******   **    **   ********      *
      *        **    **   **    **     **     ***   **      **         *
      *        **    **   **    **     **     ** *  **      **         *
      *        *******    *******      **     ** *  **      **         *
      *        **         **    **     **     **  * **      **         *
      *        **         **    **     **     **   ***      **         *
      *        **         **    **   ******   **    **      **         *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P R I N T   U T I L I T Y   P R O G R A M                   *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRINTUTL.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   January 1997.

           This program is used to print/spool/display report details.
           
           The program performs the following functions:
           
           Open the printer.
           Open the spool file.
           Set up the necessary parameters if the report is to appear
           on the screen.
           Print a line and advance the paper as requested.
           Spool a line and pass on the advance parameters.
           Display a line of print as requested (allows for left/right).
           
           The calling program/s treat all reports as if they were being
           printed on a printer and are unaware of the Users request to
           spool or display the report.

      ******************************************************************
               COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2018
                                 by James William Lemmon.
                                   (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.
      ******************************************************************

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

       COPY "PARAM.SL".

          SELECT PRNREP  ASSIGN W02-PRINTER
                         ORGANIZATION LINE SEQUENTIAL.

          SELECT PRNSPL  ASSIGN W02-SPOOLER
                         ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "PARAM.FDE".

      /
       FD  PRNREP    LABEL RECORD OMITTED
                     LINAGE IS WS-PGE-LENGTH.
       01  R-SL1.
           03  R-SLA               PIC  X(78).
           03  FILLER              PIC  X(58).
       01  R-SL2.
           03  FILLER              PIC  X(58).
           03  R-SLB               PIC  X(78).
       01  REP-LINE1.
          03  REP-DETAIL1          PIC  X(136).

       FD  PRNSPL.
       01  SPL-L1.
           03  SPL-CONT.
               05  SPL-ADV         PIC  9(02).
               05  SPL-LIN         PIC  9(02).
           03  SPL-DETAIL1         PIC  X(136).

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-LEFT                 PIC  9(04)    COMP-5.
       77  WS-RIGHT                PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-SINGLE               PIC  X(01).
       77  WS-ERROR                PIC  9(01)    VALUE ZERO.
       77  WS-LINES                PIC  9(02)    VALUE ZERO.
       77  WS-PGE-LENGTH           PIC  9(02)    VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)    VALUE 62.
       77  WS-COND                 PIC  X(06)    VALUE X"000000000000".
       77  WS-NORM                 PIC  X(06)    VALUE X"000000000000".
       77  WS-EXPP                 PIC  X(06)    VALUE X"000000000000".
       77  WS-ECAN                 PIC  X(06)    VALUE X"000000000000".
       77  WS-8LPI                 PIC  X(06)    VALUE X"000000000000".
       77  WS-6LPI                 PIC  X(06)    VALUE X"000000000000".
       77  WS-10CPI                PIC  X(06)    VALUE X"000000000000".
       77  WS-12CPI                PIC  X(06)    VALUE X"000000000000".
       77  WS-17CPI                PIC  X(06)    VALUE X"000000000000".
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
       77  WS-SKIP                 PIC  X(01)    VALUE "N".
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRINTUSB".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITY - PRINT USB/SPOOL".
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

       01  W02-IDS.
           03  W02-PRINTER.
               05  W02-PRN         PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS          PIC  X(02).
               05  W02-US          PIC  X(01).
               05  W02-TERM        PIC  X(03).
               05  W02-US2         PIC  X(01).
               05  W02-REP         PIC  X(01).
               05  W02-P           PIC  X(01).
               05  W02-USER        PIC  X(03).

       01  W05-SCREEN.
           03  W05-PRINT-DETAIL.
               05  W05-LINE                     OCCURS 40.
                   07  W05-TYPE    PIC  X(01).
                   07  W05-DETAIL  PIC  X(136).
           03  W05-PRINT-DETAIL2                REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE2                    OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  W05-SLA     PIC  X(78).
                   07  FILLER      PIC  X(58).
           03  W05-PRINT-DETAIL2                REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE3                    OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  FILLER      PIC  X(58).
                   07  W05-SLB     PIC  X(78).
           03  W05-PRINT-DETAIL3                 REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE4                     OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  W05-CHAR    PIC  X(01)    OCCURS 136.

       01  W06-SCREEN.
           03  W06-PRINT-DETAIL.
               05  W06-LINE                      OCCURS 40.
                   07  W06-CHAR    PIC  X(01)    OCCURS 78.

       01  W08-EXPD.
           03  W08-CHAR            PIC  X(06).
           03  W08-DECR                          REDEFINES W08-CHAR.
               05  W08-DEC         PIC  X(01)    COMP-X OCCURS 6.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

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

       01  LS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
           03  LS-COMMAND      PIC  X(01).
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
           03  LS-PRINTER.
               05  LS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
           03  LS-ADVANCE      PIC  9(02).
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
           03  LS-REPORT           PIC  9(01).
           03  LS-SCREEN           PIC  9(01).
           03  LS-SCREEN-LIN       PIC  9(02).
           03  LS-LINAGE           PIC  9(02).
           03  LS-PAPER            PIC  X(01).
           03  LS-ORIENTATION      PIC  X(01).

       01  L02-PRINTER-DETAILS.
           03  L02-PRINTER         PIC  X(12).
           03  L02-PGE-LENGTH      PIC  9(03).
           03  L02-PRN-LENGTH      PIC  9(03).
           03  L02-LINAGE          PIC  9(03).
           03  L02-PRN-STATUS      PIC  X(01).
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
           03  L02-PRN-TYPE        PIC  X(01).
           03  L02-PRN-LINE        PIC  X(136).
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
      *
      *       *    **** **** *****    ***  *** ****  **** 
      *       *    *    *      *     *      *  *   * *    
      *       *    ***  ***    *      ***   *  *   * ***  
      *       *    *    *      *         *  *  *   * *    
      *       **** **** *      *      ***  *** ****  **** 
      *
      *            ***  ****   ****   ***   ***  **** 
      *           *   * *      *   * *   * *   * *    
      *           *   * ***    ****  ***** *     ***  
      *           *   * *      *     *   * *  ** *    
      *            ***  *      *     *   *  ***  **** 
      *
       01  SD-LEFT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W05-SLA(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W05-SLA(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W05-SLA(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W05-SLA(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W05-SLA(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W05-SLA(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W05-SLA(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W05-SLA(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W05-SLA(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W05-SLA(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W05-SLA(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W05-SLA(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W05-SLA(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W05-SLA(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W05-SLA(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W05-SLA(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W05-SLA(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W05-SLA(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W05-SLA(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W05-SLA(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W05-SLA(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W05-SLA(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W05-SLA(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W05-SLA(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W05-SLA(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W05-SLA(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W05-SLA(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W05-SLA(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W05-SLA(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W05-SLA(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W05-SLA(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W05-SLA(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W05-SLA(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W05-SLA(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W05-SLA(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W05-SLA(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W05-SLA(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W05-SLA(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W05-SLA(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W05-SLA(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.
      *
      *     ****  ***  ***  *   * *****    ***  *** ****  ****  
      *     *   *  *  *   * *   *   *     *      *  *   * *     
      *     ****   *  *     *****   *      ***   *  *   * ***   
      *     *   *  *  *  ** *   *   *         *  *  *   * *     
      *     *   * ***  ***  *   *   *      ***  *** ****  ****  
      *     
      *              ***  ****   ****   ***   ***  **** 
      *             *   * *      *   * *   * *   * *    
      *             *   * ***    ****  ***** *     ***  
      *             *   * *      *     *   * *  ** *    
      *              ***  *      *     *   *  ***  **** 
      *
       01  SD-RIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W05-SLB(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W05-SLB(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W05-SLB(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W05-SLB(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W05-SLB(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W05-SLB(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W05-SLB(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W05-SLB(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W05-SLB(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W05-SLB(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W05-SLB(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W05-SLB(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W05-SLB(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W05-SLB(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W05-SLB(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W05-SLB(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W05-SLB(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W05-SLB(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W05-SLB(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W05-SLB(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W05-SLB(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W05-SLB(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W05-SLB(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W05-SLB(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W05-SLB(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W05-SLB(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W05-SLB(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W05-SLB(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W05-SLB(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W05-SLB(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W05-SLB(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W05-SLB(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W05-SLB(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W05-SLB(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W05-SLB(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W05-SLB(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W05-SLB(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W05-SLB(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W05-SLB(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W05-SLB(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.
      *
      *   ****   ***  *   * ****   ***  *   *        ***   *** ****   ***  *    *   
      *   *   * *   * **  * *   * *   * ** **       *     *    *   * *   * *    *   
      *   ****  ***** * * * *   * *   * * * *  ***   ***  *    ****  *   * *    *   
      *   *   * *   * *  ** *   * *   * *   *           * *    *   * *   * *    *   
      *   *   * *   * *   * ****   ***  *   *        ***   *** *   *  ***  **** ****
      *     
      *      *    **** **** *****     ***  ****     ****  ***  ***  *   * *****  
      *      *    *    *      *      *   * *   *    *   *  *  *   * *   *   *    
      *      *    ***  ***    *      *   * ****     ****   *  *     *****   *    
      *      *    *    *      *      *   * *   *    *   *  *  *  ** *   *   *    
      *      **** **** *      *       ***  *   *    *   * ***  ***  *   *   *    
      *
       01  SD-RANDOM.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W06-LINE(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W06-LINE(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W06-LINE(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W06-LINE(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W06-LINE(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W06-LINE(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W06-LINE(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W06-LINE(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W06-LINE(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W06-LINE(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W06-LINE(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W06-LINE(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W06-LINE(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W06-LINE(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W06-LINE(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W06-LINE(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W06-LINE(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W06-LINE(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W06-LINE(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W06-LINE(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W06-LINE(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W06-LINE(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W06-LINE(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W06-LINE(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W06-LINE(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W06-LINE(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W06-LINE(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W06-LINE(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W06-LINE(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W06-LINE(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W06-LINE(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W06-LINE(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W06-LINE(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W06-LINE(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W06-LINE(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W06-LINE(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W06-LINE(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W06-LINE(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W06-LINE(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W06-LINE(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.

       01  SD-CRT-CONTROL.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Grey.
               05  LINE 46 COLUMN  2 VALUE "L"                                    FOREGROUND-COLOR Brown HIGHLIGHT. 
               05          COLUMN  3 VALUE   "eft, ".
               05          COLUMN  8 VALUE        "R"                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  9 VALUE         "ight, ".
               05          COLUMN 15 VALUE              X"1B201A"                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 18 VALUE                   ",".
               05          COLUMN 19 VALUE                    "N"                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 20 VALUE                     "ext, ".
               05          COLUMN 25 VALUE                          "P"           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 26 VALUE                           "age or ".
               05          COLUMN 33 VALUE                                  "Esc" FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 36 VALUE                                     "ape".
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID DATA-FILE-IDS LS-SCREEN-PRINT L02-PRINTER-DETAILS.
       AA000-MAIN      SECTION.
       AA000-INIT.
           MOVE LS-PARID             TO WS-PARID.
           MOVE LS-SCREEN-PRINT      TO WS-SCREEN-PRINT.

       AA10-OPTIONS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Must the Screen be cleared - Already been opened.     ³
      *    ³ If a USER selects the screen as output WS-COMMAND is set  ³
      *    ³ to 'I' and this test will clear the screen to a white     ³
      *    ³ background and then exit.                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-COMMAND = "I"
               PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
               MOVE WS-SCREEN-PRINT  TO LS-SCREEN-PRINT
               GO TO AA99.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Printer/Screen/Disk has not been opened yet.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF NOT(L02-PRN-STATUS = "O")
               PERFORM SAVE-SCREEN-3.
      *
      *     ****    P R O C E S S   T H E   R E Q U E S T
      *
             PERFORM PRINT-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Printer/Screen/Disk has been closed.             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF L02-PRN-STATUS = "C"
               PERFORM RESTORE-SCREEN-3.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Move Printer/Screen/Disk controls to linkage area.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE WS-SCREEN-PRINT  TO LS-SCREEN-PRINT.

       AA99.
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
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "PARAM.RD".

       PRINT-SCREEN    SECTION.
       PRINT-STATUS-TEST.
             EVALUATE WS-COMMAND
      *
      *    ****   O P E N   T H E   P R I N T E R   F I L E
      *
               WHEN "O"  IF L02-PRN-STATUS = "C"
                             PERFORM OPEN-PRINT-FILE
                         END-IF
                         GO TO PRINT-SCREEN-EXIT
      *
      *    ****   C L O S E   T H E   P R I N T E R   F I L E
      *
               WHEN "C"  IF L02-PRN-STATUS = "O"
                             PERFORM CLOSE-PRINT-FILE
                         END-IF
                         GO TO PRINT-SCREEN-EXIT
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
               WHEN "S"  IF L02-PRN-STATUS = "O"
                             PERFORM NEW-PAGE
                         END-IF
                         GO TO PRINT-SCREEN-EXIT
             END-EVALUATE.
             MOVE L02-PRN-LINE       TO REP-DETAIL1.
      *
      *    ****   C H E C K   I F   P R I N T E R   F I L E   O P E N
      *
           IF L02-PRN-STATUS = "C"
               MOVE "Printer file not open" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO PRINT-SCREEN-EXIT.
      *
      *    ****   C H E C K   I F   P R I N T E R   O R   S C R E E N
      *
           IF WS-PRINTER = "S"
               PERFORM SCREEN-PRINT
           ELSE
               PERFORM LINE-PRINT-SPOOL.

       PRINT-SCREEN-EXIT.
             EXIT.
      *
      *    ****   O P E N   P R I N T E R   / S P O O L   F I L E
      *
       OPEN-PRINT-FILE SECTION 50.
       OPEN-PRINTER.
           IF L02-LINAGE = 99
               OPEN INPUT PARAM.
             MOVE "P"                TO WS-OPTION.
           
       OPEN-PRINTER-NO.
             MOVE SPACES             TO WS-ERR-MES.
           IF LS-PRINTER = "S"
               STRING "'P'rinter or 'D'isk  [" DELIMITED SIZE WS-OPTION DELIMITED SIZE "]" DELIMITED SIZE
                                     INTO WS-OPT-MES
               PERFORM OPT-MESSAGE TEST AFTER UNTIL WS-OPTION = "P" OR "D"
           ELSE
               STRING "'P'rinter,'S'creen or 'D'isk  [" DELIMITED SIZE WS-OPTION DELIMITED SIZE "]" DELIMITED SIZE
                                     INTO WS-OPT-MES
               PERFORM OPT-MESSAGE TEST AFTER UNTIL WS-OPTION = "P" OR "D" OR "S".
             MOVE WS-OPTION          TO WS-PRINTER.
           IF WS-OPTION = "S"
               PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
               MOVE 62               TO L02-PRN-LENGTH
               MOVE 66               TO L02-PGE-LENGTH
               GO TO OPEN-PRINTER-STATUS.
           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
               END-IF
           ELSE
               MOVE 110              TO WS-USUB.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           EVALUATE WS-REPORT
             WHEN 1   PERFORM OPEN-REPORT-1
             WHEN 2   PERFORM OPEN-REPORT-2
             WHEN 3
             WHEN 4
             WHEN 5
             WHEN 6   PERFORM OPEN-REPORT-3
             WHEN 7   PERFORM OPEN-REPORT-4
             WHEN 8   PERFORM OPEN-REPORT-5
             WHEN 9   PERFORM OPEN-REPORT-6
           END-EVALUATE.
             DIVIDE W25-RESULT BY 2  GIVING WS-PARKEY REMAINDER WS-S6.
             ADD 1                   TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP(WS-S6)    TO WS-COND.
             MOVE TRM-NORP(WS-S6)    TO WS-NORM.
             MOVE TRM-EXPP(WS-S6)    TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN(WS-S6)    TO WS-ECAN.
             MOVE TRM-8LPI(WS-S6)    TO WS-8LPI.
             MOVE TRM-6LPI(WS-S6)    TO WS-6LPI.
             MOVE TRM-10CPI(WS-S6)   TO WS-10CPI.
             MOVE TRM-12CPI(WS-S6)   TO WS-12CPI.
             MOVE TRM-17CPI(WS-S6)   TO WS-17CPI.
             MOVE TRM-LENGTH(WS-S6)  TO WS-PRN-LENGTH L02-PRN-LENGTH.
             MOVE TRM-PAGE(WS-S6)    TO WS-PGE-LENGTH L02-PGE-LENGTH.
             MOVE TRM-SINGLE(WS-S6)  TO WS-PAPER.
           IF TRM-PRN-TYPE(WS-S6) = "H"
               MOVE 77               TO WS-SCREEN-LIN
               MOVE WS-COND          TO WS-ECAN
               MOVE 048              TO W08-DEC(4)
               MOVE 056              TO W08-DEC(5)
               MOVE W08-EXPD         TO WS-EXPP.
           IF WS-PRINTER = "D"
               MOVE "WS"             TO W02-WS
               MOVE "_"              TO W02-US W02-US2
               MOVE LS-USER          TO W02-TERM
               MOVE WS-REPORT        TO W02-REP
               MOVE "."              TO W02-P
               MOVE LS-SYS-ID        TO W02-USER
               MOVE 9                TO WS-PRN-NO.
      *
      *      Is the printer LPT1, 2 or 3
      *
           IF WS-PRN-NO < 4
               PERFORM CHECK-PRINTER THRU CHECK-PRINTER-EXIT
               IF WS-ERROR = 1
                   MOVE "D"          TO WS-OPTION
                   GO TO OPEN-PRINTER-NO
               END-IF
           ELSE
               MOVE "Waiting for printer response" TO WS-ERR-MES
               PERFORM OPT-CURSOR THRU OPT-REPLY.
           IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
               COMPUTE WS-PGE-LENGTH = WS-PGE-LENGTH / 6 * 8
               MOVE WS-PGE-LENGTH    TO L02-PGE-LENGTH
               IF WS-PGE-LENGTH > 88
                   SUBTRACT 7 FROM WS-PGE-LENGTH GIVING WS-PRN-LENGTH L02-PRN-LENGTH
               ELSE
                   SUBTRACT 6 FROM WS-PGE-LENGTH GIVING WS-PRN-LENGTH L02-PRN-LENGTH.
           IF NOT(WS-LINAGE = ZERO OR WS-PGE-LENGTH)
               COMPUTE WS-PRN-LENGTH = WS-LINAGE - (WS-PGE-LENGTH - WS-PRN-LENGTH)
               MOVE WS-LINAGE        TO WS-PGE-LENGTH L02-PGE-LENGTH
               MOVE WS-PRN-LENGTH    TO L02-PRN-LENGTH.
           IF WS-PRINTER = "D"
               OPEN EXTEND PRNSPL
           ELSE
               OPEN OUTPUT PRNREP.
             PERFORM RESTORE-SCREEN.
             CALL X"91" USING X91-RES X91-FUN PRNSPL.
             CALL X"91" USING X91-RES X91-FUN PRNREP.
           IF WS-PRINTER = "D"
               MOVE SPACES           TO SPL-DETAIL1
               MOVE WS-PGE-LENGTH    TO SPL-LIN
               MOVE ZERO             TO SPL-ADV
               WRITE SPL-L1 BEFORE 0 LINES
               MOVE ZERO             TO SPL-LIN.
           IF L02-PRN-TYPE = 8
      *
      *    ****    SET PRINTER TO 8 LPI
      *
               MOVE WS-8LPI          TO REP-DETAIL1
               IF WS-PRINTER = "D"
                   MOVE ZERO         TO SPL-ADV
                   MOVE REP-DETAIL1  TO SPL-DETAIL1
                   WRITE SPL-L1
               ELSE
                   WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE WS-10CPI           TO REP-DETAIL1.
           IF (WS-PGE-LENGTH > 66 AND < 88) OR (WS-PGE-LENGTH > 88)
               IF WS-PRINTER = "D"
                   MOVE 2            TO SPL-ADV
                   MOVE REP-DETAIL1  TO SPL-DETAIL1
                   WRITE SPL-L1
               ELSE
                   WRITE REP-LINE1 BEFORE 2 LINES
               END-IF
           ELSE
           IF WS-PRINTER = "D"
               MOVE ZERO             TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 0 LINES.
               GO TO OPEN-PRINTER-STATUS.

       OPEN-REPORT-1.
           IF TRM-PICP(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-PICP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-PICP(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
               MOVE 2                TO WS-PRN-NO
           ELSE
           IF TRM-PICP(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-PICP(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-PICP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN1(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-2.
           IF TRM-INVP(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-INVP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-INVP(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
               MOVE 2                TO WS-PRN-NO
           ELSE
           IF TRM-INVP(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-INVP(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-INVP(WS-S6) = 9
               MOVE LS-USER  TO W02-USER.
             ADD 501 TRM-PRN2(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-3.
           IF TRM-STDP(WS-S6) = 1 OR 9
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-STDP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-STDP(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
               MOVE 2                TO WS-PRN-NO
           ELSE
           IF TRM-STDP(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-STDP(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER.
             ADD 501 TRM-PRN3(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-4.
           IF TRM-STMP(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN4(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-5.
           IF TRM-CSHP(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
               MOVE ZERO             TO WS-PRN-NO
           ELSE
           IF TRM-CSHP(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
               MOVE 1                TO WS-PRN-NO
           ELSE
           IF TRM-CSHP(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
               MOVE 2                TO WS-PRN-NO
           ELSE
           IF TRM-CSHP(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-CSHP(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-CSHP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN5(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-6.
           IF TRM-POS(WS-S6) = 1
               MOVE "LPT1"           TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 2
               MOVE "LPT2"           TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 3
               MOVE "LPT3"           TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 4
               MOVE "COM1"           TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 5
               MOVE "COM2"           TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN6(WS-S6) GIVING W25-RESULT.

       CHECK-PRINTER.
             MOVE ZERO               TO WS-ERROR WS-S1 WS-S2 WS-S3 WS-S4 WS-S5 WS-S6 WS-S7 WS-S8.
             CALL "PC_TEST_PRINTER" USING WS-PRN-NO WS-PRN-STAT RETURNING WS-STATUS.
           IF WS-PRN-STAT > 127
               MOVE 1                TO WS-S1
               SUBTRACT 128          FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 63
               MOVE 1                TO WS-S2
               SUBTRACT 64           FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 31
               MOVE 1                TO WS-S3
               SUBTRACT 32           FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 15
               MOVE 1                TO WS-S4
               SUBTRACT 16           FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 7
               MOVE 1                TO WS-S5
               SUBTRACT 8            FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 3
               MOVE 1                TO WS-S6
               SUBTRACT 4            FROM WS-PRN-STAT.
           IF WS-PRN-STAT > 1
               MOVE 1                TO WS-S7
               SUBTRACT 2            FROM WS-PRN-STAT.
             MOVE WS-PRN-STAT        TO WS-S8.
           IF WS-S3 = 0 AND WS-S5 = 0 AND WS-S8 = 0
               GO TO CHECK-PRINTER-EXIT.

       PRINTER-NOT-READY.
             MOVE "Check printer! - 'R'etry or 'A'bort  [ ] " TO WS-OPT-MES.
             PERFORM OPT-MESSAGE TEST AFTER UNTIL WS-OPTION = "R" OR "A".
           IF WS-OPTION = "R"
               GO TO CHECK-PRINTER.
             MOVE 1                  TO WS-ERROR.

       CHECK-PRINTER-EXIT.
             EXIT.

       OPEN-SCREEN.
             MOVE 1                  TO WS-S8.

       OPEN-SCREEN-LOOP.
             MOVE SPACES             TO W05-LINE(WS-S8).
           IF WS-S8 < 20
               ADD 1                 TO WS-S8
               GO TO OPEN-SCREEN-LOOP.
             MOVE ZERO               TO WS-SCREEN-LIN.
             MOVE 1                  TO WS-SCREEN WS-LEFT.
             MOVE 78                 TO WS-RIGHT.

       OPEN-SCREEN-EXIT.
             EXIT.

       OPEN-PRINTER-STATUS.
             MOVE SPACES             TO L02-PRN-LINE.
             MOVE "O"                TO L02-PRN-STATUS.
           IF L02-LINAGE = 99
               CLOSE PARAM.

       OPEN-PRINTER-EXIT.
             EXIT.

       CLOSE-PRINT-FILE  SECTION 51.
       CLOSE-PRINTER.
           IF WS-PRINTER = "S"
               GO TO CLOSE-PRINTER-STATUS.
             MOVE WS-10CPI           TO REP-DETAIL1.
           IF WS-PRINTER = "D"
               MOVE ZERO             TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 0 LINES.
             MOVE WS-6LPI            TO REP-DETAIL1.
           IF WS-PRINTER = "D"
               MOVE ZERO             TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 0 LINES.
           IF WS-PRINTER = "D"
               CLOSE PRNSPL
           ELSE
               CLOSE PRNREP.

       CLOSE-PRINTER-STATUS.
             MOVE "C"                TO L02-PRN-STATUS.

       CLOSE-PRINTER-EXIT.
             EXIT.
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
       NEW-PAGE        SECTION 52.
       SKIP-TO-NEW-PAGE.
             MOVE SPACES             TO REP-DETAIL1.
      *
      *    ****   I G N O R E S K I P   I F S C R E E N   I S
      *    S E L E C T E D
      *
           IF WS-PRINTER = "S"
               GO TO NEW-PAGE-EXIT.
           IF WS-PRINTER = "D"
               MOVE "PAGE"           TO SPL-CONT
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
               MOVE ZERO             TO SPL-LIN
           ELSE
               WRITE REP-LINE1 BEFORE PAGE.
           IF WS-COMMAND NOT = "E"
               IF (WS-PGE-LENGTH > 66 AND < 88) OR (WS-PGE-LENGTH > 88)
                   IF WS-PRINTER = "D"
                       MOVE 2        TO SPL-ADV
                       MOVE REP-DETAIL1 TO SPL-DETAIL1
                       WRITE SPL-L1
                   ELSE
                       WRITE REP-LINE1 BEFORE 2 LINES.
           
       NEW-PAGE-EXIT.
             EXIT.
           
       LINE-PRINT-SPOOL  SECTION 53.
       PRINT-A-LINE.
           IF WS-ADVANCE = 0
               PERFORM PRINT-ZERO
           ELSE
           IF WS-ADVANCE = 1
               PERFORM PRINT-SINGLE
           ELSE
           IF WS-ADVANCE = 2
               PERFORM PRINT-DOUBLE
           ELSE
           IF WS-ADVANCE = 99
               PERFORM PRINT-PAGE
           ELSE
               MOVE WS-ADVANCE       TO WS-LINES
               PERFORM PRINT-ADVANCE.
             MOVE LINAGE-COUNTER     TO L02-LINAGE.
             GO TO LINE-PRINT-EXIT.
           
       PRINT-ZERO.
           IF L02-PRN-TYPE = "C"
               MOVE WS-COND          TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "E"
               MOVE WS-EXPP          TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "X"
               MOVE WS-ECAN          TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "1"
               MOVE WS-10CPI         TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "2"
               MOVE WS-12CPI         TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "3"
               MOVE WS-17CPI         TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "6"
               MOVE WS-6LPI          TO REP-DETAIL1
           ELSE
           IF L02-PRN-TYPE = "8"
               MOVE WS-8LPI          TO REP-DETAIL1.
           IF WS-PRINTER = "D"
               MOVE ZERO             TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
              WRITE REP-LINE1 BEFORE 0 LINES.

       PRINT-SINGLE.
           IF WS-PRINTER = "D"
               MOVE 1                TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 1 LINES.

       PRINT-DOUBLE.
           IF WS-PRINTER = "D"
               MOVE 2                TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-PAGE.
           IF WS-PRINTER = "D"
               MOVE "PAGE"           TO SPL-CONT
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
               MOVE ZERO             TO SPL-LIN
           ELSE
               WRITE REP-LINE1 BEFORE PAGE.
           IF WS-COMMAND NOT = "E"
               IF (WS-PGE-LENGTH > 66 AND < 88) OR (WS-PGE-LENGTH > 88)
                   MOVE SPACES       TO REP-DETAIL1
                   IF WS-PRINTER = "D"
                       MOVE 2        TO SPL-ADV
                       MOVE REP-DETAIL1 TO SPL-DETAIL1
                       WRITE SPL-L1
                   ELSE
                       WRITE REP-LINE1 BEFORE 2 LINES.

       PRINT-ADVANCE.
           IF WS-PRINTER = "D"
               MOVE 1                TO SPL-ADV
               MOVE REP-DETAIL1      TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               WRITE REP-LINE1 BEFORE 1 LINES.
             MOVE SPACES             TO REP-DETAIL1.
             SUBTRACT 1              FROM WS-LINES.
           IF WS-LINES > ZERO
               GO TO PRINT-ADVANCE.

       LINE-PRINT-EXIT.
             EXIT.

       SCREEN-PRINT    SECTION 54.
       SCREEN-PRINT-INIT.
             MOVE WS-SCREEN-LIN      TO WS-S8.
           IF WS-SCREEN-LIN < 20
               ADD 1                 TO WS-S8
           ELSE
               GO TO SCREEN-PRINT-SCROLL.

       DISPLAY-A-PRINT-LINE.
             MOVE L02-PRN-TYPE       TO W05-TYPE(WS-S8).
             MOVE L02-PRN-LINE       TO W05-DETAIL(WS-S8).
           IF WS-LEFT > 1 AND < 60
               PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP
           ELSE
           IF WS-SCREEN = 1
               DISPLAY SD-LEFT
           ELSE
               DISPLAY SD-RIGHT.
             GO TO SCREEN-PRINT-CHECK.

       SCREEN-SHIFT-LEFT.
           IF WS-LEFT > 1
               SUBTRACT 1            FROM WS-LEFT WS-RIGHT.
               PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-RIGHT.
           IF WS-RIGHT < 136
               ADD 1                 TO WS-LEFT WS-RIGHT.
               PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-DATA.
             MOVE 1                  TO WS-S1 WS-S2.
             MOVE WS-LEFT            TO WS-S3.

       SCREEN-SHIFT-LOOP.
             MOVE W05-CHAR(WS-S1, WS-S3) TO W06-CHAR(WS-S1, WS-S2).
           IF WS-S2 < 78
               ADD 1                 TO WS-S2 WS-S3
               GO TO SCREEN-SHIFT-LOOP.
           IF WS-S1 < 20
               ADD 1                 TO WS-S1
               MOVE 1                TO WS-S2
               MOVE WS-LEFT          TO WS-S3
               GO TO SCREEN-SHIFT-LOOP.
             DISPLAY SD-RANDOM.

       SCREEN-PRINT-SCROLL.
             MOVE 1                  TO WS-S7.
             MOVE 2                  TO WS-S8.

       SCREEN-PRINT-LOOP.
             MOVE W05-LINE(WS-S8)    TO W05-LINE(WS-S7).
           IF WS-S8 < 20
               ADD 1                 TO WS-S7 WS-S8
               GO TO SCREEN-PRINT-LOOP.
             GO TO DISPLAY-A-PRINT-LINE.

       SCREEN-PRINT-CHECK.
             MOVE WS-S8              TO WS-SCREEN-LIN.
             MOVE SPACES             TO L02-PRN-LINE.
           IF WS-COMMAND = "E"
               GO TO SCREEN-CONTROL.
           IF (WS-S8 = 20) AND (LS-ADVANCE = WS-ADVANCE)
               PERFORM SCREEN-CONTROL THRU SCREEN-CONTROL-EXIT
               IF (WS-COMMAND = "Z") OR (WS-ADVANCE = 1)
                   GO TO SCREEN-PRINT-EXIT.
           IF WS-ADVANCE > 1
               SUBTRACT 1            FROM WS-ADVANCE
               GO TO SCREEN-PRINT-INIT.
           IF WS-SCREEN-LIN < 20
               GO TO SCREEN-PRINT-EXIT.

       SCREEN-CONTROL.
             DISPLAY "L"                                   AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                      "eft, "                                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                           "R"                                     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                            "ight, "                               WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                 X"1B201A"                         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                        ","                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                         "N"                       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                          "ext, "                  WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                               "P"                 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                "age or "          WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue           
                                                       "Esc"       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                          "ape"    WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             
       SCREEN-CONTROL-OPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY     MOVE "Z"  TO WS-COMMAND
                                  GO TO SCREEN-CONTROL-EXIT
                 WHEN PAGE-DOWN   GO TO SCREEN-CLEAR
                 WHEN OTHER       PERFORM AA900-ALARM
                                  GO TO SCREEN-CONTROL-OPT
               END-EVALUATE
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X      TO WS-OPTION
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN LEFT-KEY    PERFORM SCREEN-SHIFT-LEFT
                                  GO TO SCREEN-CONTROL-OPT
                 WHEN RIGHT-KEY   PERFORM SCREEN-SHIFT-RIGHT
                                  GO TO SCREEN-CONTROL-OPT
                 WHEN DOWN-KEY    GO TO SCREEN-CONTROL-EXIT
                 WHEN OTHER       PERFORM AA900-ALARM
                                  GO TO SCREEN-CONTROL-OPT
               END-EVALUATE
           ELSE
               GO TO SCREEN-CONTROL-OPT.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "L" OR "N" OR "R" OR "P")
               GO TO SCREEN-CONTROL-OPT.
           IF WS-OPTION = "R"
               MOVE 2                TO WS-SCREEN
               MOVE 59               TO WS-LEFT
               MOVE 136              TO WS-RIGHT
           ELSE
           IF WS-OPTION = "L"
               MOVE 1                TO WS-SCREEN WS-LEFT
               MOVE 78               TO WS-RIGHT
           ELSE
           IF WS-OPTION = "P"
               GO TO SCREEN-CLEAR
           ELSE
               DISPLAY CLEAR-L50
               GO TO SCREEN-CONTROL-EXIT.
           IF WS-SCREEN = 1
               DISPLAY SD-LEFT
           ELSE
               DISPLAY SD-RIGHT.
             GO TO SCREEN-CONTROL-OPT.

       SCREEN-CLEAR.
             MOVE 1                  TO WS-S8.

       SCREEN-CLEAR-LOOP.
             MOVE SPACES             TO W05-LINE(WS-S8).
           IF WS-S8 < 20
               ADD 1                 TO WS-S8
               GO TO SCREEN-CLEAR-LOOP.
             MOVE ZERO               TO WS-SCREEN-LIN.

       SCREEN-CONTROL-EXIT.
             EXIT.

       SCREEN-PRINT-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
       ZB000-ERROR      SECTION 9.
      
       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 15
              MOVE W02-PARAM         TO WS-FILE
              MOVE WS-PARKEY         TO WS-KEY.

       COPY "DISPERR.PRO".
