      $set LINKCOUNT"772"
     2******************************************************************
     3*                                                                *
     4*    ******   *******   **     **  ********   ******     ****    *
     5*   **    **  **    **  ***    **  **        **    **   **  **   *
     6*   **        **    **  ** *   **  **             **   **    **  *
     7*   **        *******   **  *  **  *****        **     **    **  *
     8*   **        **   **   **   * **  **         **       **    **  *
     9*   **    **  **    **  **    ***  **        **         **  **   *
    10*    ******   **    **  **     **  **        ********    ****    *
    11*                                                                *
    12*     ENGLISH                                                    *
    13*                                                                *
    14*     P R I N T   C R E D I T   N O T E   P R O G R A M          *
    15*                                         (MAXI)                 *
      *     VERSION 8.13.08 - June 2008				       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Jun 2008	- Include user number and name on the invoice  *
      * 		  header record to enable lookup of person     *
      * 		  who captured the Invoice/Credit note.        *
    17* 							       *
    18******************************************************************
    19 IDENTIFICATION DIVISION.
    20 PROGRAM-ID.     CRNF20.
    21 AUTHOR.         J.W.LEMMON (APAC).
    22 DATE-WRITTEN.   JANUARY 2006.
    23
    24		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
    25                               by James William Lemmon.
    26                                 (Id No. 4412165050082).
    27
    28             All rights reserved.
    29
    30		   e-mail jwlemmon@gmail.com.
    31
    32 SECURITY.
    33             This program is free software; you can redistribute
    34             it and/or modify it under the terms of the GNU General
    35             Public License as published by the Free Software
    36             Foundation; either version 2 of the License, or (at
    37             your option) any later version.
    38
    39             This program is distributed in the hope that it will
    40             be useful, but WITHOUT ANY WARRANTY; without even the
    41             implied warranty of MERCHANTABILITY or FITNESS FOR A
    42             PARTICULAR PURPOSE.  See the GNU General Public License
    43             for more details.
    44
    45             You should have received a copy of the GNU General
    46             Public License along with this program; if not, write
    47             to the Free Software Foundation, Inc., 59 Temple Place
    48             - Suite 330, Boston, MA 02111-1307, USA.
    49
    50 ENVIRONMENT DIVISION.
    51 CONFIGURATION SECTION.
    52 SPECIAL-NAMES.
    53                 CURSOR IS CSTART
    54                 CONSOLE IS CRT
    55                 CRT STATUS IS KEY-STATUS.
    56 INPUT-OUTPUT SECTION.
    57 FILE-CONTROL.
    58
    59 COPY APACFIDS.SL.
    69
    70 COPY AUDIT.SL.
    78
    79 COPY CARDEX.SL.
   128
   129 COPY CONTROL.SL.
   137
   138 COPY DBTRAN.SL.
   149
   150 COPY DEBTOR.ISL.
   161
   162 COPY DEBTRN.SL.
   169
   170 COPY DEPART.SL.
   181
   182 COPY GARTEE.SL.
   198
   199 COPY INVOIC.SL.
   215
   216 COPY LEDTRF.SL.
   225
   226 COPY PARAM.SL.
   235
   236 COPY RECOVER.SL.
   242
   243 COPY SALE.SL.
   250
   251 COPY SHARED.SL.
   258
   259 COPY STOCK.SL.
   278
   279 COPY TXTRAN.SL.
   289
   290	   SELECT PRNREP  ASSIGN W02-PRINTER
   291                    ORGANIZATION LINE SEQUENTIAL.
   292
   293/
   294 DATA DIVISION.
   295 FILE SECTION.
   296
   297 COPY APACFIDS.FDE.
   311
   312 COPY AUDIT.FDE.
   396
   397 COPY CARDEX.FDE.
   423
   424 COPY CONTROL.FDE.
   552
   553 COPY DBTRAN.FDE.
   586
   587 COPY DEBTOR.FDE.
   679
   680 COPY DEBTRN.GFD.
   716
   717 COPY DEPART.FDE.
   873
   874 COPY GARTEE.FDE.
   894
   895 COPY INVOIC.FDE.
  1006
  1007 COPY LEDTRF.FDE.
  1069
  1070 COPY PARAM.FDE.
  1811
  1812 COPY RECOVER.GFD.
  2077
  2078 COPY SALE.FDE.
  2171
  2172 COPY SHARED.FDE.
  2202
  2203 COPY STOCK.FDE.
  2359
  2360 COPY TXTRAN.FDE.
  2401
  2402 FD  PRNREP    LABEL RECORD OMITTED
  2403               LINAGE WS-PGE-LENGTH.
  2404 01  REP-LINE1.
  2405     03  REP-DETAIL1     PIC  X(80).
  2406
  2407 01  REP-L1.
  2408     03  FILLER          PIC  X(10).
  2409     03  BAL-REP         PIC  X(60).
  2410     03  FILLER          PIC  X(10).
  2411
  2412 01  REP-LINE2.
  2413     03  REP-DETAIL2     PIC  X(96).
  2414
  2415 01  INV-L1.
  2416     03  FILLER          PIC  X(01).
  2417     03  INV-NADD        PIC  X(48).
  2418     03  INV-RADD      REDEFINES INV-NADD.
  2419	       05  INV-PC      PIC  X(04).
  2420         05  FILLER      PIC  X(44).
  2421     03  FILLER          PIC  X(14).
  2422     03  INV-PAGE        PIC  X(06).
  2423     03  INV-P-NO        PIC  Z(03)9.
  2424     03  FILLER          PIC  X(23).
  2425
  2426 01  INV-L1A.
  2427     03  FILLER          PIC  X(01).
  2428     03  INV-NAME        PIC  X(48).
  2429     03  INV-DADD      REDEFINES INV-NAME.
  2430         05  INV-VATH.
  2431           07  INV-PC1   PIC  X(04).
  2432           07  FILLER    PIC  X(03).
  2433         05  INV-VATNO.
  2434           07  FILLER    PIC  X(41).
  2435     03  FILLER          PIC  X(13).
  2436     03  INV-GNO         PIC  X(14).
  2437     03  FILLER          PIC  X(20).
  2438
  2439 01  INV-L2.
  2440     03  FILLER          PIC X(64).
  2441     03  INV-CREDIT.
  2442         05  FILLER      PIC X(06).
  2443         05  INV-TYPE    PIC X(20).
  2444         05  FILLER      PIC X(06).
  2445     03  INV-REPRINT REDEFINES INV-CREDIT.
  2446         05  FILLER      PIC X(06).
  2447         05  INV-EXPAND  PIC X(06).
  2448         05  INV-COPY    PIC X(12).
  2449         05  INV-CANEX   PIC X(06).
  2450         05  FILLER      PIC X(02).
  2451
  2452 01  INV-L3.
  2453     03  FILLER          PIC  X(01).
  2454     03  INV-DTE         PIC  99/99/9999.
  2455     03  FILLER          PIC  X(01).
  2456     03  INV-TIME        PIC  X(08).
  2457     03  FILLER          PIC  X(01).
  2458     03  INV-SMAN        PIC  X(19).
  2459     03  FILLER          PIC  X(01).
  2460     03  INV-REF         PIC  X(08).
  2461     03  FILLER          PIC  X(08).
  2462     03  INV-AC          PIC  X(06).
  2463     03  FILLER          PIC  X(06).
  2464     03  INV-TERMS       PIC  X(04).
  2465     03  FILLER          PIC  X(07).
  2466     03  INV-ORD         PIC  X(12).
  2467     03  FILLER          PIC  X(04).
  2468
  2469 01  INV-L4.
  2470     03  FILLER          PIC  X(01).
  2471     03  INV-ITM         PIC  X(11).
  2472     03  FILLER          PIC  X(01).
  2473     03  INV-MES.
  2474         05  FILLER      PIC  X(01).
  2475         05  INV-DESC.
  2476             07  FILLER  PIC  X(20).
  2477		   07  INV-FWD PIC  X(22).
  2478     03  INV-REMKS REDEFINES INV-MES.
  2479         05  FILLER      PIC  X(01).
  2480         05  INV-RMKS    PIC  X(43).
  2481     03  FILLER          PIC  X(01).
  2482     03  INV-QNT         PIC  Z(04)9.99.
  2483     03  FILLER          PIC  X(01).
  2484     03  INV-PRICE       PIC  Z(03)9.99-.
  2485     03  FILLER          PIC  X(01).
  2486     03  INV-DSC         PIC  Z9.99 BLANK WHEN ZERO.
  2487     03  FILLER          PIC  X(02).
  2488     03  INV-VAL         PIC  Z(06)9.99-.
  2489     03  FILLER          PIC  X(02).
  2490
  2491 01  INV-L5.
  2492     03  FILLER          PIC  X(13).
  2493     03  INV-HD          PIC  X(10).
  2494     03  INV-SERNOS.
  2495         05  FILLER      PIC  X(01).
  2496         05  INV-SERNO   PIC  X(10).
  2497         05  FILLER      PIC  X(23).
  2498     03  FILLER          PIC  X(39).
  2499
  2500/
  2501 WORKING-STORAGE SECTION.
  2502 77  WS-CHECK        PIC  X(18)    VALUE
  2503                     "aeWlimemnomLalismJ".
  2504 77  WS-DISC         PIC  99V99    COMP-3.
  2505 77  WS-TDISC        PIC  99V99    COMP-3.
  2506 77  WS-SUB          PIC  9(04)    COMP-5.
  2507 77  WS-SUPER        PIC  9(02)    COMP-5.
  2508 77  WS-P1           PIC  9(04)    COMP-5.
  2509 77  WS-S1           PIC  9(04)    COMP-5.
  2510 77  WS-S2           PIC  9(04)    COMP-5.
  2511 77  WS-S3           PIC  9(04)    COMP-5.
  2512 77  WS-S4           PIC  9(04)    COMP-5.
  2513 77  WS-S5           PIC  9(04)    COMP-5.
  2514 77  WS-S6           PIC  9(04)    COMP-5.
  2515 77  WS-S7           PIC  9(04)    COMP-5.
  2516 77  WS-S8           PIC  9(04)    COMP-5.
  2517 77  WS-S9           PIC  9(04)    COMP-5.
  2518 77  WS-IXS          PIC  9(04)    COMP-5.
  2519 77  WS-IXS1         PIC  9(04)    COMP-5.
  2520 77  WS-IXS2         PIC  9(04)    COMP-5.
  2521 77  WS-IXS3         PIC  9(04)    COMP-5.
  2522 77  WS-IXS4         PIC  9(04)    COMP-5.
  2523 77  WS-DBTKEY       PIC  9(04)    COMP-5.
  2524 77  WS-PARKEY       PIC  9(04)    COMP-5.
  2525 77  WS-KEYSTR       PIC  9(06)    COMP-5.
  2526 77  WS-SALSTR       PIC  9(04)    COMP-5.
  2527 77  WS-SALKEY       PIC  9(04)    COMP-5.
  2528 77  WS-NETKEY       PIC  9(04)    COMP-5.
  2529 77  WS-AUDKEY       PIC  9(06)    COMP-5.
  2530 77  WS-RECKEY       PIC  9(06)    COMP-5.
  2531 77  WS-RECOVER      PIC  9(06)    COMP-5.
  2532 77  WS-SHARED       PIC  9(04)    COMP-5 VALUE 1.
  2533 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
  2534 77  WS-STOCK        PIC  X(01).
  2535 77  WS-CARDEX       PIC  X(01).
  2536 77  WS-USE-ITM      PIC  X(01).
  2537 77  WS-EXT-STK      PIC  X(01).
  2538 77  WS-PRC-IND      PIC  X(01).
  2539 77  WS-GAR          PIC  9(01) VALUE 0.
  2540 77  WS-NUM          PIC  Z9.
  2541 77  WS-TME2         PIC  9(08).
  2542 77  WS-PAR          PIC  9(01) VALUE 0.
  2543 77  WS-INO          PIC  9(02) VALUE 0.
  2544 77  WS-PGE-LENGTH   PIC  9(02) VALUE 68.
  2545 77  WS-LINES        PIC  9(02) VALUE 0.
  2546 77  WS-FIRST        PIC  9(03) VALUE 0.
  2547 77  WS-LAST         PIC  9(03) VALUE 0.
  2548 77  WS-LASTINV      PIC  9(04)    COMP-5.
  2549 77  WS-LINE         PIC  9(04)    COMP-5.
  2550 77  WS-PAGE         PIC  9(02)    COMP-5.
  2551 77  WS-L1           PIC  9(04)    COMP-5.
  2552 77  WS-COND         PIC  X(06).
  2553 77  WS-NORM         PIC  X(06).
  2554 77  WS-EXPP         PIC  X(06).
  2555 77  WS-ECAN         PIC  X(06).
  2556 77  WS-8LPI         PIC  X(06).
  2557 77  WS-6LPI         PIC  X(06).
  2558 77  WS-10CPI	   PIC	X(06).
  2559 77  WS-12CPI        PIC  X(06).
  2560 77  WS-17CPI        PIC  X(06).
  2561 77  WS-OPTION       PIC  X(01).
  2562 77  WS-ALLOCATE     PIC  X(01).
  2563 77  WS-SKIP         PIC  X(01) VALUE " ".
  2564 77  WS-LOOK         PIC  X(01) VALUE "S".
  2565 77  WS-ETYPE        PIC  X(01) VALUE "I".
  2566 77  WS-ERROR        PIC  9(01).
  2567 77  WS-PRN-NO       PIC  X(01)    COMP-X VALUE 9.
  2568 77  WS-PRN-STAT     PIC  X(01)    COMP-X.
  2569 77  WS-COMP         PIC  9(01).
  2570 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
  2571 77  WS-CONREC       PIC  9(01) VALUE ZERO.
  2572 77  WS-HEAD         PIC  X(01).
  2573 77  WS-TYPE         PIC  X(01).
  2574 77  WS-AGE          PIC  X(01).
  2575 77  WS-PRC          PIC  X(01).
  2576 77  WS-INPUT        PIC  X(01) VALUE "N".
  2577 77  WS-COPY         PIC  X(01) VALUE "N".
  2578 77  WS-NOSELL       PIC  X(01) VALUE "N".
  2579 77  WS-PRNINV       PIC  X(01) VALUE "Y".
  2580 77  WS-PER          PIC  Z(02)9.99.
  2581 77  WS-CR           PIC  X(01).
  2582 77  TODAY-DDMMYY    PIC  9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
      /
003050 01  WS-DB-LINE.
003080     03  WS-TOP-LNE2.
003090	       05  WS-TCR  PIC X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
003130         05  FILLER      PIC  X(39).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".

003480 COPY WS.WS.

       COPY LEDTRF.WS.

003520 01  W00-TRAN-CODES.
003530     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
003540     03  W00-E-DESC  PIC  X(12).
003550     03  W00-A-DESC  PIC  X(12).
003560     03  W00-ACTION  PIC  9(01).
003570     03  W00-T-CODE  PIC  9(02)    COMP-5.
003580     03  W00-T-GL    PIC  9(06)    COMP-3.
003590     03  W00-T-DAY   PIC  9(06)    COMP-5.
003600     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
003610     03  W00-T-MTD   PIC  9(06)    COMP-5.
003620     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
003630     03  W00-T-YTD   PIC  9(06)    COMP-5.
003640     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

       COPY CARDEX.ID.

003700 COPY CONTROL.ID.

003720 COPY DBTRAN.ID.

003740 COPY DEBTOR.ID.

000140 COPY DEBTRN.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY LEDTRF.ID.

003800 COPY INVOIC.ID.

       COPY PARAM.ID.

003820 COPY RECOVER.ID.

003840 COPY SALE.ID.

       COPY SHARED.ID.

003860 COPY STOCK.ID.

003880 COPY TXTRAN.ID.

003900 01  W02-FILE-IDS.
003910     03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "CRNF01.".
               05  W02-USER   PIC X(05) VALUE SPACES.
	   03  W02-CSHDRW     PIC X(04) VALUE "COM1".

       COPY W05.VAT.

       COPY W10.SAL.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

004610 01  W15-DISCOUNT.
004620     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

004720 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004770     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004780     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004790     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
  3521	   03  W25-TIME        PIC 9(08).
  3522     03  W25-TRED REDEFINES  W25-TIME.
  3523         05  W25-HH      PIC 9(02).
  3524         05  W25-MM      PIC 9(02).
  3525         05  W25-SS      PIC 9(02).
  3526         05  FILLER      PIC 9(02).

       COPY W40.DBT.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

005610 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).
005690     03  W60-ITM.
005700         05  W60-ICHAR OCCURS 14 PIC X(01).

005810 COPY W70DEBT.IWS.

005830 01  W75-KEYS.
005840     03  W75-S          PIC 9(02) COMP-5.
005850     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

006040 01  W80-ACCOUNT.
006050     03  W80-ACNO        PIC X(06).
006060     03  W80-CRED REDEFINES W80-ACNO.
006070         05  W80-CNO     PIC 9(05).
006080         05  W80-CAC REDEFINES W80-CNO.
006090             07  W80-CTYPE PIC 9(01).
006100             07  W80-SER   PIC 9(04).
006110         05  FILLER        PIC 9(01).
006120     03  W80-TYPE        PIC 9(01).

       COPY W90.DBT.

       COPY W91.TRN.

       COPY W95.STM.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S08A.
           03  LINE 16 COLUMN  6 VALUE "Allocation:".
           03  LINE 17 COLUMN  6 VALUE "Ref. No.".
	   03	       COLUMN 18 VALUE "Date".
	   03	       COLUMN 26 VALUE "Transaction".
	   03	       COLUMN 46 VALUE "Value".
	   03	       COLUMN 57 VALUE "Balance".
	   03	       COLUMN 69 VALUE "Allocate".

       01  S-ALLOC.
	   03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				   PIC Z(08)9.99 FROM W75-BALANCE.
	   03	       COLUMN 51 VALUE "Allocated : ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				   PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-DTRN.
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(08) FROM DBT-REF.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC 9999/99/99 FROM DBT-DATE.
	   03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(12) FROM W00-E-DESC.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-VALUE.
	   03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-OUTSTD.
	   03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 USING DBT-ALLOC AUTO.

       COPY ERROR.CRT.

      /
  4548 PROCEDURE DIVISION USING LS-PARID
  4549                          LS-USER-ID
  4550                          LS0-PROGRAMS
				LS0-SECURITY
  4551                          W40-COMPANY
  4552                          W90-TRAN.
  4553 AA000-MAIN      SECTION.
  4554 AA000-INIT.
  4555       PERFORM ZA000-INIT.
  4556       PERFORM BD000.
  4557       GO TO AZ000-EOJ.
  4558
  4559/    *************************************************************
  4560*    ****   T H I S   R O U T I N E   I S   U S E D   T O
  4561*           D I S P L A Y   E R R O R   M E S S A G E S
  4562*    *************************************************************
  4563*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  4564*    ³                      ERROR-MESSAGE                        ³
  4565*    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  4566*    ³ To display an error message with " - Press ANY key" at    ³
  4567*    ³ the end; use PERFORM ERROR-MESSAGE.                       ³
  4568*    ³                                                           ³
  4569*    ³ To display just the error message; use PERFORM            ³
  4570*    ³ ERROR-LENGTH THRU ERROR-EXIT.                             ³
  4571*    ³                                                           ³
  4572*    ³ To display the error message higher or lower (default is  ³
  4573*    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
  4574*    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
  4575*    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
  4576*    ³ THRU ERROR-EXIT.                                          ³
  4577*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  4578
  4579 COPY ERROR.SCR.
  4685
  4686/    *************************************************************
  4687*    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
  4688*            S C R E E N ,   K E Y B O A R D   &   M O U S E
  4689*    *************************************************************
  4690*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  4691*    ³       SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
  4692*    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
  4693*    ³                      SCREEN-SHADOW                        ³
  4694*    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  4695*    ³ To routine is used to display a shadow down the right and ³
  4696*    ³ along the bottom of a pop-up box. The parameters that are ³
  4697*    ³ required:                                                 ³
  4698*    ³           SHADE-ROW   - Top line of the box + 1.          ³
  4699*    ³           SHADE-COL   - Left line of box + 2.             ³
  4700*    ³           SHADE-WIDTH - Width of the box - 2.             ³
  4701*    ³           SHADE-LINES - Hight of box - 1.                 ³
  4702*    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
  4703*    ³                      ERROR-MESSAGE                        ³
  4704*    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  4705*    ³ To display an error message with " - Press ANY key" at    ³
  4706*    ³ the end; use PERFORM ERROR-MESSAGE.                       ³
  4707*    ³                                                           ³
  4708*    ³ To display just the error message; use PERFORM            ³
  4709*    ³ ERROR-LENGTH THRU ERROR-EXIT.                             ³
  4710*    ³                                                           ³
  4711*    ³ To display the error message higher or lower (default is  ³
  4712*    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
  4713*    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
  4714*    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
  4715*    ³ THRU ERROR-EXIT.                                          ³
  4716*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  4717
  4718 COPY FUNCTION.SCR.
  4942
  4943 COPY LOCKED.RC2.
  4983
  4984*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  4985*    ³          ERASE SCREEN FROM SPECIFIED LOCATION             ³
  4986*    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  4987*    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
  4988*    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
  4989*    ³ The screen is cleared with Column 1 and 80 containing "³" ³
  4990*    ³ and columns 2 to 79 containing spaces.                    ³
  4991*    ³  eg.                                                      ³
  4992*    ³      MOVE 0801      TO CPOS.                              ³
  4993*    ³      PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.           ³
  4994*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  4995
  4996 COPY CLEAR.CRT.

010970 COPY DEBTOR.TRN.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011110 AC000		  SECTION.

011130 COPY AUDIT.RD.

011150 COPY CONTROL.RD.

011170 COPY DBTRAN.RD1.

011190 COPY DEBTOR.RD.

000140 COPY DEBTRN.RD.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

       COPY LEDTRF.RD.

011250 COPY INVOIC.RD.

011270 COPY PARAM.RD.

011290 COPY SALE.RD.

       COPY SHARED.RD.

011310 COPY STOCK.RD.

011330 COPY TXTRAN.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011350 AD000-WRITE	       SECTION.

011370 COPY AUDIT.WR.

       COPY CARDEX.WR.

011390 COPY CONTROL.WR.

011410 COPY DBTRAN.WR.

011430 COPY DEBTOR.WR.

000140 COPY DEBTRN.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011490 COPY INVOIC.WR.

       COPY LEDTRF.WR.

011510 COPY PARAM.WR.

011530 COPY SALE.WR.

       COPY SHARED.WR.

011550 COPY STOCK.WR.

011570 COPY TXTRAN.WR.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ==>  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ==>      (REWRITE)    ³
      *        ³ 1 = RECORD WAS ADDED	     ==>      (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ==>      (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011670 AY000	       SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
011740 AY02.
011750       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011770	     PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011780       MOVE DEB-RECORD1    TO REC-DEBTOR.
011790       GO TO AY50.
011800 AY04.
011810       MOVE 04             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011830       MOVE TRA-RECORD1    TO REC-DBTRAN.
011840       GO TO AY50.
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
011850 AY09.
011860       MOVE 09             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE WS-ACTION      TO REC-TYPE.
011900       MOVE GUA-RECORD1    TO REC-GARTEE.
011910       GO TO AY50.
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
012030 AY22.
012040       MOVE 22             TO REC-FILE.
012050       MOVE ZERO           TO REC-KEY.
012060       MOVE WS-ACTION      TO REC-TYPE.
012070       MOVE DOC-REC1       TO REC-INVOIC.
012080       GO TO AY50.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
012090 AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
012140       GO TO AY50.
012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
012200 AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390         DISPLAY "Write error Recovery file - Status " AT 2212
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012430	     CLOSE RECOVER.
012440	     OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
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
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013100 AY999.
013110       EXIT.
  7071/
  7072 AZ000-END               SECTION.
  7073 AZ000-EOJ.
  7074       MOVE WS-10CPI       TO REP-DETAIL2.
  7075       WRITE REP-LINE2 BEFORE 0 LINES.
  7076       MOVE WS-6LPI        TO REP-DETAIL2.
  7077       WRITE REP-LINE2 BEFORE 0 LINES.
  7078       CLOSE RECOVER
  7079             PRNREP.
  7080 AZ100.
  7081       EXIT PROGRAM.
  7082*
  7083*    ****    Read debtor record - using account number.
  7084*
  7085 CA155-GET-DEBTOR  SECTION 2.
  7086 CA155.
  7087       MOVE W80-ACNO       TO DEB-ACNO.
  7088       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
  7089     IF WS-F-ERROR = 6
  7090         MOVE 1            TO WS-ERROR
  7091     ELSE
  7092     IF DEB-ACNO NOT = W80-ACNO
  7093         MOVE 2            TO WS-ERROR
  7094     ELSE
  7095         MOVE 0            TO WS-ERROR
  7096         MOVE DEB-TYPE     TO W80-TYPE.
  7097 CA160-EXIT.
  7098       EXIT.
  7099*
  7100 CA165-NAME        SECTION 2.
  7101 CA165.
  7102       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
  7103                           INTO W40-SURNAME
  7104                                W40-FNAME
  7105                                W40-SNAME
  7106       OVERFLOW
  7107           MOVE 1          TO WS-ERROR.
  7108 CA170-EXIT.
  7109       EXIT.
  7110*
  7111 CA175-ADDRESS     SECTION 2.
  7112 CA175.
  7113       UNSTRING W60-ADDRESS DELIMITED BY "*"
  7114                           INTO W40-ADD1
  7115                                W40-ADD2
  7116                                W40-ADD3
  7117                                W40-ADD4
  7118       OVERFLOW
  7119           MOVE 1          TO WS-ERROR.
  7120 CA175-EXIT.
  7121       EXIT.
  7122*
  7123 CA185-ADDRESS     SECTION 2.
  7124 CA185.
  7125       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
  7126                           INTO W40-PADD1
  7127                                W40-PADD2
  7128                                W40-PADD3
  7129				      W40-PADD4
  7130       OVERFLOW
  7131           MOVE 1          TO WS-ERROR.
  7132 CA185-EXIT.
  7133       EXIT.
  7134
  7135 COPY LEDTRF.UPD.
  7206
  7207/
  7208*    ****    G E N E R A T E   A   D E B T O R
  7209*            T R A N S A C T I O N
  7210*
  7211 GENERATE-DEBTOR-TRAN SECTION 2.
  7212
  7213 DEBTOR-TRAN.
  7214
  7215 LINK-DEBTOR-TRANS.
  7216       INITIALIZE TRA-RECORD1.
  7217       MOVE W20-DTE        TO TRA-DATE.
  7218       MOVE W20-DTE        TO DEB-ACTIVE.
  7219       MOVE W90-CODE       TO TRA-CODE.
  7220       MOVE W90-REF        TO TRA-REF TRA-REG.
  7221       MOVE DEB-ACNO       TO TRA-AC.
  7222     IF W90-DB NOT = ZERO
  7223         MOVE W90-DB       TO TRA-VALUE
  7224     ELSE
  7225         MOVE W90-CR       TO TRA-VALUE.
  7226     IF W00-ACTION = 1
  7227         MULTIPLY -1 BY W90-NONTAX
  7228                           GIVING TRA-TAXFREE
  7229     ELSE
  7230         MOVE W90-NONTAX   TO TRA-TAXFREE.
  7231       MOVE W90-TTAX       TO TRA-TAX.
  7232       ADD TRA-TAX         TO W00-T-VAT.
  7233       PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
  7234       MOVE 1              TO WS-ACTION.
  7235       PERFORM AY04 THRU AY59.
  7236       MOVE 1              TO TAX-ACTYPE.
  7237       MOVE TRA-CODE       TO TAX-CODE.
  7238       MOVE TRA-DATE       TO TAX-DATE.
  7239       MOVE TRA-REF        TO TAX-REF.
  7240       MOVE TRA-VALUE      TO TAX-VALUE.
  7241       MOVE TRA-TAXFREE    TO TAX-TAXFREE.
  7242*      MOVE TRA-TAX        TO TAX-VAT.
  7243       MOVE TRA-AC         TO TAX-AC.
  7244       IF W90-CR NOT = ZERO
  7245           MOVE "1"        TO TAX-TYPE
  7246       ELSE
  7247           MOVE "2"        TO TAX-TYPE.
  7248       MOVE ZERO           TO TAX-SEQ.
  7249       MOVE 1              TO WS-S9.
  7250
  7251 DEBTOR-TAX.
  7252       MULTIPLY -1 BY W90-TAXVAL(WS-S9)
  7253                           GIVING TAX-VAL(WS-S9).
  7254       MULTIPLY -1 BY W90-VAT(WS-S9)
  7255                           GIVING TAX-VAT(WS-S9).
  7256     IF WS-S9 < 12
  7257         ADD 1             TO WS-S9
  7258         GO TO DEBTOR-TAX.
  7259       PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
  7260       MOVE 1              TO WS-ACTION.
  7261       PERFORM AY19 THRU AY59.
  7262       ADD TRA-VALUE       TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
  7263       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
  7264       PERFORM UPDATE-TRAN-RECORD.
  7265	   IF WX-INT = "Y"
  7266         PERFORM DEBTOR-INTEGRATE.
  7267     IF W90-CR NOT = ZERO
  7268         GO TO DEBTOR-CR-TRANS.
  7269       ADD W90-DB          TO DEB-DEBT
  7270                              DEB-BAL
  7271                              DEB-PURYTD
  7272                              DEB-MTHPUR (W20-MONTH).
  7273       GO TO DEBTOR-TRAN-EXIT.
  7274*
  7275*    ****    I N T E G R A T I O N   T O   T H E   G / L E D G E R
  7276*
  7277 DEBTOR-INTEGRATE.
  7278       MOVE W20-DTE        TO WX-DATE.
  7279       MOVE "  DT"         TO WX-LEDG.
  7280       MOVE W90-CODE       TO WX-TRN.
  7281       MOVE W00-E-DESC     TO WX-DESC.
  7282       MOVE "- ACCOUNTS RECEIVABLE"
  7283                           TO WX-BOOK.
  7284       MOVE TRA-VALUE      TO WX-VALUE.
  7285       MOVE WX-DEBGL       TO WX-AC.
  7286       MOVE 01             TO WX-TYPE.
  7287       PERFORM GL-TRF-UPDATE.
  7288
  7289 DEBTOR-CR-TRANS.
  7290       SUBTRACT W90-CR     FROM DEB-CRED.
  7291       ADD W90-CR          TO DEB-MTHPUR (W20-MONTH) DEB-PURYTD.
  7292       ADD W90-CR          TO DEB-BAL.
  7293     IF OPEN-ITEM
  7294         GO TO DEBTOR-TRAN-EXIT.
  7295     IF WS-AGE = "M"
  7296         GO TO DEBTOR-TRAN-EXIT.
  7297       MOVE ZERO           TO W90-CUR W90-30 W90-60 W90-90.
  7298       MOVE W90-CR         TO W90-VALUE.
  7299     IF WS-AGE = "1"
  7300         ADD W90-VALUE     TO DEB-120
  7301         MOVE W90-VALUE    TO W90-120.
  7302     IF DEB-120 < ZERO
  7303         SUBTRACT DEB-120  FROM W90-120
  7304         MOVE DEB-120      TO W90-VALUE
  7305         MOVE ZERO         TO DEB-120
  7306         MOVE "9"          TO WS-AGE.
  7307     IF WS-AGE = "9"
  7308         ADD W90-VALUE     TO DEB-90
  7309         MOVE W90-VALUE    TO W90-90.
  7310     IF DEB-90 < ZERO
  7311         SUBTRACT DEB-90   FROM W90-90
  7312         MOVE DEB-90       TO W90-VALUE
  7313         MOVE ZERO         TO DEB-90
  7314         MOVE "6"          TO WS-AGE.
  7315     IF WS-AGE = "6"
  7316         ADD W90-VALUE     TO DEB-60
  7317         MOVE W90-VALUE    TO W90-60.
  7318     IF DEB-60 < ZERO
  7319         SUBTRACT DEB-60   FROM W90-60
  7320         MOVE DEB-60       TO W90-VALUE
  7321         MOVE ZERO         TO DEB-60
  7322         MOVE "3"          TO WS-AGE.
  7323	   IF WS-AGE = "3"
  7324         ADD W90-VALUE     TO DEB-30
  7325         MOVE W90-VALUE    TO W90-30.
  7326     IF DEB-30 < ZERO
  7327         SUBTRACT DEB-30   FROM W90-30
  7328         MOVE DEB-30       TO W90-VALUE
  7329         MOVE ZERO         TO DEB-30
  7330         MOVE "C"          TO WS-AGE.
  7331     IF WS-AGE = "C"
  7332         ADD W90-VALUE     TO DEB-CUR
  7333         MOVE W90-VALUE    TO W90-CUR.
  7334     IF DEB-CUR < ZERO
  7335         SUBTRACT DEB-CUR  FROM W90-CUR
  7336         MOVE ZERO         TO DEB-CUR.
  7337
  7338 DEBTOR-TRAN-EXIT.
  7339       EXIT.
  7340
  7341 COPY DEBTOR.ALL.

  7539/
  7540 BA000                    SECTION 50.
  7541 BA90.
  7542       MOVE W70-AUDIT      TO WS-AUDKEY.
  7543       MOVE LOW-VALUES     TO AUD-REC1.
  7544       PERFORM AY16 THRU AY59.
  7545       MOVE 7              TO AUD-TYPE.
  7546       MOVE WS-DRAW        TO AUD-SUB.
  7547       MOVE W90-REF        TO AUD-CSH.
  7548       MOVE W90-EXCTOT     TO AUD-SELL.
  7549       MOVE W90-TCOST      TO AUD-COST.
  7550       MOVE W10-SMAN       TO AUD-SMAN.
  7551       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7552       ADD 1               TO W70-AUDIT.
  7553       MOVE 1              TO WS-AUDKEY.
  7554       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7555       PERFORM AY16 THRU AY59.
  7556       MOVE 1              TO AUD-REP7.
  7557       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7558
  7559 BA95.
  7560       MOVE W70-AUDIT      TO WS-AUDKEY.
  7561       MOVE LOW-VALUES     TO AUD-REC1.
  7562       PERFORM AY16 THRU AY59.
  7563       MOVE 3              TO AUD-TYPE
  7564       MOVE DEB-ACNO       TO AUD-AC.
  7565       MOVE WS-DRAW        TO AUD-SUB.
  7566       MOVE W90-REF        TO AUD-REF.
  7567       MOVE W90-TTAX       TO AUD-TAX.
  7568       MOVE W90-TOTAL      TO AUD-VAL.
  7569       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7570       ADD 1               TO W70-AUDIT.
  7571       MOVE 1              TO WS-AUDKEY.
  7572       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7573       PERFORM AY16 THRU AY59.
  7574       MOVE 1              TO AUD-REP3.
  7575       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7576 BA100.
  7577       MOVE W70-AUDIT      TO WS-AUDKEY.
  7578       MOVE LOW-VALUES     TO AUD-REC1.
  7579       PERFORM AY16 THRU AY59.
  7580       MOVE 8              TO AUD-TYPE.
  7581       MOVE WS-DRAW        TO AUD-SUB.
  7582       MOVE W10-SMAN       TO AUD-SMAN.
  7583       MOVE W90-REF        TO AUD-CSH.
  7584       MOVE W90-VALUE      TO AUD-SEL.
  7585       MOVE W90-ICST       TO AUD-CST.
  7586       MOVE STK-CODE       TO AUD-EXT-STOCK.
  7587       MOVE WS-SUPER       TO AUD-SUPER.
  7588       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7589       ADD 1               TO W70-AUDIT.
  7590       MOVE 1              TO WS-AUDKEY.
  7591       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7592       PERFORM AY16 THRU AY59.
  7593       MOVE 1              TO AUD-REP8.
  7594       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7595
  7596/
  7597 BB050               SECTION 50.
  7598 BB50.
  7599     IF WS-PRNINV = "N"
  7600         GO TO BB60.
  7601       MOVE SPACES         TO REP-DETAIL2.
  7602     IF WS-HEAD = "N"
  7603         WRITE INV-L1 BEFORE 11 LINES
  7604         GO TO BB60.
  7605       MOVE W95-COMP       TO INV-NADD.
  7606       WRITE INV-L1 BEFORE 1 LINES.
  7607       MOVE W95-ADD1       TO INV-NADD.
  7608       WRITE INV-L1 BEFORE 1 LINES.
  7609       MOVE SPACES         TO REP-DETAIL2.
  7610       MOVE W95-ADD2       TO INV-NADD.
  7611       WRITE INV-L1 BEFORE 1 LINES.
  7612       MOVE W95-ADD3       TO INV-NADD.
  7613       WRITE INV-L1 BEFORE 1 LINES.
  7614       MOVE W95-ADD4       TO INV-NADD.
  7615       WRITE INV-L1 BEFORE 1 LINES.
  7616 BB55.
  7617       MOVE SPACES         TO REP-DETAIL2.
  7618       MOVE W95-PC         TO INV-PC.
  7619       WRITE INV-L1 BEFORE 1 LINES.
  7620       MOVE SPACES         TO REP-DETAIL2.
  7621       WRITE INV-L1 BEFORE 1 LINES.
  7622       GO TO BB60.
  7623 BB60.
  7624       EXIT.
  7625/
  7626 BB070               SECTION 50.
  7627 BB70.
  7628       INITIALIZE GUA-RECORD1.
  7629       MOVE ZERO           TO WS-F-ERROR.
  7630       MOVE STK-CODE       TO GUA-EXT-CODE.
  7631       MOVE W10-SERNO      TO GUA-SERIAL.
  7632       PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
  7633     IF WS-F-ERROR = 9
  7634         GO TO BB99.
  7635       MOVE 2              TO WS-ACTION.
  7636       PERFORM AY09 THRU AY59.
  7637       PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
  7638 BB99.
  7639       EXIT.
  7640/
  7641*    ****    C R E D I T   N O T E
  7642*
  7643 BD000           SECTION 50.
  7644 BD0-PASS.
  7645       MOVE 1              TO WS-FIRST WS-LAST.
  7646       MOVE "C"            TO WS-CR.
  7647       MOVE ZERO           TO W90-DISCOUNT.
  7648 BD05.
  7649*
  7650*    ****    D E B T O R   A C C O U N T   N U M B E R
  7651*
  7652       MOVE W40-ACNO       TO W80-ACNO.
  7653       PERFORM CA155-GET-DEBTOR.
  7654 BD06.
  7655*
  7656*    ****    S A L E S M A N   N U M B E R
  7657*
  7658       MOVE W40-SMAN       TO W10-SMAN.
  7659       MOVE W40-LAST       TO WS-LAST.
  7660       MOVE W40-PRC        TO WS-PRC.
  7661*
  7662*    ****    P R O C E S S   T H E   C R E D I T   N O T E
  7663*
  7664 BD118.
  7665       MOVE 1              TO WS-SALKEY.
  7666       PERFORM AY60 THRU AY999.
  7667       PERFORM AY02 THRU AY59.
  7668       MOVE W70-INVOICE    TO W90-REF.
  7669       DISPLAY W90-REF AT 0510
  7670               WITH FOREGROUND-COLOR 6 HIGHLIGHT
  7671                    BACKGROUND-COLOR 0.
  7672     IF W70-INVOICE = 99999999
  7673         MOVE 1            TO W70-INVOICE
  7674     ELSE
  7675         ADD 1             TO W70-INVOICE.
  7676 BD145.
  7677*
  7678*    ****    P R I N T   C R - N O T E   H E A D I N G S
  7679*
  7680       MOVE SPACES         TO REP-DETAIL2.
  7681       MOVE "CREDIT NOTE"  TO INV-TYPE.
  7682       WRITE INV-L1 BEFORE 2 LINES.
  7683       MOVE SPACES         TO REP-DETAIL2.
  7684       MOVE W40-NAME       TO INV-NAME.
  7685       WRITE INV-L1 BEFORE 1 LINES.
  7686       MOVE W40-ADD1       TO INV-NAME.
  7687     IF W05-VATNO NOT = SPACES
  7688         MOVE W05-VATNO    TO INV-GNO.
  7689       WRITE INV-L1 BEFORE 1 LINES.
  7690       MOVE SPACES         TO REP-DETAIL2.
  7691       MOVE W40-ADD2       TO INV-NAME.
  7692       WRITE INV-L1 BEFORE 1 LINES.
  7693       MOVE W40-ADD3       TO INV-NAME.
  7694       WRITE INV-L1 BEFORE 1 LINES.
  7695       MOVE W40-ADD4       TO INV-NAME.
  7696       WRITE INV-L1 BEFORE 1 LINES.
  7697 BD146.
  7698*
  7699*    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
  7700*
  7701       ADD 1               TO WS-PAGE.
  7702       MOVE SPACES TO REP-DETAIL2.
  7703       MOVE W40-PC1        TO INV-PC1.
  7704       WRITE INV-L1 BEFORE 3 LINES.
  7705       MOVE SPACES         TO REP-DETAIL2.
  7706*
  7707*    ****    C L I E N T   V A T   N U M B E R
  7708*
  7709     IF NOT (W40-TAX = SPACES)
  7710         MOVE "Vat No"     TO INV-VATH
  7711         MOVE W40-TAX      TO INV-VATNO.
  7712       WRITE INV-L1 BEFORE 4 LINES.
  7713       MOVE SPACES         TO REP-DETAIL2.
  7714       MOVE W12-TODAY      TO INV-DTE.
  7715       MOVE W95-TIME       TO INV-TIME.
  7716     IF W10-SMAN NOT = ZERO
  7717         MOVE W40-ASSIST   TO INV-SMAN.
  7718       MOVE W90-REF        TO INV-REF.
  7719       MOVE W80-ACNO       TO INV-AC.
  7720       MOVE ALL "*"        TO INV-TERMS.
  7721       MOVE W90-ORD        TO INV-ORD.
  7722       WRITE INV-L1 BEFORE 6 LINES.
  7723       MOVE SPACES         TO REP-DETAIL2.
  7724       MOVE 23             TO WS-LINE.
  7725 BD150.
  7726       EXIT.
  7727 BD153.
  7728       MOVE WS-PAGE        TO DOC-PAGE.
  7729       MOVE W80-ACNO       TO DOC-AC.
  7730       MOVE 100            TO DOC-SQN.
  7731       MOVE ZERO           TO DOC-TYPE.
  7732       MOVE W40-NAME       TO DOC-NAME.
  7733       MOVE ZERO           TO DOC-PICNO.
  7734       MOVE WS-CR          TO DOC-DOC.
  7735       MOVE W90-EXP        TO DOC-EXP.
  7736       MOVE WS-PRC         TO DOC-PRC.
  7737       MOVE W90-ORD        TO DOC-ORD.
  7738       MOVE W90-REF        TO DOC-REF.
  7739       MOVE W90-REG        TO DOC-OREF.
  7740       MOVE W12-T-YMD      TO DOC-DTE.
  7741       MOVE W10-SMAN       TO DOC-SMAN.
  7742       MOVE W90-TOTAL      TO DOC-VALUE.
	     MOVE LS0-NO	 TO DOC-USER.
	     MOVE LS0-NAME	 TO DOC-UNME.
  7743*
  7744*    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
  7745*
  7746 BD154.
  7747       PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
  7748       MOVE 1              TO WS-ACTION.
  7749       PERFORM AY22 THRU AY59.
  7750*
  7751*    ****    R E A D   T H E   S A L E   F I L E   A N D
  7752*            P R I N T   T H E   I N V O I C E
  7753*
  7754 BD155.
  7755     IF WS-SALKEY > WS-LAST
  7756	       GO TO BD175.
  7757       PERFORM READ-SALE THRU READ-SALE-EXIT.
  7758     IF WS-LINE > 1
  7759         SUBTRACT 1        FROM WS-LINE
  7760     ELSE
  7761         PERFORM BD170.
  7762     IF SAL-EXT-ITEM = SPACES
  7763         GO TO BD167.
  7764       MOVE SAL-EXT-ITEM   TO STK-CODE.
  7765       PERFORM AY05 THRU AY59.
  7766*
  7767*    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
  7768*            R E C O R D
  7769*
  7770       MOVE W80-ACNO       TO CRD-AC.
  7771       MOVE ZERO           TO CRD-SEQ.
  7772       MOVE W10-SMAN       TO CRD-SMAN.
  7773       MOVE W90-REF        TO CRD-REF.
  7774       MOVE STK-SUPP       TO CRD-SUPP.
  7775       MOVE W12-T-YMD      TO CRD-DTE.
  7776       MOVE W40-NAME       TO CRD-NAME.
  7777       MOVE SPACES         TO CRD-WHS.
	     MOVE STK-LDG	 TO CRD-DEPT.
  7778       MOVE 1              TO DOC-TYPE.
  7779       INITIALIZE DOC-DET2.
  7780       MOVE SAL-EXT-ITEM   TO DOC-EXT-ITEM CRD-EXT-CODE.
  7781       MOVE SAL-DESC       TO DOC-DESC.
  7782       MOVE SAL-QNT        TO W10-QUANT DOC-QNT.
  7783*      MOVE SAL-CASES      TO W10-CASES.
  7784       MOVE SAL-PACK       TO WS-P1.
  7785*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  7786*    ³ To avoid confusion, the system logs details to the sales  ³
  7787*    ³ history file as single units and not as packs.            ³
  7788*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  7789       MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
  7790                           GIVING W10-QNT.
  7791       MULTIPLY W10-QNT BY -1
  7792                           GIVING CRD-QNT.
  7793       MOVE SAL-COST       TO W10-COST DOC-COST.
  7794       COMPUTE CRD-COST = (SAL-COST / STK-UNT(WS-P1)) * -1.
  7795       MOVE SAL-VAT        TO DOC-VAT.
  7796       MOVE SAL-SELL       TO W10-SELL  DOC-SELL.
  7797       MOVE SAL-VAL        TO W25-VALUE DOC-VAL.
  7798       MOVE SAL-EXT-ITEM   TO INV-ITM.
  7799       MOVE SAL-DESC       TO INV-DESC.
  7800       MOVE SAL-QNT        TO INV-QNT.
  7801       MOVE SAL-SELL       TO INV-PRICE.
  7802       MOVE SAL-VAL        TO INV-VAL.
  7803       MOVE SAL-DISC       TO W90-CDISC W90-EXCDIS.
  7804       MOVE SAL-DSC        TO WS-DISC INV-DSC.
  7805       ADD W90-CDISC W25-VALUE
  7806                           GIVING W90-VALUE.
  7807       MOVE SAL-TAXVAL     TO DOC-TAXVAL.
  7808       MOVE SAL-NONTAX     TO DOC-NONTAX.
  7809       MOVE SAL-CONVERT    TO DOC-CONVERT.
  7810       MOVE SAL-VAT-SUB    TO WS-VAT-SUB
  7811                              DOC-VAT-SUB.
  7812     IF WS-VAT-SUB = ZERO
  7813         MOVE ZERO         TO DOC-V-PER
  7814	   ELSE
  7815         MOVE W05-VAT(WS-VAT-SUB)
  7816                           TO DOC-V-PER.
  7817*      MOVE SAL-SERNO      TO W10-SERNO DOC-SERNO.
  7818       MOVE SAL-SUPER      TO WS-SUPER.
  7819*
  7820*    ****   I F   I N C L U S I V E   V A L U E  -
  7821*           C A L C U L A T E   E X C L U S I V E   V A L U E
  7822*
  7823     IF W90-TAX = SPACES
  7824       IF W90-EXP NOT = "X"
  7825         IF WS-VAT-SUB > ZERO
  7826             MOVE W05-VAT(WS-VAT-SUB)
  7827                           TO W05-RATE
  7828             COMPUTE W90-VALUE ROUNDED = W90-VALUE -
  7829                 (((W90-VALUE / (W05-RATE + 100.00000)) *
  7830                    W05-RATE))
  7831             COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
  7832                 (((W90-CDISC / (W05-RATE + 100.00000)) *
  7833                    W05-RATE)).
  7834       ADD W90-VALUE       TO W90-EXCTOT.
  7835*
  7836*    ****    C A L C U L A T E   E X C L U S I V E
  7837*            U N I T   S E L L I N G   P R I C E
  7838*
  7839       COMPUTE CRD-SELL ROUNDED = (W90-VALUE / SAL-QNT).
  7840     IF WS-CARDEX = "Y"
  7841         PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
  7842         MOVE 1            TO WS-ACTION
  7843         PERFORM AY43 THRU AY59.
  7844       COMPUTE W25-CREDIT = W90-VALUE * -1.
  7845       WRITE INV-L1 BEFORE 1 LINES.
  7846       MOVE SPACES         TO REP-DETAIL2.
  7847       ADD W25-CREDIT      TO W90-STOTAL.
  7848*
  7849*    ****    H A S   L I N E   D I S C O U N T   B E E N
  7850*            A P P L I E D
  7851*
  7852     IF W90-CDISC = ZERO
  7853         GO TO BD160.
  7854     IF WS-LINE > 1
  7855         SUBTRACT 1        FROM WS-LINE
  7856     ELSE
  7857         PERFORM BD170.
  7858*
  7859*    ****    L O G   T H E   L I N E   D I S C O U N T
  7860*
  7861       MOVE WS-DISC        TO DOC-DSC.
  7862       MOVE W90-CDISC      TO DOC-DISC.
  7863 BD160.
  7864 BD162.
  7865*
  7866*    ****    U P D A T E   S A L E S   A N A L Y S I S
  7867*
  7868       MOVE STK-LDG        TO DPT-CODE.
  7869       PERFORM AY38 THRU AY59.
  7870       SUBTRACT W90-VALUE  FROM DPT-RET-DAY
  7871                                DPT-RET-MTD
  7872				      DPT-RET-YTD.
  7873*
  7874*    ****    I N T E G R A T I O N   T O   T H E   G / L E D G E R
  7875*
  7876     IF WX-INT = "Y"
  7877         MOVE W12-T-YMD    TO WX-DATE
  7878         MOVE STK-LDG      TO WX-LEDG
  7879         MOVE "RETURNS - ACCOUNTS RECEIVABLE"
  7880                           TO WX-NAR
  7881         COMPUTE WX-VALUE = W90-VALUE * -1
  7882         MOVE DPT-SALE-GL  TO WX-AC
  7883         MOVE 05           TO WX-TYPE
  7884         PERFORM GL-TRF-UPDATE.
  7885*
  7886*    ****    U P D A T E   C O S T - O F - S A L E S
  7887*
  7888       COMPUTE W90-BDT ROUNDED = (1.000 * W10-QUANT * W10-COST).
  7889       ADD W90-BDT         TO DPT-CRET-DAY
  7890                              DPT-CRET-MTD
  7891                              DPT-CRET-YTD.
  7892*
  7893*    ****    I N T E G R A T I O N   T O   T H E   G / L E D G E R
  7894*
  7895     IF WX-INT = "Y"
  7896         IF W90-BDT NOT = ZERO
  7897             COMPUTE WX-VALUE = W90-BDT * -1
  7898             MOVE DPT-COST-GL
  7899                           TO WX-AC
  7900             MOVE 09       TO WX-TYPE
  7901             PERFORM GL-TRF-UPDATE
  7902             MOVE W90-BDT  TO WX-VALUE
  7903             MOVE DPT-PUR-GL
  7904                           TO WX-AC
  7905             MOVE 20       TO WX-TYPE
  7906             PERFORM GL-TRF-UPDATE.
  7907*
  7908*    ****    U P D A T E   D I S C O U N T   G I V E N
  7909*
  7910       SUBTRACT W90-EXCDIS FROM DPT-DISC-DAY
  7911                                DPT-DISC-MTD
  7912                                DPT-DISC-YTD.
  7913       ADD W90-EXCDIS      TO DPT-RET-DAY
  7914                              DPT-RET-MTD
  7915                              DPT-RET-YTD.
  7916       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  7917*
  7918*    ****    C H E C K   I F   S T O C K   C O N T R O L L E D
  7919*
  7920     IF (WS-PRC-IND = "Y" AND STK-IND = 1) OR
  7921        (STK-IND = 0)
  7922         ADD W10-QNT       TO STK-QUANT
  7923         IF STK-USE-CASES = "Y"
  7924             ADD SAL-CASES TO STK-CASES.
  7925       SUBTRACT W90-BDT    FROM W90-BCR.
  7926       SUBTRACT W10-QNT    FROM STK-YTD.
  7927       SUBTRACT W10-QNT    FROM STK-MTD.
  7928       ADD W90-VALUE       TO STK-YTDV.
  7929       SUBTRACT W90-BDT    FROM STK-MTDC.
  7930	     SUBTRACT W90-BDT	 FROM STK-YTDC.
  7931       ADD W90-VALUE       TO STK-MTDV.
  7932       ADD W90-VALUE       TO W90-SALES.
  7933       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
  7934*
  7935*    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
  7936*            A U D I T   F I L E
  7937*
  7938       MULTIPLY W90-BDT BY -1
  7939                           GIVING W90-ICST.
  7940       PERFORM BA100.
  7941       ADD 1               TO DOC-SEQ.
  7942       MOVE ZERO           TO DOC-SUB.
  7943       PERFORM BD154.
  7944*
  7945*    ****    G O   A N D   G E T   T H E   N E X T   I T E M
  7946*
  7947 BD165.
  7948       ADD 1               TO WS-SALKEY.
  7949       GO TO BD155.
  7950 BD167.
  7951       MOVE 3              TO DOC-TYPE.
  7952       INITIALIZE DOC-DET2.
  7953*
  7954*     C H E C K   F O R   S E R I A L   N U M B E R S
  7955*
  7956     IF SAL-SUB NOT = ZERO
  7957         MOVE 1            TO WS-S1
  7958         PERFORM BD168
  7959         MOVE SAL-MSER     TO INV-DESC DOC-MSER
  7960         ADD 1             TO DOC-SUB
  7961     ELSE
  7962         MOVE ZERO         TO DOC-SUB
  7963         ADD 1             TO DOC-SEQ
  7964         MOVE SAL-DESC     TO INV-RMKS DOC-DESC.
  7965       WRITE INV-L1 BEFORE 1 LINES.
  7966       MOVE SPACES         TO REP-DETAIL2.
  7967       PERFORM BD154.
  7968       GO TO BD165.
  7969 BD168.
  7970       MOVE SAL-SNO (WS-S1)
  7971                           TO W10-SERNO.
  7972       PERFORM BB070.
  7973       ADD 1               TO WS-S1.
  7974     IF WS-S1 < 3
  7975         IF SAL-SNO (WS-S1) NOT = SPACES
  7976             GO TO BD168.
  7977 BD170.
  7978*
  7979*    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
  7980*
  7981       WRITE INV-L1 BEFORE 1 LINES.
  7982       MOVE SPACES         TO REP-DETAIL2.
  7983       MOVE "C/FWD"        TO INV-FWD.
  7984       MOVE W90-STOTAL     TO INV-VAL.
  7985       WRITE INV-L1 BEFORE 14 LINES.
  7986       PERFORM BB050.
  7987       PERFORM BD145 THRU BD150.
  7988	     MOVE "B/FWD"	 TO INV-DESC.
  7989       MOVE W90-STOTAL     TO INV-VAL.
  7990       WRITE INV-L1 BEFORE 1 LINES.
  7991       MOVE SPACES         TO REP-DETAIL2.
  7992       SUBTRACT 2          FROM WS-LINE.
  7993 BD175.
  7994*
  7995*    ****    I N V O I C E   T O T A L S
  7996*
  7997       WRITE INV-L1 BEFORE WS-LINE LINES.
  7998       MOVE SPACES         TO REP-DETAIL2.
  7999       WRITE INV-L1 BEFORE 1 LINES.
  8000       MOVE 9              TO DOC-TYPE.
  8001       MOVE 99900          TO DOC-SQN.
  8002       INITIALIZE DOC-DET3.
  8003       MOVE W90-CR         TO DOC-SUBTOT.
  8004*
  8005*    ****    P R I N T   D I S C O U N T   A N D   U P D A T E
  8006*            T H E   S U N D R Y   D I S C O U N T   T O T A L S
  8007*
  8008     IF W90-TDISC NOT = ZERO
  8009         MOVE W90-TDISC    TO DOC-DSCNT
  8010         IF W90-LDISC = ZERO
  8011             MOVE "YYYY"   TO DPT-CODE
  8012             PERFORM AY38 THRU AY59
  8013             SUBTRACT W90-TDISC
  8014                           FROM DPT-DISC-DAY
  8015                                DPT-DISC-MTD
  8016                                DPT-DISC-YTD
  8017*
  8018*    ****    I N T E G R A T I O N   T O   T H E   G / L E D G E R
  8019*
  8020             IF WX-INT = "Y"
  8021                 COMPUTE WX-VALUE = W90-EXCDIS * -1
  8022                 MOVE DPT-DISC-GL  TO WX-AC
  8023                 MOVE DPT-CODE     TO WX-LEDG
  8024                 MOVE 16           TO WX-TYPE
  8025                 PERFORM GL-TRF-UPDATE
  8026             END-IF
  8027             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  8028*
  8029*    ****    P R I N T   T H E   T A X   F I G U R E S   A N D
  8030*            U P D A T E   T A X   T O T A L S
  8031*
  8032     IF (W90-TAX NOT = SPACES) OR
  8033        (W90-EXP = "X")
  8034         MOVE SPACES       TO INV-FWD
  8035     ELSE
  8036     IF W05-VATNO NOT = SPACES
  8037         MOVE "Vat inclusive"
  8038                           TO INV-FWD.
  8039*
  8040*    ****    I N C L U S I V E   T A X
  8041*
  8042*            TAX CALCULATED IN DTPCRN
  8043*                W90-NONTAX   =TOTAL NON TAXABLE VALUE
  8044*                W90-TTAX     =TOTAL TAX (ALL RATES)
  8045*                W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
  8046* 	       W90-VAT(?)   =TOTAL VAT PER RATE
  8047*                W90-TOTAL    =TOTAL FOR CREDIT NOTE
  8048*
  8049*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  8050*
  8051       MOVE W05-RATE       TO WS-PER.
  8052       MOVE W90-TTAX       TO DOC-TAX.
  8053     IF W90-TAX NOT = SPACES
  8054         MOVE W90-TTAX     TO INV-VAL.
  8055
  8056 BD180.
  8057       WRITE INV-L1 BEFORE 2 LINES.
  8058       MOVE SPACES         TO REP-DETAIL2.
  8059       MOVE ALL "*"        TO DOC-TERMS.
  8060*
  8061*    ****    C R E D I T   N O T E   T O T A L   ( PRINT & LOG )
  8062*
  8063       MOVE W90-TOTAL      TO W90-CR.
  8064       MOVE W90-BCR        TO W90-TCOST.
  8065       MOVE W90-CR         TO DOC-TOT .
  8066       MOVE W90-EXCTOT     TO DOC-TAXABLE.
  8067       MOVE W90-NONTAX     TO DOC-NON-TXBL.
  8068       MOVE W90-BAL        TO DOC-BAL.
  8069       MOVE ZERO           TO DOC-ADVTAX.
  8070       PERFORM BD154.
  8071       MOVE W90-CR         TO INV-VAL.
  8072       WRITE INV-L1 BEFORE 11 LINES.
  8073       MOVE SPACES         TO REP-DETAIL2.
  8074       MOVE W12-T-YMD      TO W20-DTE.
  8075*
  8076*    ****    U P D A T E   T H E   D E B T O R   A C C O U N T
  8077*
  8078       MOVE 4              TO W90-CODE.
  8079       PERFORM GET-DEBTOR-TRAN-RECORD.
  8080       PERFORM GENERATE-DEBTOR-TRAN.
  8081       MOVE 9              TO WS-PARKEY.
  8082       PERFORM AY01 THRU AY59.
  8083       MOVE 1              TO W05-V-RATE.
  8084
  8085 BD185-VAT.
  8086     IF W90-VAT(W05-V-RATE) = ZERO
  8087         GO TO BD185-CHECK-END.
  8088       MOVE W05-VAT-CODE   TO DPT-CODE.
  8089       PERFORM AY38 THRU AY59.
  8090       SUBTRACT W90-VAT(W05-V-RATE)
  8091                           FROM DPT-O-VRET-DAY
  8092                                DPT-O-VRET-MTD
  8093                                DPT-O-VRET-YTD
  8094       SUBTRACT W90-TAXVAL(W05-V-RATE)
  8095                           FROM DPT-V-RET-DAY
  8096                                DPT-V-RET-MTD
  8097                                DPT-V-RET-YTD.
  8098       ADD W90-TAXVAL(W05-V-RATE)
  8099                           TO PAR-CVAT-DAY
  8100                              PAR-CVAT-MTD
  8101                              PAR-CVAT-YTD.
  8102*
  8103*    ****    I N T E G R A T I O N   T O   T H E   G / L E D G E R
  8104*
  8105     IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
  8106          MOVE W90-VAT(W05-V-RATE)
  8107                           TO WX-VALUE
  8108*         COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
  8109          MOVE DPT-I-VAT-GL
  8110                           TO WX-AC
  8111          MOVE DPT-CODE    TO WX-LEDG
  8112          MOVE 11          TO WX-TYPE
  8113          PERFORM GL-TRF-UPDATE.
  8114       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  8115
  8116 BD185-CHECK-END.
  8117     IF W05-V-RATE < 6
  8118         ADD 1             TO W05-V-RATE
  8119         GO TO BD185-VAT.
  8120       ADD W90-NONTAX      TO PAR-CNGS-DAY
  8121                              PAR-CNGS-MTD
  8122                              PAR-CNGS-YTD.
  8123       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8124*
  8125*    ****    L O G   T H E   S A L E S M A N   &   C R   N O T E -
  8126*            A U D I T   F I L E   D E T A I L S
  8127*
  8128       PERFORM BA90.
  8129       PERFORM BA95.
  8130*
  8131*    ****    P R I N T   T H E   N E X T   I N V O I C E   /
  8132*            C R E D I T   N O T E   H E A D I N G
  8133*            ( OVERLAP WITH DISK PROCESSING )
  8134*
  8135       PERFORM BB050.
  8136*
  8137*    ****    U P D A T E   T H E   D E B T O R   C A T E G O R Y
  8138*
  8139       ADD 121 DEB-CAT     GIVING W90-RESULT.
  8140       DIVIDE W90-RESULT BY 2
  8141                           GIVING W90-CATKEY
  8142                           REMAINDER W90-CS.
  8143       ADD 1               TO W90-CS.
  8144       MOVE W90-CATKEY     TO WS-PARKEY.
  8145       PERFORM AY01 THRU AY59.
  8146       ADD W90-CR          TO PAR-CCRMTD (W90-CS)
  8147                              PAR-CCRYTD (W90-CS).
  8148       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8149*
  8150*    ****    U P D A T E   T H E   S A L E S M A N   R E C O R D
  8151*
  8152     IF W10-SMAN NOT = ZERO
  8153         ADD 100 W10-SMAN  GIVING WS-PARKEY
  8154         PERFORM AY01 THRU AY59
  8155         ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
  8156         ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
  8157         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8158*
  8159*    ****    R E W R I T E   T H E   D E B T O R   R E C O R D
  8160*
  8161       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
  8162*
  8163*    ****    U P D A T E   T H E   D E B T O R   C O N T R O L
  8164*            R E C O R D
  8165*
  8166       ADD W90-CR          TO W70-OUT.
  8167       ADD W90-CUR         TO W70-MTD.
  8168       ADD W90-30          TO W70-ONE.
  8169       ADD W90-60          TO W70-TWO.
  8170       ADD W90-90          TO W70-THREE.
  8171       ADD W90-120         TO W70-FOUR.
  8172       SUBTRACT W90-CR     FROM W70-CR.
  8173*
  8174*    ****    U P D A T E   T H E   S T O C K   C O N T R O L
  8175*            R E C O R D
  8176*
  8177       ADD W90-BAL         TO STK-TOTAL.
  8178       ADD W90-SALES       TO STK-SMTD STK-SYTD.
  8179*
  8180*    ****    U P D A T E   T H E   C O S T - O F - S A L E S
  8181*
  8182 BD190.
  8183       MOVE 8              TO WS-PARKEY.
  8184       PERFORM AY01 THRU AY59.
  8185       ADD W90-BCR         TO PAR-CSHT PAR-CMTD PAR-CYTD.
  8186       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8187*
  8188*    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
  8189*
  8190 BD195.
  8191       PERFORM AY70 THRU AY999.
  8192*
  8193*    ****    C L O S E   F I L E S
  8194*            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
  8195*
  8196       CLOSE PARAM
  8197             DEPART
  8198             DEBTOR
  8199             AUDIT
  8200             STOCK
  8201             INVOICE.
  8202       OPEN I-O PARAM
  8203                DEPART
  8204                DEBTOR
  8205                AUDIT
  8206                STOCK
  8207                INVOICE.
  8208       DISPLAY CLEAR-L25.
  8209     IF OPEN-ITEM
  8210         PERFORM BD270 THRU BD299.
  8211       GO TO BD999.
  8212
  8213 BD270.
  8214       DISPLAY CLR-SCREEN.
  8215       DISPLAY "CREDIT TRANSACTION ALLOCATION" AT 0226
  8216                WITH FOREGROUND-COLOR 15.
  8217       DISPLAY "Account :" AT 0302.
  8218       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
  8219       DISPLAY W80-ACNO AT 0312 WITH FOREGROUND-COLOR 11.
  8220	     DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR 11.
  8221       DISPLAY "Date" AT 0504
  8222               "Cde/description" AT 0511
  8223               "Ref. No." AT 0527
  8224               "Value" AT 0543.
  8225       PERFORM READ-DBTRAN THRU READ-DBTRAN-EXIT.
  8226       MOVE TRA-CODE       TO W90-CODE.
  8227       PERFORM GET-DEBTOR-TRAN-DESC.
  8228       MOVE TRA-DATE       TO W22-DTE2.
  8229       MOVE W22-CC2        TO W22-CC3.
  8230       MOVE W22-YY2        TO W22-YY3.
  8231       MOVE W22-MM2        TO W22-MM3.
  8232       MOVE W22-DD2        TO W22-DD3.
  8233       MOVE W22-DTE3       TO W90-DTE.
  8234       MOVE TRA-REF        TO W90-REF.
  8235       DISPLAY W90-DTE AT 0702 WITH FOREGROUND-COLOR 11.
  8236       DISPLAY W90-CODE AT 0711 WITH FOREGROUND-COLOR 11.
  8237       DISPLAY W00-E-DESC AT 0714 WITH FOREGROUND-COLOR 11.
  8238       DISPLAY TRA-REF AT 0727 WITH FOREGROUND-COLOR 11.
  8239       MOVE TRA-VALUE      TO W95-B.
  8240       DISPLAY W95-V2 AT 0736 WITH FOREGROUND-COLOR 11.
  8241       COMPUTE W90-CR = (TRA-VALUE + TRA-ALLOCATED) * -1.
  8242       MOVE TRA-RECORD1    TO W91-TRAN.
  8243       MOVE ZERO           TO W90-ALLOCATED.
  8244       DISPLAY S08A.
  8245       MOVE 19             TO CLIN.
  8246       PERFORM ALLOCATE-CREDIT.
  8247     IF (WS-ERROR = 9) OR
  8248        (W75-ALLOCATED = W91-ALLOCATED)
  8249         GO TO BD290.
  8250       PERFORM AY60 THRU AY999.
  8251       MOVE 0              TO WS-ACTION.
  8252       PERFORM AY02 THRU AY59.
  8253       MOVE ZERO           TO W90-90 W90-120 W90-CUR
  8254                              W90-60 W90-INT W90-30.
  8255
  8256 BD275.
  8257     IF W75-KEY < WS-DBTKEY
  8258         GO TO BD285.
  8259       PERFORM READ-DEBTRN THRU READ-DEBTRN-EXIT.
  8260     IF DBT-UPDATE = SPACE
  8261         GO TO BD280.
  8262       ADD DBT-ALLOC       TO DBT-PAID.
  8263     IF DBT-VALUE > DBT-PAID
  8264         MOVE "X"          TO DBT-TYPE
  8265     ELSE
  8266         MOVE "P"          TO DBT-TYPE.
  8267     IF DBT-CODE = 20
  8268         SUBTRACT DBT-ALLOC
  8269                           FROM DEB-INT
  8270         ADD DBT-ALLOC     TO W90-INT
  8271     ELSE
  8272     IF DBT-AGE = 5
  8273         SUBTRACT DBT-ALLOC
  8274                           FROM DEB-120
  8275         ADD DBT-ALLOC     TO W90-120
  8276     ELSE
  8277     IF DBT-AGE = 4
  8278	       SUBTRACT DBT-ALLOC
  8279                           FROM DEB-90
  8280         ADD DBT-ALLOC     TO W90-90
  8281     ELSE
  8282     IF DBT-AGE = 3
  8283         SUBTRACT DBT-ALLOC
  8284                           FROM DEB-60
  8285         ADD DBT-ALLOC     TO W90-60
  8286     ELSE
  8287     IF DBT-AGE = 2
  8288         SUBTRACT DBT-ALLOC
  8289                           FROM DEB-30
  8290         ADD DBT-ALLOC     TO W90-30
  8291     ELSE
  8292     IF DBT-AGE = 1
  8293         SUBTRACT DBT-ALLOC
  8294                           FROM DEB-CUR
  8295         ADD DBT-ALLOC     TO W90-CUR.
  8296       MOVE DBT-KEY        TO TRA-KEY.
  8297       PERFORM READ-DBTRAN THRU READ-DBTRAN-EXIT.
  8298       MOVE 0              TO WS-ACTION.
  8299       PERFORM AY04 THRU AY59.
  8300       MOVE DBT-TRA        TO TRA-RECORD1.
  8301       MOVE W90-REF        TO TRA-REC.
  8302       PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
  8303
  8304 BD280.
  8305       ADD 1               TO WS-DBTKEY.
  8306       GO TO BD275.
  8307
  8308 BD285.
  8309       MOVE W91-TRAN       TO TRA-RECORD1.
  8310       MOVE 0              TO WS-ACTION.
  8311       PERFORM AY04 THRU AY59.
  8312       ADD W75-ALLOCATED   TO TRA-ALLOCATED.
  8313       PERFORM REWRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
  8314       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
  8315       SUBTRACT W90-120    FROM W70-FOUR.
  8316       SUBTRACT W90-90     FROM W70-THREE.
  8317       SUBTRACT W90-60     FROM W70-TWO.
  8318       SUBTRACT W90-30     FROM W70-ONE.
  8319       SUBTRACT W90-CUR    FROM W70-MTD.
  8320       SUBTRACT W90-INT    FROM W70-INTEREST.
  8321       PERFORM AY70 THRU AY999.
  8322
  8323 BD290.
  8324       CLOSE DEBTRN.
  8325       DISPLAY CLEAR-L25.
  8326
  8327 BD299.
  8328       EXIT.
  8329
  8330 BD999.
  8331       EXIT.
  8332/
  8333 ZA000-INIT              SECTION 90.
  8334 ZA000-OPEN.
  8335       MOVE LS-COMPANY     TO WS-TOP-COMP.
  8336       MOVE LS-PARID       TO WS-PARID.
  8337       MOVE LS-L-OR-N      TO W02-L-OR-N.
  8338       MOVE WS-SYS-ID      TO W02-SYSID.
  8339       MOVE LS-TODAY-DDMMYY
  8340                           TO TODAY-DDMMYY W12-TODAY.
  8341       MOVE LS-USUB        TO WS-USUB.
  8342       MOVE LS-DRAW        TO WS-DRAW.
  8343       MOVE W90-AGE        TO WS-AGE.
  8344*
  8345*    ****    P A R A M E T E R   F I L E
  8346*
  8347 ZA00A.
  8348       MOVE "PARAM"        TO AFID-KEY.
  8349
  8350 ZA00-READ-APACFIDS.
  8351       READ APACFIDS WITH IGNORE LOCK
  8352         KEY IS AFID-KEY.
  8353     IF WS-STATUS = "00"
  8354         GO TO ZA00-READ-APACFIDS-EXIT.
  8355       STRING "Missing " DELIMITED SIZE
  8356               AFID-KEY DELIMITED BY " "
  8357               " file ID - Status " DELIMITED SIZE
  8358               WS-STATUS DELIMITED SIZE
  8359               INTO WS-ERR-MES.
  8360       PERFORM ERROR-LENGTH THRU ERROR-EXIT.
  8361       STOP RUN.
  8362
  8363 ZA00-READ-APACFIDS-EXIT.
  8364       EXIT.
  8365
  8366 ZA00A-CONTINUE.
  8367       MOVE AFID-PATH      TO W02-PARAM.
  8368       MOVE "AUDITF"       TO AFID-KEY.
  8369       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8370       MOVE AFID-PATH      TO W02-AUDITF.
  8371       MOVE "CARDEX"       TO AFID-KEY.
  8372       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8373       MOVE AFID-PATH      TO W02-CARDEX.
  8374       MOVE "DBTRAN"       TO AFID-KEY.
  8375       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8376       MOVE AFID-PATH      TO W02-DBTRAN.
  8377       MOVE "DEBTOR"       TO AFID-KEY.
  8378       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8379       MOVE AFID-PATH      TO W02-DEBTOR.
  8380       MOVE "DEBTRN"       TO AFID-KEY.
  8381       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8382       INSPECT AFID-PATH REPLACING FIRST "XXX"
  8383                         BY LS-USER.
  8384       MOVE AFID-PATH      TO W02-DEBTRN.
  8385       MOVE "DEPART"       TO AFID-KEY.
  8386       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8387       MOVE AFID-PATH      TO W02-DEPART.
  8388       MOVE "GARTEE"       TO AFID-KEY.
  8389       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8390	     MOVE AFID-PATH	 TO W02-GARTEE.
  8391       MOVE "INVOIC"       TO AFID-KEY.
  8392       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8393       MOVE AFID-PATH      TO W02-INVOIC.
  8394       MOVE "LEDTRF"       TO AFID-KEY.
  8395       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8396       MOVE AFID-PATH      TO W02-LEDTRF.
  8397       MOVE "NETWORK"      TO AFID-KEY.
  8398       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8399       MOVE AFID-PATH      TO W02-NETWORK.
  8400       MOVE "RECOVER"      TO AFID-KEY.
  8401       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8402       INSPECT AFID-PATH REPLACING FIRST "XXX"
  8403                         BY LS-USER.
  8404       MOVE AFID-PATH      TO W02-RECOVER.
  8405       MOVE "SALE"         TO AFID-KEY.
  8406       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8407       INSPECT AFID-PATH REPLACING FIRST "XXX"
  8408                         BY LS-USER.
  8409       MOVE AFID-PATH      TO W02-SALE.
  8410       MOVE "SHARED"       TO AFID-KEY.
  8411       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8412       MOVE AFID-PATH      TO W02-SHARED.
  8413       MOVE "STOCKF"       TO AFID-KEY.
  8414       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8415       MOVE AFID-PATH      TO W02-STOCKF.
  8416       MOVE "TXTRAN"       TO AFID-KEY.
  8417       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8418       MOVE AFID-PATH      TO W02-TXTRAN.
  8419       MOVE 3              TO WS-PARKEY.
  8420       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8421       MOVE PAR-VAT-REG    TO W05-VATNO.
  8422       MOVE PAR-CRDX       TO WS-CARDEX.
  8423       MOVE PAR-PRICED-IND TO WS-PRC-IND.
  8424     IF LS-USER NUMERIC
  8425         IF LS-USE > ZERO AND < 110
  8426             MOVE LS-USE   TO WS-USUB
  8427             GO TO ZA01A.
  8428       MOVE 110            TO WS-USUB.
  8429
  8430 ZA01A.
  8431       ADD 2549 WS-USUB    GIVING W25-RESULT.
  8432       MOVE W25-KEY        TO WS-PARKEY.
  8433       ADD 1 W25-SUB       GIVING WS-S6.
  8434       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8435     IF TRM-INVP (WS-S6) = 1
  8436         MOVE "LPT1"       TO W02-PRINTER
  8437         MOVE ZERO         TO WS-PRN-NO
  8438     ELSE
  8439     IF TRM-INVP (WS-S6) = 2
  8440         MOVE "LPT2"       TO W02-PRINTER
  8441         MOVE 1            TO WS-PRN-NO
  8442     ELSE
  8443     IF TRM-INVP (WS-S6) = 3
  8444         MOVE "LPT3"       TO W02-PRINTER
  8445         MOVE 2            TO WS-PRN-NO
  8446     ELSE
  8447     IF TRM-INVP (WS-S6) = 4
  8448	       MOVE "COM1"	 TO W02-PRINTER
  8449     ELSE
  8450     IF TRM-INVP (WS-S6) = 5
  8451         MOVE "COM2"       TO W02-PRINTER
  8452     ELSE
  8453     IF TRM-INVP (WS-S6) = 9
  8454         MOVE LS-USER      TO W02-USER.
  8455       ADD 501 TRM-PRN2 (WS-S6)
  8456                           GIVING W25-RESULT.
  8457       DIVIDE W25-RESULT BY 2
  8458                           GIVING WS-PARKEY
  8459                           REMAINDER WS-S6.
  8460       ADD 1               TO WS-S6.
  8461       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8462       MOVE TRM-CONP (WS-S6)
  8463                           TO WS-COND.
  8464       MOVE TRM-NORP (WS-S6)
  8465                           TO WS-NORM.
  8466       MOVE TRM-EXPP (WS-S6)
  8467                           TO WS-EXPP.
  8468       MOVE TRM-ECAN (WS-S6)
  8469                           TO WS-ECAN.
  8470       MOVE TRM-8LPI (WS-S6)
  8471                           TO WS-8LPI.
  8472       MOVE TRM-6LPI (WS-S6)
  8473                           TO WS-6LPI.
  8474       MOVE TRM-10CPI (WS-S6)
  8475                           TO WS-10CPI.
  8476       MOVE TRM-12CPI (WS-S6)
  8477                           TO WS-12CPI.
  8478       MOVE TRM-17CPI (WS-S6)
  8479                           TO WS-17CPI.
  8480 ZA01B.
  8481       OPEN OUTPUT RECOVER.
  8482       CLOSE RECOVER.
  8483       OPEN I-O RECOVER.
  8484     IF WS-PRN-NO < 3
  8485         PERFORM ZA210 THRU ZA215
  8486         IF WS-ERROR = 1
  8487             GO TO ZA205
  8488     ELSE
  8489         DISPLAY "Waiting for printer response" AT 2525
  8490                  WITH BACKGROUND-COLOR 3
  8491                       FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
  8492       OPEN OUTPUT PRNREP.
  8493       CALL X"91" USING X91-RES X91-FUN PRNREP.
  8494       DISPLAY SPACES AT 2525
  8495               WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
  8496       MOVE WS-12CPI       TO REP-DETAIL2.
  8497       WRITE REP-LINE2 BEFORE 0 LINES.
  8498       MOVE WS-8LPI        TO REP-DETAIL2.
  8499       WRITE REP-LINE2 BEFORE 0 LINES.
  8500       MOVE SPACES         TO REP-DETAIL2.
  8501       MOVE 1              TO WS-PARKEY.
  8502       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8503       MOVE PAR-DMY        TO W12-TODAY.
  8504       MOVE PAR-YMD        TO W12-T-YMD.
  8505       MOVE PAR-COMPANY    TO W95-COMP.
  8506	     MOVE PAR-TELEPHONE  TO W95-TEL.
  8507     IF PAR-STMINV = "I" OR "X"
  8508         MOVE "N"          TO WS-HEAD
  8509     ELSE
  8510         MOVE "Y"          TO WS-HEAD.
  8511       ADD 1               TO WS-PARKEY.
  8512       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8513       MOVE PAR-ADDRESS1   TO W95-ADD1.
  8514       MOVE PAR-ADDRESS2   TO W95-ADD2.
  8515       MOVE PAR-ADDRESS3   TO W95-ADD3.
  8516       MOVE PAR-ADDRESS4   TO W95-ADD4.
  8517       MOVE PAR-POST-CDE   TO W95-POST.
  8518     IF W95-ADD4 = SPACES
  8519         MOVE W95-PC       TO W95-ADD4
  8520         MOVE SPACES       TO W95-PC.
  8521     IF W95-ADD3 = SPACES
  8522         MOVE W95-ADD4     TO W95-ADD3
  8523         MOVE SPACES       TO W95-ADD4.
  8524     IF W95-ADD2 = SPACES
  8525         MOVE W95-ADD3     TO W95-ADD2
  8526         MOVE SPACES       TO W95-ADD3.
  8527     IF W95-ADD1 = SPACES
  8528         MOVE W95-ADD2     TO W95-ADD1
  8529         MOVE SPACES       TO W95-ADD2.
  8530       MOVE 5              TO WS-PARKEY.
  8531       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8532       MOVE PAR-DISCOUNT   TO W15-DISCOUNT.
  8533       MOVE 1              TO W05-V-RATE.
  8534
  8535 ZA05-VAT.
  8536       MOVE W05-VAT-CODE   TO DPT-CODE.
  8537       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
  8538       MOVE DPT-R-DATE     TO WS-VAT-DATE(W05-V-RATE).
  8539       MOVE DPT-RATE       TO W05-VAT(W05-V-RATE).
  8540       ADD 6 W05-V-RATE    GIVING WS-S1.
  8541       MOVE DPT-P-RATE     TO W05-VAT(WS-S1).
  8542     IF W05-V-RATE < 6
  8543         ADD 1             TO W05-V-RATE
  8544         GO TO ZA05-VAT.
  8545       MOVE 6              TO WS-PARKEY.
  8546       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8547     IF PAR-INTEGRATE NOT = "Y"
  8548         MOVE "N"          TO WX-INT
  8549     ELSE
  8550         MOVE "Y"          TO WX-INT.
  8551       MOVE PAR-DEBGL      TO WX-DEBGL.
  8552       MOVE PAR-CREGL      TO WX-CREGL.
  8553       MOVE PAR-INTGL      TO WX-INTGL.
  8554       MOVE PAR-BNKGL      TO WX-BNKGL.
  8555       MOVE PAR-UNPROF     TO WX-UNPROF.
  8556       MOVE PAR-REDGL      TO WX-REDGL.
  8557       MOVE PAR-ADJGL      TO WX-ADJGL.
  8558       MOVE PAR-RLGL       TO WX-RLGL.
  8559       MOVE PAR-DSGL       TO WX-DSGL.
  8560       ACCEPT W25-TIME FROM TIME.
  8561       MOVE W25-HH         TO W95-HH.
  8562       MOVE W25-MM         TO W95-MM.
  8563       MOVE W25-SS         TO W95-SS.
  8564	     GO TO ZA999.
  8565
  8566 ZA205.
  8567       EXIT PROGRAM.
  8568
  8569 ZA210.
  8570       MOVE ZERO           TO WS-ERROR
  8571                              WS-S1 WS-S2 WS-S3 WS-S4
  8572                              WS-S5 WS-S6 WS-S7 WS-S8.
  8573       CALL "PC_TEST_PRINTER" USING WS-PRN-NO
  8574                                    WS-PRN-STAT
  8575                              RETURNING WS-STATUS.
  8576     IF WS-PRN-STAT > 127
  8577         MOVE 1            TO WS-S1
  8578         SUBTRACT 128      FROM WS-PRN-STAT.
  8579     IF WS-PRN-STAT > 63
  8580         MOVE 1            TO WS-S2
  8581         SUBTRACT 64       FROM WS-PRN-STAT.
  8582     IF WS-PRN-STAT > 31
  8583         MOVE 1            TO WS-S3
  8584         SUBTRACT 32       FROM WS-PRN-STAT.
  8585     IF WS-PRN-STAT > 15
  8586         MOVE 1            TO WS-S4
  8587         SUBTRACT 16       FROM WS-PRN-STAT.
  8588     IF WS-PRN-STAT > 7
  8589         MOVE 1            TO WS-S5
  8590         SUBTRACT 8        FROM WS-PRN-STAT.
  8591     IF WS-PRN-STAT > 3
  8592         MOVE 1            TO WS-S6
  8593         SUBTRACT 4        FROM WS-PRN-STAT.
  8594     IF WS-PRN-STAT > 1
  8595         MOVE 1            TO WS-S7
  8596         SUBTRACT 2        FROM WS-PRN-STAT.
  8597       MOVE WS-PRN-STAT    TO WS-S8.
  8598     IF WS-S3 = 0 AND
  8599        WS-S5 = 0 AND
  8600        WS-S8 = 0
  8601         GO TO ZA215.
  8602       DISPLAY "Check printer - " AT 2304
  8603                WITH FOREGROUND-COLOR 6 HIGHLIGHT
  8604               "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
  8605               "etry - "
  8606               "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
  8607               "bort".
  8608 ZA212.
  8609       ACCEPT WS-OPTION AT 2334
  8610              WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
  8611       CALL "CBL_TOUPPER" USING WS-OPTION
  8612                          BY VALUE WS-LENGTH
  8613                          RETURNING WS-STATUS.
  8614       IF NOT (WS-OPTION = "A" OR "R")
  8615           GO TO ZA212.
  8616       IF WS-OPTION = "A"
  8617           MOVE 1          TO WS-ERROR
  8618           GO TO ZA215.
  8619         GO TO ZA210.
  8620
  8621 ZA215.
  8622	     EXIT.
  8623
  8624 ZA999.
  8625       EXIT.
  8626
  8627 I-O-ERRORS      SECTION  91.
  8628 OPEN-ERROR.
  8629       DISPLAY CLR-SCREEN.
  8630       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
  8631       PERFORM DISPLAY-FILE-NAME.
  8632
  8633 READ-ERROR.
  8634       DISPLAY CLR-SCREEN.
  8635       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
  8636       PERFORM DISPLAY-FILE-NAME.
  8637
  8638 WRITE-ERROR.
  8639       DISPLAY CLR-SCREEN.
  8640       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
  8641       PERFORM DISPLAY-FILE-NAME.
  8642
  8643
  8644 DISPLAY-FILE-NAME.
  8645     IF WS-F-ERROR = 1
  8646         MOVE W02-AUDITF TO WS-FILE
  8647         MOVE WS-AUDKEY TO WS-KEY
  8648     ELSE
  8649     IF WS-F-ERROR = 2
  8650         MOVE W02-NETWORK TO WS-FILE
  8651         MOVE WS-NETKEY TO WS-KEY
  8652     ELSE
  8653     IF WS-F-ERROR = 5
  8654         MOVE W02-DBTRAN TO WS-FILE
  8655         MOVE ZERO         TO WS-KEY
  8656         MOVE TRA-KEY      TO WS-KEYX
  8657     ELSE
  8658     IF WS-F-ERROR = 6
  8659         MOVE W02-DEBTOR TO WS-FILE
  8660         MOVE ZERO         TO WS-KEY
  8661         MOVE DEB-ACNO     TO WS-KEYX
  8662     ELSE
  8663     IF WS-F-ERROR = 7
  8664         MOVE W02-DEPART   TO WS-FILE
  8665         MOVE ZERO         TO WS-KEY
  8666         MOVE DPT-CODE     TO WS-KEYX
  8667     ELSE
  8668     IF WS-F-ERROR = 9
  8669         MOVE W02-GARTEE   TO WS-FILE
  8670         MOVE ZERO         TO WS-KEY
  8671         MOVE GUA-KEY      TO WS-KEYX
  8672     ELSE
  8673     IF WS-F-ERROR = 12
  8674         MOVE W02-INVOIC   TO WS-FILE
  8675         MOVE ZERO         TO WS-KEY
  8676         MOVE DOC-KEY      TO WS-KEYX
  8677     ELSE
  8678     IF WS-F-ERROR = 15
  8679         MOVE WS-PARID     TO WS-FILE
  8680	       MOVE WS-PARKEY	 TO WS-KEY
  8681     ELSE
  8682     IF WS-F-ERROR = 18
  8683         MOVE W02-RECOVER  TO WS-FILE
  8684         MOVE WS-RECKEY    TO WS-KEY
  8685     ELSE
  8686     IF WS-F-ERROR = 19
  8687         MOVE W02-SALE TO WS-FILE
  8688         MOVE WS-SALKEY TO WS-KEY
  8689     ELSE
  8690     IF WS-F-ERROR = 22
  8691         MOVE W02-STOCKF   TO WS-FILE
  8692         MOVE ZERO         TO WS-KEY
  8693         MOVE STK-CODE     TO WS-KEYX
  8694     ELSE
  8695     IF WS-F-ERROR = 31
  8696         MOVE W02-DEBTRN   TO WS-FILE
  8697         MOVE WS-DBTKEY    TO WS-KEY
  8698     ELSE
  8699     IF WS-F-ERROR = 32
  8700         MOVE W02-TXTRAN   TO WS-FILE
  8701         MOVE ZERO         TO WS-KEY
  8702         MOVE TAX-KEY      TO WS-KEYX
  8703     ELSE
  8704	   IF WS-F-ERROR = 37
  8705         MOVE W02-SHARED   TO WS-FILE
  8706         MOVE WS-SHARED    TO WS-KEY
  8707     ELSE
  8708     IF WS-F-ERROR = 40
  8709         MOVE W02-LEDTRF   TO WS-FILE
  8710         MOVE ZERO         TO WS-KEY
  8711         MOVE XFR-KEY      TO WS-KEYX
  8712     ELSE
  8713     IF WS-F-ERROR = 43
  8714         MOVE W02-CARDEX   TO WS-FILE
  8715         MOVE ZERO         TO WS-KEY
  8716         MOVE CRD-KEY      TO WS-KEYX.
  8717     IF WS-STATUS = "10"
  8718         MOVE "End of FILE" TO WS-STAT-MESSAGE
  8719     ELSE
  8720     IF WS-STATUS = "22"
  8721         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
  8722     ELSE
  8723     IF WS-STATUS = "23"
  8724         MOVE "Invalid record number" TO WS-STAT-MESSAGE
  8725     ELSE
  8726     IF WS-STATUS = "24"
  8727         MOVE "DISK full" TO WS-STAT-MESSAGE
  8728     ELSE
  8729     IF WS-STATUS = "30"
  8730         MOVE "DISK error" TO WS-STAT-MESSAGE
  8731     ELSE
  8732     IF WS-STATUS = "94"
  8733         MOVE "FILE locked" TO WS-STAT-MESSAGE.
  8734       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
  8735       DISPLAY "Status " AT 1212
  8736                WS-STATUS WITH FOREGROUND-COLOR 11
  8737               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
  8738	   IF WS-STATUS NOT = "94"
  8739         DISPLAY "Key    " AT 1412
  8740                  WS-KEY WITH FOREGROUND-COLOR 11
  8741         DISPLAY "Reverse backup or contact program Support"
  8742                  AT 1612.
  8743         DISPLAY "Please make a note of these details" AT 1812.
  8744       STOP RUN.
