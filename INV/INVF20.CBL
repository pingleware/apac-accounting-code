     1$set LINKCOUNT"772"
     2******************************************************************
     3*                                                                *
     4*   ********  **     **  **    **  ********   ******     ****    *
     5*      **     ***    **  **    **  **        **    **   **  **   *
     6*      **     ** *   **  **    **  **             **   **    **  *
     7*      **     **  *  **  **    **  *****        **     **    **  *
     8*      **     **   * **   **  **   **         **       **    **  *
     9*      **     **    ***    ****    **        **         **  **   *
    10*   ********  **     **     **     **        ********    ****    *
    11*                                                                *
    12*     ENGLISH                                                    *
    13*                                                                *
    14*               I N V O I C I N G   P R O G R A M                *
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
    20 PROGRAM-ID.     INVF20.
    21 AUTHOR.         James William Lemmon.
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
   162 COPY DEPART.SL.
   173
   174 COPY GARTEE.SL.
   190
   191 COPY INVOIC.SL.
   207
   208 COPY LEDTRF.SL.
   217
   218 COPY PARAM.SL.

   228 COPY RECOVER.SL.
   234
   235 COPY SALE.SL.
   242
   243 COPY SPARTS.SL.
   254
   255 COPY SHARED.SL.
   262
   263 COPY STOCK.SL.
   282
   283 COPY TXTRAN.SL.
   293
   294 COPY WSTOCK.SL.
   306
   307     SELECT PRNREP  ASSIGN W02-PRINTER
   308                    ORGANIZATION LINE SEQUENTIAL.
   309
   310/
   311 DATA DIVISION.
   312 FILE SECTION.
   313
   314 COPY APACFIDS.FDE.
   328
   329 COPY AUDIT.FDE.
   413
   414 COPY CARDEX.FDE.
   440
   441 COPY CONTROL.FDE.
   569
   570 COPY DBTRAN.FDE.
   603
   604 COPY DEBTOR.FDE.
   696
   697 COPY DEPART.FDE.
   853
   854 COPY GARTEE.FDE.
   874
   875 COPY INVOIC.FDE.
   986
   987 COPY LEDTRF.FDE.
  1049
  1050 COPY PARAM.FDE.
  1791
  1792 COPY RECOVER.GFD.
  2057
  2058 COPY SALE.FDE.
  2151
  2152 COPY SPARTS.FDE.
  2170
  2171 COPY SHARED.FDE.
  2201
  2202 COPY STOCK.FDE.
  2358
  2359 COPY TXTRAN.FDE.
  2400
  2401 COPY WSTOCK.FDE.
  2420
  2421 FD  PRNREP    LABEL RECORD OMITTED
  2422               LINAGE WS-PGE-LENGTH.
  2423 01  REP-LINE1.
  2424     03  REP-DETAIL1     PIC  X(80).
  2425
  2426 01  REP-L1.
  2427     03  FILLER          PIC  X(10).
  2428     03  BAL-REP         PIC  X(60).
  2429     03  FILLER          PIC  X(10).
  2430
  2431 01  REP-LINE2.
  2432     03  REP-DETAIL2     PIC  X(96).
  2433
  2434 01  INV-L1.
  2435     03  FILLER          PIC  X(01).
  2436     03  INV-NADD        PIC  X(48).
  2437     03  INV-RADD      REDEFINES INV-NADD.
  2438         05  INV-PC      PIC  X(04).
  2439         05  FILLER      PIC  X(44).
  2440     03  FILLER          PIC  X(14).
  2441     03  INV-PAGE        PIC  X(06).
  2442     03  INV-P-NO        PIC  Z(03)9.
  2443     03  FILLER          PIC  X(23).
  2444
  2445 01  INV-L1A.
  2446     03  FILLER          PIC  X(01).
  2447     03  INV-NAME        PIC  X(48).
  2448     03  INV-DADD      REDEFINES INV-NAME.
  2449         05  INV-VATH.
  2450           07  INV-PC1   PIC  X(04).
  2451           07  FILLER    PIC  X(03).
  2452         05  INV-VATNO.
  2453           07  FILLER    PIC  X(41).
  2454     03  FILLER          PIC  X(13).
  2455     03  INV-GNO         PIC  X(14).
  2456     03  FILLER          PIC  X(20).
  2457
  2458 01  INV-L2.
  2459     03  FILLER          PIC X(64).
  2460	   03  INV-CREDIT.
  2461         05  FILLER      PIC X(06).
  2462         05  INV-TYPE    PIC X(20).
  2463         05  FILLER      PIC X(06).
  2464     03  INV-REPRINT REDEFINES INV-CREDIT.
  2465         05  FILLER      PIC X(06).
  2466         05  INV-EXPAND  PIC X(06).
  2467         05  INV-COPY    PIC X(12).
  2468         05  INV-CANEX   PIC X(06).
  2469         05  FILLER      PIC X(02).
  2470
  2471 01  INV-L3.
  2472     03  FILLER          PIC  X(01).
  2473     03  INV-DTE         PIC  99/99/9999.
  2474     03  FILLER          PIC  X(01).
  2475     03  INV-TIME        PIC  X(08).
  2476     03  FILLER          PIC  X(01).
  2477     03  INV-SMAN        PIC  X(19).
  2478     03  FILLER          PIC  X(01).
  2479     03  INV-REF         PIC  X(08).
  2480     03  FILLER          PIC  X(08).
  2481     03  INV-AC          PIC  X(06).
  2482     03  FILLER          PIC  X(06).
  2483     03  INV-TERMS       PIC  X(04).
  2484     03  FILLER          PIC  X(07).
  2485     03  INV-ORD         PIC  X(12).
  2486     03  FILLER          PIC  X(04).
  2487
  2488 01  INV-L4.
  2489     03  FILLER          PIC  X(01).
  2490     03  INV-ITM         PIC  X(11).
  2491     03  FILLER          PIC  X(01).
  2492     03  INV-MES.
  2493         05  FILLER      PIC  X(01).
  2494         05  INV-DESC.
  2495             07  FILLER  PIC  X(20).
  2496             07  INV-FWD PIC  X(22).
  2497     03  INV-REMKS REDEFINES INV-MES.
  2498         05  FILLER      PIC  X(01).
  2499         05  INV-RMKS    PIC  X(43).
  2500     03  FILLER          PIC  X(01).
  2501     03  INV-QNT         PIC  Z(04)9.99.
  2502     03  FILLER          PIC  X(01).
  2503     03  INV-PRICE       PIC  Z(03)9.99-.
  2504     03  FILLER          PIC  X(01).
  2505     03  INV-DSC         PIC  Z9.99 BLANK WHEN ZERO.
  2506     03  FILLER          PIC  X(02).
  2507     03  INV-VAL         PIC  Z(06)9.99-.
  2508     03  FILLER          PIC  X(02).
  2509
  2510 01  INV-L5.
  2511     03  FILLER          PIC  X(13).
  2512     03  INV-HD          PIC  X(10).
  2513     03  INV-SERNOS.
  2514         05  FILLER      PIC  X(01).
  2515         05  INV-SERNO   PIC  X(10).
  2516         05  FILLER      PIC  X(23).
  2517     03  FILLER          PIC  X(39).
  2518
  2519/
  2520 WORKING-STORAGE SECTION.
  2521 77  WS-CHECK        PIC  X(18)    VALUE
  2522                     "aeWlimemnomLalismJ".
  2523 77  WS-DISC         PIC  99V99    COMP-3.
  2524 77  WS-TDISC        PIC  99V99    COMP-3.
  2525 77  WS-CASH-AC      PIC  X(06).
  2526 77  WS-SUB          PIC  9(04)    COMP-5.
  2527 77  WS-SUPER        PIC  9(02)    COMP-5.
  2528 77  WS-P1           PIC  9(04)    COMP-5.
  2529 77  WS-S1           PIC  9(04)    COMP-5.
  2530 77  WS-S2           PIC  9(04)    COMP-5.
  2531 77  WS-S3           PIC  9(04)    COMP-5.
  2532 77  WS-S4           PIC  9(04)    COMP-5.
  2533 77  WS-S5           PIC  9(04)    COMP-5.
  2534 77  WS-S6           PIC  9(04)    COMP-5.
  2535 77  WS-S7           PIC  9(04)    COMP-5.
  2536 77  WS-S8           PIC  9(04)    COMP-5.
  2537 77  WS-S9           PIC  9(04)    COMP-5.
  2538 77  WS-IXS          PIC  9(04)    COMP-5.
  2539 77  WS-IXS1         PIC  9(04)    COMP-5.
  2540 77  WS-IXS2         PIC  9(04)    COMP-5.
  2541 77  WS-IXS3         PIC  9(04)    COMP-5.
  2542 77  WS-IXS4         PIC  9(04)    COMP-5.
  2543 77  WS-PARKEY       PIC  9(04)    COMP-5.
  2544 77  WS-ENQPOS       PIC  9(06)    COMP-5.
  2545 77  WS-ENQEND       PIC  9(06)    COMP-5.
  2546 77  WS-KEYSTR       PIC  9(06)    COMP-5.
  2547 77  WS-SALSTR       PIC  9(04)    COMP-5.
  2548 77  WS-SALKEY       PIC  9(04)    COMP-5.
  2549 77  WS-NETKEY       PIC  9(04)    COMP-5.
  2550 77  WS-AUDKEY       PIC  9(06)    COMP-5.
  2551 77  WS-RECKEY       PIC  9(06)    COMP-5.
  2552 77  WS-RECOVER      PIC  9(06)    COMP-5.
  2553 77  WS-SHARED       PIC  9(04)    COMP-5 VALUE 1.
  2554 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
  2555 77  WS-STOCK        PIC  X(01).
  2556 77  WS-CARDEX       PIC  X(01).
  2557 77  WS-USE-ITM      PIC  X(01).
  2558 77  WS-EXT-STK      PIC  X(01).
  2559 77  WS-PRC-IND      PIC  X(01).
  2560 77  WS-GAR          PIC  9(01) VALUE 0.
  2561 77  WS-NUM          PIC  Z9.
  2562 77  WS-START        PIC  9(08).
  2563 77  WS-END          PIC  9(08).
  2564 77  WS-TME2         PIC  9(08).
  2565 77  WS-PAR          PIC  9(01) VALUE 0.
  2566 77  WS-INO          PIC  9(02) VALUE 0.
  2567 77  WS-PGE-LENGTH   PIC  9(02) VALUE 68.
  2568 77  WS-LINES        PIC  9(02) VALUE 0.
  2569 77  WS-FIRST        PIC  9(03) VALUE 0.
  2570 77  WS-LAST         PIC  9(03) VALUE 0.
  2571 77  WS-LASTINV      PIC  9(04)    COMP-5.
  2572 77  WS-LINE         PIC  9(04)    COMP-5.
  2573 77  WS-PAGE         PIC  9(02)    COMP-5 VALUE ZERO.
  2574 77  WS-L1           PIC  9(04)    COMP-5.
  2575 77  WS-COND         PIC  X(06).
  2576 77  WS-NORM         PIC  X(06).
  2577 77  WS-EXPP	   PIC	X(06).
  2578 77  WS-ECAN         PIC  X(06).
  2579 77  WS-8LPI         PIC  X(06).
  2580 77  WS-6LPI         PIC  X(06).
  2581 77  WS-10CPI        PIC  X(06).
  2582 77  WS-12CPI        PIC  X(06).
  2583 77  WS-17CPI        PIC  X(06).
  2584 77  WS-OPTION       PIC  X(01).
  2585 77  WS-SKIP         PIC  X(01) VALUE " ".
  2586 77  WS-LOOK         PIC  X(01) VALUE "S".
  2587 77  WS-ETYPE        PIC  X(01) VALUE "I".
  2588 77  WS-ERROR        PIC  9(01).
  2589 77  WS-COMP         PIC  9(01).
  2590 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
  2591 77  WS-CONREC       PIC  9(01) VALUE ZERO.
  2592 77  WS-HEAD         PIC  X(01).
  2593 77  WS-TYPE         PIC  X(01).
  2594 77  WS-AGE          PIC  X(01).
  2595 77  WS-PRC          PIC  X(01).
  2596 77  WS-DISCOUNT     PIC  X(01) VALUE "N".
  2597 77  WS-INPUT        PIC  X(01) VALUE "N".
  2598 77  WS-COPY         PIC  X(01) VALUE "N".
  2599 77  WS-NOSELL       PIC  X(01) VALUE "N".
  2600 77  WS-PRNINV       PIC  X(01) VALUE "Y".
  2601 77  WS-BATCH        PIC  9(05).
  2602 77  WS-PER          PIC  Z(02)9.99.
  2603 77  WS-CR           PIC  X(01).
  2604 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
  2605 77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
  2606 77  TODAY-DDMMYY    PIC  9(08) COMP-5.
  2607 77  WS-USUB         PIC  9(04) COMP-5.
  2608 77  WS-DRAW         PIC  9(02).
  2609/
  2610 01  WS-DB-LINE.
  2611     03  WS-TOP-LNE2.
  2612         05  WS-TCR  PIC X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  2613-          "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
  2614     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
  2615         05  FILLER      PIC  X(01).
  2616         05  WS-TOP-COMP PIC  X(40).
  2617         05  FILLER      PIC  X(39).
  2618     03  WS-MID-LNE2.
  2619         05  FILLER      PIC  X(01) VALUE "³".
  2620         05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
  2621         05  FILLER      PIC  X(01) VALUE "³".
  2622
  2623 COPY WS.WS.
  3021
  3022 COPY LEDTRF.WS.
  3047
  3048 01  W00-TRAN-CODES.
  3049     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
  3050     03  W00-E-DESC  PIC  X(12).
  3051     03  W00-A-DESC  PIC  X(12).
  3052     03  W00-ACTION  PIC  9(01).
  3053     03  W00-T-CODE  PIC  9(02)    COMP-5.
  3054     03  W00-T-GL    PIC  9(06)    COMP-3.
  3055     03  W00-T-DAY   PIC  9(06)    COMP-5.
  3056     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
  3057     03  W00-T-MTD   PIC  9(06)    COMP-5.
  3058     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
  3059     03  W00-T-YTD   PIC  9(06)    COMP-5.
  3060     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.
  3061
  3062 01  WS-PARID.
  3063     03  WS-SYS-ID       PIC  X(03).
  3064
  3065 01  W02-FID.
  3066
  3067 COPY APACFIDS.ID.
  3072
  3073 COPY AUDIT.ID.
  3075
  3076 COPY CARDEX.ID.
  3078
  3079 COPY CONTROL.ID.
  3081
  3082 COPY DBTRAN.ID.
  3084
  3085 COPY DEBTOR.ID.
  3087
  3088 COPY DEPART.ID.
  3090
  3091 COPY GARTEE.ID.
  3093
  3094 COPY LEDTRF.ID.
  3096
  3097 COPY INVOIC.ID.
  3099
  3100 COPY PARAM.ID.
  3102
  3103 COPY RECOVER.ID.
  3105
  3106 COPY SALE.ID.
  3108
  3109 COPY SHARED.ID.
  3111
  3112 COPY SPARTS.ID.
  3114
  3115 COPY STOCK.ID.
  3117
  3118 COPY TXTRAN.ID.
  3120
  3121 COPY WSTOCK.ID.
  3123
  3124 01  W02-FILE-IDS.
  3125     03  W02-PRINTER.
  3126	       05  W02-REPORT PIC X(07) VALUE "INVF20.".
  3127         05  W02-USER   PIC X(05) VALUE SPACES.
  3128
  3129     03  W02-CSHDRW     PIC X(04) VALUE "COM1".
  3130
  3131 COPY W05.VAT.
  3155
  3156 COPY W10.SAL.
  3437
       01  W10-REF.
           03  W10-NUM         PIC  9(08).

  3438 COPY W12.WS.
  3461
  3462 01  W15-DISCOUNT.
  3463     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.
  3464
  3465 COPY W20.WS.
  3508
  3509 01  W25-CALCS.
  3510     03  W25-RESULT      PIC 9(05)V99.
  3511     03  W25-RESULT1 REDEFINES W25-RESULT.
  3512         05  W25-WHOLE   PIC 9(03).
  3513         05  W25-DECIMAL PIC 9(02).
  3514     03  W25-RESULT2 REDEFINES W25-RESULT.
  3515         05  W25-KEY     PIC 9(04).
  3516         05  W25-SUB     PIC 9(01).
  3517         05  FILLER      PIC 9(02).
  3518     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
  3519     03  W25-VALUE       PIC S9(07)V99 COMP-3.
  3520     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
  3521     03  W25-TIME        PIC 9(08).
  3522     03  W25-TRED REDEFINES  W25-TIME.
  3523         05  W25-HH      PIC 9(02).
  3524         05  W25-MM      PIC 9(02).
  3525         05  W25-SS      PIC 9(02).
  3526         05  FILLER      PIC 9(02).
  3527
  3528 01  W30-MESSAGE.
  3529     03  W30-PERC        PIC Z9.99.
  3530     03  FILLER          PIC  X(40) VALUE
  3531         "% Settlement discount if paid in 30 DAYS".
  3532
  3533 COPY W40.DBT.
  3654
  3655 COPY W40.WS.
  3782
  3783 COPY FUNCTION.WS.
  3964
  3965 COPY W50.WS.
  4080
  4081 COPY W70DEBT.IWS.
  4115
  4116 01  W80-ACCOUNT.
  4117	   03  W80-ACNO        PIC X(06).
  4118     03  W80-CRED REDEFINES W80-ACNO.
  4119         05  W80-CNO     PIC 9(05).
  4120         05  W80-CAC REDEFINES W80-CNO.
  4121             07  W80-CTYPE PIC 9(01).
  4122             07  W80-SER   PIC 9(04).
  4123         05  FILLER        PIC 9(01).
  4124     03  W80-TYPE        PIC 9(01).
  4125
  4126 COPY W90.DBT.
  4205
  4206 COPY W95.STM.
  4229
  4230 01  W100-EDIT.
  4231     03  W100-LBAL       PIC Z(06)9.99-.
  4232     03  W100-LARR       PIC Z(06)9.99.
  4233	   03  W100-BAL        PIC Z(08)9.99-.
  4234     03  W100-CUR        PIC Z(08)9.99-.
  4235     03  W100-30         PIC Z(08)9.99-.
  4236     03  W100-60         PIC Z(08)9.99-.
  4237     03  W100-90         PIC Z(08)9.99-.
  4238     03  W100-120        PIC Z(08)9.99-.
  4239     03  W100-INT        PIC Z(08)9.99-.
  4240     03  W100-CRL        PIC Z(06)9.
  4241     03  W100-SER        PIC ZZ9.
  4242     03  W100-V2.
  4243         05  W100-S6V2   PIC Z(05)9.99-.
  4244     03  W100-V3 REDEFINES W100-V2.
  4245         05  W100-PRICE  PIC X(09).
  4246         05  FILLER      PIC X(01).
  4247     03  W100-V4 REDEFINES W100-V2.
  4248         05  W100-EXT    PIC Z(06)9.99.
  4249     03  W100-V5 REDEFINES W100-V2.
  4250         05  W100-QNT    PIC Z(05)9.99-.
  4251
  4252 LINKAGE SECTION.
  4253
  4254 COPY CHAIN.LS.

  4333/
  4334 SCREEN SECTION.
  4335
  4336 COPY BLANK.CRT.
  4482
  4483 COPY ERROR.CRT.

  4504/
  4505 PROCEDURE DIVISION USING LS-PARID
  4506                          LS-USER-ID
  4507				LS0-PROGRAMS
				LS0-SECURITY
  4508                          W40-COMPANY
  4509                          W90-TRAN.
  4510 AA000-MAIN      SECTION.
  4511 AA000-INIT.
  4512       PERFORM ZA000-INIT.
  4513     IF W40-ACNO = "newinv"
  4514         PERFORM BB050
  4515     ELSE
  4516     IF W40-REPRINT = "Y"
  4517         PERFORM BE000
  4518     ELSE
  4519         PERFORM BC000.
  4520       GO TO AZ000-EOJ.
  4521
  4522 COPY ERROR.SCR.
  4628
  4629 COPY FUNCTION.SCR.
  4853
  4854 COPY LOCKED.RC2.
  4894
  4895 COPY DEBTOR.TRN.

  4983/
  4984*    ****    READ FILES ROUTINES
  4985*
  4986 AC000              SECTION.
  4987
  4988 COPY AUDIT.RD.
  5021
  5022 COPY CONTROL.RD.
  5060
  5061 COPY DBTRAN.RD1.
  5147
  5148 COPY DEBTOR.RDI.
  5280
  5281 COPY DEPART.RD.
  5408
  5409 COPY GARTEE.RD.
  5563
  5564 COPY LEDTRF.RD.
  5594
  5595 COPY PARAM.RD.
  5664
  5665 COPY SALE.RD.
  5682
  5683 COPY SHARED.RD.
  5720
  5721 COPY SPARTS.RD.
  5796
  5797 COPY STOCK.RD.
  5957
  5958 COPY TXTRAN.RD.
  6055
  6056 COPY WSTOCK.RD.
  6173
  6174/
  6175 AD000-WRITE             SECTION.
  6176
  6177 COPY AUDIT.WR.
  6202
  6203 COPY CARDEX.WR.
  6241
  6242 COPY CONTROL.WR.
  6284
  6285 COPY DBTRAN.WR.
  6324
  6325 COPY DEBTOR.WR.
  6360
  6361 COPY DEPART.WR.
  6387
  6388 COPY GARTEE.WR.
  6434
  6435 COPY INVOIC.WR.
  6478
  6479 COPY LEDTRF.WR.
  6505
  6506 COPY PARAM.WR.
  6540
  6541 COPY SHARED.WR.
  6559
  6560 COPY STOCK.WR.
  6603
  6604 COPY TXTRAN.WR.
  6649
  6650 COPY WSTOCK.WR.
  6685
  6686/
  6687*       ****   *****   ***    ***   *   *  *****  ****   *   *
  6688*       *   *  *      *   *  *   *  *   *  *      *   *  *   *
  6689*       ****   ***    *      *   *  *   *  ***    ****    * *
  6690*       *   *  *      *   *  *   *   * *   *      *   *    *
  6691*       *   *  *****   ***    ***     *    *****  *   *    *
  6692*    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  6693*          THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
  6694*    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  6695*        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  6696*        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
  6697*        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
  6698*        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
  6699*        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
  6700*        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  6701 AY000           SECTION.
  6702*    *************************************************************
  6703*    ****           P A R A M E T E R   F I L E               ****
  6704*    *************************************************************
  6705 AY01.
  6706       MOVE 01             TO REC-FILE.
  6707       MOVE WS-PARKEY      TO REC-KEY.
  6708       MOVE ZERO           TO REC-TYPE.
  6709       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  6710       MOVE PAR-RECORD1    TO REC-PARAM.
  6711       GO TO AY50.
  6712*    *************************************************************
  6713*    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
  6714*    *************************************************************
  6715 AY02.
  6716       MOVE 02             TO REC-FILE.
  6717       MOVE ZERO           TO REC-KEY.
  6718       MOVE ZERO           TO REC-TYPE
  6719       PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
  6720       MOVE DEB-RECORD1    TO REC-DEBTOR.
  6721       GO TO AY50.
  6722*    *************************************************************
  6723*    ****    D E B T O R   T R A N S A C T I O N   F I L E    ****
  6724*    *************************************************************
  6725 AY04.
  6726       MOVE 04             TO REC-FILE.
  6727       MOVE ZERO           TO REC-KEY.
  6728       MOVE WS-ACTION      TO REC-TYPE
  6729       MOVE TRA-RECORD1    TO REC-DBTRAN.
  6730       GO TO AY50.
  6731*    *************************************************************
  6732*    ****                S T O C K   F I L E                  ****
  6733*    *************************************************************
  6734 AY05.
  6735       MOVE 05             TO REC-FILE.
  6736       MOVE ZERO           TO REC-KEY.
  6737       MOVE ZERO           TO REC-TYPE.
  6738       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
  6739       MOVE STK-RECORD1    TO REC-STOCKF.
  6740       GO TO AY50.
  6741*    *************************************************************
  6742*    ****             G U A R A N T E E   F I L E             ****
  6743*    *************************************************************
  6744 AY09.
  6745       MOVE 09             TO REC-FILE.
  6746       MOVE ZERO           TO REC-KEY.
  6747       MOVE WS-ACTION      TO REC-TYPE.
  6748       MOVE GUA-RECORD1    TO REC-GARTEE.
  6749       GO TO AY50.
  6750*    *************************************************************
  6751*    ****         D A I L Y   -   A U D I T   F I L E         ****
  6752*    *************************************************************
  6753 AY16.
  6754       MOVE 16             TO REC-FILE.
  6755       MOVE WS-AUDKEY      TO REC-KEY.
  6756       MOVE WS-ACTION      TO REC-TYPE.
  6757       MOVE AUD-REC1       TO REC-AUDITF.
  6758       GO TO AY50.
  6759*    *************************************************************
  6760*    ****       V A L U E   A D D E D   T A X   F I L E       ****
  6761*    *************************************************************
  6762 AY19.
  6763       MOVE 19             TO REC-FILE.
  6764       MOVE ZERO           TO REC-KEY.
  6765       MOVE WS-ACTION      TO REC-TYPE.
  6766       MOVE TAX-RECORD1    TO REC-TXTRAN.
  6767       GO TO AY50.
  6768*    *************************************************************
  6769*    ****        D E B T O R   I N V O I C E   F I L E        ****
  6770*    *************************************************************
  6771 AY22.
  6772       MOVE 22             TO REC-FILE.
  6773       MOVE ZERO           TO REC-KEY.
  6774       MOVE WS-ACTION      TO REC-TYPE.
  6775       MOVE DOC-REC1       TO REC-INVOIC.
  6776       GO TO AY50.
  6777*    *************************************************************
  6778*    ****      N E T W O R K   (S H A R E D)   F I L E        ****
  6779*    *************************************************************
  6780 AY37.
  6781       MOVE 37             TO REC-FILE.
  6782       MOVE WS-SHARED      TO REC-KEY.
  6783       MOVE ZERO           TO REC-TYPE.
  6784       MOVE SHR-RECORD     TO REC-NETWORK.
  6785       GO TO AY50.
  6786*    *************************************************************
  6787*    ****            D E P A R T M E N T   F I L E            ****
  6788*    *************************************************************
  6789 AY38.
  6790       MOVE 38             TO REC-FILE.
  6791       MOVE ZERO           TO REC-KEY.
  6792       MOVE ZERO           TO REC-TYPE.
  6793       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
  6794     IF WS-F-ERROR = 7
  6795         MOVE "XXXX"       TO DPT-CODE
  6796         GO TO AY38.
  6797       MOVE DPT-RECORD     TO REC-DEPART.
  6798       GO TO AY50.
  6799*    *************************************************************
  6800*    ****             C O N T R O L   F I L E                 ****
  6801*    *************************************************************
  6802 AY39.
  6803       MOVE 39             TO REC-FILE.
  6804       MOVE WS-NETKEY      TO REC-KEY.
  6805       MOVE ZERO           TO REC-TYPE.
  6806       MOVE NET-RECORD     TO REC-NETWORK.
  6807       GO TO AY50.
  6808*    *************************************************************
  6809*    ****           I N T E G R A T I O N   F I L E           ****
  6810*    *************************************************************
  6811 AY40.
  6812       MOVE 40             TO REC-FILE.
  6813       MOVE ZERO           TO REC-KEY.
  6814       MOVE WS-ACTION      TO REC-TYPE
  6815       MOVE XFR-REC        TO REC-LEDTRF.
  6816       GO TO AY50.
  6817*    *************************************************************
  6818*    ****       S A L E S   -   C A R D E X   F I L E         ****
  6819*    *************************************************************
  6820 AY43.
  6821       MOVE 43             TO REC-FILE.
  6822       MOVE ZERO           TO REC-KEY.
  6823       MOVE WS-ACTION      TO REC-TYPE
  6824       MOVE CRD-REC1       TO REC-CARDEX.
  6825       GO TO AY50.
  6826*    *************************************************************
  6827*    ****       W A R E H O U S E   S T O C K   F I L E       ****
  6828*    *************************************************************
  6829 AY052.
  6830       MOVE 52             TO REC-FILE.
  6831       MOVE ZERO           TO REC-KEY.
  6832       MOVE WS-ACTION      TO REC-TYPE
  6833       PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
  6834       MOVE WST-RECORD1    TO REC-WSTOCK.
  6835       GO TO AY50.
  6836*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  6837*    ³              End transaction in recovery log              ³
  6838*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  6839 AY49.
  6840       MOVE 99             TO REC-FILE.
  6841       MOVE ZERO           TO REC-KEY.
  6842       MOVE SPACES         TO REC-DETAIL.
  6843       PERFORM AY50.
  6844       ADD 1               TO WS-TRANS.
  6845*    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  6846*    ³ If recovery records exceed 195 - Clear (Close and Open    ³
  6847*    ³ new). This allows for quicker recoveries when required.   ³
  6848*    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
  6849     IF WS-RECOVER > 195
  6850         CLOSE RECOVER
  6851         OPEN OUTPUT RECOVER
  6852         CLOSE RECOVER
  6853         OPEN I-O RECOVER
  6854         MOVE ZERO         TO WS-RECOVER.
  6855       GO TO AY59.
  6856 AY50.
  6857       ADD 1               TO WS-RECOVER.
  6858       MOVE WS-RECOVER     TO WS-RECKEY.
  6859       MOVE WS-TRANS       TO REC-TRAN.
  6860	     WRITE REC-RECORD.
  6861     IF WS-STATUS NOT = "00"
  6862         DISPLAY "Write error Recovery file - Status " AT 2212
  6863                  WITH FOREGROUND-COLOR 14
  6864                  WS-STATUS WITH FOREGROUND-COLOR 15
  6865         STOP RUN.
  6866       CLOSE RECOVER.
  6867       OPEN I-O RECOVER.
  6868 AY59.
  6869       EXIT.
  6870*
  6871*    ****    Start processing transaction
  6872*
  6873 AY60.
  6874     IF WS-SKIP = "Y" OR "A"
  6875         GO TO AY999.
  6876       MOVE 1              TO WS-COUNT.
  6877       MOVE 5              TO WS-SHARED.
  6878       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
  6879       MOVE SHR-STOCK      TO WS-STOCK.
  6880*
  6881*    ****    Read PARAM record 4 - Lock record and start recovery
  6882*            file procedure.
  6883*
  6884       MOVE 4              TO WS-PARKEY.
  6885       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
  6886     IF PAR-USERS < 24
  6887         MOVE 1            TO WS-SUB
  6888         MOVE ZERO         TO WS-WAIT
  6889         GO TO AY62.
  6890*
  6891*    ****   Q   F U L L  -  W A I T   F O R   4   S E C O N D S
  6892*
  6893       DISPLAY "WAITING" AT 2551
  6894                WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
  6895       COMMIT.
  6896       ACCEPT WS-STIME FROM TIME.
  6897       MOVE 400            TO WS-WAIT.
  6898       PERFORM LOCK-REC-LOOP.
  6899       DISPLAY SPACE AT 2551
  6900               WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
  6901       GO TO AY60.
  6902
  6903 AY61.
  6904       MOVE "DS"           TO PAR-PROG(WS-USUB).
  6905       MOVE LS-USER        TO PAR-USR(WS-USUB).
  6906       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
  6907*
  6908*    ****    Read the DEBTOR control record in the NETWORK file
  6909*            and lock it. Log details to the RECOVERY file.
  6910*
  6911       MOVE 1              TO WS-NETKEY.
  6912       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
  6913       PERFORM AY39 THRU AY59.
  6914       MOVE NET-DEBTOR     TO W70-DEBT.
  6915     IF WS-STOCK = "Y"
  6916         MOVE 3            TO WS-SHARED
  6917         PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
  6918	       PERFORM AY37 THRU AY59
  6919         MOVE SHR-RECORD   TO NET-RECORD
  6920     ELSE
  6921         MOVE 3            TO WS-NETKEY
  6922         PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
  6923         PERFORM AY39 THRU AY59.
  6924       GO TO AY999.
  6925*
  6926*    ****   A R E   A N Y   U P D A T E S   I N   P R O G R E S S
  6927*
  6928 AY62.
  6929     IF NOT (PAR-PROG(WS-SUB) = SPACES OR
  6930             PAR-USR(WS-SUB) = SPACES)
  6931*
  6932*    ****   A R E   D E B T O R   O R   S T O C K   F I L E S
  6933*           B E I N G   U P D A T E D
  6934*
  6935         IF NOT (PAR-PROG(WS-SUB) = SPACES)
  6936             IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
  6937*
  6938*    ****   Y E S   -   S E T   W A I T   P E R I O D
  6939*
  6940                 GO TO AY62-WAIT
  6941             ELSE
  6942                 ADD 1     TO WS-SUB
  6943                 GO TO AY62
  6944             END-IF
  6945         ELSE
  6946*
  6947*    ****   I S   T H I S   P R O G R A M   I N   T H E  Q
  6948*
  6949         IF PAR-USR(WS-SUB) = LS-USER
  6950*
  6951*    ****   I S   I T   N E X T   I N   L I N E   T O   P R O C E S S
  6952*
  6953             IF WS-WAIT = ZERO
  6954                 GO TO AY63
  6955             ELSE
  6956                 GO TO AY62-WAIT
  6957             END-IF
  6958         ELSE
  6959             GO TO AY62-WAIT
  6960         END-IF
  6961     END-IF.
  6962       MOVE LS-USER        TO PAR-USR(WS-SUB).
  6963       MOVE WS-SUB         TO PAR-USERS.
  6964       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  6965       GO TO AY62-CHECK.
  6966*
  6967*    ****   S E T   W A I T   P E R I O D
  6968*
  6969 AY62-WAIT.
  6970       MOVE 300            TO WS-WAIT.
  6971     IF NOT (PAR-USR(WS-SUB) = LS-USER)
  6972         IF WS-SUB < 24
  6973             ADD 1         TO WS-SUB
  6974             GO TO AY62.
  6975
  6976 AY62-CHECK.
  6977     IF WS-WAIT > ZERO
  6978         COMMIT
  6979         DISPLAY "Waiting" AT 2572
  6980                  WITH BACKGROUND-COLOR 3
  6981                       FOREGROUND-COLOR 14 BLINK
  6982         ACCEPT WS-STIME FROM TIME
  6983         PERFORM LOCK-REC-LOOP
  6984         DISPLAY "Waiting" AT 2572
  6985                  WITH BACKGROUND-COLOR 3
  6986                       FOREGROUND-COLOR 14 BLINK
  6987         GO TO AY60.
  6988       DISPLAY SPACE AT 2572
  6989               WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
  6990
  6991 AY63.
  6992       MOVE WS-SUB         TO WS-USUB.
  6993       GO TO AY61.
  6994
  6995 AY70.
  6996       MOVE 4              TO WS-PARKEY.
  6997       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
  6998*
  6999*    ****    Write links back and unlock PARAM record 4 and
  7000*            NETWORK record 1.
  7001*
  7002     IF WS-STOCK = "Y"
  7003         MOVE NET-RECORD   TO SHR-RECORD
  7004         PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
  7005     ELSE
  7006         PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
  7007       MOVE 1              TO WS-NETKEY.
  7008       MOVE W70-DEBT       TO NET-DEBTOR.
  7009       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
  7010       PERFORM AY49 THRU AY59.
  7011       MOVE 1              TO WS-USUB.
  7012
  7013 AY72.
  7014     IF NOT (PAR-USR(WS-USUB) = LS-USER)
  7015         ADD 1             TO WS-USUB
  7016         GO TO AY72.
  7017
  7018 AY75.
  7019       MOVE SPACES         TO PAR-PROG(WS-USUB)
  7020                              PAR-USR(WS-USUB).
  7021     IF WS-USUB < 24
  7022         ADD 1 WS-USUB     GIVING WS-SUB
  7023     ELSE
  7024         GO TO AY80.
  7025     IF NOT (PAR-PROG(WS-SUB) = SPACES)
  7026         MOVE PAR-PROG(WS-SUB)
  7027                           TO PAR-PROG(WS-USUB)
  7028         MOVE PAR-USR(WS-SUB)
  7029                           TO PAR-USR(WS-USUB)
  7030         ADD 1             TO WS-USUB
  7031         GO TO AY75.
  7032
  7033 AY80.
  7034	     SUBTRACT 1 FROM WS-USUB
  7035                           GIVING PAR-USERS.
  7036       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  7037     IF LS0-POS NOT = 2
  7038         CLOSE RECOVER
  7039         OPEN I-O RECOVER.
  7040       COMMIT.
  7041
  7042 AY999.
  7043       EXIT.
  7044/
  7045 AZ000-END               SECTION.
  7046 AZ000-EOJ.
  7047       MOVE WS-10CPI       TO REP-DETAIL2.
  7048       WRITE REP-LINE2 BEFORE 0 LINES.
  7049       MOVE WS-6LPI        TO REP-DETAIL2.
  7050       WRITE REP-LINE2 BEFORE 0 LINES.
  7051       CLOSE RECOVER
  7052             PRNREP.
  7053 AZ100.
  7054       EXIT PROGRAM.
  7055
  7056*
  7057*    ****    Read debtor record - using account number.
  7058*
  7059 CA155-GET-DEBTOR  SECTION 2.
  7060 CA155.
  7061       MOVE W80-ACNO       TO DEB-ACNO.
  7062       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
  7063     IF WS-F-ERROR = 6
  7064         MOVE 1            TO WS-ERROR
  7065     ELSE
  7066     IF DEB-ACNO NOT = W80-ACNO
  7067         MOVE 2            TO WS-ERROR
  7068     ELSE
  7069         MOVE 0            TO WS-ERROR
  7070         MOVE DEB-TYPE     TO W80-TYPE.
  7071 CA160-EXIT.
  7072       EXIT.
  7073
  7074 COPY LEDTRF.UPD.
  7145
  7146/
  7147*    ****    G E N E R A T E   A   D E B T O R
  7148*            T R A N S A C T I O N
  7149*
  7150 GENERATE-DEBTOR-TRAN SECTION 2.
  7151
  7152 DEBTOR-TRAN.
  7153
  7154 LINK-DEBTOR-TRANS.
  7155       INITIALIZE TRA-RECORD1.
  7156       MOVE W20-DTE        TO TRA-DATE.
  7157       MOVE W20-DTE        TO DEB-ACTIVE.
  7158       MOVE W90-CODE       TO TRA-CODE.
  7159       MOVE W90-REF        TO TRA-REF.
  7160       MOVE W90-REG        TO TRA-REG.
  7161       MOVE W90-ORD        TO TRA-ORD.
  7162       MOVE DEB-ACNO       TO TRA-AC.
  7163     IF W90-DB NOT = ZERO
  7164         MOVE W90-DB       TO TRA-VALUE
  7165     ELSE
  7166         MOVE W90-CR       TO TRA-VALUE.
  7167     IF W00-ACTION = 1
  7168         MULTIPLY -1 BY W90-NONTAX
  7169                           GIVING TRA-TAXFREE
  7170     ELSE
  7171         MOVE W90-NONTAX   TO TRA-TAXFREE.
  7172       MOVE W90-TTAX       TO TRA-TAX.
  7173       ADD TRA-TAX         TO W00-T-VAT.
  7174       PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
  7175       MOVE 1              TO WS-ACTION.
  7176       PERFORM AY04 THRU AY59.
  7177       MOVE 1              TO TAX-ACTYPE.
  7178       MOVE TRA-CODE       TO TAX-CODE.
  7179       MOVE TRA-DATE       TO TAX-DATE.
  7180       MOVE TRA-REF        TO TAX-REF.
  7181       MOVE TRA-VALUE      TO TAX-VALUE.
  7182       MOVE TRA-TAXFREE    TO TAX-TAXFREE.
  7183       MOVE TRA-AC         TO TAX-AC.
  7184       IF W90-CR NOT = ZERO
  7185           MOVE "1"        TO TAX-TYPE
  7186       ELSE
  7187           MOVE "2"        TO TAX-TYPE.
  7188       MOVE ZERO           TO TAX-SEQ.
  7189       MOVE 1              TO WS-S9.
  7190
  7191 DEBTOR-TAX.
  7192       MOVE W90-TAXVAL(WS-S9)
  7193                           TO TAX-VAL(WS-S9).
  7194       MOVE W90-VAT(WS-S9) TO TAX-VAT(WS-S9).
  7195     IF WS-S9 < 12
  7196         ADD 1             TO WS-S9
  7197         GO TO DEBTOR-TAX.
  7198       PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
  7199       MOVE 1              TO WS-ACTION.
  7200       PERFORM AY19 THRU AY59.
  7201       ADD TRA-VALUE       TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
  7202       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
  7203       PERFORM UPDATE-TRAN-RECORD.
  7204	   IF WX-INT = "Y"
  7205         PERFORM DEBTOR-INTEGRATE.
  7206     IF W90-CR NOT = ZERO
  7207         GO TO DEBTOR-CR-TRANS.
  7208       ADD W90-DB          TO DEB-DEBT
  7209                              DEB-BAL
  7210                              DEB-PURYTD
  7211                              DEB-MTHPUR (W20-MONTH).
  7212       GO TO DEBTOR-TRAN-EXIT.
  7213
  7214 DEBTOR-INTEGRATE.
  7215       MOVE W20-DTE        TO WX-DATE.
  7216       MOVE "  DT"         TO WX-LEDG.
  7217       MOVE W90-CODE       TO WX-TRN.
  7218       MOVE W00-E-DESC     TO WX-DESC.
  7219       MOVE "- ACCOUNTS RECEIVABLE"
  7220                           TO WX-BOOK.
  7221       MOVE TRA-VALUE      TO WX-VALUE.
  7222       MOVE WX-DEBGL       TO WX-AC.
  7223       MOVE 01             TO WX-TYPE.
  7224       PERFORM GL-TRF-UPDATE.
  7225
  7226 DEBTOR-CR-TRANS.
  7227       SUBTRACT W90-CR     FROM DEB-CRED
  7228                                DEB-MTHPUR (W20-MONTH).
  7229       ADD W90-CR          TO DEB-PURYTD.
  7230       ADD W90-CR          TO DEB-BAL.
  7231     IF OPEN-ITEM
  7232         GO TO DEBTOR-TRAN-EXIT.
  7233     IF WS-AGE = "M"
  7234         GO TO DEBTOR-TRAN-EXIT.
  7235       MOVE ZERO           TO W90-CUR W90-30 W90-60 W90-90.
  7236       MOVE W90-CR         TO W90-VALUE.
  7237     IF WS-AGE = "1"
  7238         ADD W90-VALUE     TO DEB-120
  7239         MOVE W90-VALUE    TO W90-120.
  7240     IF DEB-120 < ZERO
  7241         SUBTRACT DEB-120  FROM W90-120
  7242         MOVE DEB-120      TO W90-VALUE
  7243         MOVE ZERO         TO DEB-120
  7244         MOVE "9"          TO WS-AGE.
  7245     IF WS-AGE = "9"
  7246         ADD W90-VALUE     TO DEB-90
  7247         MOVE W90-VALUE    TO W90-90.
  7248     IF DEB-90 < ZERO
  7249         SUBTRACT DEB-90   FROM W90-90
  7250         MOVE DEB-90       TO W90-VALUE
  7251         MOVE ZERO         TO DEB-90
  7252         MOVE "6"          TO WS-AGE.
  7253     IF WS-AGE = "6"
  7254         ADD W90-VALUE     TO DEB-60
  7255         MOVE W90-VALUE    TO W90-60.
  7256     IF DEB-60 < ZERO
  7257         SUBTRACT DEB-60   FROM W90-60
  7258         MOVE DEB-60       TO W90-VALUE
  7259         MOVE ZERO         TO DEB-60
  7260         MOVE "3"          TO WS-AGE.
  7261     IF WS-AGE = "3"
  7262	       ADD W90-VALUE	 TO DEB-30
  7263         MOVE W90-VALUE    TO W90-30.
  7264     IF DEB-30 < ZERO
  7265         SUBTRACT DEB-30   FROM W90-30
  7266         MOVE DEB-30       TO W90-VALUE
  7267         MOVE ZERO         TO DEB-30
  7268         MOVE "C"          TO WS-AGE.
  7269     IF WS-AGE = "C"
  7270         ADD W90-VALUE     TO DEB-CUR
  7271         MOVE W90-VALUE    TO W90-CUR.
  7272     IF DEB-CUR < ZERO
  7273         SUBTRACT DEB-CUR  FROM W90-CUR
  7274         MOVE ZERO         TO DEB-CUR.
  7275
  7276 DEBTOR-TRAN-EXIT.
  7277       EXIT.
  7278/
  7279 BB050               SECTION 5.
  7280 BB50.
  7281     IF WS-PRNINV = "N"
  7282         GO TO BB60.
  7283       MOVE SPACES         TO REP-DETAIL2.
  7284     IF WS-HEAD = "N"
  7285         WRITE INV-L1 BEFORE 11 LINES
  7286         GO TO BB60.
  7287       MOVE W95-COMP       TO INV-NADD.
  7288       WRITE INV-L1 BEFORE 1 LINES.
  7289       MOVE W95-ADD1       TO INV-NADD.
  7290       WRITE INV-L1 BEFORE 1 LINES.
  7291       MOVE SPACES         TO REP-DETAIL2.
  7292       MOVE W95-ADD2       TO INV-NADD.
  7293       WRITE INV-L1 BEFORE 1 LINES.
  7294       MOVE W95-ADD3       TO INV-NADD.
  7295       WRITE INV-L1 BEFORE 1 LINES.
  7296       MOVE W95-ADD4       TO INV-NADD.
  7297       WRITE INV-L1 BEFORE 1 LINES.
  7298 BB55.
  7299       MOVE SPACES         TO REP-DETAIL2.
  7300       MOVE W95-PC         TO INV-PC.
  7301       WRITE INV-L1 BEFORE 1 LINES.
  7302       MOVE SPACES         TO REP-DETAIL2.
  7303       WRITE INV-L1 BEFORE 1 LINES.
  7304
  7305 BB60.
  7306       EXIT.
  7307
  7308/
  7309 BA000           SECTION 50.
  7310 BA80.
  7311       MOVE W70-AUDIT      TO WS-AUDKEY.
  7312       MOVE LOW-VALUES     TO AUD-REC1.
  7313       PERFORM AY16 THRU AY59.
  7314       MOVE STK-CODE       TO AUD-EXT-CODE.
  7315       MOVE 2              TO AUD-TYPE.
  7316       MOVE ZERO           TO AUD-SUB.
  7317       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7318       ADD 1               TO W70-AUDIT.
  7319       MOVE 1              TO WS-AUDKEY.
  7320       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7321       PERFORM AY16 THRU AY59.
  7322       MOVE 1              TO AUD-REP2.
  7323       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7324 BA90.
  7325       MOVE W70-AUDIT      TO WS-AUDKEY.
  7326       MOVE LOW-VALUES     TO AUD-REC1.
  7327       PERFORM AY16 THRU AY59.
  7328       MOVE 7              TO AUD-TYPE.
  7329       MOVE WS-DRAW        TO AUD-SUB.
  7330       MOVE W90-REF        TO AUD-CSH.
  7331       MOVE W90-EXCTOT     TO AUD-SELL.
  7332       MOVE W90-TCOST      TO AUD-COST.
  7333       MOVE W10-SMAN       TO AUD-SMAN.
  7334       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7335       ADD 1               TO W70-AUDIT.
  7336       MOVE 1              TO WS-AUDKEY.
  7337       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7338       PERFORM AY16 THRU AY59.
  7339       MOVE 1              TO AUD-REP7.
  7340       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7341 BA95.
  7342       MOVE W70-AUDIT      TO WS-AUDKEY.
  7343       MOVE LOW-VALUES     TO AUD-REC1.
  7344       PERFORM AY16 THRU AY59.
  7345       MOVE 3              TO AUD-TYPE
  7346       MOVE DEB-ACNO       TO AUD-AC.
  7347       MOVE WS-DRAW        TO AUD-SUB.
  7348       MOVE W90-REF        TO AUD-REF.
  7349       MOVE W90-TTAX       TO AUD-TAX.
  7350       MOVE W90-TOTAL      TO AUD-VAL.
  7351       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7352       ADD 1               TO W70-AUDIT.
  7353       MOVE 1              TO WS-AUDKEY.
  7354       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7355       PERFORM AY16 THRU AY59.
  7356       MOVE 1              TO AUD-REP3.
  7357       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7358 BA100.
  7359       MOVE W70-AUDIT      TO WS-AUDKEY.
  7360       MOVE LOW-VALUES     TO AUD-REC1.
  7361       PERFORM AY16 THRU AY59.
  7362       MOVE 8              TO AUD-TYPE.
  7363       MOVE WS-DRAW        TO AUD-SUB.
  7364       MOVE W10-SMAN       TO AUD-SMAN.
  7365       MOVE W90-REF        TO AUD-CSH.
  7366	     MOVE W90-VALUE	 TO AUD-SEL.
  7367       MOVE W90-ICST       TO AUD-CST.
  7368       MOVE STK-CODE       TO AUD-EXT-STOCK.
  7369       MOVE WS-SUPER       TO AUD-SUPER.
  7370       MOVE WS-DISCOUNT    TO AUD-DISC.
  7371       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7372       ADD 1               TO W70-AUDIT.
  7373       MOVE 1              TO WS-AUDKEY.
  7374       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
  7375       PERFORM AY16 THRU AY59.
  7376       MOVE 1              TO AUD-REP8.
  7377       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
  7378/
  7379 BB070               SECTION 50.
  7380 BB70.
  7381       INITIALIZE GUA-RECORD1.
  7382       MOVE ZERO           TO WS-F-ERROR.
  7383       MOVE STK-CODE       TO GUA-EXT-CODE.
  7384       MOVE W10-SERNO      TO GUA-SERIAL.
  7385     IF WS-GAR = 1
  7386         GO TO BB80.
  7387       MOVE STK-DESC       TO GUA-DESC.
  7388       MOVE W40-NAME       TO GUA-NAME.
  7389       MOVE W12-T-YMD      TO GUA-DATE1 W20-DTE.
  7390       MOVE STK-GUAR       TO GUA-PERIOD.
  7391       ADD GUA-PERIOD      TO W20-MONTH.
  7392
  7393 BB75.
  7394     IF W20-MONTH > 12
  7395         SUBTRACT 12       FROM W20-MONTH
  7396         IF W20-YEAR = 99
  7397             MOVE ZERO     TO W20-YEAR
  7398             ADD 1         TO W20-CENT
  7399             GO TO BB75
  7400         ELSE
  7401             ADD 1         TO W20-YEAR
  7402             GO TO BB75.
  7403       MOVE W20-DTE        TO GUA-DATE2.
  7404       MOVE W90-REF        TO GUA-INV.
  7405       PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
  7406       MOVE 1              TO WS-ACTION.
  7407       PERFORM AY09 THRU AY59.
  7408       GO TO BB99.
  7409 BB80.
  7410       PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
  7411     IF WS-F-ERROR = 9
  7412         GO TO BB99.
  7413       MOVE 2              TO WS-ACTION.
  7414       PERFORM AY09 THRU AY59.
  7415       PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
  7416 BB99.
  7417       EXIT.
  7418/
  7419*    ****    I N V O I C E   A N D   C A S H   S A L E
  7420*
  7421 BC000         SECTION 50.
  7422 BC05.
  7423       MOVE 1              TO WS-FIRST WS-LAST.
  7424       MOVE "I"            TO WS-CR.
  7425 BC10.
  7426*
  7427*    ****    D E B T O R   A C C O U N T   N U M B E R
  7428*
  7429       MOVE W40-ACNO       TO W80-ACNO.
  7430       PERFORM CA155-GET-DEBTOR.
  7431*
  7432*    ****    S A L E S M A N   N U M B E R
  7433*
  7434       MOVE W40-SMAN       TO W10-SMAN.
  7435       MOVE W40-PRNINV     TO WS-PRNINV.
  7436       MOVE W40-LAST       TO WS-LAST.
  7437       MOVE W40-PRC        TO WS-PRC.
  7438*
  7439*    ****    P R O C E S S   T H E   S A L E
  7440*
  7441 BC188.
  7442       MOVE 1              TO WS-SALKEY.
  7443       PERFORM AY60 THRU AY999.
  7444       PERFORM AY02 THRU AY59.
  7445       MOVE W70-INVOICE    TO W90-REF.
  7446       DISPLAY W90-REF AT 0510
  7447               WITH FOREGROUND-COLOR 6 HIGHLIGHT
  7448                    BACKGROUND-COLOR 0.
  7449     IF W70-INVOICE = 99999999
  7450         MOVE 1            TO W70-INVOICE
  7451     ELSE
  7452         ADD 1             TO W70-INVOICE.
  7453 BC215.
  7454*
  7455*    ****    P R I N T   I N V O I C E   H E A D I N G S
  7456*
  7457       MOVE SPACES         TO REP-DETAIL2.
  7458       MOVE "TAX INVOICE"  TO INV-TYPE.
  7459     IF WS-PRNINV = "N"
  7460         GO TO BC220.
  7461       WRITE INV-L1 BEFORE 2 LINES.
  7462       MOVE SPACES         TO REP-DETAIL2.
  7463       MOVE W40-NAME       TO INV-NAME.
  7464       WRITE INV-L1 BEFORE 1 LINES.
  7465       MOVE W40-ADD1       TO INV-NAME.
  7466     IF W05-VATNO NOT = SPACES
  7467         MOVE W05-VATNO    TO INV-GNO.
  7468       WRITE INV-L1 BEFORE 1 LINES.
  7469       MOVE SPACES         TO REP-DETAIL2.
  7470       MOVE W40-ADD2       TO INV-NAME.
  7471       WRITE INV-L1 BEFORE 1 LINES.
  7472       MOVE W40-ADD3       TO INV-NAME.
  7473       WRITE INV-L1 BEFORE 1 LINES.
  7474       MOVE W40-ADD4       TO INV-NAME.
  7475       WRITE INV-L1 BEFORE 1 LINES.
  7476*
  7477*    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
  7478*
  7479 BC216.
  7480       ADD 1               TO WS-PAGE.
  7481       MOVE SPACES         TO REP-DETAIL2.
  7482       MOVE W40-PC1        TO INV-PC1.
  7483       WRITE INV-L1 BEFORE 3 LINES.
  7484       MOVE SPACES         TO REP-DETAIL2.
  7485*
  7486*    ****    C L I E N T   V A T   N U M B E R
  7487*
  7488     IF NOT (W40-TAX = SPACES)
  7489         MOVE "Vat No"     TO INV-VATH
  7490         MOVE W40-TAX      TO INV-VATNO.
  7491       WRITE INV-L1 BEFORE 4 LINES.
  7492       MOVE SPACES         TO REP-DETAIL2.
  7493       MOVE W12-TODAY      TO INV-DTE.
  7494       MOVE W95-TIME       TO INV-TIME.
  7495     IF W10-SMAN NOT = ZERO
  7496         MOVE W40-ASSIST   TO INV-SMAN.
  7497       MOVE W90-REF        TO INV-REF.
  7498       MOVE W80-ACNO       TO INV-AC.
  7499*
  7500*    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
  7501*
  7502     IF DEB-TERMS = "0"
  7503         MOVE " 7 D"       TO INV-TERMS
  7504     ELSE
  7505     IF DEB-TERMS = "2"
  7506         MOVE "60 D"       TO INV-TERMS
  7507     ELSE
  7508     IF DEB-TERMS = "1"
  7509         MOVE "30 D"       TO INV-TERMS
  7510     ELSE
  7511         MOVE "COD"        TO INV-TERMS.
  7512       MOVE W90-ORD        TO INV-ORD.
  7513       WRITE INV-L1 BEFORE 6 LINES.
  7514       MOVE SPACES         TO REP-DETAIL2.
  7515       MOVE 23             TO WS-LINE.
  7516 BC220.
  7517       EXIT.
  7518 BC222.
  7519       MOVE WS-PAGE        TO DOC-PAGE.
  7520       MOVE W80-ACNO       TO DOC-AC.
  7521       MOVE 100            TO DOC-SQN.
  7522       MOVE ZERO           TO DOC-TYPE.
  7523       MOVE W40-NAME       TO DOC-NAME.
  7524       MOVE ZERO           TO DOC-PICNO.
  7525       MOVE WS-CR          TO DOC-DOC.
  7526       MOVE W90-EXP        TO DOC-EXP.
  7527       MOVE WS-PRC         TO DOC-PRC.
  7528       MOVE W90-ORD        TO DOC-ORD.
  7529       MOVE W90-REF        TO DOC-REF.
  7530       MOVE W90-REG        TO DOC-OREF.
  7531       MOVE W12-T-YMD      TO DOC-DTE.
  7532       MOVE W10-SMAN       TO DOC-SMAN.
  7533	     MOVE W90-TOTAL	 TO DOC-VALUE.
	     MOVE LS0-NO	 TO DOC-USER.
	     MOVE LS0-NAME	 TO DOC-UNME.
  7534*
  7535*    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
  7536*
  7537 BC225.
  7538       PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
  7539       MOVE 1              TO WS-ACTION.
  7540       PERFORM AY22 THRU AY59.
  7541*
  7542*    ****    R E A D   T H E   S A L E   F I L E   A N D
  7543*            P R I N T   T H E   I N V O I C E
  7544*
  7545 BC235.
  7546     IF WS-SALKEY > WS-LAST
  7547         GO TO BC255.
  7548       PERFORM READ-SALE THRU READ-SALE-EXIT.
  7549     IF WS-LINE > 1
  7550         SUBTRACT 1        FROM WS-LINE
  7551     ELSE
  7552         PERFORM BC250.
  7553     IF SAL-EXT-ITEM = SPACES
  7554         GO TO BC247.
  7555       MOVE SAL-EXT-ITEM   TO STK-CODE.
  7556       PERFORM AY05 THRU AY59.
  7557*
  7558*    ****    W A R E H O U S E   S T O C K
  7559*
  7560     IF NOT (W40-CODE = SPACES) AND
  7561            (STK-IND < 2)
  7562         MOVE W40-CODE     TO WST-WAR
  7563         MOVE SAL-EXT-ITEM TO WST-EXT-CODE
  7564         PERFORM AY052 THRU AY59.
  7565*
  7566*    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
  7567*            R E C O R D
  7568*
  7569       MOVE W80-ACNO       TO CRD-AC.
  7570       MOVE ZERO           TO CRD-SEQ.
  7571       MOVE W10-SMAN       TO CRD-SMAN.
  7572       MOVE W90-REF        TO CRD-REF.
  7573       MOVE STK-SUPP       TO CRD-SUPP.
  7574       MOVE W12-T-YMD      TO CRD-DTE.
  7575       MOVE W40-NAME       TO CRD-NAME.
  7576     IF STK-IND < 2
  7577         MOVE W40-CODE     TO CRD-WHS
  7578     ELSE
  7579         MOVE SPACES       TO CRD-WHS.
	     MOVE STK-LDG	 TO CRD-DEPT.
  7580       MOVE 1              TO DOC-TYPE.
  7581       INITIALIZE DOC-DET2.
  7582       MOVE SAL-EXT-ITEM   TO DOC-EXT-ITEM CRD-EXT-CODE.
  7583       MOVE SAL-DESC       TO DOC-DESC.
  7584       MOVE SAL-QNT        TO W10-QUANT DOC-QNT.
  7585       MOVE SAL-PACK       TO WS-P1.
  7586*
  7587*    ****    C H E C K   I F   D I S C O U N T   G I V E N
  7588*
  7589       MOVE "N"            TO WS-DISCOUNT.
  7590       ADD .009 SAL-SELL   GIVING W10-MSELL.
  7591     IF NOT (SAL-SPEC = "Y")
  7592	      IF ((WS-PRC = "1") AND (W10-MSELL < STK-SL(WS-P1))) OR
  7593           ((WS-PRC = "2") AND (W10-MSELL < STK-CSH(WS-P1))) OR
  7594           ((WS-PRC = "3") AND (W10-MSELL < STK-WSL(WS-P1)))
  7595             MOVE "Y"      TO WS-DISCOUNT.
  7596       MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
  7597                           GIVING W10-QNT.
  7598       MOVE W10-QNT        TO CRD-QNT.
  7599       MOVE SAL-COST       TO W10-COST  DOC-COST.
  7600       COMPUTE CRD-COST = SAL-COST / STK-UNT(WS-P1).
  7601       MOVE SAL-VAT        TO DOC-VAT.
  7602       MOVE SAL-SELL       TO W10-SELL  DOC-SELL.
  7603       MOVE SAL-VAL        TO W25-VALUE DOC-VAL.
  7604     IF WS-PRNINV = "Y"
  7605         MOVE SAL-EXT-ITEM TO INV-ITM
  7606         MOVE SAL-DESC     TO INV-DESC
  7607         MOVE SAL-QNT      TO INV-QNT
  7608         MOVE SAL-SELL     TO INV-PRICE
  7609         MOVE SAL-VAL      TO INV-VAL.
  7610       MOVE SAL-DISC       TO W90-CDISC W90-EXCDIS.
  7611       MOVE SAL-DSC        TO WS-DISC INV-DSC.
  7612*
  7613*      C H E C K   C H E C K   C H E C K   C H E C K
  7614*
  7615       SUBTRACT W90-CDISC FROM W25-VALUE
  7616                           GIVING W90-VALUE.
  7617       MOVE SAL-TAXVAL     TO DOC-TAXVAL.
  7618       MOVE SAL-NONTAX     TO DOC-NONTAX.
  7619       MOVE SAL-SERNO      TO W10-SERNO DOC-SERNO.
  7620       MOVE SAL-SUPER      TO WS-SUPER.
  7621       MOVE SAL-VAT-SUB    TO WS-VAT-SUB
  7622                              DOC-VAT-SUB.
  7623     IF WS-VAT-SUB = ZERO
  7624         MOVE ZERO         TO DOC-V-PER
  7625     ELSE
  7626         MOVE W05-VAT(WS-VAT-SUB)
  7627                           TO DOC-V-PER.
  7628     IF W90-TAX = SPACES
  7629       IF W90-EXP NOT = "X"
  7630         IF WS-VAT-SUB > ZERO
  7631             MOVE W05-VAT(WS-VAT-SUB)
  7632                           TO W05-RATE
  7633             COMPUTE W90-VALUE ROUNDED = W90-VALUE -
  7634                 (((W90-VALUE / (W05-RATE + 100.00000)) *
  7635                    W05-RATE))
  7636             COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
  7637                 (((W90-CDISC / (W05-RATE + 100.00000)) *
  7638                    W05-RATE)).
  7639       ADD W90-VALUE       TO W90-EXCTOT.
  7640       COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
  7641     IF WS-CARDEX = "Y"
  7642         PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
  7643         MOVE 1            TO WS-ACTION
  7644         PERFORM AY43 THRU AY59.
  7645     IF WS-PRNINV = "Y"
  7646         WRITE INV-L1 BEFORE 1 LINES
  7647         MOVE SPACES       TO REP-DETAIL2.
  7648       ADD W25-VALUE       TO W90-STOTAL.
  7649*
  7650*    ****    H A S   L I N E   D I S C O U N T   B E E N
  7651*            A P P L I E D
  7652*
  7653     IF W90-CDISC = ZERO
  7654         GO TO BC240.
  7655     IF WS-PRNINV = "Y"
  7656         IF WS-LINE > 1
  7657             SUBTRACT 1    FROM WS-LINE
  7658         ELSE
  7659             PERFORM BC250.
  7660*
  7661*    ****    L O G   T H E   L I N E   D I S C O U N T
  7662*
  7663       MOVE WS-DISC        TO DOC-DSC.
  7664       MULTIPLY -1         BY W90-CDISC
  7665                           GIVING DOC-DISC.
  7666 BC240.
  7667 BC242.
  7668     IF WS-COPY = "Y"
  7669         GO TO BC246.
  7670*
  7671*    ****    U P D A T E   S A L E S   A N A L Y S I S
  7672*
  7673       MOVE STK-LDG        TO DPT-CODE.
  7674       PERFORM AY38 THRU AY59.
  7675       ADD W90-VALUE       TO DPT-SALES-DAY
  7676                              DPT-SALES-MTD
  7677                              DPT-SALES-YTD.
  7678     IF WX-INT = "Y"
  7679         MOVE W12-T-YMD    TO WX-DATE
  7680         MOVE STK-LDG      TO WX-LEDG
  7681         MOVE "SALES - ACCOUNTS RECEIVABLE"
  7682                           TO WX-NAR
  7683         COMPUTE WX-VALUE = W90-VALUE * -1
  7684         MOVE DPT-SALE-GL  TO WX-AC
  7685         MOVE 04           TO WX-TYPE
  7686         PERFORM GL-TRF-UPDATE.
  7687*
  7688*    ****    U P D A T E   C O S T - O F - S A L E S
  7689*
  7690       COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
  7691       ADD W90-BCR         TO DPT-COST-DAY
  7692                              DPT-COST-MTD
  7693                              DPT-COST-YTD.
  7694     IF WX-INT = "Y"
  7695         IF W90-BCR NOT = ZERO
  7696             MOVE W90-BCR  TO WX-VALUE
  7697             MOVE DPT-COST-GL
  7698                           TO WX-AC
  7699             MOVE 08       TO WX-TYPE
  7700             PERFORM GL-TRF-UPDATE
  7701             COMPUTE WX-VALUE = W90-BCR * -1
  7702             MOVE DPT-PUR-GL
  7703                           TO WX-AC
  7704             MOVE 19       TO WX-TYPE
  7705             PERFORM GL-TRF-UPDATE.
  7706*
  7707*    ****    U P D A T E   D I S C O U N T   G I V E N
  7708*
  7709       ADD W90-EXCDIS      TO DPT-DISC-DAY
  7710                              DPT-DISC-MTD
  7711                              DPT-DISC-YTD
  7712                              DPT-SALES-DAY
  7713                              DPT-SALES-MTD
  7714                              DPT-SALES-YTD.
  7715       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  7716       MOVE W12-T-YMD      TO STK-ACT.
  7717*
  7718*    ****    C H E C K   I F   S T O C K   C O N T R O L L E D
  7719*
  7720     IF WS-PRC-IND = "Y"
  7721         IF STK-IND > 1
  7722             GO TO BC245
  7723         END-IF
  7724     ELSE
  7725     IF STK-IND > ZERO
  7726         IF STK-IND = 1
  7727             PERFORM BC245
  7728             PERFORM DB000
  7729             GO TO BC246
  7730         END-IF
  7731         GO TO BC245.
  7732*
  7733*    ****    W A R E H O U S E   S T O C K
  7734*
  7735     IF NOT (W40-CODE = SPACES)
  7736         SUBTRACT W10-QNT  FROM WST-QUANT
  7737     ELSE
  7738         SUBTRACT W10-QNT  FROM STK-QUANT
  7739         IF STK-USE-CASES = "Y"
  7740             SUBTRACT SAL-CASES
  7741                           FROM STK-CASES.
  7742     IF STK-IND = 1
  7743         GO TO BC245.
  7744     IF STK-DISC > ZERO
  7745         GO TO BC245.
  7746*
  7747*    ****    C H E C K   R E - O R D E R   L E V E L   A N D
  7748*            R E P O R T   L O W - L E V E L S
  7749*
  7750     IF W40-CODE = SPACES
  7751     IF STK-QUANT NOT > STK-LEVEL
  7752         MOVE 1            TO STK-DISC
  7753         PERFORM BA80.
  7754 BC245.
  7755       ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
  7756       ADD W10-QNT         TO STK-MTD STK-YTD.
  7757       ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
  7758       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
  7759*
  7760*    ****    W A R E H O U S E   S T O C K
  7761*
  7762     IF NOT (W40-CODE = SPACES) AND (STK-IND < 2)
  7763         ADD W10-QUANT     TO WST-MTD WST-YTD
  7764         ADD W90-VALUE     TO WST-MTDV WST-YTDV
  7765         PERFORM REWRITE-WSTOCK THRU WRITE-WSTOCK-EXIT.
  7766*
  7767*    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
  7768*            A U D I T   F I L E
  7769*
  7770       MOVE W90-BCR        TO W90-ICST.
  7771       PERFORM BA100.
  7772       ADD 1               TO DOC-SEQ.
  7773       MOVE ZERO           TO DOC-SUB.
  7774       PERFORM BC225.
  7775*
  7776*    ****    G O   A N D   G E T   T H E   N E X T   I T E M
  7777*
  7778 BC246.
  7779       ADD 1               TO WS-SALKEY.
  7780       GO TO BC235.
  7781
  7782 BC247.
  7783       MOVE 3              TO DOC-TYPE.
  7784       INITIALIZE DOC-DET2.
  7785*
  7786*     C H E C K   F O R   S E R I A L   N U M B E R S
  7787*
  7788     IF SAL-SUB NOT = ZERO
  7789         MOVE 1            TO WS-S1
  7790         PERFORM BC248
  7791         MOVE SAL-MSER     TO INV-DESC DOC-MSER
  7792         ADD 1             TO DOC-SUB
  7793     ELSE
  7794         MOVE ZERO         TO DOC-SUB
  7795         ADD 1             TO DOC-SEQ
  7796         MOVE SAL-DESC     TO INV-RMKS DOC-DESC.
  7797     IF WS-PRNINV = "Y"
  7798         WRITE INV-L1 BEFORE 1 LINES.
  7799       MOVE SPACES         TO REP-DETAIL2.
  7800       PERFORM BC225.
  7801       GO TO BC246.
  7802
  7803 BC248.
  7804       MOVE SAL-SNO (WS-S1)
  7805                           TO W10-SERNO.
  7806       MOVE ZERO           TO WS-GAR.
  7807       PERFORM BB070.
  7808       ADD 1               TO WS-S1.
  7809     IF WS-S1 < 3
  7810         IF SAL-SNO (WS-S1) NOT = SPACES
  7811             GO TO BC248.
  7812*
  7813*    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
  7814*
  7815 BC250.
  7816     IF WS-PRNINV = "Y"
  7817         WRITE INV-L1 BEFORE 1 LINES
  7818         MOVE SPACES       TO REP-DETAIL2
  7819         MOVE "C/FWD"      TO INV-FWD
  7820         MOVE W90-STOTAL   TO INV-VAL
  7821         WRITE INV-L1 BEFORE 14 LINES.
  7822       PERFORM BB050.
  7823       PERFORM BC215 THRU BC220.
  7824	   IF WS-PRNINV = "Y"
  7825         MOVE "B/FWD"      TO INV-DESC
  7826         MOVE W90-STOTAL   TO INV-VAL
  7827         WRITE INV-L1 BEFORE 1 LINES
  7828         MOVE SPACES       TO REP-DETAIL2
  7829         SUBTRACT 2        FROM WS-LINE.
  7830 BC255.
  7831*
  7832*    ****    I N V O I C E   T O T A L S
  7833*
  7834     IF WS-PRNINV = "N"
  7835         GO TO BC256.
  7836       WRITE INV-L1 BEFORE WS-LINE LINES.
  7837       MOVE SPACES         TO REP-DETAIL2.
  7838       WRITE INV-L1 BEFORE 1 LINES.
  7839
  7840 BC256.
  7841     IF NOT (W40-PADD1 = SPACES)
  7842         MOVE 5            TO DOC-TYPE
  7843         MOVE 99800        TO DOC-SQN
  7844         INITIALIZE DOC-DET5
  7845         MOVE W40-PADD1    TO DOC-AD1
  7846         MOVE W40-PADD2    TO DOC-AD2
  7847         MOVE W40-PADD3    TO DOC-AD3
  7848         MOVE W40-PADD4    TO DOC-AD4
  7849         MOVE W40-PC2      TO DOC-PC
  7850         PERFORM BC225.
  7851       MOVE 9              TO DOC-TYPE.
  7852       MOVE 99900          TO DOC-SQN.
  7853       INITIALIZE DOC-DET3.
  7854       MOVE W90-DB         TO DOC-SUBTOT.
  7855*
  7856*    ****    U P D A T E   S U N D R Y   D I S C O U N T   T O T A L S
  7857*
  7858     IF W90-TDISC NOT = ZERO
  7859         MULTIPLY -1 BY W90-TDISC
  7860                           GIVING DOC-DSCNT
  7861         IF W90-LDISC = ZERO
  7862             IF WS-COPY = "N"
  7863                 MOVE "YYYY"
  7864                           TO DPT-CODE
  7865                 PERFORM AY38 THRU AY59
  7866                 ADD W90-TDISC
  7867                           TO DPT-DISC-DAY
  7868                              DPT-DISC-MTD
  7869                              DPT-DISC-YTD
  7870                 IF WX-INT = "Y"
  7871                     MOVE W90-EXCDIS   TO WX-VALUE
  7872                     MOVE DPT-DISC-GL  TO WX-AC
  7873                     MOVE DPT-CODE     TO WX-LEDG
  7874                     MOVE 15           TO WX-TYPE
  7875                     PERFORM GL-TRF-UPDATE
  7876                 END-IF
  7877                 PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  7878*
  7879*    ****    P R I N T   T H E   T A X   F I G U R E S   A N D
  7880*            U P D A T E   T A X   T O T A L S
  7881*
  7882	   IF (W90-TAX NOT = SPACES) OR
  7883        (W90-EXP = "X")
  7884         MOVE SPACES       TO INV-FWD
  7885     ELSE
  7886     IF W05-VATNO NOT = SPACES
  7887         MOVE "Vat inclusive"
  7888                           TO INV-FWD.
  7889*
  7890*            TAX CALCULATED IN DTPINV
  7891*                W90-NONTAX   =TOTAL NON TAXABLE VALUE
  7892*                W90-TTAX     =TOTAL TAX (ALL RATES)
  7893*                W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
  7894*                W90-VAT(?)   =TOTAL VAT PER RATE
  7895*                W90-TOTAL    =TOTAL FOR INV/CREDIT NOTE
  7896*
  7897*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  7898*
  7899       MOVE W05-RATE       TO WS-PER.
  7900     IF WS-COPY = "Y"
  7901         GO TO BC258.
  7902
  7903 BC258.
  7904       MOVE W90-TTAX       TO DOC-TAX.
  7905     IF W90-TAX NOT = SPACES
  7906         MOVE W90-TTAX     TO INV-VAL.
  7907
  7908 BC260.
  7909     IF WS-PRNINV = "Y"
  7910         WRITE INV-L1 BEFORE 2 LINES.
  7911       MOVE SPACES         TO REP-DETAIL2.
  7912       MOVE DEB-TERMS      TO DOC-TERMS.
  7913*
  7914*    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
  7915*
  7916       MOVE W90-TOTAL      TO W90-DB.
  7917     IF WS-COPY = "N"
  7918         MOVE W90-BDT      TO W90-TCOST
  7919         MOVE W90-DB       TO DOC-TOT
  7920         MOVE W90-EXCTOT   TO DOC-TAXABLE
  7921         MOVE W90-NONTAX   TO DOC-NON-TXBL
  7922         MOVE W90-BAL      TO DOC-BAL
  7923         MOVE W90-CONTACT  TO DOC-CONTACT
  7924         MOVE W40-TEL      TO DOC-TEL
  7925         PERFORM BC225.
  7926     IF WS-PRNINV = "Y"
  7927         MOVE W90-DB       TO INV-VAL
  7928         WRITE INV-L1 BEFORE 11 LINES.
  7929       MOVE SPACES         TO REP-DETAIL2.
  7930       MOVE W12-T-YMD      TO W20-DTE.
  7931*
  7932*    ****    U P D A T E   T H E   D E B T O R   A C C O U N T
  7933*
  7934       MOVE 1              TO W90-CODE.
  7935       PERFORM GET-DEBTOR-TRAN-RECORD.
  7936       PERFORM GENERATE-DEBTOR-TRAN.
  7937       MOVE 9              TO WS-PARKEY.
  7938       PERFORM AY01 THRU AY59.
  7939       MOVE 1              TO W05-V-RATE.
  7940
  7941 BC262-VAT.
  7942     IF W90-VAT(W05-V-RATE) = ZERO
  7943         GO TO BC262-CHECK-END.
  7944       MOVE W05-VAT-CODE   TO DPT-CODE.
  7945       PERFORM AY38 THRU AY59.
  7946*
  7947*    ****    V A T   S T A T I S T I C S
  7948*
  7949       ADD W90-VAT(W05-V-RATE)
  7950                           TO DPT-O-VAT-DAY
  7951                              DPT-O-VAT-MTD
  7952                              DPT-O-VAT-YTD.
  7953       ADD W90-TAXVAL(W05-V-RATE)
  7954                           TO DPT-V-SALES-DAY
  7955                              DPT-V-SALES-MTD
  7956                              DPT-V-SALES-YTD.
  7957     IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
  7958          COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
  7959          MOVE DPT-O-VAT-GL
  7960                           TO WX-AC
  7961          MOVE DPT-CODE    TO WX-LEDG
  7962          MOVE 10          TO WX-TYPE
  7963          PERFORM GL-TRF-UPDATE.
  7964       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
  7965       ADD W90-TAXVAL(W05-V-RATE)
  7966                           TO PAR-DVAT-DAY
  7967                              PAR-DVAT-MTD
  7968                              PAR-DVAT-YTD.
  7969
  7970 BC262-CHECK-END.
  7971     IF W05-V-RATE < 6
  7972         ADD 1             TO W05-V-RATE
  7973         GO TO BC262-VAT.
  7974       ADD W90-NONTAX      TO PAR-DNGS-DAY
  7975                              PAR-DNGS-MTD
  7976                              PAR-DNGS-YTD.
  7977       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  7978       MOVE ZERO           TO W90-CUR W90-30 W90-60
  7979                              W90-120 W90-90.
  7980*
  7981*    ****    L O G   T H E   S A L E S M A N   &   I N V O I C E -
  7982*            A U D I T   F I L E   D E T A I L S
  7983*
  7984       PERFORM BA90.
  7985       PERFORM BA95.
  7986*
  7987*    ****    P R I N T   T H E   N E X T   I N V O I C E
  7988*            H E A D I N G
  7989*            ( OVERLAP WITH DISK PROCESSING )
  7990*
  7991       PERFORM BB050.
  7992*
  7993*    ****    U P D A T E   T H E   D E B T O R   C A T E G O R Y
  7994*
  7995       ADD 121 DEB-CAT     GIVING W90-RESULT.
  7996       DIVIDE W90-RESULT BY 2
  7997                           GIVING W90-CATKEY
  7998				 REMAINDER W90-CS.
  7999       ADD 1               TO W90-CS.
  8000       MOVE W90-CATKEY     TO WS-PARKEY.
  8001       PERFORM AY01 THRU AY59.
  8002       ADD W90-DB          TO PAR-CDRMTD (W90-CS).
  8003       ADD W90-DB          TO PAR-CDRYTD (W90-CS).
  8004       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8005*
  8006*    ****    U P D A T E   T H E   S A L E S M A N   R E C O R D
  8007*
  8008     IF W10-SMAN NOT = ZERO
  8009         ADD 100 W10-SMAN  GIVING WS-PARKEY
  8010         PERFORM AY01 THRU AY59
  8011         ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
  8012         ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
  8013         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8014*
  8015*    ****    R E W R I T E   T H E   D E B T O R   R E C O R D
  8016*
  8017       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
  8018*
  8019*    ****    U P D A T E   T H E   D E B T O R   C O N T R O L
  8020*            R E C O R D
  8021*
  8022       ADD W90-DB          TO W70-OUT
  8023                              W70-DT.
  8024       ADD W90-CUR         TO W70-MTD.
  8025       ADD W90-30          TO W70-ONE.
  8026       ADD W90-60          TO W70-TWO.
  8027       ADD W90-90          TO W70-THREE.
  8028       ADD W90-120         TO W70-FOUR.
  8029*
  8030*    ****    U P D A T E   T H E   S T O C K   C O N T R O L
  8031*            R E C O R D
  8032*
  8033 BC265.
  8034       SUBTRACT W90-BAL    FROM STK-TOTAL.
  8035       ADD W90-SALES       TO STK-SMTD STK-SYTD.
  8036 BC266.
  8037*
  8038*    ****    U P D A T E   T H E   C O S T - O F - S A L E S
  8039*
  8040       MOVE 8              TO WS-PARKEY.
  8041       PERFORM AY01 THRU AY59.
  8042       ADD W90-BDT         TO PAR-CSHT PAR-CMTD PAR-CYTD.
  8043       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
  8044 BC267.
  8045*
  8046*    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
  8047*
  8048       PERFORM AY70 THRU AY999.
  8049*
  8050*    ****    C L O S E   F I L E S
  8051*            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
  8052*
  8053       CLOSE PARAM
  8054             DEPART
  8055             DEBTOR
  8056		   AUDIT
  8057             STOCK
  8058             INVOICE.
  8059       OPEN I-O PARAM
  8060                DEPART
  8061                DEBTOR
  8062                AUDIT
  8063                STOCK
  8064                INVOICE.
  8065       DISPLAY CLEAR-L25.
  8066 BC599.
  8067       EXIT.
  8068
  8069/
  8070*    ****    I N V O I C E   R E - P R I N T
  8071*
  8072 BE000         SECTION 51.
  8073 BE00.
  8074       MOVE 1              TO WS-FIRST.
  8075*
  8076*    ****    D E B T O R   A C C O U N T   N U M B E R
  8077*
  8078       MOVE W40-ACNO       TO W80-ACNO.
  8079       PERFORM CA155-GET-DEBTOR.
  8080       MOVE W40-SMAN       TO W10-SMAN.
  8081       MOVE W40-LAST       TO WS-LAST.
  8082       MOVE W40-PRC        TO WS-PRC.
  8083       MOVE W40-CR         TO WS-CR.
  8084       MOVE W40-DTE        TO W20-DTE.
  8085       DISPLAY "Print Document Heading " AT 2502
  8086                WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
  8087                "Y" WITH BACKGROUND-COLOR 3
  8088                         FOREGROUND-COLOR 6 HIGHLIGHT "/"
  8089                "N" WITH BACKGROUND-COLOR 3
  8090                         FOREGROUND-COLOR 6 HIGHLIGHT " > <".
  8091
  8092 BE38.
  8093       ACCEPT WS-OPTION AT 2530
  8094              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
  8095       CALL "CBL_TOUPPER" USING WS-OPTION
  8096                          BY VALUE WS-LENGTH
  8097                          RETURNING WS-STATUS.
  8098     IF NOT (WS-OPTION = "Y" OR "N")
  8099         GO TO BE38.
  8100     IF WS-OPTION = "Y"
  8101         PERFORM BB050.
  8102       DISPLAY CLEAR-L25.
  8103       DISPLAY "Re-printing Document" AT 2520 WITH
  8104                BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
  8105     IF W90-EXP = "E" OR "X"
  8106         MOVE W05-VATNO    TO W90-TAX
  8107     ELSE
  8108         MOVE SPACES       TO W90-TAX.
  8109       MOVE 1              TO WS-SALKEY.
  8110*
  8111*    ****    P R I N T   I N V O I C E   H E A D I N G S
  8112*
  8113 BE65.
  8114       MOVE SPACES         TO REP-DETAIL2.
  8115     IF WS-CR = "C"
  8116         MOVE "CREDIT NOTE"
  8117                           TO INV-TYPE
  8118     ELSE
  8119         MOVE "TAX INVOICE"
  8120                           TO INV-TYPE.
  8121       WRITE INV-L1 BEFORE 2 LINES.
  8122       MOVE SPACES         TO REP-DETAIL2.
  8123       MOVE W40-NAME       TO INV-NAME.
  8124       WRITE INV-L1 BEFORE 1 LINES.
  8125       MOVE W40-ADD1       TO INV-NAME.
  8126     IF W90-TAX NOT = SPACES
  8127	       MOVE W90-TAX	 TO INV-GNO.
  8128       WRITE INV-L1 BEFORE 1 LINES.
  8129       MOVE SPACES         TO REP-DETAIL2.
  8130       MOVE W40-ADD2       TO INV-NAME.
  8131       WRITE INV-L1 BEFORE 1 LINES.
  8132       MOVE W40-ADD3       TO INV-NAME.
  8133       WRITE INV-L1 BEFORE 1 LINES.
  8134       MOVE W40-ADD4       TO INV-NAME.
  8135       WRITE INV-L1 BEFORE 1 LINES.
  8136
  8137 BE66.
  8138       MOVE SPACES         TO REP-DETAIL2.
  8139       MOVE W40-PC1        TO INV-PC1.
  8140       WRITE INV-L1 BEFORE 1 LINES.
  8141       MOVE SPACES         TO REP-DETAIL2.
  8142       MOVE WS-EXPP        TO INV-EXPAND.
  8143       MOVE "    COPY"     TO INV-COPY.
  8144       MOVE WS-ECAN        TO INV-CANEX.
  8145       WRITE INV-L1 BEFORE 0 LINES.
  8146       MOVE SPACES         TO REP-DETAIL2.
  8147       WRITE INV-L1 BEFORE 2 LINES.
  8148*
  8149*    ****    C L I E N T   V A T   N U M B E R
  8150*
  8151     IF NOT (W40-TAX = SPACES)
  8152         MOVE "Vat No"     TO INV-VATH
  8153         MOVE W40-TAX      TO INV-VATNO.
  8154       WRITE INV-L1 BEFORE 4 LINES.
  8155       MOVE SPACES         TO REP-DETAIL2.
  8156       MOVE W20-DTE        TO W22-DTE2.
  8157       MOVE W22-CC2        TO W22-CC3.
  8158       MOVE W22-YY2        TO W22-YY3.
  8159       MOVE W22-MM2        TO W22-MM3.
  8160       MOVE W22-DD2        TO W22-DD3.
  8161       MOVE W22-DTE3       TO INV-DTE.
  8162       MOVE "N/A"          TO INV-TIME.
  8163     IF W10-SMAN NOT = ZERO
  8164         MOVE W40-ASSIST   TO INV-SMAN.
  8165       MOVE W90-REF        TO INV-REF.
  8166       MOVE W80-ACNO       TO INV-AC.
  8167*
  8168*    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
  8169*
  8170     IF DEB-TERMS = "0"
  8171         MOVE " 7 D"       TO INV-TERMS
  8172     ELSE
  8173     IF DEB-TERMS = "2"
  8174         MOVE "60 D"       TO INV-TERMS
  8175     ELSE
  8176     IF DEB-TERMS = "1"
  8177         MOVE "30 D"       TO INV-TERMS
  8178     ELSE
  8179         MOVE "COD"        TO INV-TERMS.
  8180       MOVE W90-ORD        TO INV-ORD.
  8181       WRITE INV-L1 BEFORE 6 LINES.
  8182       MOVE SPACES         TO REP-DETAIL2.
  8183       MOVE 23             TO WS-LINE.
  8184
  8185 BE70.
  8186       EXIT.
  8187*
  8188*    ****    R E A D   T H E   S A L E   F I L E   A N D
  8189*            P R I N T   T H E   I N V O I C E
  8190*
  8191 BE75.
  8192     IF WS-SALKEY > WS-LAST
  8193         GO TO BE90.
  8194       PERFORM READ-SALE THRU READ-SALE-EXIT.
  8195     IF WS-LINE > 1
  8196         SUBTRACT 1        FROM WS-LINE
  8197     ELSE
  8198         PERFORM BE88.
  8199     IF SAL-EXT-ITEM = SPACES
  8200         GO TO BE86.
  8201       MOVE SAL-EXT-ITEM   TO STK-CODE.
  8202       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
  8203*
  8204*    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
  8205*            R E C O R D
  8206*
  8207       MOVE SAL-QNT        TO W10-QUANT.
  8208       MOVE SAL-COST       TO W10-COST.
  8209       MOVE SAL-SELL       TO W10-SELL.
  8210       MOVE SAL-VAL        TO W25-VALUE.
  8211       MOVE SAL-EXT-ITEM   TO INV-ITM.
  8212       MOVE SAL-DESC       TO INV-DESC.
  8213       MOVE SAL-QNT        TO INV-QNT.
  8214       MOVE SAL-SELL       TO INV-PRICE.
  8215       MOVE SAL-VAL        TO INV-VAL.
  8216       MOVE SAL-DISC       TO W90-CDISC W90-EXCDIS.
  8217       MOVE SAL-DSC        TO WS-DISC INV-DSC.
  8218       ADD W90-CDISC W25-VALUE
  8219                           GIVING W90-VALUE.
  8220       MOVE SAL-SUPER      TO WS-SUPER.
  8221       WRITE INV-L1 BEFORE 1 LINES.
  8222       MOVE SPACES         TO REP-DETAIL2.
  8223       ADD W25-VALUE       TO W90-STOTAL.
  8224*
  8225*    ****    H A S   L I N E   D I S C O U N T   B E E N
  8226*            A P P L I E D
  8227*
  8228     IF W90-CDISC = ZERO
  8229         GO TO BE80.
  8230     IF WS-LINE > 1
  8231         SUBTRACT 1        FROM WS-LINE
  8232     ELSE
  8233         PERFORM BE88.
  8234 BE80.
  8235*
  8236*    ****    G O   A N D   G E T   T H E   N E X T   I T E M
  8237*
  8238 BE84.
  8239       ADD 1               TO WS-SALKEY.
  8240       GO TO BE75.
  8241*
  8242*     C H E C K   F O R   S E R I A L   N U M B E R S
  8243*
  8244 BE86.
  8245     IF SAL-SUB NOT = ZERO
  8246         MOVE SAL-MSER     TO INV-DESC DOC-MSER
  8247     ELSE
  8248     IF SAL-TYPE = "R"
  8249         MOVE SAL-MSER     TO INV-RMKS
  8250     ELSE
  8251         MOVE SAL-DESC     TO INV-MES.
  8252       WRITE INV-L1 BEFORE 1 LINES.
  8253       MOVE SPACES         TO REP-DETAIL2.
  8254       GO TO BE84.
  8255 BE88.
  8256*
  8257*    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
  8258*
  8259       WRITE INV-L1 BEFORE 1 LINES.
  8260       MOVE SPACES         TO REP-DETAIL2.
  8261       MOVE "C/FWD"        TO INV-FWD.
  8262       MOVE W90-STOTAL     TO INV-VAL.
  8263       WRITE INV-L1 BEFORE 9 LINES.
  8264       PERFORM BB050.
  8265       PERFORM BE65 THRU BE70.
  8266       MOVE "B/FWD - O/B"  TO INV-DESC.
  8267       MOVE W90-STOTAL     TO INV-VAL.
  8268       WRITE INV-L1 BEFORE 1 LINES.
  8269       MOVE SPACES         TO REP-DETAIL2.
  8270       SUBTRACT 2          FROM WS-LINE.
  8271
  8272 BE90.
  8273*
  8274*    ****    I N V O I C E   T O T A L S
  8275*
  8276       WRITE INV-L1 BEFORE WS-LINE LINES.
  8277       MOVE SPACES         TO REP-DETAIL2.
  8278       WRITE INV-L1 BEFORE 1 LINES.
  8279*
  8280*    ****    U P D A T E   T H E   S U N D R Y   D I S C O U N T
  8281*            T O T A L S
  8282*
  8283     IF W90-TDISC NOT = ZERO
  8284         MULTIPLY -1 BY W90-TDISC
  8285                           GIVING DOC-DSCNT.
  8286*
  8287*    ****    P R I N T   T H E   T A X   F I G U R E S   A N D
  8288*            U P D A T E   T A X   T O T A L S
  8289*
  8290     IF W90-TAX NOT = SPACES
  8291         MOVE SPACES       TO INV-FWD
  8292     ELSE
  8293     IF W05-VATNO NOT = SPACES
  8294         MOVE "Vat inclusive"
  8295                           TO INV-FWD.
  8296     IF W90-TAX NOT = SPACES
  8297         MOVE W90-TTAX     TO INV-VAL.
  8298
  8299 BE95.
  8300       WRITE INV-L1 BEFORE 2 LINES.
  8301	     MOVE SPACES	 TO REP-DETAIL2.
  8302*
  8303*    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
  8304*
  8305       MOVE W90-TOTAL      TO W90-DB.
  8306       MOVE W90-BDT        TO W90-TCOST.
  8307       MOVE W90-DB         TO INV-VAL.
  8308       WRITE INV-L1 BEFORE 11 LINES.
  8309       MOVE SPACES         TO REP-DETAIL2.
  8310       MOVE W12-T-YMD      TO W20-DTE.
  8311*
  8312*    ****    P R I N T   T H E   N E X T   I N V O I C E
  8313*            H E A D I N G
  8314*            ( OVERLAP WITH DISK PROCESSING )
  8315*
  8316 BE96.
  8317       PERFORM BB050.
  8318
  8319 BE99.
  8320       EXIT.
  8321/
  8322 DB000                   SECTION 53.
  8323 DB00.
  8324       MOVE SAL-EXT-ITEM   TO PRT-EXT-ITEM.
  8325       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
  8326     IF WS-F-ERROR = 21
  8327         GO TO DB999.
  8328*
  8329*    ****  P R O D U C T I O N   D E T A I L S
  8330*
  8331       MOVE 1              TO WS-S6.
  8332 DB05.
  8333     IF PRT-EXT-CODE (WS-S6) = SPACES
  8334         ADD 1             TO WS-S6
  8335         GO TO DB15.
  8336       MOVE PRT-EXT-CODE (WS-S6)
  8337                           TO STK-CODE.
  8338       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
  8339     IF WS-F-ERROR = 22
  8340         ADD 1             TO WS-S6
  8341         GO TO DB15.
  8342       PERFORM AY05 THRU AY59.
  8343       MULTIPLY W10-QUANT BY PRT-QUANT(WS-S6)
  8344                   GIVING W10-PQUANT.
  8345       SUBTRACT W10-PQUANT FROM STK-QUANT.
  8346       ADD W10-PQUANT      TO STK-MTD STK-YTD.
  8347     IF STK-DISC > ZERO
  8348         GO TO DB10.
  8349*
  8350*    ****    C H E C K   R E - O R D E R   L E V E L   A N D
  8351*            R E P O R T   L O W - L E V E L S
  8352*
  8353     IF STK-QUANT NOT > STK-LEVEL
  8354         MOVE 1            TO STK-DISC
  8355         PERFORM BA80.
  8356
  8357 DB10.
  8358       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
  8359       ADD 1               TO WS-S6.
  8360
  8361 DB15.
  8362     IF WS-S6 < 31
  8363        GO TO DB05.
  8364
  8365 DB999.
  8366      EXIT.
  8367/
  8368 ZA000-INIT              SECTION 90.
  8369 ZA000-OPEN.
  8370       MOVE LS-COMPANY     TO WS-TOP-COMP.
  8371       MOVE LS-PARID       TO WS-PARID.
  8372       MOVE LS-L-OR-N      TO W02-L-OR-N.
  8373       MOVE WS-SYS-ID      TO W02-SYSID.
  8374       MOVE LS-TODAY-DDMMYY
  8375                           TO TODAY-DDMMYY W12-TODAY.
  8376       MOVE LS-USUB        TO WS-USUB.
  8377       MOVE LS-DRAW        TO WS-DRAW.
  8378*
  8379*    ****    P A R A M E T E R   F I L E
  8380*
  8381 ZA00A.
  8382       MOVE "PARAM"        TO AFID-KEY.
  8383
  8384 ZA00-READ-APACFIDS.
  8385       READ APACFIDS WITH IGNORE LOCK
  8386         KEY IS AFID-KEY.
  8387     IF WS-STATUS = "00"
  8388         GO TO ZA00-READ-APACFIDS-EXIT.
  8389       STRING "Missing " DELIMITED SIZE
  8390               AFID-KEY DELIMITED BY " "
  8391               " file ID - Status " DELIMITED SIZE
  8392               WS-STATUS DELIMITED SIZE
  8393               INTO WS-ERR-MES.
  8394       PERFORM ERROR-LENGTH THRU ERROR-EXIT.
  8395       STOP RUN.
  8396
  8397 ZA00-READ-APACFIDS-EXIT.
  8398       EXIT.
  8399
  8400 ZA00A-CONTINUE.
  8401       MOVE AFID-PATH      TO W02-PARAM.
  8402       MOVE "AUDITF"       TO AFID-KEY.
  8403       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8404       MOVE AFID-PATH      TO W02-AUDITF.
  8405       MOVE "CARDEX"       TO AFID-KEY.
  8406       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8407       MOVE AFID-PATH      TO W02-CARDEX.
  8408       MOVE "DBTRAN"       TO AFID-KEY.
  8409       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8410       MOVE AFID-PATH      TO W02-DBTRAN.
  8411       MOVE "DEBTOR"       TO AFID-KEY.
  8412       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8413       MOVE AFID-PATH      TO W02-DEBTOR.
  8414       MOVE "DEPART"       TO AFID-KEY.
  8415       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8416       MOVE AFID-PATH      TO W02-DEPART.
  8417       MOVE "GARTEE"       TO AFID-KEY.
  8418       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8419       MOVE AFID-PATH      TO W02-GARTEE.
  8420       MOVE "INVOIC"       TO AFID-KEY.
  8421       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8422       MOVE AFID-PATH      TO W02-INVOIC.
  8423       MOVE "LEDTRF"       TO AFID-KEY.
  8424       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8425	     MOVE AFID-PATH	 TO W02-LEDTRF.
  8426       MOVE "NETWORK"      TO AFID-KEY.
  8427       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8428       MOVE AFID-PATH      TO W02-NETWORK.
  8429       MOVE "RECOVER"      TO AFID-KEY.
  8430       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8431       INSPECT AFID-PATH REPLACING FIRST "XXX"
  8432                         BY LS-USER.
  8433       MOVE AFID-PATH      TO W02-RECOVER.
  8434       MOVE "SALE"         TO AFID-KEY.
  8435       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8436       INSPECT AFID-PATH REPLACING FIRST "XXX"
  8437                         BY LS-USER.
  8438       MOVE AFID-PATH      TO W02-SALE.
  8439       MOVE "SHARED"       TO AFID-KEY.
  8440       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8441       MOVE AFID-PATH      TO W02-SHARED.
  8442       MOVE "SPARTS"       TO AFID-KEY.
  8443       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8444       MOVE AFID-PATH      TO W02-SPARTS.
  8445       MOVE "STOCKF"       TO AFID-KEY.
  8446       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8447       MOVE AFID-PATH      TO W02-STOCKF.
  8448       MOVE "TXTRAN"       TO AFID-KEY.
  8449       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8450       MOVE AFID-PATH      TO W02-TXTRAN.
  8451       MOVE "WSTOCK"       TO AFID-KEY.
  8452       PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
  8453       MOVE AFID-PATH      TO W02-WSTOCK.
  8454       MOVE 3              TO WS-PARKEY.
  8455       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8456     IF NOT (PAR-STOCK = "A" OR "N")
  8457         MOVE "A"          TO PAR-STOCK.
  8458       MOVE PAR-STOCK      TO W10-STCK.
  8459       MOVE PAR-VAT-REG    TO W05-VATNO.
  8460       MOVE PAR-CRDX       TO WS-CARDEX.
  8461       MOVE PAR-PRICED-IND TO WS-PRC-IND.
  8462     IF LS-USER NUMERIC
  8463         IF LS-USE > ZERO AND < 110
  8464             MOVE LS-USE   TO WS-USUB
  8465             GO TO ZA01A.
  8466       MOVE 110            TO WS-USUB.
  8467 ZA01A.
  8468       ADD 2549 WS-USUB    GIVING W25-RESULT.
  8469       MOVE W25-KEY        TO WS-PARKEY.
  8470       ADD 1 W25-SUB       GIVING WS-S6.
  8471       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8472     IF TRM-INVP (WS-S6) = 1
  8473         MOVE "LPT1"       TO W02-PRINTER
  8474     ELSE
  8475     IF TRM-INVP (WS-S6) = 2
  8476         MOVE "LPT2"       TO W02-PRINTER
  8477     ELSE
  8478     IF TRM-INVP (WS-S6) = 3
  8479         MOVE "LPT3"       TO W02-PRINTER
  8480     ELSE
  8481     IF TRM-INVP (WS-S6) = 4
  8482         MOVE "COM1"       TO W02-PRINTER
  8483	   ELSE
  8484     IF TRM-INVP (WS-S6) = 5
  8485         MOVE "COM2"       TO W02-PRINTER
  8486     ELSE
  8487     IF TRM-INVP (WS-S6) = 9
  8488         MOVE LS-USER      TO W02-USER.
  8489       ADD 501 TRM-PRN2 (WS-S6)
  8490                           GIVING W25-RESULT.
  8491       DIVIDE W25-RESULT BY 2
  8492                           GIVING WS-PARKEY
  8493                           REMAINDER WS-S6.
  8494       ADD 1               TO WS-S6.
  8495       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8496       MOVE TRM-CONP (WS-S6)
  8497                           TO WS-COND.
  8498       MOVE TRM-NORP (WS-S6)
  8499                           TO WS-NORM.
  8500       MOVE TRM-EXPP (WS-S6)
  8501                           TO WS-EXPP.
  8502       MOVE TRM-ECAN (WS-S6)
  8503                           TO WS-ECAN.
  8504       MOVE TRM-8LPI (WS-S6)
  8505                           TO WS-8LPI.
  8506       MOVE TRM-6LPI (WS-S6)
  8507                           TO WS-6LPI.
  8508       MOVE TRM-10CPI (WS-S6)
  8509                           TO WS-10CPI.
  8510       MOVE TRM-12CPI (WS-S6)
  8511                           TO WS-12CPI.
  8512       MOVE TRM-17CPI (WS-S6)
  8513                           TO WS-17CPI.
  8514 ZA01B.
  8515       OPEN OUTPUT RECOVER.
  8516       CLOSE RECOVER.
  8517       OPEN I-O RECOVER.
  8518       DISPLAY "Waiting for printer response" AT 2525
  8519                WITH BACKGROUND-COLOR 3
  8520                     FOREGROUND-COLOR 14 BLINK.
  8521       OPEN OUTPUT PRNREP.
  8522       CALL X"91" USING X91-RES X91-FUN PRNREP.
  8523       DISPLAY SPACES AT 2525
  8524               WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
  8525       MOVE WS-12CPI       TO REP-DETAIL2.
  8526       WRITE REP-LINE2 BEFORE 0 LINES.
  8527       MOVE WS-8LPI        TO REP-DETAIL2.
  8528       WRITE REP-LINE2 BEFORE 0 LINES.
  8529       MOVE SPACES         TO REP-DETAIL2.
  8530       MOVE 1              TO WS-PARKEY.
  8531       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8532       MOVE PAR-DMY        TO W12-TODAY.
  8533       MOVE PAR-YMD        TO W12-T-YMD.
  8534       MOVE PAR-COMPANY    TO W95-COMP.
  8535       MOVE PAR-TELEPHONE  TO W95-TEL.
  8536     IF PAR-STMINV = "I" OR "X"
  8537         MOVE "N"          TO WS-HEAD
  8538     ELSE
  8539         MOVE "Y"          TO WS-HEAD.
  8540       ADD 1               TO WS-PARKEY.
  8541	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8542       MOVE PAR-ADDRESS1   TO W95-ADD1.
  8543       MOVE PAR-ADDRESS2   TO W95-ADD2.
  8544       MOVE PAR-ADDRESS3   TO W95-ADD3.
  8545       MOVE PAR-ADDRESS4   TO W95-ADD4.
  8546       MOVE PAR-POST-CDE   TO W95-POST.
  8547     IF W95-ADD4 = SPACES
  8548         MOVE W95-PC       TO W95-ADD4
  8549         MOVE SPACES       TO W95-PC.
  8550     IF W95-ADD3 = SPACES
  8551         MOVE W95-ADD4     TO W95-ADD3
  8552         MOVE SPACES       TO W95-ADD4.
  8553     IF W95-ADD2 = SPACES
  8554         MOVE W95-ADD3     TO W95-ADD2
  8555         MOVE SPACES       TO W95-ADD3.
  8556     IF W95-ADD1 = SPACES
  8557         MOVE W95-ADD2     TO W95-ADD1
  8558         MOVE SPACES       TO W95-ADD2.
  8559       MOVE 5              TO WS-PARKEY.
  8560       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8561       MOVE PAR-DISCOUNT   TO W15-DISCOUNT.
  8562       MOVE 1              TO W05-V-RATE.
  8563
  8564 ZA05-VAT.
  8565       MOVE W05-VAT-CODE   TO DPT-CODE.
  8566       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
  8567       MOVE DPT-R-DATE     TO WS-VAT-DATE(W05-V-RATE).
  8568       MOVE DPT-RATE       TO W05-VAT(W05-V-RATE).
  8569       ADD 6 W05-V-RATE    GIVING WS-S1.
  8570       MOVE DPT-P-RATE     TO W05-VAT(WS-S1).
  8571     IF W05-V-RATE < 6
  8572         ADD 1             TO W05-V-RATE
  8573         GO TO ZA05-VAT.
  8574       MOVE 6              TO WS-PARKEY.
  8575       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
  8576     IF PAR-INTEGRATE NOT = "Y"
  8577         MOVE "N"          TO WX-INT
  8578     ELSE
  8579         MOVE "Y"          TO WX-INT.
  8580       MOVE PAR-DEBGL      TO WX-DEBGL.
  8581       MOVE PAR-CREGL      TO WX-CREGL.
  8582       MOVE PAR-INTGL      TO WX-INTGL.
  8583       MOVE PAR-BNKGL      TO WX-BNKGL.
  8584       MOVE PAR-UNPROF     TO WX-UNPROF.
  8585       MOVE PAR-REDGL      TO WX-REDGL.
  8586       MOVE PAR-ADJGL      TO WX-ADJGL.
  8587       MOVE PAR-RLGL       TO WX-RLGL.
  8588       MOVE PAR-DSGL       TO WX-DSGL.
  8589       ACCEPT W25-TIME FROM TIME.
  8590       MOVE W25-HH         TO W95-HH.
  8591       MOVE W25-MM         TO W95-MM.
  8592       MOVE W25-SS         TO W95-SS.
  8593       GO TO ZA999.
  8594 ZA49.
  8595       DISPLAY "Too many files OPEN" AT 0812
  8596                WITH FOREGROUND-COLOR 14.
  8597       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
  8598       DISPLAY "Press any key to continue" AT 1212.
  8599	     ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
  8600       GO TO ZA205.
  8601 ZA200.
  8602       DISPLAY "Files locked - Try later" AT 2312
  8603                WITH FOREGROUND-COLOR 14
  8604               " " WS-STATUS WITH FOREGROUND-COLOR 15.
  8605       DISPLAY "Press any key" AT 2512
  8606                WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
  8607       ACCEPT WS-OPTION AT 2526
  8608              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
  8609 ZA205.
  8610       EXIT PROGRAM.
  8611 ZA999.
  8612       EXIT.
  8613
  8614 I-O-ERRORS      SECTION  91.
  8615 OPEN-ERROR.
  8616       DISPLAY CLR-SCREEN.
  8617       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
  8618       PERFORM DISPLAY-FILE-NAME.
  8619
  8620 READ-ERROR.
  8621       DISPLAY CLR-SCREEN.
  8622       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
  8623       PERFORM DISPLAY-FILE-NAME.
  8624
  8625 WRITE-ERROR.
  8626       DISPLAY CLR-SCREEN.
  8627       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
  8628       PERFORM DISPLAY-FILE-NAME.
  8629
  8630
  8631 DISPLAY-FILE-NAME.
  8632     IF WS-F-ERROR = 1
  8633         MOVE W02-AUDITF   TO WS-FILE
  8634         MOVE WS-AUDKEY    TO WS-KEY
  8635     ELSE
  8636     IF WS-F-ERROR = 2
  8637         MOVE W02-NETWORK  TO WS-FILE
  8638         MOVE WS-NETKEY    TO WS-KEY
  8639     ELSE
  8640     IF WS-F-ERROR = 5
  8641         MOVE W02-DBTRAN   TO WS-FILE
  8642         MOVE ZERO         TO WS-KEY
  8643         MOVE TRA-KEY      TO WS-KEYX
  8644     ELSE
  8645     IF WS-F-ERROR = 6
  8646         MOVE W02-DEBTOR   TO WS-FILE
  8647         MOVE ZERO         TO WS-KEY
  8648         MOVE DEB-ACNO     TO WS-KEYX
  8649     ELSE
  8650     IF WS-F-ERROR = 7
  8651         MOVE W02-DEPART   TO WS-FILE
  8652         MOVE ZERO         TO WS-KEY
  8653         MOVE DPT-CODE     TO WS-KEYX
  8654     ELSE
  8655     IF WS-F-ERROR = 9
  8656         MOVE W02-GARTEE   TO WS-FILE
  8657	       MOVE ZERO	 TO WS-KEY
  8658         MOVE GUA-KEY      TO WS-KEYX
  8659     ELSE
  8660     IF WS-F-ERROR = 12
  8661         MOVE W02-INVOIC   TO WS-FILE
  8662         MOVE ZERO         TO WS-KEY
  8663         MOVE DOC-KEY      TO WS-KEYX
  8664     ELSE
  8665     IF WS-F-ERROR = 15
  8666         MOVE WS-PARID     TO WS-FILE
  8667         MOVE WS-PARKEY    TO WS-KEY
  8668     ELSE
  8669     IF WS-F-ERROR = 18
  8670         MOVE W02-RECOVER  TO WS-FILE
  8671         MOVE WS-RECKEY    TO WS-KEY
  8672     ELSE
  8673     IF WS-F-ERROR = 19
  8674         MOVE W02-SALE     TO WS-FILE
  8675         MOVE WS-SALKEY    TO WS-KEY
  8676     ELSE
  8677     IF WS-F-ERROR = 21
  8678         MOVE W02-SPARTS   TO WS-FILE
  8679         MOVE ZERO         TO WS-KEY
  8680         MOVE PRT-EXT-ITEM TO WS-KEYX
  8681     ELSE
  8682     IF WS-F-ERROR = 22
  8683         MOVE W02-STOCKF   TO WS-FILE
  8684         MOVE ZERO         TO WS-KEY
  8685         MOVE STK-CODE     TO WS-KEYX
  8686     ELSE
  8687     IF WS-F-ERROR = 32
  8688         MOVE W02-TXTRAN   TO WS-FILE
  8689         MOVE ZERO         TO WS-KEY
  8690         MOVE TAX-KEY      TO WS-KEYX
  8691     ELSE
  8692	   IF WS-F-ERROR = 37
  8693         MOVE W02-SHARED   TO WS-FILE
  8694         MOVE WS-SHARED    TO WS-KEY
  8695     ELSE
  8696     IF WS-F-ERROR = 40
  8697         MOVE W02-LEDTRF   TO WS-FILE
  8698         MOVE ZERO         TO WS-KEY
  8699         MOVE XFR-KEY      TO WS-KEYX
  8700     ELSE
  8701     IF WS-F-ERROR = 43
  8702         MOVE W02-CARDEX   TO WS-FILE
  8703         MOVE ZERO         TO WS-KEY
  8704         MOVE CRD-KEY      TO WS-KEYX
  8705     ELSE
  8706     IF WS-F-ERROR = 52
  8707         MOVE W02-WSTOCK   TO WS-FILE
  8708         MOVE ZERO         TO WS-KEY
  8709         MOVE WST-KEY      TO WS-KEYX.
  8710     IF WS-STATUS = "10"
  8711         MOVE "End of FILE" TO WS-STAT-MESSAGE
  8712     ELSE
  8713     IF WS-STATUS = "22"
  8714         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
  8715	   ELSE
  8716     IF WS-STATUS = "23"
  8717         MOVE "Invalid record number" TO WS-STAT-MESSAGE
  8718     ELSE
  8719     IF WS-STATUS = "24"
  8720         MOVE "DISK full" TO WS-STAT-MESSAGE
  8721     ELSE
  8722     IF WS-STATUS = "30"
  8723         MOVE "DISK error" TO WS-STAT-MESSAGE
  8724     ELSE
  8725     IF WS-STATUS = "94"
  8726         MOVE "FILE locked" TO WS-STAT-MESSAGE.
  8727       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
  8728       DISPLAY "Status " AT 1212
  8729                WS-STATUS WITH FOREGROUND-COLOR 11
  8730               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
  8731     IF WS-STATUS NOT = "94"
  8732         DISPLAY "Key    " AT 1412
  8733                  WS-KEY WITH FOREGROUND-COLOR 11
  8734         DISPLAY "Reverse backup or contact program Support"
  8735                  AT 1612.
  8736         DISPLAY "Please make a note of these details" AT 1812.
  8737       STOP RUN.
