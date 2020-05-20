       01  LS-USER-ID.
           03  LS-USER.
               05  LS-USE          PIC  9(03).
           03  LS-COMPANY.
               05  LS-COMPCHR      PIC  X(01)    OCCURS 40.
           03  LS-TERMINAL         PIC  X(14).
           03  LS-INSTR            PIC  X(01).
           03  LS-STOCK-CODE.
               05  LS-NEXT-PRG.
                   07  LS-ACNO     PIC  X(06).
                   07  FILLER      PIC  X(12).
           03  LS-DAYS             PIC  9(02)    COMP-5.
           03  LS-DEM              PIC  9(02)    COMP-5.
           03  LS-USUB             PIC  9(04)    COMP-5.
           03  LS-DRAW             PIC  9(02)    VALUE 40.
           03  LS-CSS              PIC  X(01).
           03  LS-CLR              PIC  X(01)    REDEFINES LS-CSS.
           03  LS-OK               PIC  X(01).
           03  LS-RET              PIC  X(01).
           03  LS-TODAY-DDMMYY     PIC  9(08)    COMP-5.
           03  LS-TODAY-YYMMDD     PIC  9(08)    COMP-5.
      *    03  LS-CUR-YR           PIC  X(01)    COMP-X.
      *    03  LS-CUR-CEN          PIC  X(01)    COMP-X.
      *    03  LS-PRV-CEN          PIC  X(01)    COMP-X.
      *    03  LS-NXT-CEN          PIC  X(01)    COMP-X.
           03  LS-SLS-ORDNO.
               05  LS-SLS-JOBNO    PIC  X(08)    VALUE SPACES.
           03  LS-CARDEX           PIC  X(01).
           03  LS-DSKDRV           PIC  X(01)    VALUE "N".
           03  APAC-SYSTEM         PIC  X(12).
           03  APAC-VERSION        PIC  X(04).
           03  LS-PRG-NAME         PIC  X(08)    VALUE "APAC-POS".
           03  LS-L-OR-N           PIC  X(08)    VALUE "SYSTEMS\".

       01  LS-PARID.
           03  LS-SYS-ID           PIC  X(03).

       01  LS0-PROGRAMS.
           03  LS0-MODULES.
               05  LS0-DTP         PIC  9(01).
               05  LS0-STP         PIC  9(01).
               05  LS0-HPD         PIC  9(01).
               05  LS0-CRP         PIC  9(01).
               05  LS0-GLP         PIC  9(01).
               05  LS0-POS         PIC  9(01).
               05  LS0-JCP         PIC  9(01).
               05  LS0-VHP         PIC  9(01).
               05  LS0-WAG         PIC  9(01).
               05  LS0-SAL         PIC  9(01).
               05  LS0-LEV         PIC  9(01).
               05  LS0-WHS         PIC  9(01).
               05  LS0-RTE         PIC  9(01).
               05  LS0-TEMP.
                   07  LS0-DBT     PIC  X(06).
                   07  LS0-DSK     PIC  X(01).

       01  LS0-SECURITY.
           03  LS0-NO              PIC  9(03).
           03  LS0-STATION.
               05  LS-TERM         PIC  9(03).
           03  LS0-NAME            PIC  X(30).
           03  LS0-PASS            PIC  X(08).
           03  LS0-DATE            PIC  9(08)    COMP-3.
           03  LS0-INSTR           PIC  X(01).
           03  LS0-GL              PIC  X(01).
           03  LS0-GLLEV           PIC  9(01).
           03  LS0-STOCK           PIC  X(01).
           03  LS0-STLEV           PIC  9(01).
           03  LS0-DEBT            PIC  X(01).
           03  LS0-DBLEV           PIC  9(01).
           03  LS0-CRED            PIC  X(01).
           03  LS0-CRLEV           PIC  9(01).
           03  LS0-PARM            PIC  X(01).
           03  LS0-PALEV           PIC  9(01).
           03  LS0-SALES           PIC  X(01).
           03  LS0-SALEV           PIC  9(01).
           03  LS0-JOBS            PIC  X(01).
           03  LS0-JBLEV           PIC  9(01).
           03  LS0-WAGES           PIC  X(01).
           03  LS0-WGLEV           PIC  9(01).
           03  LS0-ADM             PIC  X(01).
           03  LS0-DIV             PIC  9(03).
           03  LS0-SP              PIC  9(02).
           03  LS0-ORD             PIC  9(06)    COMP-5.
           03  LS0-SEQ             PIC  9(01).
           03  LSO-RECV            PIC  X(02).
