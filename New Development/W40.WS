       01  W40-BINARY.
           03  W40-BIN.
               05  W40-DEC         PIC  9(02)    COMP-X.
           03  W40-HEX                           REDEFINES W40-BIN.
               05  W40-CHAR        PIC  X(01).
           03  W40-N1              PIC  9(02)    COMP-X.
           03  W40-N2              PIC  9(02)    COMP-X.
           03  W40-SWITCH          PIC  9(02)    COMP-X.
           03  W40-CSHPASS         PIC  X(06)            VALUE SPACES.

       01  W41-SAVE.
           03  W41-LENGTH          PIC  9(04)    COMP-X  VALUE 4000.
           03  W41-START           PIC  9(04)    COMP-X  VALUE 1.
           03  W41-BUFFER          PIC  9(04)    COMP-X  VALUE 1.

       01  W42-ATTRIB.
           03  FILLER              PIC X(4000).

       01  W42-ATTRIB2.
           03  W42-LINES OCCURS 50.
               05  W42-BYTE                      OCCURS 80.
                   07  W42-CHAR    PIC  9(02)    COMP-X.

       01  W42-ATTRIB3.
           03  FILLER              PIC  X(4000).

       01  W42-ATTRIB4.
           03  W42-LINE OCCURS 50.
               05  W42-BTE                       OCCURS 80.
                   07  W42-CHR     PIC  9(02)    COMP-X.

       01  W43-SCREEN.
           03  FILLER              PIC  X(4000).

       01  W43-SCREEN2.
           03  W43-SCREEN2A.
               05  W43-LINE                      OCCURS 50.
      *
      *    ****   U S E D   I N   L O O K U P   R O U T I N E S
      *                 C R E D I T O R S
      *                 D E B T O R S
      *                 G E N E R A L   L E D G E R
      *                 G U A R A N T E E
      *                 J O B   C O S T I N G
      *                 J O B   R E M A R K S
      *                 S A L E S O R D E R
      *                 S T O C K
      *                 W A R E H O U S E
      *
      *    ****   U S E D   I N   A L L O C A T I O N   O F
      *              C R E D I T S   -   D E B T O R S
      *
                   07  W43-DET.
                       09  FILLER  PIC  X(09).
      *
      *    ****   Used in Lookup routines - PURCHASE ORDER
      *
                       09  W43-DET2.
                           11  FILLER PIC  X(16).
      *
      *    ****   Used in Lookup routines - ROUTE
      *
                           11  W43-DET2A.
                               13  FILLER PIC  X(04).
      *
      *    ****   Used in Lookup routines - COMPANY CODE
      *
                               13  W43-DET3.
                                   15 FILLER PIC  X(02).
      *
      *    ****   Used in Lookup routines
      *
                                   15 W43-DET3A PIC  X(42).
                               13  FILLER       PIC  X(02).
                       09  FILLER               PIC  X(05).

           03  W43-SCREEN2B  REDEFINES W43-SCREEN2A.
               05  W43-LINE2                     OCCURS 50.
                   07  FILLER      PIC  X(10).
      *
      *    ****   Used in Lookup routines - MODEL NUMBERS
      *
                   07  W43-DET4A.
                       09  FILLER  PIC  X(28).
      *
      *    ****   Used in Lookup routines
      *
                       09  W43-DET4.
                           11  FILLER PIC  X(13).
      *
      *    ****   Used in Lookup routines - DEBTOR STANDING ORDERS
      *
                           11  W43-DET5 PIC  X(24).
                       09  FILLER    PIC  X(02).
                   07  FILLER        PIC  X(03).

           03  W43-SCREEN2C                      REDEFINES W43-SCREEN2A.
               05  W43-LINE3                     OCCURS 50.
                   07  FILLER      PIC  X(41).
      *
      *    ****   Used in Lookup routines - TECHNICIANS
      *
                   07  W43-DET6    PIC  X(34).
                   07  FILLER      PIC  X(05).


           03  W43-SCREEN2E                      REDEFINES W43-SCREEN2A.
               05  W43-LINE5                     OCCURS 50.
                   07  FILLER      PIC  X(06).
      *
      *    ****   Used in Allocate credits - DEBTORS (A/R)
      *
                   07  W43-DET8    PIC  X(56).
                   07  FILLER      PIC  X(01).
                   07  W43-DET9    PIC  X(11).
                   07  FILLER      PIC  X(06).

       01  W43-SERIALS                           REDEFINES W43-SCREEN2.
           03  W43-SERNO           PIC  X(18)    OCCURS 222.
           03  FILLER              PIC  X(04).

       01  W43-SCREEN3.
           03  FILLER              PIC X(4000).

       01  W44-FUNCTION            PIC 9(02)     COMP-X.
