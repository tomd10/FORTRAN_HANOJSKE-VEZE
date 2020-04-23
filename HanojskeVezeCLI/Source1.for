C     PROGRAM PRO RESENI ULOHY HANOJSKE VEZE
C     AUTOR: TOMAS CHABADA P4
      
C     POMOCNE METODY
      MODULE METODY
      CONTAINS
      
C     METODA PRO INICIALIZACI VSECH VEZI     
      SUBROUTINE INICIALIZACE(VEZ1, VEZ2, VEZ3, POCET)
          INTEGER, INTENT(IN) :: POCET
          INTEGER, DIMENSION(:), ALLOCATABLE, 
     &    INTENT(INOUT) :: VEZ1, VEZ2, VEZ3
          
          ALLOCATE(VEZ1(POCET))
          ALLOCATE(VEZ2(POCET))
          ALLOCATE(VEZ3(POCET))
          DO I = 1, POCET
              VEZ1(I) = I
          END DO
          VEZ2 = -1
          VEZ3 = -1
      END SUBROUTINE INICIALIZACE
      
C     RUTINA PRO VYKRESLENI STAVU VEZI NA CLI     
      SUBROUTINE VYKRESLI_VEZE(VEZ1, VEZ2, VEZ3, KOLO, POHYB)
          INTEGER, DIMENSION(:), INTENT(IN) :: VEZ1, VEZ2, VEZ3
          INTEGER, INTENT(IN) :: KOLO
          
          CHARACTER(LEN=7) :: POHYB
          CHARACTER(LEN=80) :: RADEK
          INTEGER :: POCET_CRLF
          
          PRINT*, "TAH #", KOLO
          PRINT*, "POHYB: ", POHYB
          PRINT*, ""
          DO I = 1, SIZE(VEZ1)
              RADEK = " "
              RADEK(13:13) = "|"
              RADEK(14:14) = "|"
              RADEK(39:39) = "|"
              RADEK(40:40) = "|"
              RADEK(65:65) = "|"
              RADEK(66:66) = "|"
              
              IF (I > SIZE(VEZ1)) THEN
                  PRINT*, RADEK
                  CYCLE
              END IF
          
              IF (VEZ1(I) /= -1) THEN
                  RADEK( (13 - VEZ1(I)) : 12) = REPEAT("X", VEZ1(I))
                  RADEK(15 : (15 + VEZ1(I) -1)) = REPEAT("X", VEZ1(I))
              END IF
              
              IF (VEZ2(I) /= -1) THEN
                  RADEK( (39 - VEZ2(I)) : 38) = REPEAT("X", VEZ2(I))
                  RADEK(41 : (41 + VEZ2(I) -1)) = REPEAT("X", VEZ2(I))
              END IF
              
              IF (VEZ3(I) /= -1) THEN
                  RADEK( (65 - VEZ3(I)) : 64) = REPEAT("X", VEZ3(I))
                  RADEK(67 : (67 + VEZ3(I) -1)) = REPEAT("X", VEZ3(I))
              END IF
              
              PRINT("(A)"), RADEK
          END DO
          
          POCET_CRLF = 21 - SIZE(VEZ1)
          DO I = 1, POCET_CRLF
              PRINT("(A)"), " "
          END DO
      END SUBROUTINE VYKRESLI_VEZE
      
C     METODA PRO PRESUN MEZI VEZEMI     
      SUBROUTINE POSUN(VEZ1, VEZ2)
          IMPLICIT NONE
          
C         DEKLARACE PROMENNYCH         
          INTEGER, DIMENSION(:), INTENT(INOUT) :: VEZ1, VEZ2
          INTEGER :: VRCHOL1, VRCHOL2
          INTEGER :: I, J
          
C         INICIALIZACE PROMENNYCH         
          VRCHOL1 = -1
          VRCHOL2 = -1
          
C         NALEZENI NEJVYSSICH DISKU          
          DO I = 1, SIZE(VEZ1)
              IF (VEZ1(I) /= -1) THEN
                  VRCHOL1 = VEZ1(I)
                  EXIT
              END IF
          END DO
          
          DO J = 1, SIZE(VEZ1)
              IF (VEZ2(J) /= -1) THEN
                  VRCHOL2 = VEZ2(J)
                  EXIT
              END IF
          END DO
          
          IF (VRCHOL1 > VRCHOL2) THEN
              IF (VRCHOL2 == -1) THEN
                  VEZ2(SIZE(VEZ2)) = VRCHOL1
                  VEZ1(I) = -1
              ELSE
                  VEZ1(I-1) = VRCHOL2
                  VEZ2(J) = -1
              END IF
          ELSE
              IF (VRCHOL1 == -1) THEN
                  VEZ1(SIZE(VEZ1)) = VRCHOL2
                  VEZ2(J) = -1
              ELSE
                  VEZ2(J-1) = VRCHOL1
                  VEZ1(I) = -1
              END IF
          END IF
      END SUBROUTINE POSUN
      
      END MODULE METODY
C     HLAVNI PROGRAM
      PROGRAM MAIN
      USE METODY
      IMPLICIT NONE
      
C     DEKLRACE PROMENNYCH
      INTEGER, DIMENSION(:), ALLOCATABLE :: VEZA
      INTEGER, DIMENSION(:), ALLOCATABLE :: VEZB
      INTEGER, DIMENSION(:), ALLOCATABLE :: VEZC
      
      INTEGER :: FAZE
      INTEGER :: POCET_DISKU
      INTEGER :: POCET_KROKU
      INTEGER :: I
      
      LOGICAL :: SUDY_POCET
      CHARACTER(LEN=7) :: DRUH_POHYBU
      
C     INICIALIZACE PROMENNYCH
      FAZE = 1
      DRUH_POHYBU = "       "
      
C     CTENI VSTUPU
      PRINT*, "ZADEJ POCET DISKU (3 ~ 10):"
      READ*, POCET_DISKU

C     KONTROLA VSTUPU     
      IF (POCET_DISKU < 3 .OR. POCET_DISKU > 10) THEN
          GO TO 1000
      END IF
      
      POCET_KROKU = 2 ** POCET_DISKU -1
      SUDY_POCET = MOD(POCET_DISKU, 2) == 0
      CALL INICIALIZACE(VEZA, VEZB, VEZC, POCET_DISKU)
      
      CALL VYKRESLI_VEZE(VEZA, VEZB, VEZC, 0, DRUH_POHYBU)
      PAUSE 
      
C     HLAVNI CYKLUS
      DO I = 1, POCET_KROKU
          IF (SUDY_POCET) THEN
              GO TO (10, 20, 30), FAZE
          ELSE
              GO TO (20, 10, 30), FAZE
          END IF
          
 10       CALL POSUN(VEZA, VEZB)
          DRUH_POHYBU = "A <=> B"
          GO TO 40
 20       CALL POSUN(VEZA, VEZC)
          DRUH_POHYBU = "A <=> C"
          GO TO 40
 30       CALL POSUN(VEZB, VEZC)
          DRUH_POHYBU = "B <=> C"
 
 40       FAZE = FAZE + 1
          IF (FAZE == 4) THEN
              FAZE = 1
          END IF
          
          CALL VYKRESLI_VEZE(VEZA, VEZB, VEZC, I, DRUH_POHYBU)
          PAUSE
      END DO
      
      PRINT*, "----- HOTOVO -----"
1000  PAUSE  
      END PROGRAM MAIN
      