! (C) Copyright 1996- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

!> @brief Calculate Extreme Forecast Index and statistics (ranking, anomalies, etc.)
!> 2012/03: analytic formula for EFI and linear approximation of PC
!>
!> @author L. Ferranti (2003/03/26)
!> @author F. Lalaurette (2003/05, 2004/09)
!> @author I. Tsonevsky (2012/03)
!> @author M. Diamantakis (2012/03)
!> @author F. Di Giuseppe (2020/04)
!> @author P. Maciel (2020/04)
module mo_statistics

    implicit none
    private

    public :: extreme_forecast_index
    public :: hpsort_eps_epw
    public :: compute_maxfwi
contains

  integer function extreme_forecast_index(NCLIM, NFORE, NPOINTS, CLIM, FORE, &
                                            EFI, EFI_EPSILON, MV, &
                                            SOT, SOT_EPSILON, SOT_RANK, SOT_FLAG, &
                                            ANOMALY, CANOMALY, RANK, CRANK) &
        result(err)

        ! use nag_sort, only: nag_sort_vec

        implicit none

        integer, intent(in) :: NCLIM
        integer, intent(in) :: NFORE
        integer, intent(in) :: NPOINTS
        real(kind=8), intent(inout) :: CLIM(NPOINTS, NCLIM)  !< climate data
        real(kind=8), intent(inout) :: FORE(NPOINTS, NFORE)  !< forecast data

        real(kind=8), intent(out) :: EFI(NPOINTS)
        real(kind=8), intent(in), optional :: EFI_EPSILON    ! if >0 MASK FOR ZERO VALUES APPLIED (E.G. FOR PRECIP)

        real(kind=8), intent(in), optional :: MV             !< missing value

        real(kind=8), intent(out), optional :: SOT(NPOINTS)
        real(kind=8), intent(in), optional :: SOT_EPSILON    ! if >0 MASK FOR ZERO VALUES APPLIED (E.G. FOR PRECIP)
        integer, intent(in), optional :: SOT_RANK
        integer, intent(in), optional :: SOT_FLAG

        real(kind=8), intent(out), optional :: ANOMALY(NPOINTS)        !< anomalies in FWI units
        real(kind=8), intent(out), optional :: CANOMALY(NPOINTS)       !< Normalised anomalies (divided by standard deviation)
        real(kind=8), intent(out), optional :: RANK(NPOINTS)           !< percentile
        real(kind=8), intent(out), optional :: CRANK(NPOINTS)          !< CLASSIFIED

        !
        ! LOCAL VARIABLES
        !

        integer :: i, j, k, qext, SOT_RANK_, SOT_FLAG_, CLIM_IND(NCLIM), FORE_IND(NFORE)
        real(kind=8) :: DUMMY(1) = (/0.D0/)
        real(kind=8) :: EFI_EPSILON_, SOT_EPSILON_, MV_
        real(kind=8) :: PVALS, DEFI, EFIMAX, dfdp, w1, w2, tmp1, tmp2, qeps, qclim, ref, STDCLIM, MEANCLIM, MEANFORE
        real(kind=8) :: ANOMALYI, RANKI, CANOMALYI, CRANKI, P1CLIM, P2CLIM, P3CLIM, P4CLIM, P5CLIM
        real(kind=8), dimension(:), allocatable ::  SORTED
        real(kind=8), dimension(NCLIM) :: RCLIM, DRCLIM, ACOSDIFF, ACOEF, PROD1, PROD2
        real(kind=8), dimension(NPOINTS, NCLIM) :: PC, EPSF
        real(kind=8), parameter :: P1 = 0.75, P2 = 0.85, P3 = 0.90, P4 = 0.95, P5 = 0.98
        real(kind=8), parameter :: SORT_EPS = 1.e-12
        real(kind=8), parameter :: PI = 4.0 * atan(1.D0)

        logical :: missing
        missing(i) = (FORE(i, 1) == MV_)

        err = 0

        MV_ = -9999.
        if (present(MV)) MV_ = MV

        do i = 1, NPOINTS
            if (missing(i)) then
                EFI(i) = MV_
            else
                EFI(i) = 0.
            endif
        enddo

        EFI_EPSILON_ = -1.
        if (present(EFI_EPSILON)) EFI_EPSILON_ = EFI_EPSILON

        if (present(sot)) then
            do i = 1, NPOINTS
                if (missing(i)) then
                    SOT(i) = MV_
                else
                    SOT(i) = 0.
                endif
            enddo

            SOT_EPSILON_ = 1.
            SOT_RANK_ = 90
            SOT_FLAG_ = 1
            if (present(SOT_EPSILON)) SOT_EPSILON_ = SOT_EPSILON
            if (present(SOT_RANK)) SOT_RANK_ = SOT_RANK
            if (present(SOT_FLAG)) SOT_FLAG_ = SOT_FLAG
        endif

        ! Sort climate & forecast data
        allocate (SORTED(NCLIM), STAT=err)
        if (err /= 0) return
        do i = 1, NPOINTS
            if (missing(i)) cycle
            SORTED(:) = CLIM(i, :)
            CLIM_IND(:) = 0                        ! Initialise the index vector
            call hpsort_eps_epw(NCLIM, SORTED, CLIM_IND, SORT_EPS)
            CLIM(i, :) = SORTED(:)
        enddo
        deallocate (SORTED, STAT=err)
        if (err /= 0) return

        allocate (SORTED(NFORE), STAT=err)
        if (err /= 0) return
        do i = 1, NPOINTS
            if (missing(i)) cycle
            SORTED(:) = FORE(i, :)
            FORE_IND(:) = 0                        ! Initialise the index vector
            call hpsort_eps_epw(NFORE, SORTED, FORE_IND, SORT_EPS)
            FORE(i, :) = SORTED(:)
        enddo
        deallocate (SORTED, STAT=err)
        if (err /= 0) return

        if (SOT_EPSILON_ > 0) then
            do i = 1, NPOINTS
                if (missing(i)) cycle
                do j = 1, NFORE
                    if (FORE(i, j) < 0.00005) FORE(i, j) = 0.0
                enddo
                do j = 1, NCLIM
                    if (CLIM(i, j) < 0.00005) CLIM(i, j) = 0.0
                enddo
            enddo
        endif

        ! EFI

        do j = 1, NCLIM
            RCLIM(j) = dble(j - 1) / dble(NCLIM - 1)
        enddo

        EPSF(:, :) = 0.D0
        PVALS = 1.
        PC(:, :) = 0.D0
        do j = 1, NCLIM
            do i = 1, NPOINTS
                if (missing(i)) cycle
                do k = 1, NFORE
                    if (FORE(i, k) > CLIM(i, j)) PC(i, j) = PC(i, j) + 1.D0
                enddo
                PVALS = PVALS + ((j - 1) * NFORE) / (NCLIM)
                EPSF(i, j) = FORE(i, int(PVALS))
                PVALS = 1.D0
            enddo
            PC(:, j) = 1.d0 - (PC(:, j) / NFORE)
        enddo

        ! Pre-compute and store formulae coefficients (gridpoint independent)

        do j = 1, NCLIM - 1
            ACOSDIFF(j) = acos(sqrt(RCLIM(j + 1))) - acos(sqrt(RCLIM(j)))
            PROD1(j) = sqrt(RCLIM(j + 1) * (1.d0 - RCLIM(j + 1)))
            PROD2(j) = sqrt(RCLIM(j) * (1.d0 - RCLIM(j)))
            ACOEF(j) = (1.d0 - 2.d0 * RCLIM(j)) * ACOSDIFF(j) + PROD1(j) - PROD2(j)
            DRCLIM = RCLIM(j + 1) - RCLIM(j)
        enddo

        if (EFI_EPSILON_ > 0.D0) then
            do i = 1, NPOINTS
                if (missing(i)) cycle
                EFIMAX = 0.D0
                do j = 1, NCLIM - 1
                    dFdp = (PC(i, j + 1) - PC(i, j)) / DRCLIM(j)
                    DEFI = (2.d0 * PC(i, j) - 1.0d0) * ACOSDIFF(j) + ACOEF(j) * dFdp + PROD2(j) - PROD1(j)
                    ! Treat zero values for precipitation and snowfall (EFI_EPSILON_>0)
                    if (CLIM(i, j + 1) > EFI_EPSILON_) then
                        EFIMAX = EFIMAX - ACOSDIFF(j) + PROD2(j) - PROD1(j)
                    else
                        DEFI = 0.D0
                    endif
                    EFI(i) = EFI(i) + DEFI
                enddo
                EFIMAX = max(EFIMAX, EFI_EPSILON_)
                EFI(i) = EFI(i) / EFIMAX
            enddo
        else
            do i = 1, NPOINTS
                if (missing(i)) cycle
                EFIMAX = PI / 2.D0
                do j = 1, NCLIM - 1
                    dFdp = (PC(i, j + 1) - PC(i, j)) / DRCLIM(j)
                    DEFI = (2.d0 * PC(i, j) - 1.0d0) * ACOSDIFF(j) + ACOEF(j) * dFdp + PROD2(j) - PROD1(j)
                    EFI(i) = EFI(i) + DEFI
                enddo
                EFIMAX = max(EFIMAX, EFI_EPSILON_)
                EFI(i) = EFI(i) / EFIMAX
            enddo
        endif

        ! SO99T

        if (present(sot)) then
            do i = 1, NPOINTS
                if (missing(i)) cycle
                if (SOT_RANK_ == 100) then
                    qeps = FORE(i, NFORE)
                    qclim = CLIM(i, NCLIM)
                else
                    w2 = SOT_RANK_ * (NFORE - 1) / 100.0 - int(SOT_RANK_ * (NFORE - 1) / 100.0)
                    w1 = 1 - w2
                    tmp1 = w1 * FORE(i, 1 + int(SOT_RANK_ * (NFORE - 1) / 100.0))
                    tmp2 = w2 * FORE(i, 1 + int(SOT_RANK_ * (NFORE - 1) / 100.0) + 1)
                    qeps = tmp1 + tmp2

                    w2 = SOT_RANK_ * (NCLIM - 1) / 100.0 - int(SOT_RANK_ * (NCLIM - 1) / 100.0)
                    w1 = 1 - w2
                    tmp1 = w1 * CLIM(i, 1 + int(SOT_RANK_ * (NCLIM - 1) / 100.0))
                    tmp2 = w2 * CLIM(i, 1 + int(SOT_RANK_ * (NCLIM - 1) / 100.0) + 1)
                    qclim = tmp1 + tmp2
                endif

                if (SOT_FLAG_ == 1) then
                    qext = 99
                else
                    qext = 1
                endif
                w2 = qext * (NCLIM - 1) / 100.0 - int(qext * (NCLIM - 1) / 100.0)
                w1 = 1 - w2
                tmp1 = w1 * CLIM(i, 1 + int(qext * (NCLIM - 1) / 100.0))
                tmp2 = w2 * CLIM(i, 1 + int(qext * (NCLIM - 1) / 100.0) + 1)
                ref = tmp1 + tmp2
                if ((SOT_EPSILON_ > 0) .and. ((ref - qclim) < 0.0001)) then
                    SOT(i) = MV_
                else
                    SOT(i) = -(qeps - ref) / (qclim - ref)
                endif
            enddo
        endif

        ! Statistics

        if (present(ANOMALY) .or. present(CANOMALY) .or. present(RANK) .or. present(CRANK)) then
            do i = 1, NPOINTS
                ANOMALYI = MV_
                RANKI = MV_
                CANOMALYI = MV_
                CRANKI = MV_

                if (.not. missing(i)) then
                    MEANFORE = sum(FORE(i, :)) / float(NFORE)
                    MEANCLIM = sum(CLIM(i, :)) / float(NCLIM)
                    STDCLIM = sqrt(sum((CLIM(i, :) - MEANCLIM)**2) / float(NCLIM - 1))
              
                    P1CLIM = CLIM(i, int(P1 * NCLIM))
                    P2CLIM = CLIM(i, int(P2 * NCLIM))
                    P3CLIM = CLIM(i, int(P3 * NCLIM))
                    P4CLIM = CLIM(i, int(P4 * NCLIM))
                    P5CLIM = CLIM(i, int(P5 * NCLIM))
         
             
                    ANOMALYI = (MEANFORE - MEANCLIM)

                    IF (ABS(STDCLIM) .LE. 1e-9) THEN
                        CANOMALYI = 0
                    ELSE
                        CANOMALYI = ANOMALYI / STDCLIM
                    ENDIF

                    ! search  in the climate sorted string of values for the closest number to the mean forecast, then identify the location of the number in the vector with minloc,n. n is the ordinal rank. The percentile is n/NCLIM *100
                    DUMMY = (minloc(abs(CLIM(i, :) - MEANFORE)) * 100./NCLIM)
                    RANKI = DUMMY(1)

                    if (MEANFORE <= P1CLIM) then
                        CRANKI = 1
                    else if (MEANFORE <= P2CLIM) then
                        CRANKI = 2
                    else if (MEANFORE <= P3CLIM) then
                        CRANKI = 3
                    else if (MEANFORE <= P4CLIM) then
                        CRANKI = 4
                    else if (MEANFORE <= P5CLIM) then
                        CRANKI = 5
                    else
                        CRANKI = 6
                    endif
            !        if ( RANKI .le. 90 ) THEN
            !           PRINT*,'MEANFORE'
            !           PRINT*,MEANFORE
            !           PRINT*,'==='
            !           PRINT*,CRANKI,RANKI
            !        END if
              endif
                if (present(ANOMALY)) ANOMALY(i) = ANOMALYI
                if (present(RANK)) RANK(i) = RANKI
                if (present(CANOMALY)) CANOMALY(i) = CANOMALYI
                if (present(CRANK)) CRANK(i) = CRANKI
            enddo
         endif
      end function extreme_forecast_index

    ! Copyright (C) 2010-2016 Samuel Ponce', Roxana Margine, Carla Verdi, Feliciano Giustino
    ! Copyright (C) 2007-2009 Jesse Noffsinger, Brad Malone, Feliciano Giustino
    !
    ! This file is distributed under the terms of the GNU General Public
    ! License. See the file `LICENSE' in the root directory of the
    ! present distribution, or http://www.gnu.org/copyleft.gpl.txt .
    !
    ! Adapted from flib/hpsort_eps
    !---------------------------------------------------------------------
    subroutine hpsort_eps_epw(n, ra, ind, eps)
        !---------------------------------------------------------------------
        ! sort an array ra(1:n) into ascending order using heapsort algorithm,
        ! and considering two elements being equal if their values differ
        ! for less than "eps".
        ! n is input, ra is replaced on output by its sorted rearrangement.
        ! create an index table (ind) by making an exchange in the index array
        ! whenever an exchange is made on the sorted data array (ra).
        ! in case of equal values in the data array (ra) the values in the
        ! index array (ind) are used to order the entries.
        ! if on input ind(1)  = 0 then indices are initialized in the routine,
        ! if on input ind(1) != 0 then indices are assumed to have been
        !                initialized before entering the routine and these
        !                indices are carried around during the sorting process
        !
        ! no work space needed !
        ! free us from machine-dependent sorting-routines !
        !
        ! adapted from Numerical Recipes pg. 329 (new edition)
        !
        implicit none
        !-input/output variables
        integer, intent(in)   :: n
        real(kind=8), intent(in)  :: eps
        integer :: ind(n)
        real(kind=8) :: ra(n)
        !-local variables
        integer :: i, ir, j, l, iind
        real(kind=8) :: rra
!
        ! initialize index array
        if (ind(1) .eq. 0) then
            do i = 1, n
                ind(i) = i
            enddo
        endif
        ! nothing to order
        IF (n .lt. 2) return
        ! initialize indices for hiring and retirement-promotion phase
        l = n / 2 + 1

        ir = n

        sorting: do

            ! still in hiring phase
            if (l .gt. 1) then
                l = l - 1
                rra = ra(l)
                iind = ind(l)
                ! in retirement-promotion phase.
            else
                ! clear a space at the end of the array
                rra = ra(ir)
                !
                iind = ind(ir)
                ! retire the top of the heap into it
                ra(ir) = ra(1)
                !
                ind(ir) = ind(1)
                ! decrease the size of the corporation
                ir = ir - 1
                ! done with the last promotion
                if (ir .eq. 1) then
                    ! the least competent worker at all !
                    ra(1) = rra
                    !
                    ind(1) = iind
                    exit sorting
                endif
            endif
            ! wheter in hiring or promotion phase, we
            i = l
            ! set up to place rra in its proper level
            j = l + l
            !
            do while (j .le. ir)
                if (j .lt. ir) then
                    ! compare to better underling
                    IF (hslt(ra(j), ra(j + 1), eps)) then
                        j = j + 1
                        !else if ( .not. hslt( ra (j+1),  ra (j) ) ) then
                        ! this means ra(j) == ra(j+1) within tolerance
                        !  if (ind (j) .lt.ind (j + 1) ) j = j + 1
                    endif
                endif
                ! demote rra
                if (hslt(rra, ra(j),eps)) then
                    ra(i) = ra(j)
                    ind(i) = ind(j)
                    i = j
                    j = j + j
                    !else if ( .not. hslt ( ra(j) , rra ) ) then
                    !this means rra == ra(j) within tolerance
                    ! demote rra
                    ! if (iind.lt.ind (j) ) then
                    !    ra (i) = ra (j)
                    !    ind (i) = ind (j)
                    !    i = j
                    !    j = j + j
                    ! else
                    ! set j to terminate do-while loop
                    !    j = ir + 1
                    ! endif
                    ! this is the right place for rra
                else
                    ! set j to terminate do-while loop
                    j = ir + 1
                endif
            enddo
            ra(i) = rra
            ind(i) = iind

         enddo sorting
       end subroutine hpsort_eps_epw

       logical function hslt(a, b ,eps)
         real(kind=8), intent (in):: a, b, eps
         if (abs(a - b) < eps) then
            hslt = .false.
         else
            hslt = (a < b)
         end if
       end function hslt

      
       !  internal function
       !  compare two real number and return the result
          !
         

       integer function compute_maxfwi (NT,NPOINTS,VAR,MV,MAXVAR,MINVAR, MEANVAR,TIMEMAX)&
               result(err)
         !  Subroutine to calculate the maximum of the day and the time at wich it occurs 
         ! the subroutine works on a daily assumption so imputs should be given in chunck of days
         !   
         !--------------------------
         
    
         IMPLICIT NONE
         INTEGER,INTENT(IN) :: NT                 ! number of time steps in a day
         INTEGER,INTENT(IN) :: NPOINTS            ! Number of points in the grib      
         REAL(kind=8),INTENT(IN),optional :: MV                 ! MISSING VALUE
         real(kind=8), intent(inout) :: VAR(NPOINTS, NT)  !< - data 
  
         REAL(kind=8),INTENT(OUT) :: MAXVAR(NPOINTS)  ! max FWI 
         REAL(kind=8),INTENT(OUT) :: MEANVAR(NPOINTS)  ! mean over the day FWI 
         REAL(kind=8),INTENT(OUT) :: MINVAR(NPOINTS)  ! min FWI 
         REAL(kind=8),INTENT(OUT) :: TIMEMAX(NPOINTS) ! Time @max FWI

         !
         ! LOCAL VARIABLES
         !
        real(kind=8) :: MV_
        INTEGER :: i, igrid,dt,zdt
       
        REAL(kind=8)     :: Mean=0.D0, std=0.D0
        logical :: missing
        missing(i) = (VAR(i, 1) == MV_)

        err = 0

        MV_ = -9999.
        if (present(MV)) MV_ = MV
      
     
         ! time step
         dt=INT(24/nt)
 
         ! Respect grid missing 

         DO I=1, NPOINTS
            IF (missing(I)) THEN
               MAXVAR(I) = MV
               MINVAR(I) = MV
               MEANVAR(I) = MV
               TIMEMAX(I) = MV
            ELSE
               MAXVAR(I) = 0.D0
               MINVAR(I) = 0.D0
               MEANVAR(I) = 0.D0
               TIMEMAX(I) = 0.D0
            ENDIF
         ENDDO

         ! Sort values if required

         DO IGRID=1,NPOINTS
            IF (MAXVAR(IGRID) .EQ. MV) THEN
               CYCLE
            ENDIF
            MAXVAR(IGRID)=MAXVAL(VAR(IGRID,:))
            MINVAR(IGRID)=MINVAL(VAR(IGRID,:))
           
            CALL calc_std(nt, VAR(IGRID,:),mean,std)
       !     print*,size( VAR(IGRID,:)),mean,std
             MEANVAR(IGRID)=mean
            IF ( MAXVAR(IGRID)-MINVAR(IGRID) .GT. 5.    ) THEN 
               ! search the local time only if there is daily variability 

               TIMEMAX(IGRID)=(MAXLOC(VAR(IGRID,:),DIM=1)*DT) 
            ELSE
               !reset to default that is 12 UTC shell we put MV?
               TIMEMAX(IGRID)=12
            END IF
         ! reset values that falls during night 
            IF (TIMEMAX(IGRID) .GT. 20 .OR. TIMEMAX(IGRID) .LE. 10) TIMEMAX(IGRID)=12
            
         ENDDO

       END FUNCTION  COMPUTE_MAXFWI

       SUBROUTINE  calc_std(nt, x, mean, var)
         IMPLICIT  NONE
         INTEGER, INTENT(IN)    :: nt
         REAL*8, INTENT(IN)    :: x(nt)
         REAL*8, INTENT(INOUT) :: mean, var
 
         INTEGER::it
         REAL:: Variance=0.0
         mean = 0.0                       ! compute mean
         var=0.0

         DO IT= 1, NT
            mean = mean + x(IT)
         END DO
         mean = mean / NT
         DO IT = 1, NT
            
            Variance = Variance + max( (x(IT) - Mean)**2,1e-10)
         END DO
         Variance = Variance / (NT - 1)

         var=SQRT(Variance)
                
         RETURN
       END SUBROUTINE calc_std


     end module
