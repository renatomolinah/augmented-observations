module utils

contains
!---------------------------------------------------------------------------------
! Fortran Program Name: interp1.f
!
! Technical Contact(s):   Name:  Tom Gross             Org:  NOS/CSDL
!                         Phone: 301-713-2809x139      E-Mail: tom.gross@noaa.gov
!
! Abstract:
!                To interpolate a irregular spaced or gappy time series
!        to another (probably equally spaced) time basis.  Uses
!        linear interpolation to span gaps.  Uses persistence on
!        beginning and end if original time series does not span
!        the target times.         
!
! Usage:  
!        CALL interp1(nk,x,y,ni,xi,yi)
!
!	Example usage:
!
!      		real*8 tobs(NARRAY),hobs(NARRAY)
!      		real*8 tnew(NARRAY),hnew(NARRAY)
!
!       	do i = 1,nobs
!         	read(10,*) tobs(i),hobs(i)
!      		enddo
!
!      		n = int((daylast-dayfirst)/dt + 1.0d00)
!      		do i = 1 , n
!         		 tnew(i) = dayfirst+(i-1)*dt
!      		enddo
!
!       Call interp1(nobs,tobs,hobs,n,tnew,hnew).
!  	The resultant array hnew(tnew) is an interpolated
!  	function agreeing with hobs(tobs)
!
!
! Input Parameters:  
!        x,y are filled data vectors
!        xi  is vector of new xi,yi
!        yi will be filled with interpolated values at xi inside x,y
!        x should be monotonic
!        It won't bomb otherwise, but don't trust the method!
!        If xi is less than min(x) pre-persistence fills in the blanks
!        If xi is greater than max(x) persistence fills in the blanks
!      		 
! Author Name: Tom Gross               Creation Date:  Jan.  1998 
! -----------------------------------------------------------------------------------
! B. McNoldy 8/2012
! - added the ability to have vectors be in monotonically DECREASING order as well.
! - endpoints are now extrapolated rather that copied from original endpoints
!
  subroutine interp1(nk,x,y,ni,xi,yi)

    implicit none

    !x,y are filled data vectors
    !xi  is vector of new xi,yi
    !yi will be filled with interpolated values at xi inside x,y
    !x should be monotonic
    !It won't bomb otherwise, but don't trust the method!
    integer,intent(in) :: nk, ni
    real, intent(in)   :: x(nk), y(nk), xi(ni)
    real, intent(out)  :: yi(ni)
    integer            :: kf, kl, i
    logical            :: reverse=.false.
    real               :: x2(nk), y2(nk), xi2(ni), yi2(ni)

    if (x(1) > x(nk)) reverse=.true.
    if (reverse) then
      x2(nk:1:-1)=x(1:nk)
      y2(nk:1:-1)=y(1:nk)
      xi2(ni:1:-1)=xi(1:ni)
    else
      x2=x
      y2=y
      xi2=xi
    end if

    kf = 1
    kl = 2
    do i = 1,ni
!      if (xi2(i) <= x2(1)) then
!        !first interpolates are before the data starts
!        yi2(i) = y2(kf)
!      else
        do while(xi2(i) > x2(kl) .and. kl < nk)
          !Reposition so that x(kf)<= xi(i) < x(kl)
          !When xi(i)>x(nk) it runs up till  kl =nk
          kf = kf+1
          kl = kl+1
        end do
!        if (xi2(i) >= x2(nk)) then
!          !last interpolates are after the data ends
!          yi2(i) = y2(nk)
!        else
          !linear interpolate for the range  x(kf)<= xi(i) < x(kl)
          yi2(i) = y2(kf) + (xi2(i)-x2(kf))*(y2(kl)-y2(kf))/(x2(kl)-x2(kf))
!        end if
!      end if
    end do

    if (reverse) then
      yi(ni:1:-1)=yi2(1:ni)
    else
      yi=yi2
    end if

    reverse=.false.  !reset switch to false

  end subroutine interp1


  FUNCTION Upper(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i
    DO i = 1,LEN(s1)
      ch = s1(i:i)
      IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
      s2(i:i) = ch
    END DO
  END FUNCTION Upper


end module utils
