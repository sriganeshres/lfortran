program complex_soln

   use, intrinsic :: iso_fortran_env, only: wp => real64
   use, intrinsic :: ieee_arithmetic
   implicit none

   complex(wp) :: z1, arg, x, result1, result2
   real(wp), parameter :: rhs = 4.0_wp, twopi = 8 * atan(1.0_wp), lnrhs = log(rhs)
   integer :: k
   character(*), parameter :: cfmth = '("Complex solutions to 1**x=",g0.4/a2,*(4x,a11,5x))', &
        &                       cfmt = '(i2,*(" (",es0.2,",",es0.2,")":))'

   write(*,cfmth) rhs, 'k', 'arg=i2Pik', 'exp(arg)', 'x', 'exp(arg)**x', 'exp(arg*x)'

   do k = 1, 10
      arg = cmplx(0.0_wp, twopi * k, kind=wp)
      z1  = exp(arg)
      x   = cmplx(0.0_wp, -lnrhs / (twopi * k), kind=wp)
      result1 = z1**x
      result2 = exp(arg * x)

      ! Check for NaN or Inf
      if (ieee_is_nan(real(result1)) .or. ieee_is_nan(aimag(result1)) .or. &
          ieee_is_nan(real(result2)) .or. ieee_is_nan(aimag(result2)) .or. &
          ieee_is_finite(real(result1)) .eqv. .false. .or. ieee_is_finite(aimag(result1)) .eqv. .false. .or. &
          ieee_is_finite(real(result2)) .eqv. .false. .or. ieee_is_finite(aimag(result2)) .eqv. .false.) then
         write(*, *) 'Invalid result encountered at iteration k=', k
         error stop
      end if

      write(*,cfmt) k, arg, z1, x, result1, result2
   end do
end program complex_soln
