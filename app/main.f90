program main
  use fpm_test, only: say_hello
  !use oper,     only: g
  use pade_coeffs, only: epade

  implicit none

  complex*16 pd1(20),pd2(20)
  real*8 k0, dr

  k0 = 6 / 1500.
  dr = 200.

  call epade(20_8,6_8,1_8,2_8,k0,dr,pd1,pd2)
  print *, pd1(:6)

end program main
