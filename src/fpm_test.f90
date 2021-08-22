module fpm_test
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fpm_test!"
  end subroutine say_hello
end module fpm_test
