program array_test

  use unit_test
  use array_mod

  implicit none

  type(array_type) a, b
  real, target :: c

  call test_suite_init('Array tests')

  call test_case_create('Test array type')

  call assert_equal(a%size, 0)

  call a%append(1)

  call assert_equal(a%size, 1)

  select type (val => a%value_at(1))
  type is (integer)
    call assert_equal(val, 1, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  c = 1.5
  call a%append_ptr(c)
  b = a
  call a%clear()
  call assert_equal(b%size, 2, __FILE__, __LINE__)
  call assert_true(associated(b%value_at(2), c))
  select type (val => b%value_at(1))
  type is (integer)
    call assert_equal(val, 1, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  select type (val => b%value_at(2))
  type is (real)
    call assert_equal(val, c, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call test_suite_report()

  call test_suite_final()

end program array_test
