program array_test

  use unit_test
  use array_mod

  implicit none

  type(array_type) a

  call test_case_init()

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

  call test_case_report('Test array type')

  call test_case_final()

end program array_test
