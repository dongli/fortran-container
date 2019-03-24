program list_test

  use unit_test
  use linked_list_mod

  implicit none

  type test_type
    real i
    integer j
  end type test_type

  type(linked_list_type) a
  type(test_type), pointer :: x
  type(linked_list_item_type), pointer :: item

  call test_case_init()

  call test_case_create('Test linked list insert method')

  ! Initial size should be zero.
  call assert_equal(a%size, 0, __FILE__, __LINE__)

  ! Item 1
  ! Insert a string.
  call a%insert('foo')
  call assert_equal(a%size, 1, __FILE__, __LINE__)
  select type (val => a%last_value())
  type is (character(*))
    call assert_equal(val, 'foo', __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  ! Item 2
  ! Insert an integer with key.
  call a%insert('bar', 1)
  call assert_equal(a%size, 2, __FILE__, __LINE__)
  select type (val => a%value_at(2))
  type is (integer)
    call assert_equal(val, 1, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  ! Change the value to a real number.
  call a%insert('bar', 4.2, nodup=.true.)
  call assert_equal(a%size, 2, __FILE__, __LINE__)
  call assert_true(associated(a%item('bar')), __FILE__, __LINE__)
  call assert_true(associated(a%value('bar')), __FILE__, __LINE__)
  select type (val => a%value('bar'))
  type is (real)
    call assert_equal(val, 4.2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  ! Item 3
  ! Insert a new item although its key is the same with the previous one.
  call a%insert('bar', .true.)
  call assert_equal(a%size, 3, __FILE__, __LINE__)
  select type (val => a%value('bar'))
  type is (real)
    call assert_equal(val, 4.2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  select type (val => a%last_value())
  type is (logical)
    call assert_true(val, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  ! Item 4
  ! Insert a defined type with its address.
  allocate(x)
  x%i = 1.2
  x%j = 19
  call a%insert_ptr(x)
  call assert_equal(a%size, 4, __FILE__, __LINE__)
  call assert_false(a%last_item%internal_memory, __FILE__, __LINE__)
  call assert_true(associated(a%last_item%value, x), __FILE__, __LINE__)
  select type (val => a%last_value())
  type is (test_type)
    call assert_equal(val%i, x%i, __FILE__, __LINE__)
    call assert_equal(val%j, x%j, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call a%clear()
  call assert_equal(a%size, 0, __FILE__, __LINE__)

  call assert_true(associated(x), __FILE__, __LINE__)

  call test_case_report('Test linked list insert method')

  call test_case_final()

  if (associated(x)) deallocate(x)

end program list_test
