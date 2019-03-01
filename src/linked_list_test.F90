program list_test

  use unit_test
  use linked_list_mod

  implicit none

  type test_type
    integer i
  end type test_type

  type(linked_list_type) list
  class(*), pointer :: val
  integer, pointer :: x
  type(test_type), pointer :: y

  call test_case_init()

  call test_case_create('Test linked list type')

  call assert_equal(list%size, 0)

  call list%insert('foo', 1)
  call assert_equal(list%size, 1, __FILE__, __LINE__)
  call assert_true(associated(list%item('foo')), __FILE__, __LINE__)
  call assert_true(associated(list%value('foo')), __FILE__, __LINE__)
  val => list%value('foo')
  select type (val)
  type is (integer)
    call assert_equal(val, 1, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call list%insert('foo', 4.2, nodup=.true.)
  call assert_equal(list%size, 1, __FILE__, __LINE__)
  call assert_true(associated(list%item('foo')), __FILE__, __LINE__)
  call assert_true(associated(list%value('foo')), __FILE__, __LINE__)
  val => list%value('foo')
  select type (val)
  type is (real)
    call assert_equal(val, 4.2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call list%insert('foo', 'bar', nodup=.true.)
  call assert_equal(list%size, 1, __FILE__, __LINE__)
  call assert_true(associated(list%item('foo')), __FILE__, __LINE__)
  call assert_true(associated(list%value('foo')), __FILE__, __LINE__)
  val => list%value('foo')
  select type (val)
  type is (character(*))
    call assert_equal(val, 'bar', __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call list%insert('bar', 1.2)
  call assert_equal(list%size, 2, __FILE__, __LINE__)

  allocate(x)
  x = 5
  call list%insert(x)
  call assert_equal(list%size, 3, __FILE__, __LINE__)
  val => list%last_value()
  select type (val)
  type is (integer)
    call assert_equal(val, x, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  deallocate(x)

  ! Test insert an item with value pointing to external variable
  ! so that when that variable changes, the value changes.
  allocate(y)
  y%i = 1
  call list%insert_ptr(y)
  call assert_equal(list%size, 4, __FILE__, __LINE__)
  val => list%last_value()
  select type (val)
  type is (test_type)
    call assert_equal(val%i, y%i, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  y%i = 2
  select type (val)
  type is (test_type)
    call assert_equal(val%i, y%i, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  deallocate(y)

  call test_case_report('Test linked list type')

  call test_case_final()

end program list_test
