program list_test

  use unit_test
  use linked_list_mod

  implicit none

  type(linked_list_type) list
  class(*), pointer :: val

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

  call list%insert('foo', 4.2)
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

  call list%insert('foo', 'bar')
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

  call test_case_report('Test linked list type')

  call test_case_final()

end program list_test
