program list_test

  use unit_test
  use linked_list_mod

  implicit none

  type(linked_list_type) list

  call test_case_init()

  call test_case_create('Test linked list type')

  call assert_equal(list%size, 0)

  call list%insert('foo', 1)
  call assert_equal(list%size, 1)
  call assert_true(associated(list%item('foo')))
  call assert_true(associated(list%value('foo')))
  select type (val => list%value('foo'))
  type is (integer)
    call assert_equal(val, 1)
  class default
    call assert_true(.false.)
  end select

  call list%insert('foo', 4.2)
  call assert_equal(list%size, 1)
  call assert_true(associated(list%item('foo')))
  call assert_true(associated(list%value('foo')))
  select type (val => list%value('foo'))
  type is (real)
    call assert_equal(val, 4.2)
  class default
    call assert_true(.false.)
  end select

  call list%insert('foo', 'bar')
  call assert_equal(list%size, 1)
  call assert_true(associated(list%item('foo')))
  call assert_true(associated(list%value('foo')))
  select type (val => list%value('foo'))
  type is (character(*))
    call assert_equal(val, 'bar')
  class default
    call assert_true(.false.)
  end select

  call test_case_report('Test linked list type')

  call test_case_final()

end program list_test
