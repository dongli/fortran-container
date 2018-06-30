program hash_table_test

  use unit_test
  use hash_table_mod

  implicit none

  type(hash_table_type) table
  type(hash_table_iterator_type) iter

  call test_case_init()

  call test_case_create('Test hash table type')

  table = hash_table()
  call assert_equal(table%size, 0)

  call assert_false(table%hashed('foo'))
  call table%insert('foo', 1)
  call assert_true(table%hashed('foo'))
  call assert_equal(table%size, 1)
  select type (val => table%value('foo'))
  type is (integer)
    call assert_equal(val, 1)
  class default
    call assert_true(.false.)
  end select

  call assert_false(table%hashed('bar'))
  call table%insert('bar', 4.2)
  call assert_true(table%hashed('bar'))
  call assert_equal(table%size, 2)
  select type (val => table%value('bar'))
  type is (real)
    call assert_equal(val, 4.2)
  class default
    call assert_true(.false.)
  end select

  iter = hash_table_iterator(table)
  call assert_false(iter%ended())
  call assert_equal(iter%key, 'foo')
  select type (val => iter%value)
  type is (integer)
    call assert_equal(val, 1)
  class default
    call assert_true(.false.)
  end select
  call iter%next()
  call assert_false(iter%ended())
  call assert_equal(iter%key, 'bar')
  select type (val => iter%value)
  type is (real)
    call assert_equal(val, 4.2)
  class default
    call assert_true(.false.)
  end select
  call iter%next()
  call assert_true(iter%ended())

  call test_case_report('Test hash table type')

  call test_case_final()

end program hash_table_test
