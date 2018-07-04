program hash_table_test

  use unit_test
  use hash_table_mod, hash_table_iterator => hash_table_iterator

  implicit none

  type(hash_table_type) table
  type(hash_table_iterator_type) iter

  integer count

  call test_case_init()

  call test_case_create('Test hash table type')

  table = hash_table()
  call assert_equal(table%size, 0, __FILE__, __LINE__)

  call assert_false(table%hashed('foo'), __FILE__, __LINE__)
  call table%insert('foo', 1)
  call assert_true(table%hashed('foo'), __FILE__, __LINE__)
  call assert_equal(table%size, 1, __FILE__, __LINE__)
  select type (val => table%value('foo'))
  type is (integer)
    call assert_equal(val, 1, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call table%insert('foo', 2)
  call assert_true(table%hashed('foo'), __FILE__, __LINE__)
  call assert_equal(table%size, 1, __FILE__, __LINE__)
  select type (val => table%value('foo'))
  type is (integer)
    call assert_equal(val, 2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  call assert_false(table%hashed('bar'), __FILE__, __LINE__)
  call table%insert('bar', 4.2)
  call assert_true(table%hashed('bar'), __FILE__, __LINE__)
  call assert_equal(table%size, 2, __FILE__, __LINE__)
  select type (val => table%value('bar'))
  type is (real)
    call assert_equal(val, 4.2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select

  iter = hash_table_iterator(table)
  call assert_false(iter%ended(), __FILE__, __LINE__)
  call assert_equal(iter%key, 'foo', __FILE__, __LINE__)
  select type (val => iter%value)
  type is (integer)
    call assert_equal(val, 2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  call iter%next()
  call assert_false(iter%ended(), __FILE__, __LINE__)
  call assert_equal(iter%key, 'bar', __FILE__, __LINE__)
  select type (val => iter%value)
  type is (real)
    call assert_equal(val, 4.2, __FILE__, __LINE__)
  class default
    call assert_failure(__FILE__, __LINE__)
  end select
  call iter%next()
  call assert_true(iter%ended(), __FILE__, __LINE__)

  iter = hash_table_iterator(table)
  count = 0
  do while (.not. iter%ended())
    count = count + 1
    call iter%next()
  end do
  call assert_equal(count, 2)

  call test_case_report('Test hash table type')

  call test_case_final()

end program hash_table_test
