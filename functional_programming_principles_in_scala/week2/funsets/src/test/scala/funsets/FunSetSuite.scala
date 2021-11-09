package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersection contains only elements all elements that are both sets") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val s = intersect(u1, u2)
      assert(!contains(s, 1), "Intersection 1")
      assert(contains(s, 2), "Intersection 2")
      assert(contains(s, 3), "Intersection 3")
      assert(!contains(s, 4), "Intersection 4")
  }

  test("diff contains all the elements of the first set that are not in the second set") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val s = diff(u1, u2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
      assert(!contains(s, 4), "Diff 4")
  }

  test("filter returns the subset of elements for which predicate holds") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val u = union(u1, u2)
      val s = filter(u, x => x % 2 == 0)
      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
      assert(contains(s, 4), "Filter 4")
  }

  test("filter returns the subset of elements for which predicate holds") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val s = union(u1, u2)
      assert(forall(s, x => x > 0), "Filter 1")
      assert(!forall(s, x => x % 2 == 0), "Filter 2")
  }

  test("filter returns the subset of elements for which predicate holds") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val s = union(u1, u2)
      assert(exists(s, x => x == 3), "Exists 1")
      assert(!exists(s, x => x == 5), "Exists 2")
  }

  test("filter returns the subset of elements for which predicate holds") {
    new TestSets:
      val u1 = union(union(s1, s2), s3)
      val u2 = union(union(s2, s3), s4)
      val u = union(u1, u2)
      val s = map(u, x => -x)
      assert(!contains(s, 1), "Map 1")
      assert(!contains(s, 2), "Map 2")
      assert(!contains(s, 3), "Map 3")
      assert(!contains(s, 4), "Map 4")
      assert(contains(s, -1), "Map 5")
      assert(contains(s, -2), "Map 6")
      assert(contains(s, -3), "Map 7")
      assert(contains(s, -4), "Map 8")
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
